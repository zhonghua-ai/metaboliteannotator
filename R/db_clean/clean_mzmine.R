# Load required packages
library(tidyverse)
library(stringr)
library(furrr)
library(purrr)
library(tools) # For file extension functions

# Set up parallel processing
plan(multisession)  # Use multiple sessions for parallel processing

# Function to extract name and inchikey from a single block
extract_mzmine_info <- function(block) {
  # Skip empty blocks
  if (is.na(block) || nchar(trimws(block)) < 10) {
    return(NULL)
  }
  
  # Extract NAME
  name <- str_extract(block, "NAME=([^\n]+)") %>%
    str_replace("NAME=", "") %>%
    str_trim()
  
  # Extract INCHIAUX (InChIKey)
  inchikey <- str_extract(block, "INCHIAUX=([^\n]+)") %>%
    str_replace("INCHIAUX=", "")
  
  # Extract SMILES
  smiles <- str_extract(block, "SMILES=([^\n]+)") %>%
    str_replace("SMILES=", "")
  
  # Create a data frame row
  if (!is.null(name)) {
    data.frame(
      name = name,
      inchikey = inchikey,
      smiles = smiles,
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }
}

# Function to process MZmine file
process_mzmine_file <- function(input_file) {
  # Check if input file exists
  if (!file.exists(input_file)) {
    warning(sprintf("Input file not found: %s", input_file))
    return(NULL)
  }
  
  # Read the file
  file_content <- readLines(input_file)
  
  # Combine lines and split into blocks
  content_combined <- paste(file_content, collapse = "\n")
  blocks <- str_split(content_combined, "BEGIN IONS\n")[[1]][-1] # Skip the first empty element
  
  # For each block, extract until END IONS
  blocks <- blocks %>%
    purrr::map_chr(function(b) {
      parts <- str_split(b, "END IONS")[[1]][1]
      return(parts)
    })
  
  # Process each block in parallel and combine into a data frame
  result_df <- blocks %>%
    future_map_dfr(extract_mzmine_info, .progress = TRUE) %>%
    dplyr::filter(!is.na(name))  # Remove rows with NA names
  
  # # Save the result to a CSV file if provided
  # if (!is.null(output_file)) {
  #   write.csv(result_df, output_file, row.names = FALSE)
  # }
  
  # Return the data frame and stats
  list(
    data = result_df,
    total_compounds = nrow(result_df)
  )
}

# List of input MGF files
mgf_files <- c(
  "~/Documents/Shimazu_Test/msdial_Expert/database/20241003_otavapep_pos_ms2.mgf",
  "~/Documents/Shimazu_Test/msdial_Expert/database/20241003_otavapep_neg_ms2.mgf",
  "~/Documents/Shimazu_Test/msdial_Expert/database/20241003_nihnp_pos_ms2.mgf",
  "~/Documents/Shimazu_Test/msdial_Expert/database/20241003_nihnp_neg_ms2.mgf",
  "~/Documents/Shimazu_Test/msdial_Expert/database/20241003_mcescaf_pos_ms2.mgf",
  "~/Documents/Shimazu_Test/msdial_Expert/database/20241003_mcescaf_neg_ms2.mgf",
  "~/Documents/Shimazu_Test/msdial_Expert/database/20241003_enammol_pos_ms2.mgf",
  "~/Documents/Shimazu_Test/msdial_Expert/database/20241003_enammol_neg_ms2.mgf",
  "~/Documents/Shimazu_Test/msdial_Expert/database/20241003_mcedrug_pos_ms2.mgf",
  "~/Documents/Shimazu_Test/msdial_Expert/database/20241003_mcedrug_neg_ms2.mgf",
  "~/Documents/Shimazu_Test/msdial_Expert/database/20241003_mcebio_pos_ms2.mgf",
  "~/Documents/Shimazu_Test/msdial_Expert/database/20241003_mcebio_neg_ms2.mgf",
  "~/Documents/Shimazu_Test/msdial_Expert/database/20241003_enamdisc_pos_ms2.mgf",
  "~/Documents/Shimazu_Test/msdial_Expert/database/20241003_enamdisc_neg_ms2.mgf"
)

# Output directory
output_dir <- "~/Documents/Shimazu_Test/msdial_Expert/cleaned_results/"
dir.create(output_dir, showWarnings = FALSE)

# Process each file and collect results
all_results <- list()
all_data <- list()

for (i in seq_along(mgf_files)) {
  file_path <- mgf_files[i]
  file_name <- basename(file_path)
  
  # Process the file
  result <- process_mzmine_file(file_path)
  
  if (!is.null(result)) {
    all_results[[file_name]] <- result
    all_data[[file_name]] <- result$data
    
    cat(sprintf("  Compounds found: %d\n", result$total_compounds))
    # cat(sprintf("  Output saved to: %s\n", result$output_file))
  }
}

# Step 3: Combine all data
if (length(all_data) > 0) {
  combined_data <- bind_rows(all_data)
  
  # Step 4: Remove duplicates based on inchikey
  if (nrow(combined_data) > 0 && !all(is.na(combined_data$inchikey))) {
    combined_data_unique <- combined_data %>%
      dplyr::filter(!is.na(inchikey)) %>%
      dplyr::group_by(inchikey, name) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  } else {
    combined_data_unique <- combined_data
  }
  
  # Step 5: Save the combined result
  combined_output_file <- file.path(output_dir, "combined_mzmine_results.fst")
  fst::write_fst(combined_data_unique, combined_output_file)
  
  # Print summary
  cat("\nCombined Results Summary:\n")
  cat(sprintf("Total compounds processed: %d\n", nrow(combined_data)))
  cat(sprintf("Unique compounds after deduplication: %d\n", nrow(combined_data_unique)))
  cat(sprintf("Combined output saved to: %s\n", combined_output_file))
  
} else {
  cat("\nNo data was processed successfully from any file.\n")
}
