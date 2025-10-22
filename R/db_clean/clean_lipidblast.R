# Load required packages
library(tidyverse)
library(stringr)
library(readr)
library(data.table)
library(purrr)
library(furrr)
library(future)

# Setup parallel processing
plan(multisession, workers = parallel::detectCores()-4)

# Function to extract name, inchikey and SMILES from a compound block
extract_lipidblast_info <- function(block) {
  # Skip empty blocks
  if (is.na(block) || nchar(trimws(block)) < 10) {
    return(NULL)
  }
  
  # Clean the name field if needed
  block <- gsub("^\\s*Name:\\s*Name:\\s*", "Name: ", block, perl = TRUE)
  
  # Check if the block starts with "Name: " or not
  if (!grepl("^Name:", block)) {
    lines <- strsplit(block, "\n")[[1]]
    name <- trimws(lines[1])
    block <- paste0("Name: ", name, "\n", paste(lines[-1], collapse = "\n"))
  }
  
  # Extract name and inchikey fields
  name <- stringr::str_extract(block, "^Name:\\s*([^\\n]+)")
  name <- stringr::str_replace(name, "^Name:\\s*", "")
  name <- stringr::str_trim(name)
  
  inchikey <- stringr::str_extract(block, "InChIKey:\\s*([^\\n]+)")
  if (!is.na(inchikey)) {
    inchikey <- stringr::str_replace(inchikey, "^InChIKey:\\s*", "")
    inchikey <- stringr::str_trim(inchikey)
  } else {
    inchikey <- NA_character_  # Set to NA if not found
  }
  
  # Extract SMILES from the Comments field - improved pattern matching
  smiles <- NA_character_  # Default to NA
  
  # Try multiple SMILES extraction patterns
  
  # Pattern 1: Try to extract SMILES= format
  smiles_match <- stringr::str_extract(block, "SMILES=([^\"\\n]+)")
  if (!is.na(smiles_match)) {
    smiles <- stringr::str_replace(smiles_match, "^SMILES=", "")
    smiles <- stringr::str_trim(smiles)
  }
  
  # Pattern 2: If first pattern failed, try computed SMILES
  if (is.na(smiles) || nchar(smiles) < 5) {
    computed_match <- stringr::str_extract(block, "computed SMILES=([^\"\\n]+)")
    if (!is.na(computed_match)) {
      smiles <- stringr::str_replace(computed_match, "^computed SMILES=", "")
      smiles <- stringr::str_trim(smiles)
    }
  }
  
  # Create a data frame row with name, inchikey and SMILES columns
  if (!is.null(name)) {
    data.frame(
      name = as.character(name),
      inchikey = as.character(inchikey),
      smiles = as.character(smiles),
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }
}

# Function to process LipidBlast file
process_lipidblast_file <- function(input_file, output_file) {
  # Check if input file exists
  if (!file.exists(input_file)) {
    stop(sprintf("Input file not found: %s", input_file))
  }
  
  # Read the file
  cat("Reading file...\n")
  file_content <- readLines(input_file)
  
  # Find all the lines that begin a new compound
  cat("Finding compound starts...\n")
  compound_start_indices <- which(grepl("^Name:", file_content))
  
  # Create blocks by extracting lines between start indices
  cat("Creating compound blocks...\n")
  blocks <- vector("character", length(compound_start_indices))
  for (i in 1:length(compound_start_indices)) {
    start_idx <- compound_start_indices[i]
    end_idx <- if (i < length(compound_start_indices)) compound_start_indices[i+1] - 1 else length(file_content)
    blocks[i] <- paste(file_content[start_idx:end_idx], collapse = "\n")
  }
  
  # Process each block in parallel using furrr
  cat("Processing compounds in parallel...\n")
  result_list <- furrr::future_map(blocks, extract_lipidblast_info, .progress = TRUE)
  result_df <- dplyr::bind_rows(result_list) %>%
    dplyr::filter(!is.na(name))
  
  # Remove duplicates based on inchikey and name
  result_df_unique <- result_df %>% 
    dplyr::group_by(inchikey, name, smiles) %>% 
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  # Save the result to a CSV file
  cat("Writing results to fst...\n")
  fst::write_fst(result_df_unique, output_file)
  
  # Return summary
  list(
    total_compounds = nrow(result_df),
    unique_compounds = nrow(result_df_unique),
    output_file = output_file
  )
}

# Input and output file paths
msp_file <- "/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/database/MoNA-export-LipidBlast_2022.msp"
output_dir <- "/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/cleaned_results/"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Process the file directly without renaming
output_file <- file.path(output_dir, "processed_lipidblast_name_inchikey_smiles.fst")
lipidblast_result <- process_lipidblast_file(msp_file, output_file)

# Print summary statistics
cat("\nProcessing Summary:\n")
cat(sprintf("Total compounds: %d\n", lipidblast_result$total_compounds))
cat(sprintf("Unique compounds: %d\n", lipidblast_result$unique_compounds))
cat(sprintf("Output saved to: %s\n", lipidblast_result$output_file))

# Additional analysis of the SMILES data
processed_data <- fst::read_fst(output_file)
cat("\nSMILES Analysis:\n")
cat(sprintf("Compounds with SMILES: %d (%.2f%%)\n", 
            sum(!is.na(processed_data$smiles)),
            sum(!is.na(processed_data$smiles)) / nrow(processed_data) * 100))

# Print a few examples of the extracted SMILES
cat("\nSample SMILES (first 5):\n")
print(head(processed_data$smiles[!is.na(processed_data$smiles)], 5))
