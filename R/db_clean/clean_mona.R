# Load required packages
library(tidyverse)
library(stringr)
library(readr)
library(data.table)
library(purrr)
library(furrr)
library(future)

# Setup parallel processing
plan(multisession, workers = parallel::detectCores()-2)

# Function to extract name, inchikey, SMILES and PubChem CID from a compound block
extract_mona_info <- function(block) {
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
  }
  
  # Extract SMILES and PubChem CID from Comments
  smiles <- NA
  pubchem_cid <- NA
  
  comments <- stringr::str_extract(block, "Comments:.*?(?=\\nNum Peaks:|$)")
  if (!is.na(comments)) {
    # Extract SMILES
    smiles_match <- stringr::str_extract(comments, '"SMILES=[^"]+"')
    if (!is.na(smiles_match)) {
      smiles <- stringr::str_replace(smiles_match, '"SMILES=', "")
      smiles <- stringr::str_replace(smiles, '"$', "")
    }
    
    # Extract PubChem CID
    pubchem_match <- stringr::str_extract(comments, '"pubchem cid=[^"]+"')
    if (!is.na(pubchem_match)) {
      pubchem_cid <- stringr::str_replace(pubchem_match, '"pubchem cid=', "")
      pubchem_cid <- stringr::str_replace(pubchem_cid, '"$', "")
    }
  }
  
  # Create a data frame row with name, inchikey, smiles, and pubchem_cid columns
  if (!is.null(name)) {
    data.frame(
      name = as.character(name),
      inchikey = as.character(inchikey),
      smiles = as.character(smiles),
      pubchem_cid = as.character(pubchem_cid),
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }
}

# Function to process MoNA file
process_mona_file <- function(input_file, output_file) {
  # Check if input file exists
  if (!file.exists(input_file)) {
    stop(sprintf("Input file not found: %s", input_file))
  }
  
  # Read the file
  cat("Reading file...\n")
  file_content <- readLines(input_file)
  
  # Find all the lines that begin a new compound
  cat("Finding compound starts...\n")
  compound_start_indices <- which(grepl("^Name:", file_content) | 
                                (c("", file_content[-length(file_content)]) == "" & 
                                 nchar(trimws(file_content)) > 0 & 
                                 !grepl("^\\d+", file_content) & 
                                 !grepl("^[A-Z]+=", file_content)))
  
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
  result_list <- furrr::future_map(blocks, extract_mona_info, .progress = TRUE)
  result_df <- dplyr::bind_rows(result_list) %>%
    dplyr::filter(!is.na(name))
  
  # Remove duplicates based on inchikey and name
  result_df_unique <- result_df %>% 
    dplyr::group_by(inchikey, name,smiles) %>% 
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  # Save the result to a CSV file
  cat("Writing results to fst...\n")
  fst::write_fst(result_df_unique, output_file)
  head(result_df_unique)
  # Return summary
  list(
    total_compounds = nrow(result_df),
    unique_compounds = nrow(result_df_unique),
    output_file = output_file
  )
}

# Input and output file paths
msp_file <- "/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/database/MoNA-export-LC-MS_Spectra.msp"
output_dir <- "/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/cleaned_results/"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Process the file directly without renaming
output_file <- file.path(output_dir, "processed_mona_compounds.fst")
mona_result <- process_mona_file(msp_file, output_file)

# Print summary statistics
cat("\nProcessing Summary:\n")
cat(sprintf("Total compounds: %d\n", mona_result$total_compounds))
cat(sprintf("Unique compounds: %d\n", mona_result$unique_compounds))
cat(sprintf("Output saved to: %s\n", mona_result$output_file)) 
