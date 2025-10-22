# Load required packages
library(tidyverse)
library(stringr)
library(furrr)
library(purrr)
# Set up parallel processing
plan(multisession)  # Use multiple sessions for parallel processing

# Function to extract name and inchikey from a single compound block
extract_compound_info <- function(block) {
  # Extract NAME and clean it
  name <- str_extract(block, "NAME: ([^\\n]+)") %>%
    str_replace("NAME: ", "") %>%
    str_extract("^[^;]+") %>%  # Extract only the part before the first semicolon
    str_trim()  # Remove any leading/trailing whitespace
  
  # Extract INCHIKEY
  inchikey <- str_extract(block, "INCHIKEY: ([^\\n]+)") %>%
    str_replace("INCHIKEY: ", "")
  
  # Extract SMILES
  smiles <- str_extract(block, "SMILES: ([^\\n]+)") %>%
    str_replace("SMILES: ", "")
  
  # Create a data frame row
  data.frame(
    name = name,
    inchikey = inchikey,
    smiles = smiles,
    stringsAsFactors = FALSE
  )
}

# Function to process MS-DIAL file
process_msdial_file <- function(input_file, output_file) {
  # Check if input file exists
  if (!file.exists(input_file)) {
    stop(sprintf("Input file not found: %s", input_file))
  }
  
  # Read the file
  file_content <- readLines(input_file)
  
  # Split the content into blocks (each compound)
  blocks <- paste(file_content, collapse = "\n") %>%
    str_split("\n\n") %>%
    unlist()
  
  # Process each block in parallel and combine into a data frame
  result_df <- blocks %>%
    future_map_dfr(extract_compound_info, .progress = TRUE)
  
  # Remove duplicates based on inchikey
  result_df_unique <- result_df %>%
    dplyr::filter(!is.na(inchikey)) %>%
    dplyr::group_by(inchikey) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  # Save the result to a CSV file
  fst::write_fst(result_df_unique, output_file)
  
  # Return summary statistics
  list(
    total_compounds = nrow(result_df),
    unique_compounds = nrow(result_df_unique),
    output_file = output_file
  )
}

# Process positive mode file
pos_result <- process_msdial_file(
  input_file = "/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/database/MSMS-Public_all-pos-VS19.msp",
  output_file = "/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/cleaned_results/processed_msms_spectra_msdial_pos.fst"
)

# Process negative mode file
neg_result <- process_msdial_file(
  input_file = "/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/database/MSMS-Public_all-neg-VS19.msp",
  output_file = "/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/cleaned_results/processed_msms_spectra_msdial_neg.fst"
)

# Combine positive and negative mode results
msdial_result <- rbind(fst::read_fst(pos_result$output_file), fst::read_fst(neg_result$output_file)) %>%
    dplyr::group_by(inchikey,name,smiles) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
fst::write_fst(msdial_result, '/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/cleaned_results/processed_msdial.fst')

# # Print summary statistics
# cat("\nProcessing Summary:\n")
# cat(sprintf("Positive Mode:\n"))
# cat(sprintf("  Total compounds: %d\n", pos_result$total_compounds))
# cat(sprintf("  Unique compounds: %d\n", pos_result$unique_compounds))
# cat(sprintf("  Output saved to: %s\n", pos_result$output_file))
# cat(sprintf("\nNegative Mode:\n"))
# cat(sprintf("  Total compounds: %d\n", neg_result$total_compounds))
# cat(sprintf("  Unique compounds: %d\n", neg_result$unique_compounds))
# cat(sprintf("  Output saved to: %s\n", neg_result$output_file))
# 
# ### Simplified sample test----
# test <- read.delim('/Users/zhonghua/Downloads/msdial_Expert/20241212_Sam_5uL_GIST-151#_POS_MS-DDA1_L2-进样测_001.txt')
# 
# # Function to clean compound names
# clean_compound_name <- function(x) {
#   # Remove "Unknown"
#   x <- gsub("^Unknown$", "", x)
#   
#   # Remove "w/o MS2:" prefix
#   x <- gsub("^w/o MS2:", "", x)
#   
#   # Remove "; CE0; " and everything after it
#   x <- gsub("; CE0; .*$", "", x)
#   
#   # Remove "; CE30" and similar patterns
#   x <- gsub("; CE\\d+", "", x)
#   
#   # Trim any leading/trailing whitespace
#   x <- trimws(x)
#   
#   return(x)
# }
# 
# # Apply cleaning to test$Title
# test$Title_clean <- sapply(test$Title, clean_compound_name)
# 
# # Select and prepare test data for joining
# test_name <- test |> 
#   dplyr::select(Title, Title_clean)
# 
# # First join (case-sensitive)
# test_name_with_info <- test_name |>
#   dplyr::left_join(msdial_result, 
#                   by = c("Title_clean" = "name"))
# 
# # Identify unmatched rows
# unmatched_rows <- test_name_with_info |>
#   dplyr::filter(is.na(inchikey))
# 
# # Second join (case-insensitive) for unmatched rows
# if (nrow(unmatched_rows) > 0) {
#   # Create case-insensitive versions of names in msdial_result
#   msdial_result_case_insensitive <- msdial_result |>
#     dplyr::mutate(name_lower = tolower(name))
#   
#   # Join unmatched rows with case-insensitive matching
#   unmatched_with_info <- unmatched_rows |>
#     dplyr::mutate(Title_clean_lower = tolower(Title_clean)) |>
#     dplyr::left_join(msdial_result_case_insensitive,
#                     by = c("Title_clean_lower" = "name_lower")) |>
#     dplyr::select(-Title_clean_lower, -name_lower)
#   
#   # Combine results
#   final_result <- test_name_with_info |>
#     dplyr::filter(!is.na(inchikey)) |>
#     dplyr::bind_rows(unmatched_with_info)
# } else {
#   final_result <- test_name_with_info
# }
# 
# # Clean up the final result by selecting only needed columns
# final_result <- final_result |>
#   dplyr::select(Title, Title_clean, inchikey)
# 
# # Print summary of the join results
# cat("\nJoin Summary:\n")
# cat(sprintf("Total rows in test_name: %d\n", nrow(test_name)))
# cat(sprintf("Rows matched in first join: %d\n", sum(!is.na(test_name_with_info$inchikey))))
# cat(sprintf("Rows matched in second join: %d\n", sum(!is.na(final_result$inchikey)) - sum(!is.na(test_name_with_info$inchikey))))
# cat(sprintf("Total unmatched rows: %d\n", sum(is.na(final_result$inchikey))))
# 
# # Save the final result
# openxlsx::write.xlsx(final_result, '/Users/zhonghua/Downloads/msdial_Expert/Sample_test.xlsx')
