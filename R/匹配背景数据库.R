match_with_backgroud_db <- function(compound_name){
  
  clean_name <- function(x) {
    # 1. Remove the "(...)" part at the end
    x <- sub("\\s+\\([^()]+\\)$", "", x)
    
    # 2. Remove "[M]" 
    x <- gsub("\\s*\\[M\\]\\s*", "", x)
    
    # 3. Remove "(+/-)", "(+)", or "(-)" patterns, avoiding double dashes
    x <- gsub("-\\(\\+/-\\)-", "-", x)  # Replace "-(+/-)-" with a single "-"
    x <- gsub("\\(\\+/-\\)", "", x)     # Remove remaining "(+/-)"
    
    x <- gsub("-\\(\\+\\)-", "-", x)    # Replace "-(+)-" with a single "-"
    x <- gsub("\\(\\+\\)", "", x)       # Remove remaining "(+)"
    
    x <- gsub("-\\(-\\)-", "-", x)      # Replace "-(-)- with a single "-"
    x <- gsub("\\(-\\)", "", x)         # Remove remaining "(-)"
    
    # 4. Replace Greek letters
    x <- gsub("α", "alpha", x)
    x <- gsub("β", "beta", x)
    x <- gsub("γ", "gamma", x)
    x <- gsub("δ", "delta", x)
    x <- gsub("ε", "epsilon", x)
    x <- gsub("ζ", "zeta", x)
    x <- gsub("η", "eta", x)
    x <- gsub("θ", "theta", x)
    x <- gsub("ι", "iota", x)
    x <- gsub("κ", "kappa", x)
    x <- gsub("λ", "lambda", x)
    x <- gsub("μ", "mu", x)
    x <- gsub("ν", "nu", x)
    x <- gsub("ξ", "xi", x)
    x <- gsub("ο", "omicron", x)
    
    # Remove "Unknown"
    x <- gsub("^Unknown$", "", x)
    
    # Remove "w/o MS2:" prefix
    x <- gsub("^w/o MS2:", "", x)
    
    # Remove "; " and everything after it (this will handle all semicolon cases)
    x <- gsub("; .*$", "", x)
    
    # 5. Remove possible extra whitespace characters
    x <- tolower(trimws(x))
    
    return(x)
  }
  
  # Keep the original name
  original_name <- clean_name(compound_name)
  
  name_map_df <- data.frame(
    compound_name = compound_name,
    clean_name = original_name,
    stringsAsFactors = FALSE
  )
  
  # --- Step 2: Merge with Local Database ---
  # Ensure required columns exist in the loaded database for the logic below
  # Specifically: 'cleaned_Metabolite_name_lc', 'pubchem_cid', 'inchikey', 'smiles'
  name_db <- tryCatch({
    fst::read_fst('/Users/zhonghua/Documents/Software_Development/cleaned_db.fst')
  }, error = function(e) {
    if (verbose) log_message(sprintf("Error loading local database: %s. Proceeding without local DB merge.", e$message), tag = "ERROR")
    return(NULL) # Return NULL or an empty df structure expected by merge
  })
  
  # Add placeholder columns to name_map_df if name_db couldn't be loaded or is empty
  # This allows the rest of the function to proceed, relying solely on API searches.
  if (is.null(name_db) || nrow(name_db) == 0) {
    name_map_df$pubchem_cid <- NA
    name_map_df$inchikey <- NA
    name_map_df$smiles <- NA
    name_map_df_merged_db <- name_map_df
    if (verbose) log_message("Local database not available or empty. Relying on API searches.", tag = "WARNING")
  } else {
    # Ensure required columns exist in name_db before merge
    required_db_cols <- c('cleaned_Metabolite_name_lc', 'pubchem_cid', 'inchikey', 'smiles')
    missing_cols <- required_db_cols[!required_db_cols %in% names(name_db)]
    if (length(missing_cols) > 0) {
      warning(sprintf("Local database is missing required columns: %s", paste(missing_cols, collapse=", ")))
      # Handle missing columns, e.g., add them with NA or stop
      # For now, let's add them with NA to allow merge to proceed
      for(col in missing_cols) name_db[[col]] <- NA
    }
    
    name_map_df_merged_db <- merge(name_map_df,
                                   # Select only necessary columns to avoid potential type conflicts or large merges
                                   name_db[, c('cleaned_Metabolite_name_lc', 'pubchem_cid', 'inchikey', 'smiles')],
                                   by.x = 'clean_name',
                                   by.y = 'cleaned_Metabolite_name_lc',
                                   all.x = TRUE) # Keep the input row even if no match in local DB
  }
  
  # --- Step 2a (NEW): Search by InChIKey/SMILES if CID is NA after merge ---
  # This logic assumes the function processes one compound_name at a time,
  # so name_map_df_merged_db should have only one row.
  retrieved_cid <- NA # Variable to store CID found in this step
  
  cid_na_list <- which(is.na(name_map_df_merged_db$pubchem_cid))
  
  for (i in cid_na_list) {
    current_inchikey <- name_map_df_merged_db$inchikey[i]
    current_smiles <- name_map_df_merged_db$smiles[i]
    
    # Priority 1: Search by InChIKey
    if (!is.na(current_inchikey) && nzchar(trimws(current_inchikey))) {
      if (verbose) log_message(sprintf("Local CID is NA. Trying search by InChIKey: %s", current_inchikey))
      search_url_inchikey <- sprintf("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/inchikey/%s/cids/JSON",
                                     utils::URLencode(trimws(current_inchikey), reserved = TRUE))
      response_inchikey <- retry_GET(search_url_inchikey, max_attempts = 2, wait_time = 0.1)
      
      if (!is.null(response_inchikey) && httr::status_code(response_inchikey) == 200) {
        tryCatch({
          cids_data <- jsonlite::fromJSON(rawToChar(response_inchikey$content))
          if (!is.null(cids_data$IdentifierList$CID) && length(cids_data$IdentifierList$CID) > 0) {
            retrieved_cid <- as.character(cids_data$IdentifierList$CID[1]) # Take the first CID
            name_map_df_merged_db$pubchem_cid[i] <- retrieved_cid # Update the df
            if (verbose) log_message(sprintf("Found CID %s using InChIKey %s", retrieved_cid, current_inchikey))
          } else {
            if (verbose) log_message(sprintf("InChIKey %s search returned no CIDs.", current_inchikey), tag = "INFO")
          }
        }, error = function(e) {
          if (verbose) log_message(sprintf("Error parsing InChIKey search result for %s: %s", current_inchikey, e$message), tag = "WARNING")
        })
      } else {
        if (verbose) log_message(sprintf("InChIKey search failed for %s (Status: %s)", current_inchikey, httr::status_code(response_inchikey %||% list(status_code = "NA"))), tag = "WARNING")
      }
    }
    
    # Priority 2: Search by SMILES (only if InChIKey didn't yield a CID)
    if (is.na(retrieved_cid) && !is.na(current_smiles) && nzchar(trimws(current_smiles))) {
      if (verbose) log_message(sprintf("Local CID is NA and InChIKey search failed/NA. Trying search by SMILES: %s", current_smiles))
      search_url_smiles <- sprintf("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/smiles/%s/cids/JSON",
                                   utils::URLencode(trimws(current_smiles), reserved = TRUE)) # SMILES needs URL encoding
      response_smiles <- retry_GET(search_url_smiles, max_attempts = 2, wait_time = 0.1)
      
      if (!is.null(response_smiles) && httr::status_code(response_smiles) == 200) {
        tryCatch({
          cids_data <- jsonlite::fromJSON(rawToChar(response_smiles$content))
          if (!is.null(cids_data$IdentifierList$CID) && length(cids_data$IdentifierList$CID) > 0) {
            retrieved_cid <- as.character(cids_data$IdentifierList$CID[1]) # Take the first CID
            name_map_df_merged_db$pubchem_cid[1] <- retrieved_cid # Update the df
            if (verbose) log_message(sprintf("Found CID %s using SMILES %s", retrieved_cid, substr(current_smiles, 1, 50))) # Log truncated SMILES
          } else {
            if (verbose) log_message(sprintf("SMILES search returned no CIDs for: %s", substr(current_smiles, 1, 50)), tag = "INFO")
          }
        }, error = function(e) {
          if (verbose) log_message(sprintf("Error parsing SMILES search result for %s: %s", substr(current_smiles, 1, 50), e$message), tag = "WARNING")
        })
      } else {
        if (verbose) log_message(sprintf("SMILES search failed for %s (Status: %s)", substr(current_smiles, 1, 50), httr::status_code(response_smiles %||% list(status_code = "NA"))), tag = "WARNING")
      }
    }
  } # End of check for missing CID and attempt to find via InChIKey/SMILES
  
  
  # --- Step 2b/2c (NEW): Fetch properties if CID is now known ---
  # Check the potentially updated pubchem_cid in the data frame
  cid_get_property_list <- name_map_df_merged_db$pubchem_cid[which(!is.na(name_map_df_merged_db$pubchem_cid))]
  
  get_property_use_cid <- function(m) {
    if (verbose) log_message(sprintf("Found/Confirmed CID: %s. Fetching properties directly.", m))
    final_cid = m
    current_query_compoundname <- name_map_df_merged_db[which(name_map_df_merged_db$pubchem_cid == final_cid),'compound_name']
    
    info_url <- sprintf("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/%s/property/IUPACName,MolecularFormula,MolecularWeight,CanonicalSMILES,IsomericSMILES,InChI,InChIKey/JSON",
                        final_cid)
    info_response <- retry_GET(info_url, max_attempts = 2, wait_time = 0.1)
    
    if (!is.null(info_response) && httr::status_code(info_response) == 200) {
      tryCatch({
        compound_data <- jsonlite::fromJSON(rawToChar(info_response$content))$PropertyTable$Properties
        if (!is.null(compound_data) && nrow(compound_data) > 0) {
          compound_data <- as.data.frame(compound_data, stringsAsFactors = FALSE)
          
          # Ensure all required columns exist
          required_cols <- c("CID", "IUPACName", "MolecularFormula", "MolecularWeight", "CanonicalSMILES", "IsomericSMILES", "InChI", "InChIKey")
          for (col in required_cols) {
            if (!(col %in% names(compound_data))) {
              compound_data[[col]] <- NA
            }
          }
          
          # Get database link information
          db_links <- get_kegg_id(as.numeric(compound_data$CID)) # Assuming get_kegg_id can handle numeric CID
          
          # Coalesce function helper (pick first non-NA)
          coalesce_na <- function(...) {
            valid_args <- Filter(function(y) {
              !is.null(y) && length(y) > 0 && !is.na(y[1]) && y[1] != ""
            }, list(...))
            
            if (length(valid_args) > 0) {
              # Return the first element of the first valid argument found
              return(valid_args[[1]][1])
            } else {
              return('Not_Access')
            }
          }
          
          compound_data$KEGG_ID  <- coalesce_na(db_links$KEGG)
          compound_data$HMDB_ID  <- coalesce_na(db_links$HMDB)
          compound_data$CHEBI_ID <- coalesce_na(db_links$CHEBI)
          compound_data$CTD      <- coalesce_na(db_links$CTD)
          
          # Add search name and similarity (indicate source)
          compound_data$SearchName <- current_query_compoundname
          compound_data$Similarity <- 1
          # Ensure CID is character type like in other parts
          compound_data$CID <- as.character(compound_data$CID)
          
          # Select and order columns - ensure consistency
          final_cols <- c("CID", "MolecularFormula", "MolecularWeight", "CanonicalSMILES", "IsomericSMILES", "InChI", "InChIKey",
                          "IUPACName", "KEGG_ID", "HMDB_ID", "CHEBI_ID", "CTD", "SearchName", "Similarity")
          # Add missing columns if any (shouldn't happen with check above, but safety)
          for(fcol in final_cols) { if(!fcol %in% names(compound_data)) compound_data[[fcol]] <- NA }
          
          return(compound_data[, final_cols, drop = FALSE]) # Return the dataframe
        } else {
          if (verbose) log_message(sprintf("Could not retrieve properties for CID %s, although CID was found.", final_cid), tag = "WARNING")
        }
      }, error = function(e) {
        if (verbose) log_message(sprintf("Error processing properties for CID %s: %s", final_cid, e$message), tag = "WARNING")
      })
    } else {
      if (verbose) log_message(sprintf("Failed to get info for CID: %s (Status: %s)", final_cid, httr::status_code(info_response %||% list(status_code="NA"))), tag = "WARNING")
    }
    # If fetching properties failed even with CID, fall through to name search? Or return NULL?
    # Current logic falls through, but perhaps returning NULL is better if CID was known but properties failed.
    # Let's add a return NULL here if properties fail for a known CID.
    log_message(sprintf("Failed to retrieve properties for known CID %s. No result returned.", final_cid), tag = "WARNING")
    return(NULL)
  }
  
  mapped_db_res <- plyr::ldply(cid_get_property_list, get_property_use_cid)
  
  need_pubchem_query_name_list <- name_map_df_merged_db$compound_name[which(is.na(name_map_df_merged_db$pubchem_cid))]
  
  return(list(mapped_db_res = mapped_db_res,
              need_pubchem_query_name_list = need_pubchem_query_name_list))
}
