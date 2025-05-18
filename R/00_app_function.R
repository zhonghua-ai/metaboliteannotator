#' Initialize Application Directories
#'
#' Set up the required directory structure in a specified working directory.
#'
#' @param work_path Character string specifying the path to the working directory (default: getwd()).
#'
#' @return A list of directory paths including root, R, data, process, and cache directories.
#'
#' @export
init_app <- function(work_path) {

  # create relevant directories
  base_dir <- work_path
  if (!dir.exists(base_dir)) {
    stop(sprintf("The specified base directory '%s' does not exist.", base_dir))
  }
  log_message("Start creat directory, please wait...")

  paths <- list(
    root = base_dir,
    R = file.path(base_dir, "R"),
    data = file.path(base_dir, "inst", "extdata"),
    process = file.path(base_dir, "inst", "extdata", "process_file"),
    cache = file.path(base_dir, "inst", "extdata", "cache"),
    www = file.path(base_dir, "inst", "www")
  )

  # Create necessary directories (if they don't exist)
  for (dir_path in c(paths$R, paths$data, paths$process, paths$cache, paths$www)) {
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      log_message(sprintf("Created directory: %s", dir_path))
    }
  }

  # Assign APP_PATHS to the global environment before using it
  assign("APP_PATHS", paths, envir = .GlobalEnv)
  
  log_message("Start check necessary packages, please wait...")
  
  # check necessary packages
  check_and_install_packages <- function() {
    required_packages <- c(
      "data.table",
      "digest",
      "DT",
      "readr",
      "dplyr",
      "eoffice",
      "ellmer",
      "fst",
      "fstcore",
      "future.apply",
      "ggplot2",
      "ggthemes",
      "httr",
      "jsonlite", 
      "KEGGREST",
      "knitr",
      "markdown",
      "parallel",
      "purrr",
      "R.utils",
      "rmarkdown",
      "rvest",
      "shiny",
      "shinycssloaders",
      "shinyjs",
      "stringdist", 
      "stringr",
      "tidyr",
      "utils",
      "xml2"
    )
    
    # Check and install missing packages
    for (pkg in required_packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        message(sprintf("Installing package: %s", pkg))
        install.packages(pkg)
      }
    }
    
    log_message("All required packages are installed.")
  }

  # Run the package check function
  check_and_install_packages()


  # check and update linked dataset
  log_message("Start update linked dataset, please wait...")
  pubchem_dir <- file.path(APP_PATHS$data, "pubchem_info")
  if (!dir.exists(pubchem_dir)) {
    dir.create(pubchem_dir, recursive = TRUE, showWarnings = FALSE)
    log_message(sprintf("Created directory: %s", pubchem_dir))
  }
  
  cid_parent_file <- file.path(pubchem_dir, "CID-Parent.fst")
  cid_parent_last_update_time <- file.path(pubchem_dir, "cid_parent_lastest_update_time.rds")
  
  if (!file.exists(cid_parent_file) || !file.exists(cid_parent_last_update_time)) {
    update_pubchem_data()
    log_message(sprintf('Updating Pubchem CID-Parent...'), tag = '[INFO]')
  } else {
    last_update <- readRDS(cid_parent_last_update_time)
    if (difftime(Sys.Date(), last_update, units = "days") > 7) {
      update_pubchem_data()
      log_message(sprintf('Updating Pubchem CID-Parent...'), tag = '[INFO]')
    }
  }

  ctd_dir <- file.path(APP_PATHS$data, "ctd_info")
  ctd_file <- file.path(ctd_dir, "chem-gene_data.fst")
  ctd_last_update_time <- file.path(ctd_dir, "ctd_lastest_update_time.rds")
  ctd_dir <- file.path(APP_PATHS$data, "ctd_info")
  if (!dir.exists(ctd_dir)) {
    dir.create(ctd_dir, recursive = TRUE, showWarnings = FALSE)
    log_message(sprintf("Created directory: %s", ctd_dir))
  }
  if (!file.exists(ctd_file) || !file.exists(ctd_last_update_time)) {
      update_CTD_data()
      log_message(sprintf('Updating CTD dataset...'), tag = '[INFO]')
    } else {
      last_update <- readRDS(ctd_last_update_time)
      if (difftime(Sys.Date(), last_update, units = "days") > 30) {
        update_CTD_data()
        log_message(sprintf('Updating CTD dataset...'), tag = '[INFO]')
      }
  }

  reactome_dir <- file.path(APP_PATHS$data, "reactome_info")
  reactome_file <- file.path(reactome_dir, "reactome_chebi_mapping.fst")
  reactome_last_update_time <- file.path(reactome_dir, "reactome_lastest_update_time.rds")
  if (!file.exists(reactome_file) || !file.exists(reactome_last_update_time)) {
    update_reactome_data()
    log_message(sprintf('Updating Reactome data...'), tag = '[INFO]')
  } else {
    last_update <- readRDS(reactome_last_update_time)
    if (difftime(Sys.Date(), last_update, units = "days") > 30) {
      update_reactome_data()
      log_message(sprintf('Updating Reactome data...'), tag = '[INFO]')
    }
  }

  return('init success! please run code run_ui()!')
}

#' Redirect Console Output to a Shiny UI Container
#'
#' This function evaluates an expression while capturing all console output, messages,
#' warnings and errors, then adds the corresponding messages to a specified UI container.
#'
#' @param containerId A character string specifying the ID of the UI container
#' (typically a \code{div}) where the console messages should be inserted.
#' @param expr An expression to be evaluated. All output generated during its evaluation
#' (including standard output, messages, warnings and errors) will be captured and
#' added to the UI container.
#'
#' @details
#' The function first clears the contents of the specified UI container using
#' \code{removeUI}. It defines an internal helper function (\code{appendToConsole})
#' that formats messages with a timestamp and an icon (depending on the message type)
#' and then inserts them into the UI using \code{insertUI}. Additionally, the function
#' automatically scrolls the container to the bottom to keep the latest messages visible.
#'
#' It uses \code{withCallingHandlers} to intercept output from \code{expr} (output,
#' messages, warnings, and errors) and passes each to \code{appendToConsole} with a
#' corresponding type.
#'
#' @return The value returned by evaluating \code{expr}.
#'
#' @examples
#' \dontrun{
#'   # Assuming there is a UI element with id "console", you could use:
#'   withConsoleRedirect("console", {
#'       cat("Hello, world!\n")
#'       message("This is a message")
#'       warning("This is a warning")
#'       # An error example:
#'       # stop("An error occurred")
#'   })
#' }
#'
#' @export
withConsoleRedirect <- function(containerId, expr) {
  # Clear the console
  removeUI(selector = paste0("#", containerId, " *"))

  # Create a function to add messages to the UI
  appendToConsole <- function(txt, type = "info") {
    # Check if the message already has a timestamp and tag
    if (grepl("^\\[\\d{2}:\\d{2}:\\d{2}\\]\\s+\\[[A-Z]+\\]", txt)) {
      # Message already has timestamp and tag, use it directly
      styled_text <- paste0(
        "<span class='console-", type, "'>",
        txt,
        "</span><br/>"
      )
    } else {
      # Add timestamp and tag
      timestamp <- format(Sys.time(), "[%H:%M:%S]")
      # Remove any brackets if present in type
      cleaned_type <- gsub("\\[|\\]", "", type)
      icon <- switch(tolower(cleaned_type),
                     "info" = "[INFO] ",
                     "warning" = "[WARNING] ",
                     "error" = "[ERROR] ",
                     "success" = "[SUCCESS] ",
                     "[INFO] "
      )

      formatted_text <- paste0(timestamp, " ", icon, " ", txt)
      styled_text <- paste0(
        "<span class='console-", type, "'>",
        formatted_text,
        "</span><br/>"
      )
    }

    insertUI(
      paste0("#", containerId),
      where = "beforeEnd",
      immediate = TRUE,
      ui = HTML(styled_text)
    )

    # Automatically scroll to the bottom of the console to ensure the latest message is visible
    shinyjs::runjs(sprintf("var elem = document.getElementById('%s'); if (elem) { elem.scrollTop = elem.scrollHeight; }", containerId))
  }

  # Use withCallingHandlers to capture all types of output
  withCallingHandlers(
    {
      # Capture standard output
      ret <- withVisible({
        capture.output({
          result <- expr
        }, type = "output") -> output

        # Process standard output
        if (length(output) > 0) {
          for (line in output) {
            if (nchar(trimws(line)) > 0) {
              appendToConsole(line, "info")
            }
          }
        }
        result
      })
      ret$value
    },
    message = function(m) {
      appendToConsole(m$message, "info")
      Sys.sleep(0.1)  # Give the UI a little time to update
    },
    warning = function(w) {
      appendToConsole(w$message, "warning")
      Sys.sleep(0.1)
    },
    error = function(e) {
      appendToConsole(e$message, "error")
      Sys.sleep(0.1)
    }
  )
}

#' Log Message with Timestamp and Icon
#'
#' Write a log message with timestamp and appropriate icon based on message type.
#'
#' @param ... One or more message components to be concatenated
#' @param tag Character string indicating message type: "INFO", "WARNING", "ERROR", or "SUCCESS"
#'
#' @details
#' The function prepends a timestamp and an appropriate icon to the message:
#' \itemize{
#'   \item INFO: [INFO]
#'   \item WARNING: [WARNING]
#'   \item ERROR: [ERROR]
#'   \item SUCCESS: [SUCCESS]
#' }
#'
#' @examples
#' \dontrun{
#' log_message("Process started", tag = "INFO")
#' log_message("Operation completed", tag = "SUCCESS")
#' }
#'
#' @export
log_message <- function(..., tag = "INFO") {
  # exclude the brackets in the tag parameter
  cleaned_tag <- gsub("\\[|\\]", "", tag)

  text <- paste0(...)
  timestamp <- format(Sys.time(), "[%H:%M:%S]")
  icon <- switch(tolower(cleaned_tag),
                 "info" = "[INFO] ",
                 "warning" = "[WARNING] ",
                 "error" = "[ERROR] ",
                 "success" = "[SUCCESS] ",
                 "[INFO] "
  )
  
  # Create the formatted message but don't output it directly with message()
  formatted_text <- paste0(timestamp, " ", icon, " ", text)
  
  # In Shiny context, we want to avoid using message() which causes duplication
  # Instead, we'll rely on the UI update mechanism in withConsoleRedirect
  if (exists("session") && inherits(session, "ShinySession")) {
    # When in Shiny context, return the formatted text without printing
    return(invisible(formatted_text))
  } else {
    # When not in Shiny context (e.g., direct console use), use message()
    message(formatted_text)
    return(invisible(formatted_text))
  }
}

#' Update Console Output
#'
#' Update the console output with formatted messages including timestamp and icons.
#'
#' @param text Character string containing the message to display
#' @param type Character string indicating message type: "info", "warning", "error", "success", "processing", or "completed"
#' @param console_output_setter Function to set console output
#'
#' @details
#' Formats the message with timestamp and appropriate icon based on message type.
#' Adds HTML styling for different message types.
#'
#' @keywords internal
update_console <- function(text, type = "info", console_output_setter) {
  timestamp <- format(Sys.time(), "[%H:%M:%S]")
  icon <- switch(type,
                 "info" = "[INFO]",
                 "warning" = "[WARNING]",
                 "error" = "[ERROR]",
                 "success" = "[SUCCESS]",
                 "processing" = "[PROCCESSING]",
                 "completed" = "[COMPLETE]",
                 "[INFO]"
  )
  formatted_text <- paste0(timestamp, " ", icon, " ", text)
  styled_text <- paste0("<span class='console-", type, "'>", formatted_text, "</span>")
  console_output_setter(paste0(console_output_setter(), styled_text, "\n"))
  flush.console()
}


#' Process Compounds with Parallel Processing
#'
#' @param compounds Character vector of compound names to be processed.
#' @param process_dir Character string specifying the directory where processing results will be saved.
#' @param clear_cache Logical flag indicating whether to clear the cache before processing. Defaults to TRUE.
#' @param n_cores Number of cores to use for parallel processing. Defaults to 4.
#' @param verbose Logical flag indicating whether to output detailed progress messages. Defaults to FALSE.
#'
#' @return A list containing processing results for all compounds.
#'
#' @export
process_compounds <- function(compounds, process_dir, clear_cache = TRUE, n_cores = 4, verbose = FALSE) {
  # Create directories if needed
  dir.create(process_dir, recursive = TRUE, showWarnings = FALSE)

  metabo_file <- file.path(process_dir, "metabo_name.txt")
  tryCatch({
    writeLines(compounds, metabo_file)
  }, error = function(e) {
    stop(sprintf("Cannot write to file %s: %s", metabo_file, e$message))
  })

  log_message("Starting compound processing...")
  log_message(sprintf("Results will be saved to: %s", process_dir))

  name_list <- tryCatch({
    scan(metabo_file, what = "character", sep = "\n", quiet = TRUE)
  }, error = function(e) {
    stop(sprintf("Cannot read file %s: %s", metabo_file, e$message))
  })

  name_list_unique <- unique(name_list)

  if (length(name_list_unique) == 0) {
    stop("No valid compound names")
  }

  cache_dir <- APP_PATHS$cache

  if (clear_cache) {
    clean_cache(cache_dir)
    log_message("Cache cleared, will reprocess all compounds")
  } else {
    log_message("Cache retained, will continue processing unfinished compounds")
  }



  all_results <- list()
  log_message('Step1: Processing compounds with backgroud database...')
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
    db_path <- system.file("extdata", "cleaned_db.fst", package = "MetaboliteAnnotator")

    name_db <- tryCatch({
      # fst::read_fst('/Users/zhonghua/Documents/Software_Development/cleaned_db.fst')
      fst::read_fst(db_path)
    }, error = function(e) {
      if (verbose) log_message(sprintf("Error loading local database: %s. Proceeding without local DB merge.", e$message), tag = "ERROR")
      return(NULL) # Return NULL or an empty df structure expected by merge
    })
    # log_message('Load backgroud database success...')
    
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
  

  step_1_results = lapply(name_list_unique,match_with_backgroud_db)
  all_mapped_dfs <- lapply(step_1_results, `[[`, "mapped_db_res")
  step1_mapped_db_res <- dplyr::bind_rows(all_mapped_dfs)
  
  
  names_for_step2 <- unlist(lapply(step_1_results, `[[`, "need_pubchem_query_name_list"), use.names = FALSE)
  
  target_cols <- c("SearchName", "CID", "IUPACName", "MolecularFormula",
                   "MolecularWeight", "KEGG_ID", "HMDB_ID", "CHEBI_ID",
                   "CTD", "Similarity")
  split_by_searchname <- split(step1_mapped_db_res, step1_mapped_db_res$SearchName)
  step1_results <- lapply(split_by_searchname, function(sub_df) {
    
    # Select only the target columns
    # Use drop = FALSE to ensure it remains a data frame even if only one row/col
    processed_df <- sub_df[, target_cols, drop = FALSE]
    
    # Convert column types to match the target 'results' structure
    # Use suppressWarnings for potential NAs introduced by coercion if CID is not numeric
    processed_df$CID <- suppressWarnings(as.integer(processed_df$CID))
    processed_df$MolecularWeight <- suppressWarnings(as.numeric(processed_df$MolecularWeight))
    # Similarity is already numeric in the example, but good practice to ensure
    processed_df$Similarity <- as.numeric(processed_df$Similarity) 
    
    # Create the final list structure for this SearchName
    list(
      all_results = processed_df,
      top_results = processed_df # Identical content as requested
    )
  })
  log_message('Step1: Complete...')

  # 变量保存并行处理结果
  step2_results <- NULL
  
  # Check and load required parallel processing packages
  if (!requireNamespace("parallel", quietly = TRUE)) {
    log_message("Package 'parallel' is required for parallel processing. Using sequential processing instead.", 
                tag = "WARNING")
    step2_results <- process_until_complete(names_for_step2, cache_dir = cache_dir, verbose = verbose)
  } else {
    # Set up parallel processing
    if (n_cores > 1) {
      log_message(sprintf("Setting up parallel processing with %d cores", n_cores))
      
      # 创建一个新的作用域来处理集群并确保正确关闭
      tryCatch({
        cl <- parallel::makeCluster(n_cores)
        on.exit({
          if(exists("cl") && inherits(cl, "cluster")) {
            tryCatch({
              parallel::stopCluster(cl)
            }, error = function(e) {
              # 记录错误但不中断
              log_message(sprintf("Error stopping cluster: %s", e$message), tag = "WARNING")
            })
          }
        }, add = TRUE)
        
        # 将必要的包加载到每个工作进程
        parallel::clusterEvalQ(cl, {
          library(dplyr)
          library(httr)
          library(jsonlite)
        })
        
        # 直接将所有需要的函数和变量导出到集群
        # 这避免了依赖于包的加载
        function_list <- c(
          "search_compounds", "process_results", "retry_GET", "get_kegg_id", 
          "string_similarity", "log_message", "clean_cache"
        )
        
        # 获取当前环境中的函数定义
        functions_env <- new.env()
        for (fn in function_list) {
          if (exists(fn, envir = .GlobalEnv)) {
            functions_env[[fn]] <- get(fn, envir = .GlobalEnv)
          } else if (exists(fn, envir = environment())) {
            functions_env[[fn]] <- get(fn, envir = environment())
          }
        }
        
        # 导出函数和必要的全局变量
        parallel::clusterExport(cl, c(names(functions_env), "cache_dir", "APP_PATHS", "verbose"), 
                          envir = environment())
        
        # 处理化合物
        log_message("Processing compounds in parallel...")
        
        # 使用并行lapply处理每个化合物
        parallel_results <- parallel::parLapply(cl, names_for_step2, function(compound) {
          tryCatch({
            # 在每个并行工作进程中创建必要的目录
            if (!dir.exists(cache_dir)) {
              dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
            }
            
            # 搜索化合物
            result <- search_compounds(compound, verbose = verbose)
            if (!is.null(result)) {
              # 处理结果
              processed <- process_results(result, target = compound, verbose = verbose)
              
              # 保存到缓存 - 直接保存processed而不添加额外嵌套
              if (!is.null(processed)) {
                result_path <- file.path(cache_dir, paste0(make.names(compound), ".rds"))
                saveRDS(processed, result_path)
              }
              
              # 直接返回processed，不添加额外嵌套
              return(processed)
            } else {
              return(NULL)
            }
          }, error = function(e) {
            # 记录错误但不中断进程
            return(list(error = e$message))
          })
        })
        
        # 先记录结果处理完成，再关闭集群
        log_message("Parallel processing complete")
        
        # 给结果命名
        names(parallel_results) <- names_for_step2
        
        # 过滤掉NULL结果和有错误的结果
        step2_results <- parallel_results[!sapply(parallel_results, function(x) {
          is.null(x) || !is.null(x$error)
        })]
        
      }, error = function(e) {
        log_message(sprintf("Error in parallel processing: %s", e$message), tag = "ERROR")
        # 如果并行处理失败，回退到顺序处理
        step2_results <- process_until_complete(names_for_step2, cache_dir = cache_dir, verbose = verbose)
      })
    } else {
      # 如果n_cores <= 1，使用顺序处理
      log_message("Using sequential processing (n_cores <= 1)")
      step2_results <- process_until_complete(names_for_step2, cache_dir = cache_dir, verbose = verbose)
    }
  }
  
  results <- c(step1_results,step2_results)

  # # 确保结果非空
  # if (is.null(results)) {
  #   log_message("No results obtained from processing, attempting sequential processing as fallback", tag = "WARNING")
  #   results <- process_until_complete(name_list_unique, cache_dir = cache_dir, verbose = verbose)
  # }

  if (!is.null(results) && length(results) > 0) {
    results_file <- file.path(process_dir, "all_results.rds")
    saveRDS(results, results_file)
    log_message(sprintf("Original results saved to: %s", results_file))

    valid_results <- Filter(function(x) !is.null(x$all_results), results)

    if (length(valid_results) > 0) {
      match_table <- create_match_status_table(valid_results)
      match_table_path <- file.path(process_dir, 'match_table.csv')
      write.csv(match_table, match_table_path, row.names = FALSE)
      log_message(sprintf("Match status table saved to: %s", match_table_path))

      processed_results_file <- file.path(process_dir, 'processed_results.rds')
      saveRDS(valid_results, processed_results_file)
      log_message(sprintf("Processed results saved to: %s", processed_results_file))
    } else {
      log_message("No successfully processed compound results", tag = "WARNING")
    }
  } else {
    log_message("Processing complete, but no results generated", tag = "WARNING")
  }

  log_message("Processing complete!", tag = "SUCCESS")
  
  return(results)
}

#' Identify Compound
#'
#' Compare a target compound against a list of potential matches using AI-based identification.
#'
#' @param compound_name Character string of the target compound name
#' @param api_config List containing API configuration (base_url, model, api_key)
#' @param results List containing compound search results
#' @param log_file Character string specifying path to log file
#'
#' @return List of chat results from AI identification
#'
#' @details
#' Uses an AI model to compare the target compound against potential matches.
#' Records comparison details and results in a log file.
#' Makes multiple attempts to ensure reliable identification.
#'
#' @keywords internal
identify_compound <- function(compound_name, api_config, results, log_file) {
  tryCatch({
    # Record the start time
    start_time <- Sys.time()

    # Check the results object
    if (is.null(results) || is.null(results[[compound_name]])) {
      stop(sprintf("Cannot find result data for compound '%s'", compound_name))
    }

    # Check all_results is exist
    if (is.null(results[[compound_name]]$all_results)) {
      stop(sprintf("all_results data missing for compound '%s'", compound_name))
    }

    # Get and check the group A compounds list
    group_a_compounds <- results[[compound_name]]$all_results$SearchName
    if (is.null(group_a_compounds) || length(group_a_compounds) == 0) {
      stop(sprintf("Group A list is empty for compound '%s'", compound_name))
    }

    # Use the user prompt from api_config if provided; otherwise, use the default prompt text.
    if (!is.null(api_config$prompt) && nchar(trimws(api_config$prompt)) > 0) {
      prompt <- paste0(
        "Target compound X: ", compound_name, "; ",
        "Group A: ", paste(group_a_compounds, collapse = "||"), "; ",
        "If there is a match, please only respond with the matched compound name from Group A (no need to explain the rules). ",
        "If no match is found, respond with FALSE. ",
        api_config$prompt
      )
    } else {
      prompt <- paste0(
        "Target compound X: ", compound_name, "; ",
        "Group A: ", paste(group_a_compounds, collapse = "||"), "; ",
        "Please act as a chemical compound comparison expert. Your task is to carefully determine if target compound X matches any compound in Group A (separated by '||') based on your knowledge base. ",
        "If there is a match, please only respond with the matched compound name from Group A (no need to explain the rules). ",
        "If no match is found, respond with FALSE. ",
        "For non-lipid compounds, the following cases should be considered the same compound: ",
        "1) Suffixes \"-ate\" and \"acid\" are equivalent (e.g., \"Methylmalonate\" and \"Methylmalonic acid\"); ",
        "2) Ignore naming variations (e.g., trans/E, allo/alpha, etc.); ",
        "3) DL/D/L configurations are equivalent; ",
        "4) Prefixes \"methyl\" or \"phosphoric acid\" are equivalent; ",
        "5) \"alpha\" equals Greek letter alpha, \"beta\" equals Greek letter beta, \"gamma\" equals Greek letter gamma, but distinguish alpha/beta/gamma forms!; ",
        "6) Tautomers are equivalent; ",
        "7) Resonance structures are equivalent; ",
        "8) Hydrates or solvates are equivalent (e.g., CuSO4 and CuSO4_5H2O); ",
        "9) Different ionization states are equivalent; ",
        "10) Different salt forms are equivalent (e.g., sodium and potassium salts); ",
        "11) Isotope variants are equivalent (unless specified); ",
        "20) Inorganic and organic forms of the same compound are equivalent. ",
        "For lipid compounds, carefully check structural differences. For example, LPC(13:0) and LPC 18:1 should be considered different compounds due to different carbon chain lengths and saturation. ",
        "Always carefully compare oxidation states and structural positions! ",
        "Prostaglandin compounds (e.g., PGE2, PGD2) should be strictly distinguished based on their core structure and functional groups!"
      )
    }

    # Record the comparison information to the log file
    comparison_header <- sprintf(
      "\n%s\n%s\n",
      paste(rep("=", 80), collapse=""),
      format(start_time, "%Y-%m-%d %H:%M:%S")
    )

    comparison_info <- sprintf(
      "Target compound: %s\nGroup A compounds list:\n%s\n",
      compound_name,
      paste("  -", group_a_compounds, collapse = "\n")
    )

    # Write the log header and comparison information
    cat(comparison_header, file = log_file, append = TRUE)
    cat(comparison_info, file = log_file, append = TRUE)

    # Execute 3 times of query
    chat_results <- list()
    for(i in 1:3) {
      # Each loop creates a new chat object, using the api_key in api_config
      chat <- chat_openai(
        base_url = api_config$base_url,
        model = api_config$model,
        api_key = api_config$api_key,
        echo = "text"
      )

      # Record the query start
      query_start <- sprintf("\nQuery #%d start time: %s\n",
                             i, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      cat(query_start, file = log_file, append = TRUE)

      # Use the chat object to query
      result <- chat$chat(prompt)
      chat_results[[i]] <- result

      # Record the query result
      query_result <- sprintf("Query #%d result: %s\n", i, result)
      cat(query_result, file = log_file, append = TRUE)
    }

    # Record the end time and summary
    end_time <- Sys.time()
    summary <- sprintf(
      "\nSummary:\nProcessing time: %.2f seconds\nThree query results:\n%s\n%s\n\n",
      as.numeric(difftime(end_time, start_time, units = "secs")),
      paste("  -", unlist(chat_results), collapse = "\n"),
      paste(rep("=", 80), collapse="")
    )
    cat(summary, file = log_file, append = TRUE)

    return(chat_results)

  }, error = function(e) {
    # Log error information
    error_msg <- sprintf(
      "\nError occurred at: %s\nError message: %s\n%s\n\n",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      e$message,
      paste(rep("=", 80), collapse="")
    )
    cat(error_msg, file = log_file, append = TRUE)
    return(NULL)
  })
}

#' Clean AI Logs
#'
#' Remove AI identification log and cache files for a specific model.
#'
#' @param process_dir Character string specifying the process directory path
#' @param model_name Character string specifying the AI model name
#'
#' @details
#' Removes both the log file of ai process
#'
#' @examples
#' \dontrun{
#' clean_ai_logs("path/to/process/dir", "deepseek")
#' }
#'
#' @export
clean_ai_logs <- function(process_dir, model_name) {
  # Build the log file path
  log_file <- file.path(process_dir, sprintf("AI_identify_compound_name_%s_log.txt", model_name))
  cache_file <- file.path(process_dir, sprintf("AI_identified_result_%s.rds", model_name))

  # Delete the existing log file if it exists
  if (file.exists(log_file)) {
    file.remove(log_file)
    log_message(sprintf("Cleared old AI identification log file: %s", basename(log_file)))
  }

  # Delete the existing cache file if it exists
  if (file.exists(cache_file)) {
    file.remove(cache_file)
    log_message(sprintf("Cleared old AI identification cache file: %s", basename(cache_file)))
  }
}

#' Process All Compounds
#'
#' Process a list of compounds using AI identification with retry capability.
#'
#' @param cpd_names Character vector of compound names to process
#' @param api_config List containing API configuration (base_url, model, api_key)
#' @param results List containing compound search results
#' @param process_dir Character string specifying directory to save results
#' @param model_name Character string specifying AI model name (default: "deepseek")
#'
#' @return List containing AI identification results for all compounds
#'
#' @details
#' Processes compounds in batches with retry capability.
#' Maintains a log of processing status and results.
#' Supports caching of intermediate results.
#'
#' @examples
#' \dontrun{
#' api_config <- list(base_url = "...", model = "...", api_key = "...")
#' results <- process_all_compounds(c("Glucose", "Fructose"), api_config,
#'                                 search_results, "path/to/process")
#' }
#'
#' @export
process_all_compounds <- function(cpd_names, api_config, results, process_dir,
                                  model_name = "deepseek") {
  # Validate the API configuration
  if (is.null(api_config$api_key)) {
    stop("API key is required but not provided")
  }

  # Add API configuration information when creating the log file (note do not record the actual API key)
  log_file <- file.path(process_dir, sprintf("AI_identify_compound_name_%s_log.txt", model_name))
  tryCatch({
    log_header <- sprintf(
      "AI Compound Identification Log\nModel: %s\nStart time: %s\nBase URL: %s\n\n",
      model_name,
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      api_config$base_url
    )

    # If the file does not exist, create a new file; if it exists, append content
    if (!file.exists(log_file)) {
      cat(log_header, file = log_file, append = FALSE)
      log_message(sprintf("Created new log file: %s", basename(log_file)))
    } else {
      cat("\n", log_header, file = log_file, append = TRUE)
      log_message(sprintf("Continued using existing log file: %s", basename(log_file)))
    }

    # Record the data status
    data_status <- sprintf(
      "Data status:\n- Number of compounds to process: %d\n-",
      length(cpd_names)
    )
    cat(data_status, file = log_file, append = TRUE)
    log_message(data_status)  # Also display in the console

  }, error = function(e) {
    message(sprintf("Cannot create/access the log file: %s - %s", log_file, e$message))
    stop(e)
  })

  # Create or read the cache file
  cache_file <- file.path(process_dir, sprintf("AI_identified_result_%s.rds", model_name))
  if (file.exists(cache_file)) {
    cached_results <- readRDS(cache_file)
  } else {
    cached_results <- list()
  }

  # Ensure the results data is correctly loaded
  results_file <- file.path(process_dir, 'all_results.rds')  # Use all_results.rds instead of processed_results.rds
  if (!file.exists(results_file)) {
    stop("Cannot find the processed results file: ", results_file)
  }

  # Read and validate the results data
  results <- readRDS(results_file)
  if (!is.list(results) || length(results) == 0) {
    stop("Invalid or empty results data")
  }

  # Check the results data structure
  log_message("Checking result data structure...")
  valid_results <- list()
  for (name in names(results)) {
    if (!is.null(results[[name]]$all_results)) {
      valid_results[[name]] <- results[[name]]
    }
  }

  if (length(valid_results) == 0) {
    stop("No valid compound results data found")
  }

  log_message(sprintf("Found %d valid compound results", length(valid_results)))
  results <- valid_results  # Use the validated results

  # Process each compound
  processed_count <- 0
  for(compound_name in cpd_names) {
    # Check if it has been processed
    if (!is.null(cached_results[[compound_name]])) {
      log_message(sprintf("Skipped %s (already processed)...", compound_name))
      next
    }

    # Check if the compound is in the results
    if (!compound_name %in% names(results)) {
      log_message(sprintf("Compound '%s' not found in results, skipping processing", compound_name), tag = "WARNING")
      next
    }

    # Check the compound data structure
    if (is.null(results[[compound_name]]$all_results)) {  # Use all_results to check
      log_message(sprintf("Compound '%s' has no valid comparison data, skipping processing", compound_name), tag = "WARNING")
      next
    }

    log_message(sprintf("Processing %s...", compound_name))
    result <- identify_compound(compound_name, api_config, results, log_file)
    if(!is.null(result)) {
      cached_results[[compound_name]] <- result
      # Save the cache in real time
      saveRDS(cached_results, cache_file)
      processed_count <- processed_count + 1
    }
  }

  log_message(sprintf("Processing complete, successfully processed %d compounds", processed_count))
  return(cached_results)
}

#' Process AI Results
#'
#' Process the results from AI compound identification.
#'
#' @param all_identified_compounds List containing AI identification results for all compounds
#' @param results List containing original compound search results
#'
#' @return Data frame containing processed results with compound matches and metadata
#'
#' @details
#' Processes AI identification results to create a final data frame containing:
#' \itemize{
#'   \item compound_name: Original compound name
#'   \item matched_name: Matched compound name from database
#'   \item final_result: TRUE/FALSE indicating successful match
#'   \item CID: PubChem Compound ID
#'   \item Various chemical identifiers and properties
#' }
#'
#' @export
process_ai_results <- function(all_identified_compounds, results) {
  # Add log
  log_message("Starting to process AI identification results...")
  log_message(sprintf("Number of compounds to process: %d", length(all_identified_compounds)))

  # Create an empty data frame to store the results
  final_results <- data.frame(
    compound_name = character(),
    matched_name = character(),
    final_result = character(),
    CID = integer(),
    IUPACName = character(),
    MolecularFormula = character(),
    MolecularWeight = numeric(),
    KEGG_ID = character(),
    HMDB_ID = character(),
    CHEBI_ID = character(),
    CTD = character(),
    Similarity = numeric(),
    stringsAsFactors = FALSE
  )

  # Process each compound
  for(compound_name in names(all_identified_compounds)) {
    log_message(sprintf("Processing compound: %s", compound_name))

    # Get the three results of the current compound and convert to lowercase
    results_list <- unlist(all_identified_compounds[[compound_name]])
    if (is.null(results_list) || length(results_list) == 0) {
      log_message(sprintf("Compound %s has no valid identification results, skipping", compound_name), tag = "WARNING")
      next
    }

    results_list <- tolower(results_list)
    log_message(sprintf("AI identification results: %s", paste(results_list, collapse = ", ")))

    # Check the results
    if (any(results_list == "false")) {
      log_message(sprintf("Compound %s not found a match", compound_name))
      matched_info <- data.frame(
        compound_name = compound_name,
        matched_name = NA,
        final_result = "FALSE",
        CID = NA_integer_,
        IUPACName = NA_character_,
        MolecularFormula = NA_character_,
        MolecularWeight = NA_real_,
        KEGG_ID = NA_character_,
        HMDB_ID = NA_character_,
        CHEBI_ID = NA_character_,
        CTD = NA_character_,
        Similarity = NA_real_,
        stringsAsFactors = FALSE
      )
    } else {
      # Check if all results are the same (ignore case)
      unique_results <- unique(results_list)
      if (length(unique_results) == 1) {
        # Get the first matching item from the original results (keep the original case)
        matched_name <- unlist(all_identified_compounds[[compound_name]])[1]
        log_message(sprintf("Found consistent match: %s", matched_name))

        if (!is.null(results[[compound_name]]$all_results)) {
          matched_rows <- results[[compound_name]]$all_results[
            tolower(results[[compound_name]]$all_results$SearchName) == tolower(matched_name), ]

          if (nrow(matched_rows) > 0) {
            matched_row <- matched_rows[1, ]
            matched_info <- data.frame(
              compound_name = compound_name,
              matched_name = matched_name,
              final_result = "TRUE",
              CID = as.integer(matched_row$CID),
              IUPACName = as.character(matched_row$IUPACName),
              MolecularFormula = as.character(matched_row$MolecularFormula),
              MolecularWeight = as.numeric(matched_row$MolecularWeight),
              KEGG_ID = as.character(matched_row$KEGG_ID),
              HMDB_ID = as.character(matched_row$HMDB_ID),
              CHEBI_ID = as.character(matched_row$CHEBI_ID),
              CTD = as.character(matched_row$CTD),
              Similarity = as.numeric(matched_row$Similarity),
              stringsAsFactors = FALSE
            )
          } else {
            log_message(sprintf("Cannot find matching compound information in results: %s", matched_name), tag = "WARNING")
            next
          }
        } else {
          log_message(sprintf("Compound %s missing all_results data", compound_name), tag = "WARNING")
          next
        }
      } else {
        log_message(sprintf("Inconsistent identification results for compound %s: %s",
                            compound_name, paste(unique_results, collapse = ", ")),
                    tag = "WARNING")
        next
      }
    }

    # Add to the final results
    final_results <- rbind(final_results, matched_info)
  }

  # Record the processing statistics
  log_message(sprintf("Total compounds processed: %d", length(names(all_identified_compounds))))
  log_message(sprintf("Successfully matched: %d", sum(final_results$final_result == "TRUE", na.rm = TRUE)))
  log_message(sprintf("Match rate: %.2f%%",
                      100 * sum(final_results$final_result == "TRUE", na.rm = TRUE) /
                        length(names(all_identified_compounds))))

  return(final_results)
}

#' Retry KEGG Request
#'
#' Make a KEGG API request with retry capability.
#'
#' @param fn Function to execute
#' @param ... Additional arguments passed to fn
#' @param max_attempts Integer specifying maximum number of retry attempts
#' @param wait_time Numeric specifying seconds to wait between attempts
#'
#' @return Result from the successful API call
#'
#' @details
#' Makes multiple attempts to execute the provided function with exponential backoff.
#' Useful for handling temporary API failures or rate limits.
#'
#' @keywords internal
retry_kegg_request <- function(fn, ..., max_attempts = 3, wait_time = 5) {
  for(i in 1:max_attempts) {
    tryCatch({
      result <- fn(...)
      return(result)
    }, error = function(e) {
      if(i < max_attempts) {
        log_message(sprintf("Attempt %d failed. Waiting %d seconds before retry...", i, wait_time))
        Sys.sleep(wait_time)
        wait_time <- wait_time * 1.5
      } else {
        stop(e)
      }
    })
  }
}

#' Process and Save Species Data
#'
#' Process and save KEGG pathway data for a specific species.
#'
#' @param species_code Character string specifying the KEGG species code
#'
#' @return Data frame containing processed pathway and compound data
#'
#' @details
#' Retrieves pathway and compound data from KEGG for the specified species.
#' Processes and saves the data to RDS files.
#' Creates a data frame linking compounds to pathways.
#'
#' @examples
#' \dontrun{
#' human_data <- process_and_save_species("hsa")
#' }
#'
#' @export
process_and_save_species <- function(species_code) {
  log_message(sprintf("Processing %s ...", species_code))

  # Create KEGG directory if it doesn't exist
  kegg_dir <- file.path(APP_PATHS$data, "KEGG")
  dir.create(kegg_dir, recursive = TRUE, showWarnings = FALSE)

  # Record the directory creation status
  log_message(sprintf("KEGG directory path: %s", kegg_dir))
  log_message(sprintf("KEGG directory exists: %s", dir.exists(kegg_dir)))
  if (!requireNamespace("KEGGREST", quietly = TRUE)) {
    stop("Package 'KEGGREST' is required but not installed. Please install it manually.")
  }
  species_path <- retry_kegg_request(keggLink, "pathway", species_code)

  # Filter meta pathways
  meta <- unique(species_path)[grepl(paste0(species_code, '00'), unique(species_path))]

  if (length(meta) == 0) {
    log_message(sprintf("No pathways found for %s. Skipping...", species_code))
    return(NULL)
  }

  # Get pathway info in batches
  batch_size <- 10
  n_batches <- ceiling(length(meta) / batch_size)
  species_info <- vector("list", length(meta))

  for(i in 1:n_batches) {
    start_idx <- (i-1) * batch_size + 1
    end_idx <- min(i * batch_size, length(meta))
    current_batch <- meta[start_idx:end_idx]

    log_message(sprintf("Processing batch %d/%d for %s", i, n_batches, species_code))

    batch_info <- lapply(current_batch, function(m) {
      Sys.sleep(0.1)  # Add small delay to avoid too rapid requests
      retry_kegg_request(keggGet, m)
    })

    species_info[start_idx:end_idx] <- batch_info
  }

  # Extract pathway names
  # nm <- unlist(lapply(species_info, function(x) x[[1]]$NAME))
  nm <- unlist(lapply(species_info, function(x) x[[1]]$NAME[1]))

  # Extract compound information
  compounds <- unlist(lapply(species_info, function(x) {
    g <- x[[1]]$COMPOUND
    if (length(g) > 0) {
      compound_names <- as.character(g)
      compound_ids <- names(g)
      combined <- paste(compound_ids, compound_names, sep=": ")
      paste(combined, collapse='; ')
    } else {
      NA
    }
  }))

  # Create compounds dataframe
  df_compounds <- data.frame(
    species = species_code,
    pathway_id = meta,
    pathway_name = nm,
    compounds = compounds,
    stringsAsFactors = FALSE
  )

  # Process compounds data to final format
  final_compounds_df <- lapply(1:nrow(df_compounds), function(i) {
    if (!is.na(df_compounds$compounds[i])) {
      compounds_list <- strsplit(df_compounds$compounds[i], "; ")[[1]]
      compound_pairs <- do.call(rbind, strsplit(compounds_list, ": "))
      data.frame(
        compound_KEGG_ID = compound_pairs[,1],
        compound_name = compound_pairs[,2],
        pathway_name = df_compounds$pathway_name[i],
        pathway_id = df_compounds$pathway_id[i],
        species = df_compounds$species[i],
        stringsAsFactors = FALSE
      )
    }
  }) %>% do.call(rbind, .)

  # Ensure the directory exists before saving the results
  compounds_file <- file.path(kegg_dir, paste0('KEGG_pathways_compound_', species_code, '.rds'))
  log_message(sprintf("Saving KEGG data to: %s", compounds_file))

  kegg_organism_last_update_time_path <- file.path(kegg_dir, paste0('kegg_organism_last_update_time_', species_code,'.rds'))
  kegg_organism_last_update_time <- Sys.Date()
  saveRDS(kegg_organism_last_update_time,kegg_organism_last_update_time_path)

  tryCatch({
    saveRDS(final_compounds_df, compounds_file)
    log_message(sprintf("Successfully saved KEGG data for %s", species_code))

    # Check if the file is saved successfully
    if (file.exists(compounds_file)) {
      file_size <- file.size(compounds_file)
      log_message(sprintf("File saved successfully. Size: %d bytes", file_size))
    } else {
      log_message("Warning: File was not saved properly", tag = "WARNING")
    }

  }, error = function(e) {
    log_message(sprintf("Error saving KEGG data: %s", e$message), tag = "ERROR")
    stop(sprintf("Failed to save KEGG data: %s", e$message))
  })

  log_message(sprintf("Completed processing %s. Processed %d compounds.",
                      species_code,
                      nrow(final_compounds_df)))

  return(final_compounds_df)
}

#' Check Species Cache
#'
#' Check if cached KEGG data exists for a species.
#'
#' @param species_code Character string specifying the KEGG species code
#'
#' @return Logical indicating whether valid cached data exists
#'
#' @details
#' Checks for existence of species-specific KEGG data cache file.
#' Verifies that the cache file can be read successfully.
#'
#' @keywords internal
check_species_cache <- function(species_code) {
  compounds_file <- file.path(APP_PATHS$data, "KEGG",
                              paste0('KEGG_pathways_compound_', species_code, '.rds'))

  if (file.exists(compounds_file)) {
    tryCatch({
      readRDS(compounds_file)
      return(TRUE)
    }, error = function(e) {
      log_message(sprintf("Cache files for %s exist but are corrupted. Will reprocess.",
                          species_code))
      return(FALSE)
    })
  }
  return(FALSE)
}

#' Perform Pathway Enrichment Analysis
#'
#' Perform pathway enrichment analysis on a set of KEGG compound IDs.
#'
#' @param compound_kegg_ids Character vector of KEGG compound IDs
#' @param pathway_data Data frame containing pathway and compound mapping data
#' @param analysis_level Character string specifying analysis level: "pathway_name" or "pathway_class"
#'
#' @return Data frame containing enrichment analysis results:
#' \itemize{
#'   \item pathway_name: Name of the pathway
#'   \item pathway_id: KEGG pathway ID
#'   \item compound_count: Total compounds in pathway
#'   \item matched_compounds: Number of input compounds matched
#'   \item matched_compound_ids: IDs of matched compounds
#'   \item p_value: Raw p-value from hypergeometric test
#'   \item adjusted_p_value: Benjamini-Hochberg adjusted p-value
#'   \item match_percentage: Percentage of pathway compounds matched
#'   \item neg_log10_p: -log10 of adjusted p-value
#' }
#'
#' @details
#' Performs hypergeometric test to identify significantly enriched pathways.
#' Supports analysis at both pathway and pathway class levels.
#' Handles data loading and validation.
#'
#' @export
perform_pathway_enrichment <- function(compound_kegg_ids, pathway_data, analysis_level = "pathway_name") {
  # Ensure the data is loaded successfully
  if (is.null(pathway_data)) {
    # Try to reload the data
    kegg_dir <- file.path(APP_PATHS$data, "KEGG")
    compounds_file <- file.path(kegg_dir, paste0('KEGG_pathways_compound_hsa.rds'))

    log_message(sprintf("Attempting to load KEGG data from: %s", compounds_file))

    if (!file.exists(compounds_file)) {
      stop(sprintf("KEGG data file not found at: %s", compounds_file))
    }

    tryCatch({
      pathway_data <- readRDS(compounds_file)
      log_message("Successfully loaded KEGG data from file")
    }, error = function(e) {
      stop(sprintf("Failed to load KEGG data: %s", e$message))
    })
  }

  if (nrow(pathway_data) == 0) {
    stop("Pathway data is empty")
  }

  log_message("Checking pathway data structure...")
  log_message(sprintf("Pathway data columns: %s", paste(colnames(pathway_data), collapse = ", ")))

  compound_kegg_ids <- gsub("^C", "", compound_kegg_ids)
  compound_kegg_ids <- compound_kegg_ids[compound_kegg_ids != "Not_Access"]
  pathway_data$compound_KEGG_ID <- gsub("^C", "", pathway_data$compound_KEGG_ID)

  pathway_data$pathway_class <- sapply(pathway_data$pathway_name, get_pathway_class)

  if (analysis_level == "pathway_class") {
    unique_pathways <- data.frame(
      pathway_name = unique(pathway_data$pathway_class),
      pathway_id = unique(pathway_data$pathway_class)
    )
    log_message(sprintf("Performing analysis at pathway class level (%d classes)", nrow(unique_pathways)))
  } else {
    unique_pathways <- unique(pathway_data[, c("pathway_name", "pathway_id")])
    log_message(sprintf("Performing analysis at pathway name level (%d pathways)", nrow(unique_pathways)))
  }

  total_compounds <- length(unique(pathway_data$compound_KEGG_ID))
  input_compound_count <- length(compound_kegg_ids)

  log_message(sprintf("Total KEGG compounds: %d", total_compounds))
  log_message(sprintf("Input compounds with KEGG ID: %d", input_compound_count))

  enrichment_results <- data.frame(
    pathway_name = character(),
    pathway_id = character(),
    compound_count = integer(),
    matched_compounds = integer(),
    matched_compound_ids = character(),
    p_value = numeric(),
    adjusted_p_value = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(unique_pathways)) {
    if (analysis_level == "pathway_class") {
      pathway_compounds <- unique(pathway_data$compound_KEGG_ID[
        pathway_data$pathway_class == unique_pathways$pathway_name[i]
      ])
    } else {
      pathway_compounds <- unique(pathway_data$compound_KEGG_ID[
        pathway_data$pathway_name == unique_pathways$pathway_name[i]
      ])
    }

    pathway_compound_count <- length(pathway_compounds)
    matched_compounds <- compound_kegg_ids[compound_kegg_ids %in% pathway_compounds]
    matches <- length(matched_compounds)

    if (matches > 0) {
      p_value <- phyper(
        matches - 1,
        pathway_compound_count,
        total_compounds - pathway_compound_count,
        input_compound_count,
        lower.tail = FALSE
      )

      matched_compounds_str <- paste0("C", matched_compounds, collapse = ";")

      enrichment_results <- rbind(enrichment_results, data.frame(
        pathway_name = unique_pathways$pathway_name[i],
        pathway_id = unique_pathways$pathway_id[i],
        compound_count = pathway_compound_count,
        matched_compounds = matches,
        matched_compound_ids = matched_compounds_str,
        p_value = p_value,
        adjusted_p_value = NA,
        stringsAsFactors = FALSE
      ))
    }
  }

  if (nrow(enrichment_results) > 0) {
    if (analysis_level == "pathway_name") {
      enrichment_results$pathway_class <- sapply(enrichment_results$pathway_name, get_pathway_class)
    } else {
      enrichment_results$pathway_class <- enrichment_results$pathway_name
    }

    enrichment_results$adjusted_p_value <- p.adjust(
      enrichment_results$p_value,
      method = "BH"
    )

    enrichment_results <- enrichment_results[ order(enrichment_results$adjusted_p_value), ]

    enrichment_results$match_percentage <- round(enrichment_results$matched_compounds / enrichment_results$compound_count * 100, 2)
    enrichment_results$neg_log10_p <- -log10(enrichment_results$adjusted_p_value)
  }

  return(enrichment_results)
}

#' Create Pathway Bubble Plot
#'
#' Create a bubble plot visualization of pathway enrichment results.
#'
#' @param enrichment_results Data frame containing pathway enrichment results
#'
#' @return A ggplot2 object containing the bubble plot visualization
#'
#' @details
#' Creates a bubble plot where:
#' \itemize{
#'   \item y-axis shows pathway names
#'   \item x-axis shows match percentage
#'   \item point size represents number of matched compounds
#'   \item point color represents significance (-log10 adjusted p-value)
#' }
#' Results are faceted by pathway class.
#'
#' @import ggplot2
#' @export
create_pathway_bubble_plot <- function(enrichment_results) {
  if (nrow(enrichment_results) == 0) {
    return(NULL)
  }

  plot_data <- enrichment_results
  plot_data$pathway_name <- sub(" - .*$", "", plot_data$pathway_name)
  p <- ggplot(plot_data,
              aes(x = match_percentage,
                  y = reorder(pathway_name, match_percentage))) +
    # Add the grid lines
    geom_vline(xintercept = seq(2, 8, 2), linetype = "dashed", color = "grey90") +
    # The main point layer
    geom_point(aes(size = matched_compounds,
                   color = neg_log10_p)) +
    # Optimize the color scheme
    scale_color_gradient2(
      low = "dodgerblue2",
      mid = "purple",
      high = "red2",
      midpoint = mean(plot_data$neg_log10_p)
    ) +
    # Adjust the size range of the points
    scale_size_continuous(range = c(3, 10)) +
    # The facet setting
    facet_grid(pathway_class ~ ., scales = "free_y", space = "free_y") +
    # The label setting
    labs(x = "Matched compounds (%)",
         y = NULL,  # Remove the y-axis label, as it is already obvious
         color = "-log10(adj.P)",
         size = "Compound count",
         title = "Pathway Analysis Results") +
    # The theme setting
    theme_minimal() +
    theme(
      # The text setting
      axis.text.y = element_text(size = 9, color = "black"),
      axis.text.x = element_text(size = 9, color = "black"),
      strip.text.y = element_text(angle = 0, face = "bold", size = 10),
      # The grid line setting
      panel.grid.major.y = element_line(colour = "grey90", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      # The facet setting
      strip.background = element_rect(fill = "grey95", color = NA),
      # The legend setting
      legend.position = "right",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      # The margin setting
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )

  return(p)
}

#' Get Pathway Class
#'
#' Determine the metabolic pathway class for a given pathway name.
#'
#' @param pathway_name Character string containing the pathway name
#'
#' @return Character string indicating the pathway class
#'
#' @details
#' Classifies pathways into major metabolic categories:
#' \itemize{
#'   \item Global and overview maps
#'   \item Carbohydrate metabolism
#'   \item Energy metabolism
#'   \item Lipid metabolism
#'   \item Nucleotide metabolism
#'   \item Amino acid metabolism
#'   \item And others...
#' }
#' Returns "Other" if pathway doesn't match known categories.
#'
#' @keywords internal
get_pathway_class <- function(pathway_name) {
  # Remove species-specific suffix, e.g., "- Homo sapiens (human)"
  clean_name <- sub(" - .*$", "", pathway_name)

  # Global and overview maps
  global_overview_maps <- c(
    "Metabolic pathways",
    "Biosynthesis of secondary metabolites",
    "Microbial metabolism in diverse environments",
    "Carbon metabolism",
    "2-Oxocarboxylic acid metabolism",
    "Fatty acid metabolism",
    "Biosynthesis of amino acids",
    "Nucleotide metabolism",
    "Biosynthesis of nucleotide sugars",
    "Biosynthesis of cofactors",
    "Degradation of aromatic compounds",
    "Nitrogen cycle"
  )

  # Carbohydrate metabolism
  carbohydrate_metabolism <- c(
    "Glycolysis / Gluconeogenesis",
    "Citrate cycle (TCA cycle)",
    "Pentose phosphate pathway",
    "Pentose and glucuronate interconversions",
    "Fructose and mannose metabolism",
    "Galactose metabolism",
    "Ascorbate and aldarate metabolism",
    "Starch and sucrose metabolism",
    "Amino sugar and nucleotide sugar metabolism",
    "Biosynthesis of various nucleotide sugars",
    "Pyruvate metabolism",
    "Glyoxylate and dicarboxylate metabolism",
    "Propanoate metabolism",
    "Butanoate metabolism",
    "C5-Branched dibasic acid metabolism",
    "Inositol phosphate metabolism"
  )

  # Energy metabolism
  energy_metabolism <- c(
    "Oxidative phosphorylation",
    "Photosynthesis",
    "Photosynthesis - antenna proteins",
    "Carbon fixation by Calvin cycle",
    "Other carbon fixation pathways",
    "Methane metabolism",
    "Nitrogen metabolism",
    "Sulfur metabolism"
  )

  # Lipid metabolism
  lipid_metabolism <- c(
    "Fatty acid biosynthesis",
    "Fatty acid elongation",
    "Fatty acid degradation",
    "Cutin, suberine and wax biosynthesis",
    "Mycolic acid biosynthesis",
    "Steroid biosynthesis",
    "Primary bile acid biosynthesis",
    "Secondary bile acid biosynthesis",
    "Steroid hormone biosynthesis",
    "Glycerolipid metabolism",
    "Glycerophospholipid metabolism",
    "Ether lipid metabolism",
    "Sphingolipid metabolism",
    "Arachidonic acid metabolism",
    "Linoleic acid metabolism",
    "alpha-Linolenic acid metabolism",
    "Biosynthesis of unsaturated fatty acids"
  )

  # Nucleotide metabolism
  nucleotide_metabolism <- c(
    "Purine metabolism",
    "Pyrimidine metabolism"
  )

  # Amino acid metabolism
  amino_acid_metabolism <- c(
    "Alanine, aspartate and glutamate metabolism",
    "Glycine, serine and threonine metabolism",
    "Cysteine and methionine metabolism",
    "Valine, leucine and isoleucine degradation",
    "Valine, leucine and isoleucine biosynthesis",
    "Lysine biosynthesis",
    "Lysine degradation",
    "Arginine biosynthesis",
    "Arginine and proline metabolism",
    "Histidine metabolism",
    "Tyrosine metabolism",
    "Phenylalanine metabolism",
    "Tryptophan metabolism",
    "Phenylalanine, tyrosine and tryptophan biosynthesis"
  )

  # Metabolism of other amino acids
  other_amino_acid_metabolism <- c(
    "beta-Alanine metabolism",
    "Taurine and hypotaurine metabolism",
    "Phosphonate and phosphinate metabolism",
    "Selenocompound metabolism",
    "Cyanoamino acid metabolism",
    "D-Amino acid metabolism",
    "Glutathione metabolism"
  )

  # Glycan biosynthesis and metabolism
  glycan_metabolism <- c(
    "N-Glycan biosynthesis",
    "Various types of N-glycan biosynthesis",
    "Mucin type O-glycan biosynthesis",
    "Mannose type O-glycan biosynthesis",
    "Other types of O-glycan biosynthesis",
    "Glycosaminoglycan biosynthesis - chondroitin sulfate / dermatan sulfate",
    "Glycosaminoglycan biosynthesis - heparan sulfate / heparin",
    "Glycosaminoglycan biosynthesis - keratan sulfate",
    "Glycosaminoglycan degradation",
    "Glycosylphosphatidylinositol (GPI)-anchor biosynthesis",
    "Glycosphingolipid biosynthesis - lacto and neolacto series",
    "Glycosphingolipid biosynthesis - globo and isoglobo series",
    "Glycosphingolipid biosynthesis - ganglio series",
    "Other glycan degradation",
    "Lipopolysaccharide biosynthesis",
    "O-Antigen repeat unit biosynthesis",
    "Peptidoglycan biosynthesis",
    "Teichoic acid biosynthesis",
    "Lipoarabinomannan (LAM) biosynthesis",
    "Arabinogalactan biosynthesis - Mycobacterium",
    "Exopolysaccharide biosynthesis"
  )

  # Metabolism of cofactors and vitamins
  cofactors_vitamins_metabolism <- c(
    "Thiamine metabolism",
    "Riboflavin metabolism",
    "Vitamin B6 metabolism",
    "Nicotinate and nicotinamide metabolism",
    "Pantothenate and CoA biosynthesis",
    "Biotin metabolism",
    "Lipoic acid metabolism",
    "Folate biosynthesis",
    "One carbon pool by folate",
    "Retinol metabolism",
    "Porphyrin metabolism",
    "Ubiquinone and other terpenoid-quinone biosynthesis"
  )

  # Metabolism of terpenoids and polyketides
  terpenoids_polyketides_metabolism <- c(
    "Terpenoid backbone biosynthesis",
    "Monoterpenoid biosynthesis",
    "Sesquiterpenoid and triterpenoid biosynthesis",
    "Diterpenoid biosynthesis",
    "Carotenoid biosynthesis",
    "Brassinosteroid biosynthesis",
    "Insect hormone biosynthesis",
    "Zeatin biosynthesis",
    "Limonene degradation",
    "Pinene, camphor and geraniol degradation",
    "Type I polyketide structures",
    "Biosynthesis of 12-, 14- and 16-membered macrolides",
    "Biosynthesis of ansamycins",
    "Biosynthesis of enediyne antibiotics",
    "Biosynthesis of type II polyketide backbone",
    "Biosynthesis of type II polyketide products",
    "Tetracycline biosynthesis",
    "Polyketide sugar unit biosynthesis",
    "Nonribosomal peptide structures",
    "Biosynthesis of siderophore group nonribosomal peptides",
    "Biosynthesis of vancomycin group antibiotics"
  )

  # Biosynthesis of other secondary metabolites
  other_secondary_metabolites <- c(
    "Phenylpropanoid biosynthesis",
    "Stilbenoid, diarylheptanoid and gingerol biosynthesis",
    "Flavonoid biosynthesis",
    "Flavone and flavonol biosynthesis",
    "Anthocyanin biosynthesis",
    "Isoflavonoid biosynthesis",
    "Degradation of flavonoids",
    "Indole alkaloid biosynthesis",
    "Indole diterpene alkaloid biosynthesis",
    "Isoquinoline alkaloid biosynthesis",
    "Tropane, piperidine and pyridine alkaloid biosynthesis",
    "Biosynthesis of various alkaloids",
    "Caffeine metabolism",
    "Betalain biosynthesis",
    "Glucosinolate biosynthesis",
    "Benzoxazinoid biosynthesis",
    "Penicillin and cephalosporin biosynthesis",
    "Carbapenem biosynthesis",
    "Monobactam biosynthesis",
    "Clavulanic acid biosynthesis",
    "Streptomycin biosynthesis",
    "Neomycin, kanamycin and gentamicin biosynthesis",
    "Acarbose and validamycin biosynthesis",
    "Novobiocin biosynthesis",
    "Staurosporine biosynthesis",
    "Phenazine biosynthesis",
    "Prodigiosin biosynthesis",
    "Aflatoxin biosynthesis",
    "Biosynthesis of various antibiotics",
    "Biosynthesis of various plant secondary metabolites",
    "Biosynthesis of various other secondary metabolites"
  )

  # Xenobiotics biodegradation and metabolism
  xenobiotics_metabolism <- c(
    "Benzoate degradation",
    "Aminobenzoate degradation",
    "Fluorobenzoate degradation",
    "Chloroalkane and chloroalkene degradation",
    "Chlorocyclohexane and chlorobenzene degradation",
    "Toluene degradation",
    "Xylene degradation",
    "Nitrotoluene degradation",
    "Ethylbenzene degradation",
    "Styrene degradation",
    "Atrazine degradation",
    "Caprolactam degradation",
    "Bisphenol degradation",
    "Dioxin degradation",
    "Naphthalene degradation",
    "Polycyclic aromatic hydrocarbon degradation",
    "Furfural degradation",
    "Steroid degradation",
    "Metabolism of xenobiotics by cytochrome P450",
    "Drug metabolism - cytochrome P450",
    "Drug metabolism - other enzymes"
  )

  # Chemical structure transformation maps
  chemical_structure_maps <- c(
    "Overview of biosynthetic pathways",
    "Biosynthesis of plant secondary metabolites",
    "Biosynthesis of phenylpropanoids",
    "Biosynthesis of terpenoids and steroids",
    "Biosynthesis of alkaloids derived from shikimate pathway",
    "Biosynthesis of alkaloids derived from ornithine, lysine and nicotinic acid",
    "Biosynthesis of alkaloids derived from histidine and purine",
    "Biosynthesis of alkaloids derived from terpenoid and polyketide",
    "Biosynthesis of plant hormones"
  )
  # Check each category
  if(clean_name %in% global_overview_maps) return("Global and overview maps")
  if(clean_name %in% carbohydrate_metabolism) return("Carbohydrate metabolism")
  if(clean_name %in% energy_metabolism) return("Energy metabolism")
  if(clean_name %in% lipid_metabolism) return("Lipid metabolism")
  if(clean_name %in% nucleotide_metabolism) return("Nucleotide metabolism")
  if(clean_name %in% amino_acid_metabolism) return("Amino acid metabolism")
  if(clean_name %in% other_amino_acid_metabolism) return("Metabolism of other amino acids")
  if(clean_name %in% glycan_metabolism) return("Glycan biosynthesis and metabolism")
  if(clean_name %in% cofactors_vitamins_metabolism) return("Metabolism of cofactors and vitamins")
  if(clean_name %in% terpenoids_polyketides_metabolism) return("Metabolism of terpenoids and polyketides")
  if(clean_name %in% other_secondary_metabolites) return("Biosynthesis of other secondary metabolites")
  if(clean_name %in% xenobiotics_metabolism) return("Xenobiotics biodegradation and metabolism")
  if(clean_name %in% chemical_structure_maps) return("Chemical structure transformation maps")

  return("Other")
}

#' Fetch URL Content with Retry
#'
#' Fetch content from URL with retry capability and browser simulation.
#'
#' @param url Character string containing the URL to fetch
#' @param max_attempts Integer specifying maximum number of retry attempts
#' @param delay Numeric specifying seconds to wait between attempts
#'
#' @return HTML content from the URL
#'
#' @details
#' Makes multiple attempts to fetch URL content with browser simulation.
#' Handles timeouts and HTTP errors.
#'
#' @importFrom httr GET timeout add_headers
#' @importFrom rvest read_html
#' @keywords internal
# HMDB related functions
fetch_with_retry <- function(url, max_attempts = 3, delay = 2) {
  for (attempt in 1:max_attempts) {
    tryCatch({
      # Add user agent to simulate browser access
      response <- httr::GET(url,
                            timeout(10),
                            add_headers(
                              `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
                              `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
                            ))

      if (httr::status_code(response) == 200) {
        return(rvest::read_html(response))
      } else {
        warning(sprintf("[HMDB/mzcloud]Attempt %d: HTTP status code %d",
                        attempt, httr::status_code(response)))
      }
    }, error = function(e) {
      warning(sprintf("[HMDB/mzcloud]Attempt %d failed: %s", attempt, e$message))
    })

    if (attempt < max_attempts) {
      Sys.sleep(delay)
    }
  }
  stop(sprintf("[HMDB/mzcloud]Failed to fetch URL after %d attempts", max_attempts))
}

#' Check if Compound is Endogenous in HMDB
#'
#' Check if a compound is marked as endogenous in HMDB database.
#'
#' @param hmdb_id Character string containing HMDB ID
#'
#' @return Logical indicating if compound is endogenous, or NA if unable to determine
#'
#' @details
#' Queries HMDB database to determine if compound is endogenous.
#' Handles missing or invalid HMDB IDs.
#'
#' @importFrom rvest html_node html_text
#' @keywords internal
check_hmdb_endogenous <- function(hmdb_id) {
  if (is.na(hmdb_id) || hmdb_id == "" || hmdb_id == "NA"|| hmdb_id == "Not_Access") {
    return(NA)
  }

  url <- paste0("https://hmdb.ca/metabolites/", hmdb_id)

  tryCatch({
    page <- fetch_with_retry(url)
    target_node <- rvest::html_node(page, xpath = "/html/body/main/table/tbody[1]/tr[35]/td/div/ul[3]")

    if (is.na(target_node)) {
      warning(sprintf("[HMDB]Target node not found for HMDB ID: %s", hmdb_id))
      return(NA)
    }

    a_nodes <- rvest::html_nodes(target_node, xpath = ".//a")
    result_vector <- sapply(a_nodes, rvest::html_text)

    is_endogenous <- any(grepl("Endogenous", result_vector, ignore.case = TRUE))
    return(is_endogenous)

  }, error = function(e) {
    warning(sprintf("[HMDB]Error processing HMDB ID %s: %s", hmdb_id, e$message))
    return(NA)
  })
}

#' Process ID Text from mzCloud
#'
#' Process and parse compound ID text from mzCloud search results.
#'
#' @param id_text Character string containing raw ID text from mzCloud search results
#'
#' @return Data frame containing parsed compound information with columns:
#' \itemize{
#'   \item web_name: Compound name as displayed on the web page
#'   \item molecular_formula: Chemical formula of the compound
#'   \item Synonyms: Alternative names and synonyms for the compound
#'   \item reference_ID: mzCloud reference identifier
#' }
#'
#' @details
#' The function processes raw text from mzCloud search results by:
#' \itemize{
#'   \item Splitting text into lines and removing whitespace
#'   \item Filtering out unwanted information (e.g., "view more info")
#'   \item Processing compound information in groups of 4 lines:
#'     \itemize{
#'       \item Line 1: Web display name
#'       \item Line 2: Reference ID information
#'       \item Line 3: Molecular formula
#'       \item Line 4: Compound synonyms
#'     }
#'   \item Validating data completeness and structure
#' }
#'
#' The function expects the input text to follow mzCloud's standard format.
#' If the number of lines is not a multiple of 4, a warning is issued as
#' this indicates potentially missing or malformed data.
#'
#' @note
#' This is an internal function used primarily by \code{process_mzcloud_page}.
#' The format of the input text is specific to mzCloud's HTML structure.
#'
#' @examples
#' \dontrun{
#' # Example of expected input format:
#' text <- paste(
#'   "Compound Name",
#'   "ID: Reference12345",
#'   "C6H12O6",
#'   "Glucose; Dextrose; D-Glucose",
#'   sep = "\n"
#' )
#' result <- process_id_text(text)
#' }
#'
#' @keywords internal
process_id_text <- function(id_text) {
  # Split the original text into lines by the newline character
  lines <- unlist(strsplit(id_text, "\r?\n"))

  # Remove the whitespace characters at the beginning and end of each line
  lines <- trimws(lines)

  # Remove the empty lines
  lines <- lines[lines != ""]

  # remove "view more info" or "view mass spectra"
  unwanted <- c("view more info", "view mass spectra")
  lines <- lines[!tolower(lines) %in% unwanted]

  # According to observation, the information of each compound is arranged in 4 lines in order:
  #   1. web_name
  #   2. The line containing "ID: Reference####" reference_ID information
  #   3. molecular formula (e.g., C5H9NO)
  #   4. synonyms (compound synonyms, separated by semicolons)
  #
  # Check if the total number of rows is divisible by 4, otherwise some data may be missing or formatted incorrectly
  if (length(lines) %% 4 != 0) {
    warning("[mzcloud info] The number of rows remaining after cleaning is not a multiple of 4, and some data may be missing or formatted incorrectly.")
  }

  compound_count <- length(lines) %/% 4

  # Pre-allocate the data frame
  mzcloud_compound_df <- data.frame(
    web_name         = character(compound_count),
    molecular_fomula = character(compound_count),
    Synonyms           = character(compound_count),
    reference_ID     = character(compound_count),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(compound_count)) {
    base_idx <- (i - 1) * 4
    # The 1st line: web name
    mzcloud_compound_df$web_name[i] <- lines[base_idx + 1]

    # The 2nd line: ID information, the format is "ID: Reference####", extract the part after ":"
    id_line <- lines[base_idx + 2]
    mzcloud_compound_df$reference_ID[i] <- sub("^ID:\\s*", "", id_line)

    # The 3rd line: molecular formula
    mzcloud_compound_df$molecular_fomula[i] <- lines[base_idx + 3]

    # The 4th line: compound synonyms
    mzcloud_compound_df$Synonyms[i] <- lines[base_idx + 4]
  }

  return(mzcloud_compound_df)
}

#' Process mzCloud Page
#'
#' Process compound information from mzCloud search page.
#'
#' @param search_url Character string containing the mzCloud search URL
#' @param cache_dir Character string specifying cache directory path (optional)
#'
#' @return Character string containing systematic name of compound, or 'Not_Access' if not found
#'
#' @details
#' Processes mzCloud search results to find compound systematic name.
#' Handles caching of results and retries on failure.
#' Matches compounds based on exact name or synonym matches.
#'
#' @importFrom rvest html_node html_text
#' @importFrom digest digest
#' @keywords internal
# mzCloud related functions
process_mzcloud_page <- function(search_url, cache_dir = NULL,compound_name) {
  # Generate cache key from URL
  cache_key <- digest::digest(search_url, algo = "md5")

  # If cache directory is provided, try to load from cache
  if (!is.null(cache_dir)) {
    cache_file <- file.path(cache_dir, paste0("mzcloud_", cache_key, ".rds"))
    if (file.exists(cache_file)) {
      return(readRDS(cache_file))
    }
  }

  tryCatch({
    # Read search page content
    search_page <- fetch_with_retry(search_url)

    id_node <- rvest::html_node(search_page,
                                xpath = "/html/body/div/div[2]/div/div")

    if (is.na(id_node)) {
      stop("[mzcloud]ID node not found. The page structure might have changed.")
    }

    id_text <- rvest::html_text(id_node)

    mzcloud_compounds_df <- process_id_text(id_text)

    # Use sapply to calculate the similarity of each compound name
    mzcloud_compounds_df$str_similarity <- sapply(
      mzcloud_compounds_df$web_name,
      string_similarity,
      compound_name
    )

    if(any(mzcloud_compounds_df$str_similarity == 1)) {
      # Extract the reference_ID of the row with a similarity of 1
      matched_id <- mzcloud_compounds_df$reference_ID[mzcloud_compounds_df$str_similarity == 1]
      # Build detail page URL
      detail_url <- sprintf("https://www.mzcloud.org/compound/Reference/%s",
                            gsub("Reference", "", matched_id))

      # Read detail page
      detail_page <- fetch_with_retry(detail_url)

      # Get systematic name
      systematic_name <- rvest::html_node(detail_page,
                                          xpath = "/html/body/div/div[4]/div[2]/p[5]/span") %>%
        rvest::html_text()

      # Save to cache if cache directory is provided
      if (!is.null(cache_dir)) {
        saveRDS(systematic_name, cache_file)
      }

      return(systematic_name)

    } else {

      matched_ref <- NULL
      # Process each compound's Synonyms field line by line
      for(i in seq_len(nrow(mzcloud_compounds_df))) {
        # Split the synonyms information by ";" and remove the extra whitespace
        synonyms_list <- unlist(strsplit(mzcloud_compounds_df$Synonyms[i], split = "; "))
        synonyms_list <- trimws(synonyms_list)

        # Execute string_similarity for each synonym information, compare with compound_name
        sims <- sapply(synonyms_list, function(syn) string_similarity(syn, compound_name))
        if(any(sims == 1)) {
          matched_ref <- mzcloud_compounds_df$reference_ID[i]
          break
        }
      }

      if(!is.null(matched_ref)) {
        # Build detail page URL using matched reference_ID
        detail_url <- sprintf("https://www.mzcloud.org/compound/Reference/%s",
                              gsub("Reference", "", matched_ref))

        # Read detail page
        detail_page <- fetch_with_retry(detail_url)

        # Get systematic name
        systematic_name <- rvest::html_node(detail_page,
                                            xpath = "/html/body/div/div[4]/div[2]/p[5]/span") %>%
          rvest::html_text()

        # Save to cache if cache directory is provided
        if (!is.null(cache_dir)) {
          saveRDS(systematic_name, cache_file)
        }

        return(systematic_name)
      } else {
        return('Not_Access')
      }
    }

  }, error = function(e) {
    stop(sprintf("[mzcloud]Error processing page: %s", e$message))
  })
}


#' Calculate String Similarity
#'
#' Calculate similarity between two strings using longest common subsequence (LCS) algorithm.
#'
#' @param a Character string to compare
#' @param b Character string to compare
#'
#' @return Numeric value between 0 and 1 indicating similarity:
#' \itemize{
#'   \item 1: Strings are identical (ignoring case)
#'   \item 0: Strings have no characters in common
#'   \item Values between 0 and 1 indicate partial similarity
#' }
#'
#' @details
#' The function calculates string similarity using these steps:
#' \itemize{
#'   \item Converts both strings to lowercase for case-insensitive comparison
#'   \item Finds the longest common subsequence (LCS) between the strings
#'   \item Calculates similarity ratio: 2 * LCS_length / (length(a) + length(b))
#' }
#'
#' The similarity measure has these properties:
#' \itemize{
#'   \item Symmetric: similarity(a,b) = similarity(b,a)
#'   \item Normalized: result is always between 0 and 1
#'   \item Case-insensitive: "ABC" and "abc" have similarity 1
#' }
#'
#' @note
#' This function is particularly useful for:
#' \itemize{
#'   \item Fuzzy string matching
#'   \item Finding similar compound names
#'   \item Handling slight variations in text
#' }
#'
#' @examples
#' \dontrun{
#' # Exact match (case-insensitive)
#' string_similarity("glucose", "GLUCOSE")  # Returns 1
#'
#' # Partial match
#' string_similarity("glucose", "glucosamine")  # Returns ~0.8
#'
#' # No similarity
#' string_similarity("abc", "xyz")  # Returns 0
#' }
#'
#' @keywords internal
string_similarity <- function(a, b) {
  # Convert the strings to lowercase to ignore case
  a <- tolower(a)
  b <- tolower(b)

  # Calculate the length of the longest common subsequence
  lcs_length <- function(str1, str2) {
    n1 <- nchar(str1)
    n2 <- nchar(str2)
    L <- matrix(0, n1 + 1, n2 + 1)

    for (i in 1:n1) {
      for (j in 1:n2) {
        if (substr(str1, i, i) == substr(str2, j, j)) {
          L[i + 1, j + 1] <- L[i, j] + 1
        } else {
          L[i + 1, j + 1] <- max(L[i + 1, j], L[i, j + 1])
        }
      }
    }
    return(L[n1 + 1, n2 + 1])
  }

  # Calculate the similarity ratio
  matches <- lcs_length(a, b)
  total_length <- nchar(a) + nchar(b)
  return(2.0 * matches / total_length)
}

#' Make HTTP GET Request with Retry
#'
#' Make HTTP GET request with retry capability and exponential backoff.
#'
#' @param url Character string containing the URL to request
#' @param max_attempts Integer specifying maximum number of retry attempts (default: 3)
#' @param wait_time Numeric specifying initial seconds to wait between attempts (default: 2)
#'
#' @return httr response object if successful, NULL if all attempts fail
#'
#' @details
#' Makes multiple attempts to GET the URL with exponential backoff.
#' For each retry:
#' \itemize{
#'   \item Attempts to make GET request
#'   \item Checks HTTP status code (success = 200)
#'   \item Waits with increasing delay between attempts
#'   \item Returns NULL after all attempts fail
#' }
#'
#' The function implements exponential backoff by increasing the wait time
#' between retries. This helps prevent overwhelming the server and improves
#' the chances of successful requests after temporary failures.
#'
#' Common use cases:
#' \itemize{
#'   \item API requests that may experience temporary failures
#'   \item Web scraping with rate limiting
#'   \item Network operations requiring retry logic
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' response <- retry_GET("https://api.example.com/data")
#' if (!is.null(response) && httr::status_code(response) == 200) {
#'   data <- httr::content(response)
#' }
#'
#' # With custom retry parameters
#' response <- retry_GET(
#'   "https://api.example.com/data",
#'   max_attempts = 5,
#'   wait_time = 1
#' )
#' }
#'
#' @seealso
#' \code{\link[httr]{GET}} for the underlying HTTP GET function
#'
#' @importFrom httr GET status_code
#' @keywords internal
retry_GET <- function(url, max_attempts = 3, wait_time = 2) {
  for (attempt in 1:max_attempts) {
    tryCatch({
      response <- httr::GET(url)

      # Check the HTTP status code
      status <- httr::status_code(response)
      if (status == 200) {
        return(response)
      } else {
        # cat(sprintf("Attempt %d: HTTP status code %d\n", attempt, status))
        if (attempt < max_attempts) {
          # cat(sprintf("Waiting %d seconds before retry...\n", wait_time))
          Sys.sleep(wait_time)
        }
      }
    }, error = function(e) {
      # cat(sprintf("Attempt %d failed with error: %s\n", attempt, e$message))
      if (attempt < max_attempts) {
        # cat(sprintf("Waiting %d seconds before retry...\n", wait_time))
        Sys.sleep(wait_time)
      }
    })
  }

  # If all attempts fail
  # cat(sprintf("Failed to get response after %d attempts for URL: %s\n", max_attempts, url))
  return(NULL)
}

#' Search Compounds in PubChem
#'
#' Search for compound information in PubChem database using name-based search with variants.
#'
#' @param compound_name Character string of the compound name to search
#' @param verbose Logical flag indicating whether to output detailed progress messages. Defaults to FALSE.
#'
#' @return Data frame containing search results with columns:
#' \itemize{
#'   \item CID: PubChem Compound ID
#'   \item MolecularFormula: Chemical formula
#'   \item MolecularWeight: Molecular weight
#'   \item CanonicalSMILES: Canonical SMILES notation
#'   \item IsomericSMILES: Isomeric SMILES notation
#'   \item InChI: International Chemical Identifier
#'   \item InChIKey: Hashed InChI
#'   \item IUPACName: IUPAC systematic name
#'   \item KEGG_ID: KEGG compound identifier
#'   \item HMDB_ID: Human Metabolome Database identifier
#'   \item CHEBI_ID: Chemical Entities of Biological Interest identifier
#'   \item SearchName: Original search name
#'   \item Similarity: Name similarity score (0-1)
#' }
#'
#' @export
search_compounds <- function(compound_name, verbose = FALSE) {
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

  if (verbose) log_message(sprintf("Searching for compound: %s (cleaned: %s)", compound_name, original_name))
  
  # Create name variants for searching
  name_variants <- c(
    original_name,
    gsub("[,\\-]", " ", original_name),  # Replace commas and hyphens with spaces
    gsub("[,\\-]", "", original_name),   # Remove commas and hyphens completely
    gsub("\\s+", "", original_name)      # Remove all spaces
  )
  
  if (verbose) log_message(sprintf("Generated name variants: %s", paste(unique(name_variants), collapse=", ")))
  
  # Try direct search for each variant
  for (variant in unique(name_variants)) {
    search_url <- sprintf("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/%s/property/IUPACName,MolecularFormula,MolecularWeight,CanonicalSMILES,IsomericSMILES,InChI,InChIKey/JSON",
                          utils::URLencode(variant, reserved = TRUE))
    
    if (verbose) log_message(sprintf("Trying direct search for variant: %s", variant))
    direct_response <- retry_GET(search_url, max_attempts = 2, wait_time = 0.01)
    
    if (!is.null(direct_response) && httr::status_code(direct_response) == 200) {
      tryCatch({
        compound_data <- jsonlite::fromJSON(rawToChar(direct_response$content))$PropertyTable$Properties
        if (!is.null(compound_data)) {
          compound_data <- as.data.frame(compound_data, stringsAsFactors = FALSE)
          compound_data$SearchName <- original_name
          compound_data$Similarity <- 1.0  # Full match
          
          if (verbose) log_message(sprintf("Direct search successful for variant: %s", variant))
          
          # Get database link information
          db_links <- get_kegg_id(as.numeric(compound_data$CID))
          
          compound_data$KEGG_ID  <- ifelse(length(db_links$KEGG)  == 0, 'Not_Access', db_links$KEGG)
          compound_data$HMDB_ID  <- ifelse(length(db_links$HMDB)  == 0, 'Not_Access', db_links$HMDB)
          compound_data$CHEBI_ID <- ifelse(length(db_links$CHEBI) == 0, 'Not_Access', db_links$CHEBI)
          compound_data$CTD      <- ifelse(length(db_links$CTD)      == 0, 'Not_Access', db_links$CTD)
          compound_data <- compound_data %>% dplyr::select(CID, MolecularFormula, MolecularWeight,
                                                           CanonicalSMILES, IsomericSMILES, InChI, InChIKey,
                                                           IUPACName, KEGG_ID, HMDB_ID, CHEBI_ID, CTD, SearchName, Similarity)
          return(compound_data)
        }
      }, error = function(e) {
        if (verbose) log_message(sprintf("Error in direct search for %s: %s", variant, e$message), tag = "WARNING")
      })
    } else {
      if (verbose) log_message(sprintf("Direct search failed for variant: %s", variant), tag = "WARNING")
    }
  }
  
  if (verbose) log_message("Direct search failed for all variants, trying autocomplete API")
  
  # If direct search fails, use autocomplete API
  # Try autocomplete search for each variant
  for (variant in unique(name_variants)) {
    autocomplete_url <- sprintf("https://pubchem.ncbi.nlm.nih.gov/rest/autocomplete/compound/%s",
                                utils::URLencode(variant, reserved = TRUE))
    
    if (verbose) log_message(sprintf("Trying autocomplete search for variant: %s", variant))
    response <- retry_GET(autocomplete_url, max_attempts = 2, wait_time = 0.01)
    
    if (!is.null(response) && httr::status_code(response) == 200) {
      results <- tryCatch({
        jsonlite::fromJSON(rawToChar(response$content))
      }, error = function(e) {
        if (verbose) log_message(sprintf("JSON parsing error for: %s - %s", variant, e$message), tag = "WARNING")
        NULL
      })
      
      if (!is.null(results) && !is.null(results$dictionary_terms$compound)) {
        compounds <- results$dictionary_terms$compound
        
        if (verbose) log_message(sprintf("Autocomplete found %d suggestions for variant: %s", 
                           length(compounds), variant))
        
        # use the string_similarity function provided by the user to calculate similarity and sort suggestions
        similarity_scores <- sapply(compounds, function(x) string_similarity(x, original_name))
        compounds <- compounds[order(similarity_scores, decreasing = TRUE)]
        
        compounds_info <- data.frame()
        
        # Process sorted suggestions
        for (suggestion in compounds[1:min(5, length(compounds))]) {
          if (verbose) log_message(sprintf("Processing suggestion: %s", suggestion))
          
          search_url <- sprintf("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/%s/cids/JSON", 
                                utils::URLencode(suggestion, reserved = TRUE))
          
          search_response <- retry_GET(search_url, max_attempts = 2, wait_time = 0.1)
          
          if (!is.null(search_response) && httr::status_code(search_response) == 200) {
            tryCatch({
              search_data <- jsonlite::fromJSON(rawToChar(search_response$content))
              cids <- search_data$IdentifierList$CID
              
              if (verbose) log_message(sprintf("Found %d CIDs for suggestion: %s", length(cids), suggestion))
              
              for (cid in cids[1:min(3, length(cids))]) {
                if (verbose) log_message(sprintf("Processing CID: %s", cid))
                
                info_url <- sprintf("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/%s/property/IUPACName,MolecularFormula,MolecularWeight,CanonicalSMILES,IsomericSMILES,InChI,InChIKey/JSON",
                                    cid)
                info_response <- retry_GET(info_url, max_attempts = 2, wait_time = 0.1)
                
                if (!is.null(info_response) && httr::status_code(info_response) == 200) {
                  tryCatch({
                    compound_data <- jsonlite::fromJSON(rawToChar(info_response$content))$PropertyTable$Properties
                    if (!is.null(compound_data)) {
                      compound_data <- as.data.frame(compound_data, stringsAsFactors = FALSE)
                      required_cols <- c("CID", "IUPACName", "MolecularFormula", "MolecularWeight")
                      for (col in required_cols) {
                        if (!(col %in% names(compound_data))) {
                          compound_data[[col]] <- NA
                        }
                      }
                      
                      # Get database link information
                      db_links <- get_kegg_id(as.numeric(compound_data$CID))
                      
                      compound_data$KEGG_ID  <- ifelse(length(db_links$KEGG)  == 0, 'Not_Access', db_links$KEGG)
                      compound_data$HMDB_ID  <- ifelse(length(db_links$HMDB)  == 0, 'Not_Access', db_links$HMDB)
                      compound_data$CHEBI_ID <- ifelse(length(db_links$CHEBI) == 0, 'Not_Access', db_links$CHEBI)
                      compound_data$CTD      <- ifelse(length(db_links$CTD)      == 0, 'Not_Access', db_links$CTD)
                      
                      compound_data$SearchName <- suggestion
                      compound_data$Similarity <- string_similarity(suggestion, compound_name)
                      compounds_info <- rbind(compounds_info, compound_data)
                      
                      if (verbose) log_message(sprintf("Successfully processed CID: %s", cid))
                    }
                  }, error = function(e) {
                    if (verbose) log_message(sprintf("Error processing CID %s: %s", cid, e$message), tag = "WARNING")
                  })
                } else {
                  if (verbose) log_message(sprintf("Failed to get info for CID: %s", cid), tag = "WARNING")
                }
              }
            }, error = function(e) {
              if (verbose) log_message(sprintf("Error processing suggestion '%s': %s", suggestion, e$message), tag = "WARNING")
            })
          } else {
            if (verbose) log_message(sprintf("Failed to get CIDs for suggestion: %s", suggestion), tag = "WARNING")
          }
        }
        
        if (nrow(compounds_info) > 0) {
          if (verbose) log_message(sprintf("Found %d compound info entries", nrow(compounds_info)))
          compounds_info <- compounds_info[order(compounds_info$Similarity, decreasing = TRUE), ]
          compounds_info$Similarity <- round(compounds_info$Similarity, 4)
          return(compounds_info)
        }
      }
    }
  }
  
  log_message("No results found for compound", tag = "WARNING")
  return(NULL)
}

#' Process Compound Search Results
#'
#' Process and analyze compound search results from PubChem, filtering and ranking matches based on similarity.
#'
#' @param results Data frame containing compound search results from PubChem
#' @param target Character string specifying the target compound name (default: "Prostaglandin E2 (PGE2)")
#' @param threshold_diff Numeric value specifying the similarity threshold difference for filtering results (default: 0.1)
#' @param verbose Logical flag indicating whether to output detailed progress messages. Defaults to FALSE.
#'
#' @return List containing two data frames:
#' \itemize{
#'   \item all_results: All search results sorted by similarity score
#'   \item top_results: Filtered results meeting similarity threshold criteria
#' }
#'
#' @export
process_results <- function(results, target = "Prostaglandin E2 (PGE2)", threshold_diff = 0.1, verbose = FALSE) {
  tryCatch({
    # Ensure results is a data frame
    df <- as.data.frame(results)

    # Update the required columns - FIX: Added "CTD" to the list
    required_columns <- c("SearchName", "CID", "IUPACName", "MolecularFormula",
                          "MolecularWeight", "KEGG_ID", "HMDB_ID", "CHEBI_ID", "CTD", "Similarity")

    # Check if the required columns exist
    missing_columns <- base::setdiff(required_columns, names(df))

    # If the required columns are missing, add NA columns
    if (length(missing_columns) > 0) {
      for (col in missing_columns) {
        df[[col]] <- NA
      }
      if (verbose) cat(sprintf("Added missing columns for %s: %s\n", target, paste(missing_columns, collapse = ", ")))
    }

    # Ensure all numeric columns are numeric types
    if ("MolecularWeight" %in% names(df)) {
      df$MolecularWeight <- as.numeric(as.character(df$MolecularWeight))
    }
    if ("Similarity" %in% names(df)) {
      df$Similarity <- as.numeric(as.character(df$Similarity))
    }

    # Ensure the ID columns are not discarded
    df$KEGG_ID <- as.character(df$KEGG_ID)
    df$HMDB_ID <- as.character(df$HMDB_ID)
    df$CHEBI_ID <- as.character(df$CHEBI_ID)

    # Select only the required columns
    df <- df[, required_columns]

    # Sort by similarity
    df_sorted <- df[order(df$Similarity, decreasing = TRUE), ]

    # Use the global defined progress log path
    log_file <- file.path(APP_PATHS$process, "progress_log.txt")

    # Only write detailed logs if verbose is TRUE
    if (verbose) {
      # Add separators and compound name title
      cat("\n", paste(rep("-", 80), collapse = ""), "\n", file = log_file, append = TRUE)
      cat(sprintf("Compound: %s\n", target), file = log_file, append = TRUE)
      cat(paste(rep("-", 80), collapse = ""), "\n", file = log_file, append = TRUE)

      # Write detailed logs, including all IDs
      cat("\nAll search results:\n", file = log_file, append = TRUE)
      print_df <- df_sorted
      print_df$KEGG_ID <- ifelse(is.na(print_df$KEGG_ID), "Not_Access", print_df$KEGG_ID)
      print_df$HMDB_ID <- ifelse(is.na(print_df$HMDB_ID), "Not_Access", print_df$HMDB_ID)
      print_df$CHEBI_ID <- ifelse(is.na(print_df$CHEBI_ID), "Not_Access", print_df$CHEBI_ID)
      capture.output(print(print_df, row.names = FALSE), file = log_file, append = TRUE)
    }

    # Check if there are exact matches (similarity = 1)
    exact_matches <- df_sorted[df_sorted$Similarity == 1, ]

    if (nrow(exact_matches) > 0) {
      # If there are multiple exact matches, prioritize results with KEGG IDs
      kegg_matches <- exact_matches[exact_matches$KEGG_ID != "Not_Access", ]

      if (nrow(kegg_matches) > 0) {
        top_results <- kegg_matches
        if (verbose) cat("\nFound exact matches with KEGG IDs:\n", file = log_file, append = TRUE)
      } else {
        top_results <- exact_matches
        if (verbose) cat("\nFound exact matches (no KEGG IDs):\n", file = log_file, append = TRUE)
      }
    } else {
      similarities <- df_sorted$Similarity
      diffs <- c(diff(similarities), 0)
      cutoff_point <- which(diffs < -threshold_diff)[1]

      if (is.na(cutoff_point)) {
        cutoff_point <- sum(similarities >= 0.7)
      }

      if (cutoff_point == 0) {
        cutoff_point <- 1
      }

      # Get the initial result set
      initial_results <- head(df_sorted, cutoff_point)

      # Create a combined ID column for deduplication
      initial_results$combined_id <- paste(
        ifelse(initial_results$KEGG_ID == "Not_Access", "", initial_results$KEGG_ID),
        ifelse(initial_results$HMDB_ID == "Not_Access", "", initial_results$HMDB_ID),
        ifelse(initial_results$CHEBI_ID == "Not_Access", "", initial_results$CHEBI_ID)
      )

      # Group by combined_id and keep the record with the highest similarity in each group
      top_results <- initial_results %>%
        dplyr::group_by(combined_id) %>%
        dplyr::slice_max(order_by = Similarity, n = 1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-combined_id)  # Remove the temporary column

      if (verbose) cat("\nNo exact matches found, keep the unique result with the highest similarity:\n", file = log_file, append = TRUE)
    }

    # Write the filtered results to the log, ensuring all IDs are included
    if (verbose) {
      cat(sprintf("\nFiltered %d compounds:\n", nrow(top_results)), file = log_file, append = TRUE)
      print_top <- top_results
      print_top$KEGG_ID <- ifelse(is.na(print_top$KEGG_ID), "Not_Access", print_top$KEGG_ID)
      print_top$HMDB_ID <- ifelse(is.na(print_top$HMDB_ID), "Not_Access", print_top$HMDB_ID)
      print_top$CHEBI_ID <- ifelse(is.na(print_top$CHEBI_ID), "Not_Access", print_top$CHEBI_ID)
      capture.output(print(print_top, row.names = FALSE), file = log_file, append = TRUE)

      # Write the similarity analysis to the log
      cat("\nSimilarity analysis:\n", file = log_file, append = TRUE)
      for (i in 1:nrow(top_results)) {
        similarity <- top_results$Similarity[i]
        level <- if (similarity >= 1) "Exact match"
        else if (similarity >= 0.9) "Very similar"
        else if (similarity >= 0.8) "Highly similar"
        else if (similarity >= 0.7) "Medium similar"
        else "Low similarity"

        cat(sprintf("%s: %.4f (%s)\n",
                  top_results$SearchName[i],
                  similarity,
                  level),
          file = log_file, append = TRUE)
      }

      # Add the end separator
      cat("\n", paste(rep("=", 80), collapse = ""), "\n", file = log_file, append = TRUE)
    }

    return(list(all_results = df_sorted, top_results = top_results))

  }, error = function(e) {
    cat(sprintf("Error in process_results for %s: %s\n", target, e$message))
    return(NULL)
  })
}

#' Process Compound List
#'
#' Process a list of compounds using AI identification with retry capability.
#'
#' @param compound_list Character vector of compound names to process
#' @param cache_dir Character string specifying cache directory path
#'
#' @return List containing processed results for each compound
#'
#' @details
#' Processes compounds in a list using AI identification with retry capability.
#' Supports caching of intermediate results.
#'
#' @export
process_compound_list <- function(compound_list, cache_dir = NULL) {
  # Use the global defined cache directory
  if (is.null(cache_dir)) {
    cache_dir <- APP_PATHS$cache
  }

  # Create the cache directory (if it doesn't exist)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # Cache file path
  cache_file <- file.path(cache_dir, "results_cache.rds")
  progress_file <- file.path(cache_dir, "progress.txt")

  # Read the existing results
  if (file.exists(cache_file)) {
    all_results <- readRDS(cache_file)
    cat("Loading existing results from cache...\n")
  } else {
    all_results <- list()
  }

  # Read the progress file
  if (file.exists(progress_file)) {
    # Use the same progress file reading method
    progress_lines <- readLines(progress_file)
    failed_compounds <- character(0)
    completed_compounds <- character(0)

    for (line in progress_lines) {
      matches <- regmatches(line, regexec("^(.+):\\s*(\\w+)\\s*$", line))[[1]]
      if (length(matches) == 3) {
        compound <- trimws(matches[2])
        status <- trimws(matches[3])
        if (status == "Failed") {
          failed_compounds <- c(failed_compounds, compound)
        } else if (status == "Completed") {
          completed_compounds <- c(completed_compounds, compound)
        }
      }
    }

    # Clean the failed items in the cache
    all_results[failed_compounds] <- NULL

    # Keep only the successfully completed results
    all_results <- all_results[names(all_results) %in% completed_compounds]
  }

  # Clear the progress file
  file.create(progress_file)

  # Rewrite the completed records
  if (length(all_results) > 0) {
    for (compound in names(all_results)) {
      cat(sprintf("%s: Completed\n", compound), file = progress_file, append = TRUE)
    }
  }

  for (compound in compound_list) {
    if (!is.null(all_results[[compound]])) {
      cat(sprintf("Skipping %s (already processed successfully)\n", compound))
      next
    }

    cat(sprintf("\nProcessing compound: %s\n", compound))
    results <- search_compounds(compound)

    if (!is.null(results)) {
      processed <- process_results(results, target = compound)
      if (!is.null(processed)) {
        all_results[[compound]] <- processed

        # Save the cache
        saveRDS(all_results, cache_file)

        # Update the progress file
        cat(sprintf("%s: Completed\n", compound), file = progress_file, append = TRUE)
      } else {
        # Process results are NULL, record failure
        cat(sprintf("%s: Failed\n", compound), file = progress_file, append = TRUE)
      }
    } else {
      # Search results are NULL, record failure
      cat(sprintf("%s: Failed\n", compound), file = progress_file, append = TRUE)
    }
  }

  return(all_results)
}

#' Check Processing Status
#'
#' Check the processing status of compounds in a list.
#'
#' @param compound_list Character vector of compound names
#' @param cache_dir Character string specifying cache directory path
#' @param silent Logical indicating whether to suppress console output
#'
#' @return Data frame containing processing status for each compound
#'
#' @export
check_processing_status <- function(compound_list, cache_dir = "./data/cache", silent = FALSE) {
  cache_file <- file.path(cache_dir, "results_cache.rds")
  progress_file <- file.path(cache_dir, "progress.txt")

  # Initialize the status data frame
  status_df <- data.frame(
    compound = compound_list,
    status = "Not processed",
    stringsAsFactors = FALSE
  )

  # Read the cache file
  if (file.exists(cache_file)) {
    results <- readRDS(cache_file)
    # Update the status of successfully processed compounds
    status_df$status[status_df$compound %in% names(results)] <- "Completed"
  }

  # Read the progress file
  if (file.exists(progress_file)) {
    progress_lines <- readLines(progress_file)

    # Parse each line
    for (line in progress_lines) {
      # Use the regular expression to match the compound name and status
      matches <- regmatches(line, regexec("^(.+):\\s*(\\w+)\\s*$", line))[[1]]
      if (length(matches) == 3) {  # The full match will have 3 elements: the entire line, group 1 (compound), and group 2 (status)
        compound <- trimws(matches[2])
        status <- trimws(matches[3])

        # Update the status of failed compounds
        if (status == "Failed") {
          idx <- match(compound, status_df$compound)
          if (!is.na(idx)) {
            status_df$status[idx] <- "Failed"
          }
        }
      }
    }
  }

  # Print the summary only if not silent
  if (!silent) {
    log_message("\nProcessing status summary:")
    log_message(sprintf("Total compounds: %d", length(compound_list)))
    log_message(sprintf("Completed: %d", sum(status_df$status == "Completed")))
    log_message(sprintf("Failed: %d", sum(status_df$status == "Failed")))
    log_message(sprintf("Not processed: %d", sum(status_df$status == "Not processed")))
  }

  return(status_df)
}

#' Clean Cache
#'
#' Clean the cache directory.
#'
#' @param cache_dir Character string specifying cache directory path
#'
#' @return Invisible TRUE
#'
#' @details
#' Removes all .rds and .txt files from the cache directory.
#'
#' @export
clean_cache <- function(cache_dir = "./data/cache") {
  cache_file <- file.path(cache_dir, "results_cache.rds")
  progress_file <- file.path(cache_dir, "progress.txt")

  if (file.exists(cache_file)) {
    file.remove(cache_file)
  }
  if (file.exists(progress_file)) {
    file.remove(progress_file)
  }
  log_message("Cache cleaned successfully.")
  invisible(TRUE)
}

#' Create Match Status Table
#'
#' Create a match status table for all results.
#'
#' @param all_results List containing processed results for all compounds
#'
#' @return Data frame containing match status for all compounds
#'
#' @details
#' Creates a data frame containing match status for all compounds.
#'
#' @export
create_match_status_table <- function(all_results) {
  # Create an empty data frame to store the results
  match_status <- data.frame(
    compound_name = character(),
    exact_match = logical(),
    stringsAsFactors = FALSE
  )

  # Iterate through all results
  for (compound_name in names(all_results)) {
    # Check if there is an exact match (similarity = 1)
    has_exact_match <- any(all_results[[compound_name]]$all_results$Similarity == 1)

    # Add to the data frame
    match_status <- rbind(match_status,
                          data.frame(compound_name = compound_name,
                                     exact_match = has_exact_match,
                                     stringsAsFactors = FALSE))
  }

  return(match_status)
}

#' Process Until Complete
#'
#' Process a list of compounds with multiple retry attempts until all compounds are either
#' successfully processed or definitively failed.
#'
#' @param compound_list Character vector of compound names to process
#' @param cache_dir Character string specifying cache directory path (default: "./data/cache")
#' @param max_rounds Integer specifying maximum number of processing rounds (default: 3)
#' @param verbose Logical flag indicating whether to output detailed progress messages. Defaults to FALSE.
#'
#' @return List containing processed results for all compounds
#'
#' @export
process_until_complete <- function(compound_list, cache_dir = "./data/cache", max_rounds = 3, verbose = FALSE) {
  total_compounds <- length(compound_list)
  all_results <- list()  # Used to store all results

  for (round in 1:max_rounds) {
    log_message(sprintf("=== Round %d ===", round))

    # Check the current status
    status <- check_processing_status(compound_list, cache_dir, silent = TRUE)
    remaining <- sum(status$status != "Completed")

    if (remaining == 0) {
      log_message("All compounds processed successfully!")
      break
    }

    log_message(sprintf("Processing remaining compounds: %d/%d", remaining, length(compound_list)))
    
    # Only show detailed stats if verbose
    if (verbose) {
      log_message(sprintf("Processing status summary:"))
      log_message(sprintf("Total compounds: %d", length(compound_list)))
      log_message(sprintf("Completed: %d", sum(status$status == "Completed")))
      log_message(sprintf("Failed: %d", sum(status$status == "Failed")))
      log_message(sprintf("Not processed: %d", sum(status$status == "Not processed")))
    }

    # Process the compounds that are not completed
    unfinished_compounds <- compound_list[status$status != "Completed"]
    for (i in seq_along(unfinished_compounds)) {
      compound <- unfinished_compounds[i]
      
      # Only log detailed progress if verbose
      if (verbose) {
        log_message(sprintf("Processing compound (%d/%d): %s",
                      sum(status$status == "Completed") + i,
                      total_compounds,
                      compound))
      } else if (i %% 5 == 0 || i == length(unfinished_compounds)) {
        # Log progress less frequently when not verbose
        log_message(sprintf("Progress: %d/%d compounds", i, length(unfinished_compounds)))
      }
      
      tryCatch({
        if (verbose) log_message(sprintf("Starting search for compound: %s", compound))
        result <- search_compounds(compound, verbose = verbose)
        if (!is.null(result)) {
          if (verbose) log_message(sprintf("Search successful for compound: %s", compound))
          # Save the results of a single compound
          compound_result <- list(all_results = result)
          result_path <- file.path(cache_dir, paste0(make.names(compound), ".rds"))
          saveRDS(compound_result, result_path)
          if (verbose) log_message(sprintf("Saved result to: %s", result_path))
          # Add to the total results list
          all_results[[compound]] <- compound_result
        } else {
          if (verbose) log_message(sprintf("No results found for compound: %s", compound), tag = "WARNING")
        }
      }, error = function(e) {
        log_message(sprintf("Failed to process compound %s: %s", compound, e$message), tag = "ERROR")
      })
      
      # Reduce sleep delay for faster processing
      if (verbose) {
        Sys.sleep(0.1)  # Give a little time for the UI to update when verbose
      }
    }

    # Save the current results
    results_cache_path <- file.path(cache_dir, "results_cache.rds")
    saveRDS(all_results, results_cache_path)
    log_message(sprintf("Saved all results to: %s", results_cache_path))

    # If it is the last round, display the compounds that could not be processed
    if (round == max_rounds) {
      final_status <- check_processing_status(compound_list, cache_dir, silent = TRUE)
      failed_compounds <- final_status$compound[final_status$status != "Completed"]
      if (length(failed_compounds) > 0) {
        log_message("The following compounds could not be processed:", tag = "WARNING")
        # Only show the first 10 failed compounds to avoid excessive output
        show_count <- min(10, length(failed_compounds))
        for (i in 1:show_count) {
          log_message(sprintf("- %s", failed_compounds[i]), tag = "WARNING")
        }
        if (length(failed_compounds) > show_count) {
          log_message(sprintf("... and %d more", length(failed_compounds) - show_count), tag = "WARNING")
        }
      }
    }
  }

  # Return all results
  return(all_results)
}



#' Get KEGG ID
#'
#' Get KEGG ID for a given compound ID.
#'
#' @param cid Integer specifying compound ID
#'
#' @return List containing KEGG ID, HMDB ID, CHEBI ID, and CTD ID
#'
#' @details
#' Retrieves KEGG ID, HMDB ID, CHEBI ID, and CTD ID for a given compound ID.
#'
#' @export
get_kegg_id <- function(cid) {
  # Build the URL
  url <- sprintf("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/%d/xrefs/SBURL/JSON", cid)

  # Send the GET request
  response <- httr::GET(url)

  if (httr::status_code(response) == 200) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package 'jsonlite' is required but not installed. Please install it manually.")
    }

    # Parse the JSON response
    data <- jsonlite::fromJSON(rawToChar(response$content))
    # Correctly get the URLs list
    urls <- data$InformationList$Information$SBURL[[1]]

    # Extract the KEGG ID
    kegg_ids <- character(0)
    kegg_pattern <- "www_bget\\?cpd:(.*?)$"
    kegg_matches <- grep(kegg_pattern, urls, value = TRUE)
    if (length(kegg_matches) > 0) {
      kegg_ids <- gsub(".*cpd:", "", kegg_matches)
    }

    # Extract the HMDB ID
    hmdb_ids <- character(0)
    hmdb_pattern <- "metabolites/(HMDB.*?)$"
    hmdb_matches <- grep(hmdb_pattern, urls, value = TRUE)
    if (length(hmdb_matches) > 0) {
      hmdb_ids <- gsub(".*metabolites/", "", hmdb_matches)
    }

    # Extract the CHEBI ID
    chebi_ids <- character(0)
    chebi_pattern <- "CHEBI:(\\d+)"
    chebi_matches <- grep(chebi_pattern, urls, value = TRUE)
    if (length(chebi_matches) > 0) {
      chebi_ids <- gsub(".*CHEBI:", "", chebi_matches)
    }
    
    # Extract the CTD ID
    ctd_ids <- character(0)
    ctd_pattern <- "acc=([A-Za-z0-9]+)"
    ctd_matches <- grep(ctd_pattern, urls, value = TRUE)
    if (length(ctd_matches) > 0) {
      ctd_ids <- sub(".*acc=([A-Za-z0-9]+).*", "\\1", ctd_matches)
    }
    
    # Return the result list
    return(list(
      KEGG = kegg_ids,
      HMDB = hmdb_ids,
      CHEBI = chebi_ids,
      CTD = ctd_ids
    ))
  }

  return(NULL)
}

#' Update PubChem CID-Parent Data
#'
#' Download the latest PubChem CID-Parent file and update its last update time.
#' This function is used when the CID-Parent data is missing or outdated.
#'
#' @noRd
update_pubchem_data <- function() {
  pubchem_dir <- file.path(APP_PATHS$data, "pubchem_info")
  if (!dir.exists(pubchem_dir)) {
    dir.create(pubchem_dir, recursive = TRUE, showWarnings = FALSE)
    log_message(sprintf("Created directory: %s", pubchem_dir))
  }
  options(timeout = 3600)
  url <- "https://ftp.ncbi.nlm.nih.gov/pubchem/Compound/Extras/CID-Parent.gz"
  zip_dest <- file.path(pubchem_dir, "CID-Parent.gz")
  extracted_file <- file.path(pubchem_dir, "CID-Parent")  # filename after gunzip extraction
  CID_parent_fst_file_path <- file.path(pubchem_dir, "CID-Parent.fst")
  utils::download.file(url, destfile = zip_dest, mode = "wb", method = "libcurl")
  if (!file.exists(zip_dest)) {
    stop("Download failed, please check network or URL!")
  }
  if (!requireNamespace("R.utils", quietly = TRUE)) {
    stop("Package 'R.utils' is required but not installed. Please install it manually.")
  }
  decompressed_file <- gunzip(zip_dest, destname = extracted_file, overwrite = TRUE)

  if (!requireNamespace("data.table", quietly = TRUE)) {
    utils::install.packages("data.table")
  }

  if (!requireNamespace("fst", quietly = TRUE)) {
    utils::install.packages("fst")
  }

  if (!requireNamespace("fstcore", quietly = TRUE)) {
    utils::install.packages("fstcore")
  }

  dt <- data.table::fread(
    input = decompressed_file,
    sep = "\t",
    header = FALSE,
    col.names = c("CID", "Parent"),
    showProgress = TRUE
  )

  fst::write_fst(dt, CID_parent_fst_file_path)
  cid_parent_update <- Sys.Date()
  saveRDS(cid_parent_update, file = file.path(pubchem_dir, "cid_parent_lastest_update_time.rds"))
  log_message("PubChem CID-Parent data updated successfully.")
}


#' Update CTD Data
#'
#' Download the latest CTD file and update its last update time.
#' This function is used when the CTD data is missing or outdated.
#'
#' @noRd
update_CTD_data <- function() {
  ctd_dir <- file.path(APP_PATHS$data, "ctd_info")
  if (!dir.exists(ctd_dir)) {
    dir.create(ctd_dir, recursive = TRUE, showWarnings = FALSE)
    log_message(sprintf("Created directory: %s", ctd_dir))
  }
  options(timeout = 3600)
  download_ctd_data <- function(url, skip = 28, column_names = NULL) {
    # Create a temporary file to store the downloaded data
    temp_file <- tempfile(fileext = ".csv.gz")
    
    # Download the file
    utils::download.file(url, destfile = temp_file, mode = "wb")
    
    # Read the compressed file directly (R can handle gzipped files)
    if (is.null(column_names)) {
      data <- utils::read.csv(gzfile(temp_file), skip = skip)
    } else {
      data <- utils::read.csv(gzfile(temp_file), skip = skip, 
                     header = FALSE, col.names = column_names)
    }
    
    # Clean up the temporary file
    unlink(temp_file)
    
    return(data)
  }
  
  log_message("Updating Chemical-Gene data, please wait...")
  
  # process_gene_data
  chem_gene_cols <- c("ChemicalName", "ChemicalID", "CasRN", "GeneSymbol", "GeneID", 
                     "GeneForms", "Organism", "OrganismID", "Interaction", 
                     "InteractionActions", "PubMedIDs")
  
  chem_gene_data <- download_ctd_data(
    url = "http://ctdbase.org/reports/CTD_chem_gene_ixns.csv.gz",
    skip = 29,
    column_names = chem_gene_cols
  ) |> dplyr::select(-c(ChemicalName, CasRN, GeneID, OrganismID)) |> 
    dplyr::distinct(ChemicalID, GeneSymbol, GeneForms, Organism, .keep_all = TRUE) |>
    dplyr::filter(Organism %in% c("Homo sapiens", "Mus musculus")) |> 
    dplyr::group_by(ChemicalID) |>
    dplyr::mutate(priority = dplyr::if_else(Organism == "Homo sapiens", 1, 2)) |>
    dplyr::slice_min(order_by = priority, n = 1) |>
    dplyr::select(-priority)
    # 
    # dplyr::group_by(ChemicalID, GeneSymbol, Organism) |>
    # dplyr::summarise(
    #   GeneForms = paste(unique(GeneForms), collapse = "|"),
    #   Interaction = paste(unique(Interaction), collapse = "|"),
    #   InteractionActions = paste(unique(InteractionActions), collapse = "|"),
    #   PubMedIDs = paste(unique(PubMedIDs), collapse = "|"),
    #   .groups = "drop"
    # ) |>
    # dplyr::mutate(genesymbol_form = paste0(GeneSymbol, ",", GeneForms)) |>
    # dplyr::group_by(ChemicalID) |>
    # dplyr::summarise(
    #   genesymbol_form = paste(unique(genesymbol_form), collapse = ";"),
    #   Gene_Interaction = paste(unique(Interaction), collapse = ";"),
    #   Gene_InteractionActions = paste(unique(InteractionActions), collapse = ";"),
    #   Gene_PubMedIDs = paste(unique(PubMedIDs), collapse = ";"),
    #   .groups = "drop"
    # )

  # process_GO_data
  chem_go_cols <- c("ChemicalName", "ChemicalID", "CasRN", "Ontology", "GOTermName", 
                    "GOTermID", "HighestGOLevel", "PValue", "CorrectedPValue", 
                    "TargetMatchQty", "TargetTotalQty", "BackgroundMatchQty", "BackgroundTotalQty")
  
  chem_go_data <- download_ctd_data(
    url = "http://ctdbase.org/reports/CTD_chem_go_enriched.csv.gz",
    skip = 28,
    column_names = chem_go_cols
  ) 
  
  
  chem_go_data <- chem_go_data |> dplyr::select(ChemicalID, Ontology, GOTermName, GOTermID, HighestGOLevel)
  #   dplyr::mutate(GO_combined = paste0(GOTermID, ":", GOTermName)) |> 
  #   dplyr::filter(Ontology %in% c("Biological Process", "Cellular Component", "Molecular Function"))
  # 
  # 
  # process_ontology <- function(data, ont_type, col_name) {
  #   data |>
  #     dplyr::filter(Ontology == ont_type) |>
  #     dplyr::select(ChemicalID, term = GO_combined) |>
  #     dplyr::group_by(ChemicalID) |>
  #     dplyr::summarise(!!col_name := paste(term, collapse = ";"), .groups = "drop")
  # }
  # 
  # bp_summary <- process_ontology(chem_go_data, "Biological Process", "GO_BP")
  # cc_summary <- process_ontology(chem_go_data, "Cellular Component", "GO_CC")
  # mf_summary <- process_ontology(chem_go_data, "Molecular Function", "GO_MF")
  # 
  # 
  # chem_go_summary <- bp_summary |>
  #   dplyr::full_join(cc_summary, by = "ChemicalID") |>
  #   dplyr::full_join(mf_summary, by = "ChemicalID") |>
  #   dplyr::mutate(across(starts_with("GO_"), ~ifelse(is.na(.x), "", .x)))
  
  #process_phenotype_data
  chem_phenotype_cols <- c("ChemicalName", "ChemicalID", "CasRN", "phenotypename", "phenotypeid", 
                           "comentionedterms", "organism", "organismid", "interaction", 
                           "interactions", "anatomyterms", "inferencegenesymbols", "pubmedids",'')
  
  chem_phenotype_data <- download_ctd_data(
    url = "https://ctdbase.org/reports/CTD_pheno_term_ixns.csv.gz",
    skip = 29,
    column_names = chem_phenotype_cols
  ) |> dplyr::select(ChemicalID,phenotypename,organism,interaction,interactions,pubmedids) |> 
    dplyr::distinct(ChemicalID, phenotypename, organism, .keep_all = TRUE) %>%
    dplyr::filter(organism %in% c("Homo sapiens", "Mus musculus")) |> 
    dplyr::group_by(ChemicalID) |>
    dplyr::mutate(priority = dplyr::if_else(organism == "Homo sapiens", 1, 2)) |>
    dplyr::slice_min(order_by = priority, n = 1) |>
    dplyr::select(-priority)
    # 
    # dplyr::group_by(ChemicalID, organism) %>%
    # dplyr::summarise(
    #   phenotypename = paste(unique(phenotypename), collapse = "|"),
    #   phenotype_interaction = paste(unique(interaction), collapse = "|"),
    #   phenotype_interactions = paste(unique(interactions), collapse = "|"),
    #   phenotype_pubmedids = paste(unique(pubmedids), collapse = "|"),
    #   .groups = "drop"
    # )
  
  fst::write_fst(chem_gene_data, file.path(ctd_dir, "chem-gene_data.fst"))
  fst::write_fst(chem_phenotype_data, file.path(ctd_dir, "chem-phenotype_data.fst"))
  fst::write_fst(chem_go_data, file.path(ctd_dir, "chem-go_data.fst"))
  
  ctd_update <- Sys.Date()
  saveRDS(ctd_update, file = file.path(ctd_dir, "ctd_lastest_update_time.rds"))
  log_message("CTD data updated successfully.")
}


update_reactome_data <- function() {
  reactome_dir <- file.path(APP_PATHS$data, "reactome_info")
  if (!dir.exists(reactome_dir)) {
    dir.create(reactome_dir, recursive = TRUE, showWarnings = FALSE)
    log_message(sprintf("Created directory: %s", reactome_dir))
  }

  get_latest_chebi_version <- function() {
  # ChEBI archive URL
  chebi_url <- "https://ftp.ebi.ac.uk/pub/databases/chebi/archive/"
  
  # read the webpage
  page <- xml2::read_html(chebi_url)
  
  # extract all links
  links <- rvest::html_nodes(page, "a")
  link_texts <- rvest::html_text(links)
  
  # filter out the directory links starting with "rel"
  rel_dirs <- link_texts[grepl("^rel[0-9]+", link_texts)]
  
  # if no rel directories are found, return NULL
  if (length(rel_dirs) == 0) {
    return(NULL)
  }
  
  # extract the numeric part
  rel_nums <- as.numeric(gsub("rel([0-9]+).*", "\\1", rel_dirs))
  
  # get the maximum version number
  latest_version <- max(rel_nums, na.rm = TRUE)
  
  return(latest_version)
}

file_path <- paste0('https://ftp.ebi.ac.uk/pub/databases/chebi/archive/rel',get_latest_chebi_version(),'/ontology/chebi_lite.obo')
download_chebi_file <- function(output_file = "chebi_lite.txt") {
  utils::download.file(url = file_path, 
                       destfile = output_file, 
                       mode = "wb")
  message(paste0("chebi file have been download and save as ", output_file))
}
download_chebi_file(file.path(reactome_dir, "chebi_lite.txt"))

lines <- readLines(file.path(reactome_dir, "chebi_lite.txt"))
terms_list <- list()

# mark the current term block
current_term <- NULL
current_data <- list()

# iterate each line
for (line in lines) {
  # skip empty lines
  if (trimws(line) == "") next
  
  # if encounter a new [Term], save the previous term and initialize a new one
  if (startsWith(trimws(line), "[Term]")) {
    if (!is.null(current_term)) {
      terms_list[[length(terms_list) + 1]] <- current_data
    }
    current_term <- length(terms_list) + 1
    current_data <- list()
    next
  }
  
  # parse non-empty lines, extract fields and values
  if (trimws(line) != "[Term]") {
    parts <- strsplit(trimws(line), ":")[[1]]
    if (length(parts) >= 2) {  # ensure there are fields and values
      field <- trimws(parts[1])
      value <- trimws(paste(parts[2:length(parts)], collapse = ":"))
      
      # if the field already exists, append the value (e.g., alt_id and is_a may have multiple values)
      if (field %in% c("alt_id", "is_a")) {
        if (is.null(current_data[[field]])) {
          current_data[[field]] <- value
        } else {
          current_data[[field]] <- paste(current_data[[field]], value, sep = ", ")
        }
      } else {
        current_data[[field]] <- value
      }
    }
  }
}

# save the last term
if (!is.null(current_term)) {
  terms_list[[length(terms_list) + 1]] <- current_data
}

# convert the list to a dataframe
# first get all possible column names
all_columns <- unique(c("id", "name", "alt_id", "subset", "def", "is_a", 
                        "relationship", "is_obsolete", "is_cyclic", "is_transitive", 
                        "xref", "inverse_of"))

# create a dataframe row for each term, fill missing values with NA
df <- as.data.frame(do.call(rbind, lapply(terms_list, function(x) {
  temp <- rep(NA, length(all_columns))
  names(temp) <- all_columns
  for (name in names(x)) {
    temp[name] <- if (length(x[[name]]) > 1 || grepl(",", x[[name]])) {
      paste(unique(trimws(strsplit(x[[name]], ",\\s*")[[1]])), collapse = ", ")  # ensure unique values and format
    } else {
      x[[name]]
    }
  }
  return(temp)
})), stringsAsFactors = FALSE)


# clean the data: ensure id and alt_id format consistent
df_cleaned <- df |>
  dplyr::mutate(
    id = trimws(id),  # remove extra spaces
    alt_id = trimws(alt_id),  # remove extra spaces
    alt_id = gsub("^\"|\"$", "", alt_id),  # remove possible quotes
    alt_id = gsub("\\s+", " ", alt_id),  # normalize spaces
    id = gsub("CHEBI:", "", id),  # remove "CHEBI:" from id
    alt_id = gsub("CHEBI:", "", alt_id)  # remove "CHEBI:" from alt_id
  )

# split alt_id column into a list (each alt_id separated by comma)
df_expanded <- df_cleaned |>
  tidyr::separate_rows(alt_id, sep = ",\\s*") |>
  dplyr::filter(!is.na(alt_id))  # remove empty alt_id

# create a correspondence table
# 1. id to alt_id mapping
id_alt_mapping <- df_expanded |>
  dplyr::select(id, alt_id) |>
  dplyr::rename(from_id = id, to_id = alt_id)

# 2. ensure alt_id can also be mapped back to id (bidirectional mapping)
# first get all unique alt_id, ensure they are valid
unique_alt_ids <- unique(df_expanded$alt_id)

# create a reverse mapping (alt_id to id)
alt_id_mapping <- df_expanded |>
  dplyr::select(alt_id, id) |>
  dplyr::rename(from_id = alt_id, to_id = id) |>
  dplyr::filter(from_id %in% unique_alt_ids)  # only keep valid alt_id

# merge two directions of mapping, form a complete correspondence table
correspondence_table <- dplyr::bind_rows(id_alt_mapping, alt_id_mapping) |>
  dplyr::distinct()  # remove duplicate rows

# sort by from_id for easier viewing
correspondence_table <- correspondence_table |>
  dplyr::arrange(from_id, to_id)

# Create a filtered correspondence table with unique from_id values
unique_correspondence <- correspondence_table |>
  dplyr::group_by(from_id) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup()

# install and load readr package (if not installed)
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

dat <- readr::read_tsv("https://reactome.org/download/current/ChEBI2ReactomeReactions.txt", 
                       col_names = FALSE, 
                       locale = readr::locale(encoding = "UTF-8"))

# manually set column names
colnames(dat) <- c("CHEBI_ID", "Reactome_ID", "Reactome_Link", "Reaction", "Class", "Species")
reactome_dat <- dat |> 
  dplyr::filter(Species %in% c("Homo sapiens", "Mus musculus")) |> 
  dplyr::group_by(CHEBI_ID) |>
  dplyr::mutate(priority = dplyr::if_else(Species == "Homo sapiens", 1, 2)) |>
  dplyr::slice_min(order_by = priority, n = 1) |>
  dplyr::select(-priority) |> 
  dplyr::mutate(CHEBI_ID = as.character(CHEBI_ID))

# Then create a separate mapping table
chebi_mapping <- reactome_dat |>
  dplyr::select(CHEBI_ID) |>
  dplyr::distinct() |>
  dplyr::left_join(correspondence_table, by = c('CHEBI_ID' = 'from_id')) |> 
  stats::na.omit()

# Create a dataframe for the new entries
new_entries <- reactome_dat |>
  dplyr::inner_join(chebi_mapping, by = "CHEBI_ID") |>
  dplyr::mutate(
    Original_CHEBI_ID = CHEBI_ID,
    CHEBI_ID = to_id
  ) |>
  dplyr::select(-to_id)

# Combine with original data
expanded_reactome_dat <- dplyr::bind_rows(
  reactome_dat,
  new_entries
) |>
  dplyr::distinct(CHEBI_ID, Reactome_ID, .keep_all = TRUE) |> 
  dplyr::select(CHEBI_ID,Reactome_ID, Reactome_Link, Reaction,Original_CHEBI_ID)

  # dplyr::mutate(Reactome_ID_Reaction = paste0(Reactome_ID, ":", Reaction)) |>
  # dplyr::group_by(CHEBI_ID) |>
  # dplyr::summarise(
  #   # Combine Reactome_ID_Reaction with comma separator
  #   Reactome_ID_Reaction = paste(Reactome_ID_Reaction, collapse = ";"),
  #   # Combine unique Reactome_Link with comma separator
  #   Reactome_Link = paste(unique(Reactome_Link), collapse = ";"),
  #   # Combine unique Original_CHEBI_ID with comma separator
  #   Original_CHEBI_ID = paste(unique(stats::na.omit(Original_CHEBI_ID)), collapse = ";"),
  #   .groups = "drop"
  # )

fst::write_fst(expanded_reactome_dat, file.path(reactome_dir, "reactome_chebi_mapping.fst"))

reactome_update <- Sys.Date()
saveRDS(reactome_update, file = file.path(reactome_dir, "reactome_lastest_update_time.rds"))
log_message("Reactome data updated successfully.")
}
