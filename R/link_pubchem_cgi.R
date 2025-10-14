if (!requireNamespace("httr", quietly = TRUE)) {
  stop("Package 'httr' is required. Install it with install.packages('httr').", call. = FALSE)
}

library(httr)

create_exchange <- function() {
  env <- new.env(parent = emptyenv())
  env$base_url <- "https://pubchem.ncbi.nlm.nih.gov/idexchange/"
  env$handle <- httr::handle(env$base_url)
  env$user_agent <- httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36")
  env$ssl_config <- httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L)
  env
}

submit_synonyms_for_cids <- function(exchange, synonyms) {
  input_text <- paste0(trimws(synonyms[nzchar(trimws(synonyms))]), collapse = "\n")
  
  form_data <- list(
    idstr = input_text,
    idinput = "str",
    inputtype = "synofiltered",
    inputdsn = "",
    operatortype = "samecid",
    outputtype = "cid",
    outputdsn = "",
    method = "file-pair",
    compression = "none",
    submitjob = "Submit Job"
  )
  
  message(sprintf("Submitting %d synonyms for CID lookup...", length(synonyms)))
  
  response <- tryCatch(
    httr::POST(
      url = paste0(exchange$base_url, "idexchange.cgi"),
      handle = exchange$handle,
      body = form_data,
      encode = "form",
      exchange$user_agent,
      exchange$ssl_config,
      httr::timeout(60)
    ),
    error = function(e) stop(sprintf("Request failed: %s", conditionMessage(e)))
  )
  
  httr::stop_for_status(response)
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  message(sprintf("Response status: %s", httr::status_code(response)))
  message(sprintf("Response URL: %s", response$url %||% paste0(exchange$base_url, "idexchange.cgi")))
  
  if (is_direct_result(content)) {
    message("Got direct CID results")
    return(content)
  }
  
  download_url <- extract_download_url(content, exchange$base_url)
  if (!is.null(download_url)) {
    message(sprintf("Found download URL: %s", download_url))
    return(download_url)
  }
  
  job_id <- extract_job_id(content)
  if (!is.null(job_id)) {
    message(sprintf("Job submitted with ID: %s", job_id))
    return(wait_for_job(exchange, job_id))
  }
  
  refresh_url <- extract_refresh_url(content, exchange$base_url)
  if (!is.null(refresh_url)) {
    message(sprintf("Following refresh URL: %s", refresh_url))
    Sys.sleep(3)
    return(follow_redirect(exchange, refresh_url))
  }
  
  if (is_processing_page(content)) {
    message("Job appears to be processing...")
    max_retries <- 20
    for (retry in seq_len(max_retries)) {
      wait_time <- min(15, 4 + retry)
      Sys.sleep(wait_time)
      message(sprintf("Checking job status... (attempt %d/%d, waited %ds)", retry, max_retries, wait_time))
      
      check <- tryCatch(
        httr::GET(
          url = response$url %||% paste0(exchange$base_url, "idexchange.cgi"),
          handle = exchange$handle,
          exchange$user_agent,
          exchange$ssl_config,
          httr::timeout(60)
        ),
        error = function(e) {
          message(sprintf("Request error during status check: %s", conditionMessage(e)))
          NULL
        }
      )
      
      if (is.null(check)) next
      
      httr::stop_for_status(check)
      check_content <- httr::content(check, as = "text", encoding = "UTF-8")
      
      if (is_direct_result(check_content)) {
        message("Got direct results!")
        return(check_content)
      }
      
      download_url <- extract_download_url(check_content, exchange$base_url)
      if (!is.null(download_url)) {
        message(sprintf("Found download URL after waiting: %s", download_url))
        return(download_url)
      }
      
      if (grepl("completed|finished", check_content, ignore.case = TRUE)) {
        content <- check_content
        break
      }
      
      if (!is_processing_page(check_content)) {
        message("Job no longer shows processing status")
        content <- check_content
        break
      }
    }
  }
  
  writeLines(content, con = "debug_response.html", useBytes = TRUE)
  message("Checking for immediate results in response...")
  if (nzchar(input_text) && grepl(substr(input_text, 1, min(10, nchar(input_text))), content, fixed = TRUE)) {
    message("Found input synonyms in response - checking for results...")
  }
  
  stop("Could not find job ID, download link, or direct results in response. Check debug_response.html")
}

is_direct_result <- function(content) {
  if (!nzchar(content)) return(FALSE)
  lines <- strsplit(content, "\\r?\\n")[[1]]
  length(lines) > 1 && !grepl("<html|<body|<head", content, ignore.case = TRUE)
}

extract_download_url <- function(content, base_url) {
  pattern <- 'href="([^"]*(?:\\.txt|\\.tsv|\\.csv|\\.dat)[^"]*)"'
  match <- regexpr(pattern, content, perl = TRUE, ignore.case = TRUE)
  if (match[1] == -1) return(NULL)
  url <- regmatches(content, match)[1]
  url <- sub('^href="', "", url)
  url <- sub('"$', "", url)
  if (!startsWith(url, "http")) {
    url <- paste0(base_url, url)
  }
  url
}

extract_job_id <- function(content) {
  pattern <- 'job[_\\s]*(?:id|number)["\\s]*[:=]["\\s]*([A-Za-z0-9_-]+)'
  match <- regexpr(pattern, content, perl = TRUE, ignore.case = TRUE)
  if (match[1] == -1) return(NULL)
  sub(pattern, "\\1", regmatches(content, match)[[1]], perl = TRUE)
}

extract_refresh_url <- function(content, base_url) {
  pattern <- '<meta[^>]*refresh[^>]*url=([^">]*)'
  match <- regexpr(pattern, content, perl = TRUE, ignore.case = TRUE)
  if (match[1] == -1) return(NULL)
  url <- sub(pattern, "\\1", regmatches(content, match)[[1]], perl = TRUE)
  if (!startsWith(url, "http")) {
    url <- paste0(base_url, url)
  }
  url
}

is_processing_page <- function(content) {
  grepl("processing|queued|running", content, ignore.case = TRUE)
}

follow_redirect <- function(exchange, url, max_redirects = 10) {
  current_url <- url
  for (i in seq_len(max_redirects)) {
    response <- tryCatch(
      httr::GET(
        url = current_url,
        handle = exchange$handle,
        exchange$user_agent,
        exchange$ssl_config,
        httr::timeout(30)
      ),
      error = function(e) NULL
    )
    if (is.null(response)) {
      Sys.sleep(2)
      next
    }
    
    httr::stop_for_status(response)
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    
    if (is_direct_result(content)) return(content)
    
    download_url <- extract_download_url(content, exchange$base_url)
    if (!is.null(download_url)) return(download_url)
    
    refresh_url <- extract_refresh_url(content, exchange$base_url)
    if (is.null(refresh_url)) return(content)
    
    current_url <- refresh_url
    Sys.sleep(2)
  }
  content
}

wait_for_job <- function(exchange, job_id, max_wait = 300) {
  start_time <- Sys.time()
  status_url <- paste0(exchange$base_url, "idexchange.cgi?job_id=", job_id)
  
  repeat {
    if (as.numeric(difftime(Sys.time(), start_time, units = "secs")) > max_wait) {
      stop(sprintf("Job %s did not complete within %d seconds", job_id, max_wait))
    }
    
    response <- tryCatch(
      httr::GET(
        url = status_url,
        handle = exchange$handle,
        exchange$user_agent,
        exchange$ssl_config,
        httr::timeout(300)
      ),
      error = function(e) NULL
    )
    
    if (is.null(response)) {
      Sys.sleep(5)
      next
    }
    
    httr::stop_for_status(response)
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    
    download_url <- extract_download_url(content, exchange$base_url)
    if (!is.null(download_url)) return(download_url)
    
    if (is_processing_page(content)) {
      Sys.sleep(5)
      next
    }
    
    if (grepl("error|failed", content, ignore.case = TRUE)) {
      stop("Job failed or encountered an error")
    }
    
    if (grepl("cid", content, ignore.case = TRUE) && length(strsplit(content, "\\r?\\n")[[1]]) > 2) {
      return(content)
    }
    
    Sys.sleep(5)
  }
}

download_results <- function(exchange, url_or_content) {
  if (startsWith(url_or_content, "http")) {
    response <- httr::GET(
      url_or_content,
      handle = exchange$handle,
      exchange$user_agent,
      exchange$ssl_config,
      httr::timeout(30)
    )
    httr::stop_for_status(response)
    httr::content(response, as = "text", encoding = "UTF-8")
  } else {
    url_or_content
  }
}

get_cids_from_synonyms <- function(synonyms) {
  exchange <- create_exchange()
  result <- submit_synonyms_for_cids(exchange, synonyms)
  download_results(exchange, result)
}

read_synonyms_from_file <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(sprintf("Input file '%s' not found.", file_path))
  }
  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  lines <- trimws(lines)
  lines[nzchar(lines)]
}

process_synonyms_batch <- function(synonyms, batch_size = 500, output_dir = NULL, prefix = "batch") {
  results <- list()
  exchange <- create_exchange()
  
  message(sprintf("Processing %d synonyms in batches of %d...", length(synonyms), batch_size))
  
  total_batches <- ceiling(length(synonyms) / batch_size)
  for (i in seq(1, length(synonyms), by = batch_size)) {
    batch <- synonyms[i:min(i + batch_size - 1, length(synonyms))]
    batch_num <- ceiling(i / batch_size)
    message(sprintf("Processing batch %d/%d (%d entries)...", batch_num, total_batches, length(batch)))
    
    max_retries <- 3
    batch_success <- FALSE
    
    for (attempt in seq_len(max_retries)) {
      if (attempt > 1) {
        message(sprintf("Retrying batch %d, attempt %d/%d", batch_num, attempt, max_retries))
        Sys.sleep(10)
      }
      
      result <- tryCatch(
        {
          result_url <- submit_synonyms_for_cids(exchange, batch)
          download_results(exchange, result_url)
        },
        error = function(e) {
          message(sprintf("Error processing batch %d, attempt %d: %s", batch_num, attempt, conditionMessage(e)))
          NULL
        }
      )
      
      if (is.null(result)) next
      
      if (!is.null(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
        batch_file <- file.path(output_dir, sprintf("%s_%03d.txt", prefix, batch_num))
        writeLines(result, batch_file, useBytes = TRUE)
      }
      
      parsed <- parse_results(result)
      if (length(parsed) > 0) {
        for (synonym in names(parsed)) {
          existing <- results[[synonym]]
          new_values <- unique(parsed[[synonym]])
          if (is.null(existing)) {
            results[[synonym]] <- new_values
          } else {
            results[[synonym]] <- unique(c(existing, new_values))
          }
        }
      }
      
      message(sprintf("Batch %d completed successfully. Found %d results.", batch_num, sum(lengths(parsed))))
      batch_success <- TRUE
      break
    }
    
    if (batch_success && (i + batch_size - 1) < length(synonyms)) {
      message("Waiting 10 seconds before next batch...")
      Sys.sleep(10)
    }
    
    if (!batch_success) {
      message(sprintf("Batch %d completely failed. Continuing with next batch...", batch_num))
    }
  }
  
  results
}

parse_results <- function(result_content) {
  if (!nzchar(result_content)) return(list())
  
  con <- textConnection(result_content)
  on.exit(close(con), add = TRUE)
  
  df <- tryCatch(
    utils::read.table(con, sep = "\t", header = FALSE, quote = "", comment.char = "", stringsAsFactors = FALSE, fill = TRUE),
    error = function(e) NULL
  )
  
  if (is.null(df) || nrow(df) == 0 || ncol(df) < 2) return(list())
  
  df[[1]] <- trimws(as.character(df[[1]]))
  df[[2]] <- trimws(as.character(df[[2]]))
  
  if (nrow(df) > 0 && tolower(df[[1]][1]) == "synonym" && tolower(df[[2]][1]) == "cid") {
    df <- df[-1, , drop = FALSE]
  }
  
  df <- df[nzchar(df[[1]]) & nzchar(df[[2]]), , drop = FALSE]
  if (nrow(df) == 0) return(list())
  
  parsed <- list()
  for (i in seq_len(nrow(df))) {
    synonym <- df[[1]][i]
    cid_tokens <- strsplit(df[[2]][i], "[;,]")[[1]]
    cid_tokens <- trimws(cid_tokens)
    cid_tokens <- cid_tokens[nzchar(cid_tokens)]
    if (length(cid_tokens) == 0) next
    parsed[[synonym]] <- unique(c(parsed[[synonym]], cid_tokens))
  }
  parsed
}

save_results_to_csv <- function(results, output_file) {
  if (length(results) == 0) {
    message("No results to save.")
    return(invisible(NULL))
  }
  
  synonyms <- sort(names(results))
  cid_strings <- vapply(
    synonyms,
    function(s) paste(sort(unique(results[[s]])), collapse = ";"),
    character(1),
    USE.NAMES = FALSE
  )
  
  df <- data.frame(Synonym = synonyms, CIDs = cid_strings, stringsAsFactors = FALSE)
  names(df)[2] <- "CID(s)"
  utils::write.csv(df, file = output_file, row.names = FALSE, fileEncoding = "UTF-8")
  message(sprintf("Results saved to %s", output_file))
}

load_existing_results <- function(output_file) {
  if (!file.exists(output_file)) {
    return(list())
  }
  
  df <- tryCatch(
    utils::read.csv(output_file, stringsAsFactors = FALSE, encoding = "UTF-8"),
    error = function(e) {
      message(sprintf("Error loading existing results: %s", conditionMessage(e)))
      NULL
    }
  )
  
  if (is.null(df) || nrow(df) == 0) return(list())
  
  names(df) <- trimws(names(df))
  syn_col <- which(tolower(names(df)) == "synonym")
  cid_col <- which(grepl("cid", tolower(names(df))))
  if (length(syn_col) == 0 || length(cid_col) == 0) return(list())
  
  results <- list()
  for (i in seq_len(nrow(df))) {
    synonym <- trimws(df[i, syn_col])
    cids <- trimws(strsplit(df[i, cid_col], ";")[[1]])
    cids <- cids[nzchar(cids)]
    if (!nzchar(synonym) || length(cids) == 0) next
    results[[synonym]] <- unique(cids)
  }
  message(sprintf("Loaded %d existing results from %s", length(results), output_file))
  results
}

main <- function() {
  input_file <- "synonym_input.txt"
  output_file <- "synonym_cid_results.csv"
  batch_size <- 500
  
  if (!file.exists(input_file)) {
    message(sprintf("Error: Input file '%s' not found.", input_file))
    return(invisible(NULL))
  }
  
  message("Reading synonyms from file...")
  synonyms <- read_synonyms_from_file(input_file)
  message(sprintf("Found %d synonyms to process.", length(synonyms)))
  
  if (length(synonyms) == 0) {
    message("No valid synonyms found in input file.")
    return(invisible(NULL))
  }
  
  existing_results <- load_existing_results(output_file)
  remaining_synonyms <- synonyms[!(synonyms %in% names(existing_results))]
  message(sprintf("Already processed: %d synonyms", length(existing_results)))
  message(sprintf("Remaining to process: %d synonyms", length(remaining_synonyms)))
  
  if (length(remaining_synonyms) == 0) {
    message("All synonyms have already been processed!")
    return(invisible(NULL))
  }
  
  message(sprintf("Starting batch processing with batch size %d...", batch_size))
  new_results <- process_synonyms_batch(remaining_synonyms, batch_size)
  
  all_results <- existing_results
  for (synonym in names(new_results)) {
    if (is.null(all_results[[synonym]])) {
      all_results[[synonym]] <- new_results[[synonym]]
    } else {
      all_results[[synonym]] <- unique(c(all_results[[synonym]], new_results[[synonym]]))
    }
  }
  
  message(sprintf("Processing complete. Total results: %d synonyms", length(all_results)))
  message(sprintf("New results in this session: %d synonyms", length(new_results)))
  
  if (length(all_results) > 0) {
    save_results_to_csv(all_results, output_file)
    
    failed_synonyms <- setdiff(synonyms, names(all_results))
    if (length(failed_synonyms) > 0) {
      message(sprintf("Warning: %d synonyms could not be converted.", length(failed_synonyms)))
      preview <- head(sort(failed_synonyms), 10)
      message(sprintf("Failed synonyms: %s%s",
                      paste(preview, collapse = ", "),
                      if (length(failed_synonyms) > 10) ", ..." else ""))
    }
  } else {
    message("No results obtained. Please check your internet connection and try again.")
  }
  
  invisible(all_results)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

if (sys.nframe() == 0) {
  main()
}
