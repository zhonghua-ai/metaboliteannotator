library(tidyverse)

mona <- fst::read_fst('cleaned_results/processed_mona_compounds.fst')

mzmine <- fst::read_fst('cleaned_results/combined_mzmine_results.fst')

msdial <- fst::read_fst('cleaned_results/processed_msdial.fst')

vaniya_fiehb <- fst::read_fst('cleaned_results/processed_vaniya_fiehn_compounds.fst')

massbank <- fst::read_fst('cleaned_results/processed_massbank_compounds.fst')

lipidblast <- fst::read_fst('cleaned_results/processed_lipidblast_name_inchikey_smiles.fst')


# Add source column to each dataset
mona <- mona %>% dplyr::mutate(source = "mona") |> dplyr::select(name, inchikey,smiles, pubchem_cid,source)
mzmine <- mzmine %>% dplyr::mutate(source = "mzmine",pubchem_cid = NA) |> dplyr::select(name, inchikey,smiles, pubchem_cid, source)
msdial <- msdial %>% dplyr::mutate(source = "msdial",pubchem_cid = NA) |> dplyr::select(name, inchikey,smiles, pubchem_cid, source)
vaniya_fiehb <- vaniya_fiehb %>% dplyr::mutate(source = "vaniya_fiehn") |> dplyr::select(name, inchikey,smiles, pubchem_cid, source)
massbank <- massbank %>% dplyr::mutate(source = "massbank") |> dplyr::select(name, inchikey,smiles, pubchem_cid, source)
lipidblast <- lipidblast %>% dplyr::mutate(source = "lipidblast",pubchem_cid = NA) |> dplyr::select(name, inchikey,smiles, pubchem_cid, source)


# Combine all datasets
combined_data <- dplyr::bind_rows(mona, mzmine, msdial, vaniya_fiehb, massbank, lipidblast) %>%
  dplyr::group_by(name, inchikey,smiles) %>%
  dplyr::slice(1) |> 
  dplyr::mutate(hmdb_id = NA,
         kegg_id = NA,
         chebi_id = NA,
         pubchem_cid = as.character(pubchem_cid)) |> 
  dplyr::select(name,inchikey,smiles, pubchem_cid,hmdb_id,kegg_id,chebi_id, source) 


refmet_data <- read_csv("cleaned_results/refmet_data.csv") |> 
  dplyr::select(refmet_name, pubchem_cid, formula, inchi_key, super_class, main_class, 
                sub_class, hmdb_id,kegg_id,chebi_id,lipidmaps_id) |> dplyr::rename(name = refmet_name) |> 
  dplyr::mutate(source = "refmet")

refmet_data_simplified <- refmet_data |> 
  dplyr::mutate(smiles = NA,
                pubchem_cid = as.character(pubchem_cid)) |>  
  dplyr::select(name,inchi_key,smiles, pubchem_cid,hmdb_id,kegg_id,chebi_id, source) |> 
  dplyr::rename(inchikey = inchi_key)

integrated_data <- dplyr::bind_rows(combined_data, refmet_data_simplified) |> 
  dplyr::group_by(name, inchikey) |>
  dplyr::mutate(completeness = rowSums(!is.na(dplyr::across(c(smiles, pubchem_cid, hmdb_id, kegg_id, chebi_id))))) |>
  dplyr::arrange(dplyr::desc(completeness)) |>
  dplyr::slice(1) |>
  dplyr::select(-completeness)


library(stringr)
library(tidyverse)
pattern_quotes_only <- '^"|"$'
dat <- integrated_data
dat$name3 <- str_replace_all(dat$name, pattern_quotes_only, "")

clean_compound_name <- function(x) {
  # Remove "Unknown"
  x <- gsub("^Unknown$", "", x)
  
  # Remove "w/o MS2:" prefix
  x <- gsub("^w/o MS2:", "", x)
  
  # Remove "; " and everything after it (this will handle all semicolon cases)
  x <- gsub("; .*$", "", x)
  
  # Trim any leading/trailing whitespace
  x <- trimws(x)
  
  return(x)
}
dat$cleaned_Metabolite_name <- sapply(dat$name3, clean_compound_name)
dat_inchikey_no_na <- dat |> dplyr::filter(!is.na(inchikey))
dat_filtered <- dat_inchikey_no_na %>%
  dplyr::filter(!is.na(cleaned_Metabolite_name) & (cleaned_Metabolite_name != ""))

if ("cleaned_Metabolite_name" %in% names(dat_filtered) && is.character(dat_filtered$cleaned_Metabolite_name)) {
  dat_filtered$cleaned_Metabolite_name_lc <- tolower(dat_filtered$cleaned_Metabolite_name)
  cat("'cleaned_Metabolite_name_lc' have been created。\n")
} else {
  stop("'cleaned_Metabolite_name' no existed")
}

dat_filtered_lc <- dat_filtered %>%
  filter(!is.na(cleaned_Metabolite_name_lc) & cleaned_Metabolite_name_lc != "")

deduplicated_dat <- dat_filtered_lc %>%
  group_by(cleaned_Metabolite_name_lc) %>% 
  arrange(
    desc(!is.na(pubchem_cid)),
    desc(!is.na(inchikey)),
    desc(!is.na(smiles))
  ) %>%
  slice_head(n = 1) %>% 
  ungroup() 


## export the cleaned_Metabolite_name_lc, update the Pubchem CID with https://pubchem.ncbi.nlm.nih.gov/idexchange/idexchange.cgi

fst::write_fst(deduplicated_dat |> dplyr::select(name,cleaned_Metabolite_name_lc,pubchem_cid, inchikey, smiles),
               'cleaned_db.fst')

