library(tidyverse)

mona <- fst::read_fst('/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/cleaned_results/processed_mona_compounds.fst')

mzmine <- fst::read_fst('/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/cleaned_results/combined_mzmine_results.fst')

msdial <- fst::read_fst('/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/cleaned_results/processed_msdial.fst')

vaniya_fiehb <- fst::read_fst('/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/cleaned_results/processed_vaniya_fiehn_compounds.fst')

massbank <- fst::read_fst('/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/cleaned_results/processed_massbank_compounds.fst')

lipidblast <- fst::read_fst('/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/cleaned_results/processed_lipidblast_name_inchikey_smiles.fst')


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


refmet_data <- read_csv("/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/cleaned_results/refmet_data.csv") |> 
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

# Write the deduplicated data to a new CSV file
fst::write_fst(integrated_data, 'msdial_Expert/cleaned_results/integrated_database_mzmine_msdial_mona_refmet_vaniyafiehn_massbank_lipidblast.fst')


library(stringr)
# dat <- fst::read.fst('/Users/zhonghua/Documents/Shimazu_Test/msdial_Expert/cleaned_results/integrated_database_mzmine_msdial_mona_refmet_vaniyafiehn_massbank_lipidblast.fst')
library(tidyverse)
# 新模式：只匹配开头或结尾的双引号
pattern_quotes_only <- '^"|"$'
dat <- integrated_data

# 应用这个更简单的模式到原始 `name` 列
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
# Apply cleaning to test$Title
dat$cleaned_Metabolite_name <- sapply(dat$name3, clean_compound_name)



dat_inchikey_no_na <- dat |> dplyr::filter(!is.na(inchikey))


dat_filtered <- dat_inchikey_no_na %>%
  dplyr::filter(!is.na(cleaned_Metabolite_name) & (cleaned_Metabolite_name != ""))

if ("cleaned_Metabolite_name" %in% names(dat_filtered) && is.character(dat_filtered$cleaned_Metabolite_name)) {
  dat_filtered$cleaned_Metabolite_name_lc <- tolower(dat_filtered$cleaned_Metabolite_name)
  cat("新列 'cleaned_Metabolite_name_lc' 已创建。\n")
} else {
  stop("错误: 'cleaned_Metabolite_name' 列不存在或不是字符类型。请先运行之前的清洗步骤。")
}

# --- 可选: 过滤掉新列中的 NA 或空字符串 ---
# 这样做可以避免将所有NA或空字符串视为一个需要去重的组
dat_filtered_lc <- dat_filtered %>%
  filter(!is.na(cleaned_Metabolite_name_lc) & cleaned_Metabolite_name_lc != "")

# 应用去重逻辑
deduplicated_dat <- dat_filtered_lc %>%
  group_by(cleaned_Metabolite_name_lc) %>% # 按清理后的名称分组
  arrange(
    # 优先级排序：
    # 1. pubchem_cid 不是 NA 的排在前面 (desc(TRUE) > desc(FALSE))
    desc(!is.na(pubchem_cid)),
    # 2. 如果 pubchem_cid 状态相同 (都是 NA 或都不是 NA), inchikey 不是 NA 的排在前面
    desc(!is.na(inchikey)),
    # 3. 如果 pubchem_cid 和 inchikey 状态都相同，smiles 不是 NA 的排在前面
    desc(!is.na(smiles))
    # arrange 默认是稳定的，如果所有优先级都相同，原始顺序会被保留，slice_head 会取第一个
  ) %>%
  slice_head(n = 1) %>% # 每个组只保留排序后的第一行
  ungroup() # 去除分组，方便后续操作

fst::write_fst(deduplicated_dat |> dplyr::select(name,cleaned_Metabolite_name_lc,pubchem_cid, inchikey, smiles),
               '/Users/zhonghua/Documents/Software_Development/cleaned_db.fst')
# 
# 
# # a <- fst::read.fst('/Users/zhonghua/Documents/Software_Development/cleaned_db.fst')
# cid_inchikey <- data.table::fread('/Users/zhonghua/Downloads/CID-InChI-Key')
# names(cid_inchikey) <- c('CID','InChI','inchikey')
# 
# deduplicated_dat_cid <- merge(deduplicated_dat,cid_inchikey,by = 'inchikey',all.x = T)
