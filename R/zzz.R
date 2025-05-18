#' Common Imports for zhonghuapackage
#'
#' This file registers common package dependencies so that functions from these
#' packages become available throughout the package. It also declares specific imports
#' for some functions from stats and utils.
#'
#' @name zzz_imports
#'
#' @import shiny
#' @import shinyjs
#' @import shinycssloaders
#' @import ellmer
#' @import KEGGREST
#' @import ggplot2
#' @import rmarkdown
#' @import rstudioapi
#' @import httr
#' @import knitr
#'
#' @importFrom stats p.adjust phyper reorder setNames
#' @importFrom utils URLencode capture.output flush.console head read.csv
#'     setTxtProgressBar txtProgressBar write.csv
#' @importFrom DT DTOutput renderDataTable datatable formatStyle
#' @importFrom R.utils gunzip
#' @importFrom fst fst
#' @importFrom magrittr "%>%"
#' @importFrom data.table fread
#' @importFrom digest digest
#' @importFrom DT JS
#' @importFrom eoffice topptx
#' @importFrom dplyr group_by select slice_max ungroup
#' @importFrom httr GET add_headers status_code timeout
#' @importFrom jsonlite fromJSON
#' @importFrom rvest html_node html_text read_html
#' @importFrom stringdist stringdist
#' @importFrom knitr knit
#' @importFrom stringr str_replace_all
#' @importFrom ggthemes theme_gdocs
#' @importFrom ggthemes theme_tufte
#' @importFrom ggthemes theme_solarized
#' @importFrom ggthemes theme_few
#' @importFrom ggthemes theme_stata
#' @importFrom ggthemes theme_wsj
#' @importFrom ggthemes theme_igray
#' @importFrom ggthemes theme_economist
#' @importFrom ggthemes theme_hc
#' @importFrom ggthemes theme_map
#' @importFrom ggthemes theme_pander
#' @importFrom ggthemes theme_excel
#' @importFrom ggthemes theme_excel_new
#' @importFrom ggthemes theme_wsj
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "CID", "CHEBI_ID", "CanonicalSMILES", "HMDB_ID", "IUPACName",
    "InChI", "InChIKey", "IsomericSMILES", "KEGG_ID", "MolecularFormula",
    "MolecularWeight", "SearchName", "Similarity", "combined_id", "exact_match",
    "match_percentage", "matched_compounds", "neg_log10_p", "pathway_name",
    "pubchem_script", ".",
    "APP_PATHS", "JS", "download.file", "install.packages", "styleEqual", "styleInterval"
  ))
}

.onLoad <- function(libname, pkgname) {
  # 确保www目录中的资源可以被访问
  shiny::addResourcePath("custom", system.file("www", package = pkgname))

  if (requireNamespace("conflicted", quietly = TRUE)) {
    conflicted::conflict_prefer("runExample", "shiny")
    conflicted::conflict_prefer("renderDataTable", "DT")
  }
}