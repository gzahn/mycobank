#' Get the Mycobank taxonomic classifications for a vector of taxa names
#'
#' Parses through the Mycobank database to return taxonomic classifications for a list of genera and/or genus sp. names
#' Names should be in a character vector and in the format "genus species" or just "genus" (variants and strains accepted)
#' If a given taxonomic name query consists of only one 'word' then taxonomic level of 'genus' is assumed
#'
#'
#' @import stringr
#' @import magrittr
#' @import purrr
#' @import dplyr
#'
#' @param taxa Character. Vector of taxa names to look up.
#' @param mycobank_db Data frame. Object name for Mycobank database. Typically created by get_mycobank_db()
#'
#' @return Character vector. Taxonomic ranks separated by commas in each element.
#'
#' @details
#' This will search through the database using pattern matching to pull the various basionyms, and synonyms for a given taxon name, if present. Returns NA for that taxon otherwise. The 'tidy_results' element of the returned list contains a 'long-form' tibble with all supported synonym types. Taxa with "sp." in their name will return genus-level synonyms only.
#'
#' @examples
#' db <- get_mycobank_db(overwrite = FALSE)
#' taxa <- c("Abaphospora borealis","Conisphaeria borealis var. minor","Sphaeria borealis","Nonsense name","Abaphospora sp.")
#' get_mycobank_taxonomy(taxa,db)
#'
#' @export

get_mycobank_taxonomy <- function(taxa,mycobank_db){

  '%ni%' <- Negate('%in%')

  taxa <- taxa %>% str_to_sentence()
  if(any(duplicated(taxa))){
    taxa <- unique(taxa)
    warning("Duplicated taxa query names removed.")
  }

  taxa2 <- taxa %>% str_remove(" sp.") %>% unique()

  if(length(taxa) != length(taxa2)){
    warning("Duplicate taxa names removed. Only returning unique results. Did you provide, for example: 'Abaphospora' & 'Abaphospora sp.'? This would only return results for the genus 'Abaphospora'")
  }

  # remove any spaces from mycobank column names
  names(mycobank_db) <- mycobank_db %>% names %>% str_replace_all(" ","_")

  classification_list <- c()

  for(name in taxa2){

    if(name %ni% mycobank_db$Taxon_name){x <- NA}

    if(name %in% mycobank_db$Taxon_name){
      x <- mycobank_db %>%
        dplyr::filter(Taxon_name %in% name & Name_status == "Legitimate") %>%
        pluck("Classification")}

    classification_list[name] <- x
  }
  return(classification_list)
}





