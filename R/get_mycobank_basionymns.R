#' Get the basionyms from Mycobank for a vector of taxa names
#'
#' Parses through the Mycobank synonyms and taxon names to pull the taxon basionym for a vector of query taxon names
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
#' @return Character vector. This returns a vector of the "basio_names" for your query taxa.
#'
#' @details
#' This is a shortcut function that makes use of get_mycobank_synonyms() to just pull the "basio_name".
#'
#' @examples
#' db <- get_mycobank_db(overwrite = FALSE)
#' taxa <- c("Abaphospora borealis","Conisphaeria borealis var. minor","Sphaeria borealis","Nonsense name","Abaphospora")
#' get_mycobank_basionyms(taxa,db)
#'
#' @export

get_mycobank_basionymns <- function(taxa,mycobank_db){

  stopifnot(class(taxa) == "character")
  stopifnot("data.frame" %in% class(mycobank_db))

  x <- db
  syns <- mycobank::get_mycobank_synonyms(taxa,mycobank_db)
  return(syns$basio_name)
}
