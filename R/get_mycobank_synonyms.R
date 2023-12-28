#' Get the 'current' Mycobank synonyms for a vector of taxa names
#'
#' Parses through the Mycobank synonyms and taxon names to pull various synonyms for a vector of taxon names
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
#' @return List. Contains 5 elements:  "query","current_name","basio_name","obligate_synonym","taxonomic_synonyms"
#'
#' @details
#' This will search through the database using pattern matching to pull the various basionyms, and synonyms for a given taxon name, if present. Returns NA for that taxon otherwise.
#'
#' @examples
#' db <- get_mycobank_db(overwrite = FALSE)
#' taxa <- c("Abaphospora borealis","Conisphaeria borealis var. minor","Sphaeria borealis","Nonsense name","Abaphospora")
#' synonyms <- get_mycobank_synonyms(taxa,db)
#' # To see a tidy format for the first taxon query:
#' synonyms %>% map(1) %>% as.data.frame()
#'
#' @export

get_mycobank_synonyms <- function(taxa,mycobank_db){
  synonyms <- list()
  for(taxon in taxa){
    records <- mycobank_db %>%
      dplyr::filter(Taxon_name == taxon) %>%
      dplyr::filter(Name_status == "Legitimate")
    synonyms[[taxon]] <- records
  }


  # FINDING SYNONYMS ####
  # synonyms might be listed as "Obligate synonyms: "
  # need to deal with obligate synonyms
  synonyms %>% map("Synonymy")
  query_lengths <- names(synonyms) %>% str_split(" ") %>% map_dbl(length)
  genus_queries <- which(query_lengths == 1)

  synonym_text <-
    synonyms %>%
    map("Synonymy") %>%
    map(function(x){
      x %>%
        str_squish() %>%
        str_split("Current name:")
    }) %>%
    map(1) %>%
    map(2) %>%
    map(str_squish)

  taxa
  synonym_text

  # get current names
  current_names <- c()
  for(i in seq_along(taxa)){
    # get number of words in name
    n_words <- query_lengths[i]
    current_name <- word(synonym_text[[i]],end=n_words)
    current_name <- ifelse(identical(current_name,character(0)),NA,current_name)
    current_names[taxa[i]] <- current_name
  }

  basionym_text <-
    synonym_text %>%
    map(function(x){
      gsub(".*Basionym:","",x)
    }) %>%
    map(str_squish)

  # get basionyms
  basio_names <- c()
  for(i in seq_along(taxa)){
    n_words <- query_lengths[i]
    text <- ifelse(basionym_text[[i]] %>%
                     grepl(pattern = "Basionym: "),basionym_text[[i]],NA)
    if(identical(text,logical(0))){text <- NA}
    text <- word(basionym_text[[i]],end=n_words)
    if(identical(text,character(0))){text <- NA}
    basio_names[taxa[i]] <- text
  }

  obligate_text <-
    synonym_text %>%
    map(function(x){
      gsub(".*Obligate synonyms: - ","",x)
    }) %>%
    map(str_squish)

  # get obligate synonyms
  obligate_names <- c()
  for(i in seq_along(taxa)){
    n_words <- query_lengths[i]
    text <- ifelse(obligate_text[[i]] %>%
                     grepl(pattern = "Obligate synonyms: "),obligate_text[[i]],NA)
    if(identical(text,logical(0))){text <- NA}
    text <- word(obligate_text[[i]],end=n_words)
    if(identical(text,character(0))){text <- NA}
    obligate_names[taxa[i]] <- text
  }


  tax_text <-
    synonym_text %>%
    map(function(x){
      gsub(".*Taxonomic synonyms: - ","_TAXONOMIC_SYNONYMS_",x)
    }) %>%
    map(str_squish)


  # get obligate synonyms
  tax_names <- list()
  for(i in seq_along(taxa)){
    n_words <- query_lengths[i]
    text <- ifelse(tax_text[[i]] %>%
                     grepl(pattern = "_TAXONOMIC_SYNONYMS_"),tax_text[[i]],NA)
    if(identical(text,logical(0))){text <- NA}

    synonym_list <-
      text %>%
      str_remove("_TAXONOMIC_SYNONYMS_") %>%
      str_split(" - ")

    tax_names[[taxa[i]]] <- synonym_list[[1]]
  }



  # return this as a list in final form
  data.frame(query=taxa,current_names,basio_names,obligate_synonym=obligate_names)
  names(taxa) <- taxa
  final_list <- list(query=taxa,current_name=current_names,basio_name=basio_names,obligate_synonym=obligate_names,taxonomic_synonyms=tax_names)

  return(final_list)

}
