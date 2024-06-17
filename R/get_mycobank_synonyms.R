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
#' @return List. Contains 6 elements:  "query","current_name","basio_name","obligate_synonym","taxonomic_synonyms", "tidy_results"
#'
#' @details
#' This will search through the database using pattern matching to pull the various basionyms, and synonyms for a given taxon name, if present. Returns NA for that taxon otherwise. The 'tidy_results' element of the returned list contains a 'long-form' tibble with all supported synonym types. Taxa with "sp." in their name will return genus-level synonyms only.
#'
#' @examples
#' db <- get_mycobank_db(overwrite = FALSE)
#' taxa <- c("Abaphospora borealis","Conisphaeria borealis var. minor","Sphaeria borealis","Nonsense name","Abaphospora sp.")
#' synonyms <- get_mycobank_synonyms(taxa,db)
#' # To see a tidy format for the first taxon query:
#' synonyms %>% map(1) %>% as.data.frame()
#'
#' @export

get_mycobank_synonyms <- function(taxa,mycobank_db){

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

  synonyms <- list()
  for(taxon in taxa2){
    records <- mycobank_db %>%
      dplyr::filter(Taxon_name == taxon) %>%
      dplyr::filter(Name_status == "Legitimate")
    synonyms[[taxon]] <- records
  }


  # FINDING SYNONYMS ####
  # synonyms might be listed as "Obligate synonyms: "
  # need to deal with obligate synonyms

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

  # get current names
  current_names <- c()
  for(i in seq_along(taxa2)){
    # get number of words in name
    n_words <- query_lengths[i]
    current_name <- word(synonym_text[[i]],end=n_words)
    current_name <- ifelse(identical(current_name,character(0)),NA,current_name)
    current_names[taxa2[i]] <- current_name
  }

  basionym_text <-
    synonym_text %>%
    map(function(x){
      gsub(".*Basionym:","",x)
    }) %>%
    map(str_squish)

  # get basionyms
  basio_names <- c()
  for(i in seq_along(taxa2)){
    n_words <- query_lengths[i]
    text <- ifelse(basionym_text[[i]] %>%
                     grepl(pattern = "Basionym: "),basionym_text[[i]],NA)
    if(identical(text,logical(0))){text <- NA}
    text <- word(basionym_text[[i]],end=n_words)
    if(identical(text,character(0))){text <- NA}
    basio_names[taxa2[i]] <- text
  }

  obligate_text <-
    synonym_text %>%
    map(function(x){
      gsub(".*Obligate synonyms: - ","",x)
    }) %>%
    map(str_squish)

  # get obligate synonyms
  obligate_names <- c()
  for(i in seq_along(taxa2)){
    n_words <- query_lengths[i]
    text <- ifelse(obligate_text[[i]] %>%
                     grepl(pattern = "Obligate synonyms: "),obligate_text[[i]],NA)
    if(identical(text,logical(0))){text <- NA}
    text <- word(obligate_text[[i]],end=n_words)
    if(identical(text,character(0))){text <- NA}
    obligate_names[taxa2[i]] <- text
  }


  tax_text <-
    synonym_text %>%
    map(function(x){
      gsub(".*Taxonomic synonyms: - ","_TAXONOMIC_SYNONYMS_",x)
    }) %>%
    map(str_squish)


  # get obligate synonyms
  tax_names <- list()
  for(i in seq_along(taxa2)){
    n_words <- query_lengths[i]
    text <- ifelse(tax_text[[i]] %>%
                     grepl(pattern = "_TAXONOMIC_SYNONYMS_"),tax_text[[i]],NA)
    if(identical(text,logical(0))){text <- NA}

    synonym_list <-
      text %>%
      str_remove("_TAXONOMIC_SYNONYMS_") %>%
      str_split(" - ")

    tax_names[[taxa2[i]]] <- synonym_list[[1]]
  }



  # return this as a list in final form
  # data.frame(query=taxa2,current_names,basio_names,obligate_synonym=obligate_names)
  names(taxa) <- taxa

  # add tidy results element to list

  final_list <- list(query=taxa2,current_name=current_names,basio_name=basio_names,obligate_synonym=obligate_names,taxonomic_synonyms=tax_names)



  info <- c("query","current_name","basio_name","obligate_synonym")
  all_elements <- final_list[info] %>% as_tibble()
  tidy_results <-
    tax_names %>%
    sapply("length<-", max(lengths(.))) %>%
    as_tibble() %>% pivot_longer(everything(),names_to = "query",values_to = "taxonomic_synonyms") %>%
    full_join(all_elements) %>%
    unique.data.frame() %>%
    group_by(query) %>%
    reframe(current_name=unique(current_name),
            basio_name=unique(basio_name),
            obligate_synonym=unique(obligate_synonym),
            taxonomic_synonyms=unique(taxonomic_synonyms))

  final_list[["tidy_results"]] <- tidy_results

  return(final_list)


}
