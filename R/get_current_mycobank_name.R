#' Get the 'current' Mycobank names for a vector of taxa names
#'
#' Parses through the Mycobank synonyms and taxon names to pull the 'current' name for a list of taxa names
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
#' @return Data frame. Two columns: 1) query, the query of taxon names provided; 2) current_name, the current name given in the Mycobank database. Duplicated query names removed
#'
#' @details
#' This will search through the database using pattern matching to pull the 'current name' so spelling counts. Capitalization does not affect it though.
#'
#' @examples
#' db <- get_mycobank_db(overwrite = FALSE)
#' taxa <- c("Abaphospora borealis","Conisphaeria borealis var. minor","Sphaeria borealis","Nonsense name","Abaphospora")
#' get_current_mycobank_name(taxa,db)

#'
#' @export

get_current_mycobank_name <- function(taxa,mycobank_db){

  # TESTING ####################################################################

  # taxa must be either in the form "genus" or "genus species"
  if(class(taxa) != "character"){
    stop("taxa must be a character vector of either 'genus' or 'genus species'")
  }


  # SETUP ######################################################################

  # suppress warnings
  defaultW <- getOption("warn")
  options(warn = -1)


  # switch all to lowercase
  taxa <- str_to_lower(taxa)
  lowercase_synonyms <- mycobank_db$Synonymy %>% str_to_lower() %>% str_squish()

  # remove duplicate taxa
  taxa <- unique(taxa)

  # FIND MATCHES IN SYNONYMS (How to deal with missing values???)

  # get matches in synonyms column
  synonym_matches <- list()
  for(taxon in taxa){
    synonym_matches[[taxon]] <-
    str_detect(lowercase_synonyms,pattern = taxon) %>% which()
  }

  # if synonym_matches has no hit, replace with NA
  testlength <-
  synonym_matches %>%
    map_lgl(function(x){identical(x, integer(0))}) %>%
    which() %>% unname()

  if(!identical(testlength,integer(0))){
    synonym_matches[[synonym_matches %>%
                       map_lgl(function(x){identical(x, integer(0))}) %>%
                       which()]] <- NA
  }


  lowercase_db_taxa_names <- str_to_lower(mycobank_db$Taxon_name)

  current_name <- c()
  for(query in names(synonym_matches)){


    #if taxa (query) is only one word long, assume genus
    #filter results to genus level matches for those
    query_length <- query %>% str_split(" ") %>% unlist() %>% length()
    if(query_length == 1){
      matches <- mycobank_db[which(lowercase_db_taxa_names == query),"Synonymy"] %>% str_to_lower()
      x <-
        matches %>%
        str_split("taxonomic synonyms: ") %>%
        map(1) %>%
        str_squish() %>%
        str_split(" ") %>%
        map_chr(function(x){paste(x[3],sep=" ")}) %>%
        unique()

      # deal with missing matches
      x <- ifelse(x=="NA",NA,str_to_sentence(x))

      current_name[query] <- x
    }

    if(query_length > 1){
      matches <- synonyms[synonym_matches[query] %>% unlist]
      x <-
        matches %>%
        str_split("taxonomic synonyms: ") %>%
        map(1) %>%
        str_squish() %>%
        str_split(" ") %>%
        map_chr(function(x){paste(x[3],x[4],sep=" ")}) %>%
        unique()

      # deal with missing matches
      x <- ifelse(x=="NA NA",NA,str_to_sentence(x))

      current_name[query] <- x


    }

  }

  # build current name data frame
  current_name_df <- data.frame(query = names(current_name) %>% str_to_sentence(),
                                current_name = current_name)

  # add taxonomic level to dataframe
  current_name_df$tax_level <-
  current_name_df$current_name %>%
    str_split(" ") %>%
    map_dbl(length) %>%
    unlist()
  current_name_df$tax_level[is.na(current_name_df$tax_level)] <- NA


  current_name_df <-
  current_name_df %>%
    dplyr::mutate(tax_level = dplyr::case_when(tax_level == 1 ~ "genus",
                                               is.na(current_name) ~ NA,
                                        TRUE ~ "species"))

  current_name_df$tax_level[which(is.na(current_name_df$current_name))] <- NA



  # Turn warnings back to default
  options(warn = defaultW)

  return(current_name_df)

}

