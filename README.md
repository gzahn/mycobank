# mycobank

<img src="https://github.com/gzahn/mycobank/blob/main/media/mycobank_hex_sticker.png" alt="drawing" width="200"/>


Tools for programmatically accessing and querying the [Mycobank fungal taxonomic database](https://www.mycobank.org).

___

## Installation:

```
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")
    
devtools::install_github("gzahn/mycobank")
```

___

## Citation:

Geoffrey Zahn. (2023). Mycobank R Package (v0.1) [Computer software]. Zenodo. https://doi.org/10.5281/ZENODO.10439638

[![DOI](https://zenodo.org/badge/736449862.svg)](https://zenodo.org/doi/10.5281/zenodo.10439638)



___


## Example usage:

**Download the Mycobank database.** This will download the database the first time you use it. Subsequent times, it will only return the database as a data frame unless you specify `overwrite = TRUE`.

```
db <- get_mycobank_db()
```

Congrats! Now have a party playing with the taxonomic database if you like. But I'm including some useful tools for common tasks:

Get a character vector of taxonomic names. These can be "genus species" or just "genus." Maybe this comes from a phyloseq object or ann Excel sheet? Whatever you want!

```
taxa <- c("Abaphospora borealis","Conisphaeria borealis var. minor","Sphaeria borealis","Nonsense name","Abaphospora")
```

**Get a list of basionyms, obligate & taxonomic synonyms.**

(returns a list because each query taxon is likely to have differing numbers of synonyms)

```
synonyms <- get_mycobank_synonyms(taxa,db)
```

To get a useful data frame...
 - Select which info columns you want
 - This works will all the list elements except for "taxonomic_synonyms" since it is a list itself

```
info <- c("query","current_name","basio_name","obligate_synonym")

synonyms[info] %>% as_tibble()
```



You can look at individual results by descending into the list

`synonyms$taxonomic_synonyms$Abaphospora`

...or, you could stick them all into a big data frame, padded with extra NA values so they all have the same lengths

```
synonyms$taxonomic_synonyms %>% 
  sapply("length<-", max(lengths(.))) %>% 
  as_tibble()
```

...more features coming soon, with tools to:

  - return info on anamorph/teliomorph synonyms

