# mycobank

<img src="https://github.com/gzahn/mycobank/blob/main/media/mycobank_hex_sticker2.png" alt="drawing" width="200"/>


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
<img src="https://github.com/gzahn/mycobank/blob/main/media/screenshot1.png" alt="tibble1"/>


You can look at individual results by descending into the list

`synonyms$taxonomic_synonyms$Abaphospora`

...or, you could access a 'tidy' version of all the taxonomic info
```
synonyms$tidy_results
```
<img src="https://github.com/gzahn/mycobank/blob/main/media/screenshot2.png" alt="tibble2_tidy"/>

___

**Want to see which, if any, of your query names differ from the current names in MycoBank?**

```
# this code will give a simple data frame with an additional TRUE/FALSE column indicating whether there is a difference
# useful to quickly scan your results
data.frame(query=taxa,current=synonyms$current_name) %>% 
  mutate(query = str_remove(query," sp.")) %>% 
  mutate(different = case_when(query == current ~ FALSE,
                               query != current ~ TRUE))
```

___


**Get a list of taxonomic classifications.**

(returns a character vector of taxonomic classifications for each taxon)

```
get_mycobank_taxonomy(taxa,db)
```

<img src="https://github.com/gzahn/mycobank/blob/main/media/screenshot3.png" alt="tibble3"/>


**I think that's really all there is to do here.**

If you want any other info, it's all in that database tibble waiting for your creativity!

<br>

<br>

