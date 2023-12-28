# mycobank

Tools for programmatically accessing and querying the [Mycobank fungal taxonomic database](https://www.mycobank.org).

___

## Installation:

`if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")`
    
`devtools::install_github("gzahn/mycobank")`

___

## Example usage:

**Download the Mycobank database.** This will download the database the first time you use it. Subsequent times, it will only return the database as a data frame unless you specify `overwrite = TRUE`.

`db <- get_mycobank_db()`

Congrats! Now have a party playing with the taxonomic database if you like. But I'm including some useful tools for common tasks:

**Find the 'current' Mycobank names for a vector of taxa.**

Get a character vector of taxonomic names. These can be "genus species" or just "genus." This will return a data frame of the queries and their current names.

`taxa <- c("Abaphospora borealis","Conisphaeria borealis var. minor","Sphaeria borealis","Nonsense name","Abaphospora")`

`get_current_mycobank_name(taxa,db)`

...more features coming soon, with tools to:

  - return a list of all synonyms for each taxon
  - return info on anamorph/teliomorph synonyms

