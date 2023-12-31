% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_mycobank_db.R
\name{get_mycobank_db}
\alias{get_mycobank_db}
\title{Get the Mycobank taxonomy database}
\usage{
get_mycobank_db(
  dl_path = "default",
  overwrite = FALSE,
  url = "https://www.mycobank.org/Images/MBList.zip"
)
}
\arguments{
\item{dl_path}{Character. Path to download directory. Defaults to `.libPaths()[1]`}

\item{overwrite}{Logical. If TRUE, it will re-download the database from the Mycobank website, even if you already have it.}

\item{url}{Character. Only change this if the mycobank page changes their default download url.}
}
\value{
Data frame. Side effects: Downloads and extracts the Mycobank database .zip file.
}
\description{
Downloads and parses the Mycobank database
If the database has been previously downloaded and you do not need to update it, this will just return the existing database as a data frame.
Uses system commands (wget, unzip) so make sure you have these in your system PATH
}
\examples{
x <- get_mycobank_db(overwrite=TRUE)

}
