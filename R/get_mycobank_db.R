#' Get the Mycobank taxonomy database
#'
#' Downloads and parses the Mycobank database
#' If the database has been previously downloaded and you do not need to update it, this will just return the existing database as a data frame.
#' Uses R's internal downloading protocols, but these rely on having them installed.
#'
#' @import readxl
#'
#' @param dl_path Character. Path to download directory. Defaults to `.libPaths()[1]`
#' @param overwrite Logical. If TRUE, it will re-download the database from the Mycobank website, even if you already have it.
#' @param url Character. Only change this if the mycobank page changes their default download url.
#'
#' @return Data frame. Side effects: Downloads and extracts the Mycobank database .zip file.
#'
#' @examples
#' x <- get_mycobank_db(overwrite=TRUE)
#'
#' @export

get_mycobank_db <- function(dl_path="default",overwrite=FALSE,url="https://www.mycobank.org/Images/MBList.zip"){

  # HANDLE PARAMETERS ##########################################################
  if(class(dl_path) != "character" | length(dl_path) > 1){
    stop("dl_path must be a character vector of length 1 that points to a valid downloadable location on your machine.")
  }

  if(class(overwrite) != "logical" | length(overwrite) > 1){
    stop("overwrite must be logical.")
  }

  if(class(url) != "character" | length(url) > 1){
    stop("url must be character. point it to latest mycobank database location. default is: https://www.mycobank.org/Images/MBList.zip")
  }

  if(url != "https://www.mycobank.org/Images/MBList.zip"){
    cat(paste0("Using non-default url location: ",url))
  }


  # HANDLE DOWNLOAD ############################################################

  # If Windows system, it will add ".ZIP" to downloaded file name
  if(Sys.info()[['sysname']] == "Windows"){
    cat("Windows system detected. Some versions of Windows add extra file path extensions. ")
    cat("If you get an error below like 'cannot find or open ... MBList.zip.zip' ")
    cat("Then you may have to go to the download location and manually unzip the file. ")
    cat("Then you will be able to run this command and it should work. ")
    cat("If that doesn't work, please use a Unix (Mac or Linux) computer because I cannot deal with Windows' bullshit.")
    cat("Please see options() to change download method through the option 'download.file.method' ")
  }

    # get main filepath and put mycobank database in it
    if(dl_path == "default"){
      main_fp <- .libPaths()[1]
      db_dir <- file.path(main_fp,"mycobank_db")
      # account for Windows adding ".ZIP"
      db_fp <- file.path(db_dir,"MBList.zip")
    }

    if(dl_path != "default"){
      main_fp <- dl_path
      db_dir <- file.path(main_fp,"mycobank_db")
      db_fp <- file.path(db_dir,"MBList.zip")
    }



  # make download dir if doesn't exist
  if(!dir.exists(db_dir)){
    dir.create(db_dir)
  }

  # download database if it doesn't exist already
  if(!file.exists(db_fp)){
    cat("Downloading current Mycobank database...")
    download.file(url = url,destfile = db_fp)
    cat("Download location is: ")
    cat(db_dir)
  }



  # if overwrite == TRUE and file exists already, re-download
  if(file.exists(db_fp) & overwrite == TRUE){
    cat("Re-downloading database")
    download.file(url = url,destfile = db_fp)
  }

  # if overwrite set to FALSE and file already exists, do not download
  if(file.exists(db_fp) & overwrite == FALSE){
    cat(paste0("Mycobank database is already downloaded and overwrite was set to FALSE. Database is located in: ",db_dir))
  }

  # UNZIP DATABASE #############################################################

  xls_fp <- file.path(db_dir,"MBList.xlsx")
  if(!file.exists(xls_fp)){
    unzip(db_fp, exdir = db_dir, files = "MBList.xlsx")
    }


  # RETURN DATABASE OBJECT #####################################################

  x <- readxl::read_xlsx(xls_fp)
  return(x)

}
