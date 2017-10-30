library(tidyverse)
library(stringr)
library(ggplot2)
library(readr)
library(purrr)
library(foreign)

setwd("C:/Users/Lizette/Documents/Masters/R_Programming_Fall_2017/Homework/Homework_4_5/data-raw")

url <- paste0("ftp://ftp.nhtsa.dot.gov/fars/1975/DBF/FARS1975.zip")

path_to_unzip <- paste0(getwd(), "/yearly_fars_data/year_file_full_",
                        str_extract(url, "[0-9][0-9][0-9][0-9]"),".zip")

year_download <- download.file(url, destfile = path_to_unzip)

unzip(path_to_unzip)

file.rename("PERSON.dbf", paste0("PERSON_", str_extract(url, "[0-9][0-9][0-9][0-9]"),".dbf"))

#use mapping to map function: map(vector, function)


download_data <- function() {
  for(i in 2001:2010){
    
    url <- paste0("ftp://ftp.nhtsa.dot.gov/fars/",
                  as.character(i),"/DBF/FARS", as.character(i),".zip")
  
    path <- paste0(getwd(), "/yearly_fars_data/year_file_full_", as.character(i),".zip")
    
    unzip(path)
    
    year_download <- download.file(url, destfile = path)}
    
    file.rename("PERSON.dbf", paste0("PERSON_", str_extract(url, "[0-9][0-9][0-9][0-9]"),".dbf"))
}

download_data()
