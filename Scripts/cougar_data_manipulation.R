## cougar data set

## installed packages
library(tidyverse)
library(dplyr)
library(tibble)
library(anchors)
library(plyr)
library(reshape2)
library(janitor)


## updated set gets rid of columns w no data
cougar_template <- read.csv("https://raw.githubusercontent.com/futres/fovt-data-mapping/cougar_test/Mapping%20Files/column%20name%20template.csv")
cougar_data <- read.csv("https://de.cyverse.org/dl/d/F2088922-D273-49AE-985F-8D55966627A9/1987to2019_Cougar_Weight_Length_Public_Request.csv")
aepyceros_data <- read.csv("https://de.cyverse.org/dl/d/28031164-7903-4EC1-BA86-6441741BAB35/Extant_Aepyceros_database_updated_11_2016.csv", sep = ",", dec = " ")
aepyceros_template <- read.csv("https://raw.githubusercontent.com/futres/fovt-data-mapping/master/Mapping%20Files/ontology_codeBook.csv", header = TRUE, stringsAsFactors = TRUE)


## delete empty rows and columns
delete_empty_r_and_c <- function(data){
  data <-data %>%
    mutate_all(funs(na_if(., ""))) %>%
    remove_empty("cols") %>%
    remove_empty("rows")
  return(data)
}

## update status
status <- function(data, column, check, replace) 
{
  for(i in 1:length(check))
  {
    data[,column][data[,column] == check[i]] <- replace[i]
  }
  return(data)
}


sex <- function(data, column)
{
  data[,column] <- replace(data[,column], grep("^f", data[,column], ignore.case = TRUE), "female")
  data[,column] <- replace(data[,column], grep("^m", data[,column], ignore.case = TRUE), "male")
  return(data)
}

## melt data & filter empty values
melt_data <- function(data, col1, col2)
{
  data <- melt(data, measure.vars = c(col1, col2))
  dplyr::filter(data, !is.na(value))
}

## add new column measuremnetUnit
add_col <- function(data)
{
  add_column(data, measurementUnit = NA)
}
## populate measurementUnit
measurement_unit <- function(data, change, check)
{
  data[,change] <- grepl(pattern = "w", data[,check], ignore.case = TRUE)
  data[,change][data[,change] == TRUE] <- "g"
  data[,change][data[,change] == FALSE] <- "mm"
  return(data)
}

## rename columns
col_rename<- function(data, template, old, new)
{
  names(data) <- gsub("\\.", " ", colnames(data))
  cols <- colnames(data)
  x <- c()
  for(i in 1:nrow(template))
  {
    if(isTRUE(colnames(data)[i] %in% template[,old]))
    {
      colnames(data)[i] <- template[,new][template[,old] == cols[i]]
    }
  }
  return(data)
}

# cougar_data <- cougar_data %>%
#   delete_empty_r_and_c() %>%
#   status("Status", c("A", "B", "C"), c("Intact", "Field Dressed", "Skinned")) %>%
#   sex("Sex") %>%
#   melt_data("Length", "Weight") %>%
#   add_col() %>%
#   measurement_unit("measurementUnit", "variable") %>%
#   col_rename(cougar_template, "Column.Name", "Template.Name")

aepyceros_template <- delete_empty_r_and_c(aepyceros_template)
aepyceros_data <- col_rename(aepyceros_data, aepyceros_template, "label", "term")

aepyceros_data <- aepyceros_data %>%
  delete_empty_r_and_c() %>%
  #status("Status", c("A", "B", "C"), c("Intact", "Field Dressed", "Skinned")) %>%
  sex("SEX")
  #melt_data("Length", "Weight") %>%
  #add_col() %>%
  #measurement_unit("measurementUnit", "variable") %>%
  #col_rename(data = aepyceros_data, template = aepyceros_template, old = "label", new ="term")
