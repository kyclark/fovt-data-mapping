## cougar data set

## installed packages
library(tidyverse)
library(dplyr)
library(tibble)
library(anchors)
library(plyr)
library(reshape2)


## updated set gets rid of columns w no data
cougar_template <- read.csv("https://raw.githubusercontent.com/futres/fovt-data-mapping/cougar_test/Mapping%20Files/column%20name%20template.csv")
cougar_data <- read.csv("https://de.cyverse.org/dl/d/F2088922-D273-49AE-985F-8D55966627A9/1987to2019_Cougar_Weight_Length_Public_Request.csv")
aepyceros_data <- Extant_Aepyceros_database_updated_11_2016


## update status
<<<<<<< HEAD
cougar_status <- function(data, column, check, replace) 
=======

#let's make this more general
#give the function 2 more arguments, which are vectors
#the first vector is the unique data entries (e.g., "A","B","C")
#the second vector is the appropriate terms (e.g., "whole organism","part organism - field dressed", "part organism - skinned")
cougar_status <- function(a, b, c, d)
  #make a a term
  #E.g., a = data and comment that data = dataframe; 
  #b = column, and that column must be in quotes;
  #c = old.terms, which is a vector;
  #d = new.terms, which is a vector
>>>>>>> 854288fb052cb74c4aa0247060ee0d634d2b1583
{
  for(i in 1:length(check)){
    data[,column][data[,column] == check[i]] <- replace[i]
  }
  return(data)
}

<<<<<<< HEAD
=======
## f -> female & m -> male
#we don't want to assert that a data entry is male if it is N/A
#modify so that x[,y] must start with "m" or "M" to be "Male"

# pattern = "\b[M,m]" for male; \b[F,f] for female

>>>>>>> 854288fb052cb74c4aa0247060ee0d634d2b1583
cougar_sex <- function(x, y)
{
  x[,y] <- gsub(pattern = "\\<f", replacement = "Female", x[,y], ignore.case = TRUE)
  x[,y] <- gsub(pattern = "\\<m", replacement = "Male", x[,y], ignore.case = TRUE)
  return(x)
}

## melt data & filter empty values
cougar_melt <- function(x, y, z)
{
  x <- melt(x, measure.vars = c(y, z))
  dplyr::filter(x, !is.na(value))
}

## add new column measuremnetUnit
cougar_add_col <- function(x)
{
  add_column(x, measurementUnit = NA)
}
## populate measurementUnit
cougar_measurement_unit <- function(x, y, z)
{
  x[,y] <- grepl(pattern = "w", x[,z], ignore.case = TRUE)
  x[,y][x[,y] == TRUE] <- "g"
  x[,y][x[,y] == FALSE] <- "mm"
  return(x)
}

## rename columns
cougar_col_rename <- function(a, b, c, d)
{
  cols <- colnames(a)
  x <- c()
  for(i in 1:length(cols))
  {
    if(isTRUE(colnames(a)[i] %in% b[,c]))
    {
      colnames(a)[i] <- b[,d][b[,c] == cols[i]]
    }
  }
  return(a)
}

#so great! We'll see if this works with other datasets or if we need to break it apart.
# cleanup_data <- function(x)
# {
#   x <- x %>%
#     cougar_status("Status", c("A", "B", "C"), c("Intact", "Field Dressed", "Skinned")) %>%
#     cougar_sex("Sex") %>%
#     cougar_melt("Length", "Weight") %>%
#     cougar_add_col() %>%
#     cougar_measurement_unit("measurementUnit", "variable") %>%
#     cougar_col_rename(cougar_template, "Column.Name", "Template.Name")
#   return(x)
# }
aepyceros_data1 <- aepyceros_data %>%
      #cougar_status("Status", c("A", "B", "C"), c("Intact", "Field Dressed", "Skinned")) %>%
      cougar_sex("SEX") %>%
      cougar_melt("Length", "Weight") %>%
      cougar_add_col() %>%
      cougar_measurement_unit("measurementUnit", "variable") %>%
      cougar_col_rename(cougar_template, "Column.Name", "Template.Name")


