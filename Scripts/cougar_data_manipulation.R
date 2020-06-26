## cougar data set

## installed packages
library(tidyverse)
library(dplyr)
library(tibble)
library(anchors)
library(plyr)
library(reshape2)

## updated set gets rid of columns w no data
cougar_template <- column.name.template
cougar_data <- read.csv("https://de.cyverse.org/dl/d/F2088922-D273-49AE-985F-8D55966627A9/1987to2019_Cougar_Weight_Length_Public_Request.csv")
cougar_data <- cougar_data[-c(9:11)]

## update status
cougar_status <- function(x, y) 
{
  x[,y][x[,y] == "A" | x[,y] == "a"] <- "Intact"
  x[,y][x[,y] == "B" | x[,y] == "b"] <- "Field Dressed"
  x[,y][x[,y] == "C" | x[,y] == "c"] <- "Skinned"
  return(x)
}

## f -> female & m -> male
#try to use grepl("[F][f]", "female")
cougar_sex <- function(x, y)
{
  x[,y][x[,y] == "M" | x[,y] == "m"] <- "Male"
  x[,y][x[,y] == "F" | x[,y] == "f"] <- "Female"
  return(x)
}

## melt data & filter empty values
cougar_melt <- function(x, y, z)
{
  x <- melt(x, measure.vars = c(y, z))
  dplyr::filter(x, !is.na(value))
}

## add new column measuremnetUnit
cougar_add_col <- function(x){
  add_column(x, measurementUnit = NA)
}
## populate measurementUnit
cougar_measurement_unit <- function(x, y, z)
{
  x[,y][x[,z] == "Weight"] <- "g"
  x[,y][x[,z] == "Length"] <- "mm"
  return(x)
}

#I wonder if this could be piped?:
cougar_data <- cougar_status(cougar_data, "Status")
cougar_data <- cougar_sex(cougar_data, "Sex")
cougar_data <- cougar_melt(cougar_data, "Length", "Weight")
cougar_data <- cougar_add_col(cougar_data)
cougar_data <- cougar_measurement_unit(cougar_data, "measurementUnit", "variable")

cols <- colnames(cougar_data)
x <- c()
for(i in 1:length(cols))
{
  if(isTRUE(colnames(cougar_data)[i] %in% cougar_template$Column.Name))
  {
    #change column name to reflect template column as matched in template_mapping
    colnames(cougar_data)[i] <- cougar_template$Template.Name[cougar_template$Column.Name == cols[i]]
  }
}


