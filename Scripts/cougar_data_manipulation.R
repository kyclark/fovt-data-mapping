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
cougar_data <- cougar_data[-c(9:11)]


# x[,y][x[,y] == "A" | x[,y] == "a"] <- "Intact"
# x[,y][x[,y] == "B" | x[,y] == "b"] <- "Field Dressed"
# x[,y][x[,y] == "C" | x[,y] == "c"] <- "Skinned"

## update status
cougar_status <- function(x, y) 
{
  x[,y][x[,y] == "A" | x[,y] == "a"] <- "Intact"
  x[,y][x[,y] == "B" | x[,y] == "b"] <- "Field Dressed"
  x[,y][x[,y] == "C" | x[,y] == "c"] <- "Skinned"
  return(x)
}

## f -> female & m -> male

cougar_sex <- function(x, y)
{
  x[,y] <- grepl(pattern = "f", x[,y], ignore.case = TRUE)
  x[,y][x[,y] == TRUE] <- "Female"
  x[,y][x[,y] == FALSE] <- "Male"
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

cleanup_data <- function(x)
{
  x <- x %>%
    cougar_status("Status") %>%
    cougar_sex("Sex") %>%
    cougar_melt("Length", "Weight") %>%
    cougar_add_col() %>%
    cougar_measurement_unit("measurementUnit", "variable") %>%
    cougar_col_rename(cougar_template, "Column.Name", "Template.Name")
  return(x)
}
cougar_data <- cleanup_data(cougar_data)



