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
cougar_status <- function(data, column, check, replace) 
{
  for(i in 1:length(check)){
    data[,column][data[,column] == check[i]] <- replace[i]
  }
  return(data)
}

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

aepyceros_data <- read.csv("https://de.cyverse.org/dl/d/28031164-7903-4EC1-BA86-6441741BAB35/Extant_Aepyceros_database_updated_11_2016.csv", header = TRUE, stringsAsFactors = FALSE)
aepyceros_template <- read.csv("https://raw.githubusercontent.com/futres/fovt-data-mapping/cougar_test/Mapping%20Files/aepyceros_template.csv", header = TRUE, stringsAsFactors = FALSE)

aepyceros_data_mapped <- cougar_col_rename(a = aepyceros_data, b = aepyceros_template, c = "Column.Name", d = "Template.Name")
#Problem: only doing the first one, not going through them all
#Issue: because length(cols), want nrows

aepyceros_data1 <- aepyceros_data %>%
  #cougar_status("Status", c("A", "B", "C"), c("Intact", "Field Dressed", "Skinned")) %>%
  cougar_sex("SEX") %>%
  cougar_melt("Length", "Weight") %>%
  cougar_add_col() %>%
  cougar_measurement_unit("measurementUnit", "variable") %>%
  cougar_col_rename(cougar_template, "Column.Name", "Template.Name")


