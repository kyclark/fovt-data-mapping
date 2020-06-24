## cougar data set

## installed packages
library(tidyverse)
library(dplyr)
library(tibble)

# ## update status
# ## A -> Intact
# cougar_data$Status[cougar_data$Status == "A"] <- "Intact"
# ## B -> Field Dressed
# cougar_data$Status[cougar_data$Status == "B"] <- "Field Dressed"
# ## C -> Skinned
# cougar_data$Status[cougar_data$Status == "C"] <- "Skinned"

# ## f -> female & m -> male
# ## F -> Female
# cougar_data$Sex[cougar_data$Sex == "F"] <- "Female"
# ## M -> Male
# cougar_data$Sex[cougar_data$Sex == "M"] <- "Male"

## updated set gets rid of columns w no data
cougar_data <- read.csv("https://de.cyverse.org/dl/d/F2088922-D273-49AE-985F-8D55966627A9/1987to2019_Cougar_Weight_Length_Public_Request.csv")
cougar_data <- cougar_data[-c(9:11)]
#cougar_data <- X1987_2019_Cougar_Weight_Length_Public_Request[-c(9:11)]

## update status
  # need two arguments, the dataset and the column 
  status <- function(y){
    ## A -> Intact
      y[y == "A"] <- "Intact"
    ## B -> Field Dressed
      y[y == "B"] <- "Field Dressed"
    ## C -> Skinned
      y[y == "C"] <- "Skinned"
  }
  
## f -> female & m -> male
    #need two arguments, data and column
  sex <- function(y){
    ## F -> Female
      y[y == "F"] <- "Female"
    ## M -> Male
      y[y == "M"] <- "Male"
  }
  
## melt data
test <- melt(cougar_data, id.vars = c("Date", 
                                      "Management.Unit", 
                                      "County", "Sex", 
                                      "Status", "Age"), 
             variable.name = c("Length", "Weight"))

## rename to final
cougar_data_final <- test

## rename columns
cougar_data_final <- cougar_data_final %>%
  rename(c("yearCollected" = "Date", 
           "measurementValue" = "value", 
           "Locality" = "Management.Unit", 
           "measurementType" = "variable"))

## remove empty values
cougar_data_final <- dplyr::filter(cougar_data_final,  !is.na(measurementValue))

## add weight & mass units column
cougar_data_final$measurementUnit <- NA

## populate weight with "g" and length with "mm"
cougar_data_final$measurementUnit[cougar_data_final$measurementType == "Weight"] <- "g"
cougar_data_final$measurementUnit[cougar_data_final$measurementType == "Length"] <- "mm"

## call functions
status(cougar_data_final$Status)
sex(cougar_data_final$Sex)