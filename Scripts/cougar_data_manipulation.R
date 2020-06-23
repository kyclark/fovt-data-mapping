## cougar data set

## installed packages
library(tidyverse)
library(dplyr)
library(tibble)

## updated set gets rid of columns w no data
cougar_data <- read.csv("https://de.cyverse.org/dl/d/F2088922-D273-49AE-985F-8D55966627A9/1987to2019_Cougar_Weight_Length_Public_Request.csv")
cougar_data <- cougar_data[-c(9:11)]
#cougar_data <- X1987_2019_Cougar_Weight_Length_Public_Request[-c(9:11)]

## update status
  ## A -> Intact
  cougar_data$Status[cougar_data$Status == "A"] <- "Intact"
  ## B -> Field Dressed
  cougar_data$Status[cougar_data$Status == "B"] <- "Field Dressed"
  ## C -> Skinned
  cougar_data$Status[cougar_data$Status == "C"] <- "Skinned"
  
## f -> female & m -> male
  ## F -> Female
  cougar_data$Sex[cougar_data$Sex == "F"] <- "Female"
  ## M -> Male
  cougar_data$Sex[cougar_data$Sex == "M"] <- "Male"
  
## To Do:
  #rename weight and length column (see GEOME)
  #make long version: melt(data, id.vars = metadata, variable.name = name from template)
    #stringsAsFactors = FALSE
  #get rid of rows that have "NA" as for trait values
  #change column "value" to appropriate name in template
  #add columns for units for weight and mass
    #what are those column names? (see template)
    #populat weight with "g" and length with "mm"
  
## dataset update -> trait description column
cougar_dataV2 <- cougar_data %>% add_column("Trait Description" = NA, .after = "Weight")

## if weigh has value trait description == weight 
cougar_dataV2$`Trait Description`[!(is.na(cougar_data$Weight))] <- "Weight"

## rename column "Weight" to "Trait"
cougar_dataV2 %>%
  rename("Trait Data" = Weight)
  
## melt data
melt(cougar_data, na.rm = TRUE, value.name = c("Date", "Management Unit", "County", "Sex", "Age", "Status"))
