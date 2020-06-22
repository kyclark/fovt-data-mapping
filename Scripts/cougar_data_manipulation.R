## cougar data set

## installed packages
library(tidyverse)
library(dplyr)
library(tibble)

## updated set gets rid of columns w no data
cougar_data <- X1987_2019_Cougar_Weight_Length_Public_Request[-c(9:11)]

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
  
## dataset update -> trait description column
cougar_dataV2 <- cougar_data %>% add_column("Trait Description" = NA, .after = "Weight")

## if weigh has value trait description == weight
cougar_dataV2$`Trait Description`[!(is.na(cougar_data$Weight))] <- "Weight"

## rename column "Weight" to "Trait"
cougar_dataV2 %>%
  rename("Trait Data" = Weight)
  