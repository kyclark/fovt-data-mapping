require(reshape2)

## Ray's Data
ray <- read.csv("https://de.cyverse.org/dl/d/16030E74-A54F-44B2-AA03-76B1A49FCA49/1.FuTRESEquidDbase_6_24_2019.csv", stringsAsFactors = FALSE) #how to point to latest data version?
boneAbbr <- read.csv("BoneAbbr.csv", stringsAsFactors = FALSE)

#get rid of protected sites
ray_safe <- subset(ray, subset = ray$PROTECTED...P != "P")

#cut ray's data to just limb and dental measurements for now from "focused traits"
#https://docs.google.com/spreadsheets/d/1rU15rBo-JpopEqpxBXLWSqaecBXwtYpxBLjRImcCvDQ/edit#gid=0
Rpattern <-  "pre?molar|metacarpal|metatarsal|1st phalanx III" #? allows it to be premolar or molar (note: need all three letters: p r e)
Rx <- grep(Rpattern, boneAbbr$Bone, value = TRUE)

boneAbbr_sub <- boneAbbr[boneAbbr$Bone %in% Rx,]

ray_sub1 <- ray_safe[ray_safe$BONE %in% boneAbbr_sub$Abbreviation,]
ray_sub2 <- subset(ray_safe, subset = c(ray_safe$BONE == "tibia" | 
                                        ray_safe$BONE == "humerus" | 
                                        ray_safe$BONE == "femur" | 
                                        ray_safe$BONE == "radius"))
ray_sub <- rbind(ray_sub1, ray_sub2)

#measurements are from 13:51
ray_long <- melt(ray_sub, id.vars = c(1:12))

#select out specific measurements / change measurement names and map to template
ray_long_sub <- subset(ray_long, ray_long$BONE == "humerus" & ray_long$variable == "M1" |
                         ray_long$BONE == "humerus" & ray_long$variable == "M2" |
                         ray_long$BONE == "humerus" & ray_long$variable == "M5" |
                         ray_long$BONE == "femur" & ray_long$variable == "M1" |
                         ray_long$BONE == "femur" & ray_long$variable == "M2" |
                         ray_long$BONE == "radius" & ray_long$variable == "M10" |
                         ray_long$BONE == "tibia" & ray_long$variable == "M5" |
                         ray_long$BONE == "tibia" & ray_long$variable == "M8" |
                         ray_long$BONE == "mc2" & ray_long$variable == "M3" |
                         ray_long$BONE == "mc2or4" & ray_long$variable == "M3" |
                         ray_long$BONE == "mc3" & ray_long$variable == "M5" |
                         ray_long$BONE == "mc4" & ray_long$variable == "M3")

#next change names to match template
for(i in 1:length(ray_long_sub$SPEC_ID)) {
  if(isTRUE(ray_long_sub$BONE[i] == "humerus" & ray_long_sub$variable[i] == "M1")){
    ray_long_sub$template[i] <- "humerus length"
  }
  else if(isTRUE(ray_long_sub$BONE[i] == "humerus" & ray_long_sub$variable[i] == "M2")){
    ray_long_sub$template[i] <- "humerus length from caput"
  }
  else if(isTRUE(ray_long_sub$BONE[i] == "humerus" & ray_long_sub$variable[i] == "M5")){
    ray_long_sub$template[i] <- "humerus proximal breadth"
  }
  else if(isTRUE(ray_long_sub$BONE[i] == "tibia" & ray_long_sub$variable[i] == "M5")){
    ray_long_sub$template[i] <- "tibia distal breadth"
  }
  else if(isTRUE(ray_long_sub$BONE[i] == "tibia" & ray_long_sub$variable[i] == "M8")){
    ray_long_sub$template[i] <-"tibia proximal breadth"
  }
  else if(isTRUE(ray_long_sub$BONE[i] == "radius" & ray_long_sub$variable[i] == "M10")){
    ray_long_sub$template[i] <- "radius distal breadth"
  }
  else if(isTRUE(ray_long_sub$BONE[i] == "femur" & ray_long_sub$variable[i] == "M1")){
    ray_long_sub$template[i] <- "femur length"
  }
  else if(isTRUE(ray_long_sub$BONE[i] == "femur" & ray_long_sub$variable[i] == "M2")){
    ray_long_sub$template[i] <- "femur length from trochanter"
  }
  else if(isTRUE(ray_long_sub$BONE[i] == "mc2" & ray_long_sub$variable[i] == "M3")){
    ray_long_sub$template[i] <- "metacarpal proximal breadth"
  }
  else if(isTRUE(ray_long_sub$BONE[i] == "mc3" & ray_long_sub$variable[i] == "M5")){
    ray_long_sub$template[i] <- "metacarpal proximal breadth"
  }
  else if(isTRUE(ray_long_sub$BONE[i] == "mc4" & ray_long_sub$variable[i] == "M3")){
    ray_long_sub$template[i] <- "metacarpal proximal breadth"
  }
  else if(isTRUE(ray_long_sub$BONE[i] == "mc2or4" & ray_long_sub$variable[i] == "M3")){
    ray_long_sub$template[i] <- "metacarpal proximal breadth"
  }
}

ray_clean <- ray_long_sub[!(is.na(ray_long_sub$value)),]

colnames(ray_clean)[colnames(ray_clean)=="SPEC_ID"] <- "specimenID"
colnames(ray_clean)[colnames(ray_clean)=="COUNTRY"] <- "country"
colnames(ray_clean)[colnames(ray_clean)=="LOCALITY"] <- "verbatimLocality"
colnames(ray_clean)[colnames(ray_clean)=="QUARRY"] <- "sitename"
colnames(ray_clean)[colnames(ray_clean)=="DATE.COLLECTED"] <- "verbatimEventDate"
colnames(ray_clean)[colnames(ray_clean)=="SEX"] <- "sex"
colnames(ray_clean)[colnames(ray_clean)=="AGE"] <- "ageValue"
colnames(ray_clean)[colnames(ray_clean)=="variable"] <- "measurementType"
colnames(ray_clean)[colnames(ray_clean)=="value"] <- "measurementValue"
colnames(ray_clean)[colnames(ray_clean)=="SIDE"] <- "measurementSide"
ray_clean$measurementUnit <- "mm"

#create species name
ray_clean$scientificName <- paste(ray_clean$GENUS, ray_clean$SPECIES, sep = " ")

#get rid of bone, genus, and species
ray_clean.1 <- ray_clean[,c(-2,-3,-4,-10)]

#get rid of NAs
ray_clean.2 <- ray_clean.1[!(is.na(ray_clean.1$measurementValue)),]

ray_clean.2$specimenID <- gsub("^\\s+|\\s+$", "", ray_clean.2$specimenID)

#write.csv(ray_clean.2, "ray_data.csv", row.names=FALSE)

##Kitty's data
kitty <- read.csv("https://de.cyverse.org/dl/d/0152B269-3942-4BC4-8FDC-E60B48B17EBD/MayaDeerMetrics_Cantryll_Emeryedits.csv", skip = 2, stringsAsFactors = FALSE)

#measurements are: 16:101
kitty_long <- melt(kitty, id.vars = 1:15)

#select out "focused traits"
#https://docs.google.com/spreadsheets/d/1rU15rBo-JpopEqpxBXLWSqaecBXwtYpxBLjRImcCvDQ/edit#gid=0
Kpattern <- "(?i)humerus|metacarpal|femur|astragalus|calcaneum" #(?i) makes it case insensitive
Kx <- grep(Kpattern, kitty_long$variable, value = TRUE)

kitty_sub <- kitty_long[kitty_long$variable %in% Kx,]

K2pattern <- "Femur.GLC|Femur.GL|Humerus.GLC|Humerus.GL|Metacarpal.BFp|Humerus.Bp" #check metacarpal BFp
K2x <- grep(K2pattern, kitty_sub$variable, value = TRUE)

kitty_sub2 <- kitty_sub[kitty_sub$variable %in% K2x,]

kitty_sub2$template[kitty_sub2$variable == "Femur.GLC"] <- "Femur length from trochanter"
kitty_sub2$template[kitty_sub2$variable == "Femur.GL"] <- "Femur length"
kitty_sub2$template[kitty_sub2$variable == "Humerus.GLC"] <- "Humerus length from caput"
kitty_sub2$template[kitty_sub2$variable == "Humerus.GL"] <- "Humerus length"

kitty_clean <- kitty_sub2[!(is.na(kitty_sub2$value)),]

#move modern to a different group
for(i in 1:length(kitty_clean$Period)){
  if(isTRUE(kitty_clean$Period[i] == "M" | kitty_clean$Period[i] == "F" | kitty_clean$Date[i] == "1993")){
    kitty_clean$Period[i] <- "NA"
    kitty_clean$verbatimEventDate[i] <- kitty_clean$Date[i] 
  }
  else {
    kitty_clean$verbatimEventDate[i] <- "NA"
  }
}

for(i in 1:length(kitty_clean$Period)){
  if(isTRUE(kitty_clean$Date[i] == kitty_clean$verbatimEventDate[i])){
    kitty_clean$Date[i] <- "NA"
  }
  else {
    next()
  }
}

#add units
kitty_clean$measurementUnit <- rep("mm", length(kitty_clean$value))

#rename columns
colnames(kitty_clean)[colnames(kitty_clean)=="Site"] <- "sitename"
#colnames(kitty_clean)[colnames(kitty_clean)=="EAP.Acc."] <- ""
colnames(kitty_clean)[colnames(kitty_clean)=="Provenience..field.number."] <- "contextName" #?
colnames(kitty_clean)[colnames(kitty_clean)=="ID.Catalog...cat..element.or.portion."] <- "individualID"
colnames(kitty_clean)[colnames(kitty_clean)=="Specimen.Catalog...cat..organism."] <- "catalogNumber"
#colnames(kitty_clean)[colnames(kitty_clean)=="Cantryll.Test...test..analyst.sample.number."] <- ""
colnames(kitty_clean)[colnames(kitty_clean)=="Period"] <- "culturalStratigraphyOccupationPeriod"
#colnames(kitty_clean)[colnames(kitty_clean)=="Date"] <- ""
colnames(kitty_clean)[colnames(kitty_clean)=="ID"] <- "scientificName"
colnames(kitty_clean)[colnames(kitty_clean)=="Side"] <- "measurementSide"
#colnames(kitty_clean)[colnames(kitty_clean)=="Description.completeness"] <- ""
#colnames(kitty_clean)[colnames(kitty_clean)=="Age..modern.only."] <- ""
#colnames(kitty_clean)[colnames(kitty_clean)=="Fusion"] <- ""
#colnames(kitty_clean)[colnames(kitty_clean)=="Cantryll.notes"] <- ""
colnames(kitty_clean)[colnames(kitty_clean)=="variable"] <- "measurementType"
colnames(kitty_clean)[colnames(kitty_clean)=="value"] <- "measurementValue"

kitty_clean.1 <- kitty_clean[,-10] #get rid of element type because redundant


for(i in 1:length(kitty_clean.1$Date)) {
  if(isTRUE(grepl("(?i)century", kitty_clean.1$Date[i]))) {
    kitty_clean.1$referenceSystem[i] <- "century"
  }
  else if(isTRUE(grepl("??????AD?????", kitty_clean.1$Date[i]))) {
    kitty_clean.1$referenceSystem[i] <- "AD"
  }
  else {
    kitty_clean.1$referenceSystem[i] <- "NA"
  }
}

kitty_clean.1$Date <- gsub("(?i)century|AD|th", "", kitty_clean.1$Date)
kitty_clean.1$Date <- gsub(" to ", "-", kitty_clean.1$Date)

#split dates
kitty_clean.1$minimumChronometricAge <- sapply(strsplit(as.character(kitty_clean.1$Date),'-'), "[", 1)
kitty_clean.1$maximumChronometricAge <- sapply(strsplit(as.character(kitty_clean.1$Date),'-'), "[", 2)

kitty_clean.2 <- kitty_clean.1[,-8] #get rid of date

colnames(kitty_clean.2)[colnames(kitty_clean.2)=="referenceSystem"] <- "minimumChronometricAgeReferenceSystem"
kitty_clean.2$maximumChronometricAgeReferenceSystem <- kitty_clean.2$minimumChronometricAgeReferenceSystem

colnames(kitty_clean.2)[colnames(kitty_clean.2)=="Age..modern.only."] <- "ageValue"

#write.csv(kitty_clean.2, "kitty_data.csv", row.names=FALSE)

## VertNet data
vertnet <- read.csv("https://de.cyverse.org/dl/d/338C987D-F776-4439-910F-3AD2CD1D06E2/mammals_no_bats_2019-03-13.csv", stringsAsFactors = FALSE)

#rearrange columns
#need to put catalognumber [18], lat [20], long[21], collection code [19], institution code [59], scientific name [71], locality [63], occurrence id [65]
df <- vertnet[,c(18:21,43,59,63:68,71,72,1:17,22:42,44:58,60:62,69:70,73:119)]

#select out "focused traits"
#https://docs.google.com/spreadsheets/d/1rU15rBo-JpopEqpxBXLWSqaecBXwtYpxBLjRImcCvDQ/edit#gid=0
#unneeded traits: testes [90:105]

vertnet.2 <- df[,-(90:105)]

#vertnet_sub <- vertnet.2[,vertnet.2 %in% Vx] #error: memory exhausted
#vertnet_sub2 <- cbind(needs, vertnet_sub)

#get rid of empty data
x <- length(vertnet.2$catalognumber)
half.x <- .5*x
half.x.1 <- half.x + 1
vertnet.half <- vertnet.2[1:half.x,]
vertnet.other <- vertnet.2[half.x.1:x,]

#create long version
vertnet_long.1 <- melt(vertnet.half, id.vars = 1:15) 
vertnet_long.2 <- melt(vertnet.other, id.vars = 1:15) 

vertnet_long <- rbind(vertnet_long.1, vertnet_long.2)
##NEXT: select out specific measurements / change measurement names and map to template

Vpattern <- "?1st"
Vx <- grep(Vpattern, vertnet_long$variable, value = TRUE)
vertnet.3 <- vertnet_long[vertnet_long$variable %in% Vx,]

colnames(vertnet.3)[colnames(vertnet.3)=="catalognumber"] <- "catalogNumber"
colnames(vertnet.3)[colnames(vertnet.3)=="collectioncode"] <- "collectionCode"
colnames(vertnet.3)[colnames(vertnet.3)=="decimallatitude"] <- "decimalLatitude"
colnames(vertnet.3)[colnames(vertnet.3)=="decimallongitude"] <- "decimalLongitude"
colnames(vertnet.3)[colnames(vertnet.3)=="locality"] <- "verbatimLocality"
#colnames(vertnet.3)[colnames(vertnet.3)=="maximumelevationmeters"] <- ""
#colnames(vertnet.3)[colnames(vertnet.3)=="minimumelevationmeters"] <- ""
#colnames(vertnet.3)[colnames(vertnet.3)=="occurenceid"] <- ""
#colnames(vertnet.3)[colnames(vertnet.3)=="occurenceremarks"] <- ""
#colnames(vertnet.3)[colnames(vertnet.3)=="recordedby"] <- ""
colnames(vertnet.3)[colnames(vertnet.3)=="scientificname"] <- "scientificName"
colnames(vertnet.3)[colnames(vertnet.3)=="variable"] <- "measurementType"
colnames(vertnet.3)[colnames(vertnet.3)=="value"] <- "measurementValue"

#get rid of NAs
vertnet.4 <- vernet.3[!is.na(vertnet.3$measurementValue),]



#probably want to use the gather() function from tidyverse

data %>% gather(Measurement, Value, M1:M23)