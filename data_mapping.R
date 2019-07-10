require(reshape2)

template <- read.csv("template_mapping.csv", stringsAsFactors = FALSE)
ontology <- read.csv("ontology_mapping.csv", stringsAsFactors = FALSE)

## Ray's Data
ray <- read.csv("https://de.cyverse.org/dl/d/16030E74-A54F-44B2-AA03-76B1A49FCA49/1.FuTRESEquidDbase_6_24_2019.csv", stringsAsFactors = FALSE) #how to point to latest data version?
boneAbbr <- read.csv("https://de.cyverse.org/dl/d/C82D7659-5503-455B-8F7F-883DC3F1BAE0/BoneAbbr.csv", stringsAsFactors = FALSE)
locality <- read.csv("https://de.cyverse.org/dl/d/736F420E-6474-45F0-82EE-6A43D1703DE2/2.LOCAL_6_24_2019FuTRESPROTECTED.csv", stringsAsFactors = FALSE)

#get rid of protected sites
ray_safe <- subset(ray, subset = ray$PROTECTED...P != "P")

#replace locality and country #s with actual names
for(i in 1:length(ray_safe[,1])){
  if(isTRUE(ray_safe$LOCALITY[i] != "NA")){
    ray_safe$LOCALITYName[i] <- locality$LOCALITYName[locality$LOCALITY.No == ray_safe$LOCALITY[i] & locality$COUNTRY.No == ray_safe$COUNTRY[i]]
    ray_safe$COUNTRYName[i] <- locality$COUNTRYName[locality$LOCALITY.No == ray_safe$LOCALITY[i] & locality$COUNTRY.No == ray_safe$COUNTRY[i]]
  }
  else{
    next()
  }
}

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

#create species name
ray_sub$binomial <- paste(ray_sub$GENUS, ray_sub$SPECIES, sep = " ")

ray_sub$SPEC_ID <- gsub("^\\s+|\\s+$", "", ray_sub$SPEC_ID)

#reorder columns
ray_sub1 <- ray_sub[,c(1:12,53:55,13:51)]

#measurements are from 15:54
ray_long <- melt(ray_sub1, id.vars = c(1:15), factorsAsStrings = FALSE)

#select out specific measurements / change measurement names and map to template
ray_long$measurement <- paste(ray_long$BONE, ray_long$variable, sep = " ")

ray_long_sub <- subset(ray_long, ray_long$measurement %in% ontology$measurement[ontology$dataset == "ray"])

#clean up AGE
#split dates
## NOTE: THESE DATES ARE NOT IN CONSISTENT ORDER - CHECK W RAY
## ALSO: FOR SINGLE AGES - WHERE DOES IT GO?
## GET RID OF "?" AND "recent"
## WHAT UNITES ARE AGES IN??
ray_long_sub$ageMIN <- sapply(strsplit(as.character(ray_long_sub$AGE),';|-|:'), "[", 1)
ray_long_sub$ageMAX <- sapply(strsplit(as.character(ray_long_sub$AGE),';|-|:'), "[", 2)

#get rid of NAs
ray_clean <- ray_long_sub[!(is.na(ray_long_sub$value)),]

#for some reason variables are factors, and values are characters
ray_clean$variable <- as.character(ray_clean$variable)
ray_clean$value <- as.numeric(ray_clean$value)

#next change names to match template
for(i in 1:length(ray_clean[,1])){
  ray_clean$variable[i] <- ontology$ontologyTerm[ontology$measurement == ray_clean$measurement[i]]
}

cols <- colnames(ray_clean)
x <- c()
for(i in 1:length(cols)){
  if(isTRUE(colnames(ray_clean)[i] %in% template$columnName)){
  colnames(ray_clean)[i] <- template$templateTerm[template$columnName == cols[i]]
  }
  else{
    x[i] <- colnames(ray_clean)[i]
  }
}
z <- x[!is.na(x)]

ray_clean.1 <- ray_clean[,!(colnames(ray_clean) %in% z)]

#add missing columns
ray_clean.1$individualID <- ray_clean.1$materialSampleID
ray_clean.1$measurementUnit <- rep("mm", length(ray_clean.1[1]))

#write.csv(ray_clean.1, "ray_data.csv", row.names=TRUE)

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

kitty_sub2$template[kitty_sub2$variable == "Femur.GLC"] <- "{line of medial condyle of femur to greater trochanter}"
kitty_sub2$template[kitty_sub2$variable == "Femur.GL"] <- "{femur length}"
kitty_sub2$template[kitty_sub2$variable == "Humerus.GLC"] <- "{line trochlea of humerus to caput of humerus}"
kitty_sub2$template[kitty_sub2$variable == "Humerus.GL"] <- "{humerus length}"

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
kitty_clean$materialSampleID <- kitty_clean$individualID

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

#select out "focused traits"
#https://docs.google.com/spreadsheets/d/1rU15rBo-JpopEqpxBXLWSqaecBXwtYpxBLjRImcCvDQ/edit#gid=0

#unneeded traits: testes [90:105]
vertnet.2 <- vertnet[,-(90:105)]

#rearrange columns
#need to put catalognumber [18], lat [20], long[21], collection code [19], institution code [59], scientific name [71], locality [63], occurrence id [65]
df <- vertnet.2[,c(18:21,43,59,63:69,71,72,1:17,22:42,44:58,60:62,70,73:103)]

#vertnet_sub <- vertnet.2[,vertnet.2 %in% Vx] #error: memory exhausted
#vertnet_sub2 <- cbind(needs, vertnet_sub)

#get rid of empty data
x <- length(df$catalognumber)
index <- seq(1, length(x), 50000)
steps <- length(index)

for(i in 1:length((index-1))){
  assign(paste('X',i,sep=''),df) <- df[i:i+1,]
}


index.1 <- quarter+1
index.2 <- quarter*2
index.3 <- index.2+1
index.4 <- quarter*3
index.5 <- index.4+1

first.quarter <- vertnet.2[1:quarter,]
second.quarter <- vertnet.2[index.1:index.2,]
third.quarter <- vertnet.2[index.3:index.4,]
fourth.quarter <- vertnet.2[index.5:quarter,]

#create long version
vertnet_long.1 <- melt(first.quarter, id.vars = 1:12) 
vertnet_long.2 <- melt(second.quarter, id.vars = 1:12) 
vertnet_long.3 <- melt(third.quarter, id.vars = 1:12)
vertnet_long.4 <- melt(fourth.quarter, id.vars = 1:12)

vertnet_long <- rbind(vertnet_long.1, vertnet_long.2, vertnet.3, vertnet.4)
##NEXT: select out specific measurements / change measurement names and map to template

Vpattern <- "?1st_"
Vx <- grep(Vpattern, vertnet_long$variable, value = TRUE)
vertnet.3 <- vertnet_long[vertnet_long$variable %in% Vx,]
vertnet.4 <- gsub(Vpattern, "", vertnet.3)

colnames(vertnet.4)[colnames(vertnet.4)=="catalognumber"] <- "catalogNumber"
colnames(vertnet.4)[colnames(vertnet.4)=="collectioncode"] <- "collectionCode"
colnames(vertnet.4)[colnames(vertnet.4)=="decimallatitude"] <- "decimalLatitude"
colnames(vertnet.4)[colnames(vertnet.4)=="decimallongitude"] <- "decimalLongitude"
colnames(vertnet.4)[colnames(vertnet.4)=="locality"] <- "verbatimLocality"
colnames(vertnet.4)[colnames(vertnet.4)=="maximumelevationmeters"] <- "maximumElevationInMeters"
colnames(vertnet.4)[colnames(vertnet.4)=="minimumelevationmeters"] <- "minimumElevationInMeters"
#colnames(vertnet.4)[colnames(vertnet.4)=="occurrenceid"] <- ""
#colnames(vertnet.4)[colnames(vertnet.4)=="occurrenceremarks"] <- ""
#colnames(vertnet.4)[colnames(vertnet.4)=="recordedby"] <- ""
colnames(vertnet.4)[colnames(vertnet.4)=="scientificname"] <- "scientificName"
colnames(vertnet.4)[colnames(vertnet.4)=="variable"] <- "measurementType"
colnames(vertnet.4)[colnames(vertnet.4)=="value"] <- "measurementValue"
#colnames(vertnet.4)[colnames(vertnet.4)=="references"] <- "references"

#get rid of NAs
vertnet.5 <- vernet.4[!is.na(vertnet.4$measurementValue),]

#create new column for unit type that matches with id and measurement type
V2pattern <- "?????????????_units_inferred"
V2x <- grep(V2pattern, vertnet.5$measurementType, value = TRUE)
vertnet_sub.1 <- vertnet.5[vertnet.5$measurementType %in% V2x,]
vertnet_sub.2 <- vertnet.5[!(vertnet.5$measurementType %in% V2x),]

colnames(vertnet_sub.1)[colnames(vertnet_sub.1)=="measurementValue"] <- "measurementUnit"
#get rid of extra column
vertnet_sub.3 <- vertnet_sub.1[,-("measurementType")]

#check that sub.1 and sub.2 have the same number of rows

#check that occurenceid is unique 
U <- length(unique(vertnet.5$occurrenceid))
O <- length(vertnet.5$occurrenceid)
U == O
vertnet.6 <- merge(vertnet_sub.3, vertnet_sub.2, by = "occurrenceid", all.x = TRUE, all.y = TRUE)
#check that have original length
M <- length(vertnet.6$occurrenceid)
M == O

#change names
for(i in 1:length(vertnet.6$measurementType)){
  if(isTRUE(vertnet.6$measurementType[i] == "total_length")){
    vertnet.6$measurementType[i] <- "{full body length}"
  }
  else if(isTRUE(vertnet.6$measurementType[i] =="ear_length")){
    vertnet.6$measurementType[i] <- "ear length"
  }
  else if(isTRUE(vertnet.6$measurementType[i] == "body_mass")){
    vertnet.6$measurementType[i] <- "body mass"
  }
}


#write.csv(vertnet.4, "vertnet_data.csv", rownames = FALSE)

#probably want to use the gather() function from tidyverse

data %>% gather(Measurement, Value, M1:M23)