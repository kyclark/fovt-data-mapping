# Preprocessing
The following code is for preprocessing legacy data to get it ready for the pipeline. Refer to [workflow](https://www.lucidchart.com/invitations/accept/f37251ae-abbe-4630-8192-f22350cf466a)
1. put legacy data into the discovery environment
2. update template and ontology mapping in [GitHub](https://github.com/futres/fovt-data-mapping)
3. upload new template_mapping and ontology_mapping in the discovery environment
4. map traits to ontology
5. map columns to the template 

```R
#install packages
install.packages("reshape")
install.packages("uuid")

require(reshape2)
require(uuid)
```
```R
#load mapping files
template_map <- read.csv("https://de.cyverse.org/dl/d/90DF107C-0E33-4696-8F1F-07134719C5E8/template_mapping.csv", stringsAsFactors = FALSE)
ontology_map <- read.csv("https://de.cyverse.org/dl/d/23431D1B-D2B3-4CB7-94AD-2E86008C70BE/ontology_mapping.csv", stringsAsFactors = FALSE)
template <- read.csv("https://de.cyverse.org/dl/d/105DF9B1-229D-4849-8A42-DA843240586E/template.csv", stringsAsFactors = FALSE)
```

## Ray Bernor Data
This dataset includes measurements cranial, post-cranial, and tooth measurements from Fossil Horses for extant and extinct Equid species.
This is a legacy dataset that has been actively collected and curated since 1980 by R. Bernor (currently at Howard University)

```R
#data
ray <- read.csv("https://de.cyverse.org/dl/d/16030E74-A54F-44B2-AA03-76B1A49FCA49/1.FuTRESEquidDbase_6_24_2019.csv", stringsAsFactors = FALSE) #how to point to latest data version?

#reference for bone abbreviations in the "ray" dataset
boneAbbr <- read.csv("https://de.cyverse.org/dl/d/C82D7659-5503-455B-8F7F-883DC3F1BAE0/BoneAbbr.csv", stringsAsFactors = FALSE)

#reference for locality numbers in the "ray" dataset
locality <- read.csv("https://de.cyverse.org/dl/d/736F420E-6474-45F0-82EE-6A43D1703DE2/2.LOCAL_6_24_2019FuTRESPROTECTED.csv", stringsAsFactors = FALSE)
```

```R
#get rid of protected sites
ray_safe <- subset(ray, subset = ray$PROTECTED...P != "P")
```

```R
#replace locality and country #s with actual names
for(i in 1:length(ray_safe[,1])){
  if(isTRUE(ray_safe$LOCALITY[i] != "NA")){
    ray_safe$verbatimLocality[i] <- locality$LOCALITYName[locality$LOCALITY.No == ray_safe$LOCALITY[i] & locality$COUNTRY.No == ray_safe$COUNTRY[i]]
    ray_safe$country[i] <- locality$COUNTRYName[locality$LOCALITY.No == ray_safe$LOCALITY[i] & locality$COUNTRY.No == ray_safe$COUNTRY[i]]
  }
  else{
    next()
  }
}
```

### Focused traits
We are going to focus on certain traits for now, and add in the rest of the traits in this dataset later. These traits are based off ["focused traits"](https://docs.google.com/spreadsheets/d/1rU15rBo-JpopEqpxBXLWSqaecBXwtYpxBLjRImcCvDQ/edit#gid=0)

```R
Rpattern <-  "pre?molar|metacarpal|metatarsal|1st phalanx III" #? allows it to be premolar or molar (note: need all three letters: p r e)
Rx <- grep(Rpattern, boneAbbr$Bone, value = TRUE)

boneAbbr_sub <- boneAbbr[boneAbbr$Bone %in% Rx,]

ray_sub1 <- ray_safe[ray_safe$BONE %in% boneAbbr_sub$Abbreviation,]
ray_sub2 <- subset(ray_safe, subset = c(ray_safe$BONE == "tibia" | 
                                          ray_safe$BONE == "humerus" | 
                                          ray_safe$BONE == "femur" | 
                                          ray_safe$BONE == "radius"))
ray_sub <- rbind(ray_sub1, ray_sub2)
```

```R
#create species name
ray_sub$scientificName <- paste(ray_sub$GENUS, ray_sub$SPECIES, sep = " ")

ray_sub$SPEC_ID <- gsub("^\\s+|\\s+$", "", ray_sub$SPEC_ID) #^ at beginning
ray_sub$scientificName <- gsub("  ", " ", ray_sub$scientificName)
ray_sub$scientificName <- gsub("sp..", "sp. ", ray_sub$scientificName)
ray_sub$scientificName <- gsub("^\\s+|\\s+$", "", ray_sub$scientificName) 
```

Need to create a long version of the data so that each trait has its own row.

```R
#reorder columns
ray_sub1 <- ray_sub[,c(1:12,53:55,13:51)]

#measurements are from 15:54
ray_long <- melt(ray_sub1, id.vars = c(1:15), factorsAsStrings = FALSE, variable.name = "meas.no")
#need to change variable name to something else otherwise it accidently repopulates it as measurementType.1

#select out specific measurements / change measurement names and map to template
ray_long$measurement <- paste(ray_long$BONE, ray_long$meas.no, sep = " ")

ray_long_sub <- subset(ray_long, ray_long$measurement %in% ontology_map$measurement[ontology_map$dataset == "ray"])
```

**Need to fix this section**

```R
#clean up AGE
#split dates
#Quarry should be verbatim locality
## NOTE: THESE DATES ARE NOT IN CONSISTENT ORDER - CHECK W RAY
## ALSO: FOR SINGLE AGES - WHERE DOES IT GO?
## GET RID OF "?" AND "recent"
## WHAT UNITES ARE AGES IN??
ray_long_sub$minimumChronometricAge <- sapply(strsplit(as.character(ray_long_sub$AGE),';|-|:'), "[", 1)
ray_long_sub$maximumChronometricAge <- sapply(strsplit(as.character(ray_long_sub$AGE),';|-|:'), "[", 2)
```

```R
#clean up data
#get rid of NAs
ray_clean <- ray_long_sub[!(is.na(ray_long_sub$value)),]

#for some reason variables are factors, and values are characters
ray_clean$meas.no <- as.character(ray_clean$meas.no)
ray_clean$value <- as.numeric(ray_clean$value)
```

```R
#next change names to match template
for(i in 1:length(ray_clean[,1])){
  ray_clean$measurementType[i] <- ontology_map$ontologyTerm[ontology_map$measurement == ray_clean$measurement[i]]
}

cols <- colnames(ray_clean)
x <- c()
for(i in 1:length(cols)){
  if(isTRUE(colnames(ray_clean)[i] %in% template_map$columnName)){
  colnames(ray_clean)[i] <- template_map$templateTerm[template_map$columnName == cols[i]]
  }
  else if(isTRUE(colnames(ray_clean)[i] %in% template$column)){
    colnames(ray_clean)[i] <- template$column[template$column == cols[i]]
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
```

```R
#generate UUID
for(i in 1:length(ray_clean.1[,1])){
  ray_clean.1$observationID[i] <- UUIDgenerate(use.time = NA)
}

#write.csv(ray_clean.1, "ray_data.csv", row.names = FALSE, quote = FALSE)
```

## Kitty Emery Data
This datset includes zooarcheological data for ungulates and some comparative specimens

```R
#data
kitty <- read.csv("https://de.cyverse.org/dl/d/0152B269-3942-4BC4-8FDC-E60B48B17EBD/MayaDeerMetrics_Cantryll_Emeryedits.csv", skip = 2, stringsAsFactors = FALSE)
```

```R
#create long version so each trait has its own row
#measurements are: 16:101
kitty_long <- melt(kitty, id.vars = 1:15, variable.name = "meas.no")
```

Selecting out ["focused traits"](https://docs.google.com/spreadsheets/d/1rU15rBo-JpopEqpxBXLWSqaecBXwtYpxBLjRImcCvDQ/edit#gid=0)

```R
Kpattern <- "(?i)humerus|metacarpal|femur|astragalus|calcaneum" #(?i) makes it case insensitive
Kx <- grep(Kpattern, kitty_long$meas.no, value = TRUE)

kitty_sub <- kitty_long[kitty_long$meas.no %in% Kx,]

kitty_long_sub <- subset(kitty_sub, kitty_sub$meas.no %in% ontology_map$measurement[ontology_map$dataset == "kitty"])

kitty_clean <- kitty_long_sub[!(is.na(kitty_long_sub$value)),]
```

```R
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

for(i in 1:length(kitty_clean$Date)) {
  if(isTRUE(grepl("(?i)century", kitty_clean$Date[i]))) {
    kitty_clean$referenceSystem[i] <- "century"
  }
  else if(isTRUE(grepl("??????AD?????", kitty_clean$Date[i]))) {
    kitty_clean$referenceSystem[i] <- "AD"
  }
  else {
    kitty_clean$referenceSystem[i] <- "NA"
  }
}

kitty_clean$Date <- gsub("(?i)century|AD|th", "", kitty_clean$Date)
kitty_clean$Date <- gsub(" to ", "-", kitty_clean$Date)

#split dates
kitty_clean$minimumChronometricAge <- sapply(strsplit(as.character(kitty_clean$Date),'-'), "[", 1)
kitty_clean$maximumChronometricAge <- sapply(strsplit(as.character(kitty_clean$Date),'-'), "[", 2)
```

```R
#change variable & value
kitty_clean$meas.no <- as.character(kitty_clean$meas.no)
kitty_clean$value <- as.numeric(kitty_clean$value)

#next change names to match template
for(i in 1:length(kitty_clean[,1])){
  kitty_clean$measurementType[i] <- ontology_map$ontologyTerm[ontology_map$measurement == kitty_clean$meas.no[i]]
}

cols <- colnames(kitty_clean)
x <- c()
for(i in 1:length(cols)){
  if(isTRUE(colnames(kitty_clean)[i] %in% template_map$columnName)){
    colnames(kitty_clean)[i] <- template_map$templateTerm[template_map$columnName == cols[i]]
  }
  else if(isTRUE(colnames(kitty_clean)[i] %in% template$column)){
    colnames(kitty_clean)[i] <- template$column[template$column == cols[i]]
  }
  else{
    x[i] <- colnames(kitty_clean)[i]
  }
}
z <- x[!is.na(x)]

kitty_clean.1 <- kitty_clean[,!(colnames(kitty_clean) %in% z)]
```

```R
#generate UUID
for(i in 1:length(kitty_clean.1[,1])){
  kitty_clean.1$observationID[i] <- UUIDgenerate(use.time = NA)
}

#write.csv(kitty_clean.1, "kitty_data.csv", row.names=FALSE, quote = FALSE)
```

## VertNet Data
This is all mammal data from VertNet. Selected traits are: hindfoot length, ear length, body length, & body mass.

```R
#data
vertnet <- read.csv("https://de.cyverse.org/dl/d/338C987D-F776-4439-910F-3AD2CD1D06E2/mammals_no_bats_2019-03-13.csv", 
                    stringsAsFactors = FALSE, nrows = 100)
```

Select out ["focused traits"](https://docs.google.com/spreadsheets/d/1rU15rBo-JpopEqpxBXLWSqaecBXwtYpxBLjRImcCvDQ/edit#gid=0)

```R
#unneeded traits: testes [90:105]
vertnet.2 <- vertnet[,-(90:105)]
```
```R
#rearrange columns
#need to put catalognumber [18], lat [20], long[21], collection code [19], institution code [59], scientific name [71], locality [63], occurrence id [65]
df <- vertnet.2[,c(18:21,43,59,63:69,71,72,1:17,22:42,44:58,60:62,70,73:103)]

#create long version
vertnet_long <-  melt(df, id.vars = 1:15, variable.name = "meas.no")
```

```R
##NEXT: select out specific measurements / change measurement names and map to template
Vpattern <- "X1st_"
Vx <- grep(Vpattern, vertnet_long$meas.no, value = TRUE)
vertnet_long_sub <- vertnet_long[vertnet_long$meas.no %in% Vx,]
vertnet_long_sub$meas.no <- gsub(Vpattern, "", vertnet_long_sub$meas.no)

#create new column for unit type that matches with id and measurement type
V1pattern <- "?????????????????????_high|?????????????????????_low|?????????????????????_ambiguous|?????????????????????_estimated"
V1x <- grep(V1pattern, vertnet_long_sub$meas.no, value = TRUE)
vertnet_long_sub.1 <- vertnet_long_sub[!(vertnet_long_sub$meas.no %in% V1x),]


for(i in 1:length(vertnet_long_sub.1[,1])){
  if(isTRUE(vertnet_long_sub.1$value[vertnet_long_sub.1$meas.no == "sex_notation"][i] != "")){
    vertnet_long_sub.1$sex[i] <- vertnet_long_sub.1$value[vertnet_long_sub.1$meas.no == "sex_notation"][i]
  }
  if(isTRUE(grepl(V2pattern, vertnet_long_sub$meas.no[i]))){
    x <- grep(V2pattern, vertnet_long_sub$meas.no[i], value = TRUE)
    vertnet_long_sub$measurementUnit[i] <- x
  }
  if(isTRUE(vertnet_long_sub.1$value[vertnet_long_sub.1$meas.no == "life_stage_notation"][i] != "")){
    vertnet_long_sub$lifeStage[i] <- vertnet_long_sub.1$value[vertnet_long_sub.1$meas.no == "life_stage_notation"][i]
  }
  else {
    next()
  }
}
```

```R
#clean data
#get rid of NAs
vertnet_clean <- vertnet_long_sub[!is.na(vertnet_long_sub$value),]
```

```R
#next change names to match template
for(i in 1:length(vertnet_clean[,1])){
  vertnet_clean$measurementType[i] <- ontology_map$ontologyTerm[ontology_map$measurement == vertnet_clean$meas.no[i]]
}

cols <- colnames(vertnet_clean)
x <- c()
for(i in 1:length(cols)){
  if(isTRUE(colnames(vertnet_clean)[i] %in% template_map$columnName)){
    colnames(vertnet_clean)[i] <- template_map$templateTerm[template_map$columnName == cols[i]]
  }
  else if(isTRUE(colnames(vertnet_clean)[i] %in% template$column)){
    colnames(vertnet_clean)[i] <- template$column[template$column == cols[i]]
  }
  else{
    x[i] <- colnames(vertnet_clean)[i]
  }
}
z <- x[!is.na(x)]

vertnet_clean.1 <- vertnet_clean[,!(colnames(vertnet_clean) %in% z)]
```

```R
#generate UUID
for(i in 1:length(vertnet_clean.1 [,1])){
  vertnet_clean.1$observationID[i] <- UUIDgenerate(use.time = NA)
}

#write.csv(vertnet_clean.1, "vertnet_data.csv", rownames = FALSE)
```
