# Data Cleaning
# 
# This file compiles hospital-level information from various files into one cleaned dataset


## SETUP

# Load necessary libraries
library(tidyverse)
library(haven)
library(fitdistrplus)
library(gamlss)
library(readxl)



## WAIT TIME DATA
# Source: https://data.medicare.gov/data/archives/hospital-compare

# Read in hospital wait time data
tec18 <- read.csv("Data/Timely and Effective Care - Hospital 2018.csv", 
                  header = TRUE, 
                  na.strings = "Not Available",
                  stringsAsFactors = FALSE)

# Restrict dataset to the 50 states plus Washington, DC
tec18 <- tec18 %>% 
  filter(!State %in% c("PR", "VI", "AS", "GU", "MP")) 

# Format zip codes to include leading zero when applicable
tec18$ZIP.Code <- sapply(tec18$ZIP.Code, function(x) { if(nchar(x) < 5) {paste0(0, x)} else{x} })

# Extract each hospital's identifying information (Address, City, State, ZIP, etc)
identifying_info <- tec18[tec18$Measure.ID == "EDV", 2:8]

# Extract each hospital's emergency department volume 
# (factor variable, levels = low, medium, high, very high)
edv <- tec18$Score[tec18$Measure.ID == "EDV"]

# Now that non-numeric information has been extracted, set all non-numeric score values to NA
tec18$Score <- ifelse(grepl('[0-9]', tec18$Score), as.numeric(tec18$Score), NA)

# Store Facility IDs of all hospitals in the wait time dataset for later use
ids <- data.frame(Facility.ID = unique(tec18$Facility.ID[order(tec18$Facility.ID)]))

# Extract and store wait times
wait_times <- reshape(tec18[tec18$Measure.ID %in% c("ED_1b", "ED_2b", "OP_18b", "OP_18c", "OP_22"),c(1,10,12)],
                 timevar = "Measure.ID", idvar = "Facility.ID", direction = "wide")

# Convert "Left before being seen" from percentage to proportion
wait_times$Score.OP_22 <- wait_times$Score.OP_22 / 100


## HOSPITAL OVERALL RATINGS (1 to 5 scale)
# Source: https://data.medicare.gov/data/archives/hospital-compare

# Read in hospital ratings data
ratings <- read.csv("Data/Hospital General Information.csv", na.strings = "Not Available")

# Keep only Facility ID and Rating
ratings <- ratings[,c(1,13)]

# Format Facility.ID as an integer
ratings$Facility.ID <- as.integer(levels(ratings$Facility.ID))[ratings$Facility.ID]




## COMPLICATION AND DEATH RATES
# Source: https://data.medicare.gov/data/archives/hospital-compare

# Read in hospital-level complication/death rate data
complications <- read.csv("Data/Complications and Deaths - Hospital.csv", na.strings = "Not Available")

# Reshape data, keeping only death rates for pneumonia, acute myocardial infarction (heart attack), and stroke
complications <- reshape(complications[complications$Measure.ID %in% c("MORT_30_PN", "MORT_30_AMI", "MORT_30_STK"),c(1,9,13)],
                timevar = "Measure.ID", idvar = "Facility.ID", direction = "wide")

# Format Facility.ID as an integer
complications$Facility.ID <- as.integer(levels(complications$Facility.ID))[complications$Facility.ID]



## HOSPITAL SERVICE AREA INFORMATION
# Source: https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Hospital-Service-Area-File/Download/HSAF2018.zip

# Read in hospital service area data
serv <- read.csv("Data/2018_HSAF.csv", header = TRUE, stringsAsFactors = FALSE)

# Convert Facility ID to numeric and rename 
serv$MEDICARE_PROV_NUM <- as.numeric(serv$MEDICARE_PROV_NUM)
names(serv)[1] <- "Facility.ID"

# Remove the small number of NAs
serv <- na.omit(serv)

# Remove Puerto Rico codes (which all begin with "00")
serv <- subset(serv, ZIP_CODE > 1000)

# Format zip codes to include leading zero when applicable
serv$ZIP_CODE <- sapply(serv$ZIP_CODE, function(x) {if(nchar(x)<5){paste0(0,x)}else{x}})

# Remove irrelevant columns
serv <- serv[,-c(3,4)]


## RACE DEMOGRAPHICS BY ZIP CODE
# from 2018 American Community Survey 
# SOURCE: https://data.census.gov/cedsci/table?g=0100000US.860000&tid=ACSDP5Y2018.DP05&hidePreview=false&vintage=2018&layer=VT_2018_860_00_PY_D1&cid=DP05_0001E

# Read in demographic info by zip code
race_raw <- as.matrix(read.csv("Data/Race_by_ZIP.csv", header = FALSE, stringsAsFactors = F))

# Convert to data frame and name columns properly
race <- data.frame(race_raw[3:nrow(race_raw),])
names(race) <- race_raw[1,]
race$NAME <- substr(race$NAME, 7, 11)

# Create subset of relevant columns only and Remove Puerto Rico codes
race <- subset(race, select = c("NAME", "DP05_0033E", "DP05_0064E", "DP05_0065E", "DP05_0066E", "DP05_0067E", "DP05_0071E"),
               substr(race$NAME, 1, 2) != "00")

# Convert demographic info to proportions of White, Black, Native American, etc in each zip code 
race[,2:7] <- sapply(race[,2:7], function(x) { as.numeric(levels(x))[x] })
race[,3:7] <- sapply(race[,3:7], function(x) { x / race[,2]})
race <- race[,-2]

# Rename "NAME" as "ZIP_CODE" in preparation for merge
names(race) <- c("ZIP_CODE", "White", "Black", "NativeAmerican", "Asian", "Hispanic")


## RURAN/URBAN FLAG
rural_zips <- read_xlsx("Data/forhp-eligible-zips.xlsx")$ZIP



## MERGE DATA

# Merge Hospital Service Area data with Race Demographics using "ZIP_CODE" as merge variable
serv_by_race <- merge(serv, race, all = TRUE)
serv_by_race <- serv_by_race[serv_by_race$ZIP_CODE %in% unique(serv$ZIP_CODE),]
serv_by_race <- serv_by_race[order(serv_by_race$Facility.ID),]
serv_by_race$Rural <- ifelse(serv_by_race$ZIP_CODE %in% rural_zips, 1, 0)

str(serv_by_race)

# Set up calculation of proportions of each race served by each hospital
compute_racial_stats <- function(data){
  # Store Facility IDs
  ids <- unique(data$Facility.ID)
  # Create empty matrix to store weighted means
  scores <- matrix(nrow = length(ids), ncol = 6)
  # Compute weighted average for each race for each hospital
  for(i in 1:length(ids)){
    scores[i,] <- sapply(data[data$Facility.ID == ids[i],4:9], 
                         function(x) { weighted.mean(x, w = data[data$Facility.ID == ids[i],3], na.rm = T) })
  }
  # Output results 
  results <- data.frame(ids, scores)
  names(results) <- c("Facility.ID", "White", "Black", "NativeAmerican", "Asian", "Hispanic", "RuralScore")
  return(results)
}

# Calculate proportions of each race served by each hospital
race_stats <- compute_racial_stats(serv_by_race)

# Merge the following files together, one by one:
# - Wait Times for each hospital
# - Emergency Department Volume for each hospital
# - Overall Hospital Rating for each hospital
# - Death Rates from Pneumonia / Heart Attack / Stroke for each hospital
step1 <- merge(ids, ratings, all = TRUE)
step2 <- step1[step1$Facility.ID %in% unique(tec18$Facility.ID),]
step3 <- merge(step2, complications, all = TRUE)
step4 <- step3[step3$Facility.ID %in% unique(tec18$Facility.ID),]
step5 <- merge(step4, race_stats, all = TRUE)
step6 <- step5[step5$Facility.ID %in% unique(tec18$Facility.ID),]
step7 <- step6[order(step6$Facility.ID),]
bigmerge <- step7[,-1]
rm(step1, step2, step3, step4, step5, step6, step7)

# Use cbind() to combine all merges and data into one

hospital_data <- cbind(wait_times, bigmerge, edv, identifying_info)


# Remove rows containing no waiting time data
hospital_data <- filter(hospital_data, !is.na(Score.ED_1b) | !is.na(Score.ED_2b) | !is.na(Score.OP_18b) )

# Rename variables
names(hospital_data)[c(1:10, 17)] <- c("ID", "ArrivalToAdmit", "AdmitToRoom", "TimeInER", "MH_TimeInER", 
                          "NotSeen", "OverallRating", "HeartAttackRate", "PneumoniaRate", "StrokeRate",
                          "Volume")

str(hospital_data)

write.csv(hospital_data, "2018.csv", row.names = FALSE)


