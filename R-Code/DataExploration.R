# Nonlinear Models of Race-Related Disparities in Emergency Department Length of Stay at U.S. Hospitals
# Jonathan M. Wall
# Dissertation, Summer 2020

### Data Exploration
### This file conducts exploratory data analysis, examines missing data, and produces tables and figures

### Load necessary libraries
library(descr)
library(htmlTable)
library(kableExtra)
library(knitr)
library(magick)
library(MASS)
library(RColorBrewer)
library(tidyverse)

### Read in data
hospital_data <- read.csv("CleanedData.csv")

### Format variables
hospital_data$MedicaidExpansion <- factor(hospital_data$MedicaidExpansion, levels = c("Yes", "No"))
hospital_data$ED.Volume <- factor(hospital_data$ED.Volume, levels = c("Low", "Medium", "High", "Very High"))
hospital_data$UrbanFlag <- ifelse(hospital_data$RuralScore > 0.5, "0", "1")
hospital_data$UrbanFlag <- factor(hospital_data$UrbanFlag, labels = c("Rural", "Urban"))
hospital_data$BedsFlag <- ifelse(hospital_data$Beds > 250, "2", ifelse(hospital_data$Beds > 25, "1", "0"))
hospital_data$BedsFlag <- factor(hospital_data$BedsFlag, labels = c("0-25", "26-250", "251+"))
hospital_data$AgeFlag <- ifelse(hospital_data$MedianAge >= 44, "2", ifelse(hospital_data$MedianAge >= 37, "1", "0"))
hospital_data$AgeFlag <- factor(hospital_data$AgeFlag, labels = c("0-37", "37-44", "44+"))

### Define color palattes
volume.colors <- c("#FFCC00", "#FF9900", "#FF6600", "#FF3300")
rating.colors <- brewer.pal(n = 5, name = "Blues")
medex.colors <- c("#00FF99", "#006633")
rural.colors <- c("#FFCCCC", "#FF6666")
missing.colors <- c("#333399", "#2299FF")
beds.colors <- brewer.pal(n = 3, name = "GnBu")
age.colors <- brewer.pal(n = 3, name = "Purples")

### Summaries
# Response variables
resp.desc <- cbind(sapply(hospital_data[,2:5], function(x){ 
  sub("^0?", "", sprintf("%.02d:%.02d", summary(x) %/% 60, round(summary(x) %% 60)) ) }), 
  LWBSpercent = round(summary(hospital_data$LWBSrate), digits = 4) * 100)[1:6,]
# Race covariates
race.desc <- sapply(hospital_data[,c(10,8,12,9,11)] * 100, function(x){round(summary(x), digits = 1)})
# Demographic and Hospital-level covariates
cov.desc <- cbind(Beds = round(summary(hospital_data[,18])[1:6]),
                  MedianAge = round(summary(hospital_data[,15]), digits = 1),
                  Rating = round(summary(hospital_data[,7])[1:6], digits = 1),
                  RuralPct = round(summary(hospital_data[,17] * 100), digits = 1),
                  SexRatio = round(summary(hospital_data[,16]), digits = 1))
# Response variables by State
state.summaries <- data.frame(
  summarise(group_by(hospital_data, State), 
            AdmitLOS = sub("^0?", "", sprintf("%.02d:%.02d", 
                                              round(mean(AdmitLOS, na.rm = T)) %/% 60, 
                                              round(mean(AdmitLOS, na.rm = T)) %% 60) ) ,
            WaitForBed = sub("^0?", "", sprintf("%.02d:%.02d", 
                                                round(mean(WaitForBed, na.rm = T)) %/% 60, 
                                                round(mean(WaitForBed, na.rm = T)) %% 60) ) ,
            NonAdmitLOS = sub("^0?", "", sprintf("%.02d:%.02d", 
                                                 round(mean(NonAdmitLOS, na.rm = T)) %/% 60, 
                                                 round(mean(NonAdmitLOS, na.rm = T)) %% 60) ) ,
            MHLOS = sub("^0?", "", sprintf("%.02d:%.02d", 
                                           round(mean(MHLOS, na.rm = T)) %/% 60, 
                                           round(mean(MHLOS, na.rm = T)) %% 60) ) ,
            LWBSpercent = round(mean(LWBSrate * 100, na.rm = T), digits = 2),
            MeanRating = round(mean(HospitalRating, na.rm = T), 2)))

### Correlations
# Inspect correlations between numeric variables
corrs <- round(cor(hospital_data[,c(2:12, 15:18)], use = "pairwise.complete.obs"), digits = 3)
pairs(hospital_data[,c(2:12, 15:18)])

### Histograms
# Response variables
truehist(hospital_data$AdmitLOS / 60, prob = F, xlim = c(0,14), xlab = "Time (Hours)", 
               ylab = "Number of Hospitals", main = "ED LOS: Admitted Patients")
truehist(hospital_data$WaitForBed / 60, prob = F, xlim = c(0,8.5), xlab = "Time (Hours)", 
               ylab = "Number of Hospitals", main = "Time Spent Waiting for Inpatient Bed")
truehist(hospital_data$NonAdmitLOS / 60, prob = F, xlim = c(0,6), xlab = "Time (Hours)", 
               ylab = "Number of Hospitals", main = "ED LOS: Non-Admitted Patients")
truehist(hospital_data$MHLOS /60, prob = F, xlim = c(0,18),  xlab = "Time (Hours)", 
               ylab = "Number of Hospitals", main = "ED LOS: Mental Health Patients")
truehist(hospital_data$LWBSrate * 100, prob = F, xlim = c(0,12), xlab = "Percentage", 
               ylab = "Number of Hospitals", main = "% of Patients who left ED without being seen")
# Race covariates
truehist(hospital_data$Asian * 100, main = "% of Asian / Pacific Islander Patients served", 
         xlab = "Percentage", ylab = "Number of Hospitals", xlim = c(0, 40), prob = F)
truehist(hospital_data$Black * 100, main = "% of Black / African American Patients served", 
         xlab = "Percentage", ylab = "Number of Hospitals", prob = F)
truehist(hospital_data$Hispanic * 100, main = "% of Hispanic Patients served", 
         xlab = "Percentage", ylab = "Number of Hospitals", prob = F)
truehist(hospital_data$NativeAmerican * 100, main = "% of Native American Patients served", 
         xlab = "Percentage", ylab = "Number of Hospitals", xlim = c(0, 30), prob = F)
truehist(hospital_data$White * 100, main = "% of White Patients served", 
         xlab = "Percentage", ylab = "Number of Hospitals", prob = F)
# Demographic covariates
hist(na.omit(hospital_data$RuralScore) * 100, col = 5, breaks = 50, main = "% of Patients from Rural area", 
     xlab = "Percentage", ylab = "Number of Hospitals")
truehist(hospital_data$MedianAge, main = "Median Age of Patients Served", 
         xlab = "Age", ylab = "Number of Hospitals", prob = F)
truehist(hospital_data$SexRatio, main = "Sex Ratio (Men per 100 Women)",
         xlab = "Men per 100 Women", ylab = "Number of Hospitals", xlim = c(60, 150), prob = F)
medex.plot1 <- plot(na.omit(hospital_data$MedicaidExpansion), 
                    main = "Is Hospital in State that expanded Medicaid?", ylab = "Number of Hospitals", 
                    ylim = c(0, length(na.omit(hospital_data$MedicaidExpansion))), col = medex.colors)
text(x = medex.plot1, y = table(hospital_data$MedicaidExpansion), 
     labels = paste0(as.character(round(table(hospital_data$MedicaidExpansion) * 100 / 
                                          length(hospital_data$MedicaidExpansion), 1)), "%"), pos = 3)
# Hospital-level covariates
truehist(hospital_data$Beds, xlim = c(0, 1500), main = "Beds per Hospital", 
         xlab = "Beds", ylab = "Number of Hospitals", prob = F)
rating.plot1 <- plot(na.omit(as.factor(hospital_data$HospitalRating)), col = rating.colors, 
     xlab = "Rating", ylab = "Number of Hospitals", main = "Hospital Rating (1 to 5 scale)", ylim = c(0,1300))
text(x = rating.plot1, y = table(as.factor(hospital_data$HospitalRating)), 
     labels = paste0(as.character(round(table(hospital_data$HospitalRating) * 100 / 
                                          length(hospital_data$HospitalRating), 1)), "%"), pos = 3)
volume.plot1 <- plot(na.omit(hospital_data$ED.Volume), col = volume.colors, ylim = c(0, 2000),
     xlab = "Volume Category", ylab = "Number of Hospitals", main = "Emergency Department Volume")
text(x = volume.plot1, y = table(hospital_data$ED.Volume), 
     labels = paste0(as.character(round(table(hospital_data$ED.Volume) * 100 / 
                                          length(hospital_data$ED.Volume), 1)), "%"), pos = 3)


### Plots of response variables by covariates
# Emergency Department Volume
boxplot(AdmitLOS / 60 ~ ED.Volume, data = hospital_data, ylim = c(0, 1000 / 60), col = volume.colors, 
        ylab = "ED LOS: Admitted Patients (Hours)", main = "AdmitLOS by Emergency Department Volume", xlab = "")
boxplot(WaitForBed / 60 ~ ED.Volume, data = hospital_data, ylim = c(0, 700 / 60), col = volume.colors, 
        ylab = "Hours Spent Waiting for Inpatient Bed", main = "WaitForBed by Emergency Department Volume", xlab = "")
boxplot(NonAdmitLOS / 60 ~ ED.Volume, data = hospital_data, col = volume.colors, 
        ylab = "ED LOS: Non-Admitted Patients (Hours)", main = "NonAdmitLOS by Emergency Department Volume", xlab = "")
boxplot(MHLOS / 60 ~ ED.Volume, data = hospital_data, ylim = c(0, 1000 / 60), col = volume.colors, 
        ylab = "ED LOS: Mental Health Patients (Hours)", main = "MHLOS by Emergency Department Volume", xlab = "")
boxplot(LWBSrate * 100 ~ ED.Volume, data = hospital_data, ylim = c(0, 10), col = volume.colors, 
        ylab = "% of Patients who left ED without being seen", main = "LWBSrate by Emergency Department Volume", xlab = "")
# Hospital Rating
boxplot(AdmitLOS / 60 ~ HospitalRating, data = hospital_data, ylim = c(0, 1000 / 60), col = rating.colors, 
        ylab = "ED LOS: Admitted Patients (Hours)", main = "AdmitLOS by Hospital Rating", xlab = "")
boxplot(WaitForBed / 60 ~ HospitalRating, data = hospital_data, ylim = c(0, 700 / 60), col = rating.colors, 
        ylab = "Hours Spent Waiting for Inpatient Bed", main = "WaitForBed by Hospital Rating", xlab = "")
boxplot(NonAdmitLOS / 60 ~ HospitalRating, data = hospital_data, col = rating.colors, 
        ylab = "ED LOS: Non-Admitted Patients (Hours)", main = "NonAdmitLOS by Hospital Rating", xlab = "")
boxplot(MHLOS / 60 ~ HospitalRating, data = hospital_data, ylim = c(0, 1000 / 60), col = rating.colors, 
        ylab = "ED LOS: Mental Health Patients (Hours)", main = "MHLOS by Hospital Rating", xlab = "")
boxplot(LWBSrate * 100 ~ HospitalRating, data = hospital_data, ylim = c(0, 10), col = rating.colors, 
        ylab = "% of Patients who left ED without being seen", main = "LWBSrate by Hospital Rating", xlab = "")
# Rural / Urban Status
boxplot(AdmitLOS / 60 ~ UrbanFlag, data = hospital_data, ylim = c(0,1000 / 60), col = rural.colors, 
        ylab = "ED LOS: Admitted Patients (Hours)", main = "AdmitLOS by Population Served", xlab = "")
boxplot(WaitForBed / 60 ~ UrbanFlag, data = hospital_data, ylim = c(0,700 / 60), col = rural.colors,
        ylab = "Hours Spent Waiting for Inpatient Bed", main = "WaitForBed by Population Served", xlab = "")
boxplot(NonAdmitLOS / 60 ~ UrbanFlag, data = hospital_data, col = rural.colors, 
        ylab = "ED LOS: Non-Admitted Patients (Hours)", main = "NonAdmitLOS by Population Served", xlab = "")
boxplot(MHLOS / 60 ~ UrbanFlag, data = hospital_data, ylim = c(0,1000 / 60), col = rural.colors,
        ylab = "ED LOS: Mental Health Patients (Hours)", main = "MHLOS by Population Served", xlab = "")
boxplot(LWBSrate * 100 ~ UrbanFlag, data = hospital_data, ylim = c(0,10), col = rural.colors, 
        ylab = "% of Patients who left ED without being seen", main = "LWBSrate by Population Served", xlab = "")
# Is hospital located in State that expanded Medicaid?
boxplot(AdmitLOS / 60 ~ MedicaidExpansion, data = hospital_data, ylim = c(0, 900 / 60), 
        col = medex.colors, ylab = "ED LOS: Admitted Patients (Hours)", 
        main = "AdmitLOS by Medicaid Expansion Status", xlab = "")
boxplot(WaitForBed / 60 ~ MedicaidExpansion, data = hospital_data, ylim = c(0, 700 / 60), 
        col = medex.colors, ylab = "Hours Spent Waiting for Inpatient Bed", 
        main = "WaitForBed by Medicaid Expansion Status", xlab = "")
boxplot(NonAdmitLOS / 60 ~ MedicaidExpansion, data = hospital_data, 
        col = medex.colors, ylab = "ED LOS: Non-Admitted Patients (Hours)", 
        main = "NonAdmitLOS by Medicaid Expansion Status", xlab = "")
boxplot(MHLOS / 60 ~ MedicaidExpansion, data = hospital_data, ylim = c(0, 1000 / 60), 
        col = medex.colors,ylab = "ED LOS: Mental Health Patients (Hours)", 
        main = "MHLOS by Medicaid Expansion Status", xlab = "")
boxplot(LWBSrate * 100 ~ MedicaidExpansion, data = hospital_data, ylim = c(0, 10), col = medex.colors, 
        ylab = "% of Patients who left ED without being seen", 
        main = "LWBSrate by Medicaid Expansion Status", xlab = "")
# Beds
boxplot(AdmitLOS / 60 ~ BedsFlag, data = hospital_data, ylim = c(0, 900 / 60), 
        col = beds.colors, ylab = "ED LOS: Admitted Patients (Hours)", 
        main = "AdmitLOS by Beds per Hospital", xlab = "")
boxplot(WaitForBed / 60 ~ BedsFlag, data = hospital_data, ylim = c(0, 700 / 60), 
        col = beds.colors, ylab = "Hours Spent Waiting for Inpatient Bed", 
        main = "WaitForBed by Beds per Hospital", xlab = "")
boxplot(NonAdmitLOS / 60 ~ BedsFlag, data = hospital_data, 
        col = beds.colors, ylab = "ED LOS: Non-Admitted Patients (Hours)", 
        main = "NonAdmitLOS by Beds per Hospital", xlab = "")
boxplot(MHLOS / 60 ~ BedsFlag, data = hospital_data, ylim = c(0, 1000 / 60), 
        col = beds.colors,ylab = "ED LOS: Mental Health Patients (Hours)", 
        main = "MHLOS by Beds per Hospital", xlab = "")
boxplot(LWBSrate * 100 ~ BedsFlag, data = hospital_data, ylim = c(0, 10), col = beds.colors, 
        ylab = "% of Patients who left ED without being seen", 
        main = "LWBSrate by Beds per Hospital", xlab = "")
# Median Age
boxplot(AdmitLOS / 60 ~ AgeFlag, data = hospital_data, ylim = c(0, 900 / 60), 
        col = age.colors, ylab = "ED LOS: Admitted Patients (Hours)", 
        main = "AdmitLOS by Median Age of Patients Served", xlab = "")
boxplot(WaitForBed / 60 ~ AgeFlag, data = hospital_data, ylim = c(0, 700 / 60), 
        col = age.colors, ylab = "Hours Spent Waiting for Inpatient Bed", 
        main = "WaitForBed by Median Age of Patients Served", xlab = "")
boxplot(NonAdmitLOS / 60 ~ AgeFlag, data = hospital_data, 
        col = age.colors, ylab = "ED LOS: Non-Admitted Patients (Hours)", 
        main = "NonAdmitLOS by Median Age of Patients Served", xlab = "")
boxplot(MHLOS / 60 ~ AgeFlag, data = hospital_data, ylim = c(0, 1000 / 60), 
        col = age.colors,ylab = "ED LOS: Mental Health Patients (Hours)", 
        main = "MHLOS by Median Age of Patients Served", xlab = "")
boxplot(LWBSrate * 100 ~ AgeFlag, data = hospital_data, ylim = c(0, 10), col = age.colors, 
        ylab = "% of Patients who left ED without being seen", 
        main = "LWBSrate by Median Age of Patients Served", xlab = "")


### Exploring NA values
# Read in data
raw_data <- read.csv("RawData.csv")
# Format variables
raw_data$MedicaidExpansion <- as.factor(raw_data$MedicaidExpansion)
raw_data$ED.Volume <- factor(raw_data$ED.Volume, levels = c("Low", "Medium", "High", "Very High"))
raw_data
# Build data subsets
alos.na <- raw_data[is.na(raw_data$AdmitLOS),]
wfb.na <- raw_data[is.na(raw_data$WaitForBed),]
nlos.na <- raw_data[is.na(raw_data$NonAdmitLOS),]
mhlos.na <- raw_data[is.na(raw_data$MHLOS),]
lwbs.na <- raw_data[is.na(raw_data$LWBSrate),]
volume.na <- raw_data[is.na(raw_data$ED.Volume),]
rating.na <- raw_data[is.na(raw_data$HospitalRating),]
beds.na <- raw_data[is.na(raw_data$Beds),]
na.data <- list(alos.na, wfb.na, nlos.na, mhlos.na, 
                lwbs.na, volume.na, rating.na, beds.na)
alos.valid <- raw_data[!is.na(raw_data$AdmitLOS),]
wfb.valid <- raw_data[!is.na(raw_data$WaitForBed),]
nlos.valid <- raw_data[!is.na(raw_data$NonAdmitLOS),]
mhlos.valid <- raw_data[!is.na(raw_data$MHLOS),]
lwbs.valid <- raw_data[!is.na(raw_data$LWBSrate),]
volume.valid <- raw_data[!is.na(raw_data$ED.Volume),]
rating.valid <- raw_data[!is.na(raw_data$HospitalRating),]
beds.valid <- raw_data[!is.na(raw_data$Beds),]
valid.data <- list(alos.valid, wfb.valid, nlos.valid, mhlos.valid, 
                   lwbs.valid, volume.valid, rating.valid, beds.valid)
# Build tables of missing data rates
missing.rates <- data.frame(Variable = names(raw_data)[c(2:6, 19, 7, 18)], 
                      Valid = sapply(valid.data, nrow),      
                      NAtotal = sapply(na.data, nrow),
                      NApercent = round(sapply(na.data, function(x){nrow(x) * 100 / nrow(hospital_data)}), digits = 1))
colnames(missing.rates) <- c("Variable", "N (Valid)","N (Missing)", "% Missing")
hosp.nas <- rbind(Missing.ru = round(sapply(na.data, function(x){mean(na.omit(x$RuralScore))}), digits = 2) * 200, 
           Valid.ru = round(sapply(valid.data, function(x){mean(na.omit(x$RuralScore))}), digits = 2) * 200,
           Missing.bed = round(sapply(na.data, function(x){mean(na.omit(x$Beds))}), digits = 2), 
           Valid.bed = round(sapply(valid.data, function(x){mean(na.omit(x$Beds))}), digits = 2))
demo.nas <- rbind(Missing.a = round(sapply(na.data, function(x){mean(na.omit(x$Asian))}) * 100, digits = 2), 
            Valid.a = round(sapply(valid.data, function(x){mean(na.omit(x$Asian))}) * 100, digits = 2),
            Missing.b = round(sapply(na.data, function(x){mean(na.omit(x$Black))}) * 100, digits = 2), 
           Valid.b = round(sapply(valid.data, function(x){mean(na.omit(x$Black))}) * 100, digits = 2),
           Missing.h = round(sapply(na.data, function(x){mean(na.omit(x$Hispanic))}) * 100, digits = 2), 
           Valid.h = round(sapply(valid.data, function(x){mean(na.omit(x$Hispanic))}) * 100, digits = 2),
           Missing.n = round(sapply(na.data, function(x){mean(na.omit(x$NativeAmerican))}) * 100, digits = 2), 
           Valid.n = round(sapply(valid.data, function(x){mean(na.omit(x$NativeAmerican))}) * 100, digits = 2))
hosp <- data.frame(Percent = as.vector(hosp.nas), Status = rep(c("Missing", "Valid"), 10), 
                    Covariate = rep(c("RuralScore", "Beds"), each = 2, times = 5), 
                    Response = rep(c("AdmitLOS", "WaitForBed", "NonAdmitLOS", "MHLOS", 
                                     "LWBSrate"), each = 4))
hosp.alos <- hosp[hosp$Response == "AdmitLOS",]
hosp.wfb <- hosp[hosp$Response == "WaitForBed",]
hosp.nlos <- hosp[hosp$Response == "NonAdmitLOS",]
hosp.mhlos <- hosp[hosp$Response == "MHLOS",]
hosp.lwbs <- hosp[hosp$Response == "LWBSrate",]
# Plots of missing data rates
par(mar = c(5,5,4,5))
barplot(xtabs(Percent ~ Status + Covariate, data = hosp.alos), beside = T, legend = c("Missing", "Valid"), 
        main = "Data Presence by Size and Population Served", xlab = "Response Variable: ED LOS, Admitted Patients",
        ylab = "Number of Beds per Hospital", col = missing.colors)
axis(side = 4, at = c(0,50,100,150), labels = c("0", "25", "50", "75"))
mtext(side = 4, line = 2.75, "% of Patients from Rural area")
par(mar = c(5,5,4,5))
barplot(xtabs(Percent ~ Status + Covariate, data = hosp.wfb), beside = T, legend = c("Missing", "Valid"), 
        main = "Data Presence by Size and Population Served", xlab = "Response Variable: Time Spent Waiting for Inpatient Bed",
        ylab = "Number of Beds per Hospital", col = missing.colors)
axis(side = 4, at = c(0,50,100,150), labels = c("0", "25", "50", "75"))
mtext(side = 4, line = 2.75, "% of Patients from Rural area")
par(mar = c(5,5,4,5))
barplot(xtabs(Percent ~ Status + Covariate, data = hosp.nlos), beside = T, legend = c("Missing", "Valid"), 
        main = "Data Presence by Size and Population Served", xlab = "Response Variable: ED LOS, Non-Admitted Patients",
        ylab = "Number of Beds per Hospital", col = missing.colors)
axis(side = 4, at = c(0,50,100,150), labels = c("0", "25", "50", "75"))
mtext(side = 4, line = 2.75, "% of Patients from Rural area")
par(mar = c(5,5,4,5))
barplot(xtabs(Percent ~ Status + Covariate, data = hosp.mhlos), beside = T, legend = c("Missing", "Valid"), 
        main = "Data Presence by Size and Population Served", xlab = "Response Variable: ED LOS, Mental Health Patients",
        ylab = "Number of Beds per Hospital", col = missing.colors)
axis(side = 4, at = c(0,50,100,150), labels = c("0", "25", "50", "75"))
mtext(side = 4, line = 2.75, "% of Patients from Rural area")
par(mar = c(5,5,4,5))
barplot(xtabs(Percent ~ Status + Covariate, data = hosp.lwbs), beside = T, legend = c("Missing", "Valid"), 
        main = "Data Presence by Size and Population Served", sub = "Response Variable: LWBS Rate",
        ylab = "Number of Beds per Hospital", col = missing.colors)
axis(side = 4, at = c(0,50,100,150), labels = c("0", "25", "50", "75"))
mtext(side = 4, line = 2.75, "% of Patients from Rural area")
demos <- data.frame(Percent = as.vector(demo.nas), Status = rep(c("Missing", "Valid"), 20), 
           Race = rep(c("Asian", "Black", "Hispanic", "Native American"), each = 2, times = 5), 
           Response = rep(c("AdmitLOS", "WaitForBed", "NonAdmitLOS", "MHLOS", 
                            "LWBSrate"), each = 8))
par(mar = c(5.1, 4.1, 4.1, 2.1))
barplot(xtabs(Percent ~ Status + Race , data = demos[demos$Response == "AdmitLOS",]), 
        beside = T, ylim = c(0, 20), col = missing.colors,
        legend = c("Missing", "Valid"), main = "Data Presence by Race/Ethnicity", 
        xlab = "Response Variable: ED LOS, Admitted Patients", ylab = "Percentage")
barplot(xtabs(Percent ~ Status + Race , data = demos[demos$Response == "WaitForBed",]), 
        beside = T, ylim = c(0, 20), col = missing.colors, 
        legend = c("Missing", "Valid"), main = "Data Presence by Race/Ethnicity", 
        xlab = "Response Variable: Time Spent Waiting for Inpatient Bed", ylab = "Percentage")
barplot(xtabs(Percent ~ Status + Race , data = demos[demos$Response == "NonAdmitLOS",]), 
        beside = T, ylim = c(0, 20), col = missing.colors, 
        legend = c("Missing", "Valid"), main = "Data Presence by Race/Ethnicity", 
        xlab = "Response Variable: ED LOS, Non-Admitted Patients", ylab = "Percentage")
barplot(xtabs(Percent ~ Status + Race , data = demos[demos$Response == "MHLOS",]), 
        beside = T, ylim = c(0, 20), col = missing.colors, 
        legend = c("Missing", "Valid"), main = "Data Presence by Race/Ethnicity", 
        xlab = "Response Variable: ED LOS, Mental Health Patients", ylab = "Percentage")
barplot(xtabs(Percent ~ Status + Race , data = demos[demos$Response == "LWBSrate",]), 
        beside = T, ylim = c(0, 20), col = missing.colors, 
        legend = c("Missing", "Valid"), main = "Data Presence by Race/Ethnicity", 
        xlab = "Response Variable: LWBS Rate", ylab = "Percentage")


### Table Images
# For write-up
colnames(resp.desc)[5] <- "LWBSrate (%)"
htmlTable(resp.desc, css.cell = 'padding: 0px 30px 0px;') 
htmlTable(resp.desc,css.cell = 'padding: 2px 30px 2px;')
colnames(race.desc)[4] <- "Native American"
htmlTable(race.desc,css.cell = 'padding: 2px 30px 2px;')
colnames(cov.desc)[c(2,4,5)] <- c("Median Age", "% Rural Patients", "Sex Ratio")
htmlTable(cov.desc,css.cell = 'padding: 2px 30px 2px;')
htmlTable(missing.rates, rnames = F,css.cell = 'padding: 2px 30px 2px;') 
# For appendix
colnames(state.summaries)[6:7] <- c("LWBS Rate (%)", "Rating")
htmlTable(state.summaries, rnames = F, css.cell = 'padding: 0px 40px 0px;')
htmlTable(corrs, css.cell = 'padding: 0px 20px 0px;')


### End of file ###