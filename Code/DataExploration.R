

### Prepare data
# Load necessary libraries
library(tidyverse)
library(descr)
library(knitr)
library(VIM)
library(RColorBrewer)
library(chron)
library(MASS)


### Read in and format data
hospital_data <- read.csv("WaitTimeData.csv")
hospital_data$MedicaidExpansion <- factor(hospital_data$MedicaidExpansion, levels = c("Yes", "No"))
hospital_data$ED.Volume <- factor(hospital_data$ED.Volume, levels = c("Low", "Medium", "High", "Very High"))
hospital_data$UrbanFlag <- ifelse(hospital_data$RuralScore > 0.5, "0", "1")
hospital_data$UrbanFlag <- factor(hospital_data$UrbanFlag, labels = c("Rural", "Urban"))


### Define color palattes
volume.colors <- c("#FFCC00", "#FF9900", "#FF6600", "#FF3300")
rating.colors <- brewer.pal(n = 5, name = "Blues")
medex.colors <- c("#00FF99", "#006633")
rural.colors <- c("#FFCCCC", "#FF6666")


### Summaries
# Response variables
resp.desc <- cbind(sapply(hospital_data[,2:5], function(x){ sub("^0?", "", sprintf("%.02d:%.02d", summary(x) %/% 60, round(summary(x) %% 60)) ) }), 
      LWBSpercent = round(summary(hospital_data$LWBSrate), digits = 4) * 100)[1:6,]
# Race covariates
race.desc <- sapply(hospital_data[,c(8:14)] * 100, function(x){round(summary(x), digits = 1)})
# Demographic and Hospital-level covariates
cov.desc <- cbind(Beds = round(summary(hospital_data[,18])[1:6]),
      HospitalRating = round(summary(hospital_data[,7])[1:6], digits = 1),
  sapply(cbind(hospital_data[,15:16], RuralPercent = hospital_data[,17] * 100), function(x){round(summary(x), digits = 1)}))

kable(resp.desc)
kable(race.desc)
kable(cov.desc)

### Correlations
# Inspect correlations between numeric variables
corrs <- round(cor(hospital_data[,2:18], use = "pairwise.complete.obs"), digits = 3)


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

medex.plot1 <- plot(na.omit(hospital_data$MedicaidExpansion), main = "Is Hospital in State that expanded Medicaid?",
     ylab = "Number of Hospitals", ylim = c(0, length(na.omit(hospital_data$MedicaidExpansion))), 
     col = medex.colors)
text(x = medex.plot1, y = table(hospital_data$MedicaidExpansion), 
     labels = paste0(as.character(round(table(hospital_data$MedicaidExpansion) * 100 / 
                                          length(hospital_data$MedicaidExpansion), 1)), "%"), pos = 3)


# Hospital-level covariates
truehist(hospital_data$Beds, xlim = c(0, 1500), main = "Number of Beds per Hospital", 
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


### Plots of wait times by levels of varoius covariates
# ED Volume
boxplot(AdmitLOS / 60 ~ ED.Volume, data = hospital_data, ylim = c(0, 1000 / 60), col = volume.colors, 
        ylab = "ED LOS: Admitted Patients (Hours)", xlab = "Emergency Department Volume")
boxplot(WaitForBed / 60 ~ ED.Volume, data = hospital_data, ylim = c(0, 700 / 60), col = volume.colors, 
        ylab = "Hours Spent Waiting for Inpatient Bed", xlab = "Emergency Department Volume")
boxplot(NonAdmitLOS / 60 ~ ED.Volume, data = hospital_data, col = volume.colors, 
        ylab = "ED LOS: Non-Admitted Patients (Hours)", xlab = "Emergency Department Volume")
boxplot(MHLOS / 60 ~ ED.Volume, data = hospital_data, ylim = c(0, 1000 / 60), col = volume.colors, 
        ylab = "ED LOS: Mental Health Patients (Hours)", xlab = "Emergency Department Volume")
boxplot(LWBSrate * 100 ~ ED.Volume, data = hospital_data, ylim = c(0, 10), col = volume.colors, 
        ylab = "% of Patients who left ED without being seen", xlab = "Emergency Department Volume")

# Hospital Rating
boxplot(AdmitLOS / 60 ~ HospitalRating, data = hospital_data, ylim = c(0, 1000 / 60), col = rating.colors, 
        ylab = "ED LOS: Admitted Patients (Hours)", xlab = "Hospital Rating")
boxplot(WaitForBed / 60 ~ HospitalRating, data = hospital_data, ylim = c(0, 700 / 60), col = rating.colors, 
        ylab = "Hours Spent Waiting for Inpatient Bed", xlab = "Hospital Rating")
boxplot(NonAdmitLOS / 60 ~ HospitalRating, data = hospital_data, col = rating.colors, 
        ylab = "ED LOS: Non-Admitted Patients (Hours)", xlab = "Hospital Rating")
boxplot(MHLOS / 60 ~ HospitalRating, data = hospital_data, ylim = c(0, 1000 / 60), col = rating.colors, 
        ylab = "ED LOS: Mental Health Patients (Hours)", xlab = "Hospital Rating")
boxplot(LWBSrate * 100 ~ HospitalRating, data = hospital_data, ylim = c(0, 10), col = rating.colors, 
        ylab = "% of Patients who left ED without being seen", xlab = "Hospital Rating")
# kable(summary(hospital_data[hospital_data$HospitalRating == 1, 2:6])

# Wait Times by RuralScore
boxplot(AdmitLOS / 60 ~ UrbanFlag, data = hospital_data, ylim = c(0,1000 / 60), col = rural.colors, 
        ylab = "ED LOS: Admitted Patients (Hours)", xlab = "Population Served")
boxplot(WaitForBed / 60 ~ UrbanFlag, data = hospital_data, ylim = c(0,700 / 60), col = rural.colors,
        ylab = "Hours Spent Waiting for Inpatient Bed", xlab = "Population Served")
boxplot(NonAdmitLOS / 60 ~ UrbanFlag, data = hospital_data, col = rural.colors, 
        ylab = "ED LOS: Non-Admitted Patients (Hours)", xlab = "Population Served")
boxplot(MHLOS / 60 ~ UrbanFlag, data = hospital_data, ylim = c(0,1000 / 60), col = rural.colors,
        ylab = "ED LOS: Mental Health Patients (Hours)", xlab = "Population Served")
boxplot(LWBSrate * 100 ~ UrbanFlag, data = hospital_data, ylim = c(0,10), col = rural.colors, 
        ylab = "% of Patients who left ED without being seen", xlab = "Population Served")

# Wait Times by Medicaid Expansion status

boxplot(AdmitLOS / 60 ~ MedicaidExpansion, data = hospital_data, ylim = c(0, 900 / 60), col = medex.colors, 
        ylab = "ED LOS: Admitted Patients (Hours)", xlab = "Is Hospital in State that expanded Medicaid?")
boxplot(WaitForBed / 60 ~ MedicaidExpansion, data = hospital_data, ylim = c(0, 700 / 60), col = medex.colors,
        ylab = "Hours Spent Waiting for Inpatient Bed", xlab = "Is Hospital in State that expanded Medicaid?")
boxplot(NonAdmitLOS / 60 ~ MedicaidExpansion, data = hospital_data, col = medex.colors, 
        ylab = "ED LOS: Non-Admitted Patients (Hours)", xlab = "Is Hospital in State that expanded Medicaid?")
boxplot(MHLOS / 60 ~ MedicaidExpansion, data = hospital_data, ylim = c(0, 1000 / 60), col = medex.colors,
        ylab = "ED LOS: Mental Health Patients (Hours)", xlab = "Is Hospital in State that expanded Medicaid?")
boxplot(LWBSrate * 100 ~ MedicaidExpansion, data = hospital_data, ylim = c(0, 10), col = medex.colors, 
        ylab = "% of Patients who left ED without being seen", xlab = "Is Hospital in State that expanded Medicaid?")

# Wait Times by State
state.summaries <- data.frame(summarise(group_by(hospital_data, State), AdmitLOS = sub("^0?", "", sprintf("%.02d:%.02d", 
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
                                                           LWBSpercent = round(mean(LWBSrate * 100, na.rm = T), digits = 2)))




### Exploring NA values

# Read in and format data
raw_data <- read.csv("RawData.csv")
raw_data$MedicaidExpansion <- as.factor(raw_data$MedicaidExpansion)
raw_data$ED.Volume <- factor(raw_data$ED.Volume, levels = c("Low", "Medium", "High", "Very High"))

alos.na <- raw_data[is.na(raw_data$AdmitLOS),]
wfb.na <- raw_data[is.na(raw_data$WaitForBed),]
nlos.na <- raw_data[is.na(raw_data$NonAdmitLOS),]
mhlos.na <- raw_data[is.na(raw_data$MHLOS),]
lwbs.na <- raw_data[is.na(raw_data$LWBSrate),]
na.data <- list(alos.na, wfb.na, nlos.na, mhlos.na, lwbs.na)

alos.valid <- raw_data[!is.na(raw_data$AdmitLOS),]
wfb.valid <- raw_data[!is.na(raw_data$WaitForBed),]
nlos.valid <- raw_data[!is.na(raw_data$NonAdmitLOS),]
mhlos.valid <- raw_data[!is.na(raw_data$MHLOS),]
lwbs.valid <- raw_data[!is.na(raw_data$LWBSrate),]
valid.data <- list(alos.valid, wfb.valid, nlos.valid, mhlos.valid, lwbs.valid)

# Table of missing data rates
missing.rates <- data.frame(Variable = names(raw_data)[2:6], 
                      NAtotal = sapply(na.data, nrow),
                      NApercent = round(sapply(na.data, function(x){nrow(x) * 100 / nrow(hospital_data)}), digits = 1))
kable(missing.rates)


# Missing data tend to come from small, rural hospitals
hospital.nas <- data.frame(Missing.ru = round(sapply(na.data, function(x){mean(na.omit(x$RuralScore))}), digits = 2), 
           Valid.ru = round(sapply(valid.data, function(x){mean(na.omit(x$RuralScore))}), digits = 2),
           Missing.bed = round(sapply(na.data, function(x){mean(na.omit(x$Beds))}), digits = 2), 
           Valid.bed = round(sapply(valid.data, function(x){mean(na.omit(x$Beds))}), digits = 2),
           Missing.hr = round(sapply(na.data, function(x){mean(na.omit(x$HospitalRating))}), digits = 2), 
           Valid.hr = round(sapply(valid.data, function(x){mean(na.omit(x$HospitalRating))}), digits = 2),
           row.names = names(raw_data)[2:6])

demo.nas <- data.frame( Missing.a = round(sapply(na.data, function(x){mean(na.omit(x$Asian))}) * 100, digits = 2), 
            Valid.a = round(sapply(valid.data, function(x){mean(na.omit(x$Asian))}) * 100, digits = 2),
            Missing.b = round(sapply(na.data, function(x){mean(na.omit(x$Black))}) * 100, digits = 2), 
           Valid.b = round(sapply(valid.data, function(x){mean(na.omit(x$Black))}) * 100, digits = 2),
           Missing.h = round(sapply(na.data, function(x){mean(na.omit(x$Hispanic))}) * 100, digits = 2), 
           Valid.h = round(sapply(valid.data, function(x){mean(na.omit(x$Hispanic))}) * 100, digits = 2),
           Missing.n = round(sapply(na.data, function(x){mean(na.omit(x$NativeAmerican))}) * 100, digits = 2), 
           Valid.n = round(sapply(valid.data, function(x){mean(na.omit(x$NativeAmerican))}) * 100, digits = 2),
           Missing.w = round(sapply(na.data, function(x){mean(na.omit(x$White))}) * 100, digits = 2), 
           Valid.w = round(sapply(valid.data, function(x){mean(na.omit(x$White))}) * 100, digits = 2),
           row.names = names(raw_data)[2:6])




### Tables for appendix



kable(state.summaries)
kable(corrs)





