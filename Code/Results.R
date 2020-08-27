

# Load necessary libraries
library(MASS)
library(betareg)
library(sjPlot)

# Read in and format data
hospital_data <- read.csv("WaitTimeData.csv")
hospital_data$MedicaidExpansion <- as.factor(hospital_data$MedicaidExpansion)
hospital_data$ED.Volume <- factor(hospital_data$ED.Volume, levels = c("Low", "Medium", "High", "Very High"))
hospital_data$LWBSrateAdjusted <- (hospital_data$LWBSrate * (length(na.omit(hospital_data$LWBSrate)) - 1) + 0.5) / 
  length(na.omit(hospital_data$LWBSrate))

# Build final datasets with outliers removed
hospital_data.alos.m1 <- hospital_data[-c(425),]
hospital_data.alos.m2 <- hospital_data[-c(425),]
hospital_data.alos.m3 <- hospital_data[-c(425, 1041),]
hospital_data.alos.m4 <- hospital_data[-c(425, 1041, 2796),]
hospital_data.wfb.m1 <- hospital_data
hospital_data.wfb.m2 <- hospital_data[-c(732, 425),]
hospital_data.wfb.m3 <- hospital_data[-c(425, 3611, 3365),]
hospital_data.wfb.m4 <- hospital_data[-c(425, 3611, 3365, 2344),]
hospital_data.nlos.m1 <- hospital_data[-c(160),]
hospital_data.nlos.m2 <- hospital_data[-c(160),]
hospital_data.nlos.m3 <- hospital_data
hospital_data.nlos.m4 <- hospital_data
hospital_data.mhlos.m1 <- hospital_data[-c(2684, 3314, 1689),]
hospital_data.mhlos.m2 <- hospital_data[-c(2684, 3314, 2735, 2681),]
hospital_data.mhlos.m3 <- hospital_data[-c(2684, 3314, 2735),]
hospital_data.mhlos.m4 <- hospital_data[-c(2684, 3314, 2735, 2681),]
hospital_data.lwbs.m1 <- hospital_data[-c(1475),]
hospital_data.lwbs.m2 <- hospital_data[-c(1475),]
hospital_data.lwbs.m3 <- hospital_data[-c(1475),]
hospital_data.lwbs.m4 <- hospital_data[-c(1475),]

alos.final.m1 <- glm(AdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "log"), data = hospital_data.alos.m1, na.action = na.omit)
alos.final.m2 <- glm(AdmitLOS ~ RuralScore + SexRatio + MedianAge + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "log"), data = hospital_data.alos.m2, na.action = na.omit)
alos.final.m3 <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "log"), data = hospital_data.alos.m3, na.action = na.omit)
alos.final.m4 <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                       RuralScore + SexRatio + MedianAge + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "log"), data = hospital_data.alos.m4, na.action = na.omit)

wfb.final.m1 <- glm.nb(WaitForBed ~ Black + Hispanic + Asian + NativeAmerican, 
                       data = hospital_data.wfb.m1, na.action = na.omit)
wfb.final.m2 <- glm.nb(WaitForBed ~ RuralScore + SexRatio + MedianAge + MedicaidExpansion + 
                         Black + Hispanic + Asian + NativeAmerican, 
                       data = hospital_data.wfb.m2, na.action = na.omit)
wfb.final.m3 <- glm.nb(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
                         Black + Hispanic + Asian + NativeAmerican, 
                       data = hospital_data.wfb.m3, na.action = na.omit)
wfb.final.m4 <- glm.nb(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
                         RuralScore + SexRatio + MedianAge + MedicaidExpansion + 
                         Black + Hispanic + Asian + NativeAmerican, 
                       data = hospital_data.wfb.m4, na.action = na.omit)
nlos.final.m1 <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "identity"), data = hospital_data.nlos.m1, na.action = na.omit)
nlos.final.m2 <- glm(NonAdmitLOS ~ RuralScore + SexRatio + MedianAge + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "identity"), data = hospital_data.nlos.m2, na.action = na.omit)
nlos.final.m3 <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "identity"), data = hospital_data.nlos.m3, na.action = na.omit)
nlos.final.m4 <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                       RuralScore + SexRatio + MedianAge + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "identity"), data = hospital_data.nlos.m4, na.action = na.omit)
mhlos.final.m1 <- glm(MHLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                      family = inverse.gaussian(link = "log"), data = hospital_data.mhlos.m1, na.action = na.omit)
mhlos.final.m2 <- glm(MHLOS ~ RuralScore + MedianAge + SexRatio + MedianAge + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      family = inverse.gaussian(link = "log"), data = hospital_data.mhlos.m2, na.action = na.omit)
mhlos.final.m3 <- glm(MHLOS ~ ED.Volume + HospitalRating + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      family = inverse.gaussian(link = "log"), data = hospital_data.mhlos.m3, na.action = na.omit)
mhlos.final.m4 <- glm(MHLOS ~ ED.Volume + HospitalRating + 
                        RuralScore + SexRatio + MedianAge + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      family = inverse.gaussian(link = "log"), data = hospital_data.mhlos.m4, na.action = na.omit)
lwbs.final.m1 <- betareg(LWBSrateAdjusted ~ Black + Hispanic + Asian + NativeAmerican, 
                         link = "log", data = hospital_data.lwbs.m1, na.action = na.omit)
lwbs.final.m2 <- betareg(LWBSrateAdjusted ~ RuralScore + SexRatio + MedianAge + MedicaidExpansion + 
                           Black + Hispanic + Asian + NativeAmerican, 
                         link = "log", data = hospital_data.lwbs.m2, na.action = na.omit)
lwbs.final.m3 <- betareg(LWBSrateAdjusted ~ ED.Volume + HospitalRating + 
                           Black + Hispanic + Asian + NativeAmerican, 
                         link = "log", data = hospital_data.lwbs.m3, na.action = na.omit)
lwbs.final.m4 <- betareg(LWBSrateAdjusted ~ ED.Volume + HospitalRating + 
                           RuralScore + SexRatio + MedianAge + MedicaidExpansion + 
                           Black + Hispanic + Asian + NativeAmerican, 
                         link = "log", data = hospital_data.lwbs.m4, na.action = na.omit)


# Construct tables of regression results
resp.labels <- c("Model 1: Race Only", "Model 2: Race + Demographics", "Model 3: Race + Hospital Info", 
                 "Model 4: Race + Demographics + Hospital Info")
tab_model(alos.final.m1, alos.final.m2, alos.final.m3, alos.final.m4, transform = "exp",
          show.intercept = F, p.style = "star",
          show.ci = F, title = "ED LOS: Admitted Patients - Gamma GLM with Log Link",
          dv.labels = resp.labels, CSS = list(
            css.summary = 'text-align: center;'
          ))
tab_model(wfb.final.m1, wfb.final.m2, wfb.final.m3, wfb.final.m4, transform = "exp",
          show.intercept = F, p.style = "star",
          show.ci = F, title = "Time Spent Waiting for Inpatient Bed - Negative Binomial GLM with Log Link",
          dv.labels = resp.labels, string.est = "Estimates", CSS = list(
            css.summary = 'text-align: center;'
          ))
tab_model(nlos.final.m1, nlos.final.m2, nlos.final.m3, nlos.final.m4, transform = NULL,
          show.intercept = F, p.style = "star", dv.labels = resp.labels,
          show.ci = F, title = "ED LOS: Non-Admitted Patients - Gamma GLM with Identity Link", CSS = list(
            css.summary = 'text-align: center;'
          ))
tab_model(mhlos.final.m1, mhlos.final.m2, mhlos.final.m3, mhlos.final.m4, transform = "exp",
          show.intercept = F, p.style = "star", dv.labels = resp.labels,
          show.ci = F, title = "ED LOS: Mental Health Patients - Inverse Gaussian GLM with Log Link",
          CSS = list(
            css.summary = 'text-align: center;'
          )) # Beds not significant
tab_model(lwbs.final.m1, lwbs.final.m2, lwbs.final.m3, lwbs.final.m4, transform = "exp", 
          show.intercept = F, p.style = "star",
          show.ci = F, dv.labels = resp.labels, 
          title = "LWBS Rate - Beta Regression with Log Link",CSS = list(
            css.summary = 'text-align: center;'
          )) # Beds not signifiant


nd <- data.frame(ED.Volume = "Low", HospitalRating = mean(na.omit(hospital_data$HospitalRating)),
                 RuralScore = mean(na.omit(hospital_data$RuralScore)),
                SexRatio = mean(na.omit(hospital_data$SexRatio)), 
                MedianAge = mean(na.omit(hospital_data$MedianAge)),
                MedicaidExpansion = "Yes", 
                Black = mean(na.omit(hospital_data$Black)), 
                Hispanic = 0.1,
                Asian = mean(na.omit(hospital_data$Asian)),
                NativeAmerican = mean(na.omit(hospital_data$NativeAmerican)),
                Beds = mean(na.omit(hospital_data$Beds))
                )
predict(wfb.final.m4, newdata = nd, type = "response")
hospital_data$MHLOS


coef(mhlos.final.m4)
