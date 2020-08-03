

### Build and refine models of LOS / wait time ###



### Outline

# 1. Prep work
#   - Read in data
#   - Format variables
#   - Load necessary libraries
# 2. Compare models with different response distributions and link functions
#   - Build a variety of models 
#   - Assess models with AIC
#   - Summary of most approproiate models
# 3. Fit appropriate models, with and without adjustment for covariates
# 4. Identify and remove outliers
# 5. Fit updated models with outliers removed
# 6. Inspect final models
# 7. Model Diagnostics
#   - Partial residual plots
#   - Residuals vs fitted values
#   - Independence 
#   - Linearity on link scale



############################# Read in data, format variables, load necessary libraries ##########################

# Load necessary libraries
library(MASS)
library(betareg)
library(car)

# Read in data from CSV
hospital_data <- read.csv("WaitTimeData.csv")
str(hospital_data)
hospital_data$MedicaidExpansion <- as.factor(hospital_data$MedicaidExpansion)
hospital_data$ED.Volume <- factor(hospital_data$ED.Volume, levels = c("low", "medium", "high", "very high"))




#################### Compare models with different response distributions and link functions #######################


###### Build a variety of models


### Response variable: AdmitLOS
# Gaussian
alos.gaus.id <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
             family = gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
alos.gaus.log <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
             family = gaussian(link = "log"), data = hospital_data, na.action = na.omit)
alos.gaus.inv <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
             family = gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)
# Gamma
alos.gamma.inv <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
             family = Gamma(link = "inverse"), data = hospital_data, na.action = na.omit)
alos.gamma.id <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
             family = Gamma(link = "identity"), data = hospital_data, na.action = na.omit)
alos.gamma.log <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
             family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)
# Poisson
alos.pois.log <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = poisson(link = "log"), data = hospital_data, na.action = na.omit)
alos.pois.id <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = poisson(link = "identity"), data = hospital_data, na.action = na.omit)
alos.pois.sqrt <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = poisson(link = "sqrt"), data = hospital_data, na.action = na.omit)
# Inverse Gaussian
alos.ig.id <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
alos.ig.log <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
alos.ig.inv <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)
# Negative Binomial
alos.nb <- glm.nb(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
                      data = hospital_data, na.action = na.omit)

### Response variable: WaitForBed
# Gaussian
wfb.gaus.id <- glm(WaitForBed ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
wfb.gaus.log <- glm(WaitForBed ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "log"), data = hospital_data, na.action = na.omit)
wfb.gaus.inv <- glm(WaitForBed ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)
# Gamma
wfb.gamma.inv <- glm(WaitForBed ~  Black + Hispanic + Asian + NativeAmerican, 
              family = Gamma(link = "inverse"), data = hospital_data, na.action = na.omit)
wfb.gamma.id <- glm(WaitForBed ~  Black + Hispanic + Asian + NativeAmerican, 
              family = Gamma(link = "identity"), data = hospital_data, na.action = na.omit)
wfb.gamma.log <- glm(WaitForBed ~  Black + Hispanic + Asian + NativeAmerican, 
              family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)
# Poisson
wfb.pois.log <- glm(WaitForBed ~  Black + Hispanic + Asian + NativeAmerican, 
              family = poisson(link = "log"), data = hospital_data, na.action = na.omit)
wfb.pois.id <- glm(WaitForBed ~  Black + Hispanic + Asian + NativeAmerican, 
              family = poisson(link = "identity"), data = hospital_data, na.action = na.omit)
wfb.pois.sqrt <- glm(WaitForBed ~  Black + Hispanic + Asian + NativeAmerican, 
              family = poisson(link = "sqrt"), data = hospital_data, na.action = na.omit)
# Inverse Gaussian
wfb.ig.id <- glm(WaitForBed ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
wfb.ig.log <- glm(WaitForBed ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
wfb.ig.inv <- glm(WaitForBed ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)
# Negative Binomial
wfb.nb <- glm.nb(WaitForBed ~  Black + Hispanic + Asian + NativeAmerican, 
                       data = hospital_data, na.action = na.omit)

### Response variable: NonAdmitLOS
# Gaussian
nlos.gaus.id <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
nlos.gaus.log <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "log"), data = hospital_data, na.action = na.omit)
nlos.gaus.inv <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)
# Gamma
nlos.gamma.inv <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = Gamma(link = "inverse"), data = hospital_data, na.action = na.omit)
nlos.gamma.id <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = Gamma(link = "identity"), data = hospital_data, na.action = na.omit)
nlos.gamma.log <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)
# Poisson
nlos.pois.log <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = poisson(link = "log"), data = hospital_data, na.action = na.omit)
nlos.pois.id <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = poisson(link = "identity"), data = hospital_data, na.action = na.omit)
nlos.pois.sqrt <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = poisson(link = "sqrt"), data = hospital_data, na.action = na.omit)
# Inverse Gaussian
nlos.ig.id <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
nlos.ig.log <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
nlos.ig.inv <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)
# Negative Binomial
nlos.nb <- glm.nb(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                       data = hospital_data, na.action = na.omit)

### Response variable: NonAdmitMHLOS
# Gaussian
mhlos.gaus.id <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
mhlos.gaus.log <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "log"), data = hospital_data, na.action = na.omit)
mhlos.gaus.inv <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)
# Gamma
mhlos.gamma.inv <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = Gamma(link = "inverse"), data = hospital_data, na.action = na.omit)
mhlos.gamma.id <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = Gamma(link = "identity"), data = hospital_data, na.action = na.omit)
mhlos.gamma.log <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)
# Poisson
mhlos.pois.log <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = poisson(link = "log"), data = hospital_data, na.action = na.omit)
mhlos.pois.id <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = poisson(link = "identity"), data = hospital_data, na.action = na.omit)
mhlos.pois.sqrt <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = poisson(link = "sqrt"), data = hospital_data, na.action = na.omit)
# Inverse Gaussian
mhlos.ig.id <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
mhlos.ig.log <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
mhlos.ig.inv <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)
# Negative Binomial
mhlos.nb <- glm.nb(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
                       data = hospital_data, na.action = na.omit)

### Response variable: LWBSrateAdjusted
# Adjust LWBSrate so values are between 0 and 1 
# Source: Smithson and Verkuilen, 2006
# URL: https://pdfs.semanticscholar.org/8517/79099a662b91c8153a9a40e069f0276eecbc.pdf
hospital_data$LWBSrateAdjusted <- (hospital_data$LWBSrate * (length(na.omit(hospital_data$LWBSrate)) - 1) + 0.5) / 
  length(na.omit(hospital_data$LWBSrate))
# Now begin fitting models...
# Gaussian 
lwbs.gaus.id <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
lwbs.gaus.log <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "log"), data = hospital_data, na.action = na.omit)
lwbs.gaus.inv <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)
# Gamma
lwbs.gamma.inv <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = Gamma(link = "inverse"), data = hospital_data, na.action = na.omit)
lwbs.gamma.id <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = Gamma(link = "identity"), data = hospital_data, na.action = na.omit)
lwbs.gamma.log <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)
# Poisson
lwbs.pois.log <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = poisson(link = "log"), data = hospital_data, na.action = na.omit)
lwbs.pois.id <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = poisson(link = "identity"), data = hospital_data, na.action = na.omit)
lwbs.pois.sqrt <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = poisson(link = "sqrt"), data = hospital_data, na.action = na.omit)
# Inverse Gaussian
lwbs.ig.id <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
lwbs.ig.log <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
lwbs.ig.inv <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)
# Negative Binomial
lwbs.nb <- glm.nb(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
                       data = hospital_data, na.action = na.omit)
# Beta Regression
lwbs.beta <- betareg(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
                           data = hospital_data, na.action = na.omit)



##### Assess models with AIC

# Assess AdmitLOS models (all models converged)
AIC(alos.gaus.id, alos.gaus.log, alos.gaus.inv, 
    alos.gamma.inv, alos.gamma.id, alos.gamma.log,
    alos.pois.log, alos.pois.id, alos.pois.sqrt,
    alos.ig.id, alos.ig.log, alos.ig.inv, 
    alos.nb)
# Best model: Gamma

# Assess WaitForBed models that converged / did not return error
AIC(wfb.gaus.id, wfb.pois.log, wfb.pois.sqrt, wfb.nb)
# Best model: Negative Binomial

# Assess NonAdmitLOS models (all models converged)
AIC(nlos.gaus.id, nlos.gaus.log, nlos.gaus.inv, 
    nlos.gamma.inv, nlos.gamma.id, nlos.gamma.log, 
    nlos.pois.log, nlos.pois.id, nlos.pois.sqrt,
    nlos.ig.id, nlos.ig.log, nlos.ig.inv, 
    nlos.nb)
# Best model: Gamma

# Assess NonAdmitMHLOS models (all models converged)
AIC(mhlos.gaus.id, mhlos.gaus.log, mhlos.gaus.inv, 
    mhlos.gamma.inv, mhlos.gamma.id, mhlos.gamma.log, 
    mhlos.pois.log, mhlos.pois.id, mhlos.pois.sqrt,
    mhlos.ig.id, mhlos.ig.log, mhlos.ig.inv, 
    mhlos.nb)
# Best model: Inverse Gaussian. Negative Binomial and Gamma are close 2nd/3rd

# Assess LWBSrateAdjusted models that converged / did not return error
AIC(lwbs.gaus.id, lwbs.gaus.log, lwbs.gaus.inv, 
    lwbs.gamma.inv, lwbs.gamma.id, lwbs.gamma.log, 
    lwbs.pois.log, lwbs.pois.id, lwbs.pois.sqrt, 
    lwbs.ig.id, lwbs.ig.log, 
    lwbs.nb, lwbs.beta)
# Best model: Beta



##### Summary of best models

# AdmitLOS: Gamma
# WaitForBed: Negative Binomial
# NonAdmitLOS: Gamma
# NonAdmitMHLOS: Inverse Gaussian
# LWBSrateAdjusted: Beta



###################### Fit appropriate models to data, with and without adjustment for covariates ###################


##### Fit appropriate models 

### AdmitLOS
# Basic
alos.m1 <- glm(AdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                      family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)
# Demographic covariates added
alos.m2 <- glm(AdmitLOS ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)
# Hospital-level covariates added
alos.m3 <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                     Black + Hispanic + Asian + NativeAmerican, 
                       family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)
# Both hospital-level and demographic covariates added
alos.m4 <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                     RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                       family = Gamma(link = "log"), data = hospital_data[-423,], na.action = na.omit)

### WaitForBed
# Basic
wfb.m1 <- glm.nb(WaitForBed ~ Black + Hispanic + Asian + NativeAmerican, 
                           data = hospital_data, na.action = na.omit)
# Demographic covariates added
wfb.m2 <- glm.nb(WaitForBed ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                             Black + Hispanic + Asian + NativeAmerican, 
                           data = hospital_data, na.action = na.omit)
# Hospital-level covariates added
wfb.m3 <- glm.nb(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
                             Black + Hispanic + Asian + NativeAmerican, 
                           data = hospital_data, na.action = na.omit)
# Both hospital-level and demographic covariates added
wfb.m4 <- glm.nb(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
               RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
               Black + Hispanic + Asian + NativeAmerican, 
             data = hospital_data[-423,], na.action = na.omit)

### NonAdmitLOS
# Basic
nlos.m1 <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                   family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)
# Demographic covariates added
nlos.m2 <- glm(NonAdmitLOS ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)
# Hospital-level covariates added
nlos.m3 <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)
# Both hospital-level and demographic covariates added
nlos.m4 <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                     RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)

### NonAdmitMHLOS
# Basic
mhlos.m1 <- glm(NonAdmitMHLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                    family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
# Demographic covariates added
mhlos.m2 <- glm(NonAdmitMHLOS ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
# Hospital-level covariates added
mhlos.m3 <- glm(NonAdmitMHLOS ~ Beds + ED.Volume + HospitalRating + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
# Both hospital-level and demographic covariates added
mhlos.m4 <- glm(NonAdmitMHLOS ~ Beds + ED.Volume + HospitalRating + 
                      RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)

### LWBSrateAdjusted
# Basic
lwbs.m1 <- betareg(LWBSrateAdjusted ~ Black + Hispanic + Asian + NativeAmerican, 
                                data = hospital_data, na.action = na.omit)
# Demographic covariates added
lwbs.m2 <- betareg(LWBSrateAdjusted ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                                  Black + Hispanic + Asian + NativeAmerican, 
                                data = hospital_data, na.action = na.omit)
# Hospital-level covariates added
lwbs.m3 <- betareg(LWBSrateAdjusted ~ Beds + ED.Volume + HospitalRating + 
                                  Black + Hispanic + Asian + NativeAmerican, 
                                data = hospital_data, na.action = na.omit)
# Both hospital-level and demographic covariates added
lwbs.m4 <- betareg(LWBSrateAdjusted ~ Beds + ED.Volume + HospitalRating + 
                                  RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                                  Black + Hispanic + Asian + NativeAmerican, 
                                data = hospital_data, na.action = na.omit)







####################################### Identify and remove outliers #####################################

# Cook's Distance
# Remove observations with CD > 0.5
plot(alos.m1, which = 4)
plot(alos.m2, which = 4)
plot(alos.m3, which = 4)
plot(alos.m4, which = 4)
plot(wfb.m1, which = 4) 
plot(wfb.m2, which = 4) # Remove obs 729
plot(wfb.m3, which = 4) 
plot(wfb.m4, which = 4) 
plot(nlos.m1, which = 4) # Remove obs 159
plot(nlos.m2, which = 4) # Remove obs 159
plot(nlos.m3, which = 4)
plot(nlos.m4, which = 4)
plot(mhlos.m1, which = 4)
plot(mhlos.m2, which = 4)
plot(mhlos.m3, which = 4)
plot(mhlos.m4, which = 4)
plot(lwbs.m1, which = 2)
plot(lwbs.m2, which = 2)
plot(lwbs.m3, which = 2)
plot(lwbs.m4, which = 2)

outlierTest(alos.m1, cutoff = 0.001) # 423
outlierTest(alos.m2, cutoff = 0.001) # 423
outlierTest(alos.m3, cutoff = 0.001) # 423, 1034
outlierTest(alos.m4, cutoff = 0.005) # 2776
outlierTest(wfb.m1, cutoff = 0.001) 
outlierTest(wfb.m2, cutoff = 0.001) # 423
outlierTest(wfb.m3, cutoff = 0.001) # 423, 3580, 3337
outlierTest(wfb.m4, cutoff = 0.001) # 3580, 3337
outlierTest(nlos.m1, cutoff = 0.001) # 159 
outlierTest(nlos.m2, cutoff = 0.001) # 159
outlierTest(nlos.m3, cutoff = 0.001) 
outlierTest(nlos.m4, cutoff = 0.001) 
outlierTest(mhlos.m1, cutoff = 0.001) # 2664, 3287
outlierTest(mhlos.m2, cutoff = 0.001) # 2664, 3287, 2715, 2661
outlierTest(mhlos.m3, cutoff = 0.001) # 2664, 3287, 2715
outlierTest(mhlos.m4, cutoff = 0.001) # 2664, 3287, 2715, 2661
# Since outlierTest only works on glm objects, we inspect the top 10 outliers and their fitted values
head(sort(lwbs.m1$residuals, decreasing = T), n = 10) # 1462
head(sort(lwbs.m2$residuals, decreasing = T), n = 10) # 1462
head(sort(lwbs.m3$residuals, decreasing = T), n = 10) # 1462
head(sort(lwbs.m2$residuals, decreasing = T), n = 10) # 1462


hospital_data.alos.m1 <- hospital_data[-c(423),]
hospital_data.alos.m2 <- hospital_data[-c(423),]
hospital_data.alos.m3 <- hospital_data[-c(423, 1034),]
hospital_data.alos.m4 <- hospital_data[-c(423, 1034, 2776),]
hospital_data.wfb.m1 <- hospital_data
hospital_data.wfb.m2 <- hospital_data[-c(729, 423),]
hospital_data.wfb.m3 <- hospital_data[-c(423, 3580, 3337),]
hospital_data.wfb.m4 <- hospital_data[-c(423, 3580, 3337),]
hospital_data.nlos.m1 <- hospital_data[-c(159),]
hospital_data.nlos.m2 <- hospital_data[-c(159),]
hospital_data.nlos.m3 <- hospital_data
hospital_data.nlos.m4 <- hospital_data
hospital_data.mhlos.m1 <- hospital_data[-c(2664, 3287),]
hospital_data.mhlos.m2 <- hospital_data[-c(2664, 3287, 2715, 2661),]
hospital_data.mhlos.m3 <- hospital_data[-c(2664, 3287, 2715),]
hospital_data.mhlos.m4 <- hospital_data[-c(2664, 3287, 2715, 2661),]
hospital_data.lwbs.m1 <- hospital_data[-c(1462),]
hospital_data.lwbs.m2 <- hospital_data[-c(1462),]
hospital_data.lwbs.m3 <- hospital_data[-c(1462),]
hospital_data.lwbs.m4 <- hospital_data[-c(1462),]






########################################## Fit models with outliers removed ####################################

### AdmitLOS
# Basic
alos.or.m1 <- glm(AdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                  family = Gamma(link = "log"), data = hospital_data.alos.m1, na.action = na.omit)
# Demographic covariates added
alos.or.m2 <- glm(AdmitLOS ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                    Black + Hispanic + Asian + NativeAmerican, 
                  family = Gamma(link = "log"), data = hospital_data.alos.m2, na.action = na.omit)
# Hospital-level covariates added
alos.or.m3 <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                    Black + Hispanic + Asian + NativeAmerican, 
                  family = Gamma(link = "log"), data = hospital_data.alos.m3, na.action = na.omit)
# Both hospital-level and demographic covariates added
alos.or.m4 <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                    RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                    Black + Hispanic + Asian + NativeAmerican, 
                  family = Gamma(link = "log"), data = hospital_data.alos.m4, na.action = na.omit)

### WaitForBed
# Basic
wfb.or.m1 <- glm.nb(WaitForBed ~ Black + Hispanic + Asian + NativeAmerican, 
                    data = hospital_data.wfb.m1, na.action = na.omit)
# Demographic covariates added
wfb.or.m2 <- glm.nb(WaitForBed ~ RuralScore + SexRatio + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    data = hospital_data.wfb.m2, na.action = na.omit)
# Hospital-level covariates added
wfb.or.m3 <- glm.nb(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    data = hospital_data.wfb.m3, na.action = na.omit)
# Both hospital-level and demographic covariates added
wfb.or.m4 <- glm.nb(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
                      RuralScore + MedianAge + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    data = hospital_data.wfb.m4, na.action = na.omit)

### NonAdmitLOS
# Basic
nlos.or.m1 <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                  family = Gamma(link = "log"), data = hospital_data.nlos.m1, na.action = na.omit)
# Demographic covariates added
nlos.or.m2 <- glm(NonAdmitLOS ~ RuralScore + SexRatio + MedicaidExpansion + 
                    Black + Hispanic + Asian + NativeAmerican, 
                  family = Gamma(link = "log"), data = hospital_data.nlos.m2, na.action = na.omit)
# Hospital-level covariates added
nlos.or.m3 <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                    Black + Hispanic + Asian + NativeAmerican, 
                  family = Gamma(link = "log"), data = hospital_data.nlos.m3, na.action = na.omit)
# Both hospital-level and demographic covariates added
nlos.or.m4 <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                    RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                    Black + Hispanic + Asian + NativeAmerican, 
                  family = Gamma(link = "log"), data = hospital_data.nlos.m4, na.action = na.omit)

### NonAdmitMHLOS
# Basic
mhlos.or.m1 <- glm(NonAdmitMHLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                   family = inverse.gaussian(link = "log"), data = hospital_data.mhlos.m1, na.action = na.omit)
# Demographic covariates added
mhlos.or.m2 <- glm(NonAdmitMHLOS ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = inverse.gaussian(link = "log"), data = hospital_data.mhlos.m2, na.action = na.omit)
# Hospital-level covariates added
mhlos.or.m3 <- glm(NonAdmitMHLOS ~ ED.Volume + HospitalRating + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = inverse.gaussian(link = "log"), data = hospital_data.mhlos.m3, na.action = na.omit)
# Both hospital-level and demographic covariates added
mhlos.or.m4 <- glm(NonAdmitMHLOS ~ ED.Volume + HospitalRating + 
                     RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = inverse.gaussian(link = "log"), data = hospital_data.mhlos.m4, na.action = na.omit)

### LWBSrateAdjusted
# Basic
lwbs.or.m1 <- betareg(LWBSrateAdjusted ~ Black + Hispanic + Asian + NativeAmerican, 
                      data = hospital_data.lwbs.m1, na.action = na.omit)
# Demographic covariates added
lwbs.or.m2 <- betareg(LWBSrateAdjusted ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      data = hospital_data.lwbs.m2, na.action = na.omit)
# Hospital-level covariates added
lwbs.or.m3 <- betareg(LWBSrateAdjusted ~ ED.Volume + HospitalRating + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      data = hospital_data.lwbs.m3, na.action = na.omit)
# Both hospital-level and demographic covariates added
lwbs.or.m4 <- betareg(LWBSrateAdjusted ~ ED.Volume + HospitalRating + 
                        RuralScore + SexRatio + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      data = hospital_data.lwbs.m4, na.action = na.omit)



##################################### Inspect final models ##############################################

### Inspect Model 1s (Race variables ONLY)
summary(alos.or.m1)
summary(wfb.or.m1)
summary(nlos.or.m1)
summary(mhlos.or.m1)
summary(lwbs.or.m1)

### Inspect Model 2s (Race variables + demographics)
summary(alos.or.m2)
summary(wfb.or.m2)
summary(nlos.or.m2)
summary(mhlos.or.m2)
summary(lwbs.or.m2)

### Inspect Model 3s (Race variables + hospital-level covariates)
summary(alos.or.m3)
summary(wfb.or.m3)
summary(nlos.or.m3)
summary(mhlos.or.m3)
summary(lwbs.or.m3)

### Inspect Model 4s (Race variables + demographics + hospital-level covariates)
summary(alos.or.m4)
summary(wfb.or.m4)
summary(nlos.or.m4)
summary(mhlos.or.m4)
summary(lwbs.or.m4)



###################################### Model Diagnostics ##########################################

# Vector of model names
m.names <- list(alos.or.m4, wfb.or.m4, nlos.or.m4, mhlos.or.m4, lwbs.or.m4)

# Component+Residual Plots (11 plots each)
sapply(c(names(coef(alos.or.m4))[c(2, 6:9, 11:14)], "ED.Volume", "MedicaidExpansion"), 
       function(x){crPlot(alos.or.m4, x)})
sapply(c(names(coef(wfb.or.m4))[c(2, 6:8, 10:13)], "ED.Volume", "MedicaidExpansion"), 
       function(x){crPlot(wfb.or.m4, x)})
sapply(c(names(coef(nlos.or.m4))[c(2, 6:9, 11:14)], "ED.Volume", "MedicaidExpansion"), 
       function(x){crPlot(nlos.or.m4, x)})
sapply(c(names(coef(mhlos.or.m4))[c(5:8, 10:13)], "ED.Volume", "MedicaidExpansion"), 
       function(x){crPlot(mhlos.or.m4, x)})

d1 <- lwbs.or.m4$model
sapply(names(lwbs.or.m4$model)[2:10], function(x){plot(d1[,x], resid(lwbs.or.m4))})


names(lwbs.or.m4$model)[2:10]
lwbs.or.m4$model


# Assessing constant variance
# The residuals should exhibit roughly equal spread across the range of the fitted values
# All at once
lapply(m.names[1:4], function(x){plot(x, which = 2)})
plot(lwbs.or.m4, which = 4)
# Individually
plot(alos.or.m4, which = 1)
plot(wfb.or.m4, which = 1)
plot(nlos.or.m4, which = 1)
plot(mhlos.or.m4, which = 1)
plot(lwbs.or.m4, which = 4)
# Residuals appear random across the range of fitted values for all 5 models

# Independence
# The ordered residuals should resemble a random scatter of points about the horizontal axis
# All at once
lapply(m.names, function(x){plot(resid(x), type = "b")})
# Individually
plot(resid(alos.or.m4), type = "b")
plot(resid(wfb.or.m4), type = "b")
plot(resid(nlos.or.m4), type = "b")
plot(resid(mhlos.or.m4), type = "b")
plot(resid(lwbs.or.m4), type = "b")
# Scatter is approximately random for all 5 models


# Linearity on link scale
# All at once
lapply(m.names[1:4], function(x){plot(x, which = 2)})
qqnorm(resid(lwbs.or.m4))
qqline(resid(lwbs.or.m4), lwd = 1, lty = 3)
# Individually
plot(alos.or.m4, which = 2)
plot(wfb.or.m4, which = 2)
plot(nlos.or.m4, which = 2)
plot(mhlos.or.m4, which = 2)
qqnorm(resid(lwbs.or.m4))
qqline(resid(lwbs.or.m4), lwd = 1, lty = 3)
# Results mostly hug the horizontal line, except for lwbs.or.m4


# Check for multicollinearity
# All at once
lapply(m.names, vif)
# Individually
vif(alos.or.m4) 
vif(wfb.or.m4) 
vif(nlos.or.m4) 
vif(mhlos.or.m4) 
vif(lwbs.or.m4) 
# No issues here - Max VIF is 2.58 for ED.Volume



