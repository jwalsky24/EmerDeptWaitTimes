

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
# 6. Model Diagnostics
#   - Partial residual plots
#   - Residuals vs fitted values
#   - Independence 
#   - Linearity on link scale
# 7. Inspect final models


############################# Read in data, format variables, load necessary libraries ##########################

# Load necessary libraries
library(MASS)
library(betareg)
library(car)

# Read in and format data
hospital_data <- read.csv("WaitTimeData.csv")
hospital_data$MedicaidExpansion <- as.factor(hospital_data$MedicaidExpansion)
hospital_data$ED.Volume <- factor(hospital_data$ED.Volume, levels = c("Low", "Medium", "High", "Very High"))



################# Inspect response variables to identify approrpriate distributions ########################

### Distributions to be considered: Gaussian (Normal), Gamma, Poisson, Inverse Gaussian (Wald), Negative Binomial, Beta

### Response variable: AdmitLOS
summary(hospital_data$AdmitLOS)
hist(na.omit(hospital_data$AdmitLOS), breaks = 50)
# Gaussian: could be appropriate, especially with log or inverse transformation
# Gamma: could be appropriate, values are positive and right skewed
# Poisson: could be appropriate, values are non-negative integer counts 
# Inverse Gaussian (Wald): could be appropriate, values are positive and right skewed
# Negative Binomial: could be appropriate, values are non-negative integer counts 
# Beta: not appropriate, values are not proportions

### Response variable: WaitForBed
summary(hospital_data$WaitForBed)
hist(na.omit(hospital_data$WaitForBed), breaks = 50)
# Gaussian: not appropriate, no left side tail due to zero constraint
# Gamma: not appropriate, values include zeroes
# Poisson: could be appropriate, values are non-negative integer counts 
# Inverse Gaussian (Wald): not appropriate, values include zeroes
# Negative Binomial: could be appropriate, values are non-negative integer counts 
# Beta: not appropriate, values are not proportions

### Response variable: NonAdmitLOS
summary(hospital_data$NonAdmitLOS)
hist(na.omit(hospital_data$NonAdmitLOS), breaks = 50)
# Gaussian: could be appropriate if log or inverse transformation used, values are right skewed
# Gamma: could be appropriate, values are positive and right skewed
# Poisson: could be appropriate, values are non-negative integer counts 
# Inverse Gaussian (Wald): could be appropriate, all values are positive
# Negative Binomial: could be appropriate, values are non-negative integer counts 
# Beta: not appropriate, values are not proportions

### Response variable: MHLOS
summary(hospital_data$MHLOS)
hist(na.omit(hospital_data$MHLOS), breaks = 50)
# Gaussian: could be appropriate if log or inverse transformation used, values are heavily right skewed
# Gamma: could be appropriate, values are positive and right skewed
# Poisson: could be appropriate, values are non-negative integer counts 
# Inverse Gaussian (Wald): could be appropriate, all values are positive
# Negative Binomial: could be appropriate, values are non-negative integer counts 
# Beta: not appropriate, values are not proportions

### Response variable: LWBSrate
summary(hospital_data$LWBSrate)
hist(na.omit(hospital_data$LWBSrate), breaks = 20)
# Gaussian: not appropriate, histogram is not bell-shaped
# Gamma: not appropriate, values include zeroes
# Poisson: not appropriate, values are not integer counts 
# Inverse Gaussian (Wald): not appropriate, values include zeroes
# Negative Binomial: not appropriate, values are not integer counts 
# Beta: could be appropriate, if values are transformed to be within (0,1)



#################### Compare models with different response distributions and link functions #######################

### Response variable: AdmitLOS
# Gaussian
alos.gaus.id <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                      RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    family = gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
alos.gaus.log <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                       RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = gaussian(link = "log"), data = hospital_data, na.action = na.omit)
alos.gaus.inv <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                       RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)
# Gamma
alos.gamma.inv <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                        RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      family = Gamma(link = "inverse"), data = hospital_data, na.action = na.omit)
alos.gamma.id <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                       RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "identity"), data = hospital_data, na.action = na.omit)
alos.gamma.log <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                        RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)
# Poisson
alos.pois.log <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                       RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = poisson(link = "log"), data = hospital_data, na.action = na.omit)
alos.pois.id <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                      RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    family = poisson(link = "identity"), data = hospital_data, na.action = na.omit)
alos.pois.sqrt <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                        RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      family = poisson(link = "sqrt"), data = hospital_data, na.action = na.omit)
# Inverse Gaussian
alos.ig.id <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                    RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                    Black + Hispanic + Asian + NativeAmerican, 
                  family = inverse.gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
alos.ig.log <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                     RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
alos.ig.inv <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                     RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = inverse.gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)
# Negative Binomial
alos.nb <- glm.nb(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                    RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                    Black + Hispanic + Asian + NativeAmerican, 
                  data = hospital_data, na.action = na.omit)

### Response variable: WaitForBed
# Poisson
wfb.pois.log <- glm(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
                      RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    family = poisson(link = "log"), data = hospital_data, na.action = na.omit)
wfb.pois.id <- glm(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
                     RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = poisson(link = "identity"), data = hospital_data, na.action = na.omit) # Returns error, NaNs produced
wfb.pois.sqrt <- glm(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
                       RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = poisson(link = "sqrt"), data = hospital_data, na.action = na.omit)
# Negative Binomial
wfb.nb <- glm.nb(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
                   RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                   Black + Hispanic + Asian + NativeAmerican, 
                 data = hospital_data, na.action = na.omit)

### Response variable: NonAdmitLOS
# Gaussian
nlos.gaus.id <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                      RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    family = gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
nlos.gaus.log <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                       RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = gaussian(link = "log"), data = hospital_data, na.action = na.omit)
nlos.gaus.inv <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                       RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)
# Gamma
nlos.gamma.inv <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                        RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      family = Gamma(link = "inverse"), data = hospital_data, na.action = na.omit)
nlos.gamma.id <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                       RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "identity"), data = hospital_data, na.action = na.omit)
nlos.gamma.log <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                        RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)
# Poisson
nlos.pois.log <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                       RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = poisson(link = "log"), data = hospital_data, na.action = na.omit)
nlos.pois.id <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                      RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    family = poisson(link = "identity"), data = hospital_data, na.action = na.omit)
nlos.pois.sqrt <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                        RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      family = poisson(link = "sqrt"), data = hospital_data, na.action = na.omit)
# Inverse Gaussian
nlos.ig.id <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                    RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                    Black + Hispanic + Asian + NativeAmerican, 
                  family = inverse.gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
nlos.ig.log <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                     RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
nlos.ig.inv <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                     RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = inverse.gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)
# Negative Binomial
nlos.nb <- glm.nb(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                    RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                    Black + Hispanic + Asian + NativeAmerican, 
                  data = hospital_data, na.action = na.omit)

### Response variable: MHLOS
# Gaussian
mhlos.gaus.id <- glm(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                       RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
mhlos.gaus.log <- glm(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                        RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      family = gaussian(link = "log"), data = hospital_data, na.action = na.omit)
mhlos.gaus.inv <- glm(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                        RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      family = gaussian(link = "inverse"), data = hospital_data, na.action = na.omit) # Did not converge
# Gamma
mhlos.gamma.inv <- glm(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                         RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                         Black + Hispanic + Asian + NativeAmerican, 
                       family = Gamma(link = "inverse"), data = hospital_data, na.action = na.omit)
mhlos.gamma.id <- glm(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                        RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      family = Gamma(link = "identity"), data = hospital_data, na.action = na.omit)
mhlos.gamma.log <- glm(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                         RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                         Black + Hispanic + Asian + NativeAmerican, 
                       family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)
# Poisson
mhlos.pois.log <- glm(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                        RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      family = poisson(link = "log"), data = hospital_data, na.action = na.omit)
mhlos.pois.id <- glm(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                       RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = poisson(link = "identity"), data = hospital_data, na.action = na.omit)
mhlos.pois.sqrt <- glm(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                         RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                         Black + Hispanic + Asian + NativeAmerican, 
                       family = poisson(link = "sqrt"), data = hospital_data, na.action = na.omit)
# Inverse Gaussian
mhlos.ig.id <- glm(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                     RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = inverse.gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
mhlos.ig.log <- glm(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                      RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
mhlos.ig.inv <- glm(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                      RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    family = inverse.gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)
# Negative Binomial
mhlos.nb <- glm.nb(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                     RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   data = hospital_data, na.action = na.omit)

### Response variable: LWBSrate
# Beta Regression
# Adjust LWBSrate so values are within (0,1) (Smithson and Verkuilen, 2006)
hospital_data$LWBSrateAdjusted <- (hospital_data$LWBSrate * (length(na.omit(hospital_data$LWBSrate)) - 1) + 0.5) / 
  length(na.omit(hospital_data$LWBSrate))
# Construct Beta model
lwbs.beta <- betareg(LWBSrateAdjusted ~ Beds + ED.Volume + HospitalRating + 
                       RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     data = hospital_data, na.action = na.omit)


##### Assess models with AIC

# Assess AdmitLOS models
alosmodels <- AIC(alos.gaus.id, alos.gaus.log, alos.gaus.inv, 
                  alos.gamma.inv, alos.gamma.id, alos.gamma.log,
                  alos.pois.log, alos.pois.id, alos.pois.sqrt,
                  alos.ig.id, alos.ig.log, alos.ig.inv, 
                  alos.nb)
alosmodels$AIC <- alosmodels$AIC - min(alosmodels$AIC)
alos.AIC <- alosmodels[order(alosmodels$AIC),]
alos.AIC
# Best model: Gamma with inverse link

# Assess valid WaitForBed models (wfb.pois.id returned error)
wfbmodels <- AIC(wfb.pois.log, wfb.pois.sqrt, wfb.nb)
wfbmodels$AIC <- wfbmodels$AIC - min(wfbmodels$AIC)
wfb.AIC <- wfbmodels[order(wfbmodels$AIC),]
wfb.AIC
# Best model: Negative Binomial

# Assess NonAdmitLOS models
nlosmodels <- AIC(nlos.gaus.id, nlos.gaus.log, nlos.gaus.inv, 
                  nlos.gamma.inv, nlos.gamma.id, nlos.gamma.log, 
                  nlos.pois.log, nlos.pois.id, nlos.pois.sqrt,
                  nlos.ig.id, nlos.ig.log, nlos.ig.inv, 
                  nlos.nb)
nlosmodels$AIC <- nlosmodels$AIC - min(nlosmodels$AIC)
nlos.AIC <- nlosmodels[order(nlosmodels$AIC),]
nlos.AIC
# Best model: Gamma with no link

# Assess valid MHLOS models (mhlos.gaus.inv did not converge)
mhlosmodels <- AIC(mhlos.gaus.id, mhlos.gaus.log, 
                   mhlos.gamma.inv, mhlos.gamma.id, mhlos.gamma.log, 
                   mhlos.pois.log, mhlos.pois.id, mhlos.pois.sqrt,
                   mhlos.ig.id, mhlos.ig.log, mhlos.ig.inv, 
                   mhlos.nb)
mhlosmodels$AIC <- mhlosmodels$AIC - min(mhlosmodels$AIC)
mhlos.AIC <- mhlosmodels[order(mhlosmodels$AIC),]
mhlos.AIC
# Best model: Inverse Gaussian with log link

# Assess LWBSrateAdjusted model
AIC(lwbs.beta)
# Best (and only) model: Beta


##### Summary of best models

# AdmitLOS: Gamma with lo
# WaitForBed: Negative Binomial
# NonAdmitLOS: Gamma
# MHLOS: Inverse Gaussian
# LWBSrateAdjusted: Beta



###################### Fit appropriate models to data, with and without adjustment for covariates ###################

##### Fit appropriate models 

### Response variable: AdmitLOS
# Race covariates only
alos.m1 <- glm(AdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
               family = Gamma(link = "inverse"), data = hospital_data, na.action = na.omit)
# Race covariates + Demographic covariates
alos.m2 <- glm(AdmitLOS ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                 Black + Hispanic + Asian + NativeAmerican, 
               family = Gamma(link = "inverse"), data = hospital_data, na.action = na.omit)
# Race covariates + Hospital-level covariates
alos.m3 <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                 Black + Hispanic + Asian + NativeAmerican, 
               family = Gamma(link = "inverse"), data = hospital_data, na.action = na.omit)
# Race covariates + Demographic covariates + Hospital-level covariates
alos.m4 <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                 RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                 Black + Hispanic + Asian + NativeAmerican, 
               family = Gamma(link = "inverse"), data = hospital_data, na.action = na.omit)

### Response variable: WaitForBed
# Race covariates only
wfb.m1 <- glm.nb(WaitForBed ~ Black + Hispanic + Asian + NativeAmerican, 
                           data = hospital_data, na.action = na.omit)
# Race covariates + Demographic covariates
wfb.m2 <- glm.nb(WaitForBed ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                             Black + Hispanic + Asian + NativeAmerican, 
                           data = hospital_data, na.action = na.omit)
# Race covariates + Hospital-level covariates
wfb.m3 <- glm.nb(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
                             Black + Hispanic + Asian + NativeAmerican, 
                           data = hospital_data, na.action = na.omit)
# Race covariates + Demographic covariates + Hospital-level covariates
wfb.m4 <- glm.nb(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
               RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
               Black + Hispanic + Asian + NativeAmerican, 
             data = hospital_data, na.action = na.omit)

### Response variable: NonAdmitLOS
# Race covariates only
nlos.m1 <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
               family = Gamma(link = "identity"), data = hospital_data, na.action = na.omit)
# Race covariates + Demographic covariates
nlos.m2 <- glm(NonAdmitLOS ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                 Black + Hispanic + Asian + NativeAmerican, 
               family = Gamma(link = "identity"), data = hospital_data, na.action = na.omit)
# Race covariates + Hospital-level covariates
nlos.m3 <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                 Black + Hispanic + Asian + NativeAmerican, 
               family = Gamma(link = "identity"), data = hospital_data, na.action = na.omit)
# Race covariates + Demographic covariates + Hospital-level covariates
nlos.m4 <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                 RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                 Black + Hispanic + Asian + NativeAmerican, 
               family = Gamma(link = "identity"), data = hospital_data, na.action = na.omit)

### Response variable: MHLOS
# Race covariates only
mhlos.m1 <- glm(MHLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                    family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
# Race covariates + Demographic covariates
mhlos.m2 <- glm(MHLOS ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
# Race covariates + Hospital-level covariates
mhlos.m3 <- glm(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
# Race covariates + Demographic covariates + Hospital-level covariates
mhlos.m4 <- glm(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                      RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)

### Response variable: LWBSrate (adjusted)
# Race covariates only
lwbs.m1 <- betareg(LWBSrateAdjusted ~ Black + Hispanic + Asian + NativeAmerican, 
                                data = hospital_data, na.action = na.omit)
# Race covariates + Demographic covariates
lwbs.m2 <- betareg(LWBSrateAdjusted ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                                  Black + Hispanic + Asian + NativeAmerican, 
                                data = hospital_data, na.action = na.omit)
# Race covariates + Hospital-level covariates
lwbs.m3 <- betareg(LWBSrateAdjusted ~ Beds + ED.Volume + HospitalRating + 
                                  Black + Hispanic + Asian + NativeAmerican, 
                                data = hospital_data, na.action = na.omit)
# Race covariates + Demographic covariates + Hospital-level covariates
lwbs.m4 <- betareg(LWBSrateAdjusted ~ Beds + ED.Volume + HospitalRating + 
                                  RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                                  Black + Hispanic + Asian + NativeAmerican, 
                                data = hospital_data, na.action = na.omit)



####################################### Identify and remove outliers #####################################

### Inspect Cook's Distance plots, remove observations with CD > 0.5
plot(alos.m1, which = 4)
plot(alos.m2, which = 4)
plot(alos.m3, which = 4)
plot(alos.m4, which = 4)
plot(wfb.m1, which = 4) 
plot(wfb.m2, which = 4) # Remove 732
plot(wfb.m3, which = 4) 
plot(wfb.m4, which = 4) 
plot(nlos.m1, which = 4) # Remove 160
plot(nlos.m2, which = 4) # Remove 160
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

### Outlier tests with strict cutoff (0.005)
outlierTest(alos.m1, cutoff = 0.005) # Remove 425
outlierTest(alos.m2, cutoff = 0.005) # Remove 425
outlierTest(alos.m3, cutoff = 0.005) # Remove 425, 1041
outlierTest(alos.m4, cutoff = 0.005) # Remove 425, 2796 plus 1041
outlierTest(wfb.m1, cutoff = 0.005) 
outlierTest(wfb.m2, cutoff = 0.005) # Remove 425
outlierTest(wfb.m3, cutoff = 0.005) # Remove 425, 3611, 3365
outlierTest(wfb.m4, cutoff = 0.005) # Remove 425, 3611, 3365, 2344
outlierTest(nlos.m1, cutoff = 0.005) # Remove 160 
outlierTest(nlos.m2, cutoff = 0.005) # Remove 160
outlierTest(nlos.m3, cutoff = 0.005) 
outlierTest(nlos.m4, cutoff = 0.005) 
outlierTest(mhlos.m1, cutoff = 0.005) # Remove 2684, 3314, 1689
outlierTest(mhlos.m2, cutoff = 0.005) # Remove 2684, 3314, 2735, 2681
outlierTest(mhlos.m3, cutoff = 0.005) # Remove 2684, 3314, 2735
outlierTest(mhlos.m4, cutoff = 0.005) # Remove 2684, 3314, 2735, 2681
# Since outlierTest only works on glm objects, we inspect the top 10 outliers and their fitted values
head(sort(lwbs.m1$residuals, decreasing = T), n = 10) # Remove 1475
head(sort(lwbs.m2$residuals, decreasing = T), n = 10) # Remove 1475
head(sort(lwbs.m3$residuals, decreasing = T), n = 10) # Remove 1475
head(sort(lwbs.m2$residuals, decreasing = T), n = 10) # Remove 1475

### Create new data sets for analaysis with outliers removed
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



########################################## Construct models with outliers removed #############################

### Response variable: AdmitLOS
# Race covariates only
alos.or.m1 <- glm(AdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                  family = Gamma(link = "inverse"), data = hospital_data.alos.m1, na.action = na.omit)
# Race covariates + Demographic covariates
alos.or.m2 <- glm(AdmitLOS ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                    Black + Hispanic + Asian + NativeAmerican, 
                  family = Gamma(link = "inverse"), data = hospital_data.alos.m2, na.action = na.omit)
# Race covariates + Hospital-level covariates
alos.or.m3 <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                    Black + Hispanic + Asian + NativeAmerican, 
                  family = Gamma(link = "inverse"), data = hospital_data.alos.m3, na.action = na.omit)
# Race covariates + Demographic covariates + Hospital-level covariates
alos.or.m4 <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                    RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                    Black + Hispanic + Asian + NativeAmerican, 
                  family = Gamma(link = "inverse"), data = hospital_data.alos.m4, na.action = na.omit)

### Response variable: WaitForBed
# Race covariates only
wfb.or.m1 <- glm.nb(WaitForBed ~ Black + Hispanic + Asian + NativeAmerican, 
                    data = hospital_data.wfb.m1, na.action = na.omit)
# Race covariates + Demographic covariates
wfb.or.m2 <- glm.nb(WaitForBed ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    data = hospital_data.wfb.m2, na.action = na.omit)
# Race covariates + Hospital-level covariates
wfb.or.m3 <- glm.nb(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    data = hospital_data.wfb.m3, na.action = na.omit)
# Race covariates + Demographic covariates + Hospital-level covariates
wfb.or.m4 <- glm.nb(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
                      RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    data = hospital_data.wfb.m4, na.action = na.omit)

### Response variable: NonAdmitLOS
# Race covariates only
nlos.or.m1 <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                  family = Gamma(link = "identity"), data = hospital_data.nlos.m1, na.action = na.omit)
# Race covariates + Demographic covariates
nlos.or.m2 <- glm(NonAdmitLOS ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                    Black + Hispanic + Asian + NativeAmerican, 
                  family = Gamma(link = "identity"), data = hospital_data.nlos.m2, na.action = na.omit)
# Race covariates + Hospital-level covariates
nlos.or.m3 <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                    Black + Hispanic + Asian + NativeAmerican, 
                  family = Gamma(link = "identity"), data = hospital_data.nlos.m3, na.action = na.omit)
# Race covariates + Demographic covariates + Hospital-level covariates
nlos.or.m4 <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                    RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                    Black + Hispanic + Asian + NativeAmerican, 
                  family = Gamma(link = "identity"), data = hospital_data.nlos.m4, na.action = na.omit)

### Response variable: MHLOS
# Race covariates only
mhlos.or.m1 <- glm(MHLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                   family = inverse.gaussian(link = "log"), data = hospital_data.mhlos.m1, na.action = na.omit)
# Race covariates + Demographic covariates
mhlos.or.m2 <- glm(MHLOS ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = inverse.gaussian(link = "log"), data = hospital_data.mhlos.m2, na.action = na.omit)
# Race covariates + Hospital-level covariates
mhlos.or.m3 <- glm(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = inverse.gaussian(link = "log"), data = hospital_data.mhlos.m3, na.action = na.omit)
# Race covariates + Demographic covariates + Hospital-level covariates
mhlos.or.m4 <- glm(MHLOS ~ Beds + ED.Volume + HospitalRating + 
                     RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = inverse.gaussian(link = "log"), data = hospital_data.mhlos.m4, na.action = na.omit)

### Response variable: LWBSrate (adjusted)
# Race covariates only
lwbs.or.m1 <- betareg(LWBSrateAdjusted ~ Black + Hispanic + Asian + NativeAmerican, 
                      data = hospital_data.lwbs.m1, na.action = na.omit)
# Race covariates + Demographic covariates
lwbs.or.m2 <- betareg(LWBSrateAdjusted ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      data = hospital_data.lwbs.m2, na.action = na.omit)
# Race covariates + Hospital-level covariates
lwbs.or.m3 <- betareg(LWBSrateAdjusted ~ Beds + ED.Volume + HospitalRating + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      data = hospital_data.lwbs.m3, na.action = na.omit)
# Race covariates + Demographic covariates + Hospital-level covariates
lwbs.or.m4 <- betareg(LWBSrateAdjusted ~ Beds + ED.Volume + HospitalRating + 
                        RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      data = hospital_data.lwbs.m4, na.action = na.omit)



##################################### Inspect models with outliers removed ######################################

### Response variable: AdmitLOS
summary(alos.or.m1)
summary(alos.or.m2)
summary(alos.or.m3)
summary(alos.or.m4)

### Response variable: WaitForBed
summary(wfb.or.m1)
summary(wfb.or.m2)
summary(wfb.or.m3)
summary(wfb.or.m4)

### Response variable: NonAdmitLOS
summary(nlos.or.m1)
summary(nlos.or.m2)
summary(nlos.or.m3)
summary(nlos.or.m4)

### Response variable: MHLOS
summary(mhlos.or.m1)
summary(mhlos.or.m2)
summary(mhlos.or.m3)
summary(mhlos.or.m4)

### Response variable: LWSrate (adjusted)
summary(lwbs.or.m1)
summary(lwbs.or.m2)
summary(lwbs.or.m3)
summary(lwbs.or.m4)



####################### Construct final models with insignificant covariates removed ##############################

### AdmitLOS
# Race covariates only
alos.final.m1 <- glm(AdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "inverse"), data = hospital_data.alos.m1, na.action = na.omit)
# Race covariates + Demographic covariates
alos.final.m2 <- glm(AdmitLOS ~ RuralScore + SexRatio + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "inverse"), data = hospital_data.alos.m2, na.action = na.omit)
# Race covariates + Hospital-level covariates
alos.final.m3 <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "inverse"), data = hospital_data.alos.m3, na.action = na.omit)
# Race covariates + Demographic covariates + Hospital-level covariates
alos.final.m4 <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                       RuralScore + MedianAge + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "inverse"), data = hospital_data.alos.m4, na.action = na.omit)

### WaitForBed
# Race covariates only
wfb.final.m1 <- glm.nb(WaitForBed ~ Black + Hispanic + Asian + NativeAmerican, 
                       data = hospital_data.wfb.m1, na.action = na.omit)
# Race covariates + Demographic covariates
wfb.final.m2 <- glm.nb(WaitForBed ~ RuralScore + SexRatio + MedicaidExpansion + 
                         Black + Hispanic + Asian + NativeAmerican, 
                       data = hospital_data.wfb.m2, na.action = na.omit)
# Race covariates + Hospital-level covariates
wfb.final.m3 <- glm.nb(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
                         Black + Hispanic + Asian + NativeAmerican, 
                       data = hospital_data.wfb.m3, na.action = na.omit)
# Race covariates + Demographic covariates + Hospital-level covariates
wfb.final.m4 <- glm.nb(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
                         RuralScore + MedianAge + MedicaidExpansion + 
                         Black + Hispanic + Asian + NativeAmerican, 
                       data = hospital_data.wfb.m4, na.action = na.omit)

### NonAdmitLOS
# Race covariates only
nlos.final.m1 <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "identity"), data = hospital_data.nlos.m1, na.action = na.omit)
# Race covariates + Demographic covariates
nlos.final.m2 <- glm(NonAdmitLOS ~ RuralScore + SexRatio + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "identity"), data = hospital_data.nlos.m2, na.action = na.omit)
# Race covariates + Hospital-level covariates
nlos.final.m3 <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "identity"), data = hospital_data.nlos.m3, na.action = na.omit)
# Race covariates + Demographic covariates + Hospital-level covariates
nlos.final.m4 <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                       RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                       Black + Hispanic + Asian + NativeAmerican, 
                     family = Gamma(link = "identity"), data = hospital_data.nlos.m4, na.action = na.omit)

### MHLOS
# Race covariates only
mhlos.final.m1 <- glm(MHLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                      family = inverse.gaussian(link = "log"), data = hospital_data.mhlos.m1, na.action = na.omit)
# Race covariates + Demographic covariates
mhlos.final.m2 <- glm(MHLOS ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      family = inverse.gaussian(link = "log"), data = hospital_data.mhlos.m2, na.action = na.omit)
# Race covariates + Hospital-level covariates
mhlos.final.m3 <- glm(MHLOS ~ ED.Volume + HospitalRating + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      family = inverse.gaussian(link = "log"), data = hospital_data.mhlos.m3, na.action = na.omit)
# Race covariates + Demographic covariates + Hospital-level covariates
mhlos.final.m4 <- glm(MHLOS ~ ED.Volume + HospitalRating + 
                        RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                        Black + Hispanic + Asian + NativeAmerican, 
                      family = inverse.gaussian(link = "log"), data = hospital_data.mhlos.m4, na.action = na.omit)

### LWBSrateAdjusted
# Race covariates only
lwbs.final.m1 <- betareg(LWBSrateAdjusted ~ Black + Hispanic + Asian + NativeAmerican, 
                         data = hospital_data.lwbs.m1, na.action = na.omit)
# Race covariates + Demographic covariates
lwbs.final.m2 <- betareg(LWBSrateAdjusted ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                           Black + Hispanic + Asian + NativeAmerican, 
                         data = hospital_data.lwbs.m2, na.action = na.omit)
# Race covariates + Hospital-level covariates
lwbs.final.m3 <- betareg(LWBSrateAdjusted ~ ED.Volume + HospitalRating + 
                           Black + Hispanic + Asian + NativeAmerican, 
                         data = hospital_data.lwbs.m3, na.action = na.omit)
# Race covariates + Demographic covariates + Hospital-level covariates
lwbs.final.m4 <- betareg(LWBSrateAdjusted ~ ED.Volume + HospitalRating + 
                           RuralScore + SexRatio + MedicaidExpansion + 
                           Black + Hispanic + Asian + NativeAmerican, 
                         data = hospital_data.lwbs.m4, na.action = na.omit)



###################################### Model Diagnostics ##########################################

# Vector of model names
m.names <- list(alos.final.m4, wfb.final.m4, nlos.final.m4, mhlos.final.m4, lwbs.final.m4)

# Assessing homoscedasticity with Residuals vs Fitted plots
# The residuals should exhibit roughly equal spread across the range of the fitted values
# All at once
lapply(m.names[1:4], function(x){plot(x, which = 1)})
plot(lwbs.final.m4, which = 4)
# Individually
plot(alos.final.m4, which = 1)
plot(wfb.final.m4, which = 1)
plot(nlos.final.m4, which = 1)
plot(mhlos.final.m4, which = 1)
plot(lwbs.final.m4, which = 4)
# Residuals appear random across the range of fitted values for all 5 models

# Assessing Independence
# The ordered residuals should resemble a random scatter of points about the horizontal axis
# All at once
lapply(m.names, function(x){plot(resid(x), type = "b")})
# Individually
plot(resid(alos.final.m4), type = "b")
plot(resid(wfb.final.m4), type = "b")
plot(resid(nlos.final.m4), type = "b")
plot(resid(mhlos.final.m4), type = "b")
plot(resid(lwbs.final.m4), type = "b")
# Scatter is approximately random for all 5 models

# Assessing linearity on the link scale between predictors and response
# All at once
lapply(m.names[1:4], function(x){plot(x, which = 2)})
qqnorm(resid(lwbs.final.m4))
qqline(resid(lwbs.final.m4), lwd = 1, lty = 3)
# Individually
plot(alos.final.m4, which = 2)
plot(wfb.final.m4, which = 2)
plot(nlos.final.m4, which = 2)
plot(mhlos.final.m4, which = 2)
qqnorm(resid(lwbs.final.m4))
qqline(resid(lwbs.final.m4), lwd = 1, lty = 3)
# Results mostly hug the horizontal line, heavy skewing at higher values

# Check for multicollinearity
# All at once
lapply(m.names, vif)
# Individually
vif(alos.final.m4) 
vif(wfb.final.m4) 
vif(nlos.final.m4) 
vif(mhlos.final.m4) 
vif(lwbs.final.m4) 
# No issues here - Max VIF is 2.72 for ED.Volume

# Partial residual plots (11 plots each)
# Used to assess linearity for each term in each model
sapply(c(names(coef(alos.final.m4))[c(2, 6:8, 10:13)], "ED.Volume", "MedicaidExpansion"), 
       function(x){crPlot(alos.final.m4, x)})
sapply(c(names(coef(wfb.final.m4))[c(2, 6:8, 10:13)], "ED.Volume", "MedicaidExpansion"), 
       function(x){crPlot(wfb.final.m4, x)})
sapply(c(names(coef(nlos.final.m4))[c(2, 6:9, 11:14)], "ED.Volume", "MedicaidExpansion"), 
       function(x){crPlot(nlos.final.m4, x)})
sapply(c(names(coef(mhlos.final.m4))[c(5:8, 10:13)], "ED.Volume", "MedicaidExpansion"), 
       function(x){crPlot(mhlos.final.m4, x)})
sapply(names(lwbs.final.m4$model)[2:10], function(x){plot(lwbs.final.m4$model[,x], resid(lwbs.final.m4))})



##################################### Inspect final models ##########################################

### Response variable: AdmitLOS
summary(alos.final.m1)
summary(alos.final.m2)
summary(alos.final.m3)
summary(alos.final.m4)

### Response variable: WaitForBed
summary(wfb.final.m1)
summary(wfb.final.m2)
summary(wfb.final.m3)
summary(wfb.final.m4)

### Response variable: NonAdmitLOS
summary(nlos.final.m1)
summary(nlos.final.m2)
summary(nlos.final.m3)
summary(nlos.final.m4)

### Response variable: MHLOS
summary(mhlos.final.m1)
summary(mhlos.final.m2)
summary(mhlos.final.m3)
summary(mhlos.final.m4)

### Response variable: LWSrate (adjusted)
summary(lwbs.final.m1)
summary(lwbs.final.m2)
summary(lwbs.final.m3)
summary(lwbs.final.m4)




