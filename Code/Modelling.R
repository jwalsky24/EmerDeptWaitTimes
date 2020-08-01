

library(MASS)
library(betareg)
library(car)

# Read in data from CSV
hospital_data <- read.csv("WaitTimeData.csv")
str(hospital_data)
hospital_data$MedicaidExpansion <- as.factor(hospital_data$MedicaidExpansion)
hospital_data$ED.Volume <- factor(hospital_data$ED.Volume, levels = c("low", "medium", "high", "very high"))

### Compare possible models

## AdmitLOS

# Gaussian (Normal)

alos.gaus.id <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
             family = gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
alos.gaus.log <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
             family = gaussian(link = "log"), data = hospital_data, na.action = na.omit)
alos.gaus.inv <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
             family = gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)

# GLM - Gamma

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

# Inverse Gaussian (Inverse Normal)

alos.ig.id <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
alos.ig.log <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
alos.ig.inv <- glm(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)

alos.nb <- glm.nb(AdmitLOS ~  Black + Hispanic + Asian + NativeAmerican, 
                      data = hospital_data, na.action = na.omit)

# Assess models (all models converged)
AIC(alos.gaus.id, alos.gaus.log, alos.gaus.inv, 
    alos.gamma.inv, alos.gamma.id, alos.gamma.log,
    alos.pois.log, alos.pois.id, alos.pois.sqrt,
    alos.ig.id, alos.ig.log, alos.ig.inv, 
    alos.nb)

# Best models: Gamma !!


## WaitForBed

# Gaussian

wfb.gaus.id <- glm(WaitForBed ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
wfb.gaus.log <- glm(WaitForBed ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "log"), data = hospital_data, na.action = na.omit)
wfb.gaus.inv <- glm(WaitForBed ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)

# GLM - Gamma

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

# Assess models that converged / did not return error
AIC(wfb.gaus.id, wfb.pois.log, wfb.pois.sqrt, wfb.nb)

# Best model: Negative Binomial !!


# LOS for Non-Amitted Patients

# Gaussian (Normal)

nlos.gaus.id <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
nlos.gaus.log <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "log"), data = hospital_data, na.action = na.omit)
nlos.gaus.inv <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)

# GLM - Gamma

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

# Inverse Gaussian (Inverse Normal)

nlos.ig.id <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
nlos.ig.log <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
nlos.ig.inv <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)

nlos.nb <- glm.nb(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                       data = hospital_data, na.action = na.omit)

# Assess models (all models converged)
AIC(nlos.gaus.id, nlos.gaus.log, nlos.gaus.inv, 
    nlos.gamma.inv, nlos.gamma.id, nlos.gamma.log, 
    nlos.pois.log, nlos.pois.id, nlos.pois.sqrt,
    nlos.ig.id, nlos.ig.log, nlos.ig.inv, 
    nlos.nb)

# Best models: Gamma !! 



## NonAdmitMHLOS

# Gaussian (Normal)

mhlos.gaus.id <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
mhlos.gaus.log <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "log"), data = hospital_data, na.action = na.omit)
mhlos.gaus.inv <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)

# GLM - Gamma

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

# Inverse Gaussian (Inverse Normal)

mhlos.ig.id <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
mhlos.ig.log <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
mhlos.ig.inv <- glm(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)

mhlos.nb <- glm.nb(NonAdmitMHLOS ~  Black + Hispanic + Asian + NativeAmerican, 
                       data = hospital_data, na.action = na.omit)

# Assess models (all models converged)
AIC(mhlos.gaus.id, mhlos.gaus.log, mhlos.gaus.inv, 
    mhlos.gamma.inv, mhlos.gamma.id, mhlos.gamma.log, 
    mhlos.pois.log, mhlos.pois.id, mhlos.pois.sqrt,
    mhlos.ig.id, mhlos.ig.log, mhlos.ig.inv, 
    mhlos.nb)

# Best model: Inverse Gaussian !! NB and Gamma are close second



## LWBSrateAdjusted

# Adjust LWBSrate so values are between 0 and 1 
# Source: Smithson and Verkuilen, 2006
# URL: https://pdfs.semanticscholar.org/8517/79099a662b91c8153a9a40e069f0276eecbc.pdf
hospital_data$LWBSrateAdjusted <- (hospital_data$LWBSrate * (length(na.omit(hospital_data$LWBSrate)) - 1) + 0.5) / 
  length(na.omit(hospital_data$LWBSrate))

# Gaussian (Normal)

lwbs.gaus.id <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
lwbs.gaus.log <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "log"), data = hospital_data, na.action = na.omit)
lwbs.gaus.inv <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)

# GLM - Gamma

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

# Inverse Gaussian (Inverse Normal)

lwbs.ig.id <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "identity"), data = hospital_data, na.action = na.omit)
lwbs.ig.log <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)
lwbs.ig.inv <- glm(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
              family = inverse.gaussian(link = "inverse"), data = hospital_data, na.action = na.omit)

lwbs.nb <- glm.nb(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
                       data = hospital_data, na.action = na.omit)

lwbs.beta <- betareg(LWBSrateAdjusted ~  Black + Hispanic + Asian + NativeAmerican, 
                           data = hospital_data, na.action = na.omit)

# Assess models that converged / did not return error
AIC(lwbs.gaus.id, lwbs.gaus.log, lwbs.gaus.inv, 
    lwbs.gamma.inv, lwbs.gamma.id, lwbs.gamma.log, 
    lwbs.pois.log, lwbs.pois.id, lwbs.pois.sqrt, 
    lwbs.ig.id, lwbs.ig.log, 
    lwbs.nb, lwbs.beta)

# Best model: Beta !!



## Summary of best models

# AdmitLOS: Gamma
# WaitForBed: Negative Binomial
# NonAdmitLOS: Gamma
# NonAdmitMHLOS: Inverse Gaussian
# LWBSrateAdjusted: Beta





############################################################




#### AdmitLOS

# Model 1: Basic
alos.m1 <- glm(AdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                      family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)

# Model 2: Demographic covariates added
alos.m2 <- glm(AdmitLOS ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)

# Model 3: Hospital-level covariates added
alos.m3 <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                     Black + Hispanic + Asian + NativeAmerican, 
                       family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)

# Model 4: Both hospital-level and demographic covariates added
alos.m4 <- glm(AdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                     RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                       family = Gamma(link = "log"), data = hospital_data[-423,], na.action = na.omit)



##### WaitForBed

# Model 1: Basic
wfb.m1 <- glm.nb(WaitForBed ~ Black + Hispanic + Asian + NativeAmerican, 
                           data = hospital_data, na.action = na.omit)

# Model 2: Demographic covariates added
wfb.m2 <- glm.nb(WaitForBed ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                             Black + Hispanic + Asian + NativeAmerican, 
                           data = hospital_data, na.action = na.omit)

# Model 3: Hospital-level covariates added
wfb.m3 <- glm.nb(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
                             Black + Hispanic + Asian + NativeAmerican, 
                           data = hospital_data, na.action = na.omit)

# Model 4: Both hospital-level and demographic covariates added
wfb.m4 <- glm.nb(WaitForBed ~ Beds + ED.Volume + HospitalRating + 
               RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
               Black + Hispanic + Asian + NativeAmerican, 
             data = hospital_data[-423,], na.action = na.omit)


#### NonAdmitLOS

# Model 1: Basic
nlos.m1 <- glm(NonAdmitLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                   family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)

# Model 2: Demographic covariates added
nlos.m2 <- glm(NonAdmitLOS ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)

# Model 3: Hospital-level covariates added
nlos.m3 <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)

# Model 4: Both hospital-level and demographic covariates added
nlos.m4 <- glm(NonAdmitLOS ~ Beds + ED.Volume + HospitalRating + 
                     RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                     Black + Hispanic + Asian + NativeAmerican, 
                   family = Gamma(link = "log"), data = hospital_data, na.action = na.omit)


#### NonAdmitMHLOS

# Model 1: Basic
mhlos.m1 <- glm(NonAdmitMHLOS ~ Black + Hispanic + Asian + NativeAmerican, 
                    family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)

# Model 2: Demographic covariates added
mhlos.m2 <- glm(NonAdmitMHLOS ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)

# Model 3: Hospital-level covariates added
mhlos.m3 <- glm(NonAdmitMHLOS ~ Beds + ED.Volume + HospitalRating + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)

# Model 4: Both hospital-level and demographic covariates added
mhlos.m4 <- glm(NonAdmitMHLOS ~ Beds + ED.Volume + HospitalRating + 
                      RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                      Black + Hispanic + Asian + NativeAmerican, 
                    family = inverse.gaussian(link = "log"), data = hospital_data, na.action = na.omit)


##### LWBSrateAdjusted

# Model 1: Basic
lwbs.m1 <- betareg(LWBSrateAdjusted ~ Black + Hispanic + Asian + NativeAmerican, 
                                data = hospital_data, na.action = na.omit)

# Model 2: Demographic covariates added
lwbs.m2 <- betareg(LWBSrateAdjusted ~ RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                                  Black + Hispanic + Asian + NativeAmerican, 
                                data = hospital_data, na.action = na.omit)

# Model 3: Hospital-level covariates added
lwbs.m3 <- betareg(LWBSrateAdjusted ~ Beds + ED.Volume + HospitalRating + 
                                  Black + Hispanic + Asian + NativeAmerican, 
                                data = hospital_data, na.action = na.omit)

# Model 4: Both hospital-level and demographic covariates added
lwbs.m4 <- betareg(LWBSrateAdjusted ~ Beds + ED.Volume + HospitalRating + 
                                  RuralScore + MedianAge + SexRatio + MedicaidExpansion + 
                                  Black + Hispanic + Asian + NativeAmerican, 
                                data = hospital_data, na.action = na.omit)



### Inspect Model 1s (Race variables ONLY)
summary(alos.m1)
summary(wfb.m1)
summary(nlos.m1)
summary(mhlos.m1)
summary(lwbs.m1)

### Inspect Model 2s (Race variables + demographics)
summary(alos.m2)
summary(wfb.m2)
summary(nlos.m2)
summary(mhlos.m2)
summary(lwbs.m2)

### Inspect Model 3s (Race variables + hospital-level covariates)
summary(alos.m3)
summary(wfb.m3)
summary(nlos.m3)
summary(mhlos.m3)
summary(lwbs.m3)

### Inspect Model 4s (Race variables + demographics + hospital-level covariates)
summary(alos.m4)
summary(wfb.m4)
summary(nlos.m4)
summary(mhlos.m4)
summary(lwbs.m4)










########################################################################

### MODEL DIAGNOSTICS

# Component+Residual Plots (11 plots each)
sapply(c(names(coef(alos.m4))[c(2, 6:9, 11:14)], "ED.Volume", "MedicaidExpansion"), 
       function(x){crPlot(alos.m4, x)})
sapply(c(names(coef(wfb.m4))[c(2, 6:9, 11:14)], "ED.Volume", "MedicaidExpansion"), 
       function(x){crPlot(alos.m4, x)})
sapply(c(names(coef(nlos.m4))[c(2, 6:9, 11:14)], "ED.Volume", "MedicaidExpansion"), 
       function(x){crPlot(alos.m4, x)})
sapply(c(names(coef(mhlos.m4))[c(2, 6:9, 11:14)], "ED.Volume", "MedicaidExpansion"), 
       function(x){crPlot(alos.m4, x)})
sapply(c(names(coef(lwbs.m4))[c(2, 6:9, 11:14)], "ED.Volume", "MedicaidExpansion"), 
       function(x){crPlot(alos.m4, x)})

# Assessing constant variance
# The residuals should exhibit roughly equal spread across the range of the fitted values
plot(alos.m4, which = 1)
plot(wfb.m4, which = 1)
plot(nlos.m4, which = 1)
plot(mhlos.m4, which = 1)
plot(lwbs.m4, which = 4)
# Residuals appear random across the range of fitted values for all 5 models

# Independence
# The ordered residuals should resemble a random scatter of points about the horizontal axis

# Run all at once
plot(resid(alos.m4), type = "b")
plot(resid(wfb.m4), type = "b")
plot(resid(nlos.m4), type = "b")
plot(resid(mhlos.m4), type = "b")
plot(resid(lwbs.m4), type = "b")
# Scatter is approximately random for all 5 models

# Linearity on link scale
plot(alos.m4, which = 2)
plot(wfb.m4, which = 2)
plot(nlos.m4, which = 2)
plot(mhlos.m4, which = 2)
qqnorm(resid(lwbs.m4))
qqline(resid(lwbs.m4), lwd = 1, lty = 3)
# Results mostly hug the horizontal line, except for lwbs.m4

# Check for multicollinearity
vif(alos.m4) 
vif(wfb.m4) 
vif(nlos.m4) 
vif(mhlos.m4) 
vif(lwbs.m4) 
# No issues here - Max VIF is 2.58 for ED.Volume


