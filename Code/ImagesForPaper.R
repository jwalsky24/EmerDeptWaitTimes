






# Response variables
jpeg("Images/AdmitLOS.jpg")
truehist(hospital_data$AdmitLOS / 60, prob = F, xlim = c(0,14), xlab = "Time (Hours)", 
         ylab = "Number of Hospitals", main = "AdmitLOS", col = "#696969")
dev.off()

jpeg("Images/WaitForBed.jpg")
truehist(hospital_data$WaitForBed / 60, prob = F, xlim = c(0,8.5), xlab = "Time (Hours)", 
         ylab = "Number of Hospitals", main = "WaitForBed", col = "#808080")
dev.off()

jpeg("Images/NonAdmitLOS.jpg")
truehist(hospital_data$NonAdmitLOS / 60, prob = F, xlim = c(0,6), xlab = "Time (Hours)", 
         ylab = "Number of Hospitals", main = "NonAdmitLOS", col = "#A9A9A9")
dev.off()

jpeg("Images/MHLOS.jpg")
truehist(hospital_data$MHLOS /60, prob = F, xlim = c(0,18),  xlab = "Time (Hours)", 
         ylab = "Number of Hospitals", main = "MHLOS", col = "#C0C0C0")
dev.off()

jpeg("Images/LWBSrate.jpg")
truehist(hospital_data$LWBSrate * 100, prob = F, xlim = c(0,12), xlab = "Percentage", 
         ylab = "Number of Hospitals", main = "LWBSrate", col = "#D3D3D3", h = 1)
dev.off()







# Race covariates
jpeg("Images/Asian.jpg")
truehist(hospital_data$Asian * 100, main = "% of Asian / Pacific Islander Patients served", 
         xlab = "Percentage", ylab = "Number of Hospitals", xlim = c(0, 40), prob = F)
dev.off()

jpeg("Images/Black.jpg")
truehist(hospital_data$Black * 100, main = "% of Black / African American Patients served", 
         xlab = "Percentage", ylab = "Number of Hospitals", prob = F)
dev.off()

jpeg("Images/Hispanic.jpg")
truehist(hospital_data$Hispanic * 100, main = "% of Hispanic Patients served", 
         xlab = "Percentage", ylab = "Number of Hospitals", prob = F)
dev.off()

jpeg("Images/NativeAmerican.jpg")
truehist(hospital_data$NativeAmerican * 100, main = "% of Native American Patients served", 
         xlab = "Percentage", ylab = "Number of Hospitals", xlim = c(0, 30), prob = F)
dev.off()

jpeg("Images/White.jpg")
truehist(hospital_data$White * 100, main = "% of White Patients served", 
         xlab = "Percentage", ylab = "Number of Hospitals", prob = F)
dev.off()


# Demographic covariates

jpeg("Images/RuralScore.jpg")
hist(na.omit(hospital_data$RuralScore) * 100, col = 5, breaks = 50, main = "% of Patients from Rural area", 
     xlab = "Percentage", ylab = "Number of Hospitals")
dev.off()


jpeg("Images/MedianAge.jpg")
truehist(hospital_data$MedianAge, main = "Median Age of Patients Served", 
         xlab = "Age", ylab = "Number of Hospitals", prob = F)
dev.off()


jpeg("Images/SexRatio.jpg")
truehist(hospital_data$SexRatio, main = "Sex Ratio (Men per 100 Women)",
         xlab = "Men per 100 Women", ylab = "Number of Hospitals", xlim = c(60, 150), prob = F)
dev.off()


jpeg("Images/MedicaidExpansion.jpg")
medex.plot1 <- plot(na.omit(hospital_data$MedicaidExpansion), 
                    main = "Is Hospital in State that expanded Medicaid?", ylab = "Number of Hospitals", 
                    ylim = c(0, length(na.omit(hospital_data$MedicaidExpansion))), col = medex.colors)
text(x = medex.plot1, y = table(hospital_data$MedicaidExpansion), 
     labels = paste0(as.character(round(table(hospital_data$MedicaidExpansion) * 100 / 
                                          length(hospital_data$MedicaidExpansion), 1)), "%"), pos = 3)
dev.off()

# Hospital-level covariates

jpeg("Images/Beds.jpg")
truehist(hospital_data$Beds, xlim = c(0, 1500), main = "Beds per Hospital", 
         xlab = "Beds", ylab = "Number of Hospitals", prob = F)
dev.off()


jpeg("Images/Rating.jpg")
rating.plot1 <- plot(na.omit(as.factor(hospital_data$HospitalRating)), col = rating.colors, 
                     xlab = "Rating", ylab = "Number of Hospitals", main = "Hospital Rating (1 to 5 scale)", ylim = c(0,1300))
text(x = rating.plot1, y = table(as.factor(hospital_data$HospitalRating)), 
     labels = paste0(as.character(round(table(hospital_data$HospitalRating) * 100 / 
                                          length(hospital_data$HospitalRating), 1)), "%"), pos = 3)
dev.off()


jpeg("Images/Volume.jpg")
volume.plot1 <- plot(na.omit(hospital_data$ED.Volume), col = volume.colors, ylim = c(0, 2000),
                     xlab = "Volume Category", ylab = "Number of Hospitals", main = "Emergency Department Volume")
text(x = volume.plot1, y = table(hospital_data$ED.Volume), 
     labels = paste0(as.character(round(table(hospital_data$ED.Volume) * 100 /   
                                          length(hospital_data$ED.Volume), 1)), "%"), pos = 3)
dev.off()





par(mar = c(5.1, 4.1, 4.1, 2.1))
jpeg("Images/AdmitLOS_main.jpg")
truehist(hospital_data$AdmitLOS / 60, prob = F, xlim = c(0,14), xlab = "Time (Hours)", 
         ylab = "Number of Hospitals", main = "ED LOS: Admitted Patients")
dev.off()

jpeg("Images/AdmitLOSbyVolume.jpg")
boxplot(AdmitLOS / 60 ~ ED.Volume, data = hospital_data, ylim = c(0, 1000 / 60), col = volume.colors, 
        ylab = "ED LOS: Admitted Patients (Hours)", main = "AdmitLOS by Emergency Department Volume", xlab = "")
dev.off()

jpeg("Images/AdmitLOSbyRating.jpg")
boxplot(AdmitLOS / 60 ~ HospitalRating, data = hospital_data, ylim = c(0, 1000 / 60), col = rating.colors, 
        ylab = "ED LOS: Admitted Patients (Hours)", main = "AdmitLOS by Hospital Rating", xlab = "")
dev.off()

jpeg("Images/AdmitLOSbyRuralStatus.jpg")
boxplot(AdmitLOS / 60 ~ UrbanFlag, data = hospital_data, ylim = c(0,1000 / 60), col = rural.colors, 
        ylab = "ED LOS: Admitted Patients (Hours)", main = "AdmitLOS by Population Served", xlab = "")
dev.off()

jpeg("Images/AdmitLOSbyMedicaid.jpg")
boxplot(AdmitLOS / 60 ~ MedicaidExpansion, data = hospital_data, ylim = c(0, 900 / 60), 
        col = medex.colors, ylab = "ED LOS: Admitted Patients (Hours)", 
        main = "AdmitLOS by Medicaid Expansion Status", xlab = "")
dev.off()

jpeg("Images/AdmitLOSbyBeds.jpg")
boxplot(AdmitLOS / 60 ~ BedsFlag, data = hospital_data, ylim = c(0, 900 / 60), 
        col = beds.colors, ylab = "ED LOS: Admitted Patients (Hours)", 
        main = "AdmitLOS by Beds per Hospital", xlab = "")
dev.off()

jpeg("Images/AdmitLOSbyAge.jpg")
boxplot(AdmitLOS / 60 ~ AgeFlag, data = hospital_data, ylim = c(0, 900 / 60), 
        col = age.colors, ylab = "ED LOS: Admitted Patients (Hours)", 
        main = "AdmitLOS by Median Age of Patients Served", xlab = "")
dev.off()

jpeg("Images/AdmitLOSbyMissingRace.jpg")
barplot(xtabs(Percent ~ Status + Race , data = demos[demos$Response == "AdmitLOS",]), 
        beside = T, ylim = c(0, 20), col = missing.colors,
        legend = c("Missing", "Valid"), main = "Data Presence by Race/Ethnicity", 
        xlab = "Response Variable: ED LOS, Admitted Patients", ylab = "Percentage")
dev.off()

par(mar = c(5,5,4,5))
jpeg("Images/AdmitLOSbyMissingSize.jpg")
barplot(xtabs(Percent ~ Status + Covariate, data = hosp.alos), beside = T, legend = c("Missing", "Valid"), 
        main = "Data Presence by Size and Population Served", xlab = "Response Variable: ED LOS, Admitted Patients",
        ylab = "Number of Beds per Hospital", col = missing.colors)
axis(side = 4, at = c(0,50,100,150), labels = c("0", "25", "50", "75"))
mtext(side = 4, line = 2.75, "% of Patients from Rural area")
dev.off()





par(mar = c(5.1, 4.1, 4.1, 2.1))
jpeg("Images/WaitForBed_main.jpg")
truehist(hospital_data$WaitForBed / 60, prob = F, xlim = c(0,8.5), xlab = "Time (Hours)", 
         ylab = "Number of Hospitals", main = "Time Spent Waiting for Inpatient Bed")
dev.off()

jpeg("Images/WaitForBedbyVolume.jpg")
boxplot(WaitForBed / 60 ~ ED.Volume, data = hospital_data, ylim = c(0, 700 / 60), col = volume.colors, 
        ylab = "Hours Spent Waiting for Inpatient Bed", main = "WaitForBed by Emergency Department Volume", xlab = "")
dev.off()

jpeg("Images/WaitForBedbyRating.jpg")
boxplot(WaitForBed / 60 ~ HospitalRating, data = hospital_data, ylim = c(0, 700 / 60), col = rating.colors, 
        ylab = "Hours Spent Waiting for Inpatient Bed", main = "WaitForBed by Hospital Rating", xlab = "")
dev.off()

jpeg("Images/WaitForBedbyRuralStatus.jpg")
boxplot(WaitForBed / 60 ~ UrbanFlag, data = hospital_data, ylim = c(0,700 / 60), col = rural.colors,
        ylab = "Hours Spent Waiting for Inpatient Bed", main = "WaitForBed by Population Served", xlab = "")
dev.off()

jpeg("Images/WaitForBedbyMedicaid.jpg")
boxplot(WaitForBed / 60 ~ MedicaidExpansion, data = hospital_data, ylim = c(0, 700 / 60), 
        col = medex.colors, ylab = "Hours Spent Waiting for Inpatient Bed", 
        main = "WaitForBed by Medicaid Expansion Status", xlab = "")
dev.off()

jpeg("Images/WaitForBedbyBeds.jpg")
boxplot(WaitForBed / 60 ~ BedsFlag, data = hospital_data, ylim = c(0, 700 / 60), 
        col = beds.colors, ylab = "Hours Spent Waiting for Inpatient Bed", 
        main = "WaitForBed by Beds per Hospital", xlab = "")
dev.off()

jpeg("Images/WaitForBedbyAge.jpg")
boxplot(WaitForBed / 60 ~ AgeFlag, data = hospital_data, ylim = c(0, 700 / 60), 
        col = age.colors, ylab = "Hours Spent Waiting for Inpatient Bed", 
        main = "WaitForBed by Median Age of Patients Served", xlab = "")
dev.off()

jpeg("Images/WaitForBedbyMissingRace.jpg")
barplot(xtabs(Percent ~ Status + Race , data = demos[demos$Response == "WaitForBed",]), 
        beside = T, ylim = c(0, 20), col = missing.colors, 
        legend = c("Missing", "Valid"), main = "Data Presence by Race/Ethnicity", 
        xlab = "Response Variable: Time Spent Waiting for Inpatient Bed", ylab = "Percentage")
dev.off()

par(mar = c(5,5,4,5))
jpeg("Images/WaitForBedbyMissingSize.jpg")
barplot(xtabs(Percent ~ Status + Covariate, data = hosp.wfb), beside = T, legend = c("Missing", "Valid"), 
        main = "Data Presence by Size and Population Served", xlab = "Response Variable: Time Spent Waiting for Inpatient Bed",
        ylab = "Number of Beds per Hospital", col = missing.colors)
axis(side = 4, at = c(0,50,100,150), labels = c("0", "25", "50", "75"))
mtext(side = 4, line = 2.75, "% of Patients from Rural area")
dev.off()




par(mar = c(5.1, 4.1, 4.1, 2.1))
jpeg("Images/NonAdmitLOS_main.jpg")
truehist(hospital_data$NonAdmitLOS / 60, prob = F, xlim = c(0,6), xlab = "Time (Hours)", 
         ylab = "Number of Hospitals", main = "ED LOS: Non-Admitted Patients")
dev.off()

jpeg("Images/NonAdmitLOSbyVolume.jpg")
boxplot(NonAdmitLOS / 60 ~ ED.Volume, data = hospital_data, col = volume.colors, 
        ylab = "ED LOS: Non-Admitted Patients (Hours)", main = "NonAdmitLOS by Emergency Department Volume", xlab = "")
dev.off()

jpeg("Images/NonAdmitLOSbyRating.jpg")
boxplot(NonAdmitLOS / 60 ~ HospitalRating, data = hospital_data, col = rating.colors, 
        ylab = "ED LOS: Non-Admitted Patients (Hours)", main = "NonAdmitLOS by Hospital Rating", xlab = "")
dev.off()

jpeg("Images/NonAdmitLOSbyRuralStatus.jpg")
boxplot(NonAdmitLOS / 60 ~ UrbanFlag, data = hospital_data, col = rural.colors, 
        ylab = "ED LOS: Non-Admitted Patients (Hours)", main = "NonAdmitLOS by Population Served", xlab = "")
dev.off()

jpeg("Images/NonAdmitLOSbyMedicaid.jpg")
boxplot(NonAdmitLOS / 60 ~ MedicaidExpansion, data = hospital_data, 
        col = medex.colors, ylab = "ED LOS: Non-Admitted Patients (Hours)", 
        main = "NonAdmitLOS by Medicaid Expansion Status", xlab = "")
dev.off()

jpeg("Images/NonAdmitLOSbyBeds.jpg")
boxplot(NonAdmitLOS / 60 ~ BedsFlag, data = hospital_data, 
        col = beds.colors, ylab = "ED LOS: Non-Admitted Patients (Hours)", 
        main = "NonAdmitLOS by Beds per Hospital", xlab = "")
dev.off()

jpeg("Images/NonAdmitLOSbyAge.jpg")
boxplot(NonAdmitLOS / 60 ~ AgeFlag, data = hospital_data, 
        col = age.colors, ylab = "ED LOS: Non-Admitted Patients (Hours)", 
        main = "NonAdmitLOS by Median Age of Patients Served", xlab = "")
dev.off()

jpeg("Images/NonAdmitLOSbyMissingRace.jpg")
barplot(xtabs(Percent ~ Status + Race , data = demos[demos$Response == "NonAdmitLOS",]), 
        beside = T, ylim = c(0, 20), col = missing.colors, 
        legend = c("Missing", "Valid"), main = "Data Presence by Race/Ethnicity", 
        xlab = "Response Variable: ED LOS, Non-Admitted Patients", ylab = "Percentage")
dev.off()

par(mar = c(5,5,4,5))
jpeg("Images/NonAdmitLOSbyMissingSize.jpg")
barplot(xtabs(Percent ~ Status + Covariate, data = hosp.nlos), beside = T, legend = c("Missing", "Valid"), 
        main = "Data Presence by Size and Population Served", xlab = "Response Variable: ED LOS, Non-Admitted Patients",
        ylab = "Number of Beds per Hospital", col = missing.colors)
axis(side = 4, at = c(0,50,100,150), labels = c("0", "25", "50", "75"))
mtext(side = 4, line = 2.75, "% of Patients from Rural area")
dev.off()



par(mar = c(5.1, 4.1, 4.1, 2.1))
jpeg("Images/MHLOS_main.jpg")
truehist(hospital_data$MHLOS /60, prob = F, xlim = c(0,18),  xlab = "Time (Hours)", 
         ylab = "Number of Hospitals", main = "ED LOS: Mental Health Patients")
dev.off()

jpeg("Images/MHLOSbyVolume.jpg")
boxplot(MHLOS / 60 ~ ED.Volume, data = hospital_data, ylim = c(0, 1000 / 60), col = volume.colors, 
        ylab = "ED LOS: Mental Health Patients (Hours)", main = "MHLOS by Emergency Department Volume", xlab = "")
dev.off()

jpeg("Images/MHLOSbyRating.jpg")
boxplot(MHLOS / 60 ~ HospitalRating, data = hospital_data, ylim = c(0, 1000 / 60), col = rating.colors, 
        ylab = "ED LOS: Mental Health Patients (Hours)", main = "MHLOS by Hospital Rating", xlab = "")
dev.off()

jpeg("Images/MHLOSbyRuralStatus.jpg")
boxplot(MHLOS / 60 ~ UrbanFlag, data = hospital_data, ylim = c(0,1000 / 60), col = rural.colors,
        ylab = "ED LOS: Mental Health Patients (Hours)", main = "MHLOS by Population Served", xlab = "")
dev.off()

jpeg("Images/MHLOSbyMedicaid.jpg")
boxplot(MHLOS / 60 ~ MedicaidExpansion, data = hospital_data, ylim = c(0, 1000 / 60), 
        col = medex.colors,ylab = "ED LOS: Mental Health Patients (Hours)", 
        main = "MHLOS by Medicaid Expansion Status", xlab = "")
dev.off()

jpeg("Images/MHLOSbyBeds.jpg")
boxplot(MHLOS / 60 ~ BedsFlag, data = hospital_data, ylim = c(0, 1000 / 60), 
        col = beds.colors,ylab = "ED LOS: Mental Health Patients (Hours)", 
        main = "MHLOS by Beds per Hospital", xlab = "")
dev.off()

jpeg("Images/MHLOSbyAge.jpg")
boxplot(MHLOS / 60 ~ AgeFlag, data = hospital_data, ylim = c(0, 1000 / 60), 
        col = age.colors,ylab = "ED LOS: Mental Health Patients (Hours)", 
        main = "MHLOS by Median Age of Patients Served", xlab = "")
dev.off()

jpeg("Images/MHLOSbyMissingRace.jpg")
barplot(xtabs(Percent ~ Status + Race , data = demos[demos$Response == "MHLOS",]), 
        beside = T, ylim = c(0, 20), col = missing.colors, 
        legend = c("Missing", "Valid"), main = "Data Presence by Race/Ethnicity", 
        xlab = "Response Variable: ED LOS, Mental Health Patients", ylab = "Percentage")
dev.off()

par(mar = c(5,5,4,5))
jpeg("Images/MHLOSbyMissingSize.jpg")
barplot(xtabs(Percent ~ Status + Covariate, data = hosp.mhlos), beside = T, legend = c("Missing", "Valid"), 
        main = "Data Presence by Size and Population Served", xlab = "Response Variable: ED LOS, Mental Health Patients",
        ylab = "Number of Beds per Hospital", col = missing.colors)
axis(side = 4, at = c(0,50,100,150), labels = c("0", "25", "50", "75"))
mtext(side = 4, line = 2.75, "% of Patients from Rural area")
dev.off()





par(mar = c(5.1, 4.1, 4.1, 2.1))
jpeg("Images/LWBSrate_main.jpg")
truehist(hospital_data$LWBSrate * 100, prob = F, xlim = c(0,12), xlab = "Percentage", h = 1,
         ylab = "Number of Hospitals", main = "% of Patients who left ED without being seen")
dev.off()

jpeg("Images/LWBSratebyVolume.jpg")
boxplot(LWBSrate * 100 ~ ED.Volume, data = hospital_data, ylim = c(0, 10), col = volume.colors, 
        ylab = "% of Patients who left ED without being seen", main = "LWBSrate by Emergency Department Volume", xlab = "")
dev.off()

jpeg("Images/LWBSratebyRating.jpg")
boxplot(LWBSrate * 100 ~ HospitalRating, data = hospital_data, ylim = c(0, 10), col = rating.colors, 
        ylab = "% of Patients who left ED without being seen", main = "LWBSrate by Hospital Rating", xlab = "")
dev.off()

jpeg("Images/LWBSratebyRuralStatus.jpg")
boxplot(LWBSrate * 100 ~ UrbanFlag, data = hospital_data, ylim = c(0,10), col = rural.colors, 
        ylab = "% of Patients who left ED without being seen", main = "LWBSrate by Population Served", xlab = "")
dev.off()

jpeg("Images/LWBSratebyMedicaid.jpg")
boxplot(LWBSrate * 100 ~ MedicaidExpansion, data = hospital_data, ylim = c(0, 10), col = medex.colors, 
        ylab = "% of Patients who left ED without being seen", 
        main = "LWBSrate by Medicaid Expansion Status", xlab = "")
dev.off()

jpeg("Images/LWBSratebyBeds.jpg")
boxplot(LWBSrate * 100 ~ BedsFlag, data = hospital_data, ylim = c(0, 10), col = beds.colors, 
        ylab = "% of Patients who left ED without being seen", 
        main = "LWBSrate by Beds per Hospital", xlab = "")
dev.off()

jpeg("Images/LWBSratebyAge.jpg")
boxplot(LWBSrate * 100 ~ AgeFlag, data = hospital_data, ylim = c(0, 10), col = age.colors, 
        ylab = "% of Patients who left ED without being seen", 
        main = "LWBSrate by Median Age of Patients Served", xlab = "")
dev.off()

jpeg("Images/LWBSratebyMissingRace.jpg")
barplot(xtabs(Percent ~ Status + Race , data = demos[demos$Response == "LWBSrate",]), 
        beside = T, ylim = c(0, 20), col = missing.colors, 
        legend = c("Missing", "Valid"), main = "Data Presence by Race/Ethnicity", 
        xlab = "Response Variable: LWBS Rate", ylab = "Percentage")
dev.off()

par(mar = c(5,5,4,5))
jpeg("Images/LWBSratebyMissingSize.jpg")
barplot(xtabs(Percent ~ Status + Covariate, data = hosp.lwbs), beside = T, legend = c("Missing", "Valid"), 
        main = "Data Presence by Size and Population Served", sub = "Response Variable: LWBS Rate",
        ylab = "Number of Beds per Hospital", col = missing.colors)
axis(side = 4, at = c(0,50,100,150), labels = c("0", "25", "50", "75"))
mtext(side = 4, line = 2.75, "% of Patients from Rural area")
dev.off()









jpeg("Images/resid.alos.jpg")
plot(predict(alos.final.m4, type = "response"), residuals(alos.final.m4, type = "response"),
     ylab = "Residuals", xlab = "Predicted values", main = "AdmitLOS")
dev.off()

jpeg("Images/resid.wfb.jpg")
plot(predict(wfb.final.m4, type = "response"), residuals(wfb.final.m4, type = "response"),
     ylab = "Residuals", xlab = "Predicted values", main = "WaitForBed")
dev.off()

jpeg("Images/resid.nlos.jpg")
plot(predict(nlos.final.m4, type = "response"), residuals(nlos.final.m4, type = "response"),
     ylab = "Residuals", xlab = "Predicted values", main = "NonAdmitLOS")
dev.off()

jpeg("Images/resid.mhlos.jpg")
plot(predict(mhlos.final.m4, type = "response"), residuals(mhlos.final.m4, type = "response"),
     ylab = "Residuals", xlab = "Predicted values", main = "MHLOS")
dev.off()

jpeg("Images/resid.lwbs.jpg")
plot(predict(lwbs.final.m4, type = "response"), residuals(lwbs.final.m4, type = "response"),
     ylab = "Residuals", xlab = "Predicted values", main = "LWBSrate")
dev.off()



jpeg("Images/order.alos.jpg")
plot(resid(alos.final.m4), type = "b", xlab = "Observation Number", 
     ylab = "Residuals", main = "AdmitLOS")
dev.off()

jpeg("Images/order.wfb.jpg")
plot(resid(wfb.final.m4), type = "b", xlab = "Observation Number", 
     ylab = "Residuals", main = "WaitForBed")
dev.off()

jpeg("Images/order.nlos.jpg")
plot(resid(nlos.final.m4), type = "b", xlab = "Observation Number", 
     ylab = "Residuals", main = "NonAdmitLOS")
dev.off()

jpeg("Images/order.mhlos.jpg")
plot(resid(mhlos.final.m4), type = "b", xlab = "Observation Number", 
     ylab = "Residuals", main = "MHLOS")
dev.off()

jpeg("Images/order.lwbs.jpg")
plot(resid(lwbs.final.m4), type = "b", xlab = "Observation Number", 
     ylab = "Residuals", main = "LWBSrate")
dev.off()






jpeg("Images/qq.alos.jpg")
qqPlot(resid(alos.final.m4) / sd(alos.final.m4$residuals), dist = "gamma", 
       shape = as.numeric(MASS::gamma.shape(alos.final.m4)[1]),
       main = "AdmitLOS - Gamma Distribution", xlab = "Gamma Quantiles", 
       ylab = "Sample Quantiles", envelope = F)
dev.off()

jpeg("Images/qq.wfb.jpg")
qqPlot(resid(wfb.final.m4), "nbinom", size = wfb.final.m4$theta, 
       mu = exp(coef(wfb.final.m4)[1]),
       main = "WaitForBed - Negative Binomial Distribution", xlab = "Negative Binomial Quantiles", 
       ylab = "Sample Quantiles", envelope = F)
dev.off()

jpeg("Images/qq.nlos.jpg")
qqPlot(resid(nlos.final.m4) / sd(resid(nlos.final.m4)), dist = "gamma", shape = as.numeric(MASS::gamma.shape(nlos.final.m4)[1]),
       main = "NonAdmitLOS - Gamma Distribution", xlab = "Gamma Quantiles", 
       ylab = "Sample Quantiles", envelope = F)
dev.off()

jpeg("Images/qq.mhlos.jpg")
qqPlot(resid(mhlos.final.m4) / sd(resid(mhlos.final.m4)), dist = "invgauss",
       main = "MHLOS - Inverse Gaussian Distribution", xlab = "Inverse Gaussian Quantiles", 
       ylab = "Sample Quantiles", envelope = F)
dev.off()

jpeg("Images/qq.lwbs.jpg")
qqPlot(resid(lwbs.final.m4), 
       dist = "beta", 
       shape1 = exp(coef(lwbs.final.m4)[1]) * lwbs.final.m4$coefficients$precision, 
       shape2 = (1 - exp(coef(lwbs.final.m4)[1])) * lwbs.final.m4$coefficients$precision,
       main = "LWBSrate - Beta Distribution", xlab = "Beta Quantiles", ylab = "Sample Quantiles", envelope = F)
dev.off()

