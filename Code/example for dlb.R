

# ArrivalToAdmit

# All race variables + controls
glm11 <- glm(ArrivalToAdmit ~ Black + Hispanic + Asian + NativeAmerican + Beds + OverallRating, 
             family = Gamma, data = hospital_data, na.action = na.omit)

glm11a <- glm(ArrivalToAdmit ~ OverallRating + Black + Hispanic + Asian + NativeAmerican + Beds, 
             family = Gamma, data = hospital_data, na.action = na.omit)

# Black/African-American + controls
glm12 <- glm(ArrivalToAdmit ~ Black + Beds + OverallRating, 
             family = Gamma, data = hospital_data, na.action = na.omit)

# All race variables
glm13 <- glm(ArrivalToAdmit ~ Black + Black + Asian + NativeAmerican, 
             family = Gamma, data = hospital_data, na.action = na.omit)

# Black/African-American
glm14 <- glm(ArrivalToAdmit ~ Black + Beds + OverallRating, 
             family = Gamma, data = hospital_data, na.action = na.omit)

library(car)
car::Anova(glm11)
car::Anova(glm11a)


mean(na.omit(subset(hospita)))
anova(glm11a)

summary(glm11)
summary(glm11a)


summary(glm12)
summary(glm13)
summary(glm14)
car::Anova(glm11)
anova(glm11)



# NotSeen: proportion of patients who leave ED before being seen

# All race variables + controls
glm51 <- glm(cbind(NotSeen*100,(1-NotSeen)*100) ~ Black + Hispanic + Asian + NativeAmerican + Beds + OverallRating, 
            family = binomial, data = hospital_data, na.action = na.omit)

# Hispanic + controls
glm52 <- glm(cbind(NotSeen*100,(1-NotSeen)*100) ~ Hispanic + Beds + OverallRating, 
              family = binomial, data = hospital_data, na.action = na.omit)

# All race variables
glm53 <- glm(cbind(NotSeen*100,(1-NotSeen)*100) ~ Black + Hispanic + Asian + NativeAmerican, 
             family = binomial, data = hospital_data, na.action = na.omit)

# Hispanic
glm54 <- glm(cbind(NotSeen*100,(1-NotSeen)*100) ~ Hispanic + Beds + OverallRating, 
             family = binomial, data = hospital_data, na.action = na.omit)



summary(glm51)
summary(glm511)
summary(glm52)
summary(glm53)
summary(glm54)
