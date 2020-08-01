




# Read in data form CSV
hospital_data <- read.csv("WaitTimeData.csv")
 
# Attach variables to make life easier
attach(hospital_data)

fisher.test(BlackCat, NotSeen)
?fisher.test
cor(na.omit(hospital_data[,2:21]))

t.test(baa_above$NotSeen, baa_below$NotSeen)
# Hospitals serving above average proportions of Black patients have 66.9% higher rates 
# of patients leaving the emergency department before being seen than hospitals serving
# below-average rates of Black patients (t=14.3, p<.001).

library(descr)
library(gmodels)
library(knitr)

baa_above <- subset(hospital_data, Black > mean(Black))
baa_below <- subset(hospital_data, Black <= mean(Black))

mean(na.omit(baa_above$NotSeen))
mean(na.omit(baa_above$NotSeen))

t.test()

mean(hospital_data$Black)

h0_20 <- subset(hospital_data, Black <= 0.20)
h20_40 <- subset(hospital_data, Black > 0.20 & Black <= 0.40)
h40_60 <- subset(hospital_data, Black > 0.40 & Black <= 0.60)
h60plus <- subset(hospital_data, Black > 0.60)


?xtabs
#h30_40 <- subset(hospital_data, Black > 0.30 & Black <= 0.40)
#h40_50 <- subset(hospital_data, Black > 0.40 & Black <= 0.50)
h50plus <- subset(hospital_data, Black > 0.50)


ftable(xtabs(TimeInER ~ MedicaidExpansion + Volume, data = hospital_data))

kable(hospital_data[,c("Volume", "Black")])


head(hospital_data[,c("Volume", "Black")])
gmodels::CrossTable(hospital_data$Volume, hospital_data$Rural, prop.t = F)


mean(na.omit(h0_10$TimeInER))
mean(na.omit(h10_20$TimeInER))
mean(na.omit(h20_30$TimeInER))
mean(na.omit(h30_40$TimeInER))
mean(na.omit(h40_50$TimeInER))
mean(na.omit(h50plus$TimeInER))

mean(na.omit(h0_10$ArrivalToAdmit))
mean(na.omit(h10_20$ArrivalToAdmit))
mean(na.omit(h20_30$ArrivalToAdmit))
mean(na.omit(h30_40$ArrivalToAdmit))
mean(na.omit(h40_50$ArrivalToAdmit))
mean(na.omit(h50plus$ArrivalToAdmit))




h1 <- subset(hospital_data, OverallRating %in% c(1,2))
sapply(h1[,12:19], mean)
h2 <- subset(hospital_data, OverallRating %in% c(3:5))
sapply(h2[,12:19], mean)




h3 <- subset(hospital_data, OverallRating == 3)
sapply(h3[,12:19], mean)
h4 <- subset(hospital_data, OverallRating == 4)
sapply(h4[,12:19], mean)
h5 <- subset(hospital_data, OverallRating == 5)
sapply(h5[,12:19], mean)

str(h1)





hist(exp(NotSeen))
questionr::odds.ratio(glm(NotSeen ~ Black, family = Gamma, data = hospital_data, na.action = na.omit))



