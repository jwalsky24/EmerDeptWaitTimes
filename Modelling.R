








gam1 <- gamlss(TimeInER ~ Black + Asian + Hispanic + Volume, family = GA, data = na.omit(hospital_data[,c(4, 12, 14, 15, 17)]))
summary(gam1)

