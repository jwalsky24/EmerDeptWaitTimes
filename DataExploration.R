

## SIMPLE EDA

# Inspect correlations between numeric variables

cor(na.omit(hospital_data[,2:16]))



## IDENTIFY BEST DISTRIBUTION FOR POTENTIAL RESPONSE VARIABLES

# Function that fits each numeric column in hospital_data to each of several common distributions
# to identify the distribution with the best fit. 

compare_dist <- function(data){
  
  col <- ifelse(is.null(ncol(data)), 1, ncol(data))
  results <- matrix(nrow = 6, ncol = col)
  colnames(results) <- names(data)
  rownames(results) <- c("Normal", "Exp", "Poisson", "Gamma", "Weibull", "Lognormal")
  
  for(i in 1:col){
    
    d <- as.vector(na.omit(data[,i]))
    
    results[1,i] <- ifelse(is.numeric(try(fitdist(d, "norm")$aic)), fitdist(d, "norm")$aic, NA)
    results[2,i] <- ifelse(is.numeric(try(fitdist(d, "exp")$aic)), fitdist(d, "exp")$aic, NA)
    results[3,i] <- ifelse(is.numeric(try(fitdist(round(d *100), "pois")$aic)), fitdist(round(d *100), "pois")$aic, NA)
    results[4,i] <- ifelse(is.numeric(try(fitdist(d, "gamma")$aic)), fitdist(d, "gamma")$aic, NA)
    results[5,i] <- ifelse(is.numeric(try(fitdist(d, "weibull")$aic)), fitdist(d, "weibull")$aic, NA)
    results[6,i] <- ifelse(is.numeric(try(fitdist(d, "lnorm")$aic)), fitdist(d, "lnorm")$aic, NA)

    
  }
  
  return(results)
}

# Run function
compare_dist(hospital_data[,c(2:16)])



