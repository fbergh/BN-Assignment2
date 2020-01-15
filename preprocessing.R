################
### PACKAGES ###
################

library(psych) # For describe()



################
### SETTINGS ###
################

create_plots = TRUE



#################
### FUNCTIONS ###
#################

# Function for creating QQ plots
qq_plot = function(data,ann=FALSE) {
  qqnorm(data,ann=ann)
  qqline(data)
}



####################
### LOADING DATA ###
####################

# Read the data from the csv file
ff_orig = read.csv("data/forestfires.csv", header = TRUE)

# Show head of dataframe
head(ff_orig)



#####################
### PREPROCESSING ###
#####################

# Copy the original dataframe
ff = ff_orig


### REMOVING X, Y AND RAIN ###

# Remove X, Y, and rain variables
ff$X = NULL; ff$Y = NULL; ff$rain = NULL

# Show head of dataframe
head(ff)


### CHANGING DAY TO WEEKEND ###

# Convert day variable to boolean of weekend
weekend = c("sat","sun")
ff$weekend = as.double(sapply(ff$day, function(day) day %in% weekend))
ff$day = NULL;

# Show head of dataframe
head(ff)


### CHANGING MONTH TO AVG_TEMP AND AVG_WIND ###

# Convert month variable to numbers 
month_abb_lower = lapply(month.abb,tolower)
ff$month = match(ff$month,month_abb_lower)

# Add average temperatures per month of Braganca, a place nearby Monteshino Park
# Retrieved from https://www.climatestotravel.com/climate/portugal/bragan%C3%A7a
braganca_min_temp = c(0,1,3,5,8,12,14,14,12,8,4,1)
braganca_max_temp = c(9,11,15,16,20,26,29,29,25,18,13,10)
braganca_avg_temp = (braganca_min_temp + braganca_max_temp)/2
ff$avg_temp = braganca_avg_temp[ff$month]

# Add average wind speeds per month of Braganca, a place nearby Monteshino Park
# Retrieved from https://www.windfinder.com/windstatistics/braganca
braganca_avg_wind = c(5,6,6,6,5,5,5,5,4,4,5,4)
ff$avg_wind = as.double(braganca_avg_wind[ff$month])

# Remove month
ff$month = NULL

# Show head of dataframe
head(ff)


### CONVERTING ATTRIBUTES TO DOUBLE ###

# Make area and RH attributes double
ff$area = as.double(ff$area); ff$RH = as.double(ff$RH)

# Show head of dataframe
head(ff)


### OUTLIER REMOVAL ###

# If create_plots is set to "TRUE", create QQ-plots and histograms for the FFMC and ISI attributes
if(create_plots){
  png(filename="img/FFMC-qq.png");qq_plot(ff$FFMC);title(main="Quantile-quantile plot of FFMC attribute",xlab="Theoretical Quantiles",ylab="Sample Quantiles")
  dev.off()
  png(filename="img/FFMC-hist.png");hist(ff$FFMC, breaks=50, ann=FALSE);title(main="Histogram of FFMC attribute",xlab="FFMC",ylab="Frequency")
  dev.off()
  png(filename="img/ISI-qq.png");qq_plot(ff$ISI);title(main="Quantile-quantile plot of ISI attribute",xlab="Theoretical Quantiles",ylab="Sample Quantiles")
  dev.off()
  png(filename="img/ISI-hist.png");hist(ff$ISI, breaks=50, ann=FALSE);title(main="Histogram of ISI attribute",xlab="ISI",ylab="Frequency")
  dev.off()
}

# Remove clear outliers (measurement errors)
rows_before = nrow(ff)
ff = ff[-which(ff$FFMC < 60 | ff$ISI > 30),]
removed_outliers = rows_before - nrow(ff)
cat("Number of removed outliers: ", removed_outliers)

# If create_plots is set to "TRUE", create QQ-plots and histograms for the FFMC and ISI attributes with outliers removed
if(create_plots){
  png(filename="img/FFMC-qq-fixed.png");qq_plot(ff$FFMC);title(main="Quantile-quantile plot of FFMC attribute without outliers",xlab="Theoretical Quantiles",ylab="Sample Quantiles")
  dev.off()
  png(filename="img/FFMC-hist-fixed.png");hist(ff$FFMC, breaks=50, ann=FALSE);title(main="Histogram of FFMC attribute without outliers",xlab="FFMC",ylab="Frequency")
  dev.off()
  png(filename="img/ISI-qq-fixed.png");qq_plot(ff$ISI);title(main="Quantile-quantile plot of ISI attribute without outliers",xlab="Theoretical Quantiles",ylab="Sample Quantiles")
  dev.off()
  png(filename="img/ISI-hist-fixed.png");hist(ff$ISI, breaks=50, ann=FALSE);title(main="Histogram of ISI attribute without outliers",xlab="ISI",ylab="Frequency")
  dev.off()
}


### LOG TRANSFORM ###

# Perform logarithm transform (log(x+1)) of area similar to Cortez & Morais (2007)
ff$area = log(ff$area + 1.0)

# Do log(ISI+1) because of exponential curve in predictions; log-transform improves predictions
ff$ISI = log(ff$ISI + 1.0)



####################################
### DESCRIPTION & WRITING OUTPUT ###
####################################

# Show statistics of dataframe
describe(ff)

# Write the preprocessed data to a csv file
write.csv(ff,"data/ff_preprocessed.csv", row.names = FALSE)

