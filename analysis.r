######### DATA LOADING #######

setwd("C:/Users/Jeroen/files/OneDrive - TU Eindhoven/TU_e/Q2/Linear statistical models/data analysis assignment")
#setwd("c:/Users/20174938/files/OneDrive - TU Eindhoven/TU_e/Q2/Linear statistical models/data analysis assignment")
figures_folder = "./figures"

library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

# We first inspect the structure of the file manually.
writeLines(read_lines("RPBU20102011.txt", n_max=10))

# There are 3421442 lines in the file, the last one is blank,
# so 3421440 rows with actual data.

rpbu = read_delim(
  "RPBU20102011.txt",
  delim=" ",
  skip=1,
  col_names = c("water", "datetime"),
  col_types = cols(
    water = col_double(),
    datetime = col_datetime(format = "")
  )
)

# Make sure the data are sorted on the datetime (we don't assume it).
rpbu[order(as.Date(rpbu$datetime)),]

# Add columns for year, month and day.
rpbu = rpbu %>%
  mutate(year = year(datetime), month = month(datetime), day = day(datetime))

# There are 8640 observations per day.
rpbu %>% filter(year == 2010 & month == 3 & day == 1)
rpbu %>% filter(year == 2010 & month == 3 & day == 2)
rpbu %>% filter(year == 2010 & month == 3 & day == 3)

# Let's look at some basic statistic of the water measurements.
summary(rpbu$water)
hist(rpbu$water)

# It seems that there are some missing values (NA's) in the data set.
# Moreover, the maximum value of 9995.000 is a little bit suspicious.
# Let's check how many of these wrong measurements there are.
# We assume that the water will never really be higher than around a couple of
# meters, so we try 500 centimeter.
rpbu %>% filter(water >= 500)

# It seems this wrong measurement is the only one in the data, so we decide to
# just set this value as missing.
rpbu$water[rpbu$water==9995] = NA

# Let's look at basic statistics again.
summary(rpbu$water)
hist(rpbu$water)

# We also want to have a rough idea about the location of the missing values.
rpbu %>% filter(is.na(rpbu$water)) %>% count(year, month)
# So it seems that March, April, September and November 2010 have had the most
# missing values.

# Now let's pick a day on which the water rose above 2.30 meters.
rpbu %>% filter(rpbu$water > 230) %>% count(year, month, day)

# We choose to plot all these days, because we saw that some of them have wrong
# (high) measurements.

# Helper function for plotting the water measurements on a particular day.
plot_day = function(y, m, d, save=FALSE) {
  day.data = rpbu %>% filter(year == y, month == m, day == d) 
  plot1 = ggplot(data = day.data, aes(datetime, water)) + geom_line()
  if (save) {
    ggsave(paste("day_", y, "-", m, "-", d, ".png", sep=""), path = figures_folder)
  } else {
    return(plot1)
  }
}

plot_day(2010, 3, 1)
plot_day(2010, 4, 6) # spike
plot_day(2010, 8, 29)
plot_day(2010, 8, 30)
plot_day(2010, 9, 25)
plot_day(2010, 9, 26)
plot_day(2010, 10, 4) # spike
plot_day(2010, 10, 24)

# One way to spot these peaks is to disregard measurements that deviate a lot
# from the measurement directly before it.

differences = diff(rpbu$water)
differences = c(differences, NA) # make it the same length as the original vector
summary(differences)
hist(differences)
rpbu = rpbu %>% mutate(difference = differences)

# (takes a long time to render)
ggplot(data = rpbu, aes(datetime, difference)) + geom_line()

head(rpbu)

# Get an overview of the days with big differences (more than 10 cm difference).
big_differences = rpbu %>% filter(rpbu$difference > 10) %>% count(year, month, day)
big_differences
# From this overview, we see some suspicious days:
# Year,  M,  d
# 2010,  4,  6
# 2010,  5,  2
# 2010,  5,  3
# 2010, 10,  4
# 2010, 11, 14
# 2010, 11, 23 
# 2011,  2, 20


big_differences %>%
  select(year, month, day) %>%
  rename(y = year, m= month, d = day) %>%
  mutate(save = TRUE) %>%
  pwalk(plot_day) 

######### HELPER FUNCTIONS ##########

# Helper function to plot between specific points in time.
plot_day_time = function(start_time, end_time) {
  lims <- as.POSIXct(strptime(c(start_time, end_time), 
                              format = "%Y-%m-%d %H:%M:%S",
                              tz="UTC"))
  
  day.data = rpbu %>% filter(datetime >= lims[1] & datetime <= lims[2]) 
  
  ggplot(data = day.data, aes(datetime, water)) +
    geom_line() +
    scale_x_datetime(labels = date_format("%H:%M"),
                     breaks = date_breaks(width = "60 min"),
                     limits = lims)
}

# Helper function to get the data from start_time until and including end_time.
# Also adds a column with a scaled time axis from 0 to 1, relative from
# [global_start, global_end].
get_period_scaled = function(start_time, end_time, global_start, global_end) {
  lims <- as.POSIXct(strptime(c(start_time, end_time),
                              format = "%Y-%m-%d %H:%M:%S",
                              tz="UTC"))
 
  day.data = rpbu %>%
    filter(datetime >= lims[1] & datetime <= lims[2])
  
  # get the actual first datetime and last datetime in this timespan
  start = head(day.data$datetime, n=1) 
  end = tail(day.data$datetime, n=1)

  # get the total number of seconds in the global interval
  global_seconds = time_length(interval(global_start, global_end))
 
  # add a column for t values in the interval [0,1]
  day.data = day.data %>%
    mutate(t = time_length(interval(global_start, datetime)) / global_seconds)
  
  return(day.data)
}

# Get the data in the given interval, scaled on [0,1].
get_period = function(start_time, end_time) {
  return(get_period_scaled(start_time, end_time, start_time, end_time))
}

######### EXERCISE 1 #########

# We zoom in at the moments of high tide (at 2010-03-01).
plot_day(2010, 3, 1, save=FALSE)

# peak 1
plot_day_time("2010-03-01 00:00:00", "2010-03-01 05:30:00")
plot_day_time("2010-03-01 01:10:00", "2010-03-01 04:30:00")

# peak 2
plot_day_time("2010-03-01 12:00:00", "2010-03-01 19:00:00")
plot_day_time("2010-03-01 13:35:00", "2010-03-01 17:00:00")


# We zoom in at the moments of high tide (at 2010-9-26).
plot_day(2010, 9, 26, save=FALSE)

# peak 1
plot_day_time("2010-09-26 01:00:00", "2010-09-26 07:00:00")
plot_day_time("2010-09-26 02:25:00", "2010-09-26 06:00:00")

# peak 2
plot_day_time("2010-09-26 13:00:00", "2010-09-26 19:30:00")
plot_day_time("2010-09-26 14:45:00", "2010-09-26 18:00:00")


######### EXERCISE 2 #########

library(zoo)

peak1 = get_period("2010-03-01 12:00:00", "2010-03-01 19:00:00")
peak2 = get_period("2010-09-26 13:00:00", "2010-09-26 19:30:00")

# We compute a rolling average for the two peaks on 2010-03-01.
peak1 %>%
  mutate(mva_10 = rollmean(water, k=60, fill=NA)) %>%
  ggplot(aes(datetime, mva_10)) +
  geom_line(aes(y=mva_10)) +
  scale_x_datetime(labels = date_format("%H:%M"),
                   breaks = date_breaks(width = "60 min"))

peak2 %>%
  mutate(mva_10 = rollmean(water, k=60, fill=NA)) %>%
  ggplot(aes(datetime, mva_10)) +
  geom_line(aes(y=mva_10)) +
  scale_x_datetime(labels = date_format("%H:%M"),
                 breaks = date_breaks(width = "60 min"))

######### EXERCISE 3 #########

# We think that the trend of the tide is well-modeled by a composition of two
# sinusoidal functions. By experimenting with the parameters, we found that when
# the smaller oscillation has a frequency that is three times that of the larger
# oscillation, then we get a shape that is very similar to the plots of exercise
# 2.

t = seq(0,10,.01)
a = 6
b = 1
w1 = 1
w2 = 3
phi1 = 0
phi2 = 0.3
y = a*sin(w1*(t-phi1)) + b*sin(w2*(t-phi2))
plot(t, y, type="l")


######### EXERCISE 4 #########

# Sinusoidal model
get_sm = function(day.data, w1, p1, w2, p2, w3, p3, w4, p4, w5, p5) {
  return(lm(water ~ sin(w1*(t-p1)) + sin(w2*(t-p2)) + sin(w3*(t-p3)) + sin(w4*(t-p4)) + sin(w5*(t-p5)), data=day.data))
}

plot_sm = function(day.data, sm) {
  day.data %>%
    mutate(pred = predict(sm, day.data)) %>%
    ggplot(aes(t, water)) +
    geom_line() +
    geom_line(aes(x = t, y = pred))
}

######### EXERCISE 4.1 #######

start1 = "2010-03-01 00:00:00"
end1 = "2010-03-01 05:30:00"
total_seconds_1 = time_length(interval(start1, end1)) 

sm1 = get_sm(get_period(start1, end1),
          w1 = 1.58 * pi,
          p1 = 0.16,
          w2 = 4.0 * pi,
          p2 = 0.165,
          w3 = 545 / total_seconds_1,
          p3 = 0,
          w4 = 205 / total_seconds_1,
          p4 = 0,
          w5 = 85 / total_seconds_1,
          p5 = 0)
plot_sm(get_period(start1, end1), sm1)

# It seems very difficult to fit this kind of model, even with the knowledge about the approximate
# periods of the three smaller oscillations. The addition of the three smaller oscillations does
# not seem to really improve the fit of the model.

# We now to fit on a smaller time windows, more before the actual peak.

start2 = "2010-03-01 00:00:00"
end2 = "2010-03-01 02:30:00"
plot_day_time(start2, end2)

total_seconds_2 = time_length(interval(start2, end2)) 

# We want to use the parameters, that we already found with the larger window.
# Therefore, we need to scale them

sm2 = get_sm(get_period(start2, end2),
          w1 = 1.58 * pi * total_seconds_2 / total_seconds_1,
          p1 = 0,
          w2 = 4.0 * pi * total_seconds_2 / total_seconds_1,
          p2 = 0,
          w3 = 545 / total_seconds_2,
          p3 = 0,
          w4 = 205 / total_seconds_2,
          p4 = 0,
          w5 = 85 / total_seconds_2,
          p5 = 0)
plot_sm(get_period(start2, end2), sm2)

# The last smallest oscillation does not make such a difference.
# But adding 1 and 2 does seem to improve the fit a lot.
# We could verify this by comparing nested models.

# In order to make real predictions, we must fit on data points before the actual peak.
start3 = "2010-03-01 00:00:00"
end3 = "2010-03-01 01:30:00"
plot_day_time(start3, end3)

total_seconds_3 = time_length(interval(start3, end3)) 

# We want to use the parameters, that we already found with the larger window.
# Therefore, we need to scale them

sm3 = get_sm(get_period_scaled(start3, end3, start1, end1),
          w1 = 1.58 * pi * total_seconds_3 / total_seconds_1,
          p1 = 0,
          w2 = 4.0 * pi * total_seconds_3 / total_seconds_1,
          p2 = 0,
          w3 = 545 / total_seconds_3,
          p3 = 0,
          w4 = 205 / total_seconds_3,
          p4 = 0,
          w5 = 85 / total_seconds_3,
          p5 = 0)
summary(sm3)
plot_sm(get_period_scaled(start3, end3, start1, end1), sm3)

# Now we are going to make predictions with the previous model.
# (need to automate this, make a function for it to try different 5-minute forward predictions)

plot_sm(get_period_scaled(start3, "2010-03-01 01:55:00", start1, end1), sm3)

# what it looks like at this moment, the prediction is not really good...

######### EXERCISE 4.2 #######

# We now want to make te previous approach a little bit more systematic, such 
# that we only need to supply the timespan to fit on and the time that we want
# to predict ahead.

# Fits a model on the data in the interval [start, end], uses a linear model
# with regressors sin(w(t-p)) where pairs (t,p) need to be in the lists periods
# and phases.
fit_linear = function(start, end, periods, phases) {
  day.data = get_period(start, end)
  
  total_seconds = time_length(interval(start, end))

  if (length(periods) != length(phases))
    stop('periods and phases not of same length')
  
  l = cbind(periods, phases)
  # dynamically construct the formula
  form = "water ~ "
  for (i in 1:nrow(l)) {
    w = total_seconds / l[[i,1]] * (2*pi)
    p = l[[i,2]] / total_seconds
    if (i > 1) form = paste(form, " + ", sep="")
    if (p >= 0) form = paste(form, "sin(", w, "*(t-", p, "))", sep="")
    else form = paste(form, "sin(", w, "*(t+", -p, "))", sep="")
  }
  print(form)
  lm(as.formula(form), data=day.data)
}

# Predict the number of seconds ahead.
predict_linear = function(start, end, sm, ahead=0) {
  # get a little bit more data for prediction
  d = format(as.POSIXct(end, tz="UTC") + seconds(ahead), usetz=FALSE)
  day.data = get_period_scaled(start, d, start, end)
  day.data %>%
    mutate(pred = predict(sm, day.data)) %>%
    ggplot(aes(t, water)) +
    geom_line() +
    geom_line(aes(x = t, y = pred)) +
    geom_vline(xintercept = 1, linetype="dotted", color = "blue", size=1) # to indicate from which point on is the prediction
}

# example1
start1 = "2010-09-26 00:00:00"
end1 = "2010-09-27 12:00:00"

sm1 = fit_linear(start1, end1, c(12.3*3600, 12.3/5*3600), c(0.8*3600, -1.4*3600))
sm1$coefficients

predict_linear(start1, end1, sm1)
predict_linear(start1, end1, sm1, ahead=6*3600)


# example2
start2 = "2010-09-26 06:00:00"
end2 = "2010-09-26 22:00:00"

sm2 = fit_linear(start2, end2, c(12.3*3600, 12.3/5*3600), c(0.8*3600, -1.3*3600))

predict_linear(start2, end2, sm2)
predict_linear(start2, end2, sm2, ahead=6*3600)


# real fit
start3 = "2010-03-01 11:50:00"
end3 = "2010-03-01 13:50:00"

sm3 = fit_linear(start3, end3, c(12.3*3600, 12.3/5*3600, 545, 205, 85), c(0*3600, 4*3600, 0, 0, 0))
predict_linear(start3, end3, sm3, ahead=5*60)

sm3 = fit_linear(start3, end3, c(12.3*3600, 12.3/5*3600, 545, 205, 85), c(0*3600, 4.1*3600, 0, 0, 0))
predict_linear(start3, end3, sm3, ahead=5*60)
predict_linear(start3, end3, sm3, ahead=2*60*60)

# real fit 2
start4 = "2010-09-25 14:50:00"
end4 = "2010-09-26 02:50:00"

sm4 = fit_linear(start4, end4, c(12.3*3600, 12.3/5*3600, 545, 205, 85), c(-2.3*3600, 1.2*3600, 200, 0, 0))
predict_linear(start4, end4, sm4, ahead=0*60)


######### EXERCISE 5 #########

# We are now going to suggest a non-linear model for the trend and oscillation.
# From the addition rule for sines, we get the following formula structure
#
#   y ~ b0 + b1*sin(w1*t) + b2*cos(w1*t) + b3*sin(w2*t) + b4*cos(w2*t) + etc.
#
#

start = "2010-09-26 00:00:00"
end = "2010-09-26 06:00:00"

plot_day_time(start, end)
day.data = get_period(start, end)
total_seconds = time_length(interval(start, end))

w1_start = 12.3*3600 # each trend has a period of 12 hours approximately
w2_start = 12.3/5*3600 # each sub trend happens approximately some times each period
w1_start = total_seconds / w1_start * (2*pi) # scale the parameters
w2_start = total_seconds / w2_start * (2*pi) # scale the parameters

# remove missing values
day.data = na.omit(day.data)

# scale the response variables
day.data = day.data %>%
  mutate(water = (water - (-100))/400)

# expectation function, with manually supplied gradient
expFct <- function(t, b0, w1, b1, b2, w2 = 0, b3 = 0, b4 = 0) {
  mean = b0 + b1* sin(w1*t) + b2* cos(w1*t) + b3* sin(w2*t) + b4* cos(w2*t)
  #mean = b1* sin(w1*t) + b2* cos(w1*t)
  
  partial_w1 = b1*t*cos(w1*t) - b2*t*sin(w1*t)
  partial_w2 = b3*t*cos(w2*t) - b4*t*sin(w2*t)
  partial_b0 = 1
  partial_b1 = sin(w1*t)
  partial_b2 = cos(w1*t)
  partial_b3 = sin(w2*t)
  partial_b4 = cos(w2*t)
  
  # we supply the gradient manually
  attr(mean, "gradient") <- cbind(partial_b0, partial_w1, partial_b1, partial_b2, partial_w2, partial_b3, partial_b4)
  #attr(mean, "gradient") <- cbind(partial_w1, partial_b1, partial_b2)
  return(mean)
}


# Showing mean function for various parameter combinations 
plot(water ~ t, data = day.data, xlab = "t", ylab = "water", ylim=c(0, 1))
curve(expFct(x, b0=0.2, w1=w1_start, w2=w2_start, b1=250/400, b2=-100/400, b3=10/400, b4=10/400), add=TRUE)

library(minpack.lm)

# We try to use the Levenberg-Marquardt algorithm.
nm1 <- nlsLM(water ~ expFct(t, b0, w1, b1, b2, w2, b3, b4), data=day.data,
           start=list(b0=0.2, w1=w1_start, b1=250/500, b2=-100/400, w2=w2_start, b3=10/400, b4=10/400))

summary(nm1)

plot(water ~ t, data = day.data, ylim=c(0, 1), ylab="water")
lines(day.data$t, fitted(nm1), col="blue")


nm1 <- nls(water ~ expFct(t, b0, w1, b1, b2, w2, b3, b4), data=day.data,
             start=list(b0=0.2, w1=w1_start, b1=250/500, b2=-100/400, w2=w2_start, b3=10/400, b4=10/400))

summary(nm1)

plot(water ~ t, data = day.data, ylim=c(0, 1), ylab="water")
lines(day.data$t, fitted(nm1))


######### EXERCISE 5.1 #########

# expectation function, with manually supplied gradient
expFct <- function(t, b0,
                   w1, b1, b2,
                   w2, b3, b4,
                   w3, b5, b6,
                   w4, b7, b8,
                   w5, b9, b10) {
  mean = b0 +
    b1* sin(w1*t) + b2* cos(w1*t) +
    b3* sin(w2*t) + b4* cos(w2*t) +
    b5* sin(w3*t) + b6* cos(w3*t) +
    b7* sin(w4*t) + b8* cos(w4*t) +
    b9* sin(w5*t) + b10* cos(w5*t)
  
  partial_w1 = b1*t*cos(w1*t) - b2*t*sin(w1*t)
  partial_w2 = b3*t*cos(w2*t) - b4*t*sin(w2*t)
  partial_w3 = b5*t*cos(w3*t) - b6*t*sin(w3*t)
  partial_w4 = b7*t*cos(w4*t) - b8*t*sin(w4*t)
  partial_w5 = b9*t*cos(w5*t) - b10*t*sin(w5*t)

  partial_b0 = 1
  partial_b1 = sin(w1*t)
  partial_b2 = cos(w1*t)
  partial_b3 = sin(w2*t)
  partial_b4 = cos(w2*t)
  partial_b5 = sin(w3*t)
  partial_b6 = cos(w3*t)
  partial_b7 = sin(w4*t)
  partial_b8 = cos(w4*t)
  partial_b9 = sin(w5*t)
  partial_b10 = cos(w5*t)
  
  # we supply the gradient manually
  attr(mean, "gradient") <- cbind(partial_b0,
                                  partial_w1, partial_b1, partial_b2,
                                  partial_w2, partial_b3, partial_b4,
                                  partial_w3, partial_b5, partial_b6,
                                  partial_w4, partial_b7, partial_b8,
                                  partial_w5, partial_b9, partial_b10)
  return(mean)
}

library(minpack.lm) # for Levenberg-Marquardt

get_nls_model = function(start, end,
                         y_min=-100, y_max=400,
                         w1_start=12.3*3600, w2_start=12.3/5*3600, w3_start=545, w4_start=205, w5_start=85,
                         b0, b1, b2, b3, b4 ,b5, b6, b7, b8, b9, b10) {
  day.data = na.omit(get_period(start, end)) %>% # get data and remove missing values
    mutate(water = (water - (y_min))/y_max) # scale the response variable
  
  total_seconds = time_length(interval(start, end))
  w1_start = total_seconds / w1_start * (2*pi) # scale the parameters
  w2_start = total_seconds / w2_start * (2*pi) # scale the parameters
  w3_start = total_seconds / w3_start * (2*pi) # scale the parameters
  w4_start = total_seconds / w4_start * (2*pi) # scale the parameters
  w5_start = total_seconds / w5_start * (2*pi) # scale the parameters

  # We try to use the Levenberg-Marquardt algorithm.
  nm1 <- nlsLM(water ~ expFct(t, b0, w1, b1, b2, w2, b3, b4, w3, b5, b6, w4, b7, b8, w5, b9, b10), data=day.data,
               start=list(b0=b0, w1=w1_start, b1=b1, b2=b2,
                                 w2=w2_start, b3=b3, b4=b4,
                                 w3=w3_start, b5=b5, b6=b6,
                                 w4=w4_start, b7=b7, b8=b8,
                                 w5=w5_start, b9=b9, b10=b10), trace=TRUE, control=list(maxiter=1000))
  
}

# Predict the number of seconds ahead. This is for non-linear models, because
# we decided to scale the response variable as well.
predict_nls = function(start, end, nm, ahead=0, y_min=-100, y_max=400, i=0) {
  # get a little bit more data for prediction
  d = format(as.POSIXct(end, tz="UTC") + seconds(ahead), usetz=FALSE)
  day.data = na.omit(get_period_scaled(start, d, start, end)) %>%
    mutate(water = (water - (y_min))/y_max) # scale the response variable
  day.data %>%
    mutate(pred = predict(nm, day.data)) %>%
    ggplot(aes(t, water)) +
    geom_line() +
    geom_line(aes(x = t, y = pred)) +
    geom_vline(xintercept = 1, linetype="dotted", color = "blue", size=1) # to indicate from which point on is the prediction
  ggsave(paste("prediction", i, ".png", sep=""), path = figures_folder)
}


start = "2010-03-01 11:50:00"
end = "2010-03-01 13:50:00"

start = "2010-09-26 13:00:00"
end = "2010-09-26 15:00:00"

plot_day_time(start, end)
day.data = na.omit(get_period(start, end)) %>% # get data and remove missing values
  mutate(water = (water - (-100))/300) # scale the response variable

total_seconds = time_length(interval(start, end))
w1_start = 12.3*3600 # each trend has a period of 12 hours approximately
w2_start = 12.3/5*3600 # each sub trend happens approximately some times each period
w3_start = 545
w4_start = 205
w5_start = 85
w1_start = total_seconds / w1_start * (2*pi) # scale the parameters
w2_start = total_seconds / w2_start * (2*pi) # scale the parameters
w3_start = total_seconds / w3_start * (2*pi) # scale the parameters
w4_start = total_seconds / w4_start * (2*pi) # scale the parameters
w5_start = total_seconds / w5_start * (2*pi) # scale the parameters

# We fit a linear model to find starting values for the linear parameters.
lm1 = lm(water ~ sin(w1_start*t) + cos(w1_start*t)
        + sin(w2_start*t) + cos(w2_start*t)
        + sin(w3_start*t) + cos(w3_start*t)
        + sin(w4_start*t) + cos(w4_start*t)
        + sin(w5_start*t) + cos(w5_start*t)
        , data=day.data)
b0 = lm1$coefficients[1]
b1 = lm1$coefficients[2]
b2 = lm1$coefficients[3]
b3 = lm1$coefficients[4]
b4 = lm1$coefficients[5]
b5 = lm1$coefficients[6]
b6 = lm1$coefficients[7]
b7 = lm1$coefficients[8]
b8 = lm1$coefficients[9]
b9 = lm1$coefficients[10]
b10 = lm1$coefficients[11]

# Showing mean function for various parameter combinations 
plot(water ~ t, data = day.data, xlab = "t", ylab = "water", ylim=c(0, 1))
curve(expFct(x, b0=b0, w1=w1_start, w2=w2_start, w3=w3_start, w4=w4_start, w5=w5_start,
             b1=b1, b2=b2, b3=b3, b4=b4, b5=b5, b6=b6, b7=b7, b8=b8, b9=b9, b10=b10), add=TRUE)


nm1 = get_nls_model(start, end, b0=b0, b1=b1, b2=b2, b3=b3, b4=b4, b5=b5, b6=b6, b7=b7, b8=b8, b9=b9, b10=b10)

summary(nm1)$coef
summary(nm1)

predict_nls(start, end, nm1, ahead=5*60)
predict_nls(start, end, nm1, ahead=2*60*60, i=1)


# Make a prediction for the next 5 minutes, every 1 minute.
for (i in 0:40) {
  ahead = i*60
  d = format(as.POSIXct(end, tz="UTC") + seconds(ahead), usetz=FALSE)
  nm1 = get_nls_model(start, d, b0=b0, b1=b1, b2=b2, b3=b3, b4=b4, b5=b5, b6=b6, b7=b7, b8=b8, b9=b9, b10=b10)
  predict_nls(start, d, nm1, ahead=5*60, i=i)
}



# Make a prediction for the next 5 minutes, every 1 minute.
# we now use the found values for the parameters as starter values for the next round.
for (i in 0:40) {
  ahead = i*60
  d = format(as.POSIXct(end, tz="UTC") + seconds(ahead), usetz=FALSE)
  nm1 = get_nls_model(start, d, b0=b0, b1=b1, b2=b2, b3=b3, b4=b4)
  predict_nls(start, d, nm1, ahead=5*60, i=i)
  
  # take the parameters as starting values for the next round
  b0 = summary(nm1)$coef[1]
  w1 = summary(nm1)$coef[2]
  b1 = summary(nm1)$coef[3]
  b2 = summary(nm1)$coef[4]
  w2 = summary(nm1)$coef[5]
  b3 = summary(nm1)$coef[6]
  b4 = summary(nm1)$coef[7]
}

