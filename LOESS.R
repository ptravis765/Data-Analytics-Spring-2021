# LOESS Example form
# http://r-statistics.co/Loess-Regression-With-R.html
data(economics, package='ggplot2') # load data
economics$index <- 1:nrow(economics) # create index variable
economics <- economics[1:80,] # retail 80 rows for better graphical understanding
loessMod10 <- loess(uempmed ~ index, data=economics, span=0.10) # 10% smoothing span
loessMod25 <- loess(uempmed ~ index, data=economics, span=0.25) # 25% smoothing span
loessMod50 <- loess(uempmed ~ index, data=economics, span=0.50) # 50% smoothing span

# Predict Loess
smoothed10 <- predict(loessMod10)
smoothed25 <- predict(loessMod25)
smoothed50 <- predict(loessMod50)

# From above plot, you would notice that as the span increases, the smoothing of the curve also increases.
# Code for Plot
# Plot it
plot(economics$uempmed, x=economics$date, type="l", main="Loess Smoothing and Prediction",
     xlab="Date", ylab="Unemployment (Median)")
lines(smoothed10, x=economics$date, col="red")
lines(smoothed25, x=economics$date, col="green")
lines(smoothed50, x=economics$date, col="blue")

# Fitting a curve to the data
# Local regression or local polynomial regression. It is one of the most common methods
# initially developed for scatterplot smoothing, are LOESS (locally estimated scatterplot smoothing) and
# LOWESS (locally weighted scatterplot smoothing)
# LOWESS example using Cars dataset
data("cars")
str(cars) # we need 50 observation and 2 variables
# now we create a plot, speed vs distance
plot(speed ~ dist, data = cars)
# When we look at the plot, we see that there is a positive relationship between these two variables

help("lowess")

# Now we will use the lowess() function
lowess(cars$speed ~ cars$dist)
# Now we will use the lowess() function along wit hthe line() function
# to draw the lines
lines(lowess(cars$speed ~ cars$dist, f=2/3),col="blue")
# here the f value is the smoother span. f=2/3 = 0.6666
# the default value for smoother span is 0.6666 in RStudio

# This gives the proportion of points in the plot which influence the smooth at each value
# Larger values give more smoothness
# Change the "f" value and observe the shape of the line
lines(lowess(cars$speed ~ cars$dist, f=0.75),col="gray") # f=0.75
lines(lowess(cars$speed ~ cars$dist, f=0.8),col="red") # f=0.8
lines(lowess(cars$speed ~ cars$dist, f=0.9),col="green") # f=0.9
lines(lowess(cars$speed ~ cars$dist, f=0.1),col=5) # f=0.1
lines(lowess(cars$speed ~ cars$dist, f=0.01),col=6) # f=0.01
# Observe that, when we try to have a very lower values for "f", in this example, it will try to overfit points
