getwd()
setwd("C:/Users/kadambini.indurkar/Documents/R_Sab_Isme")
getwd()

##############################################################
################Causal Impact ################################
##############################################################

library(CausalImpact)


# To illustrate how the package works, we create a simple toy dataset. It consists of a 
# response variable y and a predictor x1. Note that in practice, we'd strive for including 
# many more predictor variables and let the model choose an appropriate subset. The example 
# data has 100 observations. We create an intervention effect by lifting the response variable
# by 10 units after timepoint 71.


set.seed(1)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
x1
class(x1)
y <- 1.2 * x1 + rnorm(100) # rnorm - normally distributed random numbers
class(y)
y[71:100] <- y[71:100] + 10 # intervention effect(add 10 from 71 to 100)
y
datats <- cbind(y, x1)
datats

# Lets check
dim(datats)
head(datats)
matplot(datats, type = "l")


# To estimate a causal effect, we begin by specifying which period in the data should be 
# used for training the model (pre-intervention period) and which period for computing a 
# counterfactual prediction (post-intervention period).

pre.period <- c(1,70)
View(post.period)
length(pre.period)
post.period <- c(71,100)
class(pre.period)


impact <- CausalImpact(datats, pre.period, post.period)
library(shiny)
library(plotly)
plot(impact)

#with dates now
time.points <- seq.Date(as.Date("2014-01-01"), by = 1, length.out = 100)
datats1 <- zoo(cbind(y, x1), time.points)

# define pre.period and post.period in terms of dates..
pre.period1 <- as.Date(c("2014-01-01", "2014-03-11"))
View(pre.period1)
post.period1 <- as.Date(c("2014-03-12", "2014-04-10"))
head(data)
impact <- CausalImpact(datats1, pre.period1, post.period1)
plot(impact)


summary(impact)
summary(impact, "report")
