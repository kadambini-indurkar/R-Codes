#########################################################
######### Causal Impact On Rossmann Sales Data ##########
#########################################################

getwd()
setwd("C:/Users/kadambini.indurkar/Documents/R_Sab_Isme")

rossmann_data <- read.csv("C:/Users/kadambini.indurkar/Documents/Data/rossmann-store-sales/train.csv")
View(rossmann_data)

test_group <- subset(rossmann_data, rossmann_data$Store == 1 & rossmann_data$Promo == 1)
View(test_group)
control_group <- subset(rossmann_data, rossmann_data$Store == 2 & rossmann_data$Promo == 0)
control_group1 <- subset(rossmann_data, rossmann_data$Store == 1 & rossmann_data$Promo == 0)
summary(test_group)
summary(control_group)
summary(control_group1)


test_group <- test_group[,c(4)]
View(test_group)
typeof(test_group)
class(test_group)
control_group <- control_group[,c(4)]
control_group1 <- control_group1[,c(4)]

length(control_group1)
length(control_group)
rm(test_group_modified)
# test_group <- as.vector(test_group)
# control_group1 <- as.vector(control_group1)
# control_group <- as.vector(control_group)
View(control_group1)

test_group_modified <- control_group1
length(test_group_modified) <- 222
test_group_modified <- cbind(test_group_modified, test_group)
View(test_group_modified)
library(CausalImpact)
library(xts)
ts_y <- ts(control_group1, start = 1, frequency = 365)
ts_y <- ts_y[ts_y!=0]
ts_y
rm(ts_y)
length(ts_y) <- 222
temp <- ts(test_group, start = 1, frequency = 365)
temp
temp <- temp[temp!=0]
ts_y1 <- rbind(ts_y, temp)
ts_y
length(ts_y1)
length(ts_y1) <- 582
ts_y1
ts_x <- ts(control_group, start = 1, frequency = 365)
View(ts_y1)

pre <- c(1.00,1.605)
post <- c(1.608, 2.591)
finalTS <- cbind(ts_y1, ts_x)
cImpact <- CausalImpact(finalTS, pre, post)
finalTS
plot(cImpact)
