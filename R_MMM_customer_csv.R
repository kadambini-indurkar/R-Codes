getwd()
setwd("C:/Users/kadambini.indurkar/Documents/R_Sab_Isme/MMM")
library(ggplot2);
library(plyr);
library(dplyr);
library(Hmisc);
library(broom);
library(xts);
library(lubridate)
library(cluster)
library(zoo)
library(magrittr)
library(plotly)
library(shiny)
# library(ggplot2)
data <- read.csv('consumer_data.csv')
str(data)
getwd()
# creating a week column 
data$order_date <- date(data$order_date)

head(data)
data$week_year <- week(data$order_date)
data$week_month <- (ceiling(as.numeric(format(data$order_date,"%d"))/7))
data$date <- as.Date(data$order_date)
head(data)
summary(data)
data <- na.omit(data)
data <- subset(data, product_mrp!=0)
View(data)
# like a cleaning data thing...
data$deliverybdays[data$deliverybdays == "\\N"] <- 0
data$deliverycdays[data$deliverycdays == "\\N"] <- 0

#Outlier removal
data <- data[data$sla < 17,]
data <- data[data$product_procurement_sla < 15,]
data <- data[data$units < 4, ]


# Divide the Data into sub categories

table(data$product_analytic_sub_category)

gaming_accessory <- data[data$product_analytic_sub_category == "GamingAccessory",]
write.csv(gaming_accessory, "gaming_accessory", row.names=F)
camera_accessory <- data[data$product_analytic_sub_category == "CameraAccessory",]
write.csv(camera_accessory, "CameraAccessory", row.names=F)


home_audio <- data[data$product_analytic_sub_category == "HomeAudio",]
write.csv(home_audio, "home_audio", row.names=F)




## feature engineering


# KPI Function
KPI_FN <- function(dataset){
  ##1.Adstock Data 
  advertisement <- read.csv("advertisement.csv")
  adstock <- subset(advertisement, select=c(-Total.Investment))    
  #Dividing by 4.29 
  for( i in 3:ncol(adstock)){
    adstock[,i] = adstock[,i]/(4.29)
  }  
  merge(dataset, adstock, by=c("Year","Month"), all.x = TRUE)
  dataset[is.na(dataset)] <- 0
  ##2.Discount
  dataset$list_price = dataset$gmv/dataset$units
  #Discount = List Price - Discount
  dataset$promotional_offer = (dataset$product_mrp - dataset$list_price)/dataset$product_mrp
  ##3.Payment Model Indicator
  dataset$order_pay_id <- ifelse(dataset$s1_fact.order_payment_type   == "Prepaid",1,0)
  
  ##4.Prepaid Order %
  #Total Order Placed
  total_order <-aggregate(order_pay_id ~ Year+Month+week_year,data=dataset, FUN=NROW)
  #Total Online Order 
  online_order <- aggregate(order_pay_id~Year+Month+week_year, data= dataset, FUN=sum)
  #merge both the files
  merged <- merge(total_order,online_order,by=c("Month", "Year", "week_year"),all.x=TRUE)
  #calculating per online order  
  merged$per_order <- merged$order_pay_id.y / merged$order_pay_id.x
  #remove other variabled from 
  #merge with dataset file    
  merged <- merged[,-c(4,5)]
  #add per_order in dataset file
  dataset <- merge(dataset, merged, by=c("Month", "Year", "week_year"),all.x=TRUE)  
  dataset[is.na(dataset)] <- 0
  ##5.Brand Perception - Product Category
  dataset$product_analytic_vertical <- factor(dataset$product_analytic_vertical) 
  cluster <- aggregate(cbind(units,list_price,product_mrp)~product_analytic_vertical,dataset,mean)
  View(cluster)
  nrow(cluster)
  if(nrow(cluster) >2){
    cluster$list_price_1 <- scale(cluster$list_price)
    cluster$product_mrp_1 <- scale(cluster$product_mrp)
    cluster$units_1 <- scale(cluster$units)
    k1 <- cluster[,-c(2:4)]
    
    clust <- kmeans(k1[-1], centers=3,iter.max=50,nstart=50) 
    cluster$P_tag <- as.factor(clust$cluster)
    cluster <- cluster[,c(1,8)]        
    #   add extra column in dataset
    dataset <- merge(dataset,cluster,by=c("product_analytic_vertical"), all.x=TRUE)
    library("plyr")
    library("dplyr")
    
    k2 <- table(dataset$P_tag)
    
    levels(dataset$P_tag)[which(k2==max(table(dataset$P_tag)))] <- "Mass_p"
    levels(dataset$P_tag)[which(k2==min(table(dataset$P_tag)))] <- "Premium_p"
    levels(dataset$P_tag)[which(k2!=max(table(dataset$P_tag))& k2!=min(table(dataset$P_tag)))] <- "Middle_p"
  }
  else 
  {
    dataset$P_tag <- NA
    dataset$product_analytic_vertical <- factor(dataset$product_analytic_vertical) 
    if(tapply(dataset$product_mrp, dataset$product_analytic_vertical,mean)[[1]] > (tapply(dataset$product_mrp,dataset$product_analytic_vertical,mean)[[2]]))
    {
      dataset$P_tag[which(dataset$product_analytic_vertical == levels(dataset$product_analytic_vertical)[1])] <- "middle"
      dataset$P_tag[is.na(dataset$P_tag)] <- "mass"
    }
    else
    {
      dataset$P_tag[which(dataset$product_analytic_vertical == levels(dataset$product_analytic_vertical)[2])] <- "middle"
      dataset$P_tag[is.na(dataset$P_tag)] <- "mass"
    }
  }
  count <- table(dataset$P_tag)
  barplot(count, main= "Brand Perception",xlab="Type of Product for Market")
  
  ##6. NPS_Score
  nps <-read.csv('nps.csv',h=T)
  nps$Month <- as.character(nps$Month)
  dataset <- merge(dataset,nps,by=c("Month","Year"),all.x=TRUE)
  
  ##7. Total Investment across all Channels
  inv <- advertisement
  inv$Month <- as.character(inv$Month)
  inv$Year  <- as.character(inv$Year)
  dataset <- merge(dataset,inv,by=c("Month","Year"),all.x=TRUE)
  
  ##8. Holiday Effect 
  
  holiday_list<-c("2015-07-18","2015-07-19","2015-08-15",
                  "2015-08-16","2015-08-17","2015-08-28",
                  "2015-08-29","2015-08-30","2015-10-15",
                  "2015-10-16","2015-10-17","2015-11-07","2015-11-08","2015-11-09","2015-11-10",
                  "2015-10-11","2015-10-12","2015-11-13","2015-11-14","2015-12-25","2015-12-26",
                  "2015-12-27","2015-12-28","2015-12-29","2015-12-30","2016-01-01","2016-01-02",
                  "2016-01-03","2016-01-20","2016-01-21","2016-01-22","2016-02-01","2016-02-02",
                  "2016-02-20","2016-02-21","2016-02-14","2016-02-15","2016-03-07","2016-03-08",
                  "2016-03-09","2016-05-25","2016-05-26","2016-05-27")
  
  holiday_list <- as.Date(holiday_list)
  week_year <- week(holiday_list)
  year <- year(holiday_list)
  holiday_data <- data.frame(cbind(year,week_year))
  holiday_data$holiday_list <- holiday_list
  holiday_data$number <- 1
  holiday_data <- aggregate(number~year+week_year,holiday_data, sum)
  write.csv(holiday_data,"holiday_data.csv",row.names=F)
  
  
  dataset$paste <-paste(dataset$Year, dataset$week_year, sep='_')
  products <- as.data.frame.matrix(t(table(dataset$P_tag,dataset$week_year)))
  products$week_year <- row.names(products)
  
  holiday_data$paste <-paste(holiday_data$year, holiday_data$week_year, sep='_')
  holiday_data <- holiday_data[,-c(1:2)]
  dataset <- merge(dataset, holiday_data, by=c("paste"),all.x=TRUE)
  
  dataset$number[is.na(dataset$number)] <- 0
  dataset <- aggregate(cbind(list_price,product_mrp,gmv,units,sla,promotional_offer,number,Total.Investment,TV,Digital,Sponsorship,Content.Marketing,Online.marketing,Affiliates,SEM,product_procurement_sla,per_order,NPS)~week_year,data=dataset,FUN=mean)   
  dataset <- merge(dataset,products,by=c("week_year"),all.x=TRUE)
  
  return(dataset)
}

getwd()
home_audio_FE <- KPI_FN(home_audio)
View(home_audio_FE)
x <- read.csv("holiday_data.csv")
View(x)
gaming_accesory_FE <- KPI_FN(gaming_accessory)
View(gaming_accesory_FE)
camera_accesory_FE <- KPI_FN(camera_accessory)
View(camera_accesory_FE)
summary(camera_accesory_FE)

#############################################################################
###################### Causal Impact ########################################
#############################################################################

#home_audio_FE
#gaming_accesory_FE
#camera_accesory_FE

home_audio_FE$promoFlag <- ifelse(home_audio_FE$promotional_offer < 0.31, 0, 1)
View(home_audio_FE)
ts_gmv <- ts(home_audio_FE, start = 1, frequency = 53)
datats1 <- ts(home_audio_FE$gmv, start = 1, frequency = 53)
View(datats1)

y <- subset(ts_gmv, home_audio_FE$promoFlag==1)
x <- subset(ts_gmv, home_audio_FE$promoFlag==0)
View(y)
View(x)
y <- y[,c(4)]
y
x <- x[,c(4)]
class(x)

x <- rep(x, times = 13)
View(x)
length(x)
length(x) <- 50
x <- ts(x, start = 1, frequency = 53)
datats1 <- cbind(datats1, x)
x
datats1
library(shiny)
library(plotly)
x <- plot(home_audio_FE$week_year, home_audio_FE$gmv, type = "l")
plot_ly(home_audio_FE, x = home_audio_FE$week_year, y = home_audio_FE$gmv, type = "scatter")
# class(pre_period)
# pre_period <- subset(home_audio_FE$gmv, home_audio_FE$promoFlag==0)
# length(pre_period) <- 2
# View(pre_period)
# post_period <- subset(home_audio_FE$gmv, home_audio_FE$promoFlag==1)
# max(post_period)
# post_period[1] <- max(post_period)
# View(post_period)
# pre_period
# post_period

pre_period <- c(1.41, 1.47)
post_period <- c(1.49,1.92)
View(datats1)
View(post_period)

length(post_period) <- 2
library(CausalImpact)
View(datats1)
CI <- CausalImpact(datats1, pre_period, post_period)
plot(CI)
summary(CI, "report")
m <- CI$series
m$point.pred
datats1


###jugaad######################


# advanced KPI

advanced_kpi <- function(dataset){
  library(dplyr)
  library(zoo)
  
  myfun1 = function(x) rollmean(x, k = 2, fill = NA, align = "right")
  myfun2 = function(x) rollmean(x, k = 3, fill = NA, align = "right")
  myfun3 = function(x) rollmean(x, k = 4, fill = NA, align = "right")
  
  #dataset1<-arrange(dataset1,P_analytic_vertical,Year,week_year)
  
  x=dataset[,c("week_year","list_price","promotional_offer")]
  
  
  x1<-x %>% mutate_each(funs(myfun1),list_price,promotional_offer) %>% data.frame()
  
  x2<-x %>% mutate_each(funs(myfun2),list_price,promotional_offer) %>% data.frame()
  
  x3<-x %>% mutate_each(funs(myfun3),list_price,promotional_offer) %>% data.frame()
  
  
  x1$LP_MA1<-(x1$list_price)
  x1$PO_MA1<-(x1$promotional_offer)
  
  x2$LP_MA2<-(x2$list_price)
  x2$PO_MA2<-(x2$promotional_offer)
  
  x3$LP_MA3<-(x3$list_price)
  x3$PO_MA3<-(x3$promotional_offer)
  
  x4=cbind(x1[,-c(2:3)],x2[,-c(1:3)],x3[,-c(1:3)])
  
  
  dataset<-merge(dataset,x4,by="week_year")
  
  dataset$inc_LP_MA1<-(dataset$list_price - dataset$LP_MA1)/dataset$LP_MA1
  dataset$inc_LP_MA2<-(dataset$list_price - dataset$LP_MA2)/dataset$LP_MA2
  dataset$inc_LP_MA3<-(dataset$list_price - dataset$LP_MA3)/dataset$LP_MA3
  
  dataset$inc_PO_MA1<-(dataset$promotional_offer - dataset$PO_MA1)/dataset$PO_MA1
  dataset$inc_PO_MA2<-(dataset$promotional_offer - dataset$PO_MA2)/dataset$PO_MA2
  dataset$inc_PO_MA3<-(dataset$promotional_offer - dataset$PO_MA3)/dataset$PO_MA3
  
  #Deleting some columns
  
  dataset$LP_MA1<-NULL
  dataset$LP_MA2<-NULL
  dataset$LP_MA3<-NULL
  
  dataset$PO_MA1<-NULL
  dataset$PO_MA2<-NULL
  dataset$PO_MA3<-NULL
  print(summary(dataset))
  names(dataset)[23:28]<-c("inc_LP_MA1","inc_LP_MA2","inc_LP_MA3","inc_PO_MA1","inc_PO_MA2",
                           "inc_PO_MA3")
  print(summary(dataset))  
  #------1) Lag of List price by 1 week,2 week, 3 week
  #------2) Lag of discount(promo_off) by 1 week,2 week, 3 week
  #------3) Incremental Lag of List price & promotions/discounts by 1 week,2 week, 3 week
  
  #-----------------Lag the data after aggregating by week----#
  
  #8. Lag List price (different period lags)
  library(DataCombine)
  
  data_dum <- slide(dataset,Var="list_price",slideBy=-1)
  data_dum <- slide(data_dum,Var="list_price",slideBy=-2)
  data_dum <- slide(data_dum,Var="list_price",slideBy=-3)
  
  data_dum <- slide(data_dum,Var="promotional_offer",slideBy=-1)
  data_dum <- slide(data_dum,Var="promotional_offer",slideBy=-2)
  data_dum <- slide(data_dum,Var="promotional_offer",slideBy=-3)
  
  data_dum <- slide(data_dum,Var="NPS",slideBy=-1)
  data_dum <- slide(data_dum,Var="NPS",slideBy=-2)
  data_dum <- slide(data_dum,Var="NPS",slideBy=-3)
  
  dataset <- na.omit(data_dum)
  
  return (dataset)
  
  
  
  
  
}

library(DataCombine)
# call  above function
home_audio_final <- advanced_kpi(home_audio_FE)
gaming_accesory_final <- advanced_kpi(gaming_accesory_FE)
camera_accesory_final <- advanced_kpi(camera_accesory_FE)
View(home_audio_final)
summary(camera_accesory_final)
summary(home_audio_final)
summary(gaming_accesory_final)

eda <- function(dataset,name){
  #AdStock
  plot1 <- ggplot(dataset,aes(TV,gmv))+geom_point()+geom_smooth(aes=(method="lm"))+ggtitle(name)+labs(x="Adstock TV ",y="GMV")
  plot1
  
  plot2 <-ggplot(dataset,aes(Affiliates,gmv))+geom_point()+geom_smooth(aes=(method="lm"))+ggtitle(name)+labs(x="Affiliates ",y="GMV")
  plot2
  
  plot3 <-ggplot(dataset,aes(Content.Marketing,gmv))+geom_point()+geom_smooth(aes=(method="lm"))+ggtitle(name)+labs(x="Content Marketing",y="GMV")
  plot3
  
  plot4 <-ggplot(dataset,aes(Online.marketing,gmv))+geom_point()+geom_smooth(aes=(method="lm"))+ggtitle(name)+labs(x="Online Marketing",y="GMV")
  plot4
  
  plot5 <-ggplot(dataset,aes(SEM,gmv))+geom_point()+geom_smooth(aes=(method="lm"))+ggtitle(name)+labs(x="SEM Adstock ",y="GMV")
  plot5
  
  plot6 <-ggplot(dataset,aes(Digital,gmv))+geom_point()+geom_smooth(aes=(method="lm"))+ggtitle(name)+labs(x="Digital Adstock ",y="GMV")
  plot6
  
  return(list(plot1,plot2,plot3,plot4,plot5,plot6))
  
  
}

plots_home_audio <- eda(home_audio_final,'home_audio')
plots_home_audio



plots_gaming_accesory <- eda(gaming_accesory_final,'home_audio')
plots_gaming_accesory


plots_camera_accesory <- eda(camera_accesory_final,'camera_accesory')
plots_camera_accesory


write.csv(camera_accesory_final,'camera_accesory_final',row.names=F)
write.csv(home_audio_final,'home_audio_final',row.names=F)
write.csv(gaming_accesory_final,'gaming_accesory_final',row.names=F)
###

## Home audio

data<-read.csv("home_audio_final")
# Building the Baisc Linear regression Model
Linear_model <-data
Linear_model <- scale(Linear_model)
Linear_model <-data.frame(Linear_model)
model_1 <- lm(gmv~.,Linear_model)
print(summary(model_1))


#
library(car)
library(MASS)

all_vifs <- vif(model_1)

signif_all <- names(all_vifs)
View(signif_all)
while(any(all_vifs > 2)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))    # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("gmv ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=Linear_model)  # re-build model with new formula
  all_vifs <- vif(selectedMod)
}

##Final Model 
Linear_Final_model <- selectedMod
View(Linear_Final_model)
print(summary(Linear_Final_model))

t1 = selectedMod.predict()
t1


## K fold cross validation
library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
cross_val <- train(myForm, data = Linear_model, method = "lm",
                   trControl = train.control)
# Summarize the results
print(cross_val)


# Elasticity Analysis
train <- Linear_model
hrlm<-Linear_Final_model
# estimating the elasticity coefficients
elasticity <- function(var){
  
  elax1 <-as.numeric(hrlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 
var_list <- list()

for(i in 2:length(hrlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(hrlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(hrlm$coefficients[2:length(hrlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
print(elasticity.outputs)


print(ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
        geom_bar(position="dodge",stat="identity") + 
        coord_flip() +
        ggtitle("Home Audio - Linear Model") +xlab("Variables"))

#Multiplicative model

multi <-data
multi <- multi[,-c(20:28)]
multi$number[which(multi$number==0)] <- 0.01
multi$Content.Marketing[which(multi$Content.Marketing==0)] <- 0.01
multi$per_order[which(multi$per_order==0)] <- 0.01
multi$TV[which(multi$TV==0)] <- 0.01
multi <- log(multi)
## First model ##
multi_model <- lm(gmv~.,multi)
print(summary(multi_model))


library(car)
library(MASS)

all_vifs <- vif(multi_model)

signif_all <- names(all_vifs)

while(any(all_vifs > 1.1)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))    # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("gmv ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=multi)  # re-build model with new formula
  all_vifs <- vif(selectedMod)
}

Multi_Final_model <- selectedMod
print(summary(Multi_Final_model))
print(summary(Multi_Final_model))


library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
cross_val1 <- train(myForm, data = multi, method = "lm",
                    trControl = train.control)
# Summarize t1he results
print(cross_val1)


# Elasticity Analysis
train1 <- multi
hrlm1<-Multi_Final_model
# estimating the elasticity coefficients
elasticity1 <- function(var){
  
  elax2 <-as.numeric(hrlm1$coefficients[var]*mean(train1[,var])/mean(train1$gmv))
  
  return(elax2)
} 
var_list <- list()

for(i in 2:length(hrlm1$coefficients)){
  
  var_list[i-1] <-elasticity1(names(hrlm1$coefficients)[i])
  
}

elasticity.outputs1 <- data.frame(names(hrlm1$coefficients[2:length(hrlm1$coefficients)]))
elasticity.outputs1 <- cbind(elasticity.outputs1,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs1) <- c("Variable","Elasticity")

elasticity.outputs1$direction <- ifelse(elasticity.outputs1$Elasticity > 0, "Positive", "Negative")
print(elasticity.outputs1)


ggplot(data=elasticity.outputs1, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio Multiplicative Model") +xlab("Variables")


library(DataCombine)
Home_koyck<-data
#gmv Lag 1
Home_Koyck<-slide(Home_koyck,Var="gmv",slideBy =-1)
Home_Koyck<-na.omit(Home_koyck)
Home_Koyck<-scale(Home_koyck)
Home_Koyck<-data.frame(Home_koyck)

# Build Kyock model
Koy_model <- lm(gmv~.,Home_koyck)
print(summary(Koy_model))



all_vifs <- vif(Koy_model)
signif_all <- names(all_vifs)

while(any(all_vifs > 2)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))    # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("gmv ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=Home_koyck)  # re-build model with new formula
  all_vifs <- vif(selectedMod)
}

kyock_Final_model <- selectedMod
summary(kyock_Final_model)


library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
cross_val2 <- train(myForm, data = Home_koyck, method = "lm",
                    trControl = train.control)
# Summarize t1he results
print(cross_val2)



# Elasticity Analysis
train2 <- Home_koyck
hrlm2<-kyock_Final_model
# estimating the elasticity coefficients
elasticity2 <- function(var){
  
  elax2 <-as.numeric(hrlm2$coefficients[var]*mean(train2[,var])/mean(train2$gmv))
  
  return(elax2)
} 
var_list <- list()

for(i in 2:length(hrlm2$coefficients)){
  
  var_list[i-1] <-elasticity2(names(hrlm2$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(hrlm2$coefficients[2:length(hrlm2$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")



ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Koyck Model") +xlab("Variables")


Dis_Model <- data
Dis_model <- slide(Dis_Model, Var = "gmv",slideBy = -1)
Dis_model <- slide(Dis_model, Var = "gmv",slideBy = -2)
Dis_model <- slide(Dis_model, Var = "gmv",slideBy = -3)
Dis_model <- na.omit(Dis_model)
Dis_model <- scale(Dis_model)
Dis_model <- data.frame(Dis_model)

dist_model <- lm(gmv~.,Dis_model)
print(summary(dist_model))



all_vif <- vif(dist_model)
signif_all <- names(all_vif)

while(any(all_vif > 2)){
  var_with_max_vif <- names(which(all_vif == max(all_vif)))    # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("gmv ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=Dis_model)  # re-build model with new formula
  all_vif <- vif(selectedMod)
}

Distributed_Final_model <- selectedMod
print(summary(Distributed_Final_model))


library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
cross_val <- train(myForm, data =Dis_model , method = "lm",
                   trControl = train.control)
# Summarize t1he results
print(cross_val)



# Elasticity Analysis
train <- Dis_model
hrlm<-Distributed_Final_model
# estimating the elasticity coefficients
elasticity <- function(var){
  elax1 <-as.numeric(hrlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 
var_list <- list()

for(i in 2:length(hrlm$coefficients)){
  
  var_list[i-1] <- elasticity(names(hrlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(hrlm$coefficients[2:length(hrlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")



ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio- Distributed_lag Model") +xlab("Variables")



Multi_dist <-data[,-c(20:28)]
Multi_Dis_model_1 <- slide(Multi_dist, Var = "gmv",slideBy = -1)
Multi_Dis_model_1 <- slide(Multi_Dis_model_1, Var = "gmv",slideBy = -2)
Multi_Dis_model_1 <- slide(Multi_Dis_model_1, Var = "gmv",slideBy = -3)
Multi_dist <- na.omit(Multi_Dis_model_1)

Multi_dist$Content.Marketing[which(Multi_dist$Content.Marketing==0)] <-1
Multi_dist$per_order[which(Multi_dist$per_order==0)] <-0.01
Multi_dist$number[which(Multi_dist$number==0)] <-0.01
Multi_dist$TV[which(Multi_dist$TV==0)] <-0.01
Multi_dist <- log(Multi_dist)

distMulti <- lm(gmv~., Multi_dist)
print(summary(distMulti))



all_vif <- vif(distMulti )
signif_all <- names(all_vif)

while(any(all_vif > 1.2)){
  var_with_max_vif <- names(which(all_vif == max(all_vif)))    # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("gmv ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=Multi_dist)  # re-build model with new formula
  all_vif <- vif(selectedMod)
}

DistributedMulti_Final_model <- selectedMod
summary(DistributedMulti_Final_model)



library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
cross_val <- train(myForm, data =Multi_dist , method = "lm",
                   trControl = train.control)
# Summarize t1he results
print(cross_val)


# estimating the elasticity coefficients
train <- Multi_dist

grlm <-DistributedMulti_Final_model 
elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")



ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Multi. & DL model") +xlab("Variables")





##################################################################
########### Camera Accessory #####################################
##################################################################

data<-read.csv("camera_accesory_final")
summary(data)

# Building the Baisc Linear regression Model
Linear_model <-data
Linear_model <- scale(Linear_model)
Linear_model <-data.frame(Linear_model)
model_1 <- lm(gmv~.,Linear_model)
print(summary(model_1))

library(car)
library(MASS)

all_vifs <- vif(model_1)

signif_all <- names(all_vifs)

while(any(all_vifs > 2)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))    # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("gmv ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=Linear_model)  # re-build model with new formula
  all_vifs <- vif(selectedMod)
}

##Final Model 
Linear_Final_model <- selectedMod
print(summary(Linear_Final_model))

library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
cross_val <- train(myForm, data = Linear_model, method = "lm",
                   trControl = train.control)
# Summarize the results
print(cross_val)


# Elasticity Analysis
train <- Linear_model
hrlm<-Linear_Final_model
# estimating the elasticity coefficients
elasticity <- function(var){
  
  elax1 <-as.numeric(hrlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 
var_list <- list()

for(i in 2:length(hrlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(hrlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(hrlm$coefficients[2:length(hrlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
print(elasticity.outputs)


print(ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
        geom_bar(position="dodge",stat="identity") + 
        coord_flip() +
        ggtitle("Camera Accesory - Linear Model") +xlab("Variables"))


###########################################
## Multiplicative Model####################
###########################################

multi <-data
multi <- multi[,-c(20:28)]
multi$number[which(multi$number==0)] <- 0.01
multi$Content.Marketing[which(multi$Content.Marketing==0)] <- 0.01
multi$per_order[which(multi$per_order==0)] <- 0.01
multi$TV[which(multi$TV==0)] <- 0.01
multi <- log(multi)
## First model ##
multi_model <- lm(gmv~.,multi)
print(summary(multi_model))


library(car)
library(MASS)

all_vifs <- vif(multi_model)

signif_all <- names(all_vifs)

while(any(all_vifs > 1.1)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))    # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("gmv ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=multi)  # re-build model with new formula
  all_vifs <- vif(selectedMod)
}

Multi_Final_model <- selectedMod
print(summary(Multi_Final_model))

# K fold cross validation

library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
cross_val1 <- train(myForm, data = multi, method = "lm",
                    trControl = train.control)
# Summarize t1he results
print(cross_val1)


# Elasticity Analysis
train1 <- multi

hrlm1<-Multi_Final_model
# estimating the elasticity coefficients
elasticity1 <- function(var){
  
  elax2 <-as.numeric(hrlm1$coefficients[var]*mean(train1[,var])/mean(train1$gmv))
  
  return(elax2)
} 
var_list <- list()

for(i in 2:length(hrlm1$coefficients)){
  
  var_list[i-1] <-elasticity1(names(hrlm1$coefficients)[i])
  
}

elasticity.outputs1 <- data.frame(names(hrlm1$coefficients[2:length(hrlm1$coefficients)]))
elasticity.outputs1 <- cbind(elasticity.outputs1,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs1) <- c("Variable","Elasticity")

elasticity.outputs1$direction <- ifelse(elasticity.outputs1$Elasticity > 0, "Positive", "Negative")
print(elasticity.outputs1)


ggplot(data=elasticity.outputs1, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accesory- Multiplicative Model") +xlab("Variables")


###########################################
####### Koyck Model #######################
###########################################
library(DataCombine)
Home_koyck<-data
#gmv Lag 1
Home_Koyck<-slide(Home_koyck,Var="gmv",slideBy =-1)
Home_Koyck<-na.omit(Home_koyck)
Home_Koyck<-scale(Home_koyck)
Home_Koyck<-data.frame(Home_koyck)

# Build Kyock model
Koy_model <- lm(gmv~.,Home_koyck)
print(summary(Koy_model))

# K fold Cross validation

library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
cross_val2 <- train(myForm, data = Home_koyck, method = "lm",
                    trControl = train.control)
# Summarize t1he results
print(cross_val2)


# Elasticity Analysis
train2 <- Home_koyck
hrlm2<-kyock_Final_model
# estimating the elasticity coefficients
elasticity2 <- function(var){
  
  elax2 <-as.numeric(hrlm2$coefficients[var]*mean(train2[,var])/mean(train2$gmv))
  
  return(elax2)
} 

var_list <- list()

for(i in 2:length(hrlm2$coefficients)){
  
  var_list[i-1] <-elasticity2(names(hrlm2$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(hrlm2$coefficients[2:length(hrlm2$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")



ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accesory - Koyck Model") +xlab("Variables")


#####################################################
############ Distributed Lag Model###################
#####################################################
Dis_Model <- data
Dis_model <- slide(Dis_Model, Var = "gmv",slideBy = -1)
Dis_model <- slide(Dis_model, Var = "gmv",slideBy = -2)
Dis_model <- slide(Dis_model, Var = "gmv",slideBy = -3)
Dis_model <- na.omit(Dis_model)
Dis_model <- scale(Dis_model)
Dis_model <- data.frame(Dis_model)

dist_model <- lm(gmv~.,Dis_model)
print(summary(dist_model))


## Model Selection

all_vif <- vif(dist_model)
signif_all <- names(all_vif)

while(any(all_vif > 2)){
  var_with_max_vif <- names(which(all_vif == max(all_vif)))    # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("gmv ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=Dis_model)  # re-build model with new formula
  all_vif <- vif(selectedMod)
}

Distributed_Final_model <- selectedMod
print(summary(Distributed_Final_model))

## K fold cross validation
library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
cross_val <- train(myForm, data =Dis_model , method = "lm",
                   trControl = train.control)
# Summarize t1he results
print(cross_val)


# Elasticity Analysis
train <- Dis_model
hrlm<-Distributed_Final_model
# estimating the elasticity coefficients
elasticity <- function(var){
  elax1 <-as.numeric(hrlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 
var_list <- list()

for(i in 2:length(hrlm$coefficients)){
  
  var_list[i-1] <- elasticity(names(hrlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(hrlm$coefficients[2:length(hrlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")



ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accesory- Distributed_lag Model") +xlab("Variables")



###################################################
####### Multiplicative + Distributed Model#########
###################################################

Multi_dist <-data[,-c(20:28)]
Multi_Dis_model_1 <- slide(Multi_dist, Var = "gmv",slideBy = -1)
Multi_Dis_model_1 <- slide(Multi_Dis_model_1, Var = "gmv",slideBy = -2)
Multi_Dis_model_1 <- slide(Multi_Dis_model_1, Var = "gmv",slideBy = -3)
Multi_dist <- na.omit(Multi_Dis_model_1)

Multi_dist$Content.Marketing[which(Multi_dist$Content.Marketing==0)] <-1
Multi_dist$per_order[which(Multi_dist$per_order==0)] <-0.01
Multi_dist$number[which(Multi_dist$number==0)] <-0.01
Multi_dist$TV[which(Multi_dist$TV==0)] <-0.01
Multi_dist <- log(Multi_dist)

distMulti <- lm(gmv~., Multi_dist)
print(summary(distMulti))

# Model Slection 
all_vif <- vif(distMulti )
signif_all <- names(all_vif)

while(any(all_vif > 1.2)){
  var_with_max_vif <- names(which(all_vif == max(all_vif)))    # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("gmv ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=Multi_dist)  # re-build model with new formula
  all_vif <- vif(selectedMod)
}

DistributedMulti_Final_model <- selectedMod
summary(DistributedMulti_Final_model)

## K fold Cross Validation

library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
cross_val <- train(myForm, data =Multi_dist , method = "lm",
                   trControl = train.control)
# Summarize t1he results
print(cross_val)


# estimating the elasticity coefficients
train <- Multi_dist

grlm <-DistributedMulti_Final_model 
elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")



ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Multi. & DL model") +xlab("Variables")


######################################################
####### Game Accessory ###############################
######################################################

data<-read.csv("gaming_accesory_final")
summary(data)


# Building the Baisc Linear regression Model
Linear_model <-data
Linear_model <- scale(Linear_model)
Linear_model <-data.frame(Linear_model)
model_1 <- lm(gmv~.,Linear_model)
print(summary(model_1))

# Model Selection 
library(car)
library(MASS)

all_vifs <- vif(model_1)

signif_all <- names(all_vifs)

while(any(all_vifs > 2)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))    # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("gmv ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=Linear_model)  # re-build model with new formula
  all_vifs <- vif(selectedMod)
}

##Final Model 
Linear_Final_model <- selectedMod
print(summary(Linear_Final_model))

# K fold Cross Validation
library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
cross_val <- train(myForm, data = Linear_model, method = "lm",
                   trControl = train.control)
# Summarize the results
print(cross_val)



# Elasticity Analysis
train <- Linear_model
hrlm<-Linear_Final_model
# estimating the elasticity coefficients
elasticity <- function(var){
  
  elax1 <-as.numeric(hrlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 
var_list <- list()

for(i in 2:length(hrlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(hrlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(hrlm$coefficients[2:length(hrlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
print(elasticity.outputs)


print(ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
        geom_bar(position="dodge",stat="identity") + 
        coord_flip() +
        ggtitle("Gaming Accesory - Linear Model") +xlab("Variables"))


######################################
####### Multiplicative Model #########
######################################

multi <-data
multi <- multi[,-c(20:28)]
multi$number[which(multi$number==0)] <- 0.01
multi$Content.Marketing[which(multi$Content.Marketing==0)] <- 0.01
multi$per_order[which(multi$per_order==0)] <- 0.01
multi$TV[which(multi$TV==0)] <- 0.01
multi <- log(multi)
## First model ##
multi_model <- lm(gmv~.,multi)
print(summary(multi_model))

## Model Selection


library(car)
library(MASS)

all_vifs <- vif(multi_model)

signif_all <- names(all_vifs)

while(any(all_vifs > 1.1)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))    # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("gmv ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=multi)  # re-build model with new formula
  all_vifs <- vif(selectedMod)
}

Multi_Final_model <- selectedMod
print(summary(Multi_Final_model))

## K fold Cross Validation
library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
cross_val1 <- train(myForm, data = multi, method = "lm",
                    trControl = train.control)
# Summarize t1he results
print(cross_val1)


# Elasticity Analysis
train1 <- multi
hrlm1<-Multi_Final_model
# estimating the elasticity coefficients
elasticity1 <- function(var){
  
  elax2 <-as.numeric(hrlm1$coefficients[var]*mean(train1[,var])/mean(train1$gmv))
  
  return(elax2)
} 
var_list <- list()

for(i in 2:length(hrlm1$coefficients)){
  
  var_list[i-1] <-elasticity1(names(hrlm1$coefficients)[i])
  
}

elasticity.outputs1 <- data.frame(names(hrlm1$coefficients[2:length(hrlm1$coefficients)]))
elasticity.outputs1 <- cbind(elasticity.outputs1,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs1) <- c("Variable","Elasticity")

elasticity.outputs1$direction <- ifelse(elasticity.outputs1$Elasticity > 0, "Positive", "Negative")
print(elasticity.outputs1)


ggplot(data=elasticity.outputs1, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming Accesory- Multiplicative Model") +xlab("Variables")


## Koyck model

library(DataCombine)
Home_koyck<-data
#gmv Lag 1
Home_Koyck<-slide(Home_koyck,Var="gmv",slideBy =-1)
Home_Koyck<-na.omit(Home_koyck)
Home_Koyck<-scale(Home_koyck)
Home_Koyck<-data.frame(Home_koyck)

# Build Kyock model
Koy_model <- lm(gmv~.,Home_koyck)
print(summary(Koy_model))

# Model Selection

all_vifs <- vif(Koy_model)
signif_all <- names(all_vifs)

while(any(all_vifs > 2)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))    # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("gmv ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=Home_koyck)  # re-build model with new formula
  all_vifs <- vif(selectedMod)
}

kyock_Final_model <- selectedMod
summary(kyock_Final_model)

# K fold Cross Validation

library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
cross_val2 <- train(myForm, data = Home_koyck, method = "lm",
                    trControl = train.control)
# Summarize t1he results
print(cross_val2)



# Elasticity Analysis
train2 <- Home_koyck
hrlm2<-kyock_Final_model
# estimating the elasticity coefficients
elasticity2 <- function(var){
  
  elax2 <-as.numeric(hrlm2$coefficients[var]*mean(train2[,var])/mean(train2$gmv))
  
  return(elax2)
} 
var_list <- list()

for(i in 2:length(hrlm2$coefficients)){
  
  var_list[i-1] <-elasticity2(names(hrlm2$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(hrlm2$coefficients[2:length(hrlm2$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")



ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming Accesory - Koyck Model") +xlab("Variables")


## Distributed Lag Model

Dis_Model <- data
Dis_model <- slide(Dis_Model, Var = "gmv",slideBy = -1)
Dis_model <- slide(Dis_model, Var = "gmv",slideBy = -2)
Dis_model <- slide(Dis_model, Var = "gmv",slideBy = -3)
Dis_model <- na.omit(Dis_model)
Dis_model <- scale(Dis_model)
Dis_model <- data.frame(Dis_model)

dist_model <- lm(gmv~.,Dis_model)
print(summary(dist_model))

## Model Selection

all_vif <- vif(dist_model)
signif_all <- names(all_vif)

while(any(all_vif > 2)){
  var_with_max_vif <- names(which(all_vif == max(all_vif)))    # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("gmv ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=Dis_model)  # re-build model with new formula
  all_vif <- vif(selectedMod)
}

Distributed_Final_model <- selectedMod
print(summary(Distributed_Final_model))

## K fold cross validation

library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
cross_val <- train(myForm, data =Dis_model , method = "lm",
                   trControl = train.control)
# Summarize t1he results
print(cross_val)

# Elasticity Analysis
train <- Dis_model
hrlm<-Distributed_Final_model
# estimating the elasticity coefficients
elasticity <- function(var){
  elax1 <-as.numeric(hrlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  return(elax1)
} 
var_list <- list()

for(i in 2:length(hrlm$coefficients)){
  
  var_list[i-1] <- elasticity(names(hrlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(hrlm$coefficients[2:length(hrlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")



ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming Accesory- Distributed_lag Model") +xlab("Variables")

## Multiplicative + Distributed Model

Multi_dist <-data[,-c(20:28)]
Multi_Dis_model_1 <- slide(Multi_dist, Var = "gmv",slideBy = -1)
Multi_Dis_model_1 <- slide(Multi_Dis_model_1, Var = "gmv",slideBy = -2)
Multi_Dis_model_1 <- slide(Multi_Dis_model_1, Var = "gmv",slideBy = -3)
Multi_dist <- na.omit(Multi_Dis_model_1)

Multi_dist$Content.Marketing[which(Multi_dist$Content.Marketing==0)] <-1
Multi_dist$per_order[which(Multi_dist$per_order==0)] <-0.01
Multi_dist$number[which(Multi_dist$number==0)] <-0.01
Multi_dist$TV[which(Multi_dist$TV==0)] <-0.01
Multi_dist <- log(Multi_dist)

distMulti <- lm(gmv~., Multi_dist)
print(summary(distMulti))

## Model Selection

all_vif <- vif(distMulti )
signif_all <- names(all_vif)

while(any(all_vif > 1.2)){
  var_with_max_vif <- names(which(all_vif == max(all_vif)))    # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("gmv ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=Multi_dist)  # re-build model with new formula
  all_vif <- vif(selectedMod)
}

DistributedMulti_Final_model <- selectedMod
summary(DistributedMulti_Final_model)

## K fold Cross Validation

library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
cross_val <- train(myForm, data =Multi_dist , method = "lm",
                   trControl = train.control)
# Summarize t1he results
print(cross_val)

## 

# estimating the elasticity coefficients
train <- Multi_dist

grlm <-DistributedMulti_Final_model 
elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")



ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Multi. & DL model") +xlab("Variables")

