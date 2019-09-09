#MMM Final Kadambini Indurkar :)

# Data cleaning
# EDA
# KPI Analysis
# Final Model

#___________________________________________________________________________________________
getwd()

setwd("C:/Users/kadambini.indurkar/Documents/R_Sab_Isme/MMM/MMM_28_August_2019")
getwd()


# Load required packages 

requiredPackages = c('DataCombine','scales','glmnet','DAAG','caret','GGally','corrplot','lubridate','gdata','ggplot2','dplyr','reshape','tidyr','data.table','MASS','car')

#Installing the required packages
for(item in requiredPackages){
  if(!require(item,character.only = TRUE)){
    install.packages(item)
  }
  library(item, character.only = TRUE)
}

list.files()
ad_spend_details <- read.csv("advertisement.csv", header = T, check.names = FALSE)
# New Column - year_month

ad_spend_details$year_month <- paste(ad_spend_details$Year, ad_spend_details$Month, sep = '-')
class(ad_spend_details$Year)
ad_spend_details$Year <- as.numeric(ad_spend_details$Year)
ad_spend_details$Month <- as.numeric(ad_spend_details$Month)

# ordered factors of year_month

ad_spend_details$year_month <- factor(ad_spend_details$year_month, levels = ad_spend_details$year_month[order(ad_spend_details$Year, ad_spend_details$Month)])

# EDA
library(plotly)
library(shiny)
library(ggplot2)
View(ad_spend_details)

p1 <- ggplot(ad_spend_details, aes(x = year_month, y = `Total Investment`)) + geom_bar(stat = "identity")
ggplotly(p1)

# Observations
# 1. Spend is lowest for august
# 2. Sep, oct, nov, march - greater than 100 units

# check NAs
sapply(ad_spend_details, function(x) sum(is.na(x)))
# Year             Month  Total Investment                TV           Digital 
# 0                 0                 0                 0                 0 
# Sponsorship Content Marketing  Online marketing        Affiliates               SEM 
# 0                 0                 0                 0                 0 
# Radio             Other        year_month 
# 9                 9                 0 
#Radio and others 9 NAs -> Replace with 0 then
ad_spend_details[which(is.na(ad_spend_details$Radio)), "Radio"] <- 0
ad_spend_details[which(is.na(ad_spend_details$Other)), "Other"] <- 0

ad_details <- gather(ad_spend_details, Medium, Spend, 3:12)
View(ad_details)

#Data Scientist - EDA

p2 <- ggplot(ad_details, aes(x = Month, y = Spend, colour = Medium)) + geom_line() +
  scale_x_discrete(name = "Months Since May 2015", limits = seq(1,12,1))
ggplotly(p2)

ad_spend_details$`Total Investment` <- NULL
ad_spend_details[,3:11] <- ad_spend_details[,3:11] * 10000000

# Consumer Data or Orders Data or History Data
list.files()
Orders_Data <- read.csv("consumer_data.csv")
# 1648824 observations and 20 variables

#Check Sensitivity
# sapply(Orders_Data, function(x) length(unique(toupper(x)))-length(unique(tolower(x))))

# Check NAs
sapply(Orders_Data, function(x){sum(is.na(x))})  
#gmv, pincode me 4904 NAs

filter(Orders_Data , Orders_Data$gmv < 0 )  ## 0 row
filter(Orders_Data , Orders_Data$gmv == 0 ) ## 1349 rows 

filter(Orders_Data , Orders_Data$units < 0 )  ## 0 row
filter(Orders_Data , Orders_Data$units == 0 ) ## 0 row

table(Orders_Data$deliverybdays)  ## rows with negative as well as very high deliverybdays 
table(Orders_Data$deliverycdays)  ## rows with negative as well as very high deliverycdasy
unique(Orders_Data$s1_fact.order_payment_type)
## Two payment type available - COD , Prepaid 
table(Orders_Data$s1_fact.order_payment_type)  # (n(COD) > n(Prepaid))
table(Orders_Data$sla)


##
length(unique(Orders_Data$pincode)) ## 7565 unique values 
length(unique(Orders_Data$cust_id)) ## 1201090 distinct customers 


unique(Orders_Data$product_analytic_super_category) ## Single value CE 
unique(Orders_Data$product_analytic_sub_category)   ## 14 distinct values 
unique(Orders_Data$product_analytic_category)       ## 5 distinct values 
unique(Orders_Data$product_analytic_vertical)       ## 74 distinct values 

filter ( Orders_Data ,Orders_Data$product_analytic_sub_category %in% c("CameraAccessory", "GamingAccessory", "HomeAudio")) %>% group_by(product_analytic_sub_category , product_analytic_vertical)%>% summarise(  count = n()) 
# count = 51


filter(Orders_Data , Orders_Data$product_mrp < 0 )  ## 0 row
nrow(filter(Orders_Data , Orders_Data$product_mrp == 0 )) ## 5308 row


min(as.Date(Orders_Data$order_date))   ## "2015-05-19 13:42:09"
max(as.Date(Orders_Data$order_date))  ## "2016-07-25 01:19:45"


## Create a year-month variable 
Orders_Data$year_month <- paste(Orders_Data$Year, Orders_Data$Month , sep = '-')
Orders_Data$order_date <- as.Date(Orders_Data$order_date)
Orders_Data$start_week_date <- floor_date(as.Date(Orders_Data$order_date), unit="week" , week_start = getOption("lubridate.week.start", 1))
Orders_Data[which(Orders_Data$start_week_date < '2015-07-01'),"start_week_date"] <- '2015-07-01'


##  the week number from a date
Orders_Data$week_no <-   strftime( Orders_Data$start_week_date ,format="%V")
## Creating a year-month variable 
Orders_Data$year_month <- paste(Orders_Data$Year, Orders_Data$Month , sep = '-')
## Filter for date range July 2015 to June 2016
Orders_Data <- subset(Orders_Data, Orders_Data$order_date >= '2015-07-01' & Orders_Data$order_date < '2016-07-01')
## Filter out the rows  having missing values 
row.has.na <- apply(Orders_Data, 1, function(x){any(is.na(x))})
sum(row.has.na) #4904
## Remove the missing values 
Orders_Data <- Orders_Data[!row.has.na,] #1643311
Orders_Data$week_no <- as.numeric(Orders_Data$week_no)
## Add a varaiable to specify month number as per week start date 
Orders_Data$month_asper_week_startdate <- format(Orders_Data$start_week_date , "%m")

Orders_Data$month_asper_week_startdate <- as.numeric(Orders_Data$month_asper_week_startdate)
## Number the week from 1 to 53
## July 1st week will be 1 and june last week will be 53 
View(Orders_Data) 
#<- isse pata chala ki first min data matlab first data par week no is 27..so manipulation

Orders_Data$week_no[Orders_Data$Year == 2015 ] <- (Orders_Data$week_no[Orders_Data$Year == 2015 ]) - 26
Orders_Data$week_no[Orders_Data$Year == 2016 & Orders_Data$week_no !=53 ] <- (Orders_Data$week_no[Orders_Data$Year == 2016 & Orders_Data$week_no !=53 ]) +27
Orders_Data[which(Orders_Data$Year == 2016 & Orders_Data$Month==1 &Orders_Data$week_no == 53 ), "week_no"] <- Orders_Data[which(Orders_Data$Year == 2016 & Orders_Data$Month==1 &Orders_Data$week_no == 53 ), "week_no"] - 26


##
## Filter out the rows having mrp value 0 
Orders_Data <- Orders_Data[!Orders_Data$product_mrp == 0,]

##Orders_Data$deliverybdays <- gsub('\N' , '0' , Orders_Data$deliverybdays )
##Orders_Data$deliverycdays <- gsub('\\N' , '0' , Orders_Data$deliverycdays )

Orders_Data$deliverybdays[Orders_Data$deliverybdays < 0] = 0
Orders_Data$deliverycdays[Orders_Data$deliverycdays < 0] = 0
Orders_Data$product_procurement_sla [Orders_Data$product_procurement_sla <0 ] =0

Orders_Data$deliverybdays <- as.numeric(Orders_Data$deliverybdays)
Orders_Data$deliverycdays <- as.numeric(Orders_Data$deliverycdays)
Orders_Data$sla <- as.numeric(Orders_Data$sla)
Orders_Data$delivery_on_time <- Orders_Data $sla - (Orders_Data$deliverybdays+Orders_Data$deliverycdays+Orders_Data$product_procurement_sla)
Orders_Data$delivery_status[Orders_Data$delivery_on_time < 0] <- 'Delayed'
Orders_Data$delivery_status[Orders_Data$delivery_on_time == 0] <- 'On time'
Orders_Data$delivery_status[Orders_Data$delivery_on_time > 0] <- 'Early'

# Special sale wala data hi nahi hai :(
date <- as.Date(c("2015-07-18","2015-07-19","2015-08-15","2015-08-16","2015-08-17","2015-08-28","2015-08-29","2015-08-30","2015-10-15","2015-10-16","2015-10-17",
                  "2015-11-07","2015-11-08","2015-11-09","2015-11-10","2015-11-11","2015-11-12","2015-11-13","2015-11-14","2015-12-25","2015-12-26","2015-12-27",
                  "2015-12-28","2015-12-29","2015-12-30","2015-12-31","2016-01-01","2016-01-02","2016-01-03","2016-01-20","2016-01-21","2016-01-22","2016-02-01",
                  "2016-02-02","2016-02-14","2016-02-15","2016-02-20","2016-02-21","2016-03-07","2016-03-08","2016-03-09","2016-05-25","2016-05-26","2016-05-27"))

Orders_Data$is_special_sale_day <- ifelse(Orders_Data$order_date %in% date, "Y", "N")

# length(unique(Orders_Data$is_special_sale_day))
Orders_Data$special_sale_day='Regular Day'
Orders_Data <- within(Orders_Data, {
  special_sale_day[order_date  %in% (date[1:2])]='Eid & Rathayatra'
  special_sale_day[order_date  %in% (date[3:5])]='Independence Day'
  special_sale_day[order_date  %in% (date[6:8])]='Rakshabandhan'
  special_sale_day[order_date  %in% (date[9:11])]='Daussera'
  special_sale_day[order_date  %in% (date[12:19])]='Diwali'
  special_sale_day[order_date  %in% (date[20:29])]='Christmas & New Year'
  special_sale_day[order_date  %in% (date[30:32])]='Republic Day'
  special_sale_day[order_date  %in% (date[33:34])]='BED'
  special_sale_day[order_date  %in% (date[35:36])]='Valentine Day'
  special_sale_day[order_date  %in% (date[37:38])]='FHSD'
  special_sale_day[order_date  %in% (date[39:41])]='BSD'
  special_sale_day[order_date  %in% (date[42:44])]='Pacman'
})

# new data frame for holidays
holidays <- date   #Coverting the date vector into Date format
week <- strftime(holidays, format = "%V")   #Extracting the weeks out of date
Year <- format(as.POSIXct(holidays, format="%Y-%m-%d"),"%Y")  #Extracting the Year out of date

holiday_details <- data.frame(cbind(Year,week))   #Creating a dataframe to hold holiday details
holiday_details$holidays <- holidays
holiday_details$holiday_count <- 1
holiday_details <- aggregate(holiday_count~Year+week, holiday_details, sum)   #Aggregating the holidays couns based on week
View(holiday_details)
write.csv(holiday_details, file = "Holiday_File_2015_2016.csv")
list.files()
## NPS data

nps_data <- read.csv("nps.csv", header =  T)
# rm(nps_data)
# str(nps_data)
View(nps_data)
# t_nps_data <- transpose(nps_data)
# rm(t_nps_data)
# View(t_nps_data)

weekly_order_data <- Orders_Data %>% group_by ( Year, month_asper_week_startdate,  product_analytic_category,product_analytic_sub_category, product_analytic_vertical,year_month , week_no)%>% summarise( prepaid_cnt =  sum(ifelse (s1_fact.order_payment_type =='Prepaid' , 1 , 0)) ,cod_cnt =  sum(ifelse (s1_fact.order_payment_type =='COD' , 1,0)) ,delayed_delivery_cnt =sum(ifelse (delivery_status =='Delayed' , 1 , 0)), early_delivery_cnt =sum(ifelse (delivery_status =='Early' , 1 , 0)), onetime_delivery_cnt =sum(ifelse (delivery_status =='On time' , 1 , 0)), tot_gmv = sum(gmv) , tot_units = sum(units) , tot_product_mrp = sum( as.numeric (product_mrp)), avg_gmv = mean(gmv) , avg_mrp = mean(product_mrp) , no_of_customer = length(unique(cust_id)), no_of_orders = length(unique(order_id)) , list_price = (tot_gmv/tot_units) , avg_price = mean(list_price) )

colnames(weekly_order_data)[2] <- "Month"
list.files()
## Merge the ad data with weekly data 
weekly_order_ad_data <- merge(weekly_order_data ,ad_spend_details , by=c("Year" , "Month"))
weekly_order_ad_data <- merge(weekly_order_ad_data ,nps_data , by=c(  "Month"))
week_in_a_month <- weekly_order_ad_data %>% group_by( Month ) %>% summarize (  tot_week = length(unique(week_no)) )
weekly_order_ad_data <- merge(weekly_order_ad_data ,week_in_a_month, by = c ( "Month") )
rows_ina_week <- weekly_order_ad_data %>% group_by( week_no ) %>% summarize ( total_row = n())
View(rows_ina_week)

weekly_order_ad_data <- merge(weekly_order_ad_data ,rows_ina_week, by = c ( "week_no") )
## Convert monthly ad spend into weekly ad spend 
weekly_order_ad_data[,c(22:30)] <- weekly_order_ad_data[,c(22:30)]/(weekly_order_ad_data$tot_week*weekly_order_ad_data$total_row)
View(weekly_order_ad_data)
# New variables 
weekly_order_ad_data$discount_over_mrp <- (weekly_order_ad_data$tot_product_mrp-weekly_order_ad_data$tot_gmv)/weekly_order_ad_data$tot_product_mrp
weekly_order_ad_data$Holiday_week <- ifelse(weekly_order_ad_data$week_no == holiday_details$week, "Y", "N")
weekly_order_ad_data$value_per_visitor <- weekly_order_ad_data$tot_gmv/weekly_order_ad_data$no_of_customer


## EDA
## Create week wise sale and ad spend details for various sub category level 
ad_sale_detaitls <- weekly_order_ad_data %>% group_by (product_analytic_sub_category, week_no)%>% 
  summarise(tot_sales = sum(tot_gmv) ,
            tot_tv_spend = sum (TV), tot_dig_spend = sum (Digital), 
            tot_spon_spend = sum(Sponsorship) , tot_content_spend = sum(`Content Marketing`),
            tot_online_spend = sum(`Online marketing`) ,tot_aff_spend = sum(Affiliates),
            tot_sem_spend = sum(SEM) ,tot_radio_spend = sum(Radio), 
            tot_oter_spend = sum(Other))


## weekly sale details for different sub category 
p <- ggplot(ad_sale_detaitls , aes ( x = week_no , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_detaitls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "week", y = "Sales " ) + ggtitle ( " Sales  vs Total Ad spend")

## Total TV ad spend vs sales details
p <- ggplot(ad_sale_detaitls , aes ( x = tot_tv_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_detaitls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs TV Ad")

p <- ggplot(ad_sale_detaitls , aes ( x = tot_dig_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_detaitls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Digital Ad")

p <- ggplot(ad_sale_detaitls , aes ( x = tot_spon_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_detaitls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Sponsor Ad")

p <- ggplot(ad_sale_detaitls , aes ( x = tot_content_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_detaitls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Content Ad")

p <- ggplot(ad_sale_detaitls , aes ( x = tot_online_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_detaitls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Online Ad")

p <- ggplot(ad_sale_detaitls , aes ( x = tot_aff_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_detaitls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Affiliate Ad")

p <- ggplot(ad_sale_detaitls , aes ( x = tot_sem_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_detaitls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs SEM Ad")

p <- ggplot(ad_sale_detaitls , aes ( x = tot_radio_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_detaitls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Radio Ad")

p <- ggplot(ad_sale_detaitls , aes ( x = tot_oter_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_detaitls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Other Ad")

## Sale at different promotional and non promotional weeks
#weekly_order_ad_data %>% group_by ( promotion_type) %>% summarise( Avg_sale = mean(tot_gmv)) %>% ggplot( aes ( x=promotion_type, y =Avg_sale  )) + geom_bar(stat = "identity")


## weekly ad spend vs sales 
sale_vs_week_ad <- weekly_order_ad_data %>% group_by ( week_no) %>% summarise(tot_sales = sum(tot_gmv) , ad_spend = sum(TV+Digital+Sponsorship+`Content Marketing`+`Online marketing`+Affiliates+SEM+Radio+Other))
sale_vs_week_ad_long <- gather(sale_vs_week_ad, Type, Spend, 2:3)
p3 <- ggplot(sale_vs_week_ad_long, aes ( x= week_no , y = Spend , color = Type))+geom_line()
ggplotly(p3)


### Disocunt percentage vs average sales

discount_vs_sales <- weekly_order_ad_data[,c("tot_gmv" , "discount_over_mrp")]
discount_vs_sales$discount_range <- ifelse (discount_vs_sales$discount_over_mrp <= .1 , 'up to 10', ifelse ( discount_vs_sales$discount_over_mrp > .1 & discount_vs_sales$discount_over_mrp <= .3 , 'up to 30', ifelse(discount_vs_sales$discount_over_mrp > .3 & discount_vs_sales$discount_over_mrp <= .5 , 'up to 50','>50') ))
discount_vs_sales %>% group_by(discount_range) %>% summarise( avg_sale = mean(tot_gmv)) %>% ggplot(aes ( x=discount_range , y =avg_sale  )) + geom_bar(stat = "identity")

## Avg discount at different promotional and non promotional week
weekly_order_ad_data %>% group_by(promotion_type) %>% summarise(avg_disc = mean(discount_over_mrp)) %>% ggplot(aes(x= promotion_type, y =avg_disc )) + geom_bar(stat = "identity")

## Nps vs week
weekly_order_ad_data %>% group_by(week_no) %>% summarise(nps = mean(NPS)) %>% ggplot(aes(x= week_no, y =nps )) + geom_bar(stat = "identity")

## payment type vs number of orders
Orders_Data %>% group_by ( s1_fact.order_payment_type) %>% summarise(order_cnt = n()) %>% ggplot(aes(x= s1_fact.order_payment_type, y =order_cnt )) + geom_bar(stat = "identity")

## delivery_status  vs number of orders
Orders_Data %>% group_by ( delivery_status) %>% summarise(order_cnt = n()) %>% ggplot(aes(x= delivery_status, y =order_cnt )) + geom_bar(stat = "identity")


# 3 Different Datasets
unique(weekly_order_ad_data$product_analytic_category)
## Create a dataset only for Home audio , camera accessory and gaming accessories 
weekly_order_ad_data$product_analytic_category <- NULL 
weekly_order_ad_data$year_month.x <- NULL 

## Create a dataset only for Home audio , camera accessory and gaming accessories 
weekly_order_ad_data <- filter ( weekly_order_ad_data ,weekly_order_ad_data$product_analytic_sub_category %in% c("CameraAccessory", "GamingAccessory", "HomeAudio")) 
View(weekly_order_ad_data)

#############################################################################
###################### Causal Impact ########################################
#############################################################################

summary(weekly_order_ad_data)
weekly_order_ad_data_home_audio <- subset(weekly_order_ad_data, weekly_order_ad_data$product_analytic_sub_category == "HomeAudio")
weekly_order_ad_data$promoFlag <- ifelse(weekly_order_ad_data$discount_over_mrp < 0.27, 0, 1)
#weekly_order_ad_data$promoFlag <- ifelse(weekly_order_ad_data$discount_over_mrp < 0.27, 0, 1)

y <- subset(weekly_order_ad_data_home_audio$tot_gmv, weekly_order_ad_data$promoFlag==1)
x <- subset(weekly_order_ad_data_home_audio$tot_gmv, weekly_order_ad_data$promoFlag==0)
yy <- subset(weekly_order_ad_data_home_audio$tot_gmv, weekly_order_ad_data_home_audio$promoFlag==1)
xx <- subset(weekly_order_ad_data_home_audio$tot_gmv, weekly_order_ad_data_home_audio$promoFlag==0)
View(xx)
View(yy)
rm(y1)
rm(y)
rm(x)
y1 <- weekly_order_ad_data_home_audio$tot_gmv
rm(y1)
View(y1)
View(yy)
xx <- rep(xx, times = 2)

length(xx) <- 299
xx <- ts(xx, start = 1, frequency = 365)
yy <- ts(yy, start = 1, frequency = 365)
rm(ts_y)
ts_y <- cbind(xx,yy)
View(ts_y)
ts_y1
prePeriod <- c(1.00,1.43)
PostPeriod <- c(1.44,1.81)
library(CausalImpact)
library(xts)
ts_y
Causal_Impact <- CausalImpact(ts_y, prePeriod, PostPeriod)

plot(Causal_Impact)
summary(Causal_Impact, "report")

#??

## Dummy variable creation for character data types 
weekly_order_ad_data_chr <- weekly_order_ad_data[,c(5,32,34)]
weekly_order_ad_data_fact <- data.frame(sapply(weekly_order_ad_data_chr, function(x) factor(x)))
str(weekly_order_ad_data_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(weekly_order_ad_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data = weekly_order_ad_data_fact))[,-1]))

## Create master data set by appending dummies with main data set 
weekly_order_ad_data_overall <- cbind(weekly_order_ad_data[,c(1:4,6:31,33,35)],dummies) 
View(weekly_order_ad_data_overall)

### Outlier treatment 


p5 <- plot_ly(weekly_order_ad_data_overall, y = weekly_order_ad_data_overall$tot_gmv, type = "box")
p5
p6 <- plot_ly(weekly_order_ad_data_overall, y = weekly_order_ad_data_overall$tot_units, type = "box")
p6
p7 <- plot_ly(weekly_order_ad_data_overall, y = weekly_order_ad_data_overall$tot_product_mrp, type = "box")
p7
p8 <- plot_ly(weekly_order_ad_data_overall, y = weekly_order_ad_data_overall$TV, type = "box")
p8
p8 <- plot_ly(weekly_order_ad_data_overall, y = weekly_order_ad_data_overall$tot_product_mrp, type = "box")
p8
plot_ly(weekly_order_ad_data_overall, y = weekly_order_ad_data_overall$Sponsorship, type = "box")
plot_ly(weekly_order_ad_data_overall, y = weekly_order_ad_data_overall$`Content Marketing`, type = "box")

plot_ly(weekly_order_ad_data_overall, y = weekly_order_ad_data_overall$`Online Marketing`, type = "box")
plot_ly(weekly_order_ad_data_overall, y = weekly_order_ad_data_overall$Affiliates, type = "box")
plot_ly(weekly_order_ad_data_overall, y = weekly_order_ad_data_overall$SEM, type = "box")
plot_ly(weekly_order_ad_data_overall, y = weekly_order_ad_data_overall$Radio, type = "box")
plot_ly(weekly_order_ad_data_overall, y = weekly_order_ad_data_overall$Other, type = "box")


overall_quantile <- sapply(weekly_order_ad_data_overall[,c("tot_gmv","tot_units", "tot_product_mrp" , "TV" ,"Digital",
                                                           "Sponsorship", "Content Marketing", "Online marketing" ,"Affiliates", "SEM" ,"Radio" , "Other" )], 
                           function(x) quantile(x,seq(0,1,.01),na.rm = T)) 

## remove_outliers function for capping the value to specific quantile

remove_outliers <- function(x , lower_quantile, upper_quantile) {
  qnt <- quantile(x, probs=c(lower_quantile, upper_quantile), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  y <- x
  y[x < qnt[1]] <- qnt[1]
  y[x > qnt[2]] <- qnt[2]
  y
}


weekly_order_ad_data_overall$tot_gmv <- remove_outliers (weekly_order_ad_data_overall$tot_gmv,0, .97 ) 
weekly_order_ad_data_overall$tot_units <- remove_outliers (weekly_order_ad_data_overall$tot_units,0, .97 ) 
weekly_order_ad_data_overall$tot_product_mrp <- remove_outliers (weekly_order_ad_data_overall$tot_product_mrp,0, .97 ) 
weekly_order_ad_data_overall$TV <- remove_outliers (weekly_order_ad_data_overall$TV,0, .98 ) 
weekly_order_ad_data_overall$Digital <- remove_outliers (weekly_order_ad_data_overall$Digital,0, .95 ) 
weekly_order_ad_data_overall$Sponsorship <- remove_outliers (weekly_order_ad_data_overall$Sponsorship,0, .95 ) 
weekly_order_ad_data_overall$`Content Marketing` <- remove_outliers (weekly_order_ad_data_overall$`Content Marketing`,0, .95 ) 
weekly_order_ad_data_overall$SEM <- remove_outliers (weekly_order_ad_data_overall$SEM,0, .95 ) 
weekly_order_ad_data_overall$Radio <- remove_outliers (weekly_order_ad_data_overall$Radio,0, .95 ) 
weekly_order_ad_data_overall$Other <- remove_outliers (weekly_order_ad_data_overall$Other,0, .95 )


## Find out  how many distinct values are there are for different columns
sapply(weekly_order_ad_data_overall, function(x) length(unique(x)))
weekly_order_ad_data_overall$total_row <- NULL
weekly_order_ad_data_overall$tot_week <- NULL


##  Take back up of master dataset weekly_order_ad_data_overall

weekly_order_ad_data_overall2 <- weekly_order_ad_data_overall

## Check the correlation among multiple varaiables to decide which varaiables are highly corelated with each other
## Column 4 has been excluded as it contains sub category 

corr <- cor(weekly_order_ad_data_overall2[, -c(4)])

##Depending on higher corelation or since there vars are direct proxy to sales , so taking them out
weekly_order_ad_data_overall$avg_mrp <- NULL
weekly_order_ad_data_overall$avg_price <- NULL
weekly_order_ad_data_overall$tot_units <- NULL
weekly_order_ad_data_overall$no_of_orders <- NULL
weekly_order_ad_data_overall$tot_product_mrp <- NULL
weekly_order_ad_data_overall$avg_gmv <- NULL
weekly_order_ad_data_overall$value_per_visitor <- NULL
weekly_order_ad_data_overall$Year <- NULL
weekly_order_ad_data_overall$no_of_customer <- NULL
weekly_order_ad_data_overall$delayed_delivery_cnt <- NULL
weekly_order_ad_data_overall$early_delivery_cnt <- NULL
weekly_order_ad_data_overall$onetime_delivery_cnt <- NULL
weekly_order_ad_data_overall$cod_cnt <- NULL
weekly_order_ad_data_overall$prepaid_cnt <- NULL

# gaming_accessory <- data[data$product_analytic_sub_category == "GamingAccessory",]
# write.csv(gaming_accessory, "gaming_accessory", row.names=F)
## Create 3 data set HomeAudio, GamingAccessory and CameraAccessory  for model building. 
#list2env(split( weekly_order_ad_data_overall[,-3], weekly_order_ad_data_overall$product_analytic_sub_category), envir = .GlobalEnv)
gaming_accessory <- weekly_order_ad_data_overall[weekly_order_ad_data_overall$product_analytic_sub_category == "GamingAccessory"]
write.csv(gaming_accessory, "gaming_accessory", row.names = F)
dim(gaming_accessory)
View(gaming_accessory)
####################
unique(weekly_order_ad_data_overall$product_analytic_sub_category)
camera_accessory <- weekly_order_ad_data_overall[weekly_order_ad_data_overall$product_analytic_sub_category == "CameraAccessory"]
write.csv(camera_accessory, "camera_accessory", row.names = F)

home_audio <- weekly_order_ad_data_overall[weekly_order_ad_data_overall$product_analytic_sub_category == "HomeAudio"]
write.csv(home_audio, "HomeAudio", row.names = F)
str(HomeAudio)
str(GamingAccessory)
str(CameraAccessory)
nrow(HomeAudio)
nrow(GamingAccessory)
nrow(CameraAccessory)
dim(CameraAccessory)
View(weekly_order_ad_data_overall)

game_accessory <- read.csv("gaming_accessory")
game_accessory <- scale(game_accessory)
game_accessory <- data.frame(game_accessory)

linearModel11 <- lm(tot_gmv~., game_accessory)
summary(linearModel1)
vif(linearModel1)

# linear_camera <- CameraAccessory
# linear_camera$week_no <- NULL
# linear_camera$Month <- NULL

# #Scaling the dataset
# class(linear_camera)
# linear_camera[,1:13] <- data.frame(scale(linear_camera[,1:13], center = TRUE))
# 
# set.seed(100)
# trainindices= sample(1:nrow(linear_camera), 0.7*nrow(linear_camera))
# #Generate the train data set
# train = linear_camera[trainindices,]
# #Similarly store the rest of the observations into an object "test".
# test = linear_camera[-trainindices,]
# 
# cam_model1 <- lm(tot_gmv~. , data = train)
# summary(cam_model1)
# alias(cam_model1)
# 
# # #Scaling the dataset
# # linear_camera[,1:13] <- data.frame(scale(linear_camera[,1:13], center = TRUE))
# # 
# # set.seed(100)
# # trainindices= sample(1:nrow(linear_camera), 0.7*nrow(linear_camera))
# # #Generate the train data set
# # train = linear_camera[trainindices,]
# # #Similarly store the rest of the observations into an object "test".
# # test = linear_camera[-trainindices,]
# # View()
# # linearModel <- train
# # sum(is.na(linearModel))
# # linearModel <- scale(linearModel)
# # cam_model1 <- lm(tot_gmv~. , linearModel)
# summary(cam_model1)
# alias(cam_model1)
# 
# step_cam_model1 <- stepAIC(cam_model1, direction = "both")
# summary(step_cam_model1)
# vif(step_cam_model1)
# alias(step_cam_model1)
