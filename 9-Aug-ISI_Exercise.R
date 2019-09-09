getwd()
# Outlier detection

library(plotly)
library(shiny)
library(ggplot2)
library(data.table)
setwd("C:/Users/kadambini.indurkar/Documents/R_Sab_Isme")
getwd()
list.files()
# condition specific data fetching
datafile <- read.csv("Segmentation_Analysis.csv")
View(datafile)
grpA <- datafile[datafile$Vol_Group %in% c("A"),]
grpB <- datafile[datafile$Vol_Group %in% c("B"),]
Grp <- rbind(grpA, grpB)
View(grpA)
View(grpB)

plot(dcast(grpA, SKU ~ CoV))
# CoV
plot(dcast(grpA, SKU ~ Vol_Group, value.var = "CoV"))
#ggplot(grpA, aes(x = SKU, y = CoV)) + geom_point() + facet_grid( formula1 ~ formula2)
plot(dcast(grpA, SKU ~ CoV))
plot(dcast(grpB, SKU ~ Vol_Group, value.var = "CoV"))
boxplot(grpB$CoV, grpB$SKU)  
# f <- dcast(grpB, SKU ~ Vol_Group, value.var = "CoV")

# 
plot(dcast(grpA, SKU ~ Vol_Group, value.var = "CoV"))


# Group A- CoV
ui1 <- fluidPage(
  plotlyOutput("distPlot")
)
server1 <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(grpA, aes(x = SKU, y = CoV)) + geom_point()
    #plot(dcast(grpA, SKU ~ Vol_Group, value.var = "CoV"))

  })
}

ui2 <- fluidPage(
  plotlyOutput("distPlot")
)
server2 <- function(input, output) {
  output$distPlot <- renderPlotly({
    gplot(grpB, aes(x = SKU, y = CoV)) + geom_point()
    #plot(f)
    #plot(dcast(grpA, SKU ~ Vol_Group, value.var = "CoV"))
    
  })
}
shinyApp(ui = ui2, server = server2)



# easiest way.. interactive plots..
x <- ggplot(grpB, aes(x = SKU, y = CoV)) + geom_point()
x
ggplotly(x)

#function here
source("https://raw.githubusercontent.com/talgalili/R-code-snippets/master/boxplot.with.outlier.label.r")
#label = as.data.frame(grpB$SKU, grpB$CoV)


class(label)
label
p <- plot_ly(grpB, x = ~grpB$CoV, type = "box")
p
# 
# c1 = plotly_POST(p, filename="geom_boxplot/colored")
# c1
# boxplot(grpB$CoV, data = grpB, id = list(grpB$SKU))
# boxplot(grpB$CoV)#, data = grpB$SKU)
# boxplot(grpB$CoV)# labels=rownames(grpB$SKU))
library(tidyverse)
library(ggstatsplot)
# boxplot(CoV ~ Vol_Group, data = Grp)
# ggbetweenstats(data = starwars_nj_mf, 
#                x = gender,
#                y = height,
#                outlier.tagging = TRUE,
#                outlier.label = name)
# ggbetweenstats(data = grpB,
#                x = Vol_Group,
#                y = Grp$CoV,
#                  outlier.tagging = TRUE, outlier.label = grpA$SKU
               




##################################
boxplot(CoV ~ Vol_Group, data = Grp)
# CoV
plot(dcast(grpA, SKU ~ Vol_Group, value.var = "CoV"))
#ggplot(grpA, aes(x = SKU, y = CoV)) + geom_point() + facet_grid( formula1 ~ formula2)
plot(dcast(grpA, SKU ~ CoV))
plot(dcast(grpB, SKU ~ Vol_Group, value.var = "CoV"))
boxplot(grpB$CoV, grpB$SKU)  
# f <- dcast(grpB, SKU ~ Vol_Group, value.var = "CoV")

xa <- ggplot(grpA, aes(x = SKU, y = CoV)) + geom_point()
xa
ggplotly(xa)
pa <- plot_ly(grpA, y = ~grpA$CoV, type = "box")
pa
###
pf <-plot_ly(Grp, x = Grp$Vol_Group, y = Grp$QTY, type = "box")
pf
###

###
x <- ggplot(grpB, aes(x = SKU, y = CoV)) + geom_point()
x
ggplotly(x)
p <- plot_ly(grpB, y = ~grpB$CoV, type = "box")
p
#---------
x1a <- ggplot(grpA, aes(x = SKU, y = QTY)) + geom_point()
x1a
ggplotly(x1a)
p1a <- plot_ly(grpA, y = ~grpA$QTY, type = "box")
p1a
###
x1 <- ggplot(grpB, aes(x = SKU, y = QTY)) + geom_point()
x1
ggplotly(x1)
p1 <- plot_ly(grpB, y = ~grpB$QTY, type = "box")
p1

####
#checkIfCorrect <- plot_ly(OutlierCorrectedValue, x = OutlierCorrectedValue$PRODUCT_NUM_OBS, y)
xf <- ggplot(OutlierCorrectedValue, aes(x = PRODUCT_NUM_OBS, y = POS_GROSS_SALES_QTY)) + geom_point()
xf
ggplotly(xf)
pf <- plot_ly(OutlierCorrectedValue, y = OutlierCorrectedValue$POS_GROSS_SALES_QTY, type = "box")
pf
###################################################################

forecastperiod<-16
trainperiod<-16
frequency<-52
library(zoo)
library(forecast) # Forecasting algorithm
library(Matrix)
library(Metrics)  # for rmse, mape functions
library(gtools)   # for data manipulation
library(randomForest)
# prophet is a procedure for forecasting time series data based on
# an additive model where non-linear trends are fit with yearly,
# weekly, and daily seasonality, plus holiday effects
library(prophet)
library(forecTheta) # time series forecasting - Theta models
library(tsintermittent)
# Data read
#actual <- read.csv("Segmentation_Analysis.csv")
#actuals <- read.csv("Segmentation_Analysis.csv")
actuals <- Grp
View(actuals)
actuals <- actuals[,c(1,2,15)]
colnames(actuals)<-c("PRODUCT_NUM_OBS","C445_WK_STRT_DATE","POS_GROSS_SALES_QTY")
actuals1<-actuals
View(actuals1)
# Convert to date format
actuals1$C445_WK_STRT_DATE<-as.Date(actuals1$C445_WK_STRT_DATE)
# Assign respective quarters #??
actuals1$Quater<-as.yearqtr(actuals1$C445_WK_STRT_DATE)
##
actuals1<-subset(actuals1,actuals1$C445_WK_STRT_DATE>"2016-06-01")
actuals1<-subset(actuals1,actuals1$C445_WK_STRT_DATE<"2019-07-01")

# Outlier Corrected Value
OutlierCorrectedValue<-data.frame()

for(SKU in sort(unique(actuals1$PRODUCT_NUM_OBS))){
  skuData=subset(x=actuals1,(actuals1$PRODUCT_NUM_OBS==SKU))
  actualstimeseries<-ts(skuData[,c(3)],frequency = frequency)
  x<-tsclean(actualstimeseries,replace.missing = TRUE, lambda = NULL)
  #update gross sales with created timeseries objects
  skuData$POS_GROSS_SALES_QTY<-x 
  OutlierCorrectedValue<-smartbind(OutlierCorrectedValue,skuData) #smartbind is in gtools
} # outlierCorrection Done.
OutlierCorrectedValue

# OutlierCorrectedValue<-actuals1
# # New data frame with 14 Columns 
# OutputSKURMSE<-data.frame(matrix(ncol =14, nrow = 0))
# # name to those 14 columns
# colnames(OutputSKURMSE)<-c("SKU","ARIMA","TES","NNET","TBATS","DES","Theta Model","Snaive","Croston","Average","SES","DES Damped","Moving Average","Best Fit")
