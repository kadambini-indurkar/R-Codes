### Intermediate R ###

getwd()
setwd("C:/Users/shraiyas.joshi/Journey [O9 Solutions]/Study Material/R - Resources")

library(data.table)

df <- read.csv("Test2.csv", header = T, stringsAsFactors = F)

df_pivot <- dcast(df, Student.ID ~ Subjects, "Marks")
df_melt <- melt(df_pivot, id.var = "Student.ID")

### Plots ###

x <- c(12,15,20,19,16,9,1,18)
y <- c(50,20,10,15,16,40,100,30)
Student <- c("A","B","C","D","E","F","G","H")

## Histograms
hist(df$Marks, main = "Histogram", xlab = "Student", xlim = c(0,50), ylim = c(0,10), col = c("red", "yellow", "green", "violet")) #xlab,xlim,ylim,breaks,col,border

## Pie
pie(x, Student, radius = c(12)) #, radius, main, col, clockwise

vec <- vector()
x <- 1:100
for (i in x){
  vec <- c(vec,i^2)
}

## Bar Chart
barplot(x, xlab = Student, ylab = "Marks",main = "Chart")

## Scatter Plot
df_CG <- read.csv("CanadaGoose_SourceData.csv")
colnames(df_CG)
df_import <- unique(df_CG[, c(17,20)])
df_import1 <- na.omit(df_import)

plot(df_import1$Average.Temperature..in.Celsius., df_import1$Precipitation..in.tenths.of.mm.) #main, xlab, ylab, xlim, ylim, axes

## Line Chart
plot(df_import1$Precipitation..in.tenths.of.mm.,type = "o") #type takes the value "p" to draw only the points, "l" to draw only the lines and "o" to draw both points and lines.

summary(redwine1$fixed.acidity)
data("AirPassengers"
     )
data1 = data("AirPassengers")
View(data1)
