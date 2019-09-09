#######################
### Starting with R ###
#######################

### R Objects - Scalars
#######################
z <- 5
z

w <- sqrt(z^2)
w
i <- (z*2 + 45)/2
i

(34 + 90)/12.5

ls()
rm(z,w,i)

### Vectors
###########
v <- c(4,7,23.5,76.2,80)
v

length(v)
mode(v)

v <- c(4,7,9)
v
mode(v)

v <- c(4,7,23.5,76.2,80,TRUE)
v

u <- c(4,6,Inf,2, "cat")
u
mode(u)

k <- c(T,F,NA,TRUE, 0)
k

v[2]

v[1] <- 'hello'
v

x <- vector()  # empty vector

x[3] <- 45
x

length(x)
x[10]
length(x)

x[5] <- 4
x

v <- c(45,243,78,343,445,44,56,77)
v
v <- c(v[5],v[7])
v

v <- c(4,7,23.5,76.2,80)

x <- sqrt(v)
x

v1 <- c(4,6,87)
v2 <- c(34,32.4,12)
v1+v2

v1 <- c(4,6,8,24)
v2 <- c(10,2)
v1/v2

v3 <- v1^2
v3

v1 <- c(4,6,8,24)
2*v1

x <- c(11,13,34,6,2,4,5,68)
sort(x)
order(x)
x[order(-x)]

### Factors
###########
g <- c('f','m','m','m','f','m','f','m','f','f')
g

g <- factor(g)
g 

other.g <- factor(c('m','m','m','m','m'),levels=c('f','m'))
other.g

a <- factor(c('adult','adult','juvenile','juvenile','adult','adult',
              'adult','juvenile','adult','juvenile'))

### Generating sequences
########################
x <- 1000:1

### Indexing
############
x <- c(0,-3,4,-1,45,90,-5)
x > 0

x[x>0]

x[x <= -2 | x > 5]
x[x > 40 & x < 100]

x[1:3]
x[c(4,6)]
y <- c(1,4)
x[y]

x[-1]
x[-c(4,6)]
x <- x[-(1:3)]

### Matrices
############
mo <- c(45,23,66,77,33,44,56,12,78,23, 11)
length(mo)

m <- matrix(mo,2,7) #column wise operation
m

m <- matrix(c(45,23,66,77,33,44,56,12,78,23),2,5,byrow = T)
m

m[1,]
m[,4]
m[2,3]
m[1,-c(3,5)]

### Lists
#########
my.lst <- list(stud.id=34453, 
               stud.name="John", 
               stud.marks=c(14.3,12,15,19, "cat"))

my.lst

my.lst[1]
my.lst$stud.id

my.lst$stud.marks
my.lst[[3]]

mode(my.lst[1])
mode(my.lst[[1]])

my.lst$parents.names <- c("Ana","Mike")
my.lst

length(my.lst)
unlist(my.lst)

### Data Frames
###############
my.dataset <- data.frame(site=c('A','B','A','A','B'),
                         season=c('Winter','Summer','Summer','Spring','Fall'),
                         pH = c(7.4,6.3,8.6,7.2,8.9))

my.dataset[3,2]

a <- my.dataset$pH

my.dataset[my.dataset$pH > 7, ]
my.dataset[my.dataset$site == 'A','pH']
my.dataset[my.dataset$season == 'Summer',c('site','pH')]

my.dataset$NO3 <- c(234.5,256.6,654.1,356.7,776.4)
my.dataset

nrow(my.dataset)
ncol(my.dataset)
colnames(my.dataset)
colnames(my.dataset) <- c("area","season","pH","NO3" )
my.dataset


### Using Packages
##################
install.packages("xlsx")
update.packages()

library(dplyr)
library(xlsx)
# dplyr, xlsx

### Working Directory
#####################
getwd()
setwd("C:/Users/shraiyas.joshi/Journey [O9 Solutions]/Study Material/R - Resources")

### Importing Datasets
######################
df <- read.csv("Fact.SMFM.csv", header = T, stringsAsFactors = F)
df <- read.xlsx("Test1.xlsx", sheetName = "Sheet2")

### Working with Data Frames
############################
df <- data.frame(Date=as.Date(character()),
                 File=character(), 
                 User=character(), 
                 stringsAsFactors=FALSE) 

df <- iris

df1 <- df %>% filter(Species == "setosa")
df1 <- df %>% select(Species, Sepal.Length, Sepal.Width)
df1 <- df[1:100, ]
df1 <- df[, c(1,2,5)]

df1 <- df %>% filter(Species == "setosa")
df2 <- df %>% filter(Species == "virginica")
df3 <- rbind(df1, df2)
df4 <- cbind(df1, df2)

df1 <- df[1:10, ]
df1[5,1] <- NA
is.na(df1)
sum(is.na(df1))
sum(is.na(df1$Sepal.Length))
df1[is.na(df1$Sepal.Length)] <- 0

df2 <- na.omit(df1)

df2$Petal.Color <- "Red"

df2 <- df2 %>% arrange(Sepal.Length, Sepal.Width)

# Left, Right, Inner, Outer & Cross Joins
# DPLYR

# Table A
ID <- c(1,2,3,4,5)
Name <- c("A","B","C","D","E")
Marks <- c(24,26,30,21,28)
dfA <- as.data.frame(cbind(ID, Name, Marks))

# Table B
ID <- c(2,3,4)
TeacherName <- c("Pink","Floyd","FDFG")
dfB <- as.data.frame(cbind(ID, TeacherName))

# Joins - dplyr
df_left_join <- left_join(dfA, dfB, by = c("ID")) 
df_right_join <- right_join(dfA, dfB, by = c("ID")) 
df_inner_join <- inner_join(dfA, dfB, by = c("ID"))
df_full_join <- full_join(dfA, dfB, by = c("ID"))

# Group By - mutate & summarise
df_import <- read.csv("Segmentation_TestData.csv", header = T, stringsAsFactors = F)
df_agg <- df_import %>% group_by(Country) %>% mutate(TotalSales = sum(TotalShipments, na.rm = ))
df_agg1 <- df_import %>% group_by(Country) %>% summarise(TotalSales = sum(TotalShipments, na.rm = T))

# String Manipulation
a <- "The0 Lord1 of2 The3 Rings4"
nchar(a)
tolower(a)
toupper(a)
substr(a, start = 1, stop = 10)
sub(pattern = " ", replacement = "/", x = a)
gsub(pattern = " ", replacement = "/", x = a) #Pattern - Regex
gsub(pattern = "[0-9]+", replacement = "", x = a) 
gsub(pattern = "[^0-9]", replacement = "", x = a) 

unlist(strsplit(x = a, split = " "))
# unlist

# Date-Time manipulation
a1 <- "2019-07-30"
a2 <- "30/07/19"
a3 <- "2019-30-07"
a4 <- "July 30, 2019"

as.Date(a1, "%Y-%m-%d")
as.Date(a2, "%d/%m/%y")
as.Date(a3, "%Y-%d-%m")
as.Date(a4, "%B %d, %Y")

format(as.POSIXct("2019-07-30", format = "%Y-%m-%d"), "%m/%d/%Y")
# strptime & strftime 

### User Defined Functions
##########################

A <- function(x){
  M = mean(x)
  S= sd(x)
  CoV = M/S
  return(CoV)
}
df_import <- df_import %>% group_by(Country) %>%summarise(CoV = A(df_import$TotalShipments))

df1 <- data.frame(iris[3:5], apply(iris[1:2],2, A) )
df2 <- data.frame(iris[3:5], lapply(iris[1:2], A))
df2$Petal.Length_mean <- mean(df2$Petal.Length)