# Analise dataset of US people based on their salay(Income)
# Use of logistic calssifier to predict whether income >50K or <=50K.

# getting current working directory
getwd()

# changing path
#path = "D:/HACKATHON/Experiment with data/"

# changing directory
#setwd(path)

# reading train dataset
train = read.csv("D:/old data/HACKATHON/Experiment with data/train.csv")

#------ PART-1  exploratory analysis

# data type of each column
str(train)

#getting dimension of data frame
dim(train)

# getting names of all columns in data frame
names(train)

# viewing first six rows of data frame
head(train)

# viewing last rows
tail(train)

# convert all upper column names into lower case
#name_vector = names(train)
#names(train) = tolower(name_vector)


# group continuous & categorical variable
train_contnious = subset(train, select = c('Age',"Hours.Per.Week"))
train_categorical = subset(train, select = c('Workclass','Education','Marital.Status',
                                             'Occupation','Relationship','Race','Sex','Native.Country'))

# install.packages("pastecs")   
# load into library
library(boot)
library("pastecs")

# list of currently loaded packages
search()

# set significant digit & get detailed summary
options(scipen = 100)
options(digits = 2)
stat.desc(train_contnious)

# check no of unique category for each categorical variable
apply(train_categorical,2,function(x){ length(unique(x))})

# table gives count of distinct class of given categorical variable
table(train$Race)

# percentage of observation(probability) of each distinct class of given categorical variable
as.matrix(prop.table(table(train$Race)))

# print top 20 Native.country count
head(sort(table(train_categorical$Native.Country), decreasing = TRUE), 20)

# percentage of observation top 20 countries
head(round(sort(prop.table(table(train_categorical$Native.Country)), decreasing = TRUE),6),20)

# install.packages("gmodels")
library('gmodels')

# cross tabulation of two variables
CrossTable(train$Sex, train$Income.Group)

# install.packages("ggplot2")
library(ggplot2)

# plot in bar chart- categorical - categorical
ggplot(train, aes(Sex, fill = Income.Group )) + geom_bar() +
  labs(title=" bar chart", x='sex',y = 'count') + theme_bw()

# plot scatter plot - continuous-continuous
ggplot(train, aes(x=Age,y= Hours.Per.Week))+ geom_point() +
  labs(title = "scatter plot", x= 'age', y= 'hours/week')

# categorical - continuous
# in this case we generally make box plot for each category
# identify outlier easily
ggplot(train, aes(Sex, Hours.Per.Week)) + geom_boxplot() + labs(title= 'box plot')


#----- missing value treatment

# get missing value information by summary function of Native.Country feature
summary(train$Native.Country)

# getting null index of Native.country feature
index = which(train$Native.Country == "")

# fill null values with mode of Native.country
for (ind in index) {
  train$Native.Country[ind] = "United-States"
  
}


# get missing value information by summary function of workclass feature
summary(train$Workclass)

# getting null index of workclass feature
index = which(train$Workclass == "")

# fill null values with mode of Workclass i.e. "Private"
for (ind in index) {
  train$Workclass[ind] = "Private"
  
}



# check na values column wise in train dataset
# colSums(table(is.null(train)))

#install.packages("mlr")
#library('mlr')
# for missing value imputation use mlr package
#imputed_data = impute(train, classes = list(factor = imputeMode()))
# update train dataset with imputed value
#train <- imputed_data$data


# getting responce variable column
data = train[,12]

# check lenghth of vector
length(data)

# convect income group column into numeric variable
vect1 = c()
i = 0
for ( ele in data)
{ 
  if ( ele == '<=50K')
    vect1[i+1] = 0
  else 
    vect1[i+1] = 1
  i = i+1
}

# remove last column from train data frame
train = train[,1:11]

# bind vect1 column to train data frame
train = cbind(train,vect1)

# changing name of vect1 column to Income.Group
names(train) = c( "ID" ,"Age" ,"Workclass","Education","Marital.Status" ,"Occupation" ,
                  "Relationship","Race"    ,"Sex", "Hours.Per.Week" ,"Native.Country" ,
                  "Income.Group")




# checking class bias of income.group
table(train$Income.Group)

# getting no of col & rows from data frame
nrow(train)
ncol(train)
dim(train)


# access subset from data frame based on condition 
# creating df of gender == male & income.group == >50K
train[train$Sex == 'Male' & train$Income.Group == '>50K',]

# creating data frame where eduction == Masters 
df = train[which(train$Education == 'Masters') , c(2,3,4,10,11,12)]

# analyse workclass  feature
sort(table(df$Workclass))

# graphical representation of one categorical and other continus variable
boxplot(train$Sex, train$Hours.Per.Week)


# contigency table( two categorical variable ) of sex & workclass group 
ftable(train$Sex, train$Workclass)

# getting 2 & 5 th row from data frame train
train[c(2,5),]

# getting 2 & 5 th row with id & income.group column 
train[c(2,5),c("ID","Income.Group")]

# using split function on workclass that returns list
sp_data = split(train,f= train$Workclass)

# access never worked class employee
sp_data$`Never-worked`

# ordering of train by decreasing ID
#idx <- order(train$ID, decreasing = T)
#train[idx,]



# droping unused levels from native.country feature
train$Native.Country <- droplevels(train$Native.Country)

# getting no of levels in factor
nlevels(train$Native.Country)


# getting information of income >50k &  workclass==private & education == bachelors using dplyr package

#install.packages("dplyr")
library(dplyr)

temp = train %>% filter( train$Income.Group == ">50K" & train$Education == "Bachelors")
temp2 = temp[temp$Workclass == "Private",c("Workclass")]
length(temp2)

# contengency table on education & income group
ftable(train$Education, train$Income.Group)

# stastics on hours per week
# get mean
mean(train$Hours.Per.Week)

# get median
median(train$Hours.Per.Week)

# get standard deviation
sd(train$Hours.Per.Week)

# variance on hours per week
var(train$Hours.Per.Week)

# normal distributiuo on hours per week
#rnorm(32561, mean = 40.44, sd = 12.35)

#----- PART2 stastical modeling

# creating trainning & test samples
# Create Training Data
input_ones <- train[which(train$Income.Group == 1), ]  # all 1's
input_zeros <- train[which(train$Income.Group == 0), ]  # all 0's

set.seed(100)  # for repeatability of samples

input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_zeros))  # 0's for training. Pick as many 0's as 1's

training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's


# check dim of data set
dim(trainingData)

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]

testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 

# chech dim of test data set
dim(testData)

# remove id column from both data set(train & test)
trainingData = trainingData[,2:12]
testData = testData[,2:12]

#---- Build Logit model & predict

logitMod <- glm(Income.Group ~ Relationship + Age + Occupation + Education, data=trainingData, family=binomial(link="logit"))

predicted <- predict(logitMod, testData, type="response")  # predicted scores


summary(logitMod)


