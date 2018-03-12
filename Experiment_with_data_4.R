# Analise dataset of US people based on their salay(Income)
# Use of logistic calssifier to predict whether income >50K or <=50K.

#----------- PART 1 Setting working directory -------------#

# getting current working directory
getwd()

# changing path
#path = "D:/HACKATHON/Experiment with data/"

# changing directory
#setwd(path)


#----------- PART 2 Load & view train dataset ------------------#

train = read.csv("D:/old data/HACKATHON/Experiment with data/train.csv")

# get structure of data
str(train)

# viewing first six rows of data frame
head(train)

# viewing last rows
tail(train)


# divide train data into continuous & categorical variable
train_contnious = subset(train, select = c('Age',"Hours.Per.Week"))
train_categorical = subset(train, select = c('Workclass','Education','Marital.Status',
                                             'Occupation','Relationship','Race','Sex','Native.Country'))

# install.packages("pastecs")

# load into library
library("boot")
library("pastecs")

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


#------------PART 3 missing value treatment -------------------------------------#

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

# fill null values with mode of Workclass i.e. "Private
for (ind in index) {
  train$Workclass[ind] = "Private"
  
}

#--------- PART 4 convert Income.Group categorical column into numerical column ---------#

# getting responce variable column
data = train[,12]

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


#------------------ PART 5  Exploratory Analysis   -----------------------------------#

# checking class bias of income.group
table(train$Income.Group)

# get structure of data
str(train)

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


#-------------        PART 6 Create training & test data set        --------------------#

# remove id column from both data set(train & test)
train = train[,2:12]

# creating trainning & test samples

# Create Training Data
input_ones <- train[which(train$Income.Group == 1), ]  # all 1's
input_zeros <- train[which(train$Income.Group == 0), ]  # all 0's

set.seed(100)  # for repeatability of samples

input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's

training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's


# check data set
head(trainingData)
tail(trainingData)
str(trainingData)

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]

testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 

# chech test data set
head(testData)
tail(testData)
str(testData)


#---------------- PART 7 Compute information values for each feature ---------#

#install.packages("smbinning")
library(smbinning)
library("sqldf","gsubfn","proto","partykit") 
library("libcoin","grid") 
library("mvtnorm","rpart","RSQLite","Formula")
library(smbinning)

# segregate continuous and factor variables
factor_vars <- c ("Workclass", "Education", "MaritalStatus", "Occupation", "Relationship", "Race", "Sex", "NativeCountry")
continuous_vars <- c("Age","HoursPerWeek")

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(10))  # init for IV results

names(trainingData) = c("Age","Workclass","Education","MaritalStatus","Occupation","Relationship",
                        "Race","Sex","HoursPerWeek","NativeCountry","IncomeGroup")

trainingData$IncomeGroup = as.integer(trainingData$IncomeGroup)

# compute IV for categoricals
for(factor_var in factor_vars){
  smb <- smbinning.factor(trainingData, y="IncomeGroup", x=factor_var, maxcat = 41)  # WOE table
  print(class(smb))
  if(class(smb) != "character"){ # heck if some error occured
    print(smb$iv)
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(trainingData, y="IncomeGroup", x=continuous_var)  # WOE table
  print(class(smb))
  if(class(smb) != "character"){  # any error while calculating scores.
    print(smb$iv)
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

iv_df <- iv_df[order(-iv_df$IV), ]  # sort
print(iv_df)




#---------     PART 8  Building model & prediction         ---------------------------#

# build model
logitMod <- glm(IncomeGroup ~ Relationship + Age + Occupation + Education, data=trainingData, family=binomial(link="logit"))

# predict on test data
predicted <- predict(logitMod, testData, type="response")  # predicted scores

# set threshould to 0.5 transform predicted data into  0 or 1
index = 1
predicted_data = c()
for (data in predicted){
  if (data < 0.5) {
    predicted_data[index] = 0
  } else {
    predicted_data[index] = 1
  }
  index = index +1
}

test_matrix = data.frame(cbind(testData$Income.Group, predicted_data))

# change names of test_matrix
names(test_matrix) = c("actual","predicted")

head(test_matrix,10)
tail(test_matrix,10)

#install.packages("InformationValue")
library(InformationValue)
optCutOff <- optimalCutoff(testData$Income.Group, predicted)[1] 

# Misclassifier error 
misClassError(testData$Income.Group, predicted, threshold = optCutOff)

# sensitivity
sensitivity(testData$Income.Group, predicted, threshold = optCutOff)

# specificity
specificity(testData$Income.Group, predicted, threshold = optCutOff)

# confusion matrix
confusionMatrix(testData$Income.Group, predicted, threshold = optCutOff)


#------------------- END -----------------------#
