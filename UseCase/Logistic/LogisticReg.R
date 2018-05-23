
#load libraries and data
library (data.table)
library (plyr)
library (stringr)

# Data Gathering
train <- read.csv("/home/sathish/Sathish/Sathish/Learning/Big Data/Data Science R/R programming/UseCase/Logistic/Titanic_Data/train.csv",na.strings = c(""," ",NA,"NA"))
test <- read.csv("/home/sathish/Sathish/Sathish/Learning/Big Data/Data Science R/R programming/UseCase/Logistic/Titanic_Data/test.csv",na.strings = c(""," ",NA,"NA"))


#Data Understanding
#https://www.kaggle.com/c/titanic/data

# Data Interpretation
summary(train)

# Data Preprocessing
# Data Cleaing
# Age column - NAs should be replaced with some value
mean(na.exclude(train$Age))
# Data imputation
train$Age[is.na(train$Age)] <- mean(na.exclude(train$Age))

# Checking the cabin column - Since it has lots of class, can be igorned first
levels(factor(train$Cabin))

# Data Elimination
data_prep <- train[!is.na(train$Embarked),]

summary(data_prep)

# Data Selection
#colnames(data_prep)
col_sel <- c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked" )
data_sel = subset(data_prep, select = col_sel)
summary(data_sel)


# Data modelling
model1 <- glm(Survived ~.,data = data_sel,family = binomial(link='logit'))
summary(model1)

model2 <- glm(Survived ~Pclass+Sex+Age+SibSp,data = data_sel,family = binomial(link='logit'))
summary(model2)

new_data = subset(data_prep, select = c("Pclass","Sex","Age","SibSp"))
fitted.results = predict(model2,newdata = new_data,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != train$Survived)
print(paste('Accuracy',1-misClasificError))
