# Useing liner regression model

# dataset
data = iris
colnames(data)

# variable selection on Intuition
data_sel = data.frame(data$Sepal.Length,data$Sepal.Width,data$Petal.Length,data$Petal.Width)

# Training and Testing data
data_train = data_sel[1:100,]
data_test = data_sel[101:150,]

# Building model
# data.Petal.Width is the target value
#  repreents all other columns
linear_model1 = lm(data.Petal.Width~.,data = data_train)
linear_model2 = lm(data.Petal.Width~data.Petal.Length,data = data_train)
linear_model3 = lm(data.Petal.Width~data.Sepal.Length,data = data_train)
linear_model4 = lm(data.Petal.Width~data.Sepal.Width,data = data_train)

# summary of the model
summary(linear_model1)
summary(linear_model2)
summary(linear_model3)
summary(linear_model4)

# Test the model
predicted= predict(linear_model1,data_test)

plot(linear_model1)
plot(linear_model2)



















































