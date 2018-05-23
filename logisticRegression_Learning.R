# Useing logistic regression model

# dataset
data_sel = iris
colnames(data_sel)

# variable selection on Intuition
#data_sel = data.frame(data$Sepal.Length,data$Sepal.Width,data$Petal.Length,data$Petal.Width)

# Training and Testing data
data_train = data_sel[1:100,]
data_test = data_sel[101:150,]

# Building model
# data.Petal.Width is the target value
#  repreents all other columns
linear_model1 = glm(Species~.,data = data_train, family = "binomial")
linear_model2 = glm(Species~Petal.Length,data = data_train,family = "binomial")
linear_model3 = glm(Species~Sepal.Length,data = data_train,family = "binomial")
linear_model4 = glm(Species~Sepal.Width,data = data_train,family = "binomial")
linear_model5 = glm(Species~.,data = data_train,family = "logit")
# summary of the model
summary(linear_model1)
summary(linear_model2)
summary(linear_model3)
summary(linear_model4)
summary(linear_model5)

# Test the model
predicted= predict(linear_model1,data_test)

plot(linear_model1)
plot(linear_model2)

# R datasets
# datasets::

















































