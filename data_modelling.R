# Model Building
# Linear model

x = c(1:10)
y = x*2

xy <- data.frame(x,y)

# Train the model using the training sets and check score
linear <- lm( y~x, data = xy)
summary(linear)
#Predict Output
x1= c(1,2,3,4)
y2 =x1*2
predicted1= predict(linear,data.frame(z))
