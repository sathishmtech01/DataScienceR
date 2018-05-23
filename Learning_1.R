# Variable declaration
# variable declaration in R 
varaiable1 <-10
varaiable1 = 10

# Data Types|Data Structures in R
# 1. Array -  
# 2. Vector - 
# 3. String
# 4. Numeric
# 5. List
# 6. Matrices - 
# 7. Factors - Gives the components of the data (string or numeric)
# 8. Dataframe - Rows and columns (SQL kind of operation)

array_variable = array(3:5)
vector_variable = c(1,2,2,3,4,5)
string_variable = "Hello learning R programming"
numeric_variable = 100
list_variable = list(3:5,4:6,"hello R",vector_variable)
matrix_variable = matrix(list(1:5,1:4))
dataframe_variable = data.frame(mtcars)
factor_vaiable = factor(vector_variable) # it gives distinct value of vectors

# Data frame accesing in R
# 1. column wise
dataframe_variable$mpg
dataframe_variable[1]

# 2. row wise
dataframe_variable$mpg[1]
dataframe_variable[1,1]

dataframe_variable$test <- dataframe_variable$test+20

# summary of data frame
summary(dataframe_variable)

# 




                  
