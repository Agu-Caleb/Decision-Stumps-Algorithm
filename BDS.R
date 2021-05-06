
#Importing Boston from Mass data set
library(MASS)
attach(Boston)

# Setting seed for randomness
set.seed(1102)
# creating the train and test data sets
head(Boston)
test_size = nrow(Boston) * 0.5
print(test_size)
split_index = sample(1:length(medv), size=test_size)
train = Boston[split_index,]
test  = Boston[-split_index,]

# splitting the data set
# atributes <- c("lstat", "rm",)
# label <- c("medv")
# Trainx <- train[atributes]
# Testx  <- test[atributes]
# TrainY <- train[label]
# TestY  <- test[label]

# Selecting the relevant columns
columns <- c("medv","lstat", "rm" )
trainset <- train[columns]
testset  <- test[columns]


RSS_total= c()
DSImplementation <- function(data, name, sequence){
  
  
  condition1 = data[name] < sequence
  condition2 = data[name] >= sequence
  
  # group observations into two
  data.g1 = data[name][condition1]
  data.g2 = data[name][condition2]
  
  # make prediction value for each group
  RSS_total = sum((data$medv[condition1] - ave(data.g1))**2) + sum((data$medv[condition2] - ave(data.g2))**2)
  print(RSS_total)
  
  # set the proper value to the return values
  if(length(data.g1) == 0){
    smaller_value = 0
  } else {
    smaller_value = ave(data.g1)[1]
  }
  if(length(data.g2) == 0){
    bigger_value = 0
  } else {
    bigger_value = ave(data.g2)[1]
  }
  # return multiple values in a vector manner
  return(c(RSS_total, sequence, smaller_value, bigger_value))
}

traindatasets <- function(data, name, param) {
  result = c(1000)
  best_param = 0
  
  for (index in 1:length(param)) {
    RSS = DSImplementation(data, name, param[index])[1]
    if(RSS <= min(result)){
      best_param = param[index]
    }
    result[index] = RSS
    print(result[index])
  }
  plot(result)
  title("traindatasets result")
 # print(best_param)
  return(best_param)
}




prediction <- function(new_data, sequence, smaller_value, bigger_value){
  if(new_data < sequence){
    return(smaller_value)
  } else {
    return(bigger_value)
  }
  
}


BDSImplementation <- function(train_data, test_data, names, params, theta, B){
  clfs = data.frame(matrix(vector(), 0, 4, dimnames=list(c(), c("RSS_total", "sequence", "smaller_value", "bigger_value"))), stringsAsFactors=F)
  
  for(i in 1:B){
    # randomly choose the target col to split
    random_index = sample.int(length(names),1)
    name = names[random_index]
    param = params[random_index]
    T= DSImplementation(train_data, name, param)
    print(T)
    list[RSS_total, sequence, smaller_value, bigger_value] = rnorm(T)
    clfs[i, ] <- c(RSS_total, sequence, smaller_value, bigger_value)
  }
  # print("Your trained model")
  print(clfs)
  
  MSE = 0
  for(index in 1:dim(test_data)[1]){
    y_hat = 0
    for(i in 1:B){
      y_hat = y_hat + theta*prediction(test_data[index,], clfs[i,]$sequence, clfs[i,]$smaller_value, clfs[i,]$bigger_value)
    }
    MSE = MSE + (test_data[index, ]$medv - y_hat)**2
  }
  return(MSE/dim(test_data)[1])
}



# Setting the threshholDSImplementation
s_lstat = seq(3.8,37.9,1.1)
s_rm = seq(2.6,8.7,2.1)


lstat_bestparam = traindatasets(trainset, "lstat", s_lstat)
print(lstat_bestparam)
rm_bestparam = traindatasets(testset, "rm", s_rm)
print(rm_bestparam)

# Q1. Train your DSImplementation implementation on the traindatasets set. Find the MSE on the test set. Include it in your report.
lstat_MSE = DSImplementation(testset, "lstat", lstat_bestparam)[1]/dim(testset)[1]
print(lstat_MSE)
rm_MSE = DSImplementation(testset, "rm", rm_bestparam)[1]/dim(testset)[1]
print(rm_MSE)

# Q2.
test_mse = c()
test_mse= BDSImplementation(trainset, testset, c("lstat", "rm"), c(lstat_bestparam,rm_bestparam), 0.01, 1000)
print(test_mse)




# Q3. Plot the test MSE for a fixed value of theta as a function of B includes [1, B0] (the number of trees) for as large B0 as possible. 

title("Test MSE")
plot(1:5000,test_mse,type="l",main="Test MSE", xlab="B", ylab="Test MSE")



