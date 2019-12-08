#############################################
#Question 1

file_read_pd <- read.csv(file.choose())
data_pd <- na.omit(file_read_pd)

height <- data_pd$Height..cm
weight <- data_pd$Weight
age <- data_pd$Age
gender <- data_pd$Sex
dtframe <- data.frame(height, weight, age, gender)

rootmean <- 0 
for(i in dtframe){ 
  n <- nrow(dtframe) 
  indexes <- sample(n, n*(80/100)) 
  trainset <- dtframe[indexes,] 
  testset <- dtframe[-indexes,] 
 
  fit <- glm(trainset$height~trainset$weight, data=trainset, family='poisson') 
  summary(fit)
  pred <- predict(fit, testset) 
  actual <- testset$height~testset$weight
  rmse <- sqrt((sum((pred-actual)^2)/nrow(testset))) 
  rootmean <- rootmean + rmse 
} 

rootmean <- rootmean/1000 
rootmean  
