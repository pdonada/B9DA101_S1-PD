
file_read_pd <- read.csv(file.choose()) #London2012Athletes.csv
summary(file_read_pd)
weight <- file_read_pd$Weight
height <- file_read_pd$Height..cm
dtframe <- data.frame(weight, height)
data_pd <- na.omit(dtframe)
fit_data_pd <- glm(weight~height, data=data_pd, family='gaussian') # linear regression 
summary(fit_data_pd) 	
n <- nrow(data_pd) 
indexes <- sample(n, n*(80/100)) 
trainset <- data_pd[indexes,] 
testset <- data_pd[-indexes,] 
actual <- testset$weight
fit_trainset <- glm(weight~height, data = trainset, family = 'gaussian') 
summary(fit_trainset) 
pred_pd <- predict(fit_trainset, testset) 
head(pred_pd)
pred_weight_pd <- ifelse(pred_pd >= 0.5, 1, 0)
head(pred_weight_pd)
table(actual) 
table(pred_weight_pd) 
tab1 = table(pred_weight_pd, actual)  # create confusion matrix 
accuracy = sum(tab1 [row(tab1) == col(tab1)])/ sum(tab1) 
accuracy 


##################################################

#name <- c('gender', 'yes', 'no', 'cant say')
male <- c(200, 150, 50)
fem <- c(250, 300, 50)
datafr <- data.frame(male,fem)
#datatb <- table(datafr$male, datafr$fem)

a <- chisq.test(datafr)  # test value 
a$statistic #testvalue

alpha <- 0.05
c_value <- qchisq(1-alpha, a$parameter) 
c_value
