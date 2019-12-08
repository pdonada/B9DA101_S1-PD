
file_read_pd <- read.csv(file.choose())

height <- file_read_pd$Height..cm
weight <- file_read_pd$Weight
age <- file_read_pd$Age
gender <- file_read_pd$Sex
#dtframe <- data.frame(height, weight, age, gender)
dtframe <- data.frame(height, weight)
data_pd <- na.omit(dtframe)
fit_data_pd <- glm(height~weight, data=data_pd, family='gaussian') # linear regression 
summary(fit_data_pd) 	

n <- nrow(data_pd) 
indexes <- sample(n, n*(80/100)) 
trainset <- data_pd[indexes,] 
testset <- data_pd[-indexes,] 

fit_trainset <- glm(height~weight, data = trainset, family = 'gaussian') 
summary(fit_trainset) 

pred <- predict(fit_trainset, testset) 
