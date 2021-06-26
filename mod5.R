data <- read.csv(file = "C:/Users/andre/OneDrive/MIS470/tc.csv", header=TRUE, stringsAsFactors=FALSE)
dim(data)
library(caret)
intrain <- createDataPartition(data$Churn,p=0.7,list=FALSE) 
set.seed(2017)
training <- data[intrain,]
testing <- data[-intrain,] 
dim(training)
dim(testing)
Sys.Date()
LogModel <- glm(as.factor (Churn) ~  . , family = binomial (link = "logit"), data = training)
Sys.Date()
summary(LogModel)
Sys.Date()



testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))  

test1 <- read.csv(file = "C:/Users/andre/OneDrive/MIS470/test1.csv", 
                  header=TRUE, stringsAsFactors=FALSE)


employment.results <- predict(LogModel,newdata=test1,type='response')
employment.results
employment.results <- ifelse(employment.results > 0.5,1,0)
employment.results
Sys.Date()
        