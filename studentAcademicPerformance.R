#Garbage collection and removing objects
rm(list=ls()); 
gc()

#Importing the dataset
dataset = read.csv('student-por.csv', header = TRUE, sep = ',', stringsAsFactors = TRUE)
str(dataset)

#Strucure and summary of the dataset
View(dataset)
str(dataset)
summary(dataset)
colnames(dataset)

#Check missing values
apply(is.na(dataset),2, sum)

#Check null values
sum(is.null(dataset))

#Check for duplicate records
sum(is.na(duplicated(dataset)))

#Check for Outliers
outlier <- boxplot(dataset$absences)
outlier$out
table(dataset$absences)

#Check for Outliers Age
outlier <- boxplot(dataset$age)
outlier$out
dataset <- dataset[dataset$age != 22,]


#Converting continuous variables to categorical variables 
dataset$Medu <- as.factor(dataset$Medu)
dataset$Fedu <- as.factor(dataset$Fedu)
dataset$traveltime <- as.factor(dataset$traveltime)
dataset$studytime <- as.factor(dataset$studytime)
dataset$failures <- as.factor(dataset$failures)
dataset$famrel <- as.factor(dataset$famrel)
dataset$freetime <- as.factor(dataset$freetime)
dataset$goout <- as.factor(dataset$goout)
dataset$Dalc <- as.factor(dataset$Dalc)
dataset$Walc <- as.factor(dataset$Walc)
dataset$health <- as.factor(dataset$health)
str(dataset)



#Creating Dummy variables
install.packages('dummy')
library(dummy)
colnames(dataset)
db<-dataset[,c(7:15,24:29)] #selecting columns for which we want to create dummy variables
dummy_db<-dummy(db)
final_data <- cbind(dataset,dummy_db)
str(final_data)
colnames(final_data)

#Removing the original columns from which the dummy variables were created
final_data <- final_data[,-c(7:14)]
colnames(final_data)
final_data <- final_data[,-c(16:21)]
final_data<-final_data[,-7]
colnames(final_data)
str(final_data)

#Removing Reference variables from dataset
colnames(final_data)
final_data <- final_data[,-c(19,24,29,34,39,43,46,50,54,58,63,68,73,78,83)]
final_data_copy <- final_data
final_data<- final_data[,-c(16:17)]

#Correlation matrix
install.packages('corrplot')
library(corrplot)
dataset_cor<- final_data[,-16]
ix<-1:69 
dataset_cor[1:69] <- lapply(dataset_cor[1:69], as.integer)
Correlations <- round(cor(dataset_cor),2)
corrplot(Correlations, method = "circle", tl.col = "red", 
         cl.ratio = 0.1, cl.align.text = 'l', win.asp = 0.5)

write.csv(Correlations, file = "corrplot.csv")


#Checking multicollineaity in Database vy VIF values(Variable Inflation Factor)
install.packages('usdm')
library(usdm)
vif(dataset_cor)


#Removing Medu and Fedu from dataset
colnames(final_data)
final_data<- final_data[,-c(17:24)]
str(final_data)

final_data_v2<- final_data

#Visualizations
install.packages('dyplr')
library(scales)
library(ggplot2)
library(dplyr)

plot(as.integer(dataset$Dalc), dataset$G3, col = "blue", xlab = "Workday Alcohol Consumption", ylab = "Final Grade", 
     main = "Workday Alcohol Consumption vs Final Grade")

plot(as.integer(dataset$Walc), dataset$G3, col = "blue", xlab = "Weekend Alcohol Consumption", ylab = "Final Grade", 
     main = "Weekend Alcohol Consumption vs Final Grade")

plot(dataset$age, dataset$G3, col = "blue", xlab = "Age", ylab = "Final Grade", 
     main = "Age vs Final Grade")

ggplot(dataset, aes(x=factor(age), y=G3, group = 1, color = 'red')) + xlab ("Age") + ylab("G3") + 
  ggtitle("Trend of G3 over Age") + stat_summary(fun.y="mean", geom="line", color = 'blue')


ggplot(dataset, aes(x=factor(Dalc), y=G3, group = 1, color = 'red')) + xlab ("Workday Alcohol Consumption") + ylab("G3") + 
  ggtitle("Trend of G3 over Dalc") + stat_summary(fun.y="mean", geom="line", color = 'blue')

ggplot(dataset, aes(x=factor(Walc), y=G3, group = 1, color = 'red')) + xlab ("Weekend Alcohol Consumption") + ylab("G3") + 
  ggtitle("Trend of G3 over Walc") + stat_summary(fun.y="mean", geom="line", color = 'blue')

ggplot(dataset, aes(x=factor(address), y=G3, group = 1, color = 'red')) + xlab ("Address") + ylab("G3") + 
  ggtitle("Address vs Grades") + stat_summary(fun.y="sum", geom="bar", color = 'blue', fill = 'blue')

ggplot(dataset, aes(x=factor(higher), y=G3, group = 1)) + xlab ("Higher") + ylab("G3") + 
  ggtitle("Wants to take Higher Education vs Grades") + stat_summary(fun.y="mean", geom="bar", color = 'blue', fill = 'blue')

ggplot(dataset, aes(x=factor(school), y=G3, group = 1)) + xlab ("Student's School") + ylab("G3") + 
  ggtitle("Student's School vs Grades") + stat_summary(fun.y="mean", geom="bar", color = 'blue', fill = 'blue')

#Partition the dataset
install.packages('caTools')
library(caTools)
split <- sample.split(final_data, SplitRatio = .75)
train_data <- subset(final_data, split == TRUE)
test_data <- subset(final_data, split == FALSE)

#Splitting dataset into Training and Test Set
#MLR

#Train the dataset
set.seed(121)
obj.null <- lm(G3~1, data = train_data)
obj.full <- lm(G3~., data = train_data)
obj_forward <- step(obj.null, scope = list(lower = obj.null, upper = obj.full), direction = 'forward')
obj_backward <- step(obj.full, scope=list(lower=obj.null, upper=obj.full), direction='backward')
names(obj_backward)

#Test the dataset
y_pred = predict(obj_forward, newdata = test_data)
y_pred_b = predict(obj_backward, newdata = test_data)

# Checking Fit
install.packages('hydroGOF')
library(hydroGOF)
rmse(test_data$G3, y_pred)
rmse(test_data$G3, y_pred_b)

y_pred_b <- as.data.frame(y_pred_b)

plot(row_number(y_pred_b),y_pred_b$y_pred_b,xlim=range(c(row_number(y_pred_b),row_number(test_data$G3))),
     ylim=range(y_pred_b$y_pred_b,test_data$G3),col="red", xlab = "Record Number",
     ylab = "Predicted (Blue) and Actual (Red)", main = "Predicted Grades vs Actual Grades") 
points(row_number(test_data$G3),test_data$G3,col="blue") 

# normality (of the residual)
hist(obj_backward$residuals)

# linearity
plot(final_data$age, final_data$G3)

# Homoscedasticity
plot(obj_backward$residuals, obj_backward$fitted.values)

#Decision Tree
#Fitting Decision Tree to the training dataset
library(rpart)
set.seed(121)
dtModel = rpart(formula = G3 ~ ., method = "anova", data = train_data)
#plot(dtModel)
#text(dtModel)

pfit = prune(dtModel, cp = dtModel$cptable[which.min(dtModel$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
#plot(pfit, uniform=T, main="Pruned Classification Tree for Predicting Grades")
#text(pfit, use.n=T, all=T, cex=.8, pos = 1, offset = 0.5)
install.packages("rpart.plot")
library(rpart.plot)
prp(dtModel, type = 1, fallen.leaves = 1, clip.right.labs = 1, main = "Full Regression Tree")
prp(pfit, type = 2, fallen.leaves = 1, clip.right.labs = 0, main = "Minimum Error Regression Tree")

#Predicting Result on test set DT Model
y_pred_dt = predict(dtModel, newdata = test_data)
y_prune_dt <- predict(pfit, newdata = test_data)

y_prune_dt <- as.data.frame(y_prune_dt)
 
plot(row_number(y_prune_dt),y_prune_dt$y_prune_dt,xlim=range(c(row_number(y_prune_dt),row_number(test_data$G3))),
    ylim=range(y_prune_dt$y_prune_dt,test_data$G3),col="red", xlab = "Record Number",
    ylab = "Predicted (Blue) and Actual (Red)", main = "Predicted Grades vs Actual Grades") 
points(row_number(test_data$G3),test_data$G3,col="blue") 

rmse(test_data$G3, y_pred_dt)
rmse(test_data$G3, y_prune_dt)

# install.packages("DMwR")
# library(DMwR)
# prettyTree(dtModel, branch = 0.3, margin = 0.025, uniform = T, fwidth = 0.9, 
#            fheight = 0.5, font = 13, center = 0.1, cex = 0.75)
# prettyTree(pfit, branch = 0.4, margin = 0.1, uniform = T, fwidth = 0.9, 
#            fheight = 0.5, font = 13, center = 0.1, cex = 0.8)
# 
# y_prune_dt <- as.data.frame(y_prune_dt)
# 
# plot(row_number(y_prune_dt),y_prune_dt$y_prune_dt,xlim=range(c(row_number(y_prune_dt),row_number(test_data$G3))),
#      ylim=range(y_prune_dt$y_prune_dt,test_data$G3),col="red", xlab = "Record Number",
#      ylab = "Predicted Grades", main = "Predicted Grades vs Actual Grades") 
# points(row_number(test_data$G3),test_data$G3,col="blue") 

#plot(y_prune_dt$y_prune_dt, test_data$G3, xlab)

#Using Random Forest for Analysis

#Fitting Random Forest to the training  set
install.packages('randomForest')
library(randomForest)
set.seed(121)
randomForestModel = randomForest(G3 ~ ., data = train_data, nTree = 500, nodesize = 12, importance =TRUE)

#Predicting Test set results using Random Forest
y_pred_randomForest = predict(randomForestModel, newdata = test_data)
rmse(test_data$G3, y_pred_randomForest)

#Variable Importance Plot
varImpPlot(randomForestModel,type=1)

 
#Plotting actual vs predicted values
y_pred_randomForest <- as.data.frame(y_pred_randomForest)
plot(row_number(y_pred_randomForest),y_pred_randomForest['y_pred_randomForest'],xlim=range(c(row_number(y_pred_randomForest),row_number(test_data$G3))),
    ylim=range(y_pred_randomForest['y_pred_randomForest'],test_data$G3),col="red", xlab = "Record Number",
    ylab = "Predicted (Blue) and Actual (Red)", main = "Predicted Grades vs Actual Grades") 
points(row_number(test_data$G3),test_data$G3,col="blue") 
