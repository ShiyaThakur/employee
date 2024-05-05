#data reading
data<- read.csv("C:/Users/Shiya Thakur/OneDrive/Desktop/hr_dataF.csv")
str(data$employee_id)
data$employee_id[data$employee_id==1005] <-"A-103"
View(data)
dim(data)
data1<- read.csv("C:/Users/Shiya Thakur/OneDrive/Desktop/employee4FN.csv")
View(data1)
dim(data1)


#merging both dataset
main_df<-merge(data,data1,by ='employee_id',all.x=T)
show(main_df)
dim(main_df)


#data cleaning
sum(is.na(main_df))
colSums(is.na(main_df))



# Identify outliers TO REMOVE null values of satisfaction
col_name <- "satisfaction_level"
q1 <- quantile(main_df[[col_name]], 0.25,na.rm=TRUE)
q3 <- quantile(main_df[[col_name]], 0.75,na.rm = TRUE)
iqr <- q3 - q1
upper_threshold <- q3 + 1.5 * iqr
lower_threshold <- q1 - 1.5 * iqr
print(upper_threshold)
print(lower_threshold)
outliers <- main_df[[col_name]][main_df[[col_name]] > upper_threshold | main_df[[col_name]] < lower_threshold]
cat("Outliers in column", col_name, ":", sum(outliers), "\n")

#replace 
main_df$satisfaction_level[is.na(main_df$satisfaction_level)] <- mean(main_df$satisfaction_level, na.rm = TRUE) 
main_df

#Indentifying outliers to remove null values of last evaluation
col_name2 <- "last_evaluation"
q1 <- quantile(main_df[[col_name2]], 0.25,na.rm=TRUE)
q3 <- quantile(main_df[[col_name2]], 0.75,na.rm = TRUE)
iqr <- q3 - q1
upper_threshold <- q3 + 1.5 * iqr
lower_threshold <- q1 - 1.5 * iqr
print(upper_threshold)
print(lower_threshold)
outliers <- main_df[[col_name2]][main_df[[col_name2]] > upper_threshold | main_df[[col_name2]] < lower_threshold]
cat("Outliers in column", col_name2, ":", sum(outliers), "\n")
main_df$last_evaluation[is.na(main_df$last_evaluation)] <- mean(main_df$last_evaluation, na.rm = TRUE)

#total null values 
sum(is.na(main_df))


#check for outliers along with summary
summary(main_df)
boxplot((main_df$satisfaction_level[sapply(main_df$satisfaction_level,is.numeric)]))
boxplot((main_df$last_evaluation[sapply(main_df$last_evaluation,is.numeric)]))



#numerical analysis of no of people left
agg_perf = aggregate(satisfaction_level ~ left, data = main_df, mean)
agg_perf

#wrt categorical data analysis
#numerical analysis department wise so here will be investmwnt in each department
library(dplyr)
ds<-main_df %>% group_by(department)  %>%
  summarise(total_project = sum(number_project),total_hours = sum(average_montly_hours),total_company=sum(time_spend_company),total_accident=sum(Work_accident),total_promotion=sum(promotion_last_5years),total_satisfaction=sum(satisfaction_level),total_evaluation=sum(last_evaluation),.groups = 'drop')
View(ds)

#numerical analysis department wise but by using mean
ds1<-main_df %>% group_by(department)  %>%
  summarise(total_project = mean(number_project),total_hours = mean(average_montly_hours),total_company=mean(time_spend_company),total_accident=mean(Work_accident),total_promotion=mean(promotion_last_5years),total_satisfaction=mean(satisfaction_level),total_evaluation=mean(last_evaluation),.groups = 'drop')
View(ds1)

#removing index attribute
main_df1 = subset(main_df, select = -c(employee_id) )
main_df1

#covariance and correlation to find out how variables are inter connected
cor(main_df1[sapply(main_df1,is.numeric)])
library("dplyr")
data_num2 <- select_if(main_df1, is.numeric) 
data_num2
library(corrplot)
M = cor(data_num2)
corrplot(M, method = 'number')
corrplot(M, method = 'color')
#satisfaction level then time_spent_with_company 




#data visualisation
#graphs
library(ggplot2)
#average of satisfaction level leaving job and not leaving
ggplot(agg_perf, aes(x = left, y =satisfaction_level)) + geom_bar(stat ="identity", fill = 'blue', colour = 'black') + ggtitle("Performance v/s Voluntarily L
eaving") + labs(y = "Satisfaction_level", x ="left")
boxplot(satisfaction_level~left,data=main_df,col=c("green","red"))
boxplot(time_spend_company~left,data=main_df,col=c("green","red"))

boxplot(satisfaction_level~salary,data=main_df,col=c("green","red","blue"))
boxplot(average_montly_hours~salary,data=main_df,col=c("green","red","blue"))
# Create a bar plot using the "main_df" data frame
agg_perf2 = aggregate(satisfaction_level ~ promotion_last_5years, data = main_df, mean)
agg_perf2
ggplot(agg_perf2, aes(x = promotion_last_5years, y =satisfaction_level)) + geom_bar(stat ="identity", fill = 'blue', colour = 'black') + ggtitle("Performance v/s Voluntarily L
eaving") + labs(y = "promotion_last_5 year", x ="left")


library(GGally)
ggpairs(main_df1, aes(color = factor(left)))
pairs(main_df)


#data processing
#label encoder

#temp<-main_df
#levels(temp$salary)<-c(levels(temp$salary),1,2,3)
#temp$salary[temp$salary=='low'] <- 1
#temp$salary[temp$salary=='medium'] <-2
#temp$salary[temp$salary=='high'] <-3
#print(main_df)
#head(temp)
#unique(main_df$department)
#temp2<-temp
#levels(temp2$department)<-c(levels(temp2$department),1,2,3,4,5,6,7,8,9,10)
#temp2$department[temp2$department=='sales'] <- 1
#temp2$department[temp2$department=='hr'] <-2
#temp2$department[temp2$department=='technical'] <-3
#temp2$department[temp2$department=='IT'] <-4
#temp2$department[temp2$department=='RandD'] <-5
#temp2$department[temp2$department=='product_mng'] <-6
#temp2$department[temp2$department=='accounting'] <-7
#temp2$department[temp2$department=='support'] <-8
#temp2$department[temp2$department=='management'] <-9
#temp2$department[temp2$department=='marketing'] <-10
#head(temp2)
#show(temp2)


#data preprocessing
library(caTools)
split<- sample.split(main_df1,SplitRatio = 0.8)
split
training_data<-subset(main_df1,split="TRUE")
testing_data<-subset(main_df1,split="FALSE")


#decision tree
library(rpart)
library(rpart.plot)

# Split data into features and target
X <- main_df1[, !names(main_df1) %in% "left"]
y <- main_df1$left
X
y

# Train decision tree classifier
clf <- rpart(y ~ ., data=X)

# Plot the decision tree
rpart.plot(clf)
tree_model <- rpart(left ~ ., data = main_df1)


#Accuracy
set.seed(123)
train_index <- sample(nrow(main_df1), nrow(main_df1) * 0.8)
train_df <- main_df1[train_index, ]
test_df <- main_df1[-train_index, ]
tree_model <- rpart(left ~ ., data = train_df, method = "class")
pred <- predict(tree_model, test_df, type = "class")
accuracyds <- mean(pred == test_df$left)
print(paste("Accuracy:", accuracy*100))
#Prediction
new_data <- data.frame(satisfaction_level = c(0.5, 0.7, 0.8),
                       last_evaluation = c(0.7, 0.8, 0.6),
                       number_project = c(4, 3, 5),
                       average_montly_hours = c(160, 180, 200),
                       time_spend_company = c(3, 4, 2),
                       Work_accident = c(0, 1, 0),
                       promotion_last_5years = c(0, 0, 1),
                       department = c("sales", "hr", "technical"),
                       salary = c("low", "medium", "high"))
new_pred <- predict(tree_model, new_data, type = "class")
print(new_pred)
#in decision tree we do not require scaling as it is based on yes no but i knn we do require




#logistic regression
library(caTools)
split<-sample.split(main_df1,SplitRatio = 0.8)
split
training<-subset(main_df1,split=="TRUE")
testing<-subset(main_df1,split=="FALSE")

model<-glm(left~. ,training,family="binomial")
summary(model)
#star wise significant values #null deviance is the value we will get when all variable value will be zero its deciance from intercept#residual is when we include all our variables
#we cannot directly remove the varables which are not significajnt for that your residual deviance should not increase and aic shoud decrese

model<-glm(left~.-last_evaluation,training,family="binomial")
summary(model)

model<-glm(left~. ,training,family="binomial")
summary(model)

#not valid as both values are increasing
res<-predict(model,testing,type = "response")
print(res)
#probabilties
show(main_df1)
#confusion matrix
table(Actualvalue=testing$left,Predictedvalue=res>0.5)
#to calculate the accuracy
accuracy1=(2138+270)/(2138+164+428+270)
print(accuracy1*100)

data<-data.frame(satisfaction_level =0.38,
                 last_evaluation = 0.53,
                 number_project = 2,
                 average_montly_hours = 157,
                 time_spend_company =3 ,
                 Work_accident = 0,
                 promotion_last_5years = 0,
                 department = "sales",
                 salary ="low")
answer<-predict(model,data,type = "response")
answer




#knn
head(main_df1)
#normalization as the few values are in 1000 while others are in ones twos

normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#is numeric pe hi normalize fun 
#vectors  were converted into data frame
num_cols <- sapply(main_df1[,1:10], is.numeric)
main_df12 <- as.data.frame(lapply(main_df1[,1:10, drop=FALSE][,num_cols], normalize))
head(main_df12)
main_df1[,7:8]
main_df12 <- cbind(main_df1[,7:8], main_df12,stringsAsFactors = FALSE)
head(main_df12)

# Replace string values with integers
main_df12$salary_num <- ifelse(main_df12$salary == "high", 1,
                               ifelse(main_df12$salary == "medium", 2,
                                      ifelse(main_df12$salary == "low", 3, 0)))


unique(main_df12$department)
main_df12$dept_num <- ifelse(main_df12$department == "sales", 1,
                             ifelse(main_df12$department == "hr", 2,
                                    ifelse(main_df12$department == "IT", 4,
                                           ifelse(main_df12$department == "RandD", 5,
                                                  ifelse(main_df12$department == "product_mng", 6,
                                                         ifelse(main_df12$department == "accounting", 7,
                                                                ifelse(main_df12$department == "support", 8,
                                                                       ifelse(main_df12$department == "marketing", 9,
                                                                              ifelse(main_df12$department == "management", 10,
                                                                                     ifelse(main_df12$department == "technical", 3, 0))))))))))


View(main_df12)
main_df12 <- main_df12[,-2]
main_df12 <- main_df12[,-1]

summary(main_df12)
dim(main_df12)
14999*0.8
main_df12[,c(5,7)]
knn_train <-main_df12[1:11999,-5]
knn_test <- main_df12[12000:14999,-5]
str(knn_train)
main_df12[,5]
nrow(knn_train)
#main_df12
knn_train_labels <- main_df12[1:11999,5]
knn_train_labels
knn_test_labels <- main_df12[12000:14999,5]
length(knn_train_labels)
length(knn_test_labels)
library(class)


#graph draw for k vlue and its accuracy
# create a vector to store the accuracy scores
accuracy_scores <- numeric(127)

# iterate over values of k from 1 to 25
for (k in 1:127) {
# fit a KNN model with the current value of k
  knn_model <- knn(train = knn_train, 
                   test = knn_test, 
                   cl = knn_train_labels, 
                   k = k)
# calculate the accuracy score for the current k
accuracy <- sum(knn_model == knn_test_labels) / length(knn_model)
# store the accuracy score in the vector
accuracy_scores[k] <- accuracy
}
accuracy_scores
# plot the accuracy scores vs k values
plot(1:127, accuracy_scores, type = "l", xlab = "k", ylab = "Accuracy")
# create a data frame with k values and accuracy scores
df <- data.frame(k = 1:127, accuracy = accuracy_scores[1:127])
df


# create the plot using ggplot2
library(ggplot2)
ggplot(df, aes(x = k, y = accuracy*100)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 127, 1)) +
  scale_y_continuous(breaks = seq(0, 100, 0.75), expand = c(0, 0.1))+
  labs(x = "k", y = "Accuracy")+
  ggtitle("K value vs Accuracy Score")


#model implementation
knn_test_pred_model <- knn(train =knn_train, test = knn_test, c = knn_train_labels, k = 5)
summary(knn_test_pred_model)


#evaluation table
#accuracy prediction
confusion_matrix <- table(knn_test_labels, knn_test_pred_model)
confusion_matrix
TP <- confusion_matrix[1,1]
TN <- confusion_matrix[2,2]
FP <- confusion_matrix[1,2]
FN <- confusion_matrix[2,1]
accuracy3 <- ((TP+TN)/(TP+TN+FN+FP))*100
print(accuracy3)
precision <- ((TP)/(TP+FP))*100
print(precision) 
sensitivity <- ((TP)/(TP+FN))
print(sensitivity) 
specificity <- (TN / (TN + FP))
print(specificity) 



#relation of algo
algorithms <- c("Decision Tree","logistic regression","KNN")
score <- c(accuracy*100,accuracy1*100,accuracy3)
library(ggplot2)
ggplot(data = data.frame(algorithms, score), aes(x = algorithms, y = score)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  xlab("Algorithms") + ylab("Accuracy Score")



#data collection
#preparing and exploring data
     #normallize the numeric data
     #training nad testing the data
#trainig a model on data
#evaluate the model performance
    #sensitivity
    #precision
    #accuracy
    #specificity
#improve performance by changing k




