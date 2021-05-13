#####################################################################################################################################

#Pre-processing
if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", 
                                          repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", 
                                      repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo", 
                                   repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", 
                                         repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", 
                                     repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(ggcorrplot)
library(GGally)
library(zoo)
library(gridExtra)
library(knitr)

data<- read.csv("diabetes.csv")

#####################################################################################################################################

#Data Cleansing
summary(data)

#Converting 0 values to NA
data$Glucose[data$Glucose == 0 ] <- NA
data$BloodPressure[data$BloodPressure == 0 ] <- NA
data$SkinThickness[data$SkinThickness == 0 ] <- NA
data$Insulin[data$Insulin == 0 ] <- NA
data$BMI[data$BMI == 0 ] <- NA

#Converting NA values to mean
data<- na.aggregate(data, FUN=mean)

#Convert outcome to factor
data$Outcome <- as.factor(data$Outcome)
levels(data$Outcome) <- c(FALSE,TRUE)

#####################################################################################################################################

#Exploratory Data Analysis
#Summary
summary(data)
data %>%
  summarize(Nr_Patients = n())

#Correlation
ggcorrplot(cor(data[, 1:8]), 
           method="circle", 
           type = "lower", 
           hc.order = TRUE, 
           lab = TRUE, 
           lab_size = 3, 
           colors = c("red","white", "darkgreen"), 
           title = "Corrlation Plot")

#Clusers
ggpairs(data, columns=1:8, aes(color=Outcome)) + 
       ggtitle("Correlation and Distributions")

#Variances
#Pregnancies Variance
p1<-ggplot(data, aes(x=Outcome, y=Pregnancies)) +
  geom_boxplot()+ 
  labs(title="Pregnancies Variance",
       x="Diabetes Outcome", 
       y = "Pregnancies")

#Glucose Variance
p2<-ggplot(data, aes(x=Outcome, y=Glucose)) +
  geom_boxplot()+ 
  labs(title="Glucose Variance",
       x="Diabetes Outcome", 
       y = "Glucose")

#Blood Pressure Variance
p3<-ggplot(data, aes(x=Outcome, y=BloodPressure)) +
  geom_boxplot()+ 
  labs(title="Blood Pressure Variance",
       x="Diabetes Outcome", 
       y = "Blood Pressure")

#Skin Thickness Variance
p4<-ggplot(data, aes(x=Outcome, y=SkinThickness)) +
  geom_boxplot()+ 
  labs(title="Skin Thickness Variance",
       x="Diabetes Outcome", 
       y = "Skin Thickness")

#Insulin Variance
p5<-ggplot(data, aes(x=Outcome, y=Insulin)) +
  geom_boxplot()+ 
  labs(title="Insulin Variance",
       x="Diabetes Outcome", 
       y = "Insulin")

#BMI Variance
p6<-ggplot(data, aes(x=Outcome, y=BMI)) +
  geom_boxplot()+ 
  labs(title="BMI Variance",
       x="Diabetes Outcome", 
       y = "BMI")

#Diabetes Pedrigree Function Variance
p7<-ggplot(data, aes(x=Outcome, y=DiabetesPedigreeFunction)) +
  geom_boxplot()+ 
  labs(title="Diabetes Pedrigree Function Variance",
       x="Diabetes Outcome", 
       y = "Diabetes Pedigree Function")

#Age Variance
p8<-ggplot(data, aes(x=Outcome, y=Age)) +
  geom_boxplot()+ 
  labs(title="Age Variance",
       x="Diabetes Outcome",
       y = "Age")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol =2)

#####################################################################################################################################

#Model Training
# Splitting dataset into training and test datasets
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = data$Outcome, times = 1,
                                  p = 0.1, list = FALSE)
train_set <- data[-test_index,]
test_set <- data[test_index,]

#####################################################################################################################################

#Baseline
#predicting false outcome by default
y_hat<- rep(FALSE, nrow(test_set))%>% factor() 
#retrieving accuracy from confusion matrix
Baseline_Accuracy<- confusionMatrix(y_hat, test_set$Outcome)$overall[["Accuracy"]]
#retrieving F1 score from confusion matrix
Baseline_F1<- confusionMatrix(y_hat, test_set$Outcome)$byClass[["F1"]] 
#storing accuracy and F1 score
Models_Accuracy <- tibble(Model = "Baseline", 
                          Accuracy = Baseline_Accuracy, 
                          F1 = Baseline_F1) 
kable(Models_Accuracy)

#####################################################################################################################################

#Model 1: Logistic Regression
#training using logistic regression
Model1_glm <- glm(Outcome ~ ., data=train_set, family = "binomial")
#calculating the probabilities using the fitted model and test set
p_hat <- predict(Model1_glm, newdata = test_set, type="response") 
#assigning outcomes based on the probabilities
y_hat <- ifelse(p_hat > 0.5, TRUE, FALSE)%>% factor() 
Model1_Accuracy<- confusionMatrix(y_hat, test_set$Outcome)$overall[["Accuracy"]]
Model1_F1<- confusionMatrix(y_hat, test_set$Outcome)$byClass[["F1"]]
Models_Accuracy   <- rbind(Models_Accuracy, 
                           c("Model 1: Logistic Regression", 
                             Model1_Accuracy, 
                             Model1_F1))
kable(Models_Accuracy)

#####################################################################################################################################

#Model 2: KNN (k-Nearest Neighbours)
#training multiple k-nearest neighbours models by tuning the k paramter (nr neighbours)
Model2_knn <- train(Outcome ~ ., 
                      method = "knn",
                      data = train_set,
                      tuneGrid = expand.grid(k = 10:30))

plot(Model2_knn) #plotting accuracy vs various k values

#applying the model to the test dataset
y_hat <- predict(Model2_knn, newdata = test_set) 
Model2_Accuracy<-confusionMatrix(y_hat, test_set$Outcome)$overall[["Accuracy"]]
Model2_F1<- confusionMatrix(y_hat, test_set$Outcome)$byClass[["F1"]]
Models_Accuracy <- rbind(Models_Accuracy, 
                         c("Model 2: k-Nearest Neighbours", 
                           Model2_Accuracy, 
                           Model2_F1)) 
kable(Models_Accuracy)

#####################################################################################################################################


#Model3: Linear Discriminant Analysis (LDA)
#training using LDA
Model3_lda <- train(Outcome ~ .,method = "lda", data = train_set) 
#applying the model to the test dataset
y_hat <- predict(Model3_lda, newdata = test_set)  
Model3_Accuracy<-confusionMatrix(y_hat, test_set$Outcome)$overall[["Accuracy"]]
Model3_F1<- confusionMatrix(y_hat, test_set$Outcome)$byClass[["F1"]]
Models_Accuracy <- rbind(Models_Accuracy, 
                         c("Model 3: Linear Discriminant Analysis (LDA)", 
                           Model3_Accuracy, 
                           Model3_F1))
kable(Models_Accuracy)

#####################################################################################################################################

#Model 4: Decision Tree
#training multiple decision tree models by tuning the complexity paramter
Model4_rpart <- train(Outcome  ~ ., 
                 method = "rpart",
                 data = train_set,
                 tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25))) 

#plotting accuracy vs various cp values
plot(Model4_rpart) 

#applying the model to the test dataset
y_hat <- predict(Model4_rpart, newdata = test_set)  
Model4_Accuracy<-confusionMatrix(y_hat, test_set$Outcome)$overall[["Accuracy"]]
Model4_F1<- confusionMatrix(y_hat, test_set$Outcome)$byClass[["F1"]]
Models_Accuracy <- rbind(Models_Accuracy,
                         c("Model 4: Decision Tree", 
                           Model4_Accuracy, 
                           Model4_F1))
kable(Models_Accuracy)

#####################################################################################################################################

#Final Model
Model2_Accuracy
Model2_F1
#Variable Importance
plot(varImp(Model2_knn)) 