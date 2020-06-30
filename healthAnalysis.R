
#############################################
##
##    FEATURE EXPLORATION AND MODELING
##
##############################################



# Using the "CareDetails.csv" otained after cleaning the original dataset under the file, healthCleaing.R



# PART 1: FEATURE EXPLORATION
#         Section 1: Exploring Health.Data
#         Section 2: Correlation and feature extraction
#         Section 3: Exploring the extracted features

# PART 2: MODELING
#         Section 1: Creating the train and test sets
#         Section 2: Model Number 1: Classification Tree Model. Accuracy = 46.1%
#         Section 3: Model Number 2: Random Forest Model. Accuracy = 63.41%
#         Section 4: Model Number 3: SVM Model. Accuracy = 65.56%




# PART 1 :  FEATURE EXPLORATION


# Installing libraries

library(ggplot2)
library(corrplot)
library(dplyr)
library(psych)     #for pairs panel correlation
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
library(randomForest)


# Loading in the data 

health.Data <- read.csv("CareDetails.csv")


# Section 1: Exploring Health.Data

summary(health.Data)
str(health.Data)

unique(health.Data$state)  #55 includes 51 states and 
                           #4 Terrotories: Northern Marianas(MP), Guam(GU), Puerto Rico(PR), Virgin Islands(VI)


# Feature, Care.Stars to be used as the target variable

# What is the distribution of the target variable, Patient Care Quality Ratings 
# across all states


ggplot(health.Data, aes(x=Care.Stars)) + 
  geom_histogram(binwidth = 0.50, fill = 'light blue', color = 'black') +
  xlab("Patient Care Quality Rating") + 
  ylab("Frequency") +
  ggtitle("Patient Care Quality Rating\nacross all States") + 
  theme(plot.title = element_text(hjust = .5, face = "bold", size = 14), 
        axis.text.x = element_text(face= "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10), 
        axis.title = element_text(face = "bold", size = 14))




# How many agencies are in each state? 


agency =  health.Data %>% group_by(state) %>% summarize(howmany = n()) %>% arrange(desc(howmany))
agency                      # Maximum 2123 in TX and  Minimum 2 in VI

as.data.frame(agency)       # changing result table to a dataframe





# Plotting a bar chart to show the total number of agencies in each state


agencyPlot <- ggplot(agency, aes(x = state, y = howmany, fill=as.factor(howmany)))
agencyPlot + geom_bar(stat = "identity") +  
  scale_color_manual(breaks = c("<=500", "501<howmany<1000", ">=1000"), 
                     values=c("red", "blue", "green") )+
  xlab("State") + 
  ylab("Home Health Care Agencies") +
  labs(fill = "Agency Count") +
  ggtitle("Home Health Care Agencies per State") + 
  theme(plot.title = element_text(hjust = .5, face = "bold", size = 14), 
        axis.text.x = element_text(face= "bold", size = 10, angle = 90),
        axis.text.y = element_text(face = "bold", size = 10), 
        axis.title = element_text(face = "bold", size = 14))



## Hypothesis: Question, does having less Hospital Admissions corresponds to better care? 
# Implies lower Admit.Hosp = higher Care.Stars?


#  Plot of Admit.Hosp against Care.Stars


# Per the plot below, yes, there is negative correlation though very weak



ggplot(health.Data, aes(Admit.Hosp, Care.Stars, color = Care.Stars)) + 
  geom_point() + geom_smooth() + scale_color_gradient() +
  xlab("Hospital Admissions") + 
  ylab("Patient Care Quality Rating") +
  labs(color = "Quality\nRating") +
  ggtitle("Relationship between Hospital Admissions\nand Patient Care Quality Rating") + 
  theme(plot.title = element_text(hjust = .5, face = "bold", size = 14), 
        axis.text.x = element_text(face= "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10), 
        axis.title = element_text(face = "bold", size = 14))  



# Does having many Agencies means a high Patient Care Quality for the state?

# Selecting the top 10 states with the highest number of home health Agencies


selected_topstates = health.Data %>% group_by(state) %>% 
  summarize(howmany = n()) %>% arrange(desc(howmany)) %>% top_n(10) %>% select(state)

class(selected_topstates)


# Structure is a dataframe...Changing to a vector frame so it can be referenced by using
# as.character to change it

selected_topstates = as.character(selected_topstates$state)


select_topstar_rating <- health.Data %>% filter(state %in% selected_topstates) %>% 
  select(state, Care.Stars) %>% arrange(state)




# Ploting the top 10 states having the highest number of Home Health Agencies 
# with their median Care.Stars

# The plot below reveals a mix of high and lows median ratings

ggplot(select_topstar_rating, aes(x=reorder(state, Care.Stars, median), y = Care.Stars)) + 
  geom_boxplot(aes(fill=state)) +
  xlab("State") + 
  ylab("Patient Care Quality Rating") +
  labs(fill = "State") +
  ggtitle("Patient Care Quality Rating\nfor Top Ten States") +
  theme(plot.title = element_text(hjust = .5, face = "bold", size = 14), 
        axis.text.x = element_text(face= "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10), 
        axis.title = element_text(face = "bold", size = 14))




# Does having fewer Agencies mean a higher Patient Care Quality for the state?


# Selecting the bottom 10 states with the least number of Home Health Agencies

selected_botstates = health.Data %>% group_by(state) %>% 
  summarize(howmany = n()) %>% arrange(desc(howmany)) %>% top_n(-10) %>% select(state)


class(selected_botstates)

# Structure is a dataframe...Changing to a vector frame so it can be referenced by using
# as.character to change it


selected_botstates = as.character(selected_botstates$state)


select_botstar_rating <- health.Data %>% filter(state %in% selected_botstates) %>% 
  select(state, Care.Stars) %>% arrange(state)



# Ploting the bottom 10 states having  the least number of Home Health Agencies 
# with their median Care.Stars

# The plot below reveals a mix of high and lows median ratings

ggplot(select_botstar_rating, aes(x=reorder(state, Care.Stars, median), y = Care.Stars)) + 
  geom_boxplot(aes(fill=state)) + 
  xlab("State") + 
  ylab("Patient Care Quality Rating") +
  labs(fill = "State") +
  ggtitle("Patient Care Quality Rating\nfor bottom Ten States") +
  theme(plot.title = element_text(hjust = .5, face = "bold", size = 14), 
        axis.text.x = element_text(face= "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10), 
        axis.title = element_text(face = "bold", size = 14))







# Section 2: Correlation and Extraction


# Correlation amongst all the features in the health.Data except the target feature

corHealth <- cor(health.Data[2:21])            #Choosing which features to include

corrplot(corHealth, method = "circle")        # Plotting the correlation matrix


# Good correlation for Move.Care, Bed.Move, Bath.Check, 
# Breath.Check, Drugs.Check and Timely.Care with Care.Stars


# Extracting the necessary features from the health.Data based on the correlation

data.Health <- health.Data[,  c(1:3,10:13,15)]
colnames(data.Health)



# Necessary features are:


#[1] "state"        "Care.Stars"   "Timely.Care" 
#[4] "Move.Care"    "Bed.Move"     "Bath.Check"  
#[7] "Breath.Check" "Drugs.Check" 



# Plotting pairwise correlation amongst the features of the subset columns

pairs.panels(data.Health[, -1])





# Section 3: Exploring the Extracted features


# Plotting the Move.Care to see the relationship between Care.Stars and Move.Care

# The plot below shows that the Move.Care is highly correlated to the Care.Stars

ggplot(health.Data, aes(Move.Care, Care.Stars, color = Care.Stars)) +
  geom_point() + geom_smooth() + scale_color_gradient() +
  xlab("Walking - Movement") + 
  ylab("Patient Care Quality Rating") +
  labs(color = "Quality\nRating") +
  ggtitle("Relationship between Walking - Movement\nand Patient Care Quality Rating") + 
  theme(plot.title = element_text(hjust = .5, face = "bold", size = 14), 
        axis.text.x = element_text(face= "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10), 
        axis.title = element_text(face = "bold", size = 14))   






#   PART 2 : MODELING



# Section 1 : Creating the train and test sets



# Model to predict the Patient Care Quality Rating using a classification model

# Changing the target variable, Care.Stars to factor using the "data.Health" dataset

data.Health$Care.Stars <- as.factor(data.Health$Care.Stars)


# Taking out column 1 (State) before modeling

model_data <- data.Health[, -1]



# List of the classification target factor levels by count

table(model_data$Care.Stars)

#1  1.5    2  2.5    3  3.5    4  4.5    5 
#134  740 1286 1718 1943 1944 1529 1221  655 




# Creating train and test sets

set.seed(244)

trainIndex<-createDataPartition(model_data$Care.Stars, p=0.8, list = FALSE)


trainset <- model_data[trainIndex, ]
testset  <- model_data[-trainIndex, ]




# Section 2: Model Number 1: Classification Tree Model


model_rpart <- rpart(Care.Stars ~., data = trainset, method="class")

model_rpart

summary(model_rpart)

# Variable Importance

#Variable importance
#Bath.Check    Move.Care  Drugs.Check Breath.Check 
#23           23           20           15 
#Bed.Move  Timely.Care
#14            5      

rpart.plot(model_rpart, 
           main = "Patient Care Quality Classification Tree", cex.main=1.5,
           type = 5, digits = 3, fallen.leaves = TRUE,
           branch = 0, under = TRUE,
           box.palette = "GnYlRd", extra = 100)






# Predicting the Care.Stars of the test data

pred_rpart <- predict(model_rpart, testset[-1], type = "class")



# Confusion Matrix with Accuracy

confusionMatrix(pred_rpart, as.factor(testset$Care.Stars))  # Accuracy = 46.1%





# Section 3: Model Number 2: Random Forest Model

set.seed(244)

forest <- randomForest(Care.Stars ~., data = trainset)
forest

plot(forest)



# Predicting the Care.Stars of the test data

pred_forest<- predict(forest, testset[-1])


# Confusion Matrix with Accuracy

confusionMatrix(pred_forest, testset$Care.Stars)  # Accuracy = 63.41%




# Section 4: Model Number 3: SVM Model


set.seed(244)


svmModel = svm(formula = Care.Stars ~ ., data = trainset) 

svmModel
summary(svmModel)


# Predicting the Care.Stars of the test data

pred_SVM = predict(svmModel, newdata = testset[-1]) 



# Confusion Matrix with Accuracy

confusionMatrix(pred_SVM, testset$Care.Stars)   # Accuracy = 65.56%


