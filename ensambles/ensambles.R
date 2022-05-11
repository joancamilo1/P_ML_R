# Introduction
  
#   Objectives: The goal of this kernel is to find the best approach to identify the quality of the wine. We will go through the basic EDA and visually identify the quality via a 3D interactive plot.
# 
# Moreover, I also applied multiple ML models to make the prediction respectively. Each of the models would have its own strength.
# 
# Rpart can give you a nice decision tree plot so you can see the variable more intuitively.
# 
# Random Forest is the model most of the time you can run directly with minimum amount of tuning.
# 
# xgboost with histogram is expected to produce the better result but needs a bit of tuning.
# 
# lightGBM is expected to produce the best result but needs a bit of tuning.
# 
# svm is an alternative approach and usually give a less correlated result.
# 
# h2o - deeplearning is one of the easiest tool to apply deep learning model. I could potentially use keras but due to the size and the structure of data. I don't believe deep learning model would outperform xgboost in this case.
# 
# Confusion Matrix is used to evaluate the results.
# 
# If you have any question, please leave a comment and if you like the kernel, please give me an upvote~ Thanks!

# Basic Set up{.tabset .tabset-fade .tabset-pills}

## Load Packages



if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, skimr, GGally, plotly, viridis, caret, randomForest, e1071, rpart, xgboost, h2o, corrplot, rpart.plot, corrgram, lightgbm, visNetwork)

install.packages('Ckmeans.1d.dp')
install.packages('sparkline')
library(sparkline)
library(Ckmeans.1d.dp)
## Load Dataset



wine <- read_csv("winequality-red.csv")



# EDA{.tabset .tabset-fade .tabset-pills}

## First Glimpse via skim

# skim would give you the outlook of the dataset, number of observations, number of columns, the range of the variables, number of missing/ unique values, the histogram, etc.



wine %>% skim()



## Second Glimpse via Corrplot

# Corrplot would give you a overview of the correlation between all the variables. It is better to know the relationship in the very beginning of your analysis.



wine %>% cor() %>% corrplot.mixed(upper = "ellipse", tl.cex=.8, tl.pos = 'lt', number.cex = .8)



## Third Glimpse via Corrgram

# First of all, I learned the technique from this kernel, [Intro to Regression and Classification in R](https://www.kaggle.com/meepbobeep/intro-to-regression-and-classification-in-r/code).

# Corrgram seems like to be an alternative way to do corrplot.



wine %>% corrgram(lower.panel=panel.shade, upper.panel=panel.ellipse)



# Preprocess

# Correct column names and Turn quality variable into factor



colnames(wine) <- wine %>% colnames() %>% str_replace_all(" ","_")

wine$quality <- as.factor(wine$quality)



# GGally - ggpairs

# I have had a quick look and found the following variables: residual\_sugar, free\_sulfur\_dioxide, total\_sulfur\_dioxide, and chlorides do not have significant different across different quality.
# 
# Therefore, these variables are not included in the ggpairs model. Further, I found volatile_acidity, sulphates, and alcohol have more significate different across different quality based on the graph below.
# 
# The rest of the variables are showing some difference; however, the difference among different quality is not significant enough to determine all the quality directly by using one variable.
# 
# In the following EDA, I leverage the power of plotly to produce a 3D interactive graph to visually see different quality at different alcohol, volatile acidity, and sulphates levels.
# 
# I am expecting that we will see the trend to some extent but should not be able to directly identify the quality by using the graph.



wine %>%

  mutate(quality = as.factor(quality)) %>%

  select(-c(residual_sugar, free_sulfur_dioxide, total_sulfur_dioxide, chlorides)) %>%

  ggpairs(aes(color = quality, alpha=0.4),

          columns=1:7,

          lower=list(continuous="points"),

          upper=list(continuous="blank"),

          axisLabels="none", switch="both")



## Plotly 2D Interactive Graph



plot_ly(wine, x=~alcohol, y=~volatile_acidity, size = ~sulphates,

       type="scatter",mode="markers", color = ~quality, text=~quality)



# Ployly 3D Interactive Graph

# The graph is consistent with my expectation. The combination of the three variables could help us to some extent but we are still not able to identify the quality clearly. This calls for machine learning.
# 
# I am going to conduct the machine learning part with different models and cross validation to check which model produce the best result.



wine %>%

  plot_ly(x=~alcohol,y=~volatile_acidity,z= ~sulphates, color=~quality, hoverinfo = 'text', colors = viridis(3),

          text = ~paste('Quality:', quality,

                        '<br>Alcohol:', alcohol,

                        '<br>Volatile Acidity:', volatile_acidity,

                        '<br>sulphates:', sulphates)) %>%

  add_markers(opacity = 0.8) %>%

  layout(title = "3D Wine Quality",

         annotations=list(yref='paper',xref="paper",y=1.05,x=1.1, text="quality",showarrow=F),

         scene = list(xaxis = list(title = 'Alcohol'),

                      yaxis = list(title = 'Volatile Acidity'),

                      zaxis = list(title = 'sulphates')))



# Cross Validation Setup



set.seed(1)

inTrain <- createDataPartition(wine$quality, p=.9, list = F)

train <- wine[inTrain,]

valid <- wine[-inTrain,]

rm(inTrain)



# Decision Tree via rpart

# *Explanation of Decision Tree:*

# What is decision tree? Decision tree is a series of True/False questions and by answering these questions, it leads you to the final decision of its prediction.

# As the illustration graph shows below, temperature is predicted leveraging the power of decision tree. Decision tree is also the basic building block of any tree-based model.
# 
# After getting the basic down, let's move to the next step boostrap aggregation (bagging).
  
  # The rpart plot shows alcohol, volatile acidity, and sulphates are important variables to determine the quality, which is consistent with the explorational data analysis.



# rpart

set.seed(1)

rpart_model <- rpart(quality~alcohol+volatile_acidity+citric_acid+
                       density+pH+sulphates, train)

#???rpart.plot(rpart_model)
visTree(rpart_model)

#fancyRpartPlot(rpart_model)

rpart_result <- predict(rpart_model, newdata = valid[,!colnames(valid) %in% c("quality")], type='class')

confusionMatrix(rpart_result, valid$quality)

varImp(rpart_model)

rm(rpart_model, rpart_result)



# Random Forest

# *Explanation of Random Forest:*
  
  # Random Forest is a type of bagging but they are not the same. Decision Tree has its bias due to there is only one tree. A little bit of change in data would lead to a totally different result.

# Bagging would help to alleviate the impact by using many different "bags" of trees and, then make the decision based on each bag's output.
# 
# If the key of Bagging is The wisdom of the crowd, the key of Random Forest is The wisdom of the (random and diverse) crowd.
# 
# Bagging has a single parameter, which is the number of trees but each of the tree is fully grown (unpruned); however, building on bagging trees, random forest added the randomness into the model via random selection of feature using mtry.
# 
# Random forests has 2 parameters:
# 
# 1. The first parameter is the same as bagging (the number of trees)
# 
# 2. The second parameter (unique to randomforests) is mtry which is how many features to search over to find the best feature.
# 
# This parameter is usually 1/3*D for regression and sqrt(D) for classification. thus during tree creation randomly mtry number of features are chosen from all available features and the best feature that splits the data is chosen.
# 
# As the graph below shows, the random forest model is based on many decision trees and each of these decision tree randomly takes a subset of features.
# 
# By doing so, it increases diversity in the forest and leads to a much more robus overall prediction accurarcy than decision tree.
# 
# There are two type of trees, regression tree and classification tree.
# 
# For regression tree, the random forest model takes an average of all the individual decision tree estimates.
# 
# For classification tree, the random forest model would take the class, which have the majority votes.

# Here is the prove. Even without any tuning, random forest produce a much improved result than the decision tree model.
# 
# Advantages:
# 
# 1. RF are much easier to tune than GBM
# 
# 2. RF are harder to overfit than GBM
# 
# Disadvantages:
# 
# 1. It is generally true that a well-tuned GBM can outperform a RF.
# 
# 2. Boosted Trees can be distributed and very fast.



# randomforest

set.seed(1)

rf_model <- randomForest(quality~alcohol+volatile_acidity+citric_acid+

                           density+pH+sulphates,train)

rf_result <- predict(rf_model, newdata = valid[,!colnames(valid) %in% c("quality")])

confusionMatrix(rf_result, valid$quality)



# After reviewing the result, let's look at which variable contributes the most.
# 


varImp(rf_model)

varImpPlot(rf_model)

rm(rf_model, rf_result)



# xgboost with histogram

# *Explanation of eXtreme Gradient Boost:*
#   
#   Let's start with gradient boost. If random forest is The wisdom of the crowd, gradient boost is The wisdom of the next generation.
# 
# Starting with "weak" classifiers, models are building on the top of each other to eventually forming a very strong classifier.
# 
# Performance:
# 
# Boosting    >    Random Forest    >    Bagging    >    Single Tree
# 
# xgboost with a little bit hyper-parameter tuning achieved the better result than random forest and would be the closest one when compared to lightGBM.



# xgboost

data.train <- xgb.DMatrix(data = data.matrix(train[, !colnames(valid) %in% c("quality")]), label = train$quality)

data.valid <- xgb.DMatrix(data = data.matrix(valid[, !colnames(valid) %in% c("quality")]))

parameters <- list(

  # General Parameters

  booster            = "gbtree",

  silent             = 0,

  # Booster Parameters

  eta                = 0.08,

  gamma              = 0.7,

  max_depth          = 8,

  min_child_weight   = 2,

  subsample          = .9,

  colsample_bytree   = .5,

  colsample_bylevel  = 1,

  lambda             = 1,

  alpha              = 0,

  # Task Parameters

  objective          = "multi:softmax",   # default = "reg:linear"

  eval_metric        = "merror",

  num_class          = 7,

  seed               = 1               # reproducability seed

              , tree_method = "hist"

              , grow_policy = "lossguide"

)

xgb_model <- xgb.train(parameters, data.train, nrounds = 100)

xgb_pred <- predict(xgb_model, data.valid)

confusionMatrix(as.factor(xgb_pred+2), valid$quality)



# The we check which factor contribute the most under xgboost model.



xgb.importance(colnames(train[, !colnames(valid) %in% c("quality")]), model = xgb_model)

xgb.imp <- xgb.importance(colnames(train[, !colnames(valid) %in% c("quality")]), model = xgb_model)

xgb.ggplot.importance(importance_matrix = xgb.imp)

rm(xgb_model, xgb_pred, data.train, data.valid, parameters)



# lightGBM

# In this model, I haven't got a chance to tune the model but with a little bit hyper-parameter tuning lightGBM would achieve the best result among the models so far.



data.train <- lgb.Dataset(data = data.matrix(train[, !colnames(valid) %in% c("quality")]), label = train$quality %>% as.numeric() - 1)

params <- list(objective = "multiclass", metric = "multi_error", num_class = 6)

set.seed(1)

model <- lgb.train(params,
                   
                   data.train,
                   
                   100,
                   
                   min_data = 1
                   
                   , learning_rate = 0.06)

result <- predict(model, data.matrix(valid[,colnames(valid)!=c('quality')]))

list <- list()

for (i in 1:nrow(valid)){
  
  max = max(result[(i-1)*6+1], result[(i-1)*6+2], result[(i-1)*6+3], result[(i-1)*6+4], result[(i-1)*6+5], result[(i-1)*6+6])
  
  list[i] <- if_else(max == result[(i-1)*6+6], 6,
                     
                     if_else(max == result[(i-1)*6+5], 5,
                             
                             if_else(max == result[(i-1)*6+4], 4,
                                     
                                     if_else(max == result[(i-1)*6+3], 3,
                                             
                                             if_else(max == result[(i-1)*6+2], 2, 1)))))
  
}

pred <- list %>% as.numeric() - 1

confusionMatrix(as.factor(pred), as.factor(valid$quality %>% as.numeric() - 1))



# The we check which factor contribute the most under lightGBM model.



lgb.importance(model, percentage = TRUE)

tree_imp <- lgb.importance(model, percentage = TRUE)

lgb.plot.importance(tree_imp, measure = "Gain")



# SVM

# SVM provides an alternative solution; however, the result is not outstanding.



# svm

set.seed(1)

svm_model <- svm(quality~alcohol+volatile_acidity+citric_acid+
                   
                   density+pH+sulphates,train)

svm_result <- predict(svm_model, newdata = valid[,!colnames(valid) %in% c("quality")])

confusionMatrix(svm_result, valid$quality)

rm(svm_model, svm_result)



# h2o (deeplearning)

# h2o is one of the easiest tool to apply deep learning model; however, due to the size of the dataset, the deep learning does not outperform the xgboost model.



# h2o

h2o.init()

h2o.train <- as.h2o(train)

h2o.valid <- as.h2o(valid)

h2o.model <- h2o.deeplearning(x = setdiff(names(train), c("quality")),
                              
                              y = "quality",
                              
                              training_frame = h2o.train,
                              
                              # activation = "RectifierWithDropout", # algorithm
                              
                              # input_dropout_ratio = 0.2, # % of inputs dropout
                              
                              # balance_classes = T,
                              
                              # momentum_stable = 0.99,
                              
                              # nesterov_accelerated_gradient = T, # use it for speed
                              
                              epochs = 1000,
                              
                              standardize = TRUE,         # standardize data
                              
                              hidden = c(100, 100),       # 2 layers of 00 nodes each
                              
                              rate = 0.05,                # learning rate
                              
                              seed = 1                # reproducability seed
                              
)

h2o.predictions <- h2o.predict(h2o.model, h2o.valid) %>% as.data.frame()





confusionMatrix(h2o.predictions$predict, valid$quality)

rm(h2o.model, h2o.train, h2o.valid, h2o.predictions)



# Conclusion

# As I expected, xgboost give the best outcome among all the models in this kernel. A better hyper-parameter tuned xgboost model/ lightgbm would potientially produce a better result.
# 
# Additionally, an ensemble model might also potentially give improved result.