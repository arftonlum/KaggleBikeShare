library(tidyverse)
library(tidymodels)
library(vroom)

trainData <- vroom("train.csv")
testData <- vroom("test.csv")

#only use predictors that show up in test data
trainDatafortest <- trainData %>%
  select(-datetime, -casual, -registered)

#Create a linear Model
my_linear_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") %>%
  fit(formula = count~., data = trainDatafortest)

#Use model to make predictions
bike_predictions <- predict(my_linear_model,new_data=testData)


kaggle_submission <- bike_predictions %>%
bind_cols(., testData) %>% #Bind predictions with test data3
  select(datetime, .pred) %>% #Just keep datetime and prediction variables4
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)5
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)6
  mutate(datetime=as.character(format(datetime))) #needed

vroom_write(x=kaggle_submission, file="./LinearPreds.csv",delim=",")
