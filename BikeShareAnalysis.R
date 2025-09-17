library(tidyverse)
library(tidymodels)
library(vroom)
library(glmnet)

trainData <- vroom("train.csv")
testData <- vroom("test.csv")


trainDatafortest <- trainData %>%
  select(-casual, -registered) %>%
  mutate(count = log(count))

## Define recipe
bike_recipe <- recipe(count ~ ., data = trainDatafortest) %>% 
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>% # replace 4 with 3 in weather 
  step_mutate(weather = factor(weather, levels = c(1, 2, 3))) %>% # make weather a factor  
  step_mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M")) %>% # fix datetime 
  step_time(datetime, features = c("hour")) %>%  # get hour
  step_mutate(season = factor(season,levels = c(1, 2, 3, 4), labels = c("Winter","Spring","Summer","Fall"))) %>% # make season factor
  step_rm(temp) %>% # remove temp
  step_rm(datetime) %>% #remove datetime (non-hour)
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

  #normalize/ encode away categorical variables (step_normalize)
  #keep number of penalties lowwww. like .01

#prepped_recipe <- prep(bike_recipe)
#baked_train <- bake(prepped_recipe, new_data=trainDatafortest)
#head(baked_train)

## Define linear regression model
preg_model <- linear_reg(penalty = 0.02, mixture = 0.15) %>% #mixture(0,1) penalty > 0
  set_engine("glmnet")

## Combine into workflow
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model)%>%
  fit(data= trainDatafortest)

## Fit model on training data
#bike_fit <- fit(bike_workflow, data = trainDatafortest)
bike_predictions <- predict(bike_workflow,new_data=testData)

#add something to un-log count before the submission


kaggle_submission <- bike_predictions %>%
  bind_cols(., testData) %>% #Bind predictions with test data3
  select(datetime, .pred) %>% #Just keep datetime and prediction variables4
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)5
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)6
  mutate(count = exp(bike_predictions$.pred)) %>%
  mutate(datetime=as.character(format(datetime)))

#Could not get the format right until finally I asked Chatgpt and did the following:
kaggle_submission$datetime <- as.POSIXct(
  kaggle_submission$datetime,
  format = "%m/%d/%Y %H:%M")

kaggle_submission$datetime <- format(
  kaggle_submission$datetime,
  "%Y-%m-%d %H:%M:%S"
)

# Save submission without quotes or row names
write.csv(kaggle_submission, "LinearPreds3.csv", row.names = FALSE, quote = FALSE)
