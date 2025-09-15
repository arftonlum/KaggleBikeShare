library(tidyverse)
library(tidymodels)
library(vroom)

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
  step_rm(datetime) #remove datetime (non-hour)

prepped_recipe <- prep(bike_recipe)
baked_train <- bake(prepped_recipe, new_data=trainDatafortest)
head(baked_train)

## Define linear regression model
lin_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

## Combine into workflow
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model)

## Fit model on training data
bike_fit <- fit(bike_workflow, data = trainDatafortest)
bike_predictions <- predict(bike_fit,new_data=testData)




kaggle_submission <- bike_predictions %>%
  bind_cols(., testData) %>% #Bind predictions with test data3
  select(datetime, .pred) %>% #Just keep datetime and prediction variables4
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)5
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)6
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
write.csv(kaggle_submission, "LinearPreds2.csv", row.names = FALSE, quote = FALSE)
