library(tidyverse)
library(tidymodels)
library(vroom)
library(glmnet)
library(lmerTest)
library(bonsai)
library(lightgbm)
library(agua)

h2o::h2o.init(max_mem_size = "6G", nthreads = -1)
trainData <- vroom("train.csv")%>%
  mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M"))
testData <- vroom("test.csv") %>%
  mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M"))
head(trainData)

trainDatafortest <- trainData %>%
  select(-casual, -registered) %>%
  mutate(count = log1p(count))
head(trainDatafortest)

## Define recipe
bike_recipe <- recipe(count ~ ., data = trainDatafortest) %>% 
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_date(datetime, features = c("dow", "month", "year")) %>%
  step_time(datetime, features = c("hour")) %>%
  step_mutate(
    is_weekend = ifelse(datetime_dow %in% c("Sat", "Sun"), 1, 0),
    season = factor(season, labels = c("Winter", "Spring", "Summer", "Fall")),
    workingday = factor(workingday),
    holiday = factor(holiday)
  ) %>%
  step_poly(temp, humidity, windspeed, degree = 2) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_rm(datetime)

prepped_recipe <- prep(bike_recipe)
baked_train <- bake(prepped_recipe, new_data=trainDatafortest)
baked_test <- bake(prepped_recipe, new_data = testData)

write.csv(baked_train, "bakedtrain.csv", row.names = FALSE, quote = FALSE)
write.csv(baked_test, "bakedtest.csv", row.names = FALSE, quote = FALSE)

#These were taken to data robot for modeling and bought back as datarobotpreds

machinelearning <- vroom("datarobotpreds2.csv")%>%
  rename(.pred = count_PREDICTION)

#bike_predictions <- predict(bike_workflow,new_data=testData)

#add something to un-log count before the submission
oldtestData <- vroom("test.csv")

kaggle_submission <- machinelearning %>%
  bind_cols(., oldtestData) %>% #Bind predictions with test data3
  select(datetime, .pred) %>% #Just keep datetime and prediction variables4
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)5
  mutate(count=pmax(0, expm1(count))) %>% #pointwise max of (0, prediction)6
  mutate(datetime=as.character(format(datetime)))

#Could not get the format right until the following:
kaggle_submission$datetime <- as.POSIXct(
  kaggle_submission$datetime,
  format = "%m/%d/%Y %H:%M")

kaggle_submission$datetime <- format(
  kaggle_submission$datetime,
  "%Y-%m-%d %H:%M:%S"
)

# Save submission without quotes or row names
write.csv(kaggle_submission, "LinearPreds10.csv", row.names = FALSE, quote = FALSE)
