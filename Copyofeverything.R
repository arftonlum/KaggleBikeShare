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


#normalize/ encode away categorical variables (step_normalize)
#keep number of penalties lowwww. like .01

prepped_recipe <- prep(bike_recipe)
baked_train <- bake(prepped_recipe, new_data=trainDatafortest)
baked_test <- bake(prepped_recipe, new_data = testData)

write.csv(baked_train, "bakedtrain.csv", row.names = FALSE, quote = FALSE)
write.csv(baked_test, "bakedtest.csv", row.names = FALSE, quote = FALSE)

## Define linear regression model
#was code for penalized regression model
#preg_model <- linear_reg(penalty = tune(), 
#mixture = tune()) %>% #mixture(0,1) penalty > 0
#set_engine("glmnet")

#now with a regression tree
#now it is a random forest
#my_mod <- rand_forest(mtry = tune(),
#                    min_n=tune(),
#                   trees=500) %>%
#set_engine("ranger") %>%
#set_mode("regression")

#bart model
#bart_model <- bart(trees = tune())%>%
#set_engine("dbarts")%>%
#set_mode("regression")

#Auto Model
auto_model <- auto_ml() %>%
  set_engine("h2o", max_runtime_secs=600,max_models=30)%>%
  set_mode("regression")

## Combine into workflow
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(auto_model)%>%
  fit(data= trainDatafortest)

#code from board: mygrid <- grid_regular(mtry(ranger(1,maxNumXs)),)
L <- 5
K <-3
#grid of values to tune over
grid_of_tuning_params <- grid_regular(trees(range = c(50,500)),
                                      levels = L
)

#split data for CV
folds <- vfold_cv(trainDatafortest, v = K, repeats = 1)

#run the CV
CV_results <- bike_workflow %>%
  tune_grid(resamples = folds,
            grid = grid_of_tuning_params,
            metrics = metric_set(rmse,mae))

#plot results
collect_metrics(CV_results)%>%
  filter(.metric == "rmse")%>%
  ggplot(data=., aes(x=penalty,y=mean,color=factor(mixture))) +
  geom_line()

#find best tuning parameter
bestTune <- CV_results %>%
  select_best(metric = "rmse")

final_wf <-bike_workflow%>%
  finalize_workflow(bestTune)%>%
  fit(data = trainDatafortest)

bike_predictions <- predict(bike_workflow, new_data = testData)

machinelearning <- vroom("datarobotpreds2.csv")%>%
  rename(.pred = count_PREDICTION)

#bike_predictions <- predict(bike_workflow,new_data=testData)

#add something to un-log count before the submission
oldtestData <- vroom("test.csv")
head(oldtestData)
kaggle_submission <- machinelearning %>%
  bind_cols(., oldtestData) %>% #Bind predictions with test data3
  select(datetime, .pred) %>% #Just keep datetime and prediction variables4
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)5
  mutate(count=pmax(0, expm1(count))) %>% #pointwise max of (0, prediction)6
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
write.csv(kaggle_submission, "LinearPreds10.csv", row.names = FALSE, quote = FALSE)
