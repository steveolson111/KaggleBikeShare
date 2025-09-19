install.packages("rpart")
library(tidymodels)
library(tidyverse)
library(vroom)
library(rpart)

train <- vroom("C:/Users/Administrator/OneDrive - Brigham Young University/1School/Stat 348/BikeShare/train.csv")
test <- vroom("C:/Users/Administrator/OneDrive - Brigham Young University/1School/Stat 348/BikeShare/test.csv")
glimpse(train)

train <- train %>% select(-casual, -registered) %>%
  mutate(log_count = log1p(count)) %>% 
  select(-count)

train <- train %>% mutate(dow = wday(datetime, week_start = 1) - 1,  # 0 = Monday
                          hour = hour(datetime),
                          hour_of_week = dow * 24 + hour)
test <- test %>% mutate( dow = lubridate::wday(datetime, week_start = 1) - 1,
                         hour = lubridate::hour(datetime),
                         hour_of_week = dow * 24 + hour)
train <- train %>%
  mutate(hour_category = case_when(
    hour >= 6 & hour < 10 ~ "morning_commute",
    hour >= 10 & hour < 16 ~ "midday",
    hour >= 16 & hour < 20 ~ "evening_commute",
    TRUE ~ "night"
  ))

test <- test %>%
  mutate(hour_category = case_when(
    hour >= 6 & hour < 10 ~ "morning_commute",
    hour >= 10 & hour < 16 ~ "midday",
    hour >= 16 & hour < 20 ~ "evening_commute",
    TRUE ~ "night"))

## Create a workflow with recipe & model
bike_recipe <- recipe(log_count~., data=train) %>% # Set model formula and dataset
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Mutate for just 3 categories
  step_mutate(weather=factor(weather, levels= c(1,2,3), labels=c("Clear", "Cloudy", "Severe"))) %>% #Make something a factor
  step_mutate(season=factor(season, levels= c(1,2,3,4), labels=c("Spring", "Summer", "Fall", "Winter"))) %>% #Make something a factor
  step_mutate(newTemp=temp*atemp, difTemp=temp-atemp) %>% #Create 3 new variables
  step_date(datetime, features="dow") %>% # gets day of week and month and year
  step_time(datetime, features=c("hour", "minute")) %>% #create time variable
  step_mutate(hour_of_week_sin = sin(2 * pi * hour_of_week / 168),
              hour_of_week_cos = cos(2 * pi * hour_of_week / 168))%>%
  step_mutate(hour_sin = sin(2 * pi * hour / 168),
              hour_cos = cos(2 * pi * hour / 168))%>%
  step_rm(datetime)%>%
  step_dummy(all_nominal_predictors()) %>% #create dummy variables
  step_zv(all_predictors()) %>% #removes zero-variance predictors
  step_normalize(temp, atemp, humidity, windspeed)%>%
  step_corr(all_numeric_predictors(), threshold=0.8) # removes > than .8 corr
prepped_recipe <- prep(bike_recipe) # Sets up the preprocessing using myDataSet
baked_dataset <-bake(prepped_recipe, new_data=test)

my_mod <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n=tune()) %>% #Type of model
  set_engine("rpart") %>% # What R function to use
  set_mode("regression")

## Combine into a Workflow and fit
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(my_mod)


## Set up grid of tuning values for decision tree
tree_grid <- grid_regular(
  tree_depth(range = c(2, 15)),
  cost_complexity(range = c(-5, -1)),  # log10 scale: 10^-5 to 10^-1
  min_n(range = c(2, 20)),
  levels = 5 # how many values to try per parameter
)
## Set up K-fold CV
# Tune the workflow
set.seed(123)
folds <- vfold_cv(train, v = 5)

tuned_results <- tune_grid(
  bike_workflow,
  resamples = folds,
  grid = tree_grid,
  metrics = metric_set(rmse, rsq))

## Find best tuning parameters
best_params <- select_best(tuned_results, "rmse")
#best_params

## Finalize workflow and predict
final_wf <- finalize_workflow(
  bike_workflow,
  best_params)

final_model <- fit(final_wf, data = train)

tree_preds <- predict(final_model, new_data = test)
## Finalize workflow and predict
bike_predictions <- tree_preds %>%
  mutate(.pred = expm1(.pred))
bike_predictions ## Look at the output

## Format the Predictions for Submission to Kaggle
kaggle_submission <- bike_predictions %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)6
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write out the file
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")
