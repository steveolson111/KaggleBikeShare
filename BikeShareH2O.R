#install.packages("rpart")
library(tidymodels)
library(tidyverse)
library(vroom)
library(rpart)
library(bonsai)
library(lightgbm)


## CV tune, finalize and predict here and save results

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
  step_mutate(hour_sin = sin(2 * pi * hour / 24),   #best result was with /168 here and line below?
              hour_cos = cos(2 * pi * hour / 24))%>%
  step_mutate(hour = as.factor(hour))%>%
  step_rm(datetime)%>%
  step_dummy(all_nominal_predictors()) %>% #create dummy variables
  step_zv(all_predictors()) %>% #removes zero-variance predictors
  step_normalize(temp, atemp, humidity, windspeed)%>%
  step_corr(all_numeric_predictors(), threshold=0.8) # removes > than .8 corr
prepped_recipe <- prep(bike_recipe) # Sets up the preprocessing using myDataSet
baked_dataset <-bake(prepped_recipe, new_data=test)

#boost_model <- boost_tree(tree_depth=tune(),
#trees=tune(),
#learn_rate=tune()) %>%
#set_engine("lightgbm") %>% #or "xgboost" but lightgbm is faster
#set_mode("regression")

library(agua) #Install if necessary
system("java -version")

## Initialize an h2o session
h2o::h2o.init()

## Define the model
## max_runtime_secs = how long to let h2o.ai run
## max_models = how many models to stack
auto_model <- auto_ml() %>%
  set_engine("h2o", max_runtime_secs=60, max_models=5) %>%
  set_mode("regression")

## Combine into Workflow
automl_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(auto_model) %>%
  fit(data=train)

final_model <- fit(automl_wf, data = train)

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
