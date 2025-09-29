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
  #step_interact(~ weather:season)%>%
  step_mutate(wind_temp = windspeed * temp)%>%
  step_poly(humidity, degree = 2)%>%
  #step_interact(~ temp:humidity + windspeed:humidity)%>%
  step_mutate(dow_sin = sin(2 * pi * dow / 7),
              dow_cos = cos(2 * pi * dow / 7))%>%
  step_mutate(newTemp=temp*atemp, difTemp=temp-atemp) %>% #Create 3 new variables
  step_poly(temp, degree = 2)%>%
  step_date(datetime, features="dow") %>% # gets day of week and month and year
  step_time(datetime, features=c("hour", "minute")) %>% #create time variable
  step_mutate(is_rush_hour = as.factor(if_else(hour %in% c(7,8,9,16,17,18), 1, 0)))%>%
  step_mutate(is_work_hour = as.factor(if_else(hour >= 9 & hour < 17, 1, 0)) )%>%
  step_mutate(is_daylight = as.factor(if_else(hour >= 7 & hour <= 19, 1, 0)) )%>%
  step_mutate(hour_of_week_sin = sin(2 * pi * hour_of_week / 168),
              hour_of_week_cos = cos(2 * pi * hour_of_week / 168))%>%
  step_mutate(hour_sin = sin(2 * pi * hour / 24),
              hour_cos = cos(2 * pi * hour / 24))%>%
  step_mutate(hour_sq = hour^2)%>%
  step_mutate(
    is_weekend = as.factor(if_else(dow %in% c(0, 6), 1, 0)),
    is_morning = as.factor(if_else(hour >= 6 & hour < 12, 1, 0)),
    is_evening = as.factor(if_else(hour >= 17 & hour < 21, 1, 0)),
    is_holiday_weekend = as.factor(if_else(holiday == 1 & dow %in% c(0,6), 1, 0)),
    is_near_holiday = as.factor(if_else(lag(holiday, 1, default = 0) == 1 |
                                          lead(holiday, 1, default = 0) == 1, 1, 0)) )%>%
  step_mutate(hour = as.factor(hour))%>%
  step_rm(datetime)%>%
  step_dummy(all_nominal_predictors()) %>% #create dummy variables
  step_zv(all_predictors()) %>% #removes zero-variance predictors
  step_normalize(atemp, windspeed)%>%
  step_corr(all_numeric_predictors(), threshold=0.8) # removes > than .8 corr

#bike_recipe <- bike_recipe %>%
 # step_dummy(all_nominal_predictors()) %>%
  #step_rm(temp_poly_2,difTemp,weather_Cloudy,datetime_dow_Tue,
      #    datetime_dow_Fri,datetime_dow_Sat,is_rush_hour_X1,is_weekend_X1,is_evening_X1,)  # example dummy names to remove

prepped_recipe <- prep(bike_recipe) # Sets up the preprocessing using myDataSet
baked_dataset <-bake(prepped_recipe, new_data=train)
baked_dataset1 <-bake(prepped_recipe, new_data=test)

vroom_write(baked_dataset, file="./TrainDataRobotBikeShare.csv", delim=",")
vroom_write(baked_dataset1, file="./TestDataRobotBikeShare.csv", delim=",")

DataRobotResults <- vroom("C:/Users/Administrator/OneDrive - Brigham Young University/1School/Stat 348/BikeShare/DataRobotPreds.csv")

bike_predictions <- DataRobotResults %>%
  mutate(.pred = expm1(log_count_PREDICTION))
bike_predictions ## Look at the output

kaggle_submission <- bike_predictions %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)6
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write out the file
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")



