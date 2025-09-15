library(tidyverse)
library(tidymodels)
library(vroom)
#install.packages('GGally')
library(GGally)

train <- vroom("C:/Users/Administrator/OneDrive - Brigham Young University/1School/Stat 348/BikeShare/train.csv")
test <- vroom("C:/Users/Administrator/OneDrive - Brigham Young University/1School/Stat 348/BikeShare/test.csv")
glimpse(train)
#ggpairs(train) # Creates awesome correlation graphs for EDA!

#Recognize something about weather...

#4 Plot HW
library(patchwork)
plot1 <- ggplot(data=train, mapping=aes(x=registered, y=count)) + 
  geom_point()+ geom_smooth(se=FALSE)+ labs(title="Bike Usage 
  & Registered Users",
                                            x="Number of Registered Users", y="Total Bike Count")
plot2 <- ggplot(data=train, mapping=aes(x = factor(weather,
                                                   levels = c(4, 3, 2, 1), labels = c("Heavy Precip", "Light 
  Precip", "Cloudy", "Clear" )), y=count)) +
  geom_bar(stat="identity", fill="steelblue") + 
  labs(title = "Bike Usage by Weather Type", x="Weather Type",
       y="Total Bike Count")
plot2
plot3 <- ggplot(data=train, mapping=aes(x = factor(workingday,
                                                   levels = c(0, 1), labels = c("No", "Yes")), y=count)) + 
  geom_bar(stat="identity", fill="steelblue")+ labs(title="Working Day Usage",
                                                    x= "Working Day", y="Total Bike Count")
plot4 <- ggplot(data=train, mapping=aes(x=atemp, y=count)) + 
  geom_point()+ geom_smooth(se=FALSE)+ labs(title="Bike Usage based on 'Feels-like' Temperature",
                                            x="'Feels-like' Temperature (C)", y="Total Bike Count")

(plot1 + plot2) / (plot3 + plot4) #4 panel plot

# Logistic Regression analysis to get slightly more accurate than just linear
train <- train %>% select(-casual, -registered) %>%
  mutate(log_count = log1p(count)) %>% 
  select(-count)

## Workflows Homework
library(tidymodels)
bike_recipe <- recipe(log_count~., data=train) %>% # Set model formula and dataset
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Mutate for just 3 categories
  step_mutate(weather=factor(weather, levels= c(1,2,3), labels=c("Clear", "Cloudy", "Severe"))) %>% #Make something a factor
  step_mutate(weather=factor(season, levels= c(1,2,3,4), labels=c("Spring", "Summer", "Fall", "Winter"))) %>% #Make something a factor
  step_mutate(newTemp=temp*atemp) %>% #Create a new variable
  step_date(datetime, features="dow") %>% # gets day of week
  step_time(datetime, features=c("hour", "minute")) %>% #create time variable
  step_dummy(all_nominal_predictors()) %>% #create dummy variables
  step_zv(all_predictors()) %>% #removes zero-variance predictors
  step_corr(all_numeric_predictors(), threshold=0.5)  # removes > than .5 corr
prepped_recipe <- prep(bike_recipe) # Sets up the preprocessing using myDataSet
bake(prepped_recipe, new_data=test)

## Setup and Fit the Linear Regression Model
my_linear_model <- linear_reg() %>% #Type of model
  set_engine("lm") %>% # Engine = What R function to use
  set_mode("regression") # Regression just means quantitative response
## Generate Predictions Using Linear Model
#bike_predictions <- predict(my_linear_model,
#new_data=test)# Use fit to predict

## Combine into a Workflow and fit
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(my_linear_model) %>%
  fit(data=train)
#fit(formula=log_count~., data=train) 

## Run all the steps on test data
lin_preds <- predict(bike_workflow, new_data = test)

bike_predictions <- lin_preds %>%
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
