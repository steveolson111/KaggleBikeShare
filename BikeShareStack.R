## Libraries
library(agua) #Install if necessary
system("java -version")
R.version$arch

## Initialize an h2o session
h2o::h2o.init()

## Define the model
## max_runtime_secs = how long to let h2o.ai run
## max_models = how many models to stack
auto_model <- auto_ml() %>%
set_engine("h2o", max_runtime_secs=, max_models=) %>%
set_mode("regression")

## Combine into Workflow
automl_wf <- workflow() %>%
add_recipe(your_bike_recipe) %>%
add_model(auto_model) %>%
fit(data=full_train_set)
