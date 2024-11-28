##Libraries required
library(tidymodels)

#####Pre-process######
##Data sampling
iris_split <- initial_split(iris, prop = 0.6)
iris_split

#View the data
iris_split %>%
  training() %>%
  glimpse()

##Creating the recipe
# 1. recipe() - Starts a new set of transformations to be applied, similar to the ggplot() command. Its main argument is the model's formula.

# 2. prep() - Executes the transformations on top of the data that is supplied (typically, the training data).

iris_recipe <- training(iris_split) %>%
  recipe(Species ~.) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

iris_recipe

##Baking the testing data
iris_testing <- iris_recipe %>%
  bake(testing(iris_split))
glimpse(iris_testing)

##Juice out the training data
iris_training <- juice(iris_recipe)

glimpse(iris_training)

##Creating the model
iris_ranger <- rand_forest(trees = 500, mode = "classification") %>%
  set_engine("randomForest") %>%
  fit(Species ~ ., data = iris_training)

##Predicting
predict(iris_ranger, iris_testing)

##Adding the predictions to the baked data
iris_ranger %>%
  predict(iris_testing) %>%
  bind_cols(iris_testing) %>%
  glimpse()

##Metric validation
iris_ranger %>%
  predict(iris_testing) %>%
  bind_cols(iris_testing) %>%
  metrics(truth = Species, estimate = .pred_class)

