##part 1 (from daily assignment 15)

## 1. Load tidymodels and penguins data set.
#install penguins package if don't already have-->install.packages("palmerpenguins")

library(tidymodels)
library(tidyverse)
library(palmerpenguins)
library(ranger)
library(workflowsets)


## 2.set a seed for reproducible data
#load datast and remove na, set seed
penguins<-
  na.omit(palmerpenguins::penguins)
set.seed(123)


## 3. split the Palmer Penguins dataset into training and testing sets using a 70/30 split
#using function initial_split()
penguins<-drop_na(penguins)
resample_split<-initial_split(penguins, prop= .7, strata =species) #added strata part b/c Alan said to


## 4. Extract the training and test tibbles into unique objects
train_data <- training(resample_split)
test_data <- testing(resample_split)


## 5. Create a 10 fold cross validation dataset based on the training data
penguin_folds <- vfold_cv(train_data, v = 10)


## Part 2 (daily assignment 16)

#define multinomial logistic regression model, there are 3 species types
multinom_model <-
  multinom_reg() %>% #the formula part
  set_engine ("nnet") %>%
  set_mode("classification")
  
#define random forest model
rand_forest_model <-
  rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("classification")

#make workflow set to compare both the random forest and multinomial logistic regress model
#want to do the same thing to both models to directly compare output
penguins_wf_set <-
  workflow_set(                  #inputs need to be in format of list
    preproc =list(species ~ .),   #predicting species according to with the ~ and . means based on everything else
    models =list(multinom=         #take 2 models
    multinom_model, rf =
    rand_forest_model)
  )

#fit both models with 10-fold cross validation
#fit resamples

penguins_resample <- penguins_wf_set %>%
  workflow_map("fit_resamples",
               resamples= penguin_folds, control =
                 control_resamples(save_pred = TRUE))

#metrics
penguins_res_metrics <- 
  collect_metrics(penguins_resample)

#compare
accuracy_comparison <-
  penguins_res_metrics %>%
  filter(.metric == "accuracy")

#print comparison
print(accuracy_comparison)

#As a comment, write a sentence about what model you think is best!

# I think the rand_forest model is better because it has as slighter higher average accuracy across all resampling folds (mean) and a slightly lower variation (std)
