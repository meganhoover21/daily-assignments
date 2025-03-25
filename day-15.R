## 1. Load tidymodels and penguins data set.
#install penguins package if don't already have-->install.packages("palmerpenguins")

library(tidymodels)
library(tidyverse)
library(palmerpenguins)
penguins<-penguins_raw


## 2.set a seed for reproducible data
set.seed(123)


## 3. split the Palmer Penguins dataset into training and testing sets using a 70/30 split
#using function initial_split()
penguins<-drop_na(penguins)
resample_split<-initial_split(penguins, prop= .7)


## 4. Extract the training and test tibbles into unique objects
train_data <- training(resample_split)
test_data <- testing(resample_split)


## 5. Create a 10 fold cross validation dataset based on the training data
penguin_folds <- vfold_cv(train_data, v = 10)
