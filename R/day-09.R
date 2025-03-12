
#problem: #Fit a linear model to the cleaned data to predict Ozone from one of the possible predictors of your choosing. Why did you chose thats variable?
#ozone is one predictor variable, need to pick a different one to preict ozone predictor variable
airquality1=airquality  #load air quality data from r. 
?airquality

library(tidyverse)
install.packages("visdat")
library(visdat)
vis_dat(airquality1)
vis_miss(airquality1)
#This data has missing integer values in the ozone and solar R columns


airquality1<- airquality1 %>% #remove na values
  drop_na()

vis_miss(airquality1)  #now all data is present

#formula for linear model lm(y ~ x, data= data set) Choosing solar R bc can see in the table as it goes up, so does the ozone.
airquality_lm<- lm(Ozone ~ Solar.R, data= airquality1)

summary(airquality_lm) #shows statistic summary. r^2 is .5, which shows that there's 50% variance in dependent variable

#The r^2 value of .5 indicates the ozone can be predicted given the value of the solar radiations


#Use broom::augment to predict the Ozone of the cleaned data
library(broom)
augmented_covid <- augment(airquality_lm, data = airquality1)


# Create a ggplot of actual vs predicted Ozone
ggplot(augmented_covid, aes(x = .fitted, y = Ozone)) +
  geom_point(color = "blue") +             # Actual vs predicted points
  geom_abline(intercept = 0, slope = 1,   # Line y = x for comparison
              color = "red", linetype = "dashed") +  # Reference line
  labs(title = "Actual vs Predicted Ozone",
       x = "Predicted Ozone",
       y = "Actual Ozone") +
  theme_minimal()

#Add a red line to show where the actual and predicted values are equal This can be done by plotting a 1:1 line (e.g. intercept 0, slope 1) with geom_abline(intercept = 0, slope = 1, color = "red")
#Add a subtitle to the plot showing the correlation between the actual and predicted values are equal This can be done by plotting a 1:1 line (e.g. intercept 0, slope 1) with
#paste("Correlation:", round(cor(a$Ozone, a$.fitted),2)) assuming your augmented data object is called a
correlation_value <- round(cor(augmented_covid$Ozone, augmented_covid$.fitted), 2)

# Create a ggplot of actual vs predicted Ozone with a subtitle showing the correlation
ggplot(augmented_covid, aes(x = .fitted, y = Ozone)) +
  geom_point(color = "blue") +             # Actual vs predicted points
  geom_abline(intercept = 0, slope = 1,   # Line y = x for comparison
              color = "red", linetype = "dashed") +  # Reference line
  labs(title = "Actual vs Predicted Ozone",
       subtitle = paste("Correlation:", correlation_value),
       x = "Predicted Ozone",
       y = "Actual Ozone") +
  theme_minimal()