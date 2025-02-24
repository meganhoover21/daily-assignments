#Megan Hoover
#2-23-2025
#Practice Joins and Pivots


#Question 1: Make a faceted plot of the cumulative cases & deaths by USA region. 
#Your x axis should be the date and the y axis value/count.
#To do this you will need to join and pivot the COVID-19 data.

library(tidyverse)
url= 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid = read_csv(url) #read in full COVID data

df=data.frame(region=state.region, #create new data frame using r objects
              state=state.name,
              abbrev.=state.abb)

#join your new data.frame to the raw COVID data.  
#join df to the covid d.f. using the state column as the key. This will add region information to each row of the dataset.

covid_w_region <- merge(covid, df, by = "state", all.x = TRUE)

#compute the daily counts and cumulative totals for both cases and deaths.
#We will do this for each region.
library(dplyr)
# Arrange data by state and date
covid_w_region <- covid_w_region %>%
  arrange(state, date)

covid_w_region <- covid_w_region %>%
  group_by(region, state) %>%
  
  mutate(                                         #add new columns w/ calculations
    daily_cases = cases - lag(cases, default = 0), #This gives the value of cases from the previous row (the previous day). If it's the first row (the first date), the default = 0 argument means that it will return 0 because there is no previous row.
    daily_deaths = deaths - lag(deaths, default = 0),
    cumulative_cases = cumsum(daily_cases),
    cumulative_deaths = cumsum(daily_deaths)
  ) %>%
  ungroup()


#To make the data suitable for plotting with ggplot2, we need to reshape it from a wide format (with separate columns for cases, deaths, etc.) to a long format.
library(tidyr)

# Pivot data into long format

covid_deaths_data <- covid_w_region %>%
  select(date, state, region, deaths, cumulative_cases)

# Step 2: Pivot the data to long format
covid_deaths_long <- covid_deaths_data %>%
  pivot_longer(cols = c(deaths, cumulative_cases), 
               names_to = "metric", 
               values_to = "count")



#The facet_wrap() function allows us to create a separate plot for each region.
library(ggplot2)

ggplot(covid_deaths_long, aes(x = date, y = count, color = metric)) +
  geom_line() +  # Line plot
  facet_wrap(~ region, scales = "free_y") +  # Facet by region
  labs(title = "COVID-19 Deaths and Cumulative Deaths by Region",
       x = "Date",
       y = "Count",
       color = "Metric") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave("covid_cases_deaths_by_region.png", width = 12, height = 8, dpi = 300)
  