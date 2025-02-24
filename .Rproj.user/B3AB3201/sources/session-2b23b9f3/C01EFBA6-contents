#Name: Megan Hoover
#Date: 2/18/25
#daily excercise 7

#read in the COVID-19 data from the URL 
library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid = read_csv(url)
head(covid, 5)

##Make a faceted line plot (geom_line) of the 6** states with most cases. Your X axis should be the date and the y axis cases.**
#step 1. Identify the six states with the most current cases (yesterdays assignment + dplyr::pull)
#step 2. Filter the raw data to those 6 states (hint: %in%)
#step 3. Set up a ggplot –> add layers –> add labels –> add a facet –> add a theme
#step4. save the image to you img directory (hint: ggsave())

top_6_states <- covid %>% #taking covid data and filtering for the max date
  filter(date==max(date)) %>%
  arrange(desc(cases)) %>% #arranging from most to least cases
  slice_head(n = 6) %>% #showing the top 6 rows
  pull(state) #pulling the state names from those rows

covid_data_filtered <- covid %>%   #taking data from the states that= the anmes of the 6 states
  filter(state %in% top_6_states)


ggplot(covid_data_filtered, aes(x = date, y = cases, color = state)) +
  geom_smooth(method = "loess", se = FALSE) +  # Adds a smooth trend line (loess method)
  labs(title = "Trend of COVID-19 Cases in the Top 6 States",
       x = "Date",
       y = "Number of Cases") +
  facet_wrap(~ state, scales = "free_y") +  # Facet by state, with free y-scales
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis

##Make a column plot (geom_col) of daily total cases in the USA. Your X axis should be the date and the y axis cases.
#We can break this task into 3 steps:
#1.Identify the total cases each day in the whole country (hint: group_by(date))
#2. Set up a ggplot –> add layers –> add labels –> add a theme
#3. Save the image to your img directory (hint: ggsave())

covid$date <- as.Date(covid$date)  #change the date format to make easier

# Group by date and calculate the total cases for each day
daily_total_cases <- covid %>%
  group_by(date) %>%  # Group by date, allows to group cases by day
  summarize(total_cases = sum(cases, na.rm = TRUE)) #This sums up the cases column for each date

ggplot(daily_total_cases, aes(x = date, y = total_cases)) +
  geom_col(fill = "red",alpha=.5) + 
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1) +# Create the column plot with skyblue bars
  labs(title = "Daily Total COVID-19 Cases in the USA",
       x = "Date",
       y = "Total Cases") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity

ggsave("imgs/daily_total_cases_with_trend.png", width = 10, height = 6)
