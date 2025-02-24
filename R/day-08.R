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

covid_w_region <- inner_join(df, covid, by= "state")%>% #innerjoin (df1, df2, join by=)
group_by(region,date)%>% #group by region for each date
summarize(cases = sum(cases),
          deaths = sum(deaths)) %>% #compute the daily counts and cumulative totals for both cases and deaths.
pivot_longer(cols= c(cases, deaths),
             names_to= "type",
             values_to = "count") 



covid_w_region %>% #make facet grid when seeing relationship b/w 2 variables.by region and date
  ggplot(aes(x= date, y= count)) +
  geom_line() +
  facet_grid(type~region, scales="free_y") +
  theme_bw()

ggsave("covid_cases_deaths_by_region.png", width = 12, height = 8, dpi = 300)
  