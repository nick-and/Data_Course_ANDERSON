library(tidyverse)
test_file <- list.files(pattern = ".csv")
df <- read_csv("cleaned_covid_data.csv")

A_states <- df %>% filter(grepl("^A",Province_State))

A_states %>% 
  ggplot(aes(x=Last_Update,y=Active))+
  geom_point()+
  geom_smooth(se=FALSE, method = "loess")+
  facet_wrap(~Province_State,)

#state_max_fatality_rate
df %>% 
  group_by(Province_State) %>% 
  summarize(Max_Fatality = max(Case_Fatality_Ratio,na.rm = TRUE)) %>% 
  arrange(desc(Max_Fatality))


#5
state_max_fatality_rate %>% 
  ggplot(aes(x=reorder(Province_State,-Max_Fatality),y=Max_Fatality))+
  geom_col()+
  theme(axis.text.x=elememt_text(angle = 90,hjust =1))


#Bonus
library(dplyr)
Summary_deaths <- df %>%
  group_by(Province_State) %>% 
  summarize(MaxDeaths = sum(Deaths)) %>% 
  ggplot(aes(x=Last_Update,y=Deaths))+
  geom_point()+
  
df %>% 
  group_by(Last_Update) %>% 
  summarize(CUmulative = sum(Deaths)) %>% 
  ggplot(aes(x=Last_Update,y=Cumulative))+
  geom_col()
  
options(scipen = 999) #to change default numbers.
    
  
  
  
