library(tidyverse)
library(ggplot2)
library(patchwork)
land_data <- read_csv("landdata-states.csv")
options(scipen = 999)

#``
land_data %>% 
  ggplot(aes(x=Year,y=Land.Value,color=region))+
  geom_smooth()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "light gray"))+
  labs(x="Year",
       y="Land Value (USD)")
#2 NA Data

NA_states <- land_data %>% 
  filter(is.na(region))

#3 Tidy Mortality Rate Fig. 2

mortality_rates <- read_csv("unicef-u5mr.csv")

tidy_mr <- mortality_rates %>% 
  pivot_longer(cols = c(2:67),
               names_to = "Year",
               values_to = "Mortality Rate")

  


#4 Mortality Rate fig. 2

tidy_mr %>% 
  filter(!is.na(`Mortality Rate`)) %>% 
  ggplot(aes(x=Year,y=`Mortality Rate`,color=Continent))+
  geom_point(size=2)+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "gray"))+
  scale_x_discrete(breaks=c("U5MR.1960","U5MR.1980","U5MR.2000"),
                   labels=c("1960","1980","2000"))




tidy_mr_na <- tidy_mr %>% 
  filter(!is.na(`Mortality Rate`))



fig3 <- tidy_mr_na %>% 
  group_by(Continent,Year) %>% 
  mutate(Mean_Mortality = mean(`Mortality Rate`,na.rm = TRUE))


fig3 %>% 
  group_by(CountryName,Year) %>% 
  ggplot(aes(x=Year,y=Mean_Mortality,color=Continent))+
  geom_point()+
  geom_line(aes(color=Continent))+
  labs(y="Mean Mortality Rate (deaths per 1000 live births")+
  scale_x_discrete(breaks=c("U5MR.1960","U5MR.1980","U5MR.2000"),
                   labels=c("1960","1980","2000"))



new_col <- aggregate(`Mortality Rate`~CountryName,tidy_mr_na,mean)


tidy_mr %>% 
  ggplot(aes(x=Year,y=`Mortality Rate`))+
  geom_point(color="blue",size=.5,shape=1,stroke=1)+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "light gray"),
        axis.text.x = element_text(size = 7),
        strip.background = element_rect(fill = "White"))+
  scale_y_continuous(labels = c)+
  scale_x_discrete(breaks=c("U5MR.1960","U5MR.1980","U5MR.2000"),
                   labels=c("1960","1980","2000"))+
  facet_wrap(~Region)

             