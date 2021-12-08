library(tidyverse)
library(GGally)
library(patchwork)
library(gganimate)

dat <- read_csv("BioLog_Plate_Data.csv")
?pivot_longer

long_dat <- dat %>% 
  pivot_longer(col=c("Hr_24","Hr_48","Hr_144"),
               names_to = "Time",
               values_to = "Absorbance")
tidy_dat <- long_dat %>% 
  mutate(Type = case_when(`Sample ID`== "Clear_Creek"~"Water",
                          `Sample ID`== "Waste_Water"~"Water",
                          TRUE~"Soil"))

plot_tidy_dat <- tidy_dat %>% 
  filter(Dilution==0.1)
plot_tidy_dat %>% 
  ggplot(aes(x=Time,y=Absorbance,color=Type))+
  geom_point()+
  facet_wrap(~Substrate)

?geom_curve
