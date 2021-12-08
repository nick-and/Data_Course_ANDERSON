library(tidyverse)
library(modelr)
library(easystats)
library(broom)

df <- read_csv("FacultySalaries_1995.csv")


Sdf <- df %>% 
  pivot_longer(cols = ends_with("y"),
               names_to = "Rank",
               values_to = "Salary")
x_axis <- c("Assist","Assoc","Full")

Sdf %>% 
  ggplot(aes(x=Rank,y=Salary,fill=Rank))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60))+
  scale_x_discrete(labels=x_axis)+
  facet_grid(~Tier)

?element_text





tdf <- df %>% 
  pivot_longer(cols = starts_with("Avg"),
               names_to = "Rank",
               values_to = "Salary",
               names_prefix = "Avg")

tidy_df <- tdf %>% 
  pivot_longer(cols = starts_with("N"),
               names_to = "Staff",
               values_to = "Total",
               names_prefix = "Num")

tidy_df %>% 
  ggplot(aes(x=Rank,y=Salary,color=Rank))+
  geom_boxplot()+
  facet_grid(~Tier)



#2

mod <- glm(data = tidy_df,
           formula = Salary ~ State + Tier + Rank)
summary(mod)

mod <- anova(tidy_df(Salary ~ State + Tier + Rank))

aov(data = tidy_df,
    formula = Salary ~ State + Tier + Rank)
anova(mod)


#3

jdf <- read_csv("Juniper_Oils.csv")


tidy_jdf <- jdf %>% 
  pivot_longer(cols = all_of(c("alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene","alpha-cedrene","beta-cedrene","cis-thujopsene","alpha-himachalene","beta-chamigrene","cuparene","compound 1","alpha-chamigrene","widdrol","cedrol","beta-acorenol","alpha-acorenol","gamma-eudesmol","beta-eudesmol","alpha-eudesmol","cedr-8-en-13-ol","cedr-8-en-15-ol","compound 2","thujopsenal")),
               names_to = "ChemicalID",
               values_to = "Concentration")

tidy_jdf %>% 
  ggplot(aes(x=YearsSinceBurn,y=Concentration))+
  geom_smooth()+
  facet_wrap(~ChemicalID,scales = "free")


#4

jmod <- glm(data = tidy_jdf,
            formula = Concentration~YearsSinceBurn*ChemicalID)


anova(jmod)

tjmod <- tidy(jmod)


tjmod %>% 
  filter(p.value < 0.05) %>% 
  mutate(term=str_remove_all(term,"ChemicalID"))


?pivot_longer
?cols
