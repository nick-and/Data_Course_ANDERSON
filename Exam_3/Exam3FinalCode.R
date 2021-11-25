library(ggplot2)
library(tidyverse)
library(modelr)
library(easystats)

df <- read_csv("FacultySalaries_1995.csv")

names(df)
#1

ASalary <- df %>% 
  select("FedID","UnivName","State","Tier",ends_with("y"),
         "AvgProfSalaryAll") %>% 
  pivot_longer(cols = ends_with("y"),
               names_to = "Rank",
               values_to = "Salary",
               names_prefix = "Avg")

AVGSalary <- filter(ASalary, !Tier %in% c("VIIB"))




x_axis <- c("Assist","Assoc","Full")
AVGSalary %>% 
  ggplot(aes(x=Rank,y=Salary,fill=Rank))+
  geom_boxplot()+
  theme_minimal()+
  scale_x_discrete(labels=x_axis)+
  scale_fill_discrete(labels=x_axis)+
  theme(axis.text.x = element_text(angle = 60))+
  facet_grid(~Tier)


#2

mod <- glm(data = AVGSalary,
           formula = Salary ~ Tier + State + Rank)
summary(mod)
amod <- anova(mod)
anova(mod)
write.table(amod,file = "Salary_ANOVA_Summary.txt")


?write.table

#3

jdf <- read_csv("Juniper_Oils.csv")


tidy_jdf <- jdf %>% 
  pivot_longer(cols = all_of(c("alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene","alpha-cedrene","beta-cedrene","cis-thujopsene","alpha-himachalene","beta-chamigrene","cuparene","compound 1","alpha-chamigrene","widdrol","cedrol","beta-acorenol","alpha-acorenol","gamma-eudesmol","beta-eudesmol","alpha-eudesmol","cedr-8-en-13-ol","cedr-8-en-15-ol","compound 2","thujopsenal")),
               names_to = "ChemicalID",
               values_to = "Concentration")

tidy_jdf %>% 
  ggplot(aes(x=YearsSinceBurn,y=Concentration))+
  geom_smooth()+
  theme_minimal()+
  facet_wrap(~ChemicalID,scales = "free")

#4

jmod <- glm(data = tidy_jdf,
            formula = Concentration~YearsSinceBurn*ChemicalID)


anova(jmod)

tjmod <- tidy(jmod)


tjmod %>% 
  filter(p.value < 0.05) %>% 
  mutate(term=str_remove_all(term,"ChemicalID"))
