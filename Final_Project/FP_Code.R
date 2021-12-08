library(tidyverse)
library(lubridate)
library(png)
options(scipen = 99999)


#pictures



img <- readPNG("C:/Users/nicka/Data_Course/baby_yoda_santa.png")

img1 <- readPNG("coyoteg.png")


# Zion
zion <- read_csv("Zion_monthlyvis.csv",skip = 3)
zion <- zion %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "Zion")


arches <- read_csv("Arches_monthlyvis.csv",skip = 3)
arches <- arches %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "Arches")

grand_canyon <- read_csv("GrandCanyon_monthlyvis.csv",skip = 3)
grand_canyon <- grand_canyon %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "GrandCanyon")

bryce <- read_csv("BryceC_monthlyvis.csv",skip = 3)
bryce <- bryce %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "bryce")


capitolr <- read_csv("CapitolReef_monthlyvis.csv",skip = 3)
capitolr <- capitolr %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "capitolr")


yellowstone <- read_csv("Yellowstone_monthlyvis.csv",skip = 3)
yellowstone <- yellowstone %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "yellowstone")


yosemite <- read_csv("Yosemite_monthlyvis.csv",skip = 3)
yosemite <- yosemite %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "yosemite")


z_a <- full_join(zion,arches)

z_a_g <- full_join(z_a,grand_canyon)

zagb <- full_join(z_a_g,bryce)

zagbc <- full_join(zagb,capitolr)

zagbcy <- full_join(zagbc,yellowstone)

zagbcyy <- full_join(zagbcy,yosemite)


year20 <- filter(zagbcyy,Year==2020)

months <- c("JAN","FEB","MAR",
            "APR","MAY","JUN","JUL",
            "AUG","SEP","OCT",
            "NOV","DEC")
monthss <- c("JAN","FEB","MAR",
            "APR","MAY","JUN","JUL",
            "AUG","SEP","OCT",
            "NOV","DEC")


year20 %>% 
  mutate(Month = factor(Month,levels = months))


year20 %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_line(aes(group = 1))+
  ylab("Monthly Visitors")+
  theme(axis.text.x = element_text(angle = 90,size = 5))+
  facet_wrap(~Park)



year20yos <- year20 %>% 
  filter(Park == "yosemite")


year20yos %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_line(aes(group = 1))+
  ylab("Monthly Visitors")+
  theme(axis.text.x = element_text(angle = 90,size = 5))

year19yos <- zagbcyy %>% 
  filter(Year==2019,Park=="yosemite")

year19yos %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_line(aes(group = 1))+
  ylab("Monthly Visitors")+
  theme(axis.text.x = element_text(angle = 90,size = 5))




















garfield <- read_csv("abczfixedtemp.csv",skip = 3)

gartemp <- garfield %>% 
  mutate(Monthnumber = str_sub(Date,start = 5,end = 6)) %>% as.numeric() %>% 
  mutate(Year=str_sub(Date,end=4)) %>% as.numeric() %>% 
  mutate(Month = case_when(Monthnumber == 1 ~ "JAN",
                           Monthnumber == 2 ~ "FEB",
                           Monthnumber == 3 ~ "MAR",
                           Monthnumber == 4 ~ "APR",
                           Monthnumber == 5 ~ "MAY",
                           Monthnumber == 6 ~ "JUN",
                           Monthnumber == 7 ~ "JUL",
                           Monthnumber == 8 ~ "AUG",
                           Monthnumber == 9 ~ "SEP",
                           Monthnumber == 10 ~ "OCT",
                           Monthnumber == 11 ~ "NOV",
                           Monthnumber == 12 ~ "DEC"))
glimpse(garfield)
full_join(zagbcyy,gartemp)





gartemp <- garfield %>% 
  mutate(Monthnumber=str_sub(Date,start=5,end=6) %>% as.numeric() %>% 
  mutate(Year=str_sub(Date,end=4) %>%
  mutate(day =01) %>% 
  mutate(date = as.POSIXct(paste0(Year,"-",Monthnumber,"-",day))) %>% 
  mutate(Month = case_when(Monthnumber == 1 ~ "JAN",
                           Monthnumber == 2 ~ "FEB",
                           Monthnumber == 3 ~ "MAR",
                           Monthnumber == 4 ~ "APR",
                           Monthnumber == 5 ~ "MAY",
                           Monthnumber == 6 ~ "JUN",
                           Monthnumber == 7 ~ "JUL",
                           Monthnumber == 8 ~ "AUG",
                           Monthnumber == 9 ~ "SEP",
                           Monthnumber == 10 ~ "OCT",
                           Monthnumber == 11 ~ "NOV",
                           Monthnumber == 12 ~ "DEC"))

















months <- c("January","February","March",
            "April","May","June","July",
            "August","September","October",
            "November","December")
vals <- 1:12

df <- data.frame(months,y=vals)

df$months %>% factor(levels = c("January","February","March",
                                "April","May","June","July",
                                "August","September","October",
                                "November","December"))




#Temperature Data



temp <- read_csv("IronTempF.csv", skip = 3)


almost_temp <- temp %>% 
  mutate(Monthnumber=str_sub(Date,start=5,end=6)) %>% as.numeric() %>% 
  mutate(Year=str_sub(Date,end=4) %>% as.numeric()) %>%
  mutate(day =01) %>% 
  mutate(date = as.POSIXct(paste0(Year,"-",Monthnumber,"-",day))) %>% 
  mutate(Month = case_when(Monthnumber == 1 ~ "JAN",
                           Monthnumber == 2 ~ "FEB",
                           Monthnumber == 3 ~ "MAR",
                           Monthnumber == 4 ~ "APR",
                           Monthnumber == 5 ~ "MAY",
                           Monthnumber == 6 ~ "JUN",
                           Monthnumber == 7 ~ "JUL",
                           Monthnumber == 8 ~ "AUG",
                           Monthnumber == 9 ~ "SEP",
                           Monthnumber == 10 ~ "OCT",
                           Monthnumber == 11 ~ "NOV",
                           Monthnumber == 12 ~ "DEC"))



vis_temp <- full_join(zagbcyy,almost_temp)

#Graph with Temperature

temp_20 <- filter(vis_temp,Year == 2020)





#Adding yearly visitors
vis_temp <- full_join(zagbcyy,almost_temp)

vis_temp_y <- vis_temp %>% 
  group_by(Year,Park) %>% 
  mutate(YearlyVis = sum(MonthlyVisitor))


vis_temp_y %>% 
  filter(Year>1980) %>% 
  group_by(Year) %>% 
  ggplot(aes(x=Year,y=YearlyVis,color=Park))+
  geom_line()


#graph with temperature

zbctemp <- vis_temp_y %>% 
  group_by(Year,Park) %>% 
  summarise(YearlyTemp=mean(Value,na.rm = TRUE))

zbctemp %>% 
  filter(Park == c("Zion","bryce","capitolr")) %>% 
  ggplot(aes(x=Year,y=YearlyTemp))+
  geom_line()


################

#yosemite temp

yostemp <- read_csv("yosemitetemp.csv")

zbcr <- read_csv("ZBCRtemp.csv")

utahparktemp <- zbcr %>% 
  mutate(Year = str_sub(date,start = 1,end=4)) %>% 
  mutate(MeanMonthly = )
  filter(Year > 1978)

utahparktemp %>% 
  group_by(Year) %>% 
  summarise(YearlyTemp=mean(Value,na.rm = TRUE))

ZBCRtemp <- zbcr %>% 
  mutate(Monthnumber=str_sub(Date,start=5,end=6)) %>% as.character() %>% 
  mutate(Year=str_sub(Date,end=4)) %>% as.character()










Tempxyear <- vis_temp_y %>% 
  group_by(Year,Park) %>% 
  summarise(YearlyTemp=mean(Value,na.rm = TRUE))

Tempxyear %>% 
  ggplot(aes(x=Year,y=YearlyTemp,color=Park))+
  geom_line()

glimpse(Tempxyear)



#Distance of each park from closest major city

cities <- data.frame(Park = c("Zion","Arches","GrandCanyon","bryce","capitolr","yellowstone",
                    "yosemite"),
           Distance_from_Major_City.mi = c(1.1,5,6,2.4,4.6,57,14.9),
           NearestCity = c("Springdale","Moab","Tusayan","BryceCanyonCity","Torrey",
                            "Jackson","El Portal"),
           Hotels = c(59,100,20+22,43,4,99+10,133+11),
           Size_acres = c(146597,76679,1218375.5,35835,241900,2219789,748000))
ac <- full_join(zagbcyy,cities)

cities %>% 
  ggplot(aes(x=NearestCity,y=Hotels,fill=Park))+
  geom_bar(stat = "identity")

glimpse(cities)


finalset <- full_join(vis_temp_y,cities)










ftemp <- as.numeric(almost_temp$Year)


visitor_temp <- full_join(almost_temp,zion)


lubridate::month("JAN",format="%M")

months <- month.abb
months <- months %>% str_to_upper()
match(months,month_number)


data.frame(months) %>% 
  mutate(monthnumber=case_when(months == "JAN" ~ 1,
                               months == "FEB" ~ 2,
                               months == "MAR" ~ 3,
                               months == "APR" ~ 4,
                               months == "MAY" ~ 5,
                               months == "JUN" ~ 6,
                               months == "JUL" ~ 7,
                               months == "AUG" ~ 8,
                               months == "SEP" ~ 9,
                               months == "OCT" ~ 10,
                               months == "NOV" ~ 11,
                               months == "DEC" ~ 12))




#combining two plots

year20yos <- year20 %>% 
  filter(Park == "yosemite")


year20yos %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_line(aes(group = 1))+
  ylab("Monthly Visitors")+
  theme(axis.text.x = element_text(angle = 90,size = 5))

year19yos <- zagbcyy %>% 
  filter(Year==2019,Park=="yosemite")

year19yos %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_line(aes(group = 1))+
  ylab("Monthly Visitors")+
  theme(axis.text.x = element_text(angle = 90,size = 5))

zagbcyy %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  filter(Park=="Zion",Year>2018) %>%
  group_by(Year) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor,color=Year))+
  geom_line(aes(group = 1))





#Help from class

day1 <- 197901

day1 %>% 
  as.character() %>% 
  str_sub(end = 4)

data.frame(day1) %>% 
  mutate(Month=str_sub(day1,start=5,end=6)) %>% 
  mutate(Year=str_sub(day1,end=4)) %>% 
  mutate(day ="01") %>% 
  mutate(date = as.POSIXct(paste0(Year,"-",Month,"-",day)))





