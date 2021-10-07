library(tidyverse)
library(GGally)
library(patchwork)
library(magick)
library(png)
library(ggimage)
library(ggpubr)

setwd("C:/Users/nicka/Data_Course/Data")

img <- readPNG("C:/Users/nicka/Data_Course/baby_yoda_santa.png")
  
csv_files <- list.files(pattern = ".csv")

iris_file <- read_csv(csv_files[11])

iris_file %>% 
  ggplot(aes(x=Sepal.Length,y=Petal.Width,colour=Species))+
  geom_point()+
  background_image(img)+
  geom_text(aes(label = Species),size = 3)+
  ggtitle("What am I doing?")+
  labs(x="How long are deez petals? (ft)",
       y="How long deez sepals? (ft)")+
  geom_smooth(method = "lm",colour="yellow")+
  theme(plot.background = element_rect(fill = "yellow"),
        axis.title = element_text(colour = "lime green",size = 5,angle = 45),
        plot.title = element_text(colour = "yellow green",size = 30,angle = 190,
                                  face = "bold.italic"),
        axis.title.y = element_text(colour = "purple",angle = 110,size = 5),
        panel.grid = element_line(colour = "black"),
        panel.background = element_rect(colour = "yellow"))

