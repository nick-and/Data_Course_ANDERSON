# Data_Course_ANDERSONThis README file contains information about my uploaded assignments
This README file contains information about my uploaded assignments. vespa mandarinia

getwd("C:Users/nicka/Data_Course/Data") 


csv_files <- list.files(pattern = ".csv",path = ".",recursive = TRUE)
saves my list of csv files as an object so I don’t have to just see it in the bottom left section of R.

length(csv_files)
allows me to see how many files I have in the object I just saved.
df <- read.csv("wingspan_vs_mass.csv")

head(df[1:5])
saves my file of interest as an object so I can more easily open and read it.
Also used head command to read just the first 5 lines of data in the file.
