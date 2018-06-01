##data handling practice##

#install requared pachages
install.packages("tidyverse")
install.packages("Matrix")
install.packages("XLConnect")
install.packages("readxl")
install.packages("rJava")
library(tidyverse)
library(Matrix)
library(XLConnect, pos = 4)
library(readxl)
library(rJava)

#defina varibales
NSites <- 10
Npopulation <- 10

site_data <- read_xls("C:/Users/Alex/Documents/Exeter University/Internship/On Campus Placments/NHS/R/data_input.xls", Sheet=1)
sheet.names <- getSheets(loadWorkbook('data_input.xls'))
#load in data and adjust variables
data_input <- read.csv(file="data_input.csv", header=TRUE) %>%
  mutate(site = factor(site))

#calculte the distance between sites
distance <- matrix(nrow = NSites, ncol = NSites)

for (i in 1:NSites){
  for(j in 1:NSites){
    distance[i,j] = ((data_input[i,2] - data_input[j,2])^2 +  
                       (data_input[i,3] - data_input[j,3])^2)^0.5    
  }
}

#removing 0's from the distance matrix
distance[distance == 0] <- NA
#creating a rank matrix
rank_distance <- matrix(nrow = NSites, ncol = NSites)
#filling the rank matrix
for (i in 1:NSites){
  rank_distance[i,] <- rank(distance[i,])
}
#removing ranks that corisponde to NA's from the "distance" matrix
rank_distance[rank_distance == NSites] <- NA


