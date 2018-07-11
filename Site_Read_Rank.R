
#install requared pachages
library(tidyverse)
library(Matrix)
library(XLConnect, pos = 4)
library(readxl)
library(rJava)

#defina varibales
NSites <- 10
Npopulation <- 10

#load in data and adjust variables
data_input <- read.csv(file="data_input.csv", header=TRUE) %>%
  mutate(site = factor(site))

#Read in Specialities List
Spec_input <- read.csv(file="Specialty.csv", header=TRUE)

#Set Up Factors for specialities. [Will work when full list of specialities is loaded in.]
data_input$Speciality <- factor(data_input$Speciality, levels = Spec_input[,1]);
data_input$Spec1 <- factor(data_input$Spec1, levels = Spec_input[,1]);
data_input$Spec2 <- factor(data_input$Spec2, levels = Spec_input[,1]);
data_input$Spec3 <- factor(data_input$Spec3, levels = Spec_input[,1]);



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



