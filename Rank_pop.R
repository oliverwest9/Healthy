#Pop_rank function.

#install requared pachageS
library(tidyverse)
library(Matrix)
library(XLConnect, pos = 4)
library(readxl)

#define variables
NSites <- 10
Npopulation <- 10

#load in data and adjust variables
data_input <- read.csv(file="data_input.csv", header=TRUE) %>%
  mutate(population_node = factor(population_node))

#calculte the distance between sites
distance_pop <- matrix(nrow = Npopulation, ncol = Npopulation)

for (i in 1:Npopulation){
  for(j in 1:Npopulation){
    distance_pop[i,j] = ((data_input[i,5] - data_input[j,5])^2 +  
                     (data_input[i,6] - data_input[j,6])^2)^0.5    
  }
}

#removing 0's from the distance matrix
distance_pop[distance_pop == 0] <- NA

#creating a rank matrix
pop_rank_distance <- matrix(nrow = Npopulation, ncol = Npopulation)

#filling the rank matrix
for (i in 1:NSites){
  pop_rank_distance[i,] <- rank(distance_pop[i,])
}
#removing ranks that corisponde to NA's from the "distance" matrix
pop_rank_distance[pop_rank_distance == NSites] <- NA


