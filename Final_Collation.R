
#install requared pachages
#library(tidyverse)
library(Matrix)
library(XLConnect, pos = 4)
library(readxl)
library(rJava)


#Read in Excel sheets.
Sheet_1 <- read_excel('input_data.xlsx', sheet = 1);
Sheet_2 <- read_excel('input_data.xlsx', sheet = 2);
Sheet_3 <- read_excel('input_data.xlsx', sheet = 4);

#Create Data File.
data_input  <- merge.data.frame(Sheet_1,Sheet_2, by = "Index", all = TRUE);
data_input  <- merge.data.frame(data_input,Sheet_3, by = "Index", all = TRUE);
data_input$Index <- NULL;


#Read In Data, need to be combined.
OP_Data <- read_excel('input_data.xlsx', sheet = 3);

#define varibles
NSites <- nrow(Sheet_1)
Npopulation <- nrow(Sheet_2)

#Remove Redundant Variables
rm(Sheet_1);
rm(Sheet_2);
rm(Sheet_3);

#########################
###########1#############
#########################

#Calculte the distance between Sites 
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

#########################
###########2#############
#########################

#calculte the distance between Population Nodes.
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


#########################
###########3#############
#########################

#Projection Run Time.
Projection_length <- 5;

#Set Variables Up.
NSpeciality <- nrow(OP_Data)


#Calculate Growth Rates for intiatives.
Soc_Growth <- as.data.frame(data_input$Y1)

Soc_Growth <- matrix(nrow = NSpeciality, ncol = Projection_length)
Soc_Growth[,1] <- data_input$Y1;

#Calculate Growth Rates. [Columns Labels, need to be sorted]
for (i in 2:Projection_length)
{
  Soc_Growth[,i] <- Soc_Growth[,i-1]*(1+ data_input$Growth);
}
Soc_Growth_Round <- round(Soc_Growth)

#Calculate Follow Up Reductions Saved
FU_Saved <- matrix(nrow = NSpeciality, ncol = Projection_length);
FU_Saved[,1] <- round(data_input$Y1*data_input$FU_Redn);

for(i in 2:Projection_length)
{
  FU_Saved[,i] <- round(data_input$FU_Redn*Soc_Growth[,i]);
}


#Calculate cost per a year.
Cost_Saved<- matrix(nrow = NSpeciality, ncol = Projection_length)
Cost_Saved[,1] <- data_input$Y1*data_input$Cost;
for (i in 2:Projection_length)
{
  Cost_Saved[,i] <- data_input$Cost*Soc_Growth[,i];
}


Years <- Projection_length;

#Calculate Growth Rates for secondary effects of intiatives.
Counter <- 1;
Diag_Saved_1 <- round(as.data.frame(data_input$Y1*data_input$Prob1));
colnames(Diag_Saved_1) <- ("Year_1");
for (i in 1:Years)
{
  Diag_Saved_1[,Counter+1] <- round(Soc_Growth[,Counter]*data_input$Prob1)
  Counter <- Counter+1;
}

Diag_Saved_1 <- cbind(data_input$Spec1, Diag_Saved_1);
Diag_Saved_1[,1] <- NULL;
Diag_Saved_1[,2] <- NULL;

#Calculate Growth Rates for secondary effects of intiatives.
Counter <- 1;
Diag_Saved_2 <- round(as.data.frame(data_input$Y1*data_input$Prob2));
colnames(Diag_Saved_2) <- ("Year_1");
for (i in 1:Years)
{
  Diag_Saved_2[,Counter+1] <- round(Soc_Growth[,Counter]*data_input$Prob2)
  Counter <- Counter+1;
}
Diag_Saved_2 <- cbind(data_input$Spec2, Diag_Saved_2);
Diag_Saved_2[,1] <- NULL;
Diag_Saved_2[,2] <- NULL;

#Calculate Growth Rates for secondary effects of intiatives.
Counter <- 1;
Diag_Saved_3 <- round(as.data.frame(data_input$Y1*data_input$Prob3));
colnames(Diag_Saved_3) <- ("Year_1");
for (i in 1:Years)
{
  Diag_Saved_3[,Counter+1] <- round(Soc_Growth[,Counter]*data_input$Prob3)
  Counter <- Counter+1;
}

Diag_Saved_3 <- cbind(data_input$Spec3, Diag_Saved_3);
Diag_Saved_3[,1] <- NULL;
Diag_Saved_3[,2] <- NULL;

#########################
###########4#############
#########################

#Set Variables Up.
NSpeciality <- nrow(OP_Data)

#M-P [Hard Coded At the Moment.]
Growth <- matrix(nrow = NSpeciality, ncol = Projection_length ) 
Growth[,1] <- 0.020;
Growth[,2] <- 0.020;
Growth[,3] <- 0.015;
Growth[,4] <- 0.015;
Growth[,5] <- 0.01;



#R-W
#Calculate New Diagnoses Saved Year on Year.
#Calculate Total Saved from Effect 1 of Intiatives.
Saved_1 <- matrix(nrow = NSpeciality, ncol = Projection_length);
for (i in 1:NSpeciality){
  for (j in 1:NSpeciality){
    if (OP_Data[i,1] == as.character(data_input[j,14])){
      for (k in 1:Projection_length){
        Saved_1[i,k] <- Diag_Saved_1[j,k] ;
      }
    }
  }
}
#Calculate Total Saved from Effect 2 of Intiatives.
Saved_2 <- matrix(nrow = NSpeciality, ncol = Projection_length);
for (i in 1:NSpeciality){
  for (j in 1:NSpeciality){
    if (OP_Data[i,1] == as.character(data_input[j,16])){
      for (k in 1:Projection_length){
        Saved_2[i,k] <- Diag_Saved_2[j,k] ;
      }
    }
  }
}
#Calculate Total Saved from Effect 3 of Intiatives.
Saved_3 <- matrix(nrow = NSpeciality, ncol = Projection_length);
for (i in 1:NSpeciality){
  for (j in 1:NSpeciality){
    if (OP_Data[i,1] == as.character(data_input[j,18])){
      for (k in 1:Projection_length){
        Saved_3[i,k] <- Diag_Saved_3[j,k] ;
      }
    }
  }
}
#Sum Together all 3 effects.
Saved_Tot <- Saved_1 + Saved_2 + Saved_3;
Saved_Tot[is.na(Saved_Tot)] <- 0

New_Diag_SP <- matrix(nrow = NSpeciality, ncol = Projection_length);
New_Diag_SP[,1] <- OP_Data$New - Saved_Tot[,1];

#Include Growth of effect and produce projection.
for(i in 2:Projection_length)
{
  New_Diag_SP[,i] <- round((New_Diag_SP[,i-1]*(1+ Growth[,i-1])) -Saved_Tot[,i]);
}

#After Care Saved; W-AA

After_Care <- matrix(nrow = NSpeciality, ncol = Projection_length);
After_Care[,1] <- (OP_Data$Followup/OP_Data$New)*New_Diag_SP[,1];

#Rounding error, from New_Diag causing errors here.
for(i in 2:Projection_length){
  After_Care[,i] <- (OP_Data$Followup/OP_Data$New)*New_Diag_SP[,i] - Saved_Tot[,i]
}

#AB-AG New Follow Ups.
New_FU <- matrix(nrow = NSpeciality, ncol = Projection_length);
New_FU[,1] <- OP_Data$Followup/OP_Data$New;

for(i in 2:Projection_length){
  New_FU[,i] <- OP_Data$Followup/OP_Data$New;
}

#New Cases Per A Captia.
data_input$size[is.na(data_input$size)] <- 0;
New_Per_k <- (OP_Data$New/sum(data_input$size))*1000;

#Follow Ups Per A Captia.
FU_per_K <- (OP_Data$Followup/sum(data_input$size))*1000;

#Columns AJ-AQ relate to equipment used and percentage of time used.

#AR-Aw total sessions pregression over 5 years
Tot_Cases <- matrix(nrow = NSpeciality, ncol = Projection_length +1);
Tot_Cases[,1] <- OP_Data$New + OP_Data$Followup;

#Accumuleted Growth, []
Acc_Growth<- matrix(nrow = NSpeciality, ncol = Projection_length);
Acc_Growth[,1] <- 1 + Growth[,1];
for (i in 2:Projection_length){
  Acc_Growth[,i] <- Acc_Growth[,i-1]*(1 + Growth[,i]);
}

#Total Cases.
for ( i in 2:(Projection_length +1)){
  Tot_Cases[,i] <- round(OP_Data$New*Acc_Growth[,i-1]*(New_FU[,i-1]+1));
}

#Percentage of New Cases Attended by Nurses.*Input Variable.*
Nurse_perc_New <- matrix(nrow = NSpeciality, ncol = Projection_length+1);

Nurse_perc_New[,1] <- .0; 
Nurse_perc_New[,2] <- .05;
Nurse_perc_New[,3] <- .10;
Nurse_perc_New[,4] <- .15;   
Nurse_perc_New[,5] <- .20;
Nurse_perc_New[,6] <- .25;

#Percentage of Follow Up Cases Attended by Nurses.*Input Variable.*
Nurse_perc_Follow <- matrix(nrow = NSpeciality, ncol = Projection_length+1);

Nurse_perc_Follow[,1] <- .5;
Nurse_perc_Follow[,2] <- .55;
Nurse_perc_Follow[,3] <- .60;
Nurse_perc_Follow[,4] <- .65;
Nurse_perc_Follow[,5] <- .70;
Nurse_perc_Follow[,6] <- .75;

#Baseline Reference for New Cases.
Base_New <- matrix(nrow = NSpeciality, ncol = Projection_length+1);
Base_New[,1] <- OP_Data$New*(1+Growth[,1]);

for (i in 2:Projection_length)
{
  Base_New[,i] <- Base_New[,i-1]*(1+Growth[,i]);
}

#Baseline Refence for Follow Up Cases.
Base_Follow <- matrix(nrow = NSpeciality, ncol = Projection_length+1);
Base_Follow[,1] <- OP_Data$Followup*(1+Growth[,1]);

for (i in 2:Projection_length)
{
  Base_Follow[,i] <- Base_Follow[,i-1]*(1+Growth[,i]);
}

#Follow Up Cases Split by Nurse and GP
GP_Follow <- matrix(nrow = NSpeciality, ncol = Projection_length+1);
GP_Follow[,1] <- (OP_Data$Followup)*(1-Nurse_perc_Follow[,1]);

for (i in 1:Projection_length+1){
  GP_Follow[,i] <- After_Care[,i-1]*(1-Nurse_perc_Follow[,i]);
}

Nurse_Follow <- matrix(nrow = NSpeciality, ncol = Projection_length+1);
Nurse_Follow[,1] <- (OP_Data$Followup)*(Nurse_perc_Follow[,1]);

for (i in 1:Projection_length+1){
  Nurse_Follow[,i] <- After_Care[,i-1]*(Nurse_perc_Follow[,i]);
}

#New Cases, Split by Nurse and GP
GP_New <- matrix(nrow = NSpeciality, ncol = Projection_length+1);
GP_New[,1] <- (OP_Data$New)*(1-Nurse_perc_New[,1]);

for (i in 1:Projection_length+1){
  GP_New[,i] <- New_Diag_SP[,i-1]*(1-Nurse_perc_New[,i])
}

Nurse_New <- matrix(nrow = NSpeciality, ncol = Projection_length+1);
Nurse_New[,1] <- (OP_Data$New)*(Nurse_perc_New[,1]);

for (i in 1:Projection_length+1){
  Nurse_New[,i] <- New_Diag_SP[,i-1]*(Nurse_perc_New[,i])
}


#########################
###########5#############
#########################

#Calculate amount of working hours available in a year.
#Define a yearly 10% reduction in hours worked by GP's.
GP_Hours_Available <- matrix(nrow = Projection_length, ncol = NSites);
GP_Hours_Available[1,] <- 250;
for (i in 2:Projection_length){
  GP_Hours_Available[i,] <- GP_Hours_Available[i-1,]*0.90;
}

#Define Clinic Parameters [Fixed for now.]
#GP Working Hours
GP_Day_length <- 7;
#Rooms per a clinic
Rooms_Clinic <- matrix(nrow = Projection_length, ncol = NSites)
Rooms_Clinic <- 1;
#Lists per a day
Lists_Clinc <- 1;

#Calculate capacity in minutes.
GP_Mins_Year <- matrix(nrow = Projection_length, ncol = NSites);

for (i in 1:Projection_length){
  GP_Mins_Year[i,] <- GP_Hours_Available[i,]*GP_Day_length*Rooms_Clinic*Lists_Clinc*60;
}

#Calculate Clinic Utilisation Rate.
Clinic_Utilisation <- matrix(nrow = Projection_length, ncol = NSites);

Clinic_Utilisation <- Mins_Required/GP_Mins_Year;

#Define HR Parameters [Fixed for simplification.]
#Porters
Wage_porter <- 10;
No_porter <- 1;

#Cleaner
Wage_cleaner <- 10;
No_cleaner <- 1;

#Receptionists
wage_reception <- 10;
No_Recept <- 1;

#Nurses
wage_nurse <- 15;
No_nurse <- 0;

#Health Care Assistants
wage_HCAssist <- 10;
No_HCAssist <- 0;

#Staff Grades
wage_Grade <- 18;
No_Grade <- 0;

#SHO 
wage_SHO <- 20;
No_Sho <- 0;

#GP
wage_GP <- 50;
No_GP <- 1;

#Calculate HR Cost in a day.
HR_Cost_Day <- matrix(nrow = Projection_length, ncol = NSites);
for (i in 1:Projection_length)  {
  HR_Cost_Day[i,] <-(wage_GP+wage_reception+Wage_cleaner+Wage_porter)*GP_Day_length;
}
#Caluclate HR Cost in a year.
HR_Cost_Year <- matrix(nrow = Projection_length, ncol = NSites);
HR_Cost_Year <- HR_Cost_Day*GP_Hours_Available;

#Calculate Cost of Intiatives.
Intiative_Cost <- matrix(nrow = Projection_length, ncol = NSites);
for (i in 1:Projection_length){
  Intiative_Cost = sum(Cost_Saved[i,]);
}
