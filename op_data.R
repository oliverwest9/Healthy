#4
## OP_Data


#Set Variables Up.
NSpeciality <- nrow(OP_Data)
Projection_length <- 5

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