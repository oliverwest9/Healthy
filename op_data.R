## OP_Data

library(tidyverse);
library(readxl);

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

#AB-AG
New_FU

#AR-Aw total sessions pregression over 5 years

Progression <- matrix(nrow = NSpeciality ,ncol = Projection_length +1) #to account for year 0
colnames(Progression) <- c(paste("Yr",1:Projection_length +1 ))

for(i in 1:NSpeciality) {
  
  Progression[i,1] = (OP_Data[i,2] + OP_Data[i,3])
  Progression[i,2] = (OP_Data[i,2]) * (1+Growth[i,1])# * 1+ New_FU1 
  Progression[i,3] = (OP_Data[i,2]) * (1+Growth[i,1]) * 1+Growth[i,2]# * 1+New_FU2
  Progression[i,4] = (OP_Data[i,2]) * (1+Growth[i,1]) * 1+Growth[i,2]*Growth[i,3]# * 1+New_FU3
  Progression[i,5] = (OP_Data[i,2]) * (1+Growth[i,1]) * 1+Growth[i,2]*Growth[i,3]*Growth[i,4]# * 1+New_FU4
  Progression[i,5] = (OP_Data[i,2]) * (1+Growth[i,1]) * 1+Growth[i,2]*Growth[i,3]*Growth[i,4]* Growth[i,5]
  # * 1+New_FU4
}


#BV-CG
Aftercare_GP_Nurse <- matrix(nrow = NSpeciality ,ncol = (Projection_length +1)*2)
colnames(Aftercare_GP_Nurse) <- c(paste("GP_Aftercare_Yr",0:Projection_length), paste("Nurse_Aftercare_Yr",0:Projection_length))




