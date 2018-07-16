## OP_Data

library(tidyverse);
library(readxl);

#[Note: Factors temp removed, due to not all specialities being used currently, so factor comparisons fails].
OP_Data <- read_excel('input_data_2.xlsx', sheet = 3)

NSpeciality <- nrow(OP_Data)
Projection_length <- 5

#M-P
Growth <- matrix(nrow = NSpeciality, ncol = Projection_length ) 
colnames(Growth) <- c(paste("Growth",1:Projection_length ))

for (i in 1:NSpeciality){
  for (j in 1:Projection_length ){
    Growth[i,j] =(OP_Data[i,12] * 200/(200 + OP_Data[i,j+6] - OP_Data[i,j+5]) - 1)
  }
}

#R-AA
#need to load in Soc_pres and Output(sheet1) data for this part:
New_Diag_Soc <- as.data.frame((OP_Data$New));
colnames(New_Diag_Soc) <- ("Y1");
Index <- as.data.frame(c(1:NSpeciality));


for (i in 1:NSpeciality)
{
  if(as.character(OP_Data[i,1]) == as.character(data_input[i,14])){
    Sub_1 <- filter(Diag_Saved_1,Diag_Saved_1[,1] == as.character(OP_Data[i,1]));
    New_Diag_Soc[i,1] <- New_Diag_Soc[i,1] - Sub_1[1,2];
  }
}

for (i in 1:NSpeciality)
{
  if(as.character(OP_Data[i,1]) == as.character(data_input[i,16])){
    Test <- 1;
    Sub_2 <- filter(Diag_Saved_2,Diag_Saved_2[,1] == as.character(OP_Data[i,1]));
    New_Diag_Soc[i,1] <- New_Diag_Soc[i,1] - Sub_2[1,2];
  }
  
}


New_Diag_Soc$Y1 <- New_Diag_Soc$Y1 -

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




