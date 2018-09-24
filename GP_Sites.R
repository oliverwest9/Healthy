#Calculate amount of working hours available in a year. 
GP_Hours_Available <- matrix(nrow = Projection_length, ncol = NSites);

for (i in 1:Projection_length){
  GP_Hours_Available[i,] <- 250;
}

#Define Clinic Parameters [Fixed for now.]
#GP Working Hours
GP_Day_length <- 7;
#Rooms per a clinic
Rooms_Clinic <- 1;
#Lists per a day
Lists_Clinc <- 1;

#Calculate capacity in minutes.
GP_Mins_Year <- matrix(nrow = Projection_length, ncol = NSites);

for (i in 1:Projection_length){
  GP_Mins_Year[i,] <- GP_Hours_Available[i,]*GP_Day_length*Rooms_Clinic*Lists_Clinc*60;
}

#Calculate Clinic Utilisation Rate. [Requires input from Alex.]
Clinic_Utilisation <- matrix(nrow = Projection_length, ncol = NSites);

Clinic_Utilisation <- ???/GP_Mins_Year;

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
Intiative Cost <- matrix(nrow = Projection_length, ncol = NSites);
for (i in 1:Projection_length){
  
}
  