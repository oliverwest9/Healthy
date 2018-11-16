#Reduction in GP Hours, Parameter
Hour_Reduc <- .1;
#Hours in a working day. [Potential to make variable]
Hours_Day <- 7;
#Clinics per a site.[Potential to make variable]
Clinics <- 1;

#Calculate amount of working hours available in a year. 
GP_Hours_Available <- matrix(nrow = Projection_length, ncol = NSites);

#Load in working hours for each clinic. [Assume to be 250]
GP_Hours_Available[1,] <- 250*Hours_Day;

for(i in 2:Projection_length){
  GP_Hours_Available[i,] <- GP_Hours_Available[i-1,]*(1-Hour_Reduc);
}

#Calculate amount of working hours in a year.
GP_Mins_Available <- GP_Hours_Available*Clinics*60;

#Calculate clinic utilisation rate.[Waiting on tophalf of GP_Sites from Alex]
Clinic_utilisation_rate <- ???/GP_Mins_Available;

#Now Calculate 