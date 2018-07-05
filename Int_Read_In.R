#Read in Specialities List
Spec_input <- read.csv(file="Specialty.csv", header=TRUE)

#Index function for specialities, [To Avoid Working with Character Strings]
Spec_index <- function(Spec_No){
  Speciality_Char <- na.omit(Spec_input[Spec_input$Int_No == 2,]);
  R
  return(toString(Speciality_Char[1,1]))
}

#Calculate Growth Rates for intiatives.
Soc_Growth <- as.data.frame(data_input$Y1)

#Assuming, we are letting the model run for 5 years [Can be changed later.]
Years <- 5

#Calculate Growth Rates. [Columns Labels, need to be sorted]
for (i in 1:Years)
{
  Soc_Growth$Years <- round(Soc_Growth*data_input$Growth)
}


#Calculate Follow Up Reductions Saved
FU_Saved <- round(as.data.frame((data_input$FU_Redn)*(data_input$Y1)))

for (i in 1:Years)
{
  FU_Saved$Years <- round(data_input$FU_Redn*Soc_Growth$Years)
}

#Calculate cost per a year. [Needs to be finished]
Cost_Saved <- as.data.frame(data_input$Cost*data_input$Y1);

for (i in 1:Years)
{
  #Cost_Saved$Years <- round(Cost_Saved$
}

#Calculate Growth Rates for secondary effects of intiatives, [Tempremental, make sure you download excel input
#sheet from Git]
Diag_Saved_1 <- round(as.data.frame(data_input$Prob1*data_input$Y1))
for (i in 2:Years)
{
  Diag_Saved_1[,i] <- round(Diag_Saved_1[,i-1]*Soc_Growth[,i])
}
Diag_Saved_2 <- round(as.data.frame(data_input$Prob2*data_input$Y1))
for (i in 2:Years)
{
  Diag_Saved_2[,i] <- round(Diag_Saved_2[,i-1]*Soc_Growth[,i])
}
Diag_Saved_3 <- round(as.data.frame(data_input$Prob3*data_input$Y1))
for (i in 2:Years)
{
  Diag_Saved_3[,i] <- round(Diag_Saved_3[,i-1]*Soc_Growth[,i])
}