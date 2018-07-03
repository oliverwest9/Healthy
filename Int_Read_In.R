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