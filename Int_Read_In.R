
#Index function for specialities, [To Avoid Working with Character Strings] [Possibly redundant.]
Spec_index <- function(Spec_No){
  Speciality_Char <- na.omit(Spec_input[Spec_input$Int_No == 2,]);
  return(toString(Speciality_Char[1,1]))
}

#Calculate Growth Rates for intiatives.
Soc_Growth <- as.data.frame(data_input$Y1)

#Assuming, we are letting the model run for 5 years [Can be changed later.]
Years <- 5

#Calculate Growth Rates. [Columns Labels, need to be sorted]
for (i in 1:Years)
{
  Soc_Growth$Years <- round(Soc_Growth*data_input$Growth);
}

#Short Term Fix, to unknown problem.
Soc_Growth <- Soc_Growth[1,2] ;
Soc_Growth <- cbind(Y1 =5, Soc_Growth);

#Calculate Follow Up Reductions Saved
FU_Saved <- round(as.data.frame((data_input$FU_Redn)*(data_input$Y1)));

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
Diag_Saved_3[,2] <- NULL;