

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