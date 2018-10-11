library(tidyverse)
poc_ud <- read_csv('data/PT3_2.csv', col_types = cols(.default = "c"))
la_plot_data <-
  dplyr::filter(poc_ud, Region=='Totals',PCON=='Totals',`PCON Code`=='Totals',LAD != 'Totals',`LAD Code` != 'Totals')%>%
  select(Level,LAD,`SSA T1`,Age,`1617_Starts`,`1617_Achievements`) 
colnames(la_plot_data) <- c("level","la_name","SSA","Age","Starts","Achievements")
la_plot_data$Starts <- as.numeric(la_plot_data$Starts)
la_plot_data$Achievements <- as.numeric(la_plot_data$Achievements) # New scipt - Error : object 'SSA' not found
