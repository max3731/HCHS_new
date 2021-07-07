setwd("C:/Users/mschu/Documents/Documents/CSI/R-Skripte/")


HCHS = read.csv("C:/Users/mschu/Documents/Documents/CSI/R-Skripte/HCHS/HCHS_conn_0.5_aroma_gsr_abs.csv")


cortical = read.csv("C:/Users/mschu/Documents/Documents/CSI/R-Skripte/cortical.csv")

HCHS=cbind(HCHS,cortical)

library(ggplot2)
library(ggfortify)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(AICcmodavg)


for (i in c( "seg", "mean_conn_all", "mean_within_all", "mean_between_all" )){
  
  
  HCHS_cov = lm(as.formula(paste(i,"~ age ")), data =HCHS) # +vol+intercranial+BrainSegNotVent+mean_thickness+brainvol
  print(i)
  print(summary(HCHS_cov))
  #autoplot(HCHS_cov)
  cook=cooks.distance(HCHS_cov)
  print( length(cook[cook > 10]))
  print(lm.beta(HCHS_cov))
  print(car::outlierTest(HCHS_cov))
  
} 