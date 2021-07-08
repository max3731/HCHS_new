setwd("C:/Users/mschu/Documents/Documents/CSI/R-Skripte/")


HCHS = read.csv("C:/Users/mschu/Documents/Documents/CSI/R-Skripte/HCHS/HCHS_conn_0.5_aroma_gsr_abs.csv")


#cortical = read.csv("C:/Users/mschu/Documents/Documents/CSI/R-Skripte/cortical.csv")

#HCHS=cbind(HCHS,cortical)
# packages needed

library(ggplot2)
library(ggfortify)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(AICcmodavg)
library(lm.beta)
library(rcompanion)
library(mice)
library(VIM)
library(psych)
library(mice)
library(lattice)
library(regclass)
library(car)

HCHS = HCHS[,!(names(HCHS) %in% c("hypertension_1"))] # delete hypertension_1

# Summary of Dataframe

  round(describe(HCHS))

# Smoking into dummy variables

  smoking = transform(HCHS$smoking,id=as.numeric(factor(HCHS$smoking)))
  smoking = smoking$id
  HCHS["smoking"] <- smoking
  
# round education  
  
  HCHS$education=round(HCHS$education)
  
# Checking missing data (percentage)  
  
  pMiss <- function(x){sum(is.na(x))/length(x)*100}
  apply(HCHS,2,pMiss)
  apply(HCHS,1,pMiss)
  
  md.pattern(HCHS)# how many cases are complete...(missing value present in each variable in a data set.)
  aggr_plot <- aggr(HCHS, col=c('navyblue','red'), # plot an overview
                    numbers=TRUE, sortVars=TRUE, labels=names(data), 
                    cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
  
  marginplot(HCHS[c(16,1)]) # constrained at plotting 2 variables at a time only The red box plot on the left shows the distribution of varialbe one with variable two missing while
                            #the blue box plot shows the distribution of the remaining datapoints. Likewhise for the Ozone box plots at the bottom of the graph.
                            # If our assumption of MCAR data is correct, then we expect the red and blue box plots to be very similar
  

# Imputing Data 
  # MICEssumes that the missing data are Missing at Random (MAR), which means that the probability that a value is missing depends only on observed value and can be predicted using them.
  #It imputes data on a variable by variable basis by specifying an imputation model per variable.
  #A CART is a predictive algorithm that determines how a given variable’s values can be predicted based on other values.
  #It is composed of decision trees where each fork is a split in a predictor variable and each node at the end has a prediction for the target variable.

  
  tempData <- mice(HCHS,m=5,maxit=50,meth= "cart",seed=500)
  tempData$method
  
  densityplot( x=tempData , data= ~ psmd+diabetes+smoking+ animal+word+sumwords+word_perc+education + TMT + TMTA + TMTB + BMI + GDS )
  stripplot(tempData, pch = 20, cex = 1.2)
  HCHS <- complete(tempData,2)
  
# check for na in data frame
  is.na(HCHS)
  nas = apply(is.na(HCHS), 2, which) 
  
# Testing dependency with age

for (i in c( "seg", "mean_conn_all", "mean_within_all", "mean_between_all", "seg_asso", "mean_within_asso_all", " mean_between_asso_all",
             "seg_sensor", "mean_within_sensor_all", "mean_between_sensor_all")){
  
  
  HCHS_cov = lm(as.formula(paste(i,"~ age + education ")), data =HCHS) # +vol+intercranial+BrainSegNotVent+mean_thickness+brainvol
  print(i)
  print(summary(HCHS_cov))
  #autoplot(HCHS_cov)
  #cook=cooks.distance(HCHS_cov)
  #print( length(cook[cook > 10]))
  print(lm.beta(HCHS_cov))
  #print(car::outlierTest(HCHS_cov))
 # vif(HCHS_cov)
   print (AIC(HCHS_cov))
  
   print (autoplot(HCHS_cov))
  
} 
  
  autoplot(HCHS_cov)
  vif(HCHS_cov)
  1/vif(HCHS_cov)
  mean(vif(HCHS_cov))
  
  AIC(HCHS_cov)
  
# Outlier diagnostics
  
  HCHS$residuals = resid(HCHS_cov)
  
  HCHS$standardized.residuals = rstandard(HCHS_cov)
  
  HCHS$stundentized.residuals = rstudent(HCHS_cov)
  
  HCHS$cooks.distance = cooks.distance(HCHS_cov)
  
  HCHS$dfbeta = dfbeta(HCHS_cov)
  
  HCHS$dffit = dffits(HCHS_cov)
  
  HCHS$leverage = hatvalues(HCHS_cov) 
  
  HCHS$covariance.ratios = covratio(HCHS_cov)
  
  HCHS$standardized.residuals > 2 | HCHS$standardized.residuals < -2
  
  HCHS$large.residuals = HCHS$standardized.residuals>2 | HCHS$standardized.residuals < -2
  
  sum(HCHS$large.residuals)
  
  HCHS[HCHS$large.residuals,c("mean_conn_all", "age", "mean_thickness", "psmd", "standardized.residuals")]
  
  HCHS[HCHS$large.residuals,c("cooks.distance", "leverage", "covariance.ratios")]

  #Graphics
  
    HCHS$pc <- predict(prcomp(~mean_conn_all+age, HCHS))[,1]
    
    ggplot(HCHS,aes(x=age,y=mean_conn_all,color = mean_conn_all, fill = "transparent" ))+geom_point()  + 
      labs(x="Age", y="Global Connectivity") + 
      ggtitle("Global Connectivity \n in dependence of age") + 
      theme (legend.position = "none",plot.title = element_text(color="royalblue4", size=14, face="bold.italic", hjust = 0.5)) +
      geom_smooth(method=lm,) + scale_color_gradient(low = "#0091ff", high = "#f0650e")  +   theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
      )
    
  ### blaballasbaklsf
  