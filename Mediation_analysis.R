setwd("C:/Users/mschu/Documents/Documents/CSI/R-Skripte/")

library(readxl)
library(ggplot2)
library(car)
library(scatterplot3d)
library(rgl)
library(MASS)
library(rcompanion)
library(QuantPsyc)


library(tidyverse)

library(ggplot2)
library(car)
library(scatterplot3d)
library(rgl)
library(MASS)
library(rcompanion)
library(skimr)

library(pander)
library(mediation)
library(diagram)
library(medflex)
library(psych) 

library(mice)
library(VIM)


summary(model.0)
lm.beta(model.0)
summary(model.M, standardized=TRUE)
lm.beta(model.M)
summary(model.Y, standardized=TRUE)
lm.beta(model.Y)
summary(med.out, standardized=TRUE, rsquare=TRUE)



model.0 <- lm(TMTB ~ age+ sex + education, HCHS)
model.M <- lm(seg   ~ age + sex + education, HCHS)
model.Y <- lm(TMTB ~ seg  +age + sex + education, HCHS)
med.out <- mediation::mediate(model.M, model.Y, treat='age', mediator='seg', boot=TRUE, sims=1000)