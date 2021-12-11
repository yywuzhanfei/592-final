library(tidyverse)
library(survival)
library(survminer)
library(survMisc)
library(readr)
library(haven)
library(lubridate)
library(mfp)
library(haven)
library(emmeans)
library(AER)
library(MASS)
library(margins)
library(secr)
library(magrittr)
library("readxl")
library(skimr)
library(data.table)




co<-read_excel("Consolidated data_vF.xlsx")
co<-co%>%
  mutate(percent65.c=Percent_Pop_Above_65-mean(Percent_Pop_Above_65,na.rm=TRUE),
         density.c=Pop_Density-mean(Pop_Density,na.rm=TRUE),
         temp.c=Avg_Temp-mean(Avg_Temp,na.rm=TRUE),
         ghsa_score.c=Overall_GHSA_Score-mean(Overall_GHSA_Score,na.rm=TRUE),
         household_size.c=Household_Size_2019-mean(Household_Size_2019,na.rm=TRUE),
         stringency_index.c=Stringency_Index-mean(Stringency_Index,na.rm=TRUE),
         tests.c=Tests_Mn_30_Days_After_100th_Case-mean(Tests_Mn_30_Days_After_100th_Case,na.rm=TRUE),
         population=Population_2020,
         deaths=Deaths_Mn_30_Days_After_100th_Case
         )

co<-data.table(co)
co<-co[,.(percent65.c,density.c,temp.c,ghsa_score.c,household_size.c,
           stringency_index.c,tests.c,population,deaths)]

#univariable analysis
#independent variables of interest 
#percent65.c
mfp::mfp(deaths ~ fp(percent65.c) + offset(log(population)), 
         family = poisson,verbose = TRUE,
         data = co)

um1 <-
  glm(deaths ~ I(((percent65.c+0.1)/0.1)^3) + offset(log(population)), 
      family = poisson,
      data = co)
summary(um1)
#coviates
#stringency_index.c
mfp::mfp(deaths ~ fp(stringency_index.c) + offset(log(population)), 
         family = poisson,verbose = TRUE,
         data = co)
um2 <-
  glm(deaths ~ I(((stringency_index.c+46.8)/100)^3) + offset(log(population)), 
      family = poisson,
      data = co)
summary(um2)  

#tests.c
mfp::mfp(deaths ~ fp(tests.c) + offset(log(population)), 
         family = poisson,verbose = TRUE,
         data = co)
um3 <-
  glm(deaths ~ I(log((tests.c+5532.3)/10000)) + offset(log(population)), 
      family = poisson,
      data = co)
summary(um3)

#density.c
mfp::mfp(deaths ~ fp(density.c) + offset(log(population)), 
         family = poisson,
         data = co)

um4 <-
  glm(deaths ~ I(((density.c+147.3)/100)^0.5)+I(((density.c+147.3)/100)^1) + offset(log(population)), 
      family = poisson,
      data = co)
summary(um4)

#temp.c
mfp::mfp(deaths ~ fp(temp.c) + offset(log(population)), 
         family = poisson,
         data = co)

um5 <-
  glm(deaths ~ I(((temp.c+18.3)/10)^1)+I(((temp.c+18.3)/10)^2) + offset(log(population)), 
      family = poisson,
      data = co)
summary(um5)



#household_size.c
mfp::mfp(deaths ~ fp(household_size.c) + offset(log(population)), 
         family = poisson,
         data = co)

um6 <-
  glm(deaths ~ log((household_size.c+1.3))+I(log((household_size.c+1.3))^2) + offset(log(population)), 
      family = poisson,
      data = co)
summary(um6)

#ghsa_score.c

mfp::mfp(deaths ~ fp(ghsa_score.c) + offset(log(population)), 
         family = poisson,
         data = co)

um7 <-
  glm(deaths ~ I(((ghsa_score.c+30.2)/10)^2)+I(((ghsa_score.c+30.2)/10)^3) + offset(log(population)), 
      family = poisson,
      data = co)
summary(um7)

glm.re <- function(GLM.RESULT, digits = 2) {
  
 
  
  COEF      <- stats::coef(GLM.RESULT)
  CONFINT   <- stats::confint(GLM.RESULT)
  P_VALUE   <- summary(GLM.RESULT)$coefficients[,4]
  SD   <- summary(GLM.RESULT)$coefficients[,2]
  
  TABLE <- cbind(COEF, SD)
  TABLE     <- cbind(TABLE, CONFINT)
  
  TABLE.EXP <- cbind(TABLE, P_VALUE)
  
  colnames(TABLE.EXP)[1] <- "Coefficient"
  colnames(TABLE.EXP)[2] <- "SD"
  colnames(TABLE.EXP)[5] <- "P_VALUE"
  TABLE.EXP
  return(TABLE.EXP)
}
tab11<-glm.re(um1)
tab12<-glm.re(um2)
tab13<-glm.re(um3)
tab14<-glm.re(um4)
tab15<-glm.re(um5)
tab16<-glm.re(um6)
tab17<-glm.re(um7)

tab1=rbind(tab11,tab12,tab13,tab14,tab15,tab16,tab17)
tab1

#multivariable analysis





subset_prelim <-
  MASS::stepAIC(
    glm(deaths ~ I(((percent65.c + 0.1)/0.1)^3)+
          I(((stringency_index.c + 46.8)/100)^3) + 
          I(log((tests.c + 5532.3)/10000))+
          I(((density.c + 147.3)/100)^0.5)+I(((density.c + 147.3)/100)^1)+
          I(((temp.c + 18.3)/10)^1) +I(((temp.c + 18.3)/10)^2) +
          I(log((household_size.c + 1.3))) +I(log((household_size.c + 1.3))^2) +
          I(((ghsa_score.c+30.2)/10)^2)+I(((ghsa_score.c+30.2)/10)^3) + offset(log(population)),
        data=co, family = poisson),
    scope = list(upper =  ~ I(((percent65.c + 0.1)/0.1)^3)+
                   I(((stringency_index.c + 46.8)/100)^3) + 
                   I(log((tests.c + 5532.3)/10000))+
                   I(((density.c + 147.3)/100)^0.5)+I(((density.c + 147.3)/100)^1)+
                   I(((temp.c + 18.3)/10)^1) +I(((temp.c + 18.3)/10)^2) +
                   I(log((household_size.c + 1.3))) +I(log((household_size.c + 1.3))^2) +
                   I(((ghsa_score.c+30.2)/10)^2)+I(((ghsa_score.c+30.2)/10)^3) + offset(log(population)),
                 lower = ~1),
    direction = "backward"
  )
subset_prelim %>% summary()

#re-assess linearity

mfp::mfp(deaths ~ fp(I(((percent65.c + 0.1)/0.1)^3))+
           I(((stringency_index.c + 46.8)/100)^3) + 
           I(log((tests.c + 5532.3)/10000))+
           I(((density.c + 147.3)/100)^0.5)+I(((density.c + 147.3)/100)^1)+
           I(((temp.c + 18.3)/10)^1) +I(((temp.c + 18.3)/10)^2) +
           I(log((household_size.c + 1.3))) +I(log((household_size.c + 1.3))^2) +
           I(((ghsa_score.c+30.2)/10)^2)+I(((ghsa_score.c+30.2)/10)^3) + offset(log(population)), 
         family = poisson,
         data = co)

mfp::mfp(deaths ~ I(((percent65.c + 0.1)/0.1)^3)+
           I(((stringency_index.c + 46.8)/100)^3) + 
           fp(I(log((tests.c + 5532.3)/10000)))+
           I(((density.c + 147.3)/100)^0.5)+I(((density.c + 147.3)/100)^1)+
           I(((temp.c + 18.3)/10)^1) +I(((temp.c + 18.3)/10)^2) +
           I(log((household_size.c + 1.3))) +I(log((household_size.c + 1.3))^2) +
           I(((ghsa_score.c+30.2)/10)^2)+I(((ghsa_score.c+30.2)/10)^3) + offset(log(population)), 
         family = poisson,
         data = co)

mfp::mfp(deaths ~ I(((percent65.c + 0.1)/0.1)^3)+
           fp(I(((stringency_index.c + 46.8)/100)^3)) + 
           I(log((tests.c + 5532.3)/10000))+
           I(((density.c + 147.3)/100)^0.5)+I(((density.c + 147.3)/100)^1)+
           I(((temp.c + 18.3)/10)^1) +I(((temp.c + 18.3)/10)^2) +
           I(log((household_size.c + 1.3))) +I(log((household_size.c + 1.3))^2) +
           I(((ghsa_score.c+30.2)/10)^2)+I(((ghsa_score.c+30.2)/10)^3) + offset(log(population)), 
         family = poisson,
         data = co)

mfp::mfp(deaths ~ I(((percent65.c + 0.1)/0.1)^3)+
           I(((stringency_index.c + 46.8)/100)^3) + 
           I(log((tests.c + 5532.3)/10000))+
           I(((density.c + 147.3)/100)^0.5)+I(((density.c + 147.3)/100)^1)+
           I(((temp.c + 18.3)/10)^1) +I(((temp.c + 18.3)/10)^2) +
           I(log((household_size.c + 1.3))) +I(log((household_size.c + 1.3))^2) +
           I(((ghsa_score.c+30.2)/10)^2)+I(((ghsa_score.c+30.2)/10)^3) + offset(log(population)), 
         family = poisson,
         data = co)



#evaluate overdispersion

AER::dispersiontest(subset_prelim)



prelim3 <- MASS::glm.nb(deaths ~ I(((percent65.c + 0.1)/0.1)^3)*I(((density.c + 147.3)/100)^1)+
                          I(((stringency_index.c + 46.8)/100)^3)+
                          I(((density.c + 147.3)/100)^0.5)+I(log((tests.c + 5532.3)/10000))+
                          I(((temp.c + 18.3)/10)^1) +I(((temp.c + 18.3)/10)^2) +
                          I(log((household_size.c + 1.3))) +I(log((household_size.c + 1.3))^2) +
                          I(((ghsa_score.c+30.2)/10)^2)+I(((ghsa_score.c+30.2)/10)^3) + offset(log(population)),
                        link = log, 
                        data = co)
summary(prelim3)

prelim4 <- MASS::glm.nb(deaths ~ I(((percent65.c + 0.1)/0.1)^3)*I(((stringency_index.c + 46.8)/100)^3)+
                          I(log((tests.c + 5532.3)/10000))+
                          I(((density.c + 147.3)/100)^0.5)+
                          I(((density.c + 147.3)/100)^1)+
                          I(((temp.c + 18.3)/10)^1) +I(((temp.c + 18.3)/10)^2) +
                          I(log((household_size.c + 1.3))) +I(log((household_size.c + 1.3))^2) +
                          I(((ghsa_score.c+30.2)/10)^2)+I(((ghsa_score.c+30.2)/10)^3) + offset(log(population)),
                        link = log, 
                        data = co)
summary(prelim4)

finalmodel <- MASS::glm.nb(deaths ~ I(((percent65.c + 0.1)/0.1)^3)+I(((stringency_index.c + 46.8)/100)^3) + 
                          I(log((tests.c + 5532.3)/10000))+
                          I(((density.c + 147.3)/100)^0.5)+I(((density.c + 147.3)/100)^1)+
                          I(((temp.c + 18.3)/10)^1) +I(((temp.c + 18.3)/10)^2) +
                          I(log((household_size.c + 1.3))) +I(log((household_size.c + 1.3))^2) +
                          I(((ghsa_score.c+30.2)/10)^2)+I(((ghsa_score.c+30.2)/10)^3) + offset(log(population)),
                        link = log, 
                        data = co)
summary(finalmodel)
glm.re(finalmodel)

library(car)
influencePlot(finalmodel)

nullmodel <- MASS::glm.nb(deaths ~ 1 + offset(log(population)),
                        link = log, 
                        data = co)
anova(finalmodel,nullmodel,test="LRT")

pois_pearson_gof <-
  function(model) {
    return(
      list(
        pval = tibble(
          pred = predict(model, type = "response"),
          y = model$y
        ) %>%
        {sum((.$y - .$pred)^2/.$pred)} %>%
          pchisq(., model$df.residual, lower.tail = F),
        df = model$df.residual,
        chisqvalue=tibble(
          pred = predict(model, type = "response"),
          y = model$y
        ) %>%
        {sum((.$y - .$pred)^2/.$pred)}
      )
    )
  }

pois_dev_gof <-
  function(model) {
    return(
      list(
        pval = pchisq(model$deviance, model$df.residual, lower.tail=F),
        df = model$df.residual,
        chisqvalue=model$deviance
      )
    )
  }
pois_pearson_gof(finalmodel)
pois_dev_gof(finalmodel)






