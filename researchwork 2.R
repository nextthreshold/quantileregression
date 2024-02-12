# Quantile Regression 
install.packages("quantreg")
library(quantreg)
library(performance)
library(ggridges)
library(ggplot2)
library(tidyverse)
dt <- read.csv("C:/Users/Owner/Desktop/Salary.csv")
data(engel)
lr<-lm(foodexp~income,data=engel)
summary(lr)
check_heteroscedasticity(lr)
qm50<-rq(foodexp~income,data=engel,tau=0.5)
AIC(lr,qm50)
head(dt)
dt1<-data.frame(dt$Education.Level,dt$Salary)
head(dt1)
lr<-lm(dt1$dt.Salary~dt1$dt.Education.Level,data = dt1)
lr
summary(lr)
check_outliers(lr)
check_heteroscedasticity(lr)
check_homogeneity(lr)
mr<-rq(dt1$dt.Salary~dt1$dt.Education.Level,data=dt1,tau=0.75)
mr
AIC(lr,mr)
ggplot(data=dt1,aes(x=dt1$dt.Salary,y=dt1$dt.Education.Level,fill=factor(stat(quantile))))+
  stat_density_ridges(
    geom="density_ridges_gradient",calc_ecdf = TRUE,quantile_lines = TRUE,quantiles = c(.1,.5,.9)
  )
x<-seq(0.1,0.9,by=0.1)
head(dt1)
q<-rq(dt.Education.Level~dt.Salary,data=dt1,tau = x)
summary(q)
summary(q) %>%
  plot(parm="salary.info")
summary(dt)
head(dt)
m1<-lm(Salary~Age+Education.Level+Years.of.Experience,data=dt)
plot_models(m1,quant)
quant<-rq(Salary~Age+Education.Level+Years.of.Experience,data=dt,tau = 0.75)


q <- rq(wage ~ jobclass + age + race, data = Wage, 
        tau = seq(.05, .95, by = 0.05))

summary(q) %>% 
  plot(c("jobclass2. Information", "age", "race2. Black", "race3. Asian"))
summary(q)
