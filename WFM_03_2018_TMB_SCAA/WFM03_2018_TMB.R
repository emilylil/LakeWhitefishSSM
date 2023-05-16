# Lake Michigan 03 Stock Assessment in TMB
# Emily Morgan Liljestrand
# Created: 4/17/19
# Updated: 3/17/21
# Instructions for running
# 1) Load Libraries, clear variables, set working directory, compile and load model
# 2) Specify model attributes (Retrospective? Compared to ADMB? Simulations? Composition Likelihood?)
# 3) Run code section based on what you want to do 

#############################################################################
# SET UP THE DYNAMICS OF THE CODE (WD, ETC) AND RUN FUNCTIONS
#############################################################################

# Set up workspace, load libraries, compile and link model
############################################################################
library(TMB)
library(ggplot2)
library(RColorBrewer)
library(boot)
library(colorspace)
library(PerformanceAnalytics)

# Set up R by clearing workspace and specifying working directory to where file is saved
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
originalwd=getwd()
options(scipen=999) #turns off scientific notation in R

# Read in all the functions, including my data creation, parameter creation, etc.
# Having the functions as a different file just makes things neater below
source("WFM03_2018_TMB_Functions.R")
############################################################################

#############################################################################
# MODEL DETAILS AND COMPILATION SPECIFICATION SECTION:
#############################################################################
# How to fit the age-composition data (1- multinomial, 2-log gaussian cox process)?
agecompfit <- 1

if(agecompfit==1){
  modelname = "WFM03_2018_TMB_MN.cpp"
}

# Set up TMB specific elements from CPP: compile and load code
compile(modelname)
modelname=substr(modelname,1,nchar(modelname)-4)
dyn.load(dynlib(modelname))

############################################################################
# (1)
#Clear any previous data, parameters, bounds, obj, fit, or sdr files
rm(data,parameters,bounds,obj,fit,sdr)

#Specify the data and put it in a single list
data<-setdata() 
#Specify the parameters at starting values (ADMB estimates or ADMB starting values) and put it in a single list
parameters<-setparam_at_est(data) 
parameters<-setparam_at_init(data) 
#Specify the upper and lower bounds and put it in a single list "bounds"
# bounds<-setbounds(data)

#Make the model object without using phases, but specifying bounds and random effects
bounds<-setbounds(data)
#Optional random effects
reffects <- c("log_rec","logdevT_p1","effort_devsT","effort_devsG")
reffects <- c("effort_devsT","effort_devsG")
reffects <- c()

obj <- MakeADFun(data,parameters,DLL=modelname)
obj <- MakeADFun(data,parameters,DLL=modelname, map=list(lnM=as.factor(NA)))
obj <- MakeADFun(data,parameters,DLL=modelname, map=list(lnM=as.factor(NA)),random=reffects)
obj$report()$N

#Make the model fit using the specified model object and bounds or no bounds
fit <- nlminb(obj$par, obj$fn, obj$gr)
fit <- nlminb(obj$par, obj$fn, obj$gr,lower=bounds$lower_bounds,upper=bounds$upper_bounds)

#Report the model into a sdreport file using the model object
sdr <- sdreport(obj)

#FINAL RESULTS
summary(sdr)
#Did the model converge:
sdr$pdHess