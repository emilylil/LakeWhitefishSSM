# Lake Michigan 03 Stock Assessment in TMB
# Emily Morgan Liljestrand
# Created: 4/17/19
# Updated: 10/18/21
# Instructions for running
# 1) Load Libraries, load functions, clear variables, set working directory, compile and load model
# 2) Specify model attributes (Retrospective? Compared to ADMB? Simulations? Composition Likelihood?)
# 3) Run code section(s), descriptions specified below

#############################################################################
# SET UP THE DYNAMICS OF THE CODE (WD, ETC) AND RUN FUNCTIONS
#############################################################################

# Set up workspace, load libraries, compile and link model
############################################################################
# TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
# devtools::install_github("fishfollower/compResidual/compResidual")
# TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
# devtools::install_github("fishfollower/compResidual/compResidual", INSTALL_opts=c("--no-multiarch"),force=T)
library(TMB)
library(compResidual)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(boot)
library(colorspace)
library(PerformanceAnalytics)

# Set up R by clearing workspace and specifying working directory to where file is saved
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
originalwd=getwd()
options(scipen=999) #turns off scientific notation in R

source("WFM03_2018_TMB_SSM_Functions.R")
############################################################################

#############################################################################
# MODEL DETAILS SPECIFICATION SECTION:
#############################################################################
# How to fit the age-composition data (1- log normal and multinomial (MN), 2-multivariate lognormal (MVLN), 3-dirichlet multinomial (DMN))?
agecompfit <- 1
# Do you want the end results to be compared against the non-SSM ADMB model, Yes (T) or No (F)?
checkagainstADMB <- T

#If you're doing a multinomial, do you want to do the iterative process to estimate ESS?
estimateESS <- F
#Maximum number of times you're willing to iterate/reweight ESS before bailing out
maxreps <- 30
#If you are doing a multinomial, how do you wanna weight the ESS
ESS_w=c()
ESS_w[1] <- 0.1165
ESS_w[2] <- 0.0585
# ESS_w[1] <- 1
# ESS_w[2] <- 1

# Do you want to run simulations to check the model? 
dosimulation <- F
# If you do want to run simulations, how many total?
numsims <- 5

# Do you want to run retrospective analysis, Yes (T) or No (F)?
isretro <- F
# If you do want to run a retrospective analysis, which years do you want to start and end on?
retrostart <- 2017
retroend <- 2012
############################################################################


#############################################################################
# MODEL COMPILATION SECTION:
#############################################################################

if(agecompfit==1){
  modelname = "WFM03_2018_TMB_SSM_MN.cpp"
}
if(agecompfit==2){
  modelname = "WFM03_2018_TMB_SSM_MVLN.cpp"
}
if(agecompfit==3){
  modelname = "WFM03_2018_TMB_SSM_DMN.cpp"
}
if(agecompfit==4){
  modelname = "WFM03_2018_TMB_SSM_cLGCP.cpp"
}

modelname = "WFM03_2018_TMB_SSM_MN_OSA.cpp"
# Set up TMB specific elements from CPP: compile and load code
compile(modelname)
modelname=substr(modelname,1,nchar(modelname)-4)
dyn.load(dynlib(modelname))

# compile("compResidual.cpp")
# dyn.load("CompResidual")
#############################################################################
# CODE SECTION:
#############################################################################
# TABLE OF CONTENTS
# (1) Fitting the model using prespecified functions
# (2) Iterative reweighting to find the ESS using prespecified functions
# (3a) Simulating catch and composition data inside cpp
# (3b) Simulating catch and composition data inside R
# (4) Running a retrospective analysis (Note: must run #1 at least once before #4)

# (1)
############################################################################
#Clear any previous data, parameters, bounds, obj, fit, or sdr files
rm(data,parameters,bounds,obj,fit,sdr)

#Specify the data and put it in a single list
data<-setdata(ESS_W_T=ESS_w[1],ESS_W_G=ESS_w[2],agecompfit=agecompfit) 
#Specify the parameters and put it in a single list
# parameters<-setparam(data,agecompfit,logit_rhoalphaT=2.197225,logit_rhoalphaG=2.197225)
# log_siglow <- log(0.0667605*0.5)
# log_sighigh <- log(0.0667605*1.5)

parameters<-setparam(data,agecompfit,log_sig = -2.70663320551)
# parameters<-setparam(data,agecompfit,log_sig = log_sighigh)
#Specify the upper and lower bounds and put it in a single list
bounds<-setbounds(agecompfit)

#Make the model object using the specified data and parameters
obj <- setmodel(data,parameters,agecompfit,modelname)
#Make the model fit using the specified model object and bounds
fit <- fitmodel(obj,bounds)
# fit <- fitmodel(obj,bounds,mymapinput=list())
#Report the model into a sdreport file using the model object
sdr <- reportmodel(obj)
sdr <- sdreport(obj)
# summary(sdr)

# OSA Residuals Calculation:
# dataOSA <-setdataOSA(ESS_W_T=ESS_w[1],ESS_W_G=ESS_w[2],agecompfit=agecompfit)
# parameters<-setparam(dataOSA,agecompfit,log_sig = -2.70663320551)
# bounds<-setbounds(agecompfit)
# objOSA <- setmodel(dataOSA,parameters,agecompfit,modelname)
# fit <- fitmodel(objOSA,bounds)
# # sdr <- sdreport(objOSA)
#   
# osacompTGeneric <- suppressWarnings(oneStepPredict(objOSA, observation.name = "in_obs_PAT", data.term.indicator="keepPAT", method="oneStepGeneric",discrete=F))
# write.table(osacompTGeneric$residual,file=paste0("osacompTGenericFullFix",".txt"))
# 
# osacompGGeneric <- suppressWarnings(oneStepPredict(objOSA, observation.name = "in_obs_PAG", data.term.indicator="keepPAG", method="oneStepGeneric",discrete=F))
# write.table(osacompGGeneric$residual,file=paste0("osacompGGenericFullFix",".txt"))
# 
# jittervalues <- seq(1.005,1.1,by=0.005)
# for(i in 1:length(jittervalues))
# {
#   dataOSA <-setdataOSA(ESS_W_T=ESS_w[1],ESS_W_G=ESS_w[2],agecompfit=agecompfit,jitter=jittervalues[i])
#   parameters<-setparam(dataOSA,agecompfit,log_sig = -2.70663320551)
#   bounds<-setbounds(agecompfit)
#   objOSA <- setmodel(dataOSA,parameters,agecompfit,modelname)
#   fit <- fitmodel(objOSA,bounds)
#   # sdr <- sdreport(objOSA)
#   
#   osacompTGeneric <- suppressWarnings(oneStepPredict(objOSA, observation.name = "in_obs_PAT", data.term.indicator="keepPAT", method="oneStepGeneric",discrete=F,subset=c(1:13)))
#   write.table(osacompTGeneric$residual,file=paste0("osacompTGenericMicroFix",jittervalues[i]*100,".txt"))
#   
# }
# 
# jittervalues<- seq(1.005,1.1,by=0.005)
# for(i in 1:length(jittervalues))
# {
#   dataOSA <-setdataOSA(ESS_W_T=ESS_w[1],ESS_W_G=ESS_w[2],agecompfit=agecompfit,jitter=jittervalues[i])
#   parameters<-setparam(dataOSA,agecompfit,log_sig = -2.70663320551)
#   bounds<-setbounds(agecompfit)
#   objOSA <- setmodel(dataOSA,parameters,agecompfit,modelname)
#   fit <- fitmodel(objOSA,bounds)
#   # sdr <- sdreport(objOSA)
#   
#   osacompGGeneric <- suppressWarnings(oneStepPredict(objOSA, observation.name = "in_obs_PAG", data.term.indicator="keepPAG", method="oneStepGeneric",discrete=F,reverse=T,subset=c(41:51)))
#   write.table(osacompGGeneric$residual,file=paste0("osacompGGenericMicroSubsetFix",jittervalues[i]*100,".txt"))
# }
# 
# # osacompGGeneric <- suppressWarnings(oneStepPredict(objOSA, observation.name = "in_obs_PAG", data.term.indicator="keepPAG", method="oneStepGeneric",discrete=F))
# # write.table(osacompGGeneric$residual,file="osacompGGenericFull.txt")
# 
# # osaharvTGeneric <- suppressWarnings(oneStepPredict(objOSA, observation.name = "in_harvT", data.term.indicator="keepHarvT", method="oneStepGeneric",discrete=F,reverse=T))
# # osaharvGGeneric <- suppressWarnings(oneStepPredict(objOSA, observation.name = "in_harvG", data.term.indicator="keepHarvG", method="oneStepGeneric",discrete=F,reverse=T))


#Make a new directory to save this version of the model run
newwd <- setnewwd(originalwd,isretro,data=data)
#Save results (txt files)
saveresults(originalwd,newwd,sdr)
#Save plots depending on if intended for presentation or publication 
# savegraphspresentation(checkagainstADMB,originalwd,newwd,sdr,data)
savegraphspublication(checkagainstADMB,originalwd,newwd,sdr,data)
#Save residual plots
# saveresiduals(originalwd,newwd,sdr,dataOSA)
#Just in case one of the functions didn't do it, return the working directory to where the code is
setwd(originalwd)
############################################################################

# (2)
############################################################################
#If you want to iteratively reweight the effective sample size of composition data, do this to calculate a new sdr
if(estimateESS==T)
{
  # Based on that initial run, check the variance of the age composition (model_var) and the weights to apply to the ESS (ESS_W)
  model_var<-calcvar(sdr,data) 
  ESS_w<-calcw(sdr,data) 
  if(ESS_w[1]>1){
    ESS_w[1]<-0.05
  }
  if(ESS_w[2]>1){
    ESS_w[2]<-0.05
  }
  
  #If the initial variance of the age composition (model_var) is not 1, proceed with the iterative reweighting procedure:
  if(model_var>1.05||model_var<0.95)
  {
    #To run the remainder of the ESS calclations, we have to fix the SD values at their initial estimated numbers
    log_sig_fix <- parameters$log_sig
    log_sdSR_fix <- summary(sdr)[which(row.names(summary(sdr))=="log_sdSR"),1]  
    log_sd_logeffortT_fix <- summary(sdr)[which(row.names(summary(sdr))=="log_sd_logeffortT"),1]
    log_sd_logeffortG_fix <- summary(sdr)[which(row.names(summary(sdr))=="log_sd_logeffortG"),1]
    logit_rhoalphaT_fix <- summary(sdr)[which(row.names(summary(sdr))=="logit_rhoalphaT"),1] 
    logit_rhoalphaG_fix <- summary(sdr)[which(row.names(summary(sdr))=="logit_rhoalphaG"),1]
    
    #While the variance is still not 1, recalculate the expected proportions at age and find results
    nreps=0
    while((model_var[1]>1.05||model_var[1]<0.95)||(model_var[2]>1.05||model_var[2]<0.95))
    {
      if(nreps>maxreps){break}
      data.ess<-setdata(ESS_W_T=ESS_w[1],ESS_W_G=ESS_w[2])
      parameters.ess<-setparam(data.ess,agecompfit,log_sig = log_sig_fix,log_sdSR = log_sdSR_fix,log_sd_logeffortT = log_sd_logeffortT_fix,log_sd_logeffortG = log_sd_logeffortG_fix)
      # parameters.ess <- setparam(data=data.ess)
      bounds.ess<-setbounds()
      
      obj.ess<-setmodel(data.ess,parameters.ess,agecompfit,modelname,mymapinput = list(log_sig=as.factor(NA),log_sdSR=as.factor(NA),log_sd_logeffortT=as.factor(NA),log_sd_logeffortG=as.factor(NA)))
      # obj.ess<-setmodel(data=data.ess,parameters=parameters.ess)
      fit.ess<-fitmodel(obj.ess,bounds.ess)
      sdr.ess <- reportmodel(obj.ess)
      
      model_var<-calcvar(sdr.ess,data.ess) #Calculate the variance in age composition
      ESS_w<-calcw(sdr.ess,data.ess) 
      nreps=nreps+1
      
      if(ESS_w[1]>1){
        ESS_w[1]<-(1/nreps)
      }
      if(ESS_w[2]>1){
        ESS_w[2]<-(1/nreps)
      }
      #At this point, the model either found a set of weights for which the estimated variance is 1, or fell out after 20 iterations
    }
  }
  if(model_var<1.05&&model_var>0.95)
  {
    data<-setdata(ESS_W_T=ESS_w[1],ESS_W_G=ESS_w[2]) 
    #Specify the parameters and put it in a single list
    parameters<-setparam(data,agecompfit) 
    #Specify the upper and lower bounds and put it in a single list
    bounds<-setbounds()
    
    #Make the model object using the specified data and parameters
    obj <- setmodel(data,parameters,agecompfit,modelname)
    #Make the model fit using the specified model object and bounds
    fit <- fitmodel(obj,bounds)
    #Report the model into a sdreport file using the model object
    sdr <- reportmodel(obj)
  }
  else{print("Iterative reweighting failed to find weights where observed and expected variance match")}
}
############################################################################

# (3a)
############################################################################

if(dosimulation==T)
{
  simresults <- simmodel(data,obj,parameters,agecompfit,modelname,originalwd,newwd,estimateESS,maxreps,numsims,ESS_W_T_SIM=ESS_w)
}
############################################################################

# (3b)
############################################################################
rm(data,parameters,bounds,obj,fit,sdr)
#Specify the data and put it in a single list
simparams<-unname(fit$par)
data<-setsimdata(2017,ESS_w[1],ESS_w[2],agecompfit)
#Specify the parameters and put it in a single list
parameters<-setparam(data,agecompfit,logit_rhoalphaT=2.197225,logit_rhoalphaG=2.197225)
# parameters<-setparam(data,agecompfit,log_sig = log(0.32)) 
#Specify the upper and lower bounds and put it in a single list
bounds<-setbounds(agecompfit)

#Make the model object using the specified data and parameters
obj <- setmodel(data,parameters,agecompfit,modelname)
#Make the model fit using the specified model object and bounds
fit <- fitmodel(obj,bounds)
fit<- nlminb(obj$par, obj$fn, obj$gr,lower=bounds$lower_bounds,upper=bounds$upper_bounds)
#Report the model into a sdreport file using the model object
sdr <- reportmodel(obj)
sdr <- sdreport(obj)
# summary(sdr)

#Make a new directory to save this version of the model run
newwd <- setnewwd(originalwd,isretro,data=data)
#Save results (txt files)
saveresults(originalwd,newwd,sdr)
#Save plots depending on if intended for presentation or publication 
savegraphspresentation(checkagainstADMB,originalwd,newwd,sdr,data)
# savegraphspublication(checkagainstADMB,originalwd,newwd,sdr,data)
#Save residual plots
saveresiduals(originalwd,newwd,sdr,data)
#Just in case one of the functions didn't do it, return the working directory to where the code is
setwd(originalwd)

############################################################################

# (4)
############################################################################

rm(data,parameters,bounds,obj,fit,sdr)
if(isretro==T)
{
  # A For loop to run retrospective analysis as far as I want
  for(retroyr in retrostart:retroend)
  {
    # A regular analysis
    data<-setdata(retroyear=retroyr,ESS_W_T=ESS_w[1],ESS_W_G=ESS_w[2])
    #Specify the parameters and put it in a single list
    parameters<-setparam(data,agecompfit) 
    #Specify the upper and lower bounds and put it in a single list
    bounds<-setbounds(agecompfit)
    
    #Make the model object using the specified data and parameters
    obj <- setmodel(data,parameters,agecompfit,modelname)
    #Make the model fit using the specified model object and bounds
    fit <- fitmodel(obj,bounds)
    #Report the model into a sdreport file using the model object
    sdr <- reportmodel(obj)
    
    if(retroyr==retrostart){retrowd <- setretrowd(originalwd)}
    newwd <- setnewwd(originalwd,isretro,retroyr=retroyr,retroend,retrostart,data,retrowd)
    
    saveresults(originalwd,newwd,sdr)
    # savegraphspresentation(checkagainstADMB,originalwd,newwd,sdr,data)
    # saveresiduals(originalwd,newwd,sdr,data)
    
    setwd(originalwd)
  }
  saveretroplots(originalwd,retrowd,retrostart,retroend,data)
}