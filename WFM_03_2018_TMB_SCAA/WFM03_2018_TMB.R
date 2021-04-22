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
# MODEL DETAILS SPECIFICATION SECTION:
#############################################################################
# How to fit the age-composition data (1- multinomial, 2-log gaussian cox process)?
agecompfit <- 1
#If you're doing a multinomial, do you want to do the iterative process to estimate ESS?
estimateESS <- F
#Maximum number of times you're willing to iterate/reweight ESS before bailing out
maxreps <- 30
#If you are doing a multinomial, how do you wanna weight the ESS

ESS_w=c()
# The weights that approximate ESS that was estimated via iterative reweighting for ADMB
ESS_w[1] <- 0.1165
ESS_w[2] <- 0.0585
# Weights that assume effective sample size equals sample size
# ESS_w[1] <- 1
# ESS_w[2] <- 1

# Do you want to run retrospective analysis, Yes (T) or No (F)?
isretro <- F
# If you do want to run a retrospective analysis, which years do you want to start and end on?
retrostart <- 2017
retroend <- 2007

# Do you want to run simulations to check the model? 
dosimulation <- F
# If you do want to run simulations, how many total?
numsims <- 5

# Do you want the end results to be compared against the non-SSM ADMB model, Yes (T) or No (F)?
checkagainstADMB <- F
############################################################################


#############################################################################
# MODEL COMPILATION SECTION:
#############################################################################

if(agecompfit==1){
  modelname = "WFM03_2018_TMB_MN.cpp"
}
if(agecompfit==2){
  modelname = "WFM03_2018_TMB_cLGCP.cpp"
}

# Set up TMB specific elements from CPP: compile and load code
compile(modelname)
modelname=substr(modelname,1,nchar(modelname)-4)
dyn.load(dynlib(modelname))


#############################################################################
# CODE SECTION:
#############################################################################

# (1) Specifying, fitting, and reporting a "basic" model including phases
# (2) Fitting the model WITHOUT the phases using prespecified functions
# (3) Iterative reweighting to find the ESS using prespecified functions
# (4) Simulating catch and composition data
# (5) Running a retrospective analysis

############################################################################
# (1)
#Clear any previous data, parameters, bounds, obj, fit, or sdr files
rm(data,parameters,bounds,obj,fit,sdr)

#Specify the data and put it in a single list
data<-setdata(ESS_W_T=ESS_w[1],ESS_W_G=ESS_w[2]) 
#Specify the parameters at starting values (ADMB estimates or ADMB starting values) and put it in a single list
parameters<-setparam_at_est(data) 
parameters<-setparam_at_init(data) 
#Specify the upper and lower bounds and put it in a single list "bounds"
# bounds<-setbounds(data)

#Fit the model using three phases using mapping, such as done in the ADMB model:
#PHASE 1
bounds<-setbounds1(data)
mymap.1 <- list(log_sig=as.factor(NA),lnM=as.factor(NA),
              logselG_p1=as.factor(NA),logselG_p2=as.factor(NA),
              logselT_p1=as.factor(NA),logselT_p2=as.factor(NA),
              logdevT_p1=as.factor(rep(NA,length(parameters$logdevT_p1))),
              log_recdev=as.factor(rep(NA,length(parameters$log_recdev))),
              effort_devsT=as.factor(rep(NA,length(parameters$effort_devsT))),
              effort_devsG=as.factor(rep(NA,length(parameters$effort_devsT))))
obj <- MakeADFun(data,parameters,DLL=modelname, map=mymap.1)
# fit <- nlminb(obj$par, obj$fn, obj$gr,lower=bounds$lower_bounds,upper=bounds$upper_bounds)
fit <- nlminb(obj$par, obj$fn, obj$gr)
# sdr <- sdreport(obj)
# summary(sdr)

#PHASE 2
parameters.2 <- obj$env$parList(fit$par) # Parameter estimate after phase 1
bounds<-setbounds2(data)
mymap.2 <- list(log_sig=as.factor(NA),lnM=as.factor(NA),
              effort_devsT=as.factor(rep(NA,length(parameters$effort_devsT))),
              effort_devsG=as.factor(rep(NA,length(parameters$effort_devsT))))
obj <- MakeADFun(data,parameters.2,DLL=modelname, map=mymap.2)
# fit <- nlminb(obj$par, obj$fn, obj$gr,lower=bounds$lower_bounds,upper=bounds$upper_bounds)
fit <- nlminb(obj$par, obj$fn, obj$gr)
# sdr <- sdreport(obj)
# summary(sdr)

#PHASE 3
parameters.3 <- obj$env$parList(fit$par) # Parameter estimate after phase 2
bounds<-setbounds(data)
mymap.3 <- list(lnM=as.factor(NA))
obj <- MakeADFun(data,parameters.3,DLL=modelname,map=mymap.3)
# fit <- nlminb(obj$par, obj$fn, obj$gr,lower=bounds$lower_bounds,upper=bounds$upper_bounds)
fit <- nlminb(obj$par, obj$fn, obj$gr)
sdr <- sdreport(obj)

#FINAL RESULTS
summary(sdr)
#Did the model converge:
sdr$pdHess

#Make a new directory to save this version of the model run
newwd <- setnewwd(originalwd,isretro,data=data)
#Save results (txt files), graphs (graphics), and residuals (residual graphics)
saveresults(originalwd,newwd,sdr)
savegraphs(checkagainstADMB,originalwd,newwd,sdr,data)
saveresiduals(originalwd,newwd,sdr,data)
#Just in case one of the functions didn't do it, return the working directory to where the code is
setwd(originalwd)

############################################################################
# (2)
#Make the model object without using phases, but specifying bounds and random effects
bounds<-setbounds(data)
#Optional random effects
reffects <- c("log_rec","logdevT_p1","effort_devsT","effort_devsG")
reffects <- c("effort_devsT","effort_devsG")

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

############################################################################
# (3) Iterative reweighting of ESS (UNDER CONSTRUCTION)
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
# (4) Simulations (UNDER CONSTRUCTION)
if(dosimulation==T)
{
  simresults <- simmodel(data,obj,parameters,agecompfit,modelname,originalwd,newwd,estimateESS,maxreps,numsims,ESS_W_T_SIM=ESS_w)
}

############################################################################
# (5) Retrospective (UNDER CONSTRUCTION)
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
    bounds<-setbounds()
    
    #Make the model object using the specified data and parameters
    obj <- setmodel(data,parameters,agecompfit,modelname)
    #Make the model fit using the specified model object and bounds
    fit <- fitmodel(obj,bounds)
    #Report the model into a sdreport file using the model object
    sdr <- reportmodel(obj)

    if(retroyr==retrostart){retrowd <- setretrowd(originalwd)}
    newwd <- setnewwd(originalwd,isretro,retroyr=retroyr,retroend,retrostart,data,retrowd)
    
    saveresults(originalwd,newwd,sdr)
    savegraphs(checkagainstADMB,originalwd,newwd,sdr,data)
    saveresiduals(originalwd,newwd,sdr,data)
    
    setwd(originalwd)
  }
  saveretroplots(originalwd,retrowd,retrostart,retroend,data)
}
############################################################################