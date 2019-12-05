rm(list=ls())

require(BayesMallows)
require(fields)
require(tidyr)
require(abind)


################preliminaries###################
source("./allFunctions.R")
source("./pseudoSamplingFuncs.R")
compute_method<-"package"
data_mode<-"potatoes"

#################generate some data###################
if(data_mode=="simulation"){
  n<-20  
  N<-50
  rho0<-1:n
  alpha0<-5
  load("./Cdfootrule.RData")
  if(n>20){
    fitvec = estimate_partition_function(alpha_vector = seq(0.01,10,0.2), n_items = 50,metric = "footrule", nmc = 2000,degree=10)
  }
  origin_data<-sample_mallows(rho0 = rho0,alpha0 = alpha0, n_samples = N)
  
}else if (data_mode == "potatoes"){
  origin_data<-potato_visual
  rho0<-potato_true_ranking
  N<-dim(origin_data)[1]
  n<-dim(origin_data)[2]
}

n<-dim(origin_data)[2]
N<-dim(origin_data)[1]
lambda<-floor(n/3)
clicking_data<-createClickData(origin_data,lambda)

centre_inferred<-n+1-rank(apply(clicking_data,2,sum),ties.method = "first")
data_init<-t(apply(clicking_data,1,generateInit_oneuser_2,centre = centre_inferred))



n_samples<-100
rhoSamples<-vector()
data_curr<-data_init
sigma<-0
userArray3D<-data_init
user2D<-data_init

for(sample_i in 1:n_samples){
  print(sample_i)
  rho_curr<-sampleRho(user2D,sigma,10)
  rhoSamples<-rbind(rhoSamples,rho_curr)
  user2D<-t(apply(clicking_data,1,sampleForOneUserClicks,rho_curr=rho_curr,alpha0=10))
  userArray3D<-abind(userArray3D,user2D,along = 3)
}


