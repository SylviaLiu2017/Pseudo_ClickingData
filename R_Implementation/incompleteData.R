rm(list=ls())

require(BayesMallows)
require(fields)
require(tidyr)
require(abind)

################preliminaries###################
source("./shared/allFunctions.R")
source("./shared/pseudoSamplingFuncs.R")
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


partialMethod <- "poisson"
if(partialMethod=="topK"){
  K<-5
  partialData<-createPartialData(origin_data,method = partialMethod,K=K)
}else if(partialMethod=="random"){
  removalRate<-0.5
  partialData<-createPartialData(origin_data,method = partialMethod,remove_rate = removalRate)
}else if(partialMethod=="poisson"){
  lambda<-7
  partialData<-createPartialData(origin_data,method = partialMethod,lambda = lambda)
}



if(sum(apply(partialData,1,containsZero)) == N){
  print("all lines contains missing values")
  dataInit<-partialData
  allSupports<-apply(partialData,1,ind_support)
  zero_positions<-(apply(partialData,1,zeroLocations))
  
}else{
  print("these lines are complete:")
  print(which((apply(partialData,1,containsZero))==FALSE))
}


for(i in 1:N){
  dataInit[i,][zero_positions[[i]]] <-allSupports[[i]]
}


n_samples<-5000
rhoSamples<-vector()
data_curr<-dataInit
sigma<-0
userArray3D<-dataInit
user2D<-dataInit
for(sample_i in 1:n_samples){
  print(sample_i)
  rho_curr<-sampleRho(user2D,sigma,30)
  rhoSamples<-rbind(rhoSamples,rho_curr)
  user2D<-t(apply(partialData,1,sampleForOneUser,rho_curr=rho_curr,alpha0=30))
  userArray3D<-abind(userArray3D,user2D,along = 3)
}

heatMat_pseudo<-heatMap(rhoSamples,rho0)

par(mai=c(1,1,0.65,1))
image(heatMat_pseudo,col=tim.colors(64*10),zlim=c(0,1),axes=F,cex.lab=2)
par(mai=c(1,1,0.65,1))
image.plot(heatMat_pseudo, zlim=c(0,1),legend.only=T,horizontal = F)

###################predict next K####################

userArray3D<-userArray3D[,,2001:n_samples]
K<-1
recs<-vector()
truth<-vector()

for(userj in 1:N){
  heatMat_userj<-heatMap(t(userArray3D[userj,,]),1:n)
  recs<-rbind(recs,recommendK(heatMat_userj,K, partialData[userj,],type = "partial"))
  truth<-rbind(truth, realTopK(partialData[userj,], origin_data[userj,],K))
}

################percentage correct?###################
numCorrect<-0
for(i in 1:N){
    numCorrect<-numCorrect+length(intersect(recs[i,],truth[i,]))  
  
}

numCorrect/(N*K)
