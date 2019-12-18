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
  alpha0<-3
  load("./shared/Cdfootrule.RData")
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
lambda<-5
clicking_data<-createClickData(origin_data,lambda)

centre_inferred<-n+1-rank(apply(clicking_data,2,sum),ties.method = "first")
data_init<-t(apply(clicking_data,1,generateInit_oneuser_2,centre = centre_inferred))

n_samples<-5000
rhoSamples<-vector()
data_curr<-data_init
sigma<-0
userArray3D<-data_init
user2D<-data_init
colnames(data_init)<-1:n

start <- proc.time()  
for(sample_i in 1:n_samples){
  if(sample_i%%100==0){
    print(sample_i)
  }
  rho_curr<-sampleRho(user2D,sigma,alpha0 = 10)
  rhoSamples<-rbind(rhoSamples,rho_curr)
  user2D<-t(apply(clicking_data,1,sampleForOneUserClicks,rho_curr=rho_curr,alpha0=10))
  userArray3D<-abind(userArray3D,user2D,along = 3)
}

duration2<-proc.time()-start

userArray3D<-userArray3D[,,1001:n_samples]
K<-2
recs<-vector()
truth<-vector()

for(userj in 1:N){
  heatMat_userj<-heatMap(t(userArray3D[userj,,]),1:n)
  recs<-rbind(recs,recommendK(heatMat_userj,K, clicking_data[userj,],type="click"))
  truth<-rbind(truth, realTopK(clicking_data[userj,], origin_data[userj,],K,type="click"))
}

correctOrWrong<-recs
for(userj in 1:N){
  correctOrWrong[userj,]<-recs[userj,]%in%truth[userj,]
  
}

mean(correctOrWrong)

heatMat_pseudo<-heatMap(rhoSamples,rho0)
par(mai=c(1,1,0.65,1))
image(heatMat_pseudo,col=tim.colors(64*10),zlim=c(0,1),axes=F,cex.lab=2, main="pseudo-likelihood")
par(mai=c(1,1,0.65,1))
image.plot(heatMat_pseudo, zlim=c(0,1),legend.only=T,horizontal = F)

