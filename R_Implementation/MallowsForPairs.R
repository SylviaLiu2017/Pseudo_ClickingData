# beach_tc
library(dplyr)

assessor<-vector()
bottom_item<-vector()
top_item<-vector()

for(i in 1:N){
  data<-clicking_data[i,]
  unclicked<-which(data==0)
  clicked<-which(data==1)
  for(item in clicked){
    top_item<-c(top_item,rep(item, length(unclicked)))
    bottom_item<-c(bottom_item,unclicked)
  }
  assessor<-c(assessor,rep(i, length(unclicked)*length(clicked)))

}

preferences<-data.frame(cbind(assessor,bottom_item,top_item))
tc<-preferences
model_fit <- compute_mallows(rankings = data_init,
                             nmc= 60000*5,
                             preferences = tc,
                             aug_thinning = 50,
                             rho_thinning = 50,
                             save_aug = TRUE,
                             verbose = TRUE)
long<-model_fit$augmented_data
wide <- spread(long, item, value)

wide<-wide[wide['iteration']>10000,]

K<-2
recsML<-vector()
truth<-vector()

for(userj in 1:N){
  heatMat_userj_ML<-heatMap(wide[wide[,2]==userj,3:(n+2)],1:n)
  recsML<-rbind(recsML,recommendK(heatMat_userj_ML,K, clicking_data[userj,],type="click"))
  truth<-rbind(truth, realTopK(clicking_data[userj,], origin_data[userj,],K,type="click"))
}

correctOrWrongML<-recsML

for(userj in 1:N){
  correctOrWrongML[userj,]<-recsML[userj,]%in%truth[userj,]
  
}

mean(correctOrWrongML)

rho_long<-model_fit$rho
rho_wide<-spread(rho_long,item,value)[,3:(n+2)]
rho_wide<-rho_wide[2001:6000,]


heatMat_ML<-heatMap(rho_wide,rho0)
par(mai=c(1,1,0.65,1))
image(heatMat_ML,col=tim.colors(64*10),zlim=c(0,1),axes=F,cex.lab=2,main="Mallows")
par(mai=c(1,1,0.65,1))
image.plot(heatMat_ML, zlim=c(0,1),legend.only=T,horizontal = F)

heatMat_pseudo<-heatMap(rhoSamples,rho0)
par(mai=c(1,1,0.65,1))
image(heatMat_pseudo,col=tim.colors(64*10),zlim=c(0,1),axes=F,cex.lab=2)
par(mai=c(1,1,0.65,1))
image.plot(heatMat_pseudo, zlim=c(0,1),legend.only=T,horizontal = F)

userj<-10
heatMat_userj<-heatMap(t(userArray3D[userj,,]),origin_data[userj,])
par(mai=c(1,1,0.65,1))
image(heatMat_userj,col=tim.colors(64*10),zlim=c(0,1),axes=F,cex.lab=2,main= paste("Pseudo, user", userj))
par(mai=c(1,1,0.65,1))
image.plot(heatMat_userj, zlim=c(0,1),legend.only=T,horizontal = F)

heatMat_userj_ML<-heatMap(wide[wide[,2]==userj,3:(n+2)],origin_data[userj,])
par(mai=c(1,1,0.65,1))
image(heatMat_userj_ML,col=tim.colors(64*10),zlim=c(0,1),axes=F,cex.lab=2,main = paste("Mallows, user",userj))
par(mai=c(1,1,0.65,1))
image.plot(heatMat_userj_ML, zlim=c(0,1),legend.only=T,horizontal = F)
