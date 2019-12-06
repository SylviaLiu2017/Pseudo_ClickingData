
iteration<-rep(0,N)
userArray3D_t<-as_tibble(cbind(iteration,data_init))


start <- proc.time()  
for(sample_i in 1:n_samples){
  print(sample_i)
  rho_curr<-sampleRho(user2D,sigma,alpha0 = 10)
  rhoSamples<-rbind(rhoSamples,rho_curr)
  user2D_t<-as_tibble(t(apply(clicking_data,1,sampleForOneUserClicks,rho_curr=rho_curr,alpha0=10)))
  colnames(user2D_t)<-1:n
  iteration<-rep(sample_i,N)
  user2D_t<-cbind(iteration,user2D_t)
  userArray3D_t<-rbind(userArray3D_t,user2D_t)
}
duration2<-proc.time()-start

