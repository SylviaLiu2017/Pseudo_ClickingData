in_path<-"../relevantSimulationResults/theorem2.3/RData/"
out_path<-"../relevantSimulationResults/theorem2.3/csv/"

Ns<-c(100,200,500,1000)
ns<-c(30,35,40,45)

for(N in Ns){
  for(n in ns){
    load(paste(in_path,"N",N,"n",n,".RData",sep=""))
    write.csv(KLTable,col.names = FALSE, file=paste(out_path,"N",N,"n",n,".csv",sep=""))
  }
}

