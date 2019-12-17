rm(list=ls())
require(RcppArmadillo)
require(Rcpp)
require(BayesMallows)

sourceCpp("./clickingAlgo.cpp")
data<-potato_visual

sampleRho(data, 1.,1)
