#include <string>
#include <algorithm>
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;
using namespace std; 

rowvec generateVOrdering(rowvec r);
rowvec sampleRho(mat data, double alpha,double sigma);
rowvec cdf(rowvec probs);
  
//import functions from R
Function ranking("rank");
Function rnorm_R("rnorm");

// [[Rcpp::export]]
rowvec generateVOrdering(rowvec r){
  int n = r.n_elem;
  rowvec V_ordering = r;
  rowvec distToCentre = r - (r.max()+1)/2;
  if (n % 2 == 1){
    //ordering[which(distToCentre==0)]<-1
    int i = as_scalar(find(distToCentre==0));
    V_ordering.col(i) = 1;
  }
  vec positiveDist = distToCentre(find(distToCentre>0));
  for(int i = 0; i<positiveDist.n_elem; i++){
    float ru=float(rand())/RAND_MAX ; //a random uniform between 0 and 1
    if(ru<0.5){
      V_ordering(as_scalar(find(distToCentre == positiveDist(i)))) = positiveDist(i)*2+1;
      V_ordering(as_scalar(find(distToCentre == (-1)*positiveDist(i)))) = positiveDist(i)*2;
      
    }else {
      V_ordering(as_scalar(find(distToCentre == positiveDist(i)))) = positiveDist(i)*2;
      V_ordering(as_scalar(find(distToCentre == (-1)*positiveDist(i)))) = positiveDist(i)*2+1;
    }
  }
  
  return V_ordering;
}

// [[Rcpp::export]]
rowvec sampleRho(mat data, double alpha,double sigma){
  int n = data.n_cols;
  int N = data.n_rows;
  rowvec colsums = sum(data,0);
  rowvec ranked1= as<rowvec>(wrap(ranking(colsums, true,"first"))); // without gaussian noise
  rowvec V_order = generateVOrdering(ranked1);
  rowvec with_gaussian_noise = as<rowvec>(wrap(rnorm_R(n,V_order,sigma))); //importing normal function from R is not absolutely necessary
  rowvec In = as<rowvec>(wrap(ranking(with_gaussian_noise, true,"first")));
  rowvec rho(n);
  Row<int> support(n);
  for(int i=0; i<n; i++){
    support(i) = i+1;
  }
  
  for(int i=0; i<n; i++){
    rowvec dists(support.n_elem);
    int i_curr = as_scalar(find(In == (i+1)));
    for(int j=0; j<support.n_elem; j++){
      dists(j) = sum(abs(data.col(i_curr)-support(j)));
    }
    rowvec log_num = (-alpha)*dists - max(-alpha/(n)*(dists)) ;
    double log_denom = log(sum(exp(log_num)));
    rowvec probs= exp((log_num-log_denom));
    
    double rand_u=double(rand())/RAND_MAX ; 
    int indOfCdf = support.n_elem - sum(rand_u <= cdf(probs));
    rho(i_curr) = support(indOfCdf);
    support.shed_col(as_scalar(find(support == rho(i_curr))));
  }
  return rho;

}

// [[Rcpp::export]]
rowvec cdf(rowvec probs){
  int length = probs.n_elem;
  rowvec probs_normalized = probs / sum(probs); //in case probs do not add to 1
  rowvec cdf_result(length);
  double sum_curr = 0;
  for(int i=0; i<length;i++){
    cdf_result(i) = (sum_curr+=probs_normalized(i));
  }
  return(cdf_result);
}

/*n<-dim(data)[2]
tmpRank<-rank(apply(data,2,mean),ties.method="first")
  In<-rank(rnorm(n, mean=generateVOrderings(tmpRank),sd = sigma))
  support<-1:n
  rho<-rep(0,n)*/