
set.seed(7253)
library(mvtnorm)
#### User Parameters ####

D<-1  ### User parameter change D<-100 for Q7 ###
X0<-rep(0,D)
h<-c(1,5,10)
sigma<-diag(rep(1,D))
max_iter<-1000

#### MH Algorithm ####

log_target<-function(x){
  return (dmvnorm(x,rep(0,D),sigma,log=TRUE))
}
x_prev<-X0
Sample_list <- list()

for (j in 1:length(h)){
  MCMC_chain<-matrix(0,max_iter,D)
  acc<-0
  for ( i in 1:max_iter) {
    x_curr<-rmvnorm(1,x_prev,h[j]*sigma)
    log_accept_prob<-log_target(x_curr)-log_target(x_prev)
    U<-runif(1,0,1)
    if (log(U)<log_accept_prob){
      x_prev<-x_curr
      acc<-acc+1
    }
    MCMC_chain[i,]<-x_prev
  }
  print(acc/max_iter)
  Sample_list[[j]]<-MCMC_chain
  
}

#### Trace plots for D=1 ####
if (D==1){
  plot.ts(Sample_list[[1]],ylab="h = 1")
  dev.print(pdf, 'h_1.pdf')
  
  plot.ts(Sample_list[[2]],ylab="h = 5")
  dev.print(pdf, 'h_5.pdf')
  
  plot.ts(Sample_list[[3]],ylab="h = 10")
  dev.print(pdf, 'h_10.pdf')
  
}
