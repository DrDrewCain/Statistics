 
mini.boot <-function(vec0, nboot=100,stat=mean,alpha=0.05){ # Uses bootstrap that takes in the median as estimate
  populationSize <- length(vec0) # populationsize as the length of initalvector
  bootvec2 <-  NULL # sets bootvec as null
  for(i in 1:nboot){
    bootvec1 <- sample(vec0,replace=T) # initializes bootvec as the sample data set in xy.pck
    bootvec2 <- c(bootvec2,stat(bootvec1)) # initalizes the bootvector within mini-boot
  }
  boot <- sd(bootvec2)  # output the average of standard deviation or standard error or variance
  return(boot)
}
 
my.boot <- function(vec0, nboot=10000, alpha=0.05, stat){ # Creates the bootstrap for estimated variance
  populationSize <- length(vec0) # size of population
  StatOfpopulation <- stat(vec0) # stat of population
  standardError <- mini.boot(vec0=vec0,stat=stat)  # standard error
  bootvec2<-NULL
  for( i in 1:nboot){
    vecb<-sample(vec0,replace=T)
    statb<-stat(vecb) # stat of sample
    sde1<-mini.boot(vec0=vecb,stat=stat) # standard error of sample
    bootvec2<-c(bootvec2,(statb-StatOfpopulation)/sde1) # does calculation of boot vector (p value)
  }
  lq<-quantile(bootvec2,alpha/2) # Creates the lower quantiles
  uq<-quantile(bootvec2,1-alpha/2) # Creates the upper quantiles
  LB<-StatOfpopulation-(standardError)*uq # bootstrap lower bound
  UB<-StatOfpopulation-(standardError)*lq # bootstrap upper bound
  #Calculate lower bound for the corresponding normal distribution
  NLB<-StatOfpopulation-(standardError)*qt(1-alpha/2,populationSize-1)
  #Calculate numeric value of upper bound of the corresponding normal distribution
  NUB<-StatOfpopulation+(standardError)*qt(1-alpha/2,populationSize-1)
  #Output the confidence interval for both the bootstrap and the normal distribution
  list(bootstrap.confidence.interval=c(LB,UB),normal.confidence.interval=c(NLB,NUB))
}
