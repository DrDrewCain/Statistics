# HelperFunction
helperFunction <- function(data,element1,element2,sqrt=F) 
{
  if(sqrt) {
    l1<-smooth.spline(data[[element1]],sqrt(data[[element2]])) 
    } # Default cross validation / default fit
  else {
    l1<-smooth.spline(data[[element1]],data[[element2]]) 
    } # Default cross validation without the sqrt
  x.resid<-resid(l1) #raw resids
  std.resid<-sqrt(sum(x.resid^2))/(length(data[[element1]]) - l1$df) #standard resids 
  stud.resid<-x.resid/std.resid #studentized resids << residual adjusted by dividing it by an estimate of its standard deviation.
  KolmogorovT<-ks.test(stud.resid,pnorm)$statistic # Kolmogorov-Smirnov Test the stuentized resid, pnorm
  if(sqrt) {
    my.smooth<-sqrt(data[[element2]])-x.resid 
    } 
  else {
    my.smooth<-data[[element2]]-x.resid 
    }
  list(KolmogorovT=KolmogorovT,raw.resid=x.resid,std.resid=std.resid,smooth=my.smooth)
}
#Mixture of my own code and code learned in class. 
myFunction <- function(data=NOAA,element1=3,element2=2,sqrt=F,nboot=1000,confidence=0.9)
{
  par(mfrow=c(1,1)) #plot only one graph 
  #conditional statement assigning sqrt to one of the variables if sqrt = T
  var1=data[[element1]]
  if(sqrt) # Conditional if statement to sqrt transform when True
    var2=sqrt(data[[element2]]) # sqrt calls the data at element2 (delta.temp)
  else # Conditional else if the statement above is false.
    var2=data[[element2]] # 
  
  #initial values 
  init<-helperFunction(data,element1,element2,sqrt) # initalizes the helper function
  #Provided that you provide an excel data, provide the x and y element and whether sqrt is true or false.
 
  init.smooth<-init$smooth # The initial smooth spline and access the smooth
  init.sdresid<-init$sd.resid # initial standard residual and access the standard residual
  init.resid<-init$raw.resid # initial residual and access the raw.residual
  bootdata<-data # initalizes bootdata as data (Data is our file) so this should boot the data we want
  n1<-length(init.smooth)  # sets n1 as the length of initial smooth
  smooth.dist<-NULL # Sets the inital Smooth distribution at null
  
  #bootstrapping
  for(i in 1:nboot) 
  #for loop is iterated over a sequence having numbers from 1 to 5. In each iteration, each item of the sequence is displayed. 
  {
    temporary<-sample(init.resid,length(init.resid),replace=T) #take a random sample of residuals with replacement
    boot.newdata<-((init.smooth+temporary)) #create a temporary variable for bootstrapped data
    bootdata[[element2]]<-boot.newdata #assign it 
    initp<-helperFunction(bootdata,element1,element2) #iterate (new initial value)
    boot.smooth<-initp$smooth #assign new smoothing spline to boot.smooth 
    smooth.dist<-rbind(smooth.dist,boot.smooth-init.smooth) #appends the smooth data in data frame format and assigns it to smooth.data
  
    #this loop continuously updates the data that is being used for analysis by iterating random samples nboot times 
    #because the sample() function is pseudo random, this loop will generate new (or almost new) data sets every time the function is called 
    
  }
  
  #confidence interval
  alpha<-1-confidence
  LB<-NULL # Initializes Lowerbound as Null
  UB<-NULL # Initializes Upperbound as Null 
  for( i in 1:length(smooth.dist[1,]) ) 
  # for loop is iterated over a sequence from i to 1 in the length of smooth distribution index starting at [1]
  # or the [1] index (the 2nd index) where [0] is at the ith (1st) index
  {
    #this loop approximates an interval at every x for y-alpha/2 < y < y+alpha/2
    s1<-sort(smooth.dist[,i]) # Sorts the smooth distribution at interval of ith index where ith index is the held within excel
    n2<-length(s1) # sets n2 as the length of s1
    v1<-c(1:n2)/n2
    bvec<-approx(v1,s1,c(alpha/2,1-alpha/2))$y # bvec is the approximation of v1, s1, and the confidence of alpha/2 and 1-alpha/2
    LB<-c(LB,init.smooth[i]-bvec[2]) #bvec[2] > 0 thus every element of lowerbound should be less than the initial spline
    UB<-c(UB,init.smooth[i]-bvec[1]) #bvec[1] < 0 thus every element of upperbound should be greater than the initial spline
    
    #provided the spline is a good fit - the lower bounds and upper bounds should never be intersected by the spline 
    
  }
  
  #plotting
  plot(rep(var1,4),c(LB,init.smooth,UB,var2),xlab="X",ylab="Y",type="n") 
  points(var1,var2)
  o<-order(var1)
  lines(var1[o],LB[o],col=2)
  lines(var1[o],UB[o],col=2)
  lines(var1[o],init.smooth[o],col=3)	
  lines(smooth.spline(var1,var2,df=2),col=4)
}

NOAA<-read.csv(choose.files())
myFunction(sqrt=T)

