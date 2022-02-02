#Requirements: 

#Build a function which:
#1. takes as input a data frame, and the two elements of a frame that you want to model

#2. Plot the data fit with a smoothing spline with the default cross validation, vs the smoothing spline with 2 degrees of freedom (linear fit).

#3. Test the difference of fit using the F test

#4. Plot the normal quantile-quantile plots for the residuals 

#5 Create a histogram plot showing the difference. 


comparefits<-edit(comparefits)
 # data is our data file, DeltaTemp is our x-axis (our delta.temp) and x.disaster is our xDisaster 
 #and sqrt(anything > 0 will return non-sqrt otherwise perform sqrt transformation
function(data,DeltaTemp,xDisaster,sqrt){
  if(sqrt<0){
    par(mfrow=c(2,2)) # splits the graph into 2x2
    plot(data[,DeltaTemp],sqrt(data[,xDisaster])) # plots the regular data plot
    defaultfit<-smooth.spline(data[,DeltaTemp],sqrt(data[,xDisaster]))#cross validation
    linearfit<-smooth.spline(data[,DeltaTemp],sqrt(data[,xDisaster]),df=2) # creates the linear fit
    lines(smooth.spline(data[,DeltaTemp],sqrt(data[,xDisaster]))) #creates smooth spline line that also takes sqrt of y-axis
    lines(smooth.spline(data[,DeltaTemp],sqrt(data[,xDisaster]),df = 2), col=2) # sqrt the second element of the data so the (y axis)
    resdefault<-sqrt(data[[xDisaster]])-approx(defaultfit$x,defaultfit$y,data[[DeltaTemp]])$y # residuals for full model
    reslinear<-sqrt(data[[xDisaster]])-approx(linearfit$x,linearfit$y,data[[DeltaTemp]])$y# residuals for 2df model
    dfdefault<-length(data[,DeltaTemp])-defaultfit$df # The default model of degree of freedom calculation
    dflinear<-length(data[,DeltaTemp])-linearfit$df # The linear model of the degree of freedom calculation
    ssn<-sum(reslinear^2) # nested system calculating the sum of residual of linear by squaring it
    ssf<-sum(resdefault^2)# nested system calculating the sum of residual of default by squaring it
    #(SSN-SSF)/(dfN-dfF))/(SSF/dfF)~FdfN-dfF,dfF
    fstat<-((ssn-ssf)/(dflinear-dfdefault))/(ssf/dfdefault)
    pval<-1-pf(fstat,dflinear-dfdefault,dfdefault) 
    #Calculates F-test and P-value
    qqnorm(resdefault) # creates the Q-Q norm plot of resdefault
    qqnorm(reslinear) # creates the Q-Q norm plot for normal residuals
    results<-list(F=fstat,p=pval,df1=dflinear-dflinear,df2=dfdefault) # sets up the basis for us to be able to print out the results
    print(results) # prints the result. 
  }
  else{ # condition when sqrt is any value that is >= 0
    par(mfrow=c(2,2))  # splits the graph into 2x2
    plot(data[,x.disaster],(data[,delta.temp]))  # plots the regular data plot
    defaultfit<-smooth.spline(data[,DeltaTemp],(data[,xDisaster]))#cross validation
    linearfit<-smooth.spline(data[,DeltaTemp],(data[,xDisaster]),df=2)  # creates the linear fit
    lines(smooth.spline(data[,DeltaTemp],(data[,xDisaster])))  #creates smooth spline when default
    lines(smooth.spline(data[,DeltaTemp],(data[,xDisaster]),df = 2), col=2) # creates smooth spline when df = 2
    resdefault<-(data[[xDisaster]])-approx(defaultfit$x,defaultfit$y,data[[DeltaTemp]])$y # residuals for full model
    reslinear<-(data[[xDisaster]])-approx(linearfit$x,linearfit$y,data[[DeltaTemp]])$y# residuals for 2df model
    dfdefault<-length(data[,DeltaTemp])-defaultfit$df # The default model of degree of freedom calculation
    dflinear<-length(data[,DeltaTemp])-linearfit$df  # The linear model of the degree of freedom calculation
    ssn<-sum(reslinear^2) # nested system calculating the sum of residual of linear by squaring it
    ssf<-sum(resdefault^2) # nested system calculating the sum of residual of default by squaring it
    #(SSN-SSF)/(dfN-dfF))/(SSF/dfF)~FdfN-dfF,dfF
    fstat<-((ssn-ssf)/(dflinear-dfdefault))/(ssf/dfdefault)
    pval<-1-pf(fstat,dflinear-dfdefault,dfdefault)
    #Calculates F-test and P-value
    qqnorm(resdefault)  # creates the Q-Q norm plot of resdefault
    qqnorm(reslinear) # creates and checking the Q-Q norm plot for normal residuals
    results<-list(F=fstat,p=pval,df1=dflinear-dflinear,df2=dfdefault)  # sets up the basis for us to be able to print out the results
    print(results) # prints/outputs the results
  }
}
