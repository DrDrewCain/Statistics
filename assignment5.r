#ASSIGNMENT 5 
#Uses Leaps 
#To be added

leaps.pck <- c("leaps.pck", "leaps.then.press.plot", "regpluspress", "matrix.2ndorder.make")
leaps.then.press.plot <- function(xmat0,yvec,ncheck=5,print.ls=F, resid.plot=F) 
#uses leaps as well as PRESS and quadratic fits to find the best fit linear/quadratic regression on a data set
{
 #initialize plot dimensions (i.e 5 graphs with one row of three and a second row of two)
    n1<-ceiling(sqrt(ncheck)) #first row with three plots
    par(mfrow=c(n1,n1)) #provision row with three plots
    if(resid.plot){
        n1<-ceiling(sqrt(2*ncheck)) #dimensions for second row
 }
    par(mfrow=c(n1,n1)) #provision row with two graphs for a total of 5 best fit plots
    xmat<-matrix.2ndorder.make(xmat0) #add curvature to best linear fit to help find best regressions
    leaps.str<-leaps(xmat,yvec) #use leaps R package to check all regressions, this will provide which, label, size, and Cp data
    z1<-leaps.str$Cp #save Cp data to a matrix/vector
    o1<-order(z1) #save data order (quadratic) of z1
    matwhich<-(leaps.str$which[o1,])[1:ncheck,] #save "which" parameter from leaps calcualtion
    z2<-z1[o1][1:ncheck] #start finding best plots (highest regression correlation) based on low Cp-p calculations
    for(i in 1:ncheck){
        ls.str0<-regpluspress(xmat[,matwhich[i,]],yvec) #extracts models from leaps
        if(print.ls){
            ls.print(ls.str0)
 }
    print(i) #print plot number outputted at the ith run through the loop
    print(paste("Press=",ls.str0$press)) #PRESS (predicted residual error sum of squares) is an actual prediction with parts of the system left out, press will be returned as a statistic value 
    parvec<-matwhich[i,] #store data from plots extracted by leaps
    npar<-sum(parvec) #take sum of data from laps
    print(paste("MPSE=",ls.str0$press/(length(yvec)-(npar+1)))) #calculate Mean Squared Prediction Error (MPSE) using PRESS data, sample size, and leaps data
    MPSE<-floor(1000*ls.str0$press/(length(yvec)-(npar+1)))/1000 #save MPSE value to print out with plots
    print(paste("Cp-p=",z2[i])) #print Cp-p values, leaps should have helped us select the top 5 lowest Cp-p values
    ypred<-cbind(1,xmat[,matwhich[i,]])%*%ls.str0$coef 
    #creates ypred column sizes for the  plots's y axis, (i.e the 10,15,20,25 breakdown)
 
     plot(ypred,yvec,main=paste("I=",i,"MPSE=",MPSE,"Cp-p=",z2[i])) 
    #combines graph number, MPSE, and Cp-p data to print on each plot's header
    if(resid.plot){
        plot(ypred,ls.str0$resid) #outputs plot
        }
    }
}
regpluspress <-function(x,y){
    ls.str<-lsfit(x,y) #least squares fit of data
    reg.influence<-1/(1-hat(x)) #PRESS calculation denominator (1 - hii), this line is not necessary
    press<-sum((ls.str$resid/(1-hat(x)))^2) #calculates sum((lsfit(x,y)$resid/(1-hat(x)))^2 for predicted residual error sum of squares (PRESS) value
    ls.str$leverage<-hat(x) #save leverage information from to ls
    ls.str$press<-press #save press information to ls
    ls.str #save variable
 }
matrix.2ndorder.make <-function(x, only.quad=T){ 
    x0<-x
    dimn<-dimnames(x)[[2]] #extract the names of the variables
    num.col<-length(x[1,]) # how many columns
    for(i in 1:num.col){
    # if we are doing all 2nd order
        if(!only.quad){
            for(j in i:num.col){
                x0<-cbind(x0,x[,i]*x[,j]) #combine x0 with x[,i]*x[,j] as columns 
                dimn<-c(dimn,paste(dimn[i],dimn[j],sep=""))  #create interaction dimnames
                }
            }
        else{
        #in here only if doing only squared terms
            x0<-cbind(x0,x[,i]*x[,i])
            dimn<-c(dimn,paste(dimn[i],"2",sep="")) # squared dimension names
        }
    }
    dimnames(x0)[[2]]<-dimn #save dimension names into dimnames matrix 
    x0
 }
 Auto.mat<-data.matrix(Auto)
#plot(Auto.mat)
leaps.then.press.plot(Auto.mat[,-1],Auto.mat[,1])
