library(ISLR)
library(leaps)
library(lars)

auto.mat<-as.matrix(Auto[,-9]) # 8 varibales

y_mat <- auto.mat[,1] # get mpg from matrix of Auto data
x.auto2 <- matrix.2ndorder.make(auto.mat[,-1])

#main function
my.boot.xy<-function(data=Auto,xstring="leaps",brep=1000){
  auto.mat<-as.matrix(Auto[,-9])
  # get mpg from matrix of Auto data
  y_mat <- auto.mat[,1] 
  # output the second order matrix for the x values
  x.auto2 <- matrix.2ndorder.make(auto.mat[,-1])
  #if loop for running lars
  if(xstring=="lars"){
    library(lars)
    out0 <- my.cp.extract.leaps2(lars(x.auto2,y_mat),x.auto2,y_mat)
  }
  #if loop for leaps
  if(xstring=="leaps"){
  
    library(leaps)
    #output needs to be modified because leaps cannot take more than 31 variables
    out0 <- my.cp.extract.leaps2(leaps(x.auto2[,-c(29,32,34,35)],y_mat), x.auto2,y_mat)
  }

}
#PRESS calculator
PRESS<-function(x,y){
  resid<-lsfit(x,y)$resid
  stud.resid<-resid/(1-hat(x)) 
  press<-sum(stud.resid^2)
  press 
  }
#cvlars calculator
sumabs<-function(x){
  sum(abs(x)) 
  }
betanorm.lars <- function(str){ 
  v1<-apply(str$beta,1,sumabs) 
  v1/max(v1)
}

#mix all variables, including interactions between the variables, together to make a 2nd order matrix
matrix.2ndorder.make<-function(x, only.quad=F){
  x0<-x
  #extract the names of the variables
  dimn<-dimnames(x)[[2]]  
  # how many columns
  num.col<-length(x[1,]) 
  for(i in 1:num.col){
    # if we are doing all 2nd order 
    if(!only.quad){
      for(j in i:num.col){
        x0<-cbind(x0,x[,i]*x[,j]) 
        #create interaction dimnames
        dimn<-c(dimn,paste(dimn[i],dimn[j],sep="")) 
              }
    }
    else{
      #in here only if doing only squared terms
      x0<-cbind(x0,x[,i]*x[,i])
      dimn<-c(dimn,paste(dimn[i],"2",sep="")) # squared dimmension names 
    }
  }
  dimnames(x0)[[2]]<-dimn
  x0
}


my.cp.extract.leaps2<-function(str,matrix.train,y=y_mat){
  #order the Cp values 
  o1<-order(str$Cp)
  #look for the corresponding rows of the five smallest Cp's
  which1<-str$which[o1[1:5],]
  #initiate a pressvec
  pressvec <- NULL
  #for loop for assembling the smallest PRESS value
  for(i in 1:5){
    xmat.train <- (matrix.train)[,which1[i,]]
    ymat.train <- y
    pressvec <- c(pressvec,PRESS(xmat.train,ymat.train))
  }
  I2 <- pressvec==min(pressvec)
  min_press <- (o1[1:5])[I2] #############
  #extract rows that corresponds to the min of pressvec
  which2 <- which1[I2,]
  #extract all coefficients, including the intercept  
  coef1 <- lsfit((matrix.train[,which2]),y)$coef
  #to make the coefficient vector the same length as the x matrix 
  coef2 <- coef1[-c(1)]
  #compose the predicted y
  y_pred <- matrix.train[,which2]%*%coef2 + coef1[1]
  #output expected y versus actual y 
  plot(y_pred, y)
}


my.cp.extract.lars1<- function(str, matrix.train=x.auto2, y=y_mat){
	#order the Cp's 
  o1<-order(str$Cp)
  #the indices that will be output in lars 
  Index1 <- betanorm.lars(str)
  #order the 5 smallest indices by Cp
  Index2 <- Index1[o1[1:5]]
  #extracting indices corresponding to the cv.lars 
  index_value <- cv.lars(matrix.train,y)$index
  #extract cv values 
  cv_value <-  cv.lars(matrix.train,y)$cv
  #extract y value corresponding to index 2
  min_1 <- approx(index_value,cv_value,Index2)$y
  #extract minimum of cvlars
  min_cvlars <- min_1==min(min_1)
  #calculating the beta corresponding to the minimum cvlars
  beta1 <- dum1$beta[(o1[1:5])[min_cvlars],]
  #storing the y prediction matrix
  y_pred <- matrix.train%*%beta1
  y_pred1 <- y_pred + (mean(y_mat) - mean(y_pred))
  plot(y_pred1, y)
  
}
