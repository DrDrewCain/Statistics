library(ISLR)
library(leaps)
library(lars)

auto.mat<-as.matrix(Auto[,-9]) # 8 varibales

y_mat <- auto.mat[,1] # get mpg from matrix of Auto data
x.auto2 <- matrix.2ndorder.make(auto.mat[,-1]) # creates 2nd order matrix


LeapsandLars<-function(data=Auto,xstring="leaps"){ 
  auto.mat<-as.matrix(Auto[,-9])
  y_mat <- auto.mat[,1] # get mpg from matrix of Auto data
  x.auto2 <- matrix.2ndorder.make(auto.mat[,-1])

  # Determine whether the chosen model is leaps 
  if(xstring=="leaps"){
 
    library(leaps)
    out0 <- my.cp.extract.leaps2(leaps(x.auto2[,-c(29,32,34,35)],y_mat), x.auto2,y_mat)
  }
 # Determine whether the chosen model is lars
  if(xstring=="lars"){
    library(lars)
    out0 <- my.cp.extract.lars1(lars(x.auto2,y_mat),x.auto2,y_mat)
    # string = lars(mat.train, y)
  }  
}
# Below calculates the PRESS value (calculation for PRESS)
PRESS<-function(x,y){
  resid<-lsfit(x,y)$resid
  stud.resid<-resid/(1-hat(x))
  press<-sum(stud.resid^2)
  press }

# Below is the sum of absolute value of x function
sumabs<-function(x){ 
  sum(abs(x)) }

# Below creates the betanorm lars function // applies the betanorm calculation 
betanorm.lars <- function(str){
  v1<-apply(str$beta,1,sumabs)
  v1/max(v1)
}


matrix.2ndorder.make<-function(x, only.quad=F){
  x0<-x
  dimn<-dimnames(x)[[2]] #extract the names of the variables
  num.col<-length(x[1,]) # how many columns
  for(i in 1:num.col){
    # if we are doing all 2nd order
    if(!only.quad){
      for(j in i:num.col){
        x0<-cbind(x0,x[,i]*x[,j])
        dimn<-c(dimn,paste(dimn[i],dimn[j],sep="")) #create interaction dimnames
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


extract.leaps1<-function(str,matrix.train,y=y_mat){
  # y_mat <- auto.mat[,1]
  # auto.mat<-as.matrix(Auto[,-9])
  o1<-order(str$Cp)
  which1<-str$which[o1[1:5],]
  #initiate a pressvec
  pressvec <- NULL # Initializes press vector as null 
  for(i in 1:5){ # Takes in how many models we want in our case we need it from 1 to 5
    xmat.train <- (matrix.train)[,which1[i,]] # accesses the which value in the model
    ymat.train <- y # Creates our y value at the y matrix
    pressvec <- c(pressvec,PRESS(xmat.train,ymat.train)) # Access and finds the press statistics of the model
  }
  I2 <- pressvec==min(pressvec) # Creates a new Index
  min_press <- (o1[1:5])[I2] #############
  #extract rows that corresponds to the min of pressvec
  which2 <- which1[I2,]
  coef1 <- lsfit((matrix.train[,which2]),y)$coef # Creates our lsfit model matrix that accesses the which value coefficient
  coef2 <- coef1[-c(1)]
  y_pred <- matrix.train[,which2]%*%coef2 + coef1[1]
  plot(y_pred, y)
}


extract.lars1<- function(str, matrix.train=x.auto2, y=y_mat){
  o1<-order(str$Cp)
  # low5_cp<-str$Cp[o1[1:5]]
 
  Index1 <- betanorm.lars(str) # This is our index which creates the betanorm of the lars function 
  Index2 <- Index1[o1[1:5]] # Picks the plots from 1 to 5 of the best models
  index_value <- cv.lars(matrix.train,y)$index
  cv_value <-  cv.lars(matrix.train,y)$cv
  approx(index_value,cv_value,Index2)  
  min_1 <- approx(index_value,cv_value,Index2)$y
  min_cvlars <- min_1==min(min_1)
 
  beta1 <- str$beta[(o1[1:5])[min_cvlars],] # creates the beta variable which holds the beta value
  # Not just cvlars 
  y_pred <- matrix.train%*%beta1 # Our y predictor for the leaps 
  y_pred1 <- y_pred + (mean(y_mat) - mean(y_pred)) # Our y predictor for the lars
  plot(y_pred1, y) # Plots the predictor vs actual value 

}

Auto.mat.japan<-Auto.mat[Auto.mat[,8]==3,]
Auto.mat.germany<-Auto.mat[Auto.mat[,8]==2,]
Auto.mat.usa<-Auto.mat[Auto.mat[,8]==1,]
LeapsandLars(Auto.mat,xstring="lars")
LeapsandLars(Auto.mat.germany,xstring="lars")
LeapsandLars(Auto.mat.usa,xstring="lars")
LeapsandLars(Auto.mat.japan,xstring="lars")
