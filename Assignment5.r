library(ISLR)
library(leaps)
library(lars)

Auto.mat<-as.matrix(Auto[,-9]) # 8 varibales

y_mat <- auto.mat[,1] # get mpg from matrix of Auto data
x.auto2 <- matrix.2ndorder.make(auto.mat[,-1])


LeapsandLars<-function(data=Auto,xstring="leaps",brep=1000){ #specialized version of bootstrap
  auto.mat<-as.matrix(Auto[,-9])
  y_mat <- auto.mat[,1] # get mpg from matrix of Auto data
  x.auto2 <- matrix.2ndorder.make(auto.mat[,-1])
 
  if(xstring=="lars"){
    library(lars)
    out0 <- my.cp.extract.lars1(lars(x.auto2,y_mat),x.auto2,y_mat)
    # string = lars(mat.train, y)
  }
  if(xstring=="leaps"){
 
    library(leaps)
    out0 <- my.cp.extract.leaps2(leaps(x.auto2[,-c(29,32,34,35)],y_mat), x.auto2,y_mat)
  }

}

PRESS<-function(x,y){
  resid<-lsfit(x,y)$resid
  stud.resid<-resid/(1-hat(x))
  press<-sum(stud.resid^2)
  press }

sumabs<-function(x){
  sum(abs(x)) }

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


extract.leaps2<-function(str,matrix.train,y=y_mat){
  # y_mat <- auto.mat[,1]
  # auto.mat<-as.matrix(Auto[,-9])
  o1<-order(str$Cp)
  which1<-str$which[o1[1:5],]
  #initiate a pressvec
  pressvec <- NULL
  for(i in 1:5){
    xmat.train <- (matrix.train)[,which1[i,]]
    ymat.train <- y
    pressvec <- c(pressvec,PRESS(xmat.train,ymat.train))
  }
  I2 <- pressvec==min(pressvec)
  min_press <- (o1[1:5])[I2] #############
  #extract rows that corresponds to the min of pressvec
  which2 <- which1[I2,]
  coef1 <- lsfit((matrix.train[,which2]),y)$coef
  coef2 <- coef1[-c(1)]
  y_pred <- matrix.train[,which2]%*%coef2 + coef1[1]
  plot(y_pred, y)
}


extract.lars1<- function(str, matrix.train=x.auto2, y=y_mat){
  o1<-order(str$Cp)
  # low5_cp<-str$Cp[o1[1:5]]
 
  Index1 <- betanorm.lars(str)
  Index2 <- Index1[o1[1:5]]
  index_value <- cv.lars(matrix.train,y)$index
  cv_value <-  cv.lars(matrix.train,y)$cv
  approx(index_value,cv_value,Index2)  
  min_1 <- approx(index_value,cv_value,Index2)$y
  min_cvlars <- min_1==min(min_1)
 
  beta1 <- str$beta[(o1[1:5])[min_cvlars],]
  y_pred <- matrix.train%*%beta1
  y_pred1 <- y_pred + (mean(y_mat) - mean(y_pred))
  plot(list(y_pred1, y,beta1))
 
}

# Running the code :
# Auto.mat.japan<-Auto.mat[Auto.mat[,8]==3,]
# Auto.mat.germany<-Auto.mat[Auto.mat[,8]==2,]
# Auto.mat.usa<-Auto.mat[Auto.mat[,8]==1,]
# LeapsandLars(Auto.mat,xstring="lars")
# LeapsandLars(Auto.mat.germany,xstring="lars")
# LeapsandLars(Auto.mat.usa,xstring="lars")
