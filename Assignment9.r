# # The function name is FDR (false discovery rate) and the arguments being passed into this
# function are vector1 (vector of sample P values), alpha is the alpha level of the tests, Independence.int is
# a boolean variable that changes based on user input between being independent and not being
# independent

fdr<-function(vector1,alpha, independence.int){
# Puts in ascending order
ascendOrder<-order(vector1)
# Puts in the ordered list into vector vector1
pvec<-vector1[ascendOrder]
# Length of the vector
veclength<-length(vector1)
# If it’s independent run this block
if(independence.int){
# creates a line that the sorted p-values are compared to
  alphaline<-alpha*c(1:veclength)/veclength
}
# If it’s not independent run this block
else{
  Dependent <- c(1/1:veclength)
# creates a line that the sorted p-values are compared to
  alphaline <- alpha*c(1:veclength)/(veclength*(sum(Dependent)))
}
# plots a sorted list of p-values
plot(c(c(1:veclength),c(1:veclength)),c(alphaline,pvec),type="n",xlab="hypothesis",ylab="pvalue")
lines(c(1:veclength),alphaline)
points(c(1:veclength),pvec)
#subtracting the pvec and alphaline
dv<-pvec-alphaline
Index1<-(dv<0)
pmax<-max(pvec[Index1])
Index2<-pvec<=pmax
points(c(1:veclength)[Index2],pvec[Index2],col="red")
ascendOrder[Index2]
}
vector1<- (c((1e-5*runif(100)),runif(900))) 
