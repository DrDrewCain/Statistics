fdr<-function(v1,Q,independence.int) {

order<-order(v1)
pvec<-v1[order]
VectorLength<-length(v1)
if(independence.int) {
qline<-Q*c(1:order)/order
}
 else{
  Dependency<-c(1/1:order)
  qline<-Q*c(1:order)/order*(sum(Dependency))
}

plot(c(c(1:order),c(1:order)),c(qline,pvec),type="n",xlab="hypothesis",ylab="pvalue")
lines(c(1:order),qline)
points(c(1:order),pvec)
dv<-pvec-qline
pmax<-max(pvec[I1])
I2<-pvec<=pmax
points(c(1:m)[I2],pvec[I2],col="red")
o1[I2]
v1<- (c((1e-5*runif(100)),runif(900)))
fdr(v1, 0.05, T)
}
