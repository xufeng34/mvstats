plot.andrews <-
function(x){
   # x is a matrix or data frame of data
   if (is.data.frame(x)==TRUE)
      x<-as.matrix(x)
   t<-seq(-pi, pi, pi/30)
   m<-nrow(x); n<-ncol(x)
   f<-array(0, c(m,length(t)))
   for(i in 1:m){
      f[i,]<-x[i,1]/sqrt(2)
      for( j in 2:n){
          if (j%%2==0) 
             f[i,]<-f[i,]+x[i,j]*sin(j/2*t)
          else
             f[i,]<-f[i,]+x[i,j]*cos(j%/%2*t)
      } 
  }
  #plot(c(-pi,pi), c(min(f),max(f)), type="n", xlab="t", ylab="f(t)")
  plot(c(-pi,pi), c(min(f),max(f)), type="n", xlab="", ylab="")
  for(i in 1:m) lines(t, f[i,] , col=i)
  legend(2,max(f),rownames(x),col=1:nrow(x),lty=1:nrow(x),bty='n',cex=0.8)
}
