factanal.rank <-
function(Fac,plot=FALSE)
{
  Fs=Fac$scores          
  W=apply(Fac$loadings^2,2,sum)
  Wi=W/sum(W);
  F=Fs%*%Wi  
  #cat("\n"); print(cbind('F'=F[,1],'rank'=rank(-F[,1])))
  Ri=data.frame('F'=F,'rank'=rank(-F))
  if(plot)
  {
     plot(Fs);abline(h=0,v=0,lty=3)
     text(Fs,label=rownames(Fs),pos=1.1,adj=0.5,cex=0.85) 
  }
  #common=apply(Fac$loadings^2,1,sum);
  list(Fs=Fs,Ri=Ri)
}
