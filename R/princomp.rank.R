princomp.rank <-
function(PCA,m,plot=FALSE)
{
  W=as.matrix(PCA[[1]]^2/sum(PCA[[1]]^2))
  PCs=as.matrix(PCA$scores[,1:m])          
  PC=PCs%*%W[1:m]/sum(W[1:m])  
  #print(PC)
  ans=cbind(PCs,'PC'=PC[,1],'rank'=rank(PC[,1]))
  #cat("\n"); print(ans)
  if(plot) 
  {
   plot(PCs);abline(h=0,v=0,lty=3)
   text(PCs,label=rownames(PCs),pos=1.1,adj=0.5,cex=0.85) 
  }
  return(ans)
}
