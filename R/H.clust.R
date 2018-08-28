H.clust <-
function(X,d="euc",m="comp",proc=FALSE,plot=TRUE)
{
  D=dist(X,d)
  hc <- hclust(D,m)            
  #if(proc){ cat("\n cluster procdure: \n"); print(cbind(hc$merge,hc$height)) }
  PROC=cbind(merge=hc$merge,height=hc$height)
  if(proc) print(PROC)
  if(plot) plot(hc,ylab=d,main=m)    
  #plot(hc,hang=hang,xlab="",ylab="",main="")    
  #hc1=as.dendrogram(hc)
  #plot(hc1,xlab="G",ylab="D",horiz=TRUE) 
  #list(D=D,hc=hc,proc=proc)
  return(hc)
}
