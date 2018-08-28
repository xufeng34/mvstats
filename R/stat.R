stat <-
function(x)
{
  if (all(is.na(x))) {
        stop("All elements of ", substitute(x), " have a missing value")
  }
  if(is.vector(x))
  {
     S=cbind(n=length(x),mean=mean(x),sd=sd(x),min=min(x),max=max(x))
  }
  else  
  {
     S=cbind(n=nrow(x),mean=apply(x,2,mean),sd=apply(x,2,sd),min=apply(x,2,min),max=apply(x,2,max))
  }   
   print(round(S,4))
}
