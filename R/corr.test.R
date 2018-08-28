corr.test <-
function(X,diag=TRUE)
{
  options(digits=4)
  p=ncol(X);
  if(diag)
  {
    tp=matrix(1,p,p);
    for(i in 1:p)
    {
      for(j in 1:i) tp[i,j]=cor.test(X[,i],X[,j])$stat;
      for(j in i:p) tp[i,j]=cor.test(X[,i],X[,j])$p.value;
    }
    cat("corr test: \n"); 
    tp=round(matrix(tp,p,dimnames=list(names(X),names(X))),4)
    print(tp)
    #return(tp)
    cat("lower is t value£¬upper is p value \n")
  }
  else
  {
    cat("\n corr test: t value, p value \n"); 
    if(is.matrix(X)) var=1:p
    else var=names(X);
    for(i in 1:(p-1))
    {
       for(j in (i+1):p) #cat(i,j,round(cor.test(X[,i],X[,j])$stat,4),round(cor.test(X[,i],X[,j])$p.value,4),"\n");
       cat(' ',var[i],'-',var[j],cor.test(X[,i],X[,j])$stat,cor.test(X[,i],X[,j])$p.value,"\n")
    }
  }
}
