freq <-
function(X)
{   
    vars=names(X)
    X=as.matrix(X)
    p=ncol(X)
    tab<-function(Y)
    { 
       T1=table(Y)
       Percent=T1/sum(T1)*100
       T2=cbind(Freqency=T1,Percent,Cum.percent=cumsum(Percent))
       Total=c(sum(T1),100,100)
       print(rbind(T2,Total))
    }
    if (p == 1) { cat('\n'); tab(X); }
    else {
       for(i in 1:p) 
         { cat('\n',vars[i],"\n"); tab(X[,i]) } 
   }
}
