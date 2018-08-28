factpc <-
function(X, m=2,rotation="none",scores="regression")
{  
   options(digits=4)
   S=cor(X); 
   p<-nrow(S); diag_S<-diag(S); sum_rank<-sum(diag_S)
   #rowname<-paste("X", 1:p, sep="")
   rowname = names(X)
   colname<-paste("Factor", 1:p, sep="")
   A<-matrix(0, nrow=p, ncol=p, dimnames=list(rowname, colname))
   eig<-eigen(S)
   for (i in 1:p)
      A[,i]<-sqrt(eig$values[i])*eig$vectors[,i]
   for (i in 1:p) { if(sum(A[,i])<0) A[,i] = -A[,i] }
   h<-diag(A%*%t(A))
   rowname<-c("SS loadings", "Proportion Var", "Cumulative Var")
   B<-matrix(0, nrow=3, ncol=p, dimnames=list(rowname, colname))
   for (i in 1:p){
     B[1,i]<-sum(A[,i]^2)
     B[2,i]<-B[1,i]/sum_rank
     B[3,i]<-sum(B[1,1:i])/sum_rank
   }
   #cat("\nFactor Analysis for Princomp: \n\n");
   #cat("\n"); print(B[,1:m]);
   W=B[2,1:m]*100; 
   Vars=cbind('Vars'=B[1,],'Vars.Prop'=B[2,],'Vars.Cum'=B[3,]*100)
   #cat("\n"); print(Vars[1:m,])
   #cat("\n"); print(A[,1:m]);
   A=A[,1:m] 
   #{ cat("\n common and specific \n"); print(cbind(common=h, spcific=diag_S-h)); }
   if(rotation=="varimax")
   {   
       #stop(" factor number >= 2 !")
       cat("\n Factor Analysis for Princomp in Varimax: \n\n");
       VA=varimax(A); A=VA$loadings; 
       s2=apply(A^2,2,sum); 
       k=rank(-s2); s2=s2[k]; 
       W=s2/sum(B[1,])*100; 
       Vars=cbind('Vars'=s2,'Vars.Prop'=W,'Vars.Cum'=cumsum(W))
       rownames(Vars) <- paste("Factor", 1:m, sep="")
       #print(Vars[1:m,])
       A=A[,k]
       for (i in 1:m) { if(sum(A[,i])<0) A[,i] = -A[,i] }
       A=A[,1:m]; 
       colnames(A) <- paste("Factor", 1:m, sep="")
       #cat("\n"); print(A) 
   }
   fit<-NULL
   fit$Vars<-Vars[1:m,]
   fit$loadings <- A
   X=as.matrix(scale(X));
   PCs=X%*%solve(S)%*%A
   #if(scores) cat("\n"); print(PCs)
   fit$scores <- PCs
   #if(rank)
   { 
      W=W/sum(W);
      PC=PCs%*%W;
      #cat("\n"); print(cbind(PCs,'PC'=PC[,1],'rank'=rank(-PC[,1])))
      Ri=data.frame('F'=PC,'Ri'=rank(-PC))
      fit$Rank <- Ri
   }
   #if(plot)
   #{ plot(PCs);abline(h=0,v=0,lty=3); text(PCs,label=rownames(PCs),pos=1.1,adj=0.5,cex=0.85) }
   #if(biplot)
   #{ biplot(PCs,A) } #text(PCs,label=rownames(PCs),pos=1.1,adj=0.5,cex=0.85) 
   common=apply(A^2,1,sum);
   fit$common <- common
   fit
   #list(Vars=B[,1:m],loadings=A,scores=PCs,Ri=Ri,common=common)
}
