CI_CR <-
function(B)
{
      RI=c(0,0,0.58,0.90,1.12,1.24,1.32,1.41,1.45,1.49,1.51)#RI值，为常数
	Wi=weight(B);
	n=length(Wi);
	if(n>2)
	{
           W=matrix(Wi,ncol=1);
           A=matrix(B,nrow=sqrt(length(B)),ncol=sqrt(length(B)),byrow=TRUE)
	AW=A%*%W;
	aw=as.vector(AW);
	la_max=sum(aw/Wi)/n;
	CI=(la_max-n)/(n-1);
	CR=CI/RI[n];
	cat("\n CI=",round(CI,4),"\n")
	cat("\n CR=",round(CR,4),"\n")
	cat("\n la_max=",round(la_max,4),"\n\n")
	if(CR<=0.1)
	{
           cat(" Consistency test is OK！\n");
	   cat("\n Wi: ",round(Wi,4),"\n");
	}
	else 
	{	
	   cat(" Please adjust the judgment matrix! \n")
	   Wi=null;
	   break;
	}
        }
        else if(n<=2){ 
          return(Wi);
        }
}
