weight <-
function(B) #B为构造的判断矩阵，按行以向量的形式提供参数
{
	A=matrix(B,nrow=sqrt(length(B)),ncol=sqrt(length(B)),byrow=TRUE)
      #A=t(B);  
	n=ncol(A);
	mul_collect=c(1:n);
	for(i in 1:n)
	   mul_collect[i]=prod(A[i,])
	weight=mul_collect^(1/n);
	weight_one=weight/sum(weight);
	round(weight_one,4)
}
