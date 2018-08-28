z_data <-
function(data,converse=FALSE)
{
	n=ncol(data);
	m=nrow(data);
	score_array=array(1:(m*n),c(m,n));
	for(i in 1:n)
	{
	   score_array[,i]=z_score(data[,i],converse);
	}
	SCORE=as.matrix(score_array);
	dimnames(SCORE)[1]=dimnames(data)[1];
	dimnames(SCORE)[2]=dimnames(data)[2];
	round(SCORE,4)
}
