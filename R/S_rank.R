S_rank <-
function(data,Wi) #计算最终的加权评分
{
   wight_matrix=matrix(Wi,ncol=1,byrow=FALSE);
   score_matrix=as.matrix(data);
   Si=score_matrix%*%wight_matrix;
   print(data.frame(Si=Si,ri=rank(-Si)))
   #list(Si=Si,ri=rank(-Si))
   list(Si=Si)
}
