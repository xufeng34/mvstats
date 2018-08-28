reg.plot <-
function(fm)
{
  p=ncol(fm$model);
  if(p==2){ plot(fm$model[,2],fm$model[,1],
           xlab=names(fm$model[2]),ylab=names(fm$model[1])); abline(fm,col='red');} 
  else{ plot(rownames(fm$model),fm$model[,1],type='p',xlab='i',ylab='.y,-y^');
       lines(rownames(fm$model),fm$fit); }
}
