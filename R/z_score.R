z_score <-
function(B,converse=FALSE)  #converseΪT��TRUEʱ����������ָ�꣬Ĭ��ΪF
{
   B=as.vector(B);
   if(converse==FALSE||converse==F||converse=="")
   {
	min_value=min(B);
	max_value=max(B);
	z_score=(B-min_value)/(max_value-min_value)*60+40;
	z_score;
   }
   else if(converse==TRUE||converse==T)
   {
	min_value=min(B);
	max_value=max(B);
	
	z_score=(max_value-B)/(max_value-min_value)*60+40;
	z_score;
   }
}