Matrix2DataFrame=function(mat,x=NULL,y=NULL,xlab="x",ylab="y",zlab="z")
{
  
  if(is.null(y)){
    y=seq(0,dim(mat)[2]-1)
  }
  if(is.null(x)){
    x=seq(0,dim(mat)[1]-1)
  }
  dim=c(length(x),length(y))
  if(sum(dim[2:1]==dim(mat))==2)
  {
    mat=t(mat)
  }else if(sum(dim==dim(mat))!=2)
  {
    cat("ERROR: mat must be of dimension x,y or y,x.")
    return(invisible())
  }
  
  df=expand.grid(x,y)
  df$z=c(mat)
  
  colnames(df)=c(xlab,ylab,zlab)
  return(df)
}