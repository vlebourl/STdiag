# Functions to format data to be plotted with STdiag

# convert data from matrix to data frame.
# if values of x and y are not provided, the functions suppose
# x=seq(1,dim(mat)[1]) et
# y=seq(1,dim(mat)[2])
Matrix2DataFrame=function(mat,x=NULL,y=NULL,xlab="x",ylab="y",zlab="z")
{
  
  # check if x and y are difined, if not, gives default values
  if(is.null(y)){
    y=seq(0,dim(mat)[2]-1)
  }
  if(is.null(x)){
    x=seq(0,dim(mat)[1]-1)
  }
  
  # if dimensions do not match, return error message
  dim=c(length(x),length(y))
  if(sum(dim==dim(mat))!=2)
  {
    cat("ERROR: mat must be of dimension x,y.")
    return(invisible())
  }
  
  # create output data frame
  df=expand.grid(x,y)
  # add z column to output data frame
  df$z=c(mat)
  
  # name columns of output data frame and return
  colnames(df)=c(xlab,ylab,zlab)
  return(df)
}



# Import individual based data set: in case data set is composed of one line 
# per individual, with time and structure in columns.

Indiv2DataFrame=function(tab,classes=50,columns=c(1,2))
{
  
  # define output data frame
  DF=NULL
  
  names=names(tab[,columns])
  # get column number of x-axis variable and y-axis variable
  x=columns[1]
  y=columns[2]
  
  # test if x is a date
  is.date = inherits(tab[,x],"Date")
  
  # define classes for discretization of y axis variable in classes classes
  
  if(length(classes)==1){
    ymin=min(tab[,y],na.rm=TRUE)
    ymax=max(tab[,y],na.rm=TRUE)
    yat=seq(ymin,ymax,length.out=classes+1)
  } else if (length(classes)>1){
    if(min(tab[,y])<min(classes) | max(tab[,y])>max(classes)){
      cat("ERROR: 'classes' must span range of 'y'")
      return()
    }
    yat=classes
  }
  # discretize y-axis variable in classes classes
  for(t in unique(tab[,x]))
  {
    subtab=subset(tab,tab[,x]==t)
    h=hist(subtab[,y],breaks=yat,plot=F)
    DF=rbind(DF,x=data.frame(t,y=h$mids,counts=h$counts))
  }
  
  colnames(DF)=c(names,"counts")
  if(is.date){
    DF[,1] = as.Date(DF[,1],origin="1970-01-01")
  }
  # return result data frame
  return(DF)
}