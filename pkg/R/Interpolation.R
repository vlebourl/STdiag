# Interpolation takes a three columns dataframe, x, y and z, and runs a linear interpolation
# over x and y provided the values of z, with an defined interval between two X values (intervX)
# and two Y values (intervY)

Interpolation=function(Tab,intervX=NULL,intervY=NULL)
{
  #Test if Tab has the right format, if not, exit
  if(!is.data.frame(Tab))
  {cat("Tab must be a data frame.")
   return()}
  
  if(dim(Tab)[2]>3)
  {cat("Tab must be of dimension n x 3 with columns corresponding to x, y and z.")
   return()}
  
  # Retrieve Tab's colnames to give it back at the end
  names.Tab=names(Tab)
  colnames(Tab)=c("x","y","z")
  
  z0=min(subset(Tab$z,Tab$z>0))
  
  # Create vectors of single values of X and Y
  X=unique(Tab$x)
  Y=unique(Tab$y)
  
  # If intervX and Y are NULL, define it as the smallest interval in respectively X and Y
  if(is.null(intervX)){intervX=min(diff(sort(X)))}
  if(is.null(intervY)){intervY=min(diff(sort(Y)))}
  
  # Create vectors of x and y coordinate of output grid.
  if(intervX>0){
    xo=seq(min(X),max(X),intervX)
  }else{
    xo=X
  }
  
  if(intervY>0){
    yo=seq(min(Y),max(Y),intervY)
  }else{
    yo=Y
  }
  
  # run the interpolation, using interp in package akima, read help(akima) for further details
  TabI = interp(Tab$x,Tab$y,Tab$z,xo=xo,yo=yo,duplicate="strip")
  
  # Turn output of interp from a list to a dataframe
  TabOut=expand.grid(xo,yo)
  colnames(TabOut)=c("x","y")
  TabOut$z=as.vector(TabI$z)
  TabOut$z[TabOut$z<z0]=0
  colnames(TabOut)=names.Tab
  return(TabOut)
}

kde2dWeighted <- function (x, y, w, h, n, lims = c(range(x), range(y)),proba.min=1E-6) {
  
  if (missing(n)){
    n=c(length(unique(x)),
        length(unique(y)))
  }
  nx <- length(x)
  if (length(y) != nx) 
    stop("data vectors must be the same length")
  n<-rep(n, length.out = 2L)
  gx <- seq(lims[1], lims[2], length = n[1]) # gridpoints x
  gy <- seq(lims[3], lims[4], length = n[2]) # gridpoints y
  
  if (missing(h)) 
    h <- c(bandwidth.nrd(x), bandwidth.nrd(y));
  if (missing(w)) 
    w <- numeric(nx)+1;
  h <- h/4
  ax <- outer(gx, x, "-")/h[1] # distance of each point to each grid point in x-direction
  ay <- outer(gy, y, "-")/h[2] # distance of each point to each grid point in y-direction
  z <- (matrix(rep(w,n[1]), nrow=n[1], ncol=nx, byrow=TRUE)*matrix(dnorm(ax), n[1], nx)) %*% t(matrix(dnorm(ay), n[2], nx))/(sum(w) * h[1] * h[2]) # z is the density
  
  z[z<proba.min]=0
  return(Matrix2DataFrame(mat=z,x=gx,y=gy))
}