# This function is based on the lattice function levelplot and is specifically designed to help plot structure-time diagrams
# It takes as input a data frame of the form x, y, z, and plots z~x*y.
# Several options are coded in the function
#     -- main: character string, add a title to the plot
#     -- xlab: character string, add a label to x axis
#     -- ylab: character string, add a label to y axis
#     -- log: boolean, if TRUE z axis (color) is on a log scale, else z axis is linear
#     -- zlim: numeric vector, c(zmin, zmax), specify the range of color to use. If NULL, zlim=range(z)
#     -- znb: integer, specifies the number of colors to use
#     -- color: character string, type of color gradient to use. can be either one of gray, topo, terrain
#               heat, tim, cm or rainbow. default is rainbow
#     -- bgcolor: rgb(), color of the background of the plot

STdiag <-
  function(formula,data,
           x,y,z,
           main,
           xlab,ylab,
           log=FALSE,
           zlim,znb=50,color="",
           smooth=FALSE,sm,n,probamin=1e-6,
           interp=FALSE,intervX,intervY,
           bgcolor=rgb(254,254,226,maxColorValue=255),
           scales, colorkey,panel,
           ...)
  {
    
    # if arguments are missing, put the value to NULL
    if (missing(formula)){cat('missing');formula <- NULL}
    if (missing(data)){data <- NULL}
    if (missing(x)){x <- NULL}
    if (missing(y)){y <- NULL}
    if (missing(z)){z <- NULL}
    if (missing(main)){main <- NULL}
    if (missing(xlab)){xlab <- NULL}
    if (missing(ylab)){ylab <- NULL}
    if (missing(zlim)){zlim <- NULL}
    if (missing(scales)){scales <- list()}
    if (missing(colorkey)){colorkey <- NULL}
    
    if(inherits(formula,"matrix")){z=formula;formula=NULL}
    if(inherits(formula,"data.frame")){data=formula;formula=NULL}
    if (is.null(data)&!is.null(z))
    {
      data=Matrix2DataFrame(z,x,y)
      formula=NULL
    }
    
    # If formula is not specified, data must be of the form x, y, z
    if(is.null(formula) & dim(data)[2]!=3){
      cat("ERROR: If formula is not specified, data must be of the form x y z.\n")
      return(invisible())
    }
    
    # Retrieve names of the variables to plot and create the formula z~x*y if NULL
    if(!is.null(formula)){
      names=all.vars(formula)
    }else{
      data=data[,c(3,1,2)]
      names=colnames(data)
      formula=as.formula(paste(names[1]," ~ ",names[2],"*",names[3],sep=""))
    }
    
    if(interp){
      if(missing(intervX)){
        intervX=min(diff(unique(sort(data[[names[2]]]))))
      }
      if(missing(intervY)){
        intervY=min(diff(unique(sort(data[[names[3]]]))))
      }
      data=data.frame(data[[names[2]]],data[[names[3]]],data[[names[1]]])
      colnames(data)=names[c(2,3,1)]
      data=Interpolation(data,intervX,intervY)
    }
    if(smooth){
      if(missing(sm)){
        sm=0.5
      } 
      if(sm!=0){
        if(sm<0.1){sm=0.1}
        
        
        if(missing(n)){n=floor(c(length(unique(data[[names[2]]]))*10*min(sm,1),
                                 length(unique(data[[names[3]]]))*10*min(sm,1)
        ))}
        if(any(is.na(n))){n=floor(c(length(unique(data[[names[2]]]))*10*min(sm,1),
                                    length(unique(data[[names[3]]]))*10*min(sm,1)
        ))}
        h1=bandwidth.nrd(data[[names[2]]])*sm
        h2=bandwidth.nrd(data[[names[3]]])*sm
        data=kde2dWeighted(x=data[[names[2]]],
                           y=data[[names[3]]],
                           w=data[[names[1]]],
                           h=c(h1,h2),n=n,proba.min=probamin
        )
        main=paste("Kernel density plot:",main)
        colnames(data)=names[c(2,3,1)]
      }
    }
    
    # If log=TRUE, transform z to 
    if(log==T){
      formula<-as.formula(paste("log10(",names[1],")~",names[2],"*",names[3]))  
    }
    
    # Retrieve zmin and zmax from zlim, if NULL, takes range(z)
    if(is.null(zlim)){
      zlim=range(data[[names[1]]],na.rm=TRUE)
    }
    zmin=zlim[1]
    z0=max(min(subset(data[[names[1]]],data[[names[1]]]>0)),zmin)
    zmax=zlim[2]
    
    
    
    # Define the color scale
    if(is.list(color)){
      colo=color[[2]]
      color=color[[1]]
    }
    col=switch(color,
               gray = gray.colors(I(znb+10),start=0,end=1)[znb:1],
               topo = topo.colors(I(znb+10)),
               terrain = terrain.colors(I(znb+10)),
               heat = heat.colors(I(znb+10)),
               cm = cm.colors(I(znb+10)),
               rainbow=rainbow(I(znb+10),alpha=0.8)[znb:1],
               custom=colo,
               tim.colors(I(znb+10))         
    )
    
    # Define where to draw ticks on the colorbar.
    if(log){
      # Produce vector of position of ticks
      mat=mapply(function(x) seq(1:9)*10^x, seq(floor(log10(z0)),floor(log10(zmax))))
      at=c(mat)
      index=which(at>=z0 & at<=zmax)
      at=at[index]
      char=character(length(at))
      match=match(mat[1,],at)
      char[match[!is.na(match)]]=format(mat[1,!is.na(match)],sci=T)
      char[1]=format(at[1],sci=T)
      char[length(at)]=format(at[length(at)],sci=T)
      colKey=list(labels=list(cex=1
                              ,at=log10(at)
                              ,labels=char)
      )
      
    }else{
      colKey=list(labels=list(cex=1,
                              at=seq(signif(zmin,1),signif(zmax,1),signif((zmax-zmin)/7,1))
      )
      )
    }
    
    
    if(is.null(colorkey)){
      colKey=colKey
    }else if(is.logical(colorkey) & colorkey==TRUE){
      colKey=colKey
    }else { #if(!is.null(colorkey))
      colKey=c(colKey,colorkey)
    }
    
    # Create vector zat to position the colors
    if(log){
      zat=seq(log10(z0),log10(zmax),length.out=znb)
    }else{
      zat=seq(zmin,zmax,length.out=znb)
    }
    
    
    scales=c(scales,list(alternating=1,
                         tck=0.5))
    
    if (inherits(data[[names[2]]],c("Date","POSIXlt"))){
      Y=diff(strptime(c("2009","2010"),format='%Y'))
      M=diff(strptime(c("2009-09-01","2009-10-01"),format='%Y-%m-%d'))
      D=diff(strptime(c("2009-09-01","2009-09-02"),format='%Y-%m-%d'))
      rDate=range(data[[names[2]]])
      minD=min(diff(unique(data[[names[2]]])))
      maxD=diff(rDate)
      format=NULL
      if(maxD>D){
        format=paste0("%d")
      }
      if(maxD>M){
        format=paste0(format,"-%b")
      }
      if(maxD>Y){
        format=paste0(format,"-%y")
      }
      scales=c(scales,list(x=list(format=format)))
    }
    
    # Create the plot
    if(missing(panel)){
      panel=function(...){panel.levelplot(...)}
    }
    
    sb <- trellis.par.get() 
    sb.bu <- sb
    sb$panel.background$col <- bgcolor
    sb$axis.line$lwd <-1
    trellis.par.set(sb) 
    
    lp=levelplot(formula,data,  #formula and data to plot
                 col.regions=col,  #color scale to use
                 at=zat,  #where to put the colors
                 colorkey=colKey,  #where to draw the ticks on the color bar
                 xlab=xlab,ylab=ylab, #write axes labels
                 main=as.character(main),  #write main title
                 scales=scales, #draw ticks inside the box
                 panel=panel,
                 ...
    )
    trellis.par.set(sb.bu) 
    lp$logcol = log
    print(lp)
    return(lp)
    
  }