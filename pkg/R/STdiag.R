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
  function(formula=NULL,data=NULL,
           x=NULL,y=NULL,z=NULL,
           main=NULL,
           xlab=NULL,ylab=NULL,
           log=TRUE,
           zlim=NULL,znb=50,color="",
           density=FALSE,sm,n,
           bgcolor=rgb(254,254,226,maxColorValue=255),
           scales=list(),
           colorkey=NULL,
           ...)
  {
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
    
    if(density){
      if(missing(sm)){
        sm=0.5
      } 
      if(sm!=0){
      if(sm<0.1){sm=0.1}
      if(sm>1){sm=1}
      
      
      if(missing(n)){n=floor(c(length(unique(data[[names[2]]]))*10*sm,
          length(unique(data[[names[3]]]))*10*sm
          ))}
      if(is.na(n)){n=floor(c(length(unique(data[[names[2]]]))*10*sm,
                               length(unique(data[[names[3]]]))*10*sm
      ))}
      h1=bandwidth.nrd(data[[names[2]]])*sm
      h2=bandwidth.nrd(data[[names[3]]])*sm
      data=kde2dWeighted(x=data[[names[2]]],
                          y=data[[names[3]]],
                          w=data[[names[1]]],
                          h=c(h1,h2),n=n
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
    col=switch(color,
               gray = gray.colors(I(znb+10),start=0,end=1)[znb:1],
               topo = topo.colors(I(znb+10)),
               terrain = terrain.colors(I(znb+10)),
               heat = heat.colors(I(znb+10)),
               cm = cm.colors(I(znb+10)),
               rainbow=rainbow(I(znb+10),alpha=0.8)[znb:1],
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
      char[match[!is.na(match)]]=mat[1,!is.na(match)]
      char[1]=at[1]
      char[length(at)]=at[length(at)]
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
      colKey=colorkey
    }
    
    # Create vector zat to position the colors
    if(log){
      zat=seq(log10(z0),log10(zmax),length.out=znb)
    }else{
      zat=seq(zmin,zmax,length.out=znb)
    }
    

    scales=c(scales,list(alternating=1,
                         tck=0.5))
    # Create the plot
    sb <- trellis.par.get() 
    sb.bu <- sb
    sb$panel.background$col <- bgcolor
    sb$axis.line$lwd <-1
    trellis.par.set(sb) 
    
    lp=levelplot(formula,data,  #formula and data to plot
                 col.regions=col,  #color scale to use
                 at=zat,  #where to put the colors
                 colorkey=colKey,  #where to draw the ticks on the color bar
                 xlab=list(xlab,cex=1),ylab=list(ylab,cex=1), #write axes labels
                 main=as.character(main),  #write main title
                 scales=scales, #draw ticks inside the box
                 ...
    )
    trellis.par.set("panel.background", sb.bu) 
    return(lp)
    
  }