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
  function(formula=NULL,data,main=NULL,xlab=NULL,ylab=NULL,log=TRUE,zlim=NULL,znb=50,color="",bgcolor=rgb(254,254,226,maxColorValue=255))
  {
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
    
    
    # If log=TRUE, transform z to 
    if(log==T){
      formula<-as.formula(paste("log10(",names[1],")~",names[2],"*",names[3]))  
    }
    
    # Retrieve zmin and zmax from zlim, if NULL, takes range(z)
    if(is.null(zlim)){
      zlim=range(data[[names[1]]],na.rm=TRUE)
    }
    zmin=zlim[1]
    zmax=zlim[2]
    
    # Create vector zat to position the colors
    if(log){
      zat=seq(log10(zmin+1),log10(zmax+1),length.out=znb)
    }else{
      zat=seq(zmin,zmax,length.out=znb)
    }
    
    # Define the color scale
    col=switch(color,
               gray = gray.colors(I(znb+10),start=0,end=1)[znb:1],
               topo = topo.colors(I(znb+10)),
               terrain = terrain.colors(I(znb+10)),
               heat = heat.colors(I(znb+10)),
               cm = cm.colors(I(znb+10)),
               tim = tim.colors(I(znb+10)),
               rainbow(I(znb+10),alpha=0.8)[znb:1]
    )
    
    # Define where to draw ticks on the colorbar.
    if(log){
      # Produce vector of position of ticks
      m1=3*log10(max(zmin,1))
      m2=3*log10(zmax)
      R3=signif(10^(seq(round(m1),round(m2),1)/3),1)
      # Produce list colkey
      colKey=list(labels=list(cex=1
                              ,at=log10(R3+1)
                              ,labels=R3))
      
    }else{
      colKey=list(labels=list(cex=1,
                              at=seq(ceiling(zmin),round(zmax),signif((zmax-zmin)/6,1))
      )
      )
    }
    
    # Create the plot
    sb <- trellis.par.get("panel.background") 
    sb.bu <- sb
    sb[["col"]][1] <- bgcolor
    trellis.par.set("panel.background", sb) 
    
    lp=levelplot(formula,data,  #formula and data to plot
                 col.regions=col,  #color scale to use
                 at=zat,  #where to put the colors
                 colorkey=colKey,  #where to draw the ticks on the color bar
                 xlab=list(xlab,cex=1),ylab=list(ylab,cex=1), #write axes labels
                 main=main,  #write main title
                 scales=list(alternating=1,
                             tck=-1), #draw ticks inside the box
                 )
    print(lp)
    
    trellis.par.set("panel.background", sb.bu) 
    
  }