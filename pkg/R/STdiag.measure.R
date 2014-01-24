STdiag.measure = function(stdiag,type=c("point","line"),region=FALSE,range=3.3,pch=21,lwd=3,col.line=1,col='white',fill=1,cex=1.2){
  graphics.off()
  print(stdiag)
  
  ### som used functions
  gety = function(x,datax,datay){
    if(length(datay)==1){return(datay)}
    lm=lm(datay~datax)
    y=lm$coefficients[1]+x*lm$coefficients[2]
    return(y)
  }
  
  getregion = function(x,y,rx,ry,data){
    return(
      data[data$x>=x-rx & data$x<=x+rx & data$y>=y-ry & data$y<=y+ry,]
    )
  }
  ####
  
  if(missing(type)) type="point"
  # Select if we measure a point or a line
  eval=switch(type,
              p="point",po="point",poi="point",poin="point",point="point",
              l="line",li="line",lin="line",line="line"
  )
  
  # Recover the data
  data=data.frame(x=stdiag$panel.args.common$x,
                  y=stdiag$panel.args.common$y,
                  z=stdiag$panel.args.common$z)
  if(stdiag$logcol){data$z=exp(data$z)}
  
  # flag to ask if the selection is good  
  accept=0
  
  # if we look for a point
  if(eval=="point"){
    cat("*** Click on the point you need to measure ***\n")
    while(accept!=1){
      # click 1 time
      trellis.focus()
      i<-panel.identify(n=1)
      trellis.unfocus()
      
      # update the plot with the point
      print(update(stdiag,panel=function(...){panel.levelplot(...);panel.points(data$x[i],data$y[i],pch=pch,lwd=lwd,col=col,fill=fill,cex=cex)}))
      
      # ask if the point is correct
      cat("Keep the point (0/1)?")
      accept <- scan(what=integer(1),nmax=1)
    }
    
    # if the point is correct, return the index and the 
    # corresponding values of x, y and z
    ret = data[i,]
    ret$index=i
    rownames(ret)="point"
    
  }
  
  # if we look for a line
  if(eval=="line"){
    cat("*** Click on the beginning and end points of the line you need to measure ***\n")
    while(accept!=1){
      # click 2 times
      trellis.focus()
      i<-panel.identify(n=2)
      trellis.unfocus()
      
      # update the plot with the line 
      print(update(stdiag,panel=function(...){panel.levelplot(...);panel.lines(data$x[i],data$y[i],type="o",pch=pch,lwd=lwd,col=col.line,fill=fill,cex=cex);panel.points(data$x[i],data$y[i],pch=pch,lwd=lwd,col=col,fill=fill,cex=cex)}))
      
      # ask if the point is correct
      cat("Keep the line (0/1)?")
      accept <- scan(what=integer(1),nmax=1)
    }
    ret = data[i,]
    rownames(ret)=c("start","end")
    ret$index=i
    ret$slope=lm(y~x,ret)$coef[2]
    
  }
  
  if(region==TRUE){
    subdata=data.frame(x=numeric(0),y=numeric(0),z=numeric(0))
    xs=unique(data$x[data$x>=min(ret$x)&data$x<=max(ret$x)])
    for(xi in xs){
      yi=gety(xi,ret$x,ret$y)
      yi=data$y[which.min(abs(yi-data$y))]
      subdata=rbind(subdata,getregion(xi,yi,range*mean(diff(unique(data$x))),range*mean(diff(unique(data$y))),data))
    }
    print(update(stdiag,panel=function(...){panel.levelplot(...);panel.lines(data$x[i],data$y[i],type="o",pch=21,lwd=3,col=1,fill=0,cex=1.2);panel.points(data$x[i],data$y[i],pch=21,lwd=3,col='white',fill=1,cex=1.2); panel.points(subdata$x,subdata$y)}))
    ret=list(selection=ret,region=subdata)
  }
  
  return(ret)
}


