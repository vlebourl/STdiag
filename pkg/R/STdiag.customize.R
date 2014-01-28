STdiag.points = function(stdiag,px ,py , alpha=1,cex=0.8,col=1,font=1,pch=16,fill="transparent"){
  
  print(
    up<-update(stdiag,panel=function(...){
      stdiag$panel(...)
      panel.points(px,py,alpha=alpha,cex=cex,col=col,font=font,pch=pch,fill=fill)
    })
  )
  
  return(up)
}

STdiag.lines = function(stdiag,lx ,ly ,type="l", alpha=1,cex=0.8,col=1,font=1,pch=16,fill="transparent",lty=1,lwd=1){
  
  print(
    up<-update(stdiag,panel=function(...){
      stdiag$panel(...)
      panel.lines(lx,ly,type=type,alpha=alpha,cex=cex,col=col,font=font,pch=pch,fill=fill,lty=lty,lwd=lwd)
    })
  )
  
  return(up)
}