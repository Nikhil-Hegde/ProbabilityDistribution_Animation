library(animation)
maxscale <- 3
x <- seq(-maxscale, maxscale, 0.01)
density <- dt(x, df=31)
plot(x, density, type="l", xlab="Standardized observed difference", ylab="Probability density")

saveVideo({
  par(mar = rep(2,4))
  ani.options(interval = 0.5)
  
  plot_t <- function(PMsig,df) {
    
    # compute critical t-values based on PM/sig ratio and df
    t_crit <- function(PMsig,df) {
      
      A <- abs(min((1.81 - PMsig),0))
      B <- max((PMsig - 0.85),0)
      C <- max((PMsig - 0.26),0)
      D <- max((PMsig - (-0.26)),0)
      E <- max((PMsig - (-0.85)),0)
      Fa <- max((PMsig - (-2.45)),0)
      
      return(list('A'=A,'B'=B,'C'=C,'D'=D,'E'=E,'Fa'=Fa))
    }
    
    crit <- t_crit(PMsig,df)
    
    # define polygon coordinates for A-F scores
    A.x <- c(-crit$A,seq(-crit$A,crit$A,0.01),crit$A) 
    A.y <- c(0,dt(seq(-crit$A,crit$A,0.01),df),0)
    
    B.x <- c(-crit$B,seq(-crit$B,crit$B,0.01),crit$B) 
    B.y <- c(0,dt(seq(-crit$B,crit$B,0.01),df),0)
    
    C.x <- c(-crit$C,seq(-crit$C,crit$C,0.01),crit$C) 
    C.y <- c(0,dt(seq(-crit$C,crit$C,0.01),df),0)
    
    D.x <- c(-crit$D,seq(-crit$D,crit$D,0.01),crit$D) 
    D.y <- c(0,dt(seq(-crit$D,crit$D,0.01),df),0)
    
    E.x <- c(-crit$E,seq(-crit$E,crit$E,0.01),crit$E) 
    E.y <- c(0,dt(seq(-crit$E,crit$E,0.01),df),0)
    
    Fa.x <- c(-crit$Fa,seq(-crit$Fa,crit$Fa,0.01),crit$Fa) 
    Fa.y <- c(0,dt(seq(-crit$Fa,crit$Fa,0.01),df),0)
    
    # generate legend parameters
    leg.text <- c("A","B","C","D","E","F")
    leg.col <- c('darkgreen','green','yellow','orange','darkorange','darkred')
    PMsig2 <- sprintf("%4.2f", PMsig)
    
    # plot score capability graph
    x <- seq(-3.5,3.5,0.01)
    plot(x,dt(x,df),type='l',main='Score Capability',xlab='Standardized Difference',ylab='Probability Density')
    text(-2.5,0.3,bquote(frac(PM,sigma) == .(PMsig2)))
    polygon(Fa.x,Fa.y,col='darkred')
    polygon(E.x,E.y,col='darkorange')
    polygon(D.x,D.y,col='orange')
    polygon(C.x,C.y,col='yellow')
    polygon(B.x,B.y,col='green')
    polygon(A.x,A.y,col='darkgreen')
    legend("topright",leg.text,col=leg.col,lty=1,lwd=5,cex=0.6)
    
  }
  
  for(i in seq(2,1.81 , by = -0.01))
  {
    plot_t(i,31)
  }
},video.name = "animation.mp4", image.name= "Rplot", ffmpeg= "ffmpeg", single.opts = "utf8: false", autoplay = FALSE, interval = 0.5, 
imgdir = "norm_dir", ani.height = 400, ani.width = 600, 
title = "Demo of Prob Distn")