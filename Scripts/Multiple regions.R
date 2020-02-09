plot.niche.all <- function(z, n.groups, g.names,
                           contornar = TRUE, 
                           densidade = TRUE,
                           quantis = 10,
                           back = TRUE, title = "",
                           g.colors, n = 5,
                           cor1) {  
  
  # Color func
  cor1 <- function(cores.i, n) {
    al <- seq(0,1,(1/n))
    cores <- numeric(length(n))
    for(i in 1:n) {    
      corespar <- col2rgb(cores.i)/255
      cores[i] <- rgb(corespar[1, ], corespar[2, ],
                      corespar[3, ], alpha = al[i])
    }
    return(cores)
  }
  
  
  a <- list() 
  for(i in 1:n.groups) {
    a[[i]] <- colorRampPalette(c("transparent", cor1(g.colors[i], n)),
                               alpha = TRUE)  
  }
  
  xlim <- c(min(sapply(z, function(x){min(x$x)})),
            max(sapply(z, function(x){max(x$x)})))
  
  ylim <- c(min(sapply(z, function(x){min(x$y)})),
            max(sapply(z, function(x){max(x$y)})))
  
  image(z[[1]]$x, z[[1]]$y, as.matrix(z[[1]]$z.uncor), col = "white", 
        ylim = ylim, xlim = xlim,
        zlim = c(0.000001, max(as.matrix(z[[1]]$Z), na.rm = T)), 
        xlab = "PC1", ylab = "PC2", cex.lab = 1.5,
        cex.axis = 1.4)
  abline(h = 0, v = 0, lty = 2)
  box()
  
  if (back) {
    for(i in 1:n.groups) {
      contour(z[[i]]$x, z[[i]]$y, as.matrix(z[[i]]$Z), add = TRUE,
              levels = quantile(z[[i]]$Z[z[[i]]$Z > 0], c(0, 1)),
              drawlabels = FALSE,lty = c(1, 2),
              col = g.colors[i], lwd = 1)
    }
  }
  
  if (densidade) {
    for(i in 1:n.groups) {
      image(z[[i]]$x, z[[i]]$y, as.matrix(z[[i]]$z.uncor),
            col = a[[i]](100), add = TRUE)
    }
  }
  
  
  if(contornar){
    for(i in 1:n.groups) {
      contour(z[[i]]$x, z[[i]]$y, as.matrix(z[[i]]$z.uncor), add = TRUE,
              levels = quantile(z[[i]]$z.uncor[z[[i]]$z.uncor > 0],
                                seq(0, 1, (1/quantis)))[quantis],
              drawlabels = FALSE, lty = rev(c(rep(2,(quantis - 1)), 1)),
              col = rev(cor1(g.colors[i], quantis)),
              lwd = rev(c(rep(1, (quantis - 1)), 2)))
    }
  }
  
}
