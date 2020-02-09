plot.niche.mod <- function(z, name.axis1 = "PC1", name.axis2 = "PC2",
                           cor = F, corte,  contornar = TRUE, 
                           densidade = TRUE, quantis = 10, 
                           back = TRUE, x = "red", title = "",
                           i) {  
  
  
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
  
  
  a1 <- colorRampPalette(c("transparent",cor1(x, quantis)), alpha = TRUE)  
  
  xlim <- c(min(sapply(z, function(x){min(x$x)})),
            max(sapply(z, function(x){max(x$x)})))
  
  ylim <- c(min(sapply(z, function(x){min(x$y)})),
            max(sapply(z, function(x){max(x$y)})))
  
  graphics::image(z[[1]]$x, z[[1]]$y, as.matrix(z[[1]]$z.uncor), col = "white", 
                  ylim = ylim, xlim = xlim,
                  zlim = c(0.000001, max(as.matrix(z[[1]]$z.uncor), na.rm = T)), 
                  xlab = "PC1", ylab = "PC2", cex.lab = 1.5,
                  cex.axis = 1.4)
  
  abline(h = 0, v = 0, lty = 2)
  
  if (back) {
    contour(z[[i]]$x, z[[i]]$y, as.matrix(z[[i]]$Z),
            add = TRUE, levels = quantile(z[[i]]$Z[z[[i]]$Z > 0],
                                          c(0, 0.5)), drawlabels = FALSE,
            lty = c(1, 2), col = x, lwd = 1)
  }
  
  if (densidade) {
    image(z[[i]]$x, z[[i]]$y, as.matrix(z[[i]]$z.uncor), col = a1(100), add = TRUE)
  }
  
  
  if(contornar){
    contour(z[[i]]$x, z[[i]]$y, as.matrix(z[[i]]$z.uncor), 
            add = TRUE, levels = quantile(z[[i]]$z.uncor[z[[i]]$z.uncor > 0],
                                          seq(0, 1, (1 / quantis))),
            drawlabels = FALSE, lty = c(rep(2,(quantis - 1)), 1), 
            col = cor1(x, quantis), lwd = c(rep(1, (quantis - 1)), 2))
  }
  
  title(title)
  box()
}
