ggplot(data=HC_All, aes(x=H, y=C)) + geom_point() + facet_grid(tau ~ D)


# Function to plot the CCEP plane
# - the points may be informed (H,SC), or empty to plot just the limits
plot.ccep = function(H=NULL, SC=NULL, D=4, main='', 
                     xlim=c(0,1), ylim=c(0,0.5), lwd=1, col=1)
{
  # NOTE: var. names was inherited from original code of Rosso et al.
  
  # cont_name = paste(bp_path, '/limits/continua-N',factorial(D),'.q1', sep='')
  # trozos_name = paste(bp_path, '/limits/trozos-N',factorial(D),'.q1', sep='')
  cont_name = paste('../thesis/Data/limits/continua-N',factorial(D),'.q1', sep='')
  trozos_name = paste('../thesis/Data/limits/trozos-N',factorial(D),'.q1', sep='')
  
    
  # upper and lower limits
  continua = read.table(cont_name, skip=7)
  trozos = read.table(trozos_name, skip=7)
  
  maxY = max(trozos$V2)
  ylim = c(0,maxY)
  
  if ( !is.null(H) & !is.null(SC) )
  {
    plot(H, SC, 
         main=main, xlim=xlim, ylim=ylim, 
         xlab="Normalized Shannon Entropy", 
         ylab="Statistical Complexity"
    )
  } else {
    plot(NA, 
         main=main, xlim=xlim, ylim=ylim, 
         xlab="Normalized Shannon Entropy", 
         ylab="Statistical Complexity"
    )
  }
  
  lines(trozos, lwd=lwd, col=col)
  lines(continua, lwd=lwd, col=col)
}

# plot a region withing the min and max limits for the CCEP
# the values (alread equalized) for the limits (continua and trozos)
# must be informed
plot.region = function(alpha=0.5, col=1, lty=1, cont, troz)
{
  midy = alpha*cont[,2] + (1-alpha)*troz[,2]
  midx = alpha*cont[,1] + (1-alpha)*troz[,1]
  
  lines(midx, midy, col=col, lty=lty)
}

