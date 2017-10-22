require(rgl)

set.seed(1234567890, kind = "Mersenne-Twister")

MMersenneTwister <- matrix(data=runif(50000*3), nrow = 50000, ncol = 3)

plot3d(MMersenneTwister)


seed <- as.double(1)
RANDU <- function() {
  seed <<- ((2^16 + 3) * seed) %% (2^31)
  seed/(2^31)
}

Mrandu <- matrix(nrow = 50000, ncol = 3)
for(i in 1:50000) {
  
  Mrandu[i,] <- c(RANDU(), RANDU(), RANDU())

}

plot3d(Mrandu)
