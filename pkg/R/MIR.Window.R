###################################################
## Generates von Hann (sometimes called Hanning)
## window function
MIR.Window.VonHann <- function(N)
{
  windowFunction <- rep(0, N)
  M <- (N-1) / 2
  for(index in 1:N)
  {
    m                     <- (index-M-1)/(N-1)
    windowFunction[index] <- 0.5 + 0.5 * cos(2 * pi * m)
  }
  return(sqrt(windowFunction))
}

###################################################
## Generates Hamming window function
MIR.Window.Hamming2 <- function(N)
{
  windowFunction <- rep(0, N)
  M <- (N-1) / 2
  for(index in 1:N)
  {
    m                     <- (index-M-1)/(N-1)
    windowFunction[index] <- 0.46 + 0.54 * cos(2 * pi * m)
  }
  return(windowFunction)
}

###################################################
## Generates Hamming window function
MIR.Window.Hamming <- function(N)
{
  return(0.53836 - 0.46164 * cos(2 * pi * (0:(N-1)) / N))
}