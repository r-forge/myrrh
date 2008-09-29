
MIR.CQT.GenerateKernel <- function(minFrequency=MIR.Tools.FFFromNote(-48), maxFrequency=MIR.Tools.FFFromNote(40), binsPerOctave=48, samplingFrequency=11025, threshold = 0.0054, windowingFunction=MIR.Window.Hamming, silent=FALSE)
{
  Q <- 1 / (2^(1/binsPerOctave)-1)
  K <- ceiling(binsPerOctave * log2(maxFrequency /minFrequency))
  fftLength <- 2^ceiling(log2(ceiling(Q * samplingFrequency / minFrequency)))
  if(!silent)
  {
    cat("\tQ = ", Q, "\n\tK = ", K, "\n\tFFT Length = ", fftLength, "\n\tCalculating [....................]\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
    percentDone <- 0
  }

  tempKernel <- rep(0, fftLength)
  CQTKernel <- matrix(0, nrow=fftLength, ncol=K)
  for(k in K:1)
  {
    len <- ceiling(Q * samplingFrequency / (minFrequency * 2^((k-1)/binsPerOctave)))
    tempKernel <- tempKernel * 0
    tempKernel[((fftLength-len)/2):((fftLength+len)/2)] <- (windowingFunction(len) / len) * exp(2i*pi*Q*(0:(len-1)) / len)
    specKernel <- fft(tempKernel)
    specKernel <- ifelse(abs(specKernel) <= threshold, 0, specKernel)
    specKernel <- Conj(specKernel) / fftLength
    CQTKernel[,k] <- specKernel
    if(!silent)
    {
      if(floor(20*(K-k)/K) > percentDone)
      {
        percentDone <- floor(20*(K-k)/K)
        cat("#")
      }
    }
  }
  if(!silent)
  {
    cat("#]\n")
  }
  return(CQTKernel)
}

MIR.CQT.CQgram <- function(x, CQTKernel, samplingFrequency=11025, hopSize=1024, silent=FALSE)
{
  numberOfFrames <- floor((length(x) - nrow(CQTKernel)) / hopSize) + 1
#   frameNr <- 1
  CQgram <- matrix(0, ncol=numberOfFrames, nrow=dim(CQTKernel)[2])
  if(!silent)
  {
    percentDone <- 0
    cat("\tFrames count = ", numberOfFrames, "\n\tCalculating [....................]\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
  }
  for(frameNr in 1:numberOfFrames)
  {
    dataBegin <- (frameNr-1)*hopSize + 1
    dataEnd <- dataBegin + dim(CQTKernel)[1] - 1
    if(dataEnd > length(x))
    {
      #frameNr <- frameNr - 1
      CQGram <- CQGram[,1:(frameNr-1)]
      break
    }
    CQgram[,frameNr] <- abs(t(as.matrix(fft(x[dataBegin:dataEnd]))) %*% CQTKernel)

    if(!silent)
    {
      if(floor(20*frameNr/numberOfFrames) > percentDone)
      {
        percentDone <- floor(20*frameNr/numberOfFrames)
        cat("#")
      }
    }
  }
  cat("#]                  \n")
  return(CQgram)
}

MIR.CQT.SingleCQgram <- function(x, CQTKernel, samplingFrequency=11025, hopSize=1024, silent=FALSE)
{
}