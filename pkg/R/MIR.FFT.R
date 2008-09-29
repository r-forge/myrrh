## MIR.FFT.R
## 
## TODO: Add comment
##
## Author: sutashu
###############################################################################

###############################################################################
## MIR.CQT.CQgram -- calculates spectrogram using FFT
##
## Parameters:
##   musicsample -- data read using the loadSample() function from the sound package
##   hopSize     -- hop size in ms
##
MIR.FFT.Spectrogram <- function(musicsample, FFTSize=8192, hopSize=10, window=MIR.Window.Hamming, silent=FALSE)
{ 
  hopSizeSamples <- 0.01 * rate(musicsample)  # 10 ms for 44.1kHz data
  firstFrame     <- hopSize      # the center of the first frame is @10ms
  musicData      <- sound(musicsample)
  numberOfFrames <- floor(length(x) / hopSize + 1)
  windowFunction <- window(FFTSize)
  spectrogram    <- matrix(0, ncol=numberOfFrames, nrow=FFTSize)
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
      break
    spectrogram[,frameNr] <- fft(musicData[dataBegin:dataEnd] * windowFunction)
    
    if(!silent)
    {
      if(floor(20*frameNr/numberOfFrames) > percentDone)
      {
        percentDone <- floor(20*frameNr/numberOfFrames)
        cat("#")
      }
    }
  }
  cat("#]\n")
  return(spectrogram)
}
