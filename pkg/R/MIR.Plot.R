MIR.Plot.Spectrum <- function(soundData, logScale=F, main="")
{
  if(logScale)
    plot(abs(fft(sound(soundData)))[1,], axes=F, log="x", xlab="Frequency [Hz]", ylab="Amplitude", main=main, type="l")
  else
    plot(abs(fft(sound(soundData)))[1,], axes=F, xlab="Frequency [Hz]", ylab="Amplitude", main=main, type="l")
  axis(1, at=seq(100, rate(soundData)/2, by=100) * soundLength(soundData) / (rate(soundData)/2), label=paste(seq(100, rate(soundData)/2, by=100), "Hz"))
  axis(2)
  box()
}
