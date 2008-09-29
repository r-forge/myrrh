MIR.Tools.fundamentalAnalysis <- function(A, S, min_note, binsPerOctave, nsets=1)
{
  noteGains <- rep(0, dim(A)[2]);
  minNote   <- min_note - 1
  basisSize <- dim(A)[2]/nsets
      for(atomNr in 1:basisSize)
  {
    frequency <- MIR.FrequencyFromNote(min_note + atomNr-1);
    for(setNr in 0:(nsets-1))
    {
      noteGains[atomNr+setNr*basisSize] <- A[(noteFromFF(frequency) - minNote) * (binsPerOctave/12) + 1, atomNr+setNr*basisSize] * sum(S[atomNr+setNr*basisSize, ]);
    }
  }
  return(noteGains)
}
