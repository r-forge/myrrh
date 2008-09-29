MIR.Multipitch.Backend.HNNMA <- function(filename, hopSize = 0.01*16000, binsPerOctave=36)
{
    # Data loading options
    firstFrame    <- hopSize      # the center of the first frame is @10ms

    # Note ranges:
    #   A4 = 69 MIDI = 0 STS
    #   Lowest note in MIREX competition is 55 Hz (A0 = 33 MIDI, -36 STS)
    #   Highest note in MIREX competition is ??? (D#6 = 99 MIDI, 30 STS)
    #                   note range size = 67
    # min_note    <- -33-12
    min_note    <- -36
    basisSize   <- 70
    max_note    <- min_note + basisSize - 1
    pitch_range <- min_note:max_note

    # This numbers are for constantQ transform only
    minNote      <- min_note - 1
    maxNote      <- max_note + 2*12 + 1
    minFrequency <- MIR.Tools.FFFromNote(minNote)
    maxFrequency <- MIR.Tools.FFFromNote(maxNote)

    cat("Loading sound files...")
    musicsample  <- loadSample(filename)
    cat(" done.\n")

    cat("Generating constant Q transform kernel matrix\n")
    qKernel <- MIR.CQT.GenerateKernel(minFrequency, maxFrequency, binsPerOctave=binsPerOctave, samplingFrequency=rate(musicsample))

    cat("Calculating the constant Q trasform\n")
    musicData <- rep(0, length(sound(musicsample)) + dim(qKernel)[1] - firstFrame)
    musicData[(dim(qKernel)[1]/2+firstFrame+1):(length(sound(musicsample))+dim(qKernel)[1]/2+firstFrame)] <- sound(musicsample)
    X <- MIR.CQT.CQgram(musicData, qKernel, samplingFrequency=rate(musicsample), hopSize=hopSize)

    A <- matrix(0, nrow=dim(X)[1], ncol=basisSize)
    for(noteNumber in 1:basisSize)
    {
    frequency <- MIR.Tools.FFFromNote(min_note + noteNumber - 1)
    cat(frequency, ":\t")
    for(harmonicNumber in 1:10)
    {
        binNumber <- (noteFromFF(frequency * harmonicNumber) - minNote) * (binsPerOctave/12) + 1
        cat(binNumber, " ")
        if(binNumber+3 >= dim(X)[1])
        break
        A[binNumber, noteNumber] <- 1
        A[binNumber-1, noteNumber] <- A[binNumber, noteNumber] / 10
        A[binNumber+1, noteNumber] <- A[binNumber, noteNumber] / 10
    }
    cat("\n")
    }

    cat("\nCalculating the HNMF\n")
    sparse.code <- MIR.Tools.HNNMA(X, A, iterations=1000, epsilon=0.0001, theta=1, binsPerOctave=36)

    activities <- sparse.code$S * MIR.Tools.fundamentalAnalysis(sparse.code$A, sparse.code$S, min_note, binsPerOctave)
    maxActivitity <- max(activities)
    for(ii in 1:basisSize)
    {
    if(max(activities[ii,]) < maxActivitity*0.1)
        activities[ii,] <- 0
    else
        activities[ii,] <- activities[ii,] / max(activities[ii,])
    }

    return(activities)
}