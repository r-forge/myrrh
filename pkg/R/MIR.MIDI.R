
MIR.MIDI.NoteOn  <- function(noteNumber, velocity = 1.0) .C("noteOn",  as.integer(noteNumber), as.integer(velocity*127))
MIR.MIDI.NoteOff <- function(noteNumber, velocity = 1.0) .C("noteOff", as.integer(noteNumber), as.integer(velocity*127))

MIR.MIDI.AllNotesOff <- function(noteNumber)
{
  for(ii in 0:127)
    MIR.MIDI.NoteOff(ii)
}

MIR.MIDI.PlayNote <- function(noteNumber, duration = 0.5)
{
  MIR.MIDI.NoteOn(noteNumber)
  Sys.sleep(duration)
  MIR.MIDI.NoteOff(noteNumber)
}

MIR.MIDI.PlayMatrix <- function(pianoRoll, hop = 0.1, minNoteNumber = 0)
{
  if(ncol(pianoRoll) < 2)
    return
  if(nrow(pianoRoll) + minNoteNumber - 1 > 127)
    pianoRoll <- pianoRoll[1:(128-minNoteNumber),]
  for(rowNumber in 1:nrow(pianoRoll))
    if(pianoRoll[rowNumber,1] > 0)
      MIR.MIDI.NoteOn(rowNumber + minNoteNumber - 1)
  Sys.sleep(hop)
  for(timePos in 2:ncol(pianoRoll))
  {
    for(rowNumber in 1:nrow(pianoRoll))
    {
      if((pianoRoll[rowNumber,timePos] > 0) && (pianoRoll[rowNumber,timePos-1] == 0))
        MIR.MIDI.NoteOn(rowNumber + minNoteNumber - 1)
      if((pianoRoll[rowNumber,timePos] == 0) && (pianoRoll[rowNumber,timePos-1] > 0))
        MIR.MIDI.NoteOff(rowNumber + minNoteNumber - 1)
    }
    Sys.sleep(hop)
  }
  for(rowNumber in 1:nrow(pianoRoll))
    if(pianoRoll[rowNumber,ncol(pianoRoll)] > 0)
      MIR.MIDI.NoteOff(rowNumber + minNoteNumber - 1)
}

setConstructorS3("MIDI", function(filename)
{
  midiData = .Call("loadMIDI", as.character(filename))
  if(is.null(midiData))
    throw("Error while loading the MIDI file!")

  division <- midiData[4, which(midiData[2,] == 6)]
  if(division < 0)
    throw("SMPTE time units are not supported!\n")
  quarterNote <- 0.5  # default is 120 QPM
  currentTime <- 0
  tempo  <- matrix(0, ncol=3, nrow=0)
  events <- matrix(0, ncol=4, nrow=0) 
  for(ii in 1:ncol(midiData))
  {
    currentTime <- currentTime + midiData[1, ii]
    if(midiData[2,ii] == 5)
      currentTime <- 0
    else if(midiData[2,ii] == 0)
    {
      quarterNote  <- (midiData[3, ii]*65536 + midiData[4, ii]*256 + midiData[5, ii]) / 1000000
      if(nrow(tempo) == 0)
        tempo <- rbind(tempo, c(currentTime, quarterNote, currentTime * 0.5))
      else
      {
        midiTimeDiff <- currentTime - tempo[nrow(tempo), 1]
        tempo <- rbind(tempo, c(currentTime, quarterNote, tempo[nrow(tempo), 3] + midiTimeDiff * tempo[nrow(tempo), 2] / division))
      }
    }
    else if((midiData[2,ii] == 1) || (midiData[2,ii] == 2) || (midiData[2,ii] == 3) || (midiData[2,ii] == 4))
      events <- rbind(events, c(currentTime, midiData[2,ii], midiData[4,ii], midiData[5,ii]))
  }
  if(!is.matrix(events))
    throw("There were no events!")
  if(nrow(tempo) > 1)
  {
    newTempo <- tempo[nrow(tempo), ]
    for(ii in (nrow(tempo)-1):1)   # this is to get rid of tempo changes in the same delta time.... is it necessary? I don't remember how it got here actually
    {
      if(tempo[ii,1] != tempo[ii+1,1])
        newTempo <- rbind(tempo[ii,], newTempo)
    }
    tempo  <- matrix(newTempo, ncol=3)  # a hack, there might not have been any tempo changes and we get a vector instead of a matrix
  }

  # Convert all midi times to absolute times and sort the event list
  for(ii in 1:nrow(events))
  {
    tempos       <- which(tempo[,1] <= events[ii,1])
    lastTempo    <- tempo[length(tempos), ]
    events[ii,1] <- lastTempo[3] + lastTempo[2] * (events[ii,1] - lastTempo[1]) / division
  }
  events <- events[sort(events[,1], index.return=TRUE)$ix, ]
  events[,4] <- events[,4] / 127.0

  extend(Object(), "MIDI",
    .midiData = midiData,
    .events   = events,
    .filename = filename,
    .division = division,
    .tempo    = tempo
  );
})

setMethodS3("plot", "MIDI", function(this, requantize = TRUE, newQuantization = 0.05, ...)
{
  image(t(this$as.pianoroll(requantize, newQuantization)), col=rgb(seq(1,0,length.out=256), seq(1,0,length.out=256), seq(1,0,length.out=256)))
})

setMethodS3("tempo", "MIDI", function(this, timeinquarternotes = FALSE, ...)
{
  if(timeinquarternotes)
    return(cbind(this$.tempo[,1] / this$.division, 60.0 / this$.tempo[,2]))
  else
    return(cbind(this$.tempo[,3], 60.0 / this$.tempo[,2]))
})

setMethodS3("as.pianoroll", "MIDI", function(this, requantize = TRUE, newQuantization = 0.05, ...)
{
  maxTime <- max(this$.events[,1])
  pianoRoll <- matrix(0, nrow=128, ncol=ceiling(maxTime / newQuantization))
  for(note in 1:128)
  {
    cat("Note ", note)
    noteEvents <- matrix(this$.events[this$.events[,3] == note, ], ncol=4)
    cat(" (", nrow(noteEvents), ")\n")
    if(nrow(noteEvents) == 0)
      next

    currentNoteEventIndex <- 0
    currentAmplitude <- 0.0
    sustainPressed <- FALSE
    sustaining     <- FALSE
    for(ii in 1:ncol(pianoRoll))
    {
      time <- (ii - 1) * newQuantization
      if((currentNoteEventIndex < nrow(noteEvents)) && (time >= noteEvents[currentNoteEventIndex + 1, 1]))
      {
        currentNoteEventIndex <- currentNoteEventIndex + 1
        if(noteEvents[currentNoteEventIndex, 2] == 1)  # note on
          currentAmplitude <- noteEvents[currentNoteEventIndex, 4]
        else if((noteEvents[currentNoteEventIndex, 2] == 2) || (noteEvents[currentNoteEventIndex, 2] == 4))  # note off or all off
        {
          if(sustainPressed)
            sustaining <- TRUE
          else
#             currentAmplitude <- currentAmplitude - noteEvents[currentNoteEventIndex, 4]
            currentAmplitude <- currentAmplitude - 1.0
        }
        else
        {
          if(noteEvents[currentNoteEventIndex, 4] == 0)
          {
            sustainPressed <- FALSE
            if(sustaining)
            {
              currentAmplitude <- 0.0
              sustaining <- FALSE
            }
          }
          else
            sustainPressed <- TRUE
        }
        if(currentAmplitude < 0.0)
          currentAmplitude <- 0.0
      }
      pianoRoll[note, ii] <- currentAmplitude
    }
  }

  return(pianoRoll)
})

setMethodS3("play", "MIDI", function(this, ...)
{
  for(ii in 1:nrow(this$.events))
  {
    if(this$.events[ii,2] == 1)
    {
      MIR.MIDI.NoteOn(this$.events[ii,3], this$.events[ii,4])
      cat("Note on  ", this$.events[ii,3], " (", this$.events[ii,4], ")\n")
    }
    else if(this$.events[ii,2] == 2)
    {
      MIR.MIDI.NoteOff(this$.events[ii,3], this$.events[ii,4])
      cat("Note off ", this$.events[ii,3], " (", this$.events[ii,4], ")\n")
    }
    if(ii < nrow(this$.events))
      Sys.sleep(this$.events[ii+1,1] - this$.events[ii,1])
  }
})      

#This function writes a simple one-track MIDI file
MIR.MIDI.Save <- function(activityMatrix, filename, msPerColumn, minMIDInote, overwrite=TRUE)
{
  chunkHeader <- function(ID, len)
  {
    data <- rep(0, 4)
    data[1] <- floor(len/0x01000000)
    data[2] <- floor((len - data[1] * 0x01000000) / 0x00010000)
    data[3] <- floor((len - data[1] * 0x01000000 - data[2] * 0x00010000) / 0x00000100)
    data[4] <- (len - data[1] * 0x01000000 - data[2] * 0x00010000 - data[3] * 0x00000100)
    return(c(charToRaw(ID), as.vector(data,mode="raw")))
  }
  calculateTime <- function(timePos, sRate, hopSize)
  {
    return(2*960 * timePos * hopSize / sRate)   # in ms
  }
  generateVariableQuantity <- function(value)
  {
    variableQuantity <- vector(0, mode="raw")
    data <- rep(0, 4)
    data[1] <- floor(value/(128*128*128))  # MSB
    data[2] <- floor((value - data[1] * (128*128*128)) / (128*128))
    data[3] <- floor((value - data[1] * (128*128) - data[2] * (128*128)) / (128))
    data[4] <- (value - data[1] * (128*128*128) - data[2] * (128*128) - data[3] * (128)) # LSB
    someAlreadyWritten <- FALSE
    for(byteNr in 1:4)
    {
      if((data[byteNr] > 0) || someAlreadyWritten)
      {
        variableQuantity <- c(variableQuantity, as.raw(data[byteNr]))
        someAlreadyWritten <- TRUE
      }
    }
    if(!someAlreadyWritten)
      variableQuantity <-c(variableQuantity, as.raw(0))
    if(length(variableQuantity) > 1)
      for(byteNr in 1:(length(variableQuantity)-1))
        variableQuantity[byteNr] <- as.raw(as.numeric(variableQuantity[byteNr]) + 128)  # set the MSB
    return(variableQuantity)
  }

  # First: analyze the activity matrix, find onsets, offsets and amplitudes
  cat("Note onset/offset detection (might take a while)...\n")
  MIDInotes <- matrix(0,nrow=5,ncol=0) # Rows: onset/offset time, type (1:onset, 2:offset), velocity, noteNr, channel
  percentDone <- 0
  channel <- 1
  timePos <- 1
  msPerColumn <- msPerColumn * 1.92    # 2*960/1000
  for(note in 1:nrow(activityMatrix))
  {
    if(sum(activityMatrix[note,]) == 0)
      next
    differences <- c(activityMatrix[note,], 0) - c(0, activityMatrix[note,])
    for(onset in which(differences > 0))
      MIDInotes <- cbind(MIDInotes, c((onset-1) * msPerColumn, 1, activityMatrix[note, onset], minMIDInote + note, channel))
    for(offset in which(differences < 0))
      MIDInotes <- cbind(MIDInotes, c((offset-1) * msPerColumn, 2, 0, minMIDInote + note, channel))
  }
  MIDInotes[3,] <- ifelse(MIDInotes[3,] > 100, 100, MIDInotes[3,])
  
  # Second: sort the list event
  cat("Sorting event list...\n")
  MIDInotes <- MIDInotes[,sort(MIDInotes[1,],index.return=TRUE)$ix]

  # Third: generate the delta times
  cat("Calculating delta times...\n")
  for(eventNr in ncol(MIDInotes):2)
    MIDInotes[1,eventNr] <- MIDInotes[1,eventNr] - MIDInotes[1,eventNr-1]

  # Fourth: generate the MIDI track data
  cat("Generating MIDI data...\n")
  trackData <- vector(0, mode="raw")
  for(eventNr in 1:ncol(MIDInotes))
  {
    if(MIDInotes[2,eventNr] == 1)
    {
      trackData <- c(trackData, generateVariableQuantity(MIDInotes[1,eventNr]))
      trackData <- c(trackData, as.vector(c(144 - 1 + MIDInotes[5,eventNr], MIDInotes[4,eventNr], MIDInotes[3,eventNr]), mode="raw"))
    }
    if(MIDInotes[2,eventNr] == 2)
    {
      trackData <- c(trackData, generateVariableQuantity(MIDInotes[1,eventNr]))
      trackData <- c(trackData, as.vector(c(128 - 1 + MIDInotes[5,eventNr], MIDInotes[4,eventNr], MIDInotes[3,eventNr]), mode="raw"))
    }
  }
  trackData <- c(trackData, generateVariableQuantity(960))
  trackData <- c(trackData, as.vector(c(0xFF, 0x2F, 0x00), mode="raw"))  # End-of-track marker
  cat(length(trackData), " bytes \n")

  # Final: write the results to a MIDI file
  cat("Saving data...\n")
  MIDIfile <- file(filename, "wb")
  writeBin(chunkHeader("MThd", 6), MIDIfile)
  writeBin(as.vector(c(0,0,0,1,0xe7,0x28),mode="raw"), MIDIfile) # single-track MIDI file with 1ms time precision (simplest to write...)
  writeBin(chunkHeader("MTrk", length(trackData)), MIDIfile)
  writeBin(trackData, MIDIfile) # single-track MIDI 
  close(MIDIfile)
}

#generateInstrumentSound <- function(note, samplingFrequency=11025, sampleLength=11025, ADSR_A=0.01, ADSR_D=0.1, ADSR_S=0.29, ADSR_R=0.6, sustainLevel=0.15, partials=4)
#{
#  # Generate the sound
#  instrumentSound <- rep(0, sampleLength)
#  frequency <- 440 * 2^(note / 12)   # physical frequency
#  frequency <- frequency / samplingFrequency
#  for(partial in 1:partials)
#  {
#    if(partial - floor(partial / 2) * 2 == 0)
#      instrumentSound <- instrumentSound + sin(frequency * partial * 2 * pi * (1:sampleLength - 1)) / (partial^2)
#    else
#      instrumentSound <- instrumentSound + sin(frequency * partial * 2 * pi * (1:sampleLength - 1)) / (partial)
#  }
#
#  # Create the sound envelope
#  a <- 1 / (sampleLength * ADSR_A)
#  for(sampleNumber in 1:(sampleLength * ADSR_A))
#    instrumentSound[sampleNumber] <- instrumentSound[sampleNumber] * a * sampleNumber
#
#  a <- (1 - sustainLevel) / (sampleLength * ADSR_D)
#  for(sampleNumber in (sampleLength * ADSR_A):(sampleLength * (ADSR_A + ADSR_D)))
#    instrumentSound[sampleNumber] <- instrumentSound[sampleNumber] * (1 - a * (sampleNumber - sampleLength * ADSR_A))
#
#  for(sampleNumber in (sampleLength * (ADSR_A + ADSR_D)):(sampleLength * (ADSR_A + ADSR_D + ADSR_S)))
#    instrumentSound[sampleNumber] <- instrumentSound[sampleNumber] * sustainLevel
#
#  a <- sustainLevel / (sampleLength * ADSR_R)
#  for(sampleNumber in (sampleLength * (ADSR_A + ADSR_D + ADSR_S)):sampleLength)
#    instrumentSound[sampleNumber] <- instrumentSound[sampleNumber] * (sustainLevel - a * (sampleNumber - sampleLength * (ADSR_A + ADSR_D + ADSR_S)))
#
#  return(instrumentSound)
#}
#
#createSoundFromNoteOnsets <- function(noteActivities, minNote, frameLength, samplingFrequency=11025)
#{
#  noteCount  <- dim(noteActivities)[1]
#  songLength <- dim(noteActivities)[2]
#
#  # Generate note sounds
#  noteSounds <- matrix(0, nrow=noteCount, ncol=samplingFrequency)  # sound length of 1s
#  for(ii in 1:noteCount)
#    noteSounds[ii,] <- generateInstrumentSound(ii + minNote - 1, partials=1)
#
#  # Generate the song
#  songSound <- rep(0, songLength * frameLength + samplingFrequency)
#
#  for(noteNumber in 1:noteCount)
#  {
#    cat("Generating sound for note ", noteNumber)
#    for(jj in 2:songLength)
#    {
#      if((noteActivities[noteNumber, jj] > 0) && (noteActivities[noteNumber, jj-1] == 0))
#      {
#        cat(".")
#        noteBegin <- (jj - 1) * frameLength + 1
#        noteEnd   <- noteBegin + samplingFrequency
#        songSound[noteBegin:noteEnd] <- songSound[noteBegin:noteEnd] + noteSounds[noteNumber,] * noteActivities[noteNumber, jj]
##         songSound[noteBegin:noteEnd] <- songSound[noteBegin:noteEnd] + noteSounds[noteNumber,]
#      }
#    }
#    cat("\n")
#  }
#
#  return(songSound)
#}
#
#createSoundFromNoteOnsets.old <- function(noteActivities, minNote, frameLength, samplingFrequency=11025)
#{
#  noteCount  <- dim(noteActivities)[1]
#  songLength <- dim(noteActivities)[2]
#
#  # Generate note sounds
#  noteSounds <- matrix(0, nrow=noteCount, ncol=samplingFrequency)  # sound length of 1s
#  for(ii in 1:noteCount)
#    noteSounds[ii,] <- generateInstrumentSound(ii + minNote - 1)
#
#  # Generate the song
#  songSound <- rep(0, songLength * frameLength + samplingFrequency)
#
#  for(noteNumber in 1:noteCount)
#  {
#    cat("Generating sound for note ", noteNumber)
#    for(jj in 1:songLength)
#    {
#      if(noteActivities[noteNumber, jj] > 0)
#      {
#        cat(".")
#        noteBegin <- (jj - 1) * frameLength + 1
#        noteEnd   <- noteBegin + samplingFrequency
#        songSound[noteBegin:noteEnd] <- songSound[noteBegin:noteEnd] + noteSounds[noteNumber,] * noteActivities[noteNumber, jj]
##         songSound[noteBegin:noteEnd] <- songSound[noteBegin:noteEnd] + noteSounds[noteNumber,]
#      }
#    }
#    cat("\n")
#  }
#
#  return(songSound)
#}
