# MIR.Music.R
#
# Various functions and tables that are relevant to music theory.
#
# Author: Stanisław Andrzej Raczyński
###############################################################################

# Notes in diatonic scale
.noteNames <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
.noteNamesEnharmonic <- c("C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B")

## Table of minor keys relative to a major keys
#                   C    C#   D    D#  E   F   F#  G   G#  A   A#  B
#                        Db        Eb          Gb      Ab      Bb
.relativeMinor <- c(-10, -11, -12, -1, -2, -3, -4, -5, -6, -7, -8, -9)

#                   C   C#  D   D#  E   F   F#   G    G#   A   A#  B
#                       Db      Eb          Gb        Ab       Bb
.relativeMajor <- c(-4, -5, -6, -7, -8, -9, -10, -11, -12, -1, -2, -3)
# durowy klucz powiazany z danym kluczem molowym

## Positions in the circle of fifths:
#                       C C#  D  D#  E  F   F# G   G# A  A#  B
#                         Db     Eb         Gb     Ab    Bb
.positionsCOFmajor <- c(0, 7, 2,  9, 4, 11, 6, 1,  8, 3, 10, 5)   # major scales
.positionsCOFminor <- c(9, 4, 11, 6, 1, 8,  3, 10, 5, 0, 7,  2)   # minor scales

.majorScale      <- c(1,0,1,0,1,1,0,1,0,1,0,1)   # a major scale
.minorScale      <- c(1,0,1,1,0,1,0,1,1,0,1,0)   # a harmonic minor scale

.majorTriad      <- c(1,0,0,0,1,0,0,1,0,0,0,0)   # Major third + Perfect fifth
.minorTriad      <- c(1,0,0,1,0,0,0,1,0,0,0,0)   # Minor third + Perfect fifth 
.majorSeventh    <- c(1,0,0,0,1,0,0,1,0,0,0,1)   # Major third + Perfect fifth
.minorSeventh    <- c(1,0,0,1,0,0,0,1,0,0,1,0)   # Minor third + Perfect fifth 
.diminishedTriad <- c(1,0,0,1,0,0,1,0,0,0,0,0)   # Minor third + Diminished fifth
.augmentedTriad  <- c(1,0,0,0,1,0,0,0,1,0,0,0)   # Major third + Augmented fifth (?)

.diatonicTriadsMajor <- c(0, 10, 10, 0, 0, 10, 20)  # diatonic triads of a major scale
.diatonicTriadsMinor <- c(10, 20, 0, 10, 0, 0, 0)   # diatonic triads of a natural minor scale

MIR.Music.NoteName <- function(note) paste(.noteNames[note %% 12 + 1], floor(note / 12), sep="")

# Returns name of a key given its number
MIR.Music.KeyName <- function(key) ifelse(key > 0, paste(.noteNames[abs(key)], "-maj", sep=""), paste(.noteNames[abs(key)], "-min", sep=""))
MIR.Music.KeyNameEnharmonic <- function(key) ifelse(key > 0, paste(.noteNamesEnharmonic[abs(key)], "-maj", sep=""), paste(.noteNamesEnharmonic[abs(key)], "-min", sep=""))

names(.relativeMinor) <- MIR.Music.KeyName(1:12)
names(.relativeMajor) <- MIR.Music.KeyName(-12:-1)
names(.positionsCOFmajor) <- MIR.Music.KeyName(1:12)
names(.positionsCOFminor) <- MIR.Music.KeyName(-12:-1)

MIR.Music.KeyNumber <- function(key)
{
  if(is.numeric(key))
  {
    if((key >= -12) && (key <= 12))
      return(as.integer(key))
    else
      return(NA)
  }
  if(!is.character(key))
    return(NA)
  keyNumber <- match(key, MIR.Music.KeyName(1:12))
  if(!is.na(keyNumber))
    return(keyNumber)
  keyNumber <- match(key, MIR.Music.KeyName(-1:-12))
  if(!is.na(keyNumber))
    return(-keyNumber)
  keyNumber <- match(key, MIR.Music.KeyNameEnharmonic(1:12))
  if(!is.na(keyNumber))
    return(keyNumber)
  keyNumber <- match(key, MIR.Music.KeyNameEnharmonic(-1:-12))
  if(!is.na(keyNumber))
    return(-keyNumber)
  return(NA)  # key not found
}

MIR.Music.ChordName <- function(chord)
{
  if(is.numeric(chord))
  {
    returnValue <- chord
    returnValue[] <- 0
    for(ii in 1:length(chord))
    {
      if((chord[ii] >= 1) && (chord[ii] <= 7))             # major triad
        returnValue[ii] <- paste(as.roman(chord[ii]))
      else if((chord[ii] >= 11) && (chord[ii] <= 17))      # minor triad
        returnValue[ii] <-  tolower(paste(as.roman(chord[ii] - 10)))
      else if((chord[ii] >= 21) && (chord[ii] <= 27))      # diminished triad
        returnValue[ii] <- tolower(paste(as.roman(chord[ii] - 20), "o", sep="^"))
    }
    return(returnValue)
  }
  else
    stop(simpleError("Bad 'chord' type"))
}

MIR.Music.ChordNumber <- function(chord)
{
  if(is.numeric(chord))
  {
    if(!is.na(match(as.integer(chord), 1:7)))
      return(as.integer(chord))
    if(!is.na(match(as.integer(chord), 11:17)))
      return(as.integer(chord) - 10)
    if(!is.na(match(as.integer(chord), 21:27)))
      return(as.integer(chord) - 20)
    else
      return(NA)
  }
  if(!is.character(chord))
    return(NA)
  for(chordType in 0:2)
  {
    chordNumber <- match(chord, MIR.Music.ChordName(seq(chordType * 10 + 1, chordType * 10 + 7)))
    if(!is.na(chordNumber))
      return(chordType * 10 + chordNumber)
  }
  return(NA)  # key not found
}

# Return the distance in the circle of fifths
MIR.Music.DistanceInCOF <- function(key1, key2)
{
  distanceInCOF_ <- function(key1, key2)
  {
    if(MIR.Music.IsMinorKey(key1))
    {
      if(MIR.Music.IsMinorKey(key2))
        return(min(abs(.positionsCOFminor[-key1] - .positionsCOFminor[-key2]),
                abs(12+.positionsCOFminor[-key1] - .positionsCOFminor[-key2])))
      else if(MIR.Music.IsMajorKey(key2))
        return(min(abs(.positionsCOFminor[-key1] - .positionsCOFmajor[key2]),
                abs(12+.positionsCOFminor[-key1] - .positionsCOFmajor[key2])))
      else
        return(NA)
    }
    else if(MIR.Music.IsMajorKey(key1))
    {
      if(MIR.Music.IsMinorKey(key2))
        return(min(abs(.positionsCOFmajor[key1] - .positionsCOFminor[-key2]),
                abs(12+.positionsCOFmajor[key1] - .positionsCOFminor[-key2])))
      else if(MIR.Music.IsMajorKey(key2))
        return(min(abs(.positionsCOFmajor[key1] - .positionsCOFmajor[key2]),
                abs(12+.positionsCOFmajor[key1] - .positionsCOFmajor[key2])))
      else
        return(NA)
    }
    else
      return(NA)
  }
  return(min(distanceInCOF_(key1, key2), distanceInCOF_(key2, key1)))
}

MIR.Music.IsMajorKey <- function(key)
{
  if(!is.na(match(MIR.Music.KeyNumber(key), c(1:12))))
    return(TRUE)
  else
    return(FALSE)
}

MIR.Music.IsMinorKey <- function(key)
{
  if(!is.na(match(MIR.Music.KeyNumber(key), c(-12:-1))))
    return(TRUE)
  else
    return(FALSE)
}

MIR.Music.RelativeKey <- function(key)
{
  if(MIR.Music.IsMajorKey(key))
    return(.relativeMinor(key))
  else if(MIR.Music.IsMinorKey(key))
    return(.relativeMajor(key))
  else
    return(NA)
}

MIR.Music.NoteBelongsToKey <- function(note, key)
{ 
  if(MIR.Music.IsMajorKey(key))
    return(.majorScale[(note - MIR.Music.KeyNumber(key) + 1) %% 12 + 1] == 1) 
  else if(MIR.Music.IsMinorKey(key))
    return(.minorScale[(note + MIR.Music.KeyNumber(key) + 1) %% 12 + 1] == 1)
  else
    return(FALSE)
}

MIR.Music.IsMajorTriad <- function(chord)
{
  if(!is.na(match(chord, c(1:7))))
    return(TRUE)
  else
    return(FALSE)
}

MIR.Music.IsMinorTriad <- function(chord)
{
  if(!is.na(match(chord, c(11:17))))
    return(TRUE)
  else
    return(FALSE)
}

MIR.Music.IsDiminishedTriad <- function(chord)
{
  if(!is.na(match(chord, c(21:27))))
    return(TRUE)
  else
    return(FALSE)
}

MIR.Music.NoteBelongsToChord <- function(note, key, chord)
{
  if(MIR.Music.IsMajorTriad(chord))
  {
    if(MIR.Music.IsMajorKey(key))
    {
      chord_int <- which(.majorScale == 1)[(MIR.Music.ChordNumber(chord) + c(-1,1,3,5)) %% 7 + 1]
      return(any(((note - MIR.Music.KeyNumber(key) + 1) %% 12 + 1) == chord_int))
    }
    else if(MIR.Music.IsMinorKey(key))
    {
      chord_int <- which(.majorScale == 1)[(MIR.Music.ChordNumber(chord) + c(-1,1,3,5)) %% 7 + 1]
      return(any(((note + MIR.Music.KeyNumber(key) + 1) %% 12 + 1) == chord_int))
#       chord_int <- which(.minorScale == 1)[MIR.Music.ChordNumber(chord)]
#       return(.majorSeventh[(note - MIR.Music.KeyNumber(key) + 1 - chord_int + 1) %% 12 + 1] == 1)
    }
    else
      return(FALSE)
  }
  else if(MIR.Music.IsMinorTriad(chord))
  {
    if(MIR.Music.IsMajorKey(key))
    {
      chord_int <- which(.majorScale == 1)[(MIR.Music.ChordNumber(chord) - 10 + c(-1,1,3,5)) %% 7 + 1]
      return(any(((note - MIR.Music.KeyNumber(key) + 1) %% 12 + 1) == chord_int))
#       chord_int <- which(.majorScale == 1)[MIR.Music.ChordNumber(chord) - 10]
#       return(.minorSeventh[(note - MIR.Music.KeyNumber(key) + 1 - chord_int + 1) %% 12 + 1] == 1)
    }
    else if(MIR.Music.IsMinorKey(key))
    {
      chord_int <- which(.majorScale == 1)[(MIR.Music.ChordNumber(chord) - 10 + c(-1,1,3,5)) %% 7 + 1]
      return(any(((note + MIR.Music.KeyNumber(key) + 1) %% 12 + 1) == chord_int))
#       chord_int <- which(.minorScale == 1)[MIR.Music.ChordNumber(chord) - 10]
#       return(.minorSeventh[(note - MIR.Music.KeyNumber(key) + 1 - chord_int + 1) %% 12 + 1] == 1)
    }
    else
      return(FALSE)
  }
#   else if(MIR.Music.IsDiminishedTriad(chord))
#   {
#     if(MIR.Music.IsMajorKey(key))
#     {
#       chord_int <- which(.majorScale == 1)[MIR.Music.ChordNumber(chord) - 20]
#       return(.diminishedTriad[(note - MIR.Music.KeyNumber(key) + 1 - chord_int + 1) %% 12 + 1] == 1)
#     }
#     else if(MIR.Music.IsMinorKey(key))
#     {
#       chord_int <- which(.minorScale == 1)[MIR.Music.ChordNumber(chord) - 20]
#       return(.diminishedTriad[(note - MIR.Music.KeyNumber(key) + 1 - chord_int + 1) %% 12 + 1] == 1)
#     }
#     else
#       return(FALSE)
#   }
  else
    return(FALSE)
}

# Lerdahl distance between chords belonging to the same key,
# based on the TPS (Tonal Pitch Space) [1]
MIR.Music.ChordDistance.TPS <- function(chord1, chord2, key)
{
  if(MIR.Music.IsMajorKey(key))
  {
    root1 <- (which(.majorScale == 1)[chord1 %% 10] + MIR.Music.KeyNumber(key) - 2) %% 12
    root2 <- (which(.majorScale == 1)[chord2 %% 10] + MIR.Music.KeyNumber(key) - 2) %% 12
  }
  else
  {
    root1 <- (which(.minorScale == 1)[chord1 %% 10] - MIR.Music.KeyNumber(key) - 2) %% 12
    root2 <- (which(.minorScale == 1)[chord2 %% 10] - MIR.Music.KeyNumber(key) - 2) %% 12
  }
  distanceInCOF <- MIR.Music.DistanceInCOF(root1 + 1, root2 + 1)
  
  .calculateThirdAndFifth <- function(chord, root)
  {
    if(MIR.Music.IsMajorTriad(chord))
    {
      third <- (root + which(.majorTriad == 1)[2] - 1) %% 12
      fifth <- (root + which(.majorTriad == 1)[3] - 1) %% 12
    }
    else if(MIR.Music.IsMinorTriad(chord))
    {
      third <- (root + which(.minorTriad == 1)[2] - 1) %% 12
      fifth <- (root + which(.minorTriad == 1)[3] - 1) %% 12
    }
    else if(MIR.Music.IsDiminishedTriad(chord))
    {
      third <- (root + which(.diminishedTriad == 1)[2] - 1) %% 12
      fifth <- (root + which(.diminishedTriad == 1)[3] - 1) %% 12
    }
    else
      return(c())
    return(c(third, fifth))
  }
  
  thirdAndFifth <- .calculateThirdAndFifth(chord1, root1)
  if(length(thirdAndFifth) == 0)
    return(Inf)
  third1 <- thirdAndFifth[1]
  fifth1 <- thirdAndFifth[2]
  
  cat("Root  1 = ", root1, " (", .noteNames[root1+1], ")\n", sep="")
  cat("Third 1 = ", third1, " (", .noteNames[third1+1], ")\n", sep="")
  cat("Fifth 1 = ", fifth1, " (", .noteNames[fifth1+1], ")\n", sep="")
  if(!(MIR.Music.NoteBelongsToKey(third1, key) && MIR.Music.NoteBelongsToKey(fifth1, key)))
  {
    cat("'chord1' does not belong to key 'key'\n")
    return(NaN)
  }
  
  thirdAndFifth <- .calculateThirdAndFifth(chord2, root2)
  if(length(thirdAndFifth) == 0)
    return(Inf)
  third2 <- thirdAndFifth[1]
  fifth2 <- thirdAndFifth[2]

  cat("Root  2 = ", root2, " (", .noteNames[root2+1], ")\n", sep="")
  cat("Third 2 = ", third2, " (", .noteNames[third2+1], ")\n", sep="")
  cat("Fifth 2 = ", fifth2, " (", .noteNames[fifth2+1], ")\n", sep="")
  if(!(MIR.Music.NoteBelongsToKey(third2, key) && MIR.Music.NoteBelongsToKey(fifth2, key)))
  {
    cat("'chord2' does not belong to key 'key'\n")
    return(NaN)
  }
  
  cat("Distance in COF = ", distanceInCOF, "\n")
  for(ii in 0:11)
  {
    if(ii == root1 && ii == root2)
      cat(ii, "B ", sep="")
    else if(ii == root1 && ii != root2)
      cat(ii, "F ", sep="")
    else if(ii != root1 && ii == root2)
      cat(ii, "S ", sep="")
    else
      cat("   ")
  }
  cat("\n")
  for(ii in 0:11)
  {
    if(!is.na(match(ii, c(root1, fifth1))) && !is.na(match(ii, c(root2, fifth2))))
      cat(ii, "B ", sep="")
    else if(!is.na(match(ii, c(root1, fifth1))) && is.na(match(ii, c(root2, fifth2))))
      cat(ii, "F ", sep="")
    else if(is.na(match(ii, c(root1, fifth1))) && !is.na(match(ii, c(root2, fifth2))))
      cat(ii, "S ", sep="")
    else
      cat("   ")
  }
  cat("\n")
  for(ii in 0:11)
  {
    if(!is.na(match(ii, c(root1, fifth1, third1))) && !is.na(match(ii, c(root2, fifth2, third2))))
      cat(ii, "B ", sep="")
    else if(!is.na(match(ii, c(root1, fifth1, third1))) && is.na(match(ii, c(root2, fifth2, third2))))
      cat(ii, "F ", sep="")
    else if(is.na(match(ii, c(root1, fifth1, third1))) && !is.na(match(ii, c(root2, fifth2, third2))))
      cat(ii, "S ", sep="")
    else
      cat("   ")
  }
  cat("\n")
  for(ii in 0:11)
  {
    if(MIR.Music.NoteBelongsToKey(ii, key))
      cat(ii, "  ", sep="")
    else
      cat("   ")
  }
  cat("\n")
  for(ii in 0:11)
    cat(ii, "  ", sep="")
  cat("\n")
  
  disagreeingNotes <- 0
  if(root1 != root2)
    disagreeingNotes <- disagreeingNotes + 3
  if(fifth1 != fifth2)
    disagreeingNotes <- disagreeingNotes + 2
  if(fifth1 == root2)
    disagreeingNotes <- disagreeingNotes - 2
  if(fifth2 == root1)
    disagreeingNotes <- disagreeingNotes - 2
  if(third1 != third2)
    disagreeingNotes <- disagreeingNotes + 1
  if(third1 == root2)
    disagreeingNotes <- disagreeingNotes - 1
  if(third1 == fifth2)
    disagreeingNotes <- disagreeingNotes - 1
  if(third2 == root1)
    disagreeingNotes <- disagreeingNotes - 1
  if(third2 == fifth1)
    disagreeingNotes <- disagreeingNotes - 1
  return(disagreeingNotes + distanceInCOF)
}

# References:
#  [1] F.Lerdahl. "Tonal Pitch Space". Oxford University Press, 2001
