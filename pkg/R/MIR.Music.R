# MIR.Music.R
#
# Various functions and tables that are relevant to music theory.
#
# Author: Stanisław Andrzej Raczyński
###############################################################################

# Notes in diatonic scale
.noteNames <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")

## Tabele powiazanych kluczy molowych i durowych
#                   C    C#   D    D#  E   F   F#  G   G#  A   A#  B
#                        Db        Eb          Gb      Ab      Bb
.relativeMinor <- c(-10, -11, -12, -1, -2, -3, -4, -5, -6, -7, -8, -9)
# molowy klucz powiazany z danym kluczem durowym

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
.minorScale      <- c(1,0,1,0,1,1,0,1,0,1,0,1)   # a (?) minor scale

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
                                                    #   C C#D D#E F F#G G#A A#B C
MIR.Music.NoteBelongsToKey <- function(note, key)   #   c(1,0,1,0,1,1,0,1,0,1,0,1)
{ 
  if(MIR.Music.IsMajorKey(key))
    return(.majorScale[(note - MIR.Music.KeyNumber(key) + 1) %% 12 + 1] == 1) 
  else if(MIR.Music.IsMinorKey(key))
    return(.minorScale[(note - MIR.Music.KeyNumber(key) + 1) %% 12 + 1] == 1)
  else
    return(FALSE)
}

MIR.Music.IsMajorTriad <- function(chord)
{
  if(!is.na(match(MIR.Music.ChordNumber(chord), c(1:7))))
    return(TRUE)
  else
    return(FALSE)
}

MIR.Music.IsMinorTriad <- function(chord)
{
  if(!is.na(match(MIR.Music.ChordNumber(chord), c(11:17))))
    return(TRUE)
  else
    return(FALSE)
}

MIR.Music.IsDiminishedTriad <- function(chord)
{
  if(!is.na(match(MIR.Music.ChordNumber(chord), c(21:27))))
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
