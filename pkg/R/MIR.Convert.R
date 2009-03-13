###################################################
## Converts MIDI note number to frequency.
## Assumes 12-Tone Equal Temperamet scale.
MIR.Convert.FrequencyFromMIDInote <- function(note) return(440 * 2 ^ (note / 12))

###################################################
## Converts frequency to MIDI note number.
## Assumes 12-Tone Equal Temperamet scale.
MIR.Convert.MIDInoteFromFrequency <- function(freq) return(noteFromFF(freq))

###################################################
## Converts piano key number to frequency.
## Assumes 12-Tone Equal Temperamet scale.
## Assumes 88-key piano keyboard.
MIR.Convert.FrequencyFromPianokey <- function(pianokey) MIR.Convert.FrequencyFromMIDInote(pianokey - 49)

###################################################
## Converts frequency to piano key number.
## Assumes 12-Tone Equal Temperamet scale.
## Assumes 88-key piano keyboard.
MIR.Convert.PianokeyFromFrequency <- function(freq) MIR.Convert.NoteFromFrequency(freq) + 49

.pianonotenames <- paste(c("A", "A#", "B", rep(c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B"),7), "C"), c(0,0,0,rep(c(1,2,3,4,5,6,7),each=12),8), sep="-")
MIR.Convert.NotenameFromPianokey <- function(pianokey) .pianonotenames[pianokey]

MIR.Convert.PianokeyFromNotename <- function(noteName)
{
  noteName <- toupper(noteName)
  if(grep("^[[:upper:]]#[[:digit:]]$", noteName, perl=TRUE))
  {
  }
  else if(grep("^[[:upper:]][[:digit:]]$", noteName, perl=TRUE))
  {
    as.numeric(substr(noteName, 3,3)) * 12
  }
  else
    return(-1)
}

