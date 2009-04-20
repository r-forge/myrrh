/**************************************************************
    Parts are based on TiMidity
    Copyright (C) 1995 Tuukka Toivonen <toivonen@clinet.fi>

    The rest is by me:
    Copyright (C) 2008 Stanislaw Raczynski
 **************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#define END_OF_TRACK 1

#define EVENT_TEMPO    0
#define EVENT_NOTEON   1
#define EVENT_NOTEOFF  2
#define EVENT_SUSTAIN  3
#define EVENT_ALLOFF   4
#define EVENT_EOT      5
#define EVENT_DIVISION 6
#define EVENT_OTHER    100
  // EVENT_OTHER is for ignored events and is used to hold only delta times

const char *midiMagicChars = "MThd";
const char *trackMagicChars = "MTrk";

typedef struct {
  char magic[4];
  int32_t dataLength;
} chunkHeader;

typedef struct {
  uint16_t format;
  uint16_t tracks;
  uint16_t division;
} mainHeader;

typedef struct {
  uint32_t time;
  uint8_t type;
  uint32_t a,b,c;
  struct rawEvent *nextEvent;
} rawEvent;

static char *metaEventNames[]={"Text event: ", "Text: ", "Copyright: ", "Track name: ",
                               "Instrument: ", "Lyric: ", "Marker: ", "Cue point: "};

rawEvent *rawEventListBegin, *rawEventListEnd;
uint32_t eventCount = 0;

uint32_t SDL_SwapBE32(uint32_t value)
{
   uint32_t swapped = 0;

   swapped += (value & 0xFF000000) >> 24;
   swapped += (value & 0x00FF0000) >> 8;
   swapped += (value & 0x0000FF00) << 8;
   swapped += (value & 0x000000FF) << 24;

   return swapped;
}

uint16_t SDL_SwapBE16(uint16_t value)
{
   uint16_t swapped = 0;

   swapped += (value & 0xFF00) >> 8;
   swapped += (value & 0x00FF) << 8;

   return swapped;
}

uint32_t readVarLength(FILE *fp)
{
  int32_t l = 0;
  uint8_t c;
  for(;;)
  {
      fread(&c, 1, 1, fp);
      l += (c & 0x7f);
      if(!(c & 0x80))
        return l;
      l <<= 7;
  }
}

static int dumpstring(FILE *fp, uint32_t len)
{
  signed char *s = malloc(len+1);

  if(fread(s, 1, len, fp) != len)
  {
    free(s);
    return -1;
  }
  s[len]='\0';
  while(len--)
  {
    if(s[len]<32)
      s[len]='.';
  }
  Rprintf("%s\n", s);
  free(s);

  return 0;
}

int readMidiHeader(FILE *fp, uint32_t headerLength)
{
  mainHeader headerData;

  if(headerLength != 6)
  {
    Rprintf("The MIDI header is too short!\n");
    return -1;
  }
  if(fread(&headerData, sizeof(uint8_t), headerLength, fp) != headerLength)
  {
    Rprintf("Cannot read the main header!\n");
    return -1;
  }
  headerData.format = SDL_SwapBE16(headerData.format);
  headerData.tracks = SDL_SwapBE16(headerData.tracks);
  headerData.division = SDL_SwapBE16(headerData.division);
  addMidiEvent(0, EVENT_DIVISION, 0, headerData.division, 0);
  Rprintf("MIDI format: ");
  switch(headerData.format)
  {
    case 0:
      Rprintf("single track\n");
      break;
    case 1:
      Rprintf("multiple track\n");
      break;
    case 2:
      Rprintf("multiple song\n");
      break;
    default:
      Rprintf("unknown\n");
  }
  Rprintf("Track count: %d\n", headerData.tracks);
  if(headerData.division > 0)
    Rprintf("Delta time: %d ticks per beat\n", headerData.division);
  else if(headerData.division < 0)
    Rprintf("Delta time: %d (SMTP)\n", headerData.division);
  else
  {
    Rprintf("\nI don't understand this delta time (0)!\n");
    return -1;
  }

  if(headerData.format > 1)
  {
    Rprintf("This MIDI format is not supported!\n");
    return -1;
  }

  return 0;
}

void addMidiEvent(uint32_t time, uint8_t type, uint8_t a, uint32_t b, uint32_t c)
{
  rawEvent *newEvent = malloc(sizeof(rawEvent));
  newEvent->a = a;
  newEvent->b = b;
  newEvent->c = c;
  newEvent->time = time;
  newEvent->type = type;
  newEvent->nextEvent = NULL;
  if(rawEventListBegin == NULL)
  {
    rawEventListBegin = newEvent;
    rawEventListEnd   = newEvent;
  }
  else
  {
    rawEventListEnd->nextEvent = newEvent;
    rawEventListEnd = newEvent;
  }
  eventCount++;
}

int readMidiEvent(FILE *fp)
{
  uint32_t time, length;
  static uint8_t eventType;
  static uint8_t metaEventType = 0x00;
  static uint8_t midiEventType = 0x00;
  static uint8_t a, b, c, d, channel;

  time = readVarLength(fp);
  if(fread(&eventType, 1, 1, fp) != 1)
  {
    Rprintf("Error while trying to read MIDI event!\n");
    return -1;
  }
  if(eventType == 0xF0 || eventType == 0xF7) // SysEx event
  {
    Rprintf("SysEx event -- skipping\n");
    length = readVarLength(fp);
    fseek(fp, length, SEEK_CUR);             // skip it
    addMidiEvent(time, EVENT_OTHER, 0, 0, 0);
  }
  else if(eventType == 0xFF)                 // meta event
  {
    if(fread(&metaEventType, 1, 1, fp) != 1)
    {
      Rprintf("Error while trying to read MIDI event!\n");
      return -1;
    }
    length = readVarLength(fp);
    if(metaEventType > 0 && metaEventType < 16)
    {
      Rprintf("%s", metaEventNames[(metaEventType > 7) ? 0 : metaEventType]);
      dumpstring(fp, length);
      addMidiEvent(time, EVENT_OTHER, 0, 0, 0);
    }
    else
    {
      switch(metaEventType)
      {
        case 0x2F:
          Rprintf("End of track\n");
          addMidiEvent(time, EVENT_EOT, 0, 0, 0);
          return END_OF_TRACK;
        case 0x51:   // tempo change
          //Rprintf("Tempo change event\n");
          fread(&a, 1, 1, fp);
          fread(&b, 1, 1, fp);
          fread(&c, 1, 1, fp);
          addMidiEvent(time, EVENT_TEMPO, a, b, c);
          break;
        case 0x58:   // time signature
          Rprintf("Time signature: ");
          fread(&a, 1, 1, fp);
          fread(&b, 1, 1, fp);
          fread(&c, 1, 1, fp);
          fread(&d, 1, 1, fp);
          Rprintf("%d/%d, %d MIDI clocks per metronome click, %d notated 32nd-notes in a MIDI quarter-note\n", a, b, c, d);
          addMidiEvent(time, EVENT_OTHER, 0, 0, 0);
          break;
        case 0x59:   // key signature
          Rprintf("Key signature: ");
          fread(&a, 1, 1, fp);
          fread(&b, 1, 1, fp);
          Rprintf("%d", (int8_t)a);
          if(b == 0)
            Rprintf("-major\n");
          else if(b == 1)
            Rprintf("-minor\n");
          else
            Rprintf("-???\n");
          addMidiEvent(time, EVENT_OTHER, 0, 0, 0);
          break;
        default:
          Rprintf("Unknown meta event 0x%02X\n", metaEventType);
          fseek(fp, length, SEEK_CUR);
          addMidiEvent(time, EVENT_OTHER, 0, 0, 0);
          break;
      }
    }
  }
  else
  {
//    Rprintf("Midi event @%d", time);
    a = eventType;
//    Rprintf("eventType = 0x%02x\n", eventType);
    if(a & 0x80) /* status byte */
    {
      channel = a & 0x0F;
      midiEventType = (a>>4) & 0x07;
      fread(&a, 1, 1,fp);
//      Rprintf("0x%02x ", a);
      a &= 0x7F;
    }
    switch(midiEventType)
    {
      case 0: // Note off
        fread(&b, 1, 1, fp);
        b &= 0x7F;
        addMidiEvent(time, EVENT_NOTEOFF, channel, a, b);
        break;
      case 1: // Note on
        fread(&b, 1, 1, fp);
        b &= 0x7F;
        addMidiEvent(time, EVENT_NOTEON, channel, a, b);
        break;
      case 2: // Key Pressure (note aftertouch)
        fread(&b, 1, 1, fp);
        b &= 0x7F;
        Rprintf("\tError: Key pressure events are not supported!\n");
        return -1;
      case 3: // Control change
        fread(&b, 1, 1, fp);
        b &= 0x7F;
        switch(a)
        {
          case 7:  // main volume -- ignoring
            addMidiEvent(time, EVENT_OTHER, 0, 0, 0);
            break;
          case 64:
            addMidiEvent(time, EVENT_SUSTAIN, channel, a, b);
            break;
          case 123:
            addMidiEvent(time, EVENT_ALLOFF, channel, a, b);
            break;
          default:
            //Rprintf("\tControl change event %d: %d -- ignoring\n", a, b);
            addMidiEvent(time, EVENT_OTHER, 0, 0, 0);
            break;
        }
        break;
      case 4: // Program change
        addMidiEvent(time, EVENT_OTHER, 0, 0, 0);
        break;
      case 5: // Channel pressure
        addMidiEvent(time, EVENT_OTHER, 0, 0, 0);
        break;
      case 6: // Pitch wheel
        fread(&b, 1, 1, fp);
        addMidiEvent(time, EVENT_OTHER, 0, 0, 0);
        break;
      default:
        Rprintf("This can't be happening: event type 0x%02x\n", midiEventType);
        return -1;
    }
  }

  return 0;
}

int readMidiTruck(FILE *fp, uint32_t trackLength)
{
  int errorCode;

  Rprintf("Reading a track...\n");
  while(!(errorCode = readMidiEvent(fp)));
  if(errorCode == END_OF_TRACK)
    return 0;
  else
    return errorCode;
}

int readMidiChunk(FILE *fp)
{
  chunkHeader chunkData;
  uint32_t bytesRead;

  bytesRead = fread(&chunkData, 1, sizeof(chunkHeader), fp);
  if(bytesRead == 0)
    return 1;  // no MIDI chunks to read -- it is not an error
  else if(bytesRead != sizeof(chunkHeader))
    return -1; // incorrect MIDI chunk -- this is an error
  chunkData.dataLength = SDL_SwapBE32(chunkData.dataLength);

  if(strncmp(chunkData.magic, midiMagicChars, 4) == 0)
    return(readMidiHeader(fp, chunkData.dataLength));
  else if(strncmp(chunkData.magic, trackMagicChars, 4) == 0)
    return(readMidiTruck(fp, chunkData.dataLength));
  else
  {
    Rprintf("Warning: unknown chunk type (%c%c%c%c). Ignoring.\n", chunkData.magic[0], chunkData.magic[1],
                                                                  chunkData.magic[2], chunkData.magic[3]);
    fseek(fp, chunkData.dataLength, SEEK_CUR);
  }

  return 0;
}

int readNoteEvents(FILE *fp)
{
  int errorCode;

  while(!(errorCode = readMidiChunk(fp)));
  return errorCode;
}

void freeRawEventList(void)
{
  rawEvent *event = rawEventListBegin;
  rawEvent *nextEvent;
  if(event)
  {
    while(nextEvent = event->nextEvent)
    {
      free(event);
      event = nextEvent;
    }
    free(event);
  }
  rawEventListBegin = NULL;
  rawEventListEnd   = NULL;
}

SEXP loadMIDI(SEXP filename)
{
  SEXP midiMatrix; // a matrix we're going to return to R
  int *R_midiMatrix; // a C interface to this matrix
  char *R_fileName = CHAR(STRING_ELT(filename, 0));
  FILE *fp;
  rawEvent *event;
  int jj;

  eventCount = 0;
  freeRawEventList();

  if((fp=fopen(R_fileName, "rb"))==NULL)
  {
    Rprintf("Cannot open MIDI file '%s'!\n", R_fileName);
    return R_NilValue;
  }
  if(readNoteEvents(fp) == 1)
  {
    Rprintf("Successfully read the MIDI file!\n");
    fclose(fp);
  }
  else
  {
	Rprintf("Error while loading the MIDI file!\n");
    fclose(fp);
    return R_NilValue;
  }

  // create the matrix we are going to pass to R
  PROTECT(midiMatrix = allocMatrix(INTSXP, 5, eventCount));
  R_midiMatrix = INTEGER(midiMatrix);
  event = rawEventListBegin;
  for(jj = 0; jj < 5*eventCount; jj += 5)
  {
	if(!event)
	{
	  Rprintf("The MIDI event list is too short (found %d, expected %d)!\n", jj/5, eventCount);
	  break;
    }
	R_midiMatrix[0 + jj] = event->time;
	R_midiMatrix[1 + jj] = event->type;
	R_midiMatrix[2 + jj] = event->a;
	R_midiMatrix[3 + jj] = event->b;
	R_midiMatrix[4 + jj] = event->c;
    event = event->nextEvent;
  }
  UNPROTECT(1);  // we're done with the matrix

  freeRawEventList(); // free the memory

  if(jj < 5*eventCount)
	return R_NilValue;
  else
    return midiMatrix;
}
