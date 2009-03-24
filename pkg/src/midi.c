/**************************************************************
    Parts are based on TiMidity
    Copyright (C) 1995 Tuukka Toivonen <toivonen@clinet.fi>
    Copyright (C) 2008 Stanislaw Raczynski
 **************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define END_OF_TRACK 1

#define EVENT_TEMPO 0
#define EVENT_NOTEON 1
#define EVENT_NOTEOFF 2
#define EVENT_SUSTAIN 2
#define EVENT_ALLOFF 2

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
  uint8_t a,b,c;
  struct rawEvent *nextEvent;
} rawEvent;

typedef struct {
  uint8_t midiNumber;
  uint32_t startTime, endTime;   // times are in ms
  uint8_t velocity;
} note;

static char *metaEventNames[]={"Text event: ", "Text: ", "Copyright: ", "Track name: ",
                               "Instrument: ", "Lyric: ", "Marker: ", "Cue point: "};

struct rawEvent *rawEventListBegin, *rawEventListEnd;

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
  printf("%s\n", s);
  free(s);

  return 0;
}

int readMidiHeader(FILE *fp, uint32_t headerLength)
{
  mainHeader headerData;

  if(headerLength != 6)
  {
    printf("The MIDI header is too short!\n");
    return -1;
  }
  if(fread(&headerData, sizeof(uint8_t), headerLength, fp) != headerLength)
  {
    printf("Cannot read the main header!\n");
    return -1;
  }
  headerData.format = SDL_SwapBE16(headerData.format);
  headerData.tracks = SDL_SwapBE16(headerData.tracks);
  headerData.division = SDL_SwapBE16(headerData.division);
  printf("MIDI format: ");
  switch(headerData.format)
  {
    case 0:
      printf("single track\n");
      break;
    case 1:
      printf("multiple track\n");
      break;
    case 2:
      printf("multiple song\n");
      break;
    default:
      printf("unknown\n");
  }
  printf("Track count: %d\n", headerData.tracks);
  if(headerData.division > 0)
    printf("Delta time: %d ticks per beat\n", headerData.division);
  else if(headerData.division < 0)
    printf("Delta time: %d (SMTP)\n", headerData.division);
  else
  {
    printf("\nI don't understand this delta time (0)!\n");
    return -1;
  }

  if(headerData.format > 1)
  {
    printf("This MIDI format is not supported!\n");
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
}

int readMidiEvent(FILE *fp)
{
  uint32_t time, length;
  uint8_t eventType, metaEventType, midiEventType,
          a, b, c, d, channel;

  time = readVarLength(fp);
  if(fread(&eventType, 1, 1, fp) != 1)
  {
    printf("Error while trying to read MIDI event!\n");
    return -1;
  }
  if(eventType == 0xF0 || eventType == 0xF7) // SysEx event
  {
    printf("SysEx event -- skipping\n");
    length = readVarLength(fp);
    fseek(fp, length, SEEK_CUR);             // skip it
  }
  else if(eventType == 0xFF)                 // meta event
  {
    if(fread(&metaEventType, 1, 1, fp) != 1)
    {
      printf("Error while trying to read MIDI event!\n");
      return -1;
    }
    length = readVarLength(fp);
    if(metaEventType > 0 && metaEventType < 16)
    {
      printf("%s", metaEventNames[(metaEventType > 7) ? 0 : metaEventType]);
      dumpstring(fp, length);
    }
    else
    {
      switch(metaEventType)
      {
        case 0x2F:
          printf("End of track\n");
          return END_OF_TRACK;
        case 0x51:   // tempo change
          //printf("Tempo change event\n");
          fread(&a, 1, 1, fp);
          fread(&b, 1, 1, fp);
          fread(&c, 1, 1, fp);
          addMidiEvent(time, EVENT_TEMPO, a, b, c);
          break;
        case 0x58:   // time signature
          printf("Time signature: ");
          fread(&a, 1, 1, fp);
          fread(&b, 1, 1, fp);
          fread(&c, 1, 1, fp);
          fread(&d, 1, 1, fp);
          printf("%d/%d, %d MIDI clocks per metronome click, %d notated 32nd-notes in a MIDI quarter-note\n", a, b, c, d);
          break;
        case 0x59:   // key signature
          printf("Key signature: ");
          fread(&a, 1, 1, fp);
          fread(&b, 1, 1, fp);
          printf("%d", (int8_t)a);
          if(b == 0)
            printf("-major\n");
          else if(b == 1)
            printf("-minor\n");
          else
            printf("-???\n");
          break;
        default:
          printf("Unknown meta event 0x%02X\n", metaEventType);
          fseek(fp, length, SEEK_CUR);
          break;
      }
    }
  }
  else
  {
//    printf("Midi event @%d", time);
    a = eventType;
    if(a & 0x80) /* status byte */
    {
      channel = a & 0x0F;
      midiEventType = (a>>4) & 0x07;
      fread(&a, 1, 1,fp);
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
        printf("\tError: Key pressure events are not supported!\n");
        return -1;
      case 3: // Control change
        fread(&b, 1, 1, fp);
        b &= 0x7F;
        switch(a)
        {
          case 7:  // main volume -- ignoring
            break;
          case 64:
            addMidiEvent(time, EVENT_SUSTAIN, channel, a, b);
            break;
          case 123:
            addMidiEvent(time, EVENT_ALLOFF, channel, a, b);
            break;
          default:
            //printf("\tControl change event %d: %d -- ignoring\n", a, b);
            break;
        }
        break;
    case 4: // Program change
      break;
    case 5: // Channel pressure
      break;
    case 6: // Pitch wheel
      fread(&b, 1, 1, fp);
      break;
    default:
      printf("Unknown midi event type\n");
      return -1;
    }
  }

  return 0;
}

int readMidiTruck(FILE *fp, uint32_t trackLength)
{
  int errorCode;

  printf("Reading a track...\n");
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
    printf("Warning: unknown chunk type (%c%c%c%c). Ignoring.\n", chunkData.magic[0], chunkData.magic[1],
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
  if(newEvent)
  {
    while(nextEvent = event->nextEvent)
    {
      free(event);
      event = nextEvent;
    }
    free(event);
  }
}

int main(void)
{
  FILE *fp;

  if((fp=fopen("test.mid", "rb"))==NULL)
  {
    printf("Cannot open MIDI file '%s'!\n", "test.mid");
    exit(1);
  }
  if(readNoteEvents(fp) == 1)
    printf("Successfully read the MIDI file!\n");
  fclose(fp);

  // this is where we convert the event list into a matrix to pass it to R

  freeRawEventList();
  return 0;
}
