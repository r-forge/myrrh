
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <alsa/asoundlib.h>

#include "timidity.h"

#define MIDINOTECOUNT      128
//TODO:#define MIDITIMEPRECISION  1

snd_seq_t *seq_handle;
int in_ports[4],
    out_ports[4];


int open_seq(int in_ports[], int out_ports[], int num_in, int num_out)
{
  int ii;
  char portname[64];

  // Open the ALSA sequencer
  if (snd_seq_open(&seq_handle, "default", SND_SEQ_OPEN_DUPLEX, 0) < 0)
  {
    fprintf(stderr, "Error opening ALSA sequencer.\n");
    return(-1);
  }

  // Set the client's name
  snd_seq_set_client_name(seq_handle, "myrrh");

  // Create input and output ports
  for (ii = 0; ii < num_in; ii++)
  {
    sprintf(portname, "myrrh in %d", ii);
    if ((in_ports[ii] = snd_seq_create_simple_port(seq_handle, portname,
              SND_SEQ_PORT_CAP_WRITE|SND_SEQ_PORT_CAP_SUBS_WRITE,
              SND_SEQ_PORT_TYPE_APPLICATION)) < 0)
    {
      fprintf(stderr, "Error creating sequencer port.\n");
      return(-1);
    }
  }
  for (ii = 0; ii < num_out; ii++) {
    sprintf(portname, "myrrh out %d", ii);
    if ((out_ports[ii] = snd_seq_create_simple_port(seq_handle, portname,
              SND_SEQ_PORT_CAP_READ|SND_SEQ_PORT_CAP_SUBS_READ,
              SND_SEQ_PORT_TYPE_APPLICATION)) < 0) {
      fprintf(stderr, "Error creating sequencer port.\n");
      return(-1);
    }
  }

  return(0);
}

void R_init_myrrh(DllInfo *info)
{
  Rprintf("Initializing ALSA...\n");
  if (open_seq(in_ports, out_ports, 0, 1) < 0)
    seq_handle = NULL;
}

void noteOn(int *noteNumber)
{
  snd_seq_event_t ev;

  snd_seq_ev_clear(&ev);
  snd_seq_ev_set_source(&ev, out_ports[0]);
  snd_seq_ev_set_noteon(&ev, 0, *noteNumber, 127);
  snd_seq_ev_set_subs(&ev);     // set broadcasting to subscribers
  snd_seq_ev_set_direct(&ev);   // no scheduling

  snd_seq_event_output_direct(seq_handle, &ev);
}

void noteOff(int *noteNumber)
{
  snd_seq_event_t ev;

  snd_seq_ev_clear(&ev);
  snd_seq_ev_set_source(&ev, out_ports[0]);
  snd_seq_ev_set_noteoff(&ev, 0, *noteNumber, 127);
  snd_seq_ev_set_subs(&ev);     // set broadcasting to subscribers
  snd_seq_ev_set_direct(&ev);   // no scheduling

  snd_seq_event_output_direct(seq_handle, &ev);
}

SEXP loadMIDI(SEXP filename)
{
  SEXP midiMatrix; // a matrix we're going to return to R
  double *R_midiMatrix; // a C interface to this matrix
  char *R_fileName = CHAR(STRING_ELT(filename, 0));
  // a C interface to the parameter
  MidiSong *song; // Timidity song structure
  SDL_RWops *rw; // file handle
  int ii, jj, maxTime;

  // Open the MIDI file
  if((rw = fopen(R_fileName, "r")) == NULL)
  {
    Rprintf("Could not open %s!\n", R_fileName);
    return R_NilValue;
  }

  // Load the MIDI file
  song = Timidity_LoadSong(rw, 0);
  fclose(rw);
  if(!song)
  {
    Rprintf("Failed to load %s!\n", R_fileName);
    return R_NilValue;
  }

  // Find the maximal time
  maxTime = 0;
  for(ii = 0; ii < song->event_count; ii++)
  {
    if(!((song->events[ii].type == 1) || (song->events[ii].type == 2)))
      continue;
    if((int)((double)(song->events[ii].time) / 44.1) > maxTime)
      maxTime = (int)((double)(song->events[ii].time) / 44.1);
  }
  Rprintf("File length: %.3f seconds\n", (double)maxTime / 1000.0);

  // Create the MIDI matrix
  int noteOnTime, noteOffTime, noteCount = 0;
  PROTECT(midiMatrix = allocMatrix(REALSXP, MIDINOTECOUNT, maxTime + 1));
  R_midiMatrix = REAL(midiMatrix);
  for(ii = 0; ii < song->event_count; ii++)
  {
    if(song->events[ii].type == 1)
    {
      noteOnTime = (int)((double)(song->events[ii].time) / 44.1);
      for(jj = ii; jj < song->event_count; jj++)
      {
        if((song->events[jj].type == 2) && (song->events[jj].a == song->events[ii].a))
        {
          noteOffTime = (int)((double)(song->events[jj].time) / 44.1) - 1;
          break;
        }
      }
      if(noteOffTime < noteOnTime)
      {
        Rprintf("Offset time < Onset time!\n");
        continue;
      }
      for(jj = noteOnTime; jj < noteOffTime; jj++)
        R_midiMatrix[song->events[ii].a + MIDINOTECOUNT * jj] = song->events[ii].b;
      noteCount++;
    }
  }
  Rprintf("Loaded %d notes\n", noteCount);

  // Cleanup and exit
  Timidity_FreeSong(song);
  UNPROTECT(1);
  return midiMatrix;
}
