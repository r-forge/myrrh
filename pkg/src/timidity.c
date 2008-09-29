/*
 
    This is based on TiMidity
    Copyright (C) 1995 Tuukka Toivonen <toivonen@clinet.fi>
    Copyright (C) 2008 Stanislaw Raczynski

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   readmidi.h 
   
   */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "timidity.h"
#include "instrum.h"
#include "readmidi.h"
#include "output.h"
#include "options.h"

ToneBank *master_tonebank[128], *master_drumset[128];

#define MAXWORDS 10

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

// Workaround:
FILE *open_file(char *filename)
{
  return fopen(filename, "r");
}

int Timidity_Init_NoConfig()
{
  /* Allocate memory for the standard tonebank and drumset */
  master_tonebank[0] = safe_malloc(sizeof(ToneBank));
  memset(master_tonebank[0], 0, sizeof(ToneBank));
  master_tonebank[0]->tone = safe_malloc(128 * sizeof(ToneBankElement));
  memset(master_tonebank[0]->tone, 0, 128 * sizeof(ToneBankElement));

  master_drumset[0] = safe_malloc(sizeof(ToneBank));
  memset(master_drumset[0], 0, sizeof(ToneBank));
  master_drumset[0]->tone = safe_malloc(128 * sizeof(ToneBankElement));
  memset(master_drumset[0]->tone, 0, 128 * sizeof(ToneBankElement));

  return 0;
}

int Timidity_Init()
{
  return Timidity_Init_NoConfig();
}

MidiSong *Timidity_LoadDLSSong(SDL_RWops *rw, DLS_Patches *patches, int convertTimeToSamples)
{
  MidiSong *song;
  Sint32 events;
  int i;

  if (rw == NULL)
      return NULL;
  
  /* Allocate memory for the song */
  song = (MidiSong *)safe_malloc(sizeof(*song));
  memset(song, 0, sizeof(*song));
  song->patches = patches;

  for (i = 0; i < 128; i++)
  {
    if (master_tonebank[i])
    {
      song->tonebank[i] = safe_malloc(sizeof(ToneBank));
      memset(song->tonebank[i], 0, sizeof(ToneBank));
      song->tonebank[i]->tone = master_tonebank[i]->tone;
    }
    if (master_drumset[i])
    {
      song->drumset[i] = safe_malloc(sizeof(ToneBank));
      memset(song->drumset[i], 0, sizeof(ToneBank));
      song->drumset[i]->tone = master_drumset[i]->tone;
    }
  }

  song->amplification = DEFAULT_AMPLIFICATION;
  song->voices = DEFAULT_VOICES;
  song->drumchannels = DEFAULT_DRUMCHANNELS;

  song->rw = rw;

  song->rate = 44100;
  song->encoding = 0;
  song->encoding |= PE_16BIT;
  song->encoding |= PE_SIGNED;
  song->encoding |= PE_MONO;
//  song->write = s32tos8;

  // These should be deleted:
  song->buffer_size = 1024;
  song->resample_buffer = safe_malloc(1024 * sizeof(sample_t));
  song->common_buffer = safe_malloc(1024 * 2 * sizeof(Sint32));
  
  song->control_ratio = 44100 / CONTROLS_PER_SECOND;
  if (song->control_ratio < 1)
      song->control_ratio = 1;
  else if (song->control_ratio > MAX_CONTROL_RATIO)
      song->control_ratio = MAX_CONTROL_RATIO;

  song->lost_notes = 0;
  song->cut_notes = 0;

  if(!(song->events = read_midi_file(song, &events, &song->samples, convertTimeToSamples)))
  {
	free(song);
	return NULL;
  }

  /* The RWops can safely be closed at this point, but let's make that the
   * responsibility of the caller.
   */
  
  /* Make sure everything is okay */
  if (!song->events) {
    free(song);
    return(NULL);
  }

  song->default_instrument = 0;
  song->default_program = DEFAULT_PROGRAM;

  return(song);
}

MidiSong *Timidity_LoadSong(SDL_RWops *rw, int convertTimeToSamples)
{
  return Timidity_LoadDLSSong(rw, NULL, convertTimeToSamples);
}

void Timidity_FreeSong(MidiSong *song)
{
  int i;

//  free_instruments(song);

  for (i = 0; i < 128; i++)
  {
    if (song->tonebank[i])
      free(song->tonebank[i]);
    if (song->drumset[i])
      free(song->drumset[i]);
  }
  
  free(song->common_buffer);
  free(song->resample_buffer);
  free(song->events);
  free(song);
}

void Timidity_Exit(void)
{
  int i, j;

  for (i = 0; i < 128; i++)
  {
    if (master_tonebank[i])
    {
      ToneBankElement *e = master_tonebank[i]->tone;
      if (e != NULL)
      {
        for (j = 0; j < 128; j++)
        {
          if (e[j].name != NULL)
            free(e[j].name);
        }
        free(e);
      }
      free(master_tonebank[i]);
    }
    if (master_drumset[i])
    {
      ToneBankElement *e = master_drumset[i]->tone;
      if (e != NULL)
      {
        for (j = 0; j < 128; j++)
        {
          if (e[j].name != NULL)
            free(e[j].name);
        }
        free(e);
      }
      free(master_drumset[i]);
    }
  }

//  free_pathlist();
}
