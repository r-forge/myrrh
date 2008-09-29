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

*/

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef TIMIDITY_H
#define TIMIDITY_H

#define DEBUG_CHATTER
#if (defined DEBUG_CHATTER)
#define SNDDBG(x) printf x
#else
#define SNDDBG(x)
#endif

typedef int8_t Sint8;
typedef uint8_t Uint8;
typedef int16_t Sint16;
typedef int32_t Sint32;

typedef Sint16 sample_t;
typedef Sint32 final_volume_t;

#define SDL_RWops FILE
#define SDL_RWread(a,b,c,d) fread(b,c,d,a)
#define SDL_RWseek(a,b,c) fseek(a,b,c)

#define safe_malloc malloc

// Workarounds:
uint32_t SDL_SwapBE32(uint32_t value);
uint16_t SDL_SwapBE16(uint16_t value);
FILE *open_file(char *filename);

#define VIBRATO_SAMPLE_INCREMENTS 32

/* Maximum polyphony. */
#define MAX_VOICES	48

typedef struct {
  Sint32
    loop_start, loop_end, data_length,
    sample_rate, low_vel, high_vel, low_freq, high_freq, root_freq;
  Sint32
    envelope_rate[6], envelope_offset[6];
  float
    volume;
  sample_t *data;
  Sint32 
    tremolo_sweep_increment, tremolo_phase_increment, 
    vibrato_sweep_increment, vibrato_control_ratio;
  Uint8
    tremolo_depth, vibrato_depth,
    modes;
  Sint8
    panning, note_to_use;
} Sample;

typedef struct {
  int
    bank, program, volume, sustain, panning, pitchbend, expression, 
    mono, /* one note only on this channel -- not implemented yet */
    pitchsens;
  /* chorus, reverb... Coming soon to a 300-MHz, eight-way superscalar
     processor near you */
  float
    pitchfactor; /* precomputed pitch bend factor to save some fdiv's */
} Channel;

typedef struct {
  Uint8
    status, channel, note, velocity;
  Sample *sample;
  Sint32
    orig_frequency, frequency,
    sample_offset, sample_increment,
    envelope_volume, envelope_target, envelope_increment,
    tremolo_sweep, tremolo_sweep_position,
    tremolo_phase, tremolo_phase_increment,
    vibrato_sweep, vibrato_sweep_position;
  
  final_volume_t left_mix, right_mix;

  float
    left_amp, right_amp, tremolo_volume;
  Sint32
    vibrato_sample_increment[VIBRATO_SAMPLE_INCREMENTS];
  int
    vibrato_phase, vibrato_control_ratio, vibrato_control_counter,
    envelope_stage, control_counter, panning, panned;

} Voice;

typedef struct {
  int samples;
  Sample *sample;
} Instrument;

/* Shared data */
typedef struct {
  char *name;
  int note, amp, pan, strip_loop, strip_envelope, strip_tail;
} ToneBankElement;

typedef struct {
  ToneBankElement *tone;
  Instrument *instrument[128];
} ToneBank;

typedef struct {
    Sint32 time;
    Uint8 channel, type, a, b;
} MidiEvent;

typedef struct {
    MidiEvent event;
    void *next;
} MidiEventList;

struct _DLS_Data;
typedef struct _DLS_Data DLS_Patches;

typedef struct {
    int playing;
    SDL_RWops *rw;
    Sint32 rate;
    Sint32 encoding;
    float master_volume;
    Sint32 amplification;
    DLS_Patches *patches;
    ToneBank *tonebank[128];
    ToneBank *drumset[128];
    Instrument *default_instrument;
    int default_program;
    void (*write)(void *dp, Sint32 *lp, Sint32 c);
    int buffer_size;
    sample_t *resample_buffer;
    Sint32 *common_buffer;
    Sint32 *buffer_pointer;
    /* These would both fit into 32 bits, but they are often added in
       large multiples, so it's simpler to have two roomy ints */
    /* samples per MIDI delta-t */
    Sint32 sample_increment;
    Sint32 sample_correction;
    Channel channel[16];
    Voice voice[MAX_VOICES];
    int voices;
    Sint32 drumchannels;
    Sint32 buffered_count;
    Sint32 control_ratio;
    Sint32 lost_notes;
    Sint32 cut_notes;
    Sint32 samples;
    MidiEvent *events;
    MidiEvent *current_event;
    MidiEventList *evlist;
    Sint32 current_sample;
    Sint32 event_count;
    Sint32 at;
} MidiSong;

MidiSong *Timidity_LoadSong(SDL_RWops *rw, int convertTimeToSamples);
void Timidity_FreeSong(MidiSong *song);


#ifdef __cplusplus
}
#endif
#endif /* TIMIDITY_H */
