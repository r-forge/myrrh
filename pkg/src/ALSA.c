
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <alsa/asoundlib.h>

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
    sprintf(portname, "myrrh input %d", ii);
    if ((in_ports[ii] = snd_seq_create_simple_port(seq_handle, portname,
              SND_SEQ_PORT_CAP_WRITE|SND_SEQ_PORT_CAP_SUBS_WRITE,
              SND_SEQ_PORT_TYPE_APPLICATION)) < 0)
    {
      fprintf(stderr, "Error creating sequencer port.\n");
      return(-1);
    }
  }
  for (ii = 0; ii < num_out; ii++) {
    sprintf(portname, "myrrh output %d", ii);
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
  Rprintf("Initializing ALSA sequencer...\n");
  if (open_seq(in_ports, out_ports, 0, 1) < 0)
    seq_handle = NULL;
}

void noteOn(int *noteNumber, int *velocity)
{
  snd_seq_event_t ev;

  snd_seq_ev_clear(&ev);
  snd_seq_ev_set_source(&ev, out_ports[0]);
  snd_seq_ev_set_noteon(&ev, 0, *noteNumber, *velocity);
  snd_seq_ev_set_subs(&ev);     // set broadcasting to subscribers
  snd_seq_ev_set_direct(&ev);   // no scheduling

  snd_seq_event_output_direct(seq_handle, &ev);
}

void noteOff(int *noteNumber, int *velocity)
{
  snd_seq_event_t ev;

  snd_seq_ev_clear(&ev);
  snd_seq_ev_set_source(&ev, out_ports[0]);
  snd_seq_ev_set_noteoff(&ev, 0, *noteNumber, *velocity);
  snd_seq_ev_set_subs(&ev);     // set broadcasting to subscribers
  snd_seq_ev_set_direct(&ev);   // no scheduling

  snd_seq_event_output_direct(seq_handle, &ev);
}
