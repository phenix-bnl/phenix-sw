#ifndef __PRIMARY_H__
#define __PRIMARY_H__


typedef struct {
   int key;
   int event_track;
   int subevent_track;
   int true_track;
   short subevent;
   short idpart;
   short nfile;
   float px_momentum;
   float py_momentum;
   float pz_momentum;
} PRIMARY_ST;
#endif /*__PRIMARY_H__*/
