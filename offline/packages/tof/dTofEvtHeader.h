#ifndef __DTOFEVTHEADER_H__
#define __DTOFEVTHEADER_H__


typedef struct {
   int run;
   int date;
   int time;
   int evtseq;
   int scaledtrig;
   int rawtrig;
   int livetrig;
} DTOFEVTHEADER_ST;
#endif /*__DTOFEVTHEADER_H__*/
