#ifndef __DTECTRACK_H__
#define __DTECTRACK_H__


typedef struct {
   short id;
   float xyzin[3];
   float xyzout[3];
   float dxyin[2];
   float dxyout[2];
   float quality;
   short nhits;
   short ntime;
   short pid;
   float pidqual;
} DTECTRACK_ST;
#endif /*__DTECTRACK_H__*/
