#ifndef __DPADEVAL_H__
#define __DPADEVAL_H__


typedef struct {
   short id;
   short clusid;
   short ghitid;
   float deltax;
   float deltaz;
   float deltar;
   short nhits;
   short type;
} DPADEVAL_ST;
#endif /*__DPADEVAL_H__*/
