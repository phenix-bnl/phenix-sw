#ifndef __DEMCGEATRACKTOWER_H__
#define __DEMCGEATRACKTOWER_H__


typedef struct {
   int id;
   int trkno;
   short input;
   float xyz[3];
   int twrkey[3];
   float edep[3];
   int nextid;
} DEMCGEATRACKTOWER_ST;
#endif /*__DEMCGEATRACKTOWER_H__*/
