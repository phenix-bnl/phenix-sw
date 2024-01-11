#ifndef __DEMCGEATOWERTRACK_H__
#define __DEMCGEATOWERTRACK_H__


typedef struct {
   int id;
   int twrkey;
   short input;
   int trkno[3];
   float edep[3];
   float toffirst[3];
} DEMCGEATOWERTRACK_ST;
#endif /*__DEMCGEATOWERTRACK_H__*/
