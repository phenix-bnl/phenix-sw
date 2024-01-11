#ifndef __DEMCGEATRACK_H__
#define __DEMCGEATRACK_H__


typedef struct {
   int id;
   int trkno;
   short input;
   short anclvl;
   short pid;
   float ekin;
   float xyz[3];
   float ptot;
   float pxyz[3];
   float impxyz[3];
   int itparent;
   int idparent;
   int parent_ptr;
   int twrhit;
   float edep;
} DEMCGEATRACK_ST;
#endif /*__DEMCGEATRACK_H__*/
