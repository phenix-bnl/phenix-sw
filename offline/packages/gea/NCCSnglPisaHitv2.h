#ifndef __NCCSNGLHITV2_H_
#define __NCCSNGLHITV2_H_

#include "PHObject.h"
#include "NCCSnglPisaHit.h"

class NCCSnglPisaHitv2 : public NCCSnglPisaHit
{
 public:
  NCCSnglPisaHitv2();
  NCCSnglPisaHitv2(NCCSnglPisaHitv2 *track);  
  virtual ~NCCSnglPisaHitv2() {}
   int GetNCCCount() {return NCCCount; }
   void    SetNCCCount( const int val) {NCCCount   = val; return;}
   int GetNCC1Count() {return NCC1Count; }
   void    SetNCC1Count( const int val) {NCC1Count   = val; return;}
   int GetNCC2Count() {return NCC2Count; }
   void    SetNCC2Count( const int val) {NCC2Count   = val; return;}

   int         GetNEvent() const { return evt; }
   void    SetNEvent(const int val) {evt   = val; return;}
   int         GetId() const { return id; }
   void    SetId(const int val) {id   = val; return;}
   int         GetIarm() const { return arm; }
   void    SetIarm(const int val) {arm   = val; return;}
   int         GetMctrack() const { return mctrack; }
   void    SetMctrack(const int val) {mctrack   = val; return;}
   float         GetDedx() const { return dedx; }
   void    SetDedx(const float val) {dedx   = val; return;}
   int         GetNtrack() const { return track; }
   void    SetNtrack(const int val) {track   = val; return;}
   int         GetIncc() const { return incc; }
   void    SetIncc(const int val) {incc   = val; return;}
   int         GetSENID() const { return senid; }
   void    SetSENID(const int val) {senid   = val; return;}
   int         GetTWRID() const { return twrid; }
   void    SetTWRID(const int val) {twrid   = val; return;}
   float         GetTOFIN() const { return tof; }
   void    SetTOFIN(const float val) {tof   = val; return;}
   int         GetIsubevent() const { return isubevent; }
   void    SetIsubevent(const int val) {isubevent   = val; return;}
   int         GetNfile() const { return nfile; }
   void    SetNfile(const int val) {nfile   = val; return;}

 protected:

       int   evt; 
       int   id;
       int   arm;  // north = 1, south = 2
       int   mctrack;
       float dedx; 
       int   track;
       int   incc;
       int   twrid; 
       int   senid; 
       float tof;
       int   isubevent; 
       int   nfile;

   static int       NCCCount;    // Class global giving number of instances
   //
   static int       NCC1Count;    // Class global giving number of instances for NCC1
   static int       NCC2Count;    // Class global giving number of instances for NCC2

  ClassDef(NCCSnglPisaHitv2,1)
};
#endif
