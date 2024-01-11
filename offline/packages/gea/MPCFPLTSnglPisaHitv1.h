#ifndef __MPCFPLTSNGLHITV1_H_
#define __MPCFPLTSNGLHITV1_H_

#include "PHObject.h"
#include "MPCFPLTSnglPisaHit.h"

class MPCFPLTSnglPisaHitv1 : public MPCFPLTSnglPisaHit
{
 public:
  MPCFPLTSnglPisaHitv1();
  MPCFPLTSnglPisaHitv1(MPCFPLTSnglPisaHitv1 *track);  
  virtual ~MPCFPLTSnglPisaHitv1() {}
   int GetMPCFPLTCount() {return MPCFPLTCount; }
   void    SetMPCFPLTCount( const int val) {MPCFPLTCount   = val; return;}
   int GetMPCFPLT1Count() {return MPCFPLT1Count; }
   void    SetMPCFPLT1Count( const int val) {MPCFPLT1Count   = val; return;}
   int GetMPCFPLT2Count() {return MPCFPLT2Count; }
   void    SetMPCFPLT2Count( const int val) {MPCFPLT2Count   = val; return;}

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
       int   isubevent; 
       int   nfile;

   static int       MPCFPLTCount;    // Class global giving number of instances
   //
   static int       MPCFPLT1Count;    // Class global giving number of instances for MPCFPLT1
   static int       MPCFPLT2Count;    // Class global giving number of instances for MPCFPLT2

  ClassDef(MPCFPLTSnglPisaHitv1,1)
};
#endif
