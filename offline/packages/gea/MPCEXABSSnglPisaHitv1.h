#ifndef __MPCEXABSSNGLHITV1_H_
#define __MPCEXABSSNGLHITV1_H_

#include "PHObject.h"
#include "MPCEXABSSnglPisaHit.h"

class MPCEXABSSnglPisaHitv1 : public MPCEXABSSnglPisaHit
{
 public:
  MPCEXABSSnglPisaHitv1();
  MPCEXABSSnglPisaHitv1(MPCEXABSSnglPisaHitv1 *track);  
  virtual ~MPCEXABSSnglPisaHitv1() {}
   int GetMPCEXABSCount() {return MPCEXABSCount; }
   void    SetMPCEXABSCount( const int val) {MPCEXABSCount   = val; return;}
   int GetMPCEXABS1Count() {return MPCEXABS1Count; }
   void    SetMPCEXABS1Count( const int val) {MPCEXABS1Count   = val; return;}
   int GetMPCEXABS2Count() {return MPCEXABS2Count; }
   void    SetMPCEXABS2Count( const int val) {MPCEXABS2Count   = val; return;}

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

   static int       MPCEXABSCount;    // Class global giving number of instances
   //
   static int       MPCEXABS1Count;    // Class global giving number of instances for MPCEXABS1
   static int       MPCEXABS2Count;    // Class global giving number of instances for MPCEXABS2

  ClassDef(MPCEXABSSnglPisaHitv1,1)
};
#endif
