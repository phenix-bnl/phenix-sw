#ifndef __MPCEXENTSNGLHITV1_H_
#define __MPCEXENTSNGLHITV1_H_

#include "PHObject.h"
#include "MPCEXENTSnglPisaHit.h"

class MPCEXENTSnglPisaHitv1 : public MPCEXENTSnglPisaHit
{
 public:
  MPCEXENTSnglPisaHitv1();
  MPCEXENTSnglPisaHitv1(MPCEXENTSnglPisaHitv1 *track);  
  virtual ~MPCEXENTSnglPisaHitv1() {}
   int GetMPCEXENTCount() {return MPCEXENTCount; }
   void    SetMPCEXENTCount( const int val) {MPCEXENTCount   = val; return;}

   int         GetMctrack() const { return mctrack; }
   void    SetMctrack(const int val) {mctrack   = val; return;}

   float       GetVx() const {return vx;}
   void  SetVx(const float val) {vx = val; return;}
   float       GetVy() const {return vy;}
   void  SetVy(const float val) {vy = val; return;}
   float       GetVz() const {return vz;}
   void  SetVz(const float val) {vz = val; return;}

   float       GetPx() const {return px;}
   void  SetPx(const float val) {px = val; return;}
   float       GetPy() const {return py;}
   void  SetPy(const float val) {py = val; return;}
   float       GetPz() const {return pz;}
   void  SetPz(const float val) {pz = val; return;}

   int         GetIsubevent() const { return isubevent; }
   void    SetIsubevent(const int val) {isubevent   = val; return;}
   int         GetNfile() const { return nfile; }
   void    SetNfile(const int val) {nfile   = val; return;}
   int         GetNtrack() const { return ntrack; }
   void    SetNtrack(const int val) {ntrack   = val; return;}


 protected:

       int   mctrack;
       float vx; 
       float vy; 
       float vz; 
       float px; 
       float py; 
       float pz; 

       int isubevent; 
       int nfile;
       int ntrack; 

   static int       MPCEXENTCount;    // Class global giving number of instances

  ClassDef(MPCEXENTSnglPisaHitv1,1)
};
#endif
