#ifndef __NCCSNGLHITV1_H_
#define __NCCSNGLHITV1_H_

#include "PHObject.h"
#include "NCCSnglPisaHit.h"

class NCCSnglPisaHitv1 : public NCCSnglPisaHit
{
 public:
  NCCSnglPisaHitv1();
  NCCSnglPisaHitv1(NCCSnglPisaHitv1 *track);  
  virtual ~NCCSnglPisaHitv1() {}
   int GetNCCCount() {return NCCCount; }
   void    SetNCCCount( const int val) {NCCCount   = val; return;}
   int GetNCC1Count() {return NCC1Count; }
   void    SetNCC1Count( const int val) {NCC1Count   = val; return;}
   int GetNCC2Count() {return NCC2Count; }
   void    SetNCC2Count( const int val) {NCC2Count   = val; return;}
   int         GetIarm() const { return arm; }
   void    SetIarm(const int val) {arm   = val; return;}
   int         GetIsubevent() const { return isubevent; }
   void    SetIsubevent(const int val) {isubevent   = val; return;}
   int         GetNtrack() const { return track; }
   void    SetNtrack(const int val) {track   = val; return;}
   int         GetId() const { return id; }
   void    SetId(const int val) {id   = val; return;}
   int         GetMctrack() const { return mctrack; }
   void    SetMctrack(const int val) {mctrack   = val; return;}
   int         GetIpc() const { return ipc; }
   void    SetIpc(const int val) {ipc   = val; return;}
   float       GetXin() const { return xx; }
   void    SetXin(const float val) {xx   = val; return;}
   float       GetYin() const { return yy; }
   void    SetYin(const float val) {yy   = val; return;}
   float       GetZin() const { return zz; }
   void    SetZin(const float val) {zz   = val; return;}
   float       GetDedx() const { return dedx; }
   void    SetDedx(const float val) {dedx   = val; return;}
   float       GetXe() const { return Xe; }
   void    SetXe(const float val) {Xe   = val; return;}
   float       GetYe() const { return Ye; }
   void    SetYe(const float val) {Ye   = val; return;}
   float       GetPmom() const { return Pmom; }
   void    SetPmom(const float val) {Pmom   = val; return;}
   float       GetP_id() const { return P_id; }
   void    SetP_id(const float val) {P_id   = val; return;}
   float       GetPNum() const { return PNum; }
   void    SetPNum(const float val) {PNum   = val; return;}
   //   float       GetPathLength() const { return pathLength; }
   //   void    SetPathLength(const float val) {pathLength   = val; return;}
   int         GetNfile() const { return nfile; }
   void    SetNfile(const int val) {nfile   = val; return;}

 protected:

       int   id;
       int   mctrack;
       float xx;
       float yy;
       float zz ;
       float dedx; 
       float Xe;
       float Ye;
       float Pmom;
       float P_id;
       float PNum;
       //       float pathLength;
       int   track;   // track in subevent
       int   arm;
       int   ipc;     // which of NCC1, NCC2, or NCC3 (*not* counting from 0!!)
       int   isubevent; 
       int   nfile;

   static int       NCCCount;    // Class global giving number of instances
   //
   static int       NCC1Count;    // Class global giving number of instances for NCC1
   static int       NCC2Count;    // Class global giving number of instances for NCC2

  ClassDef(NCCSnglPisaHitv1,1)
};
#endif
