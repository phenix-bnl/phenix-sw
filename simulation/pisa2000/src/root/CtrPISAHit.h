#ifndef _CtrPISAHit_
#define _CtrPISAHit_

//////////////////////////////////////////////////////////////////////////
//      T. K. Ghosh, vanderbilt, 08. 03 .99                                                                 //
//////////////////////////////////////////////////////////////////////////

#include "TObject.h"
#include "TClonesArray.h"
#include "TH1.h"
#include "TMath.h"




class CtrPISAHit : public TObject {

private:

      Int_t detector ;
      Float_t pos[3]  ;
      Short_t pid;
      Int_t itra ;
      Float_t momv[3];
      Float_t v[3]   ;
      Int_t mctrack;
      Int_t nfile;
      Int_t isubevent;

   static CtrPISAHit *CtrHitEvt;   // Class global giving start pointer address
   static Int_t       CtrCount;    // Class global giving number of instance

public:
   CtrPISAHit() { }  // Default constructor (needed for arrays of objects)

   CtrPISAHit( Int_t detector, Float_t x, Float_t y, Float_t z, Short_t pid, 
        Int_t itra, Float_t pvx, Float_t pvy, Float_t pvz, Float_t vx, Float_t vy, 
        Float_t vz, Int_t mctrack, Int_t nfile, Int_t isubevent);
                 // main constructor

   virtual ~CtrPISAHit() { }

   static Int_t GetCtrCount() {return CtrCount; }
   static void  SetCtrCount(Int_t count) {CtrCount = count; }
   static CtrPISAHit *GetCtrHitEvt() {return CtrHitEvt; }
   static void        SetCtrHitEvt(CtrPISAHit *ptr) {CtrHitEvt = ptr; }

   static void CtrClear(){ delete [] CtrHitEvt; CtrHitEvt = 0; }


   Int_t       GetDetector() const { return detector; }
   Float_t       GetX() const { return pos[0]; }
   Float_t       GetY() const { return pos[1]; }
   Float_t       GetZ() const { return pos[2]; }
   Short_t       GetPid() const { return pid; }
   Int_t       GetNtrack() const { return itra; }
   Float_t       GetPvx() const { return momv[0]; }
   Float_t       GetPvy() const { return momv[1]; }
   Float_t       GetPvz() const { return momv[2]; }
   Float_t       GetVx() const { return v[0]; }
   Float_t       GetVy() const { return v[1]; }
   Float_t       GetVz() const { return v[2]; }
   Int_t         GetMctrack() const { return mctrack; }
   void          SetMctrack(const Int_t ntrack) { mctrack =  itra  ; }
   Int_t         GetIsubevent() const { return isubevent; }
   Int_t         GetNfile() const { return nfile; }
   void          SetNfile(const Int_t file) { nfile = file; }

   ClassDef(CtrPISAHit,1)  // A Tof hit instance
};


#endif





















