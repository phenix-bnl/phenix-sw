#ifndef _ZdcPISAHit_
#define _ZdcPISAHit_

#include "TObject.h"
#include "TClonesArray.h"
#include "TH1.h"
#include "TMath.h"

class ZdcPISAHit : public TObject 
{
private:
    Float_t pos_m[3];
    Float_t dele;
    Float_t p_m[3];
    Float_t tof;
    Int_t   partl;
    Int_t   north_south;   
    Int_t   module;
    Int_t   track;
    Int_t   isubevent;
    Int_t   mctrack;
    Int_t   nfile;
   
   static ZdcPISAHit *ZdcHitEvt;   // Class global giving start pointer address
   static Int_t       ZdcCount;    // Class global giving number of instance 

public:
   ZdcPISAHit() { }  // Default constructor (needed for arrays of objects)

   ZdcPISAHit(Float_t xm,
              Float_t ym,
              Float_t zm,
              Float_t pxm,
              Float_t pym,
              Float_t pzm,
              Float_t dele,
              Float_t tof, 
              Int_t   pid,
              Int_t   dir,
              Int_t   mod,
              Int_t   track,
              Int_t   isubevent,
	      Int_t   mctrack,
              Int_t   nfile);  // main constructor
   virtual ~ZdcPISAHit() { }

   static Int_t GetZdcCount() {return ZdcCount; }
   static void  SetZdcCount(Int_t count) {ZdcCount = count; }
   static ZdcPISAHit *GetZdcHitEvt() {return ZdcHitEvt; }
   static void        SetZdcHitEvt(ZdcPISAHit *ptr) {ZdcHitEvt = ptr; }

   static void ZdcClear(){ delete [] ZdcHitEvt; ZdcHitEvt = 0; }

   Float_t       GetXm()        const { return pos_m[0]; }
   Float_t       GetYm()        const { return pos_m[1]; }
   Float_t       GetZm()        const { return pos_m[2]; }
   Float_t       GetDele()      const { return dele; }
   Float_t       GetPxm()       const { return p_m[0]; }
   Float_t       GetPym()       const { return p_m[1]; }
   Float_t       GetPzm()       const { return p_m[2]; }
   Float_t       GetTof()       const { return tof; }
   Int_t         GetPartl()     const { return partl; }
   Int_t         GetDirection() const { return north_south; }
   Int_t         GetNmodule()   const { return module; }
   Int_t         GetNtrack()    const { return track; }
   void          SetMctrack(const Int_t itrack) { mctrack = itrack; }
   Int_t         GetMctrack()   const { return mctrack; }
   Int_t         GetIsubevent() const { return isubevent; }
   Int_t         GetNfile()     const { return nfile; }
   void          SetNfile(const Int_t file) { nfile = file; }

   ClassDef(ZdcPISAHit,1)  // A Zdc hit instance

};

#endif


