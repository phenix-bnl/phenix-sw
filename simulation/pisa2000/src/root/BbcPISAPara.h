#ifndef _BBCPISAPARA_
#define _BBCPISAPARA_

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//                                                                      //
// Description of the BBC parameters (event independent)                //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TObject.h"
#include "TClonesArray.h"


class BbcPISAPara : public TObject {

private:
   Int_t color;
   Int_t seen;
   Int_t medabs;
   Int_t medatt;
   Int_t medbac;
   Int_t medcov;
   Int_t medfro;
   Int_t medmot;
   Int_t medpmt;
   Int_t medqua;
   Int_t medstr;

   Float_t absorb[3];

   //
   // In STAF DIO Dave Morrison has started the filling at the backbd[1] position
   // The backbd[0] is not defined
   // This can work if the BBC code in STAF does not use backbd[0]
   // Same is true for some of the other parameter arrays here
   //
   Float_t backbd[4];  // need to check this indexing?

   Float_t covert;
   Float_t frontb[4];  // need to check this indexing?
   Float_t pmtsiz[4];  // need to check this indexing?

   Float_t quartz[10];

   Float_t spacin;
   Float_t struc[4];   // need to check this indexing?
   Float_t zposit[3];  // need to check this indexing?

public:
   BbcPISAPara() { }  // Default constructor (needed for arrays of objects)
   BbcPISAPara(const Int_t i[], const Float_t f[]);  // Main constructor

   virtual ~BbcPISAPara() { }

   ClassDef(BbcPISAPara,1)  // A BBC parameter instance
};

#endif

