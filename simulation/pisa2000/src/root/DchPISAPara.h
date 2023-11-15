#ifndef _DCHPISAPARA_
#define _DCHPISAPARA_

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Description of the DCH parameters (event independent)                //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TObject.h"
#include "TClonesArray.h"


class DchPISAPara : public TObject {
private:
  Int_t ncells;    /* number of cells per arm */
  Int_t ngusset;    /* number of gussets per arm */
  Int_t ti_switch;    /* set to 0 if Al is used in support */
  Float_t suppzlength;    /* z distance between the arms */
  Float_t inradius;    /* inner radius of gas volume */
  Float_t outradius;    /* outer radius of gas volume */
  Float_t phibotw;    /* phi at bottom of west arm */
  Float_t phitopw;    /* phi at top of west arm */
  Float_t phitope;    /* phi at top of east arm */
  Float_t phibote;    /* phi at bottom of east arm */
  Float_t rplane[42];    /* radius of each DC plane at z=0 */
  Float_t planethick;    /* radial thickness of sensitive regions */
  Float_t uvangle;    /* angle of UV wires with X wires in degrees */
  Float_t winthickin;    /* thickness of the inner mylar window cover */
  Float_t winthickout;    /* thickness of the outer mylar window cover */
  Float_t supptiside;    /* thickness of the side of the C-section */
  Float_t suppalside;    /* thickness of the side of the C-section */
  Float_t suppzthick;    /* height of the side of the C-section */
  Float_t supptibase;    /* thickness of the base of the C-section */
  Float_t suppalbase;    /* thickness of the base of the C-section */
  Float_t x1baserad;    /* radial thickness of X1 support base */
  Float_t x2baserad;    /* radial thickness of X2 support base */
  Float_t x1basez;    /* height of X1 support base */
  Float_t x2basez;    /* height of X2 support base */
  Float_t x1slotthick;    /* thickness of X1 Al slots */
  Float_t x2slotthick;    /* thickness of X2 Al slots */
  Float_t x1slotz;    /* height of X1 Al slots */
  Float_t x2slotz;    /* height of X2 Al slots */
  Float_t x1suppthick;    /* thickness of X1 G10 support cards */
  Float_t x2suppthick;    /* thickness of X2 G10 support cards */
  Float_t x1suppz;    /* height of X1 G10 support cards */
  Float_t x2suppz;    /* height of X2 G10 support cards */
  Float_t x1rextent;    /* radial extension of X1 support */
  Float_t x2rextent;    /* radial extension of X2 support */
  Float_t u1rextent;    /* radial extension of U1 support */
  Float_t v1rextent;    /* radial extension of V1 support */
  Float_t u2rextent;    /* radial extension of U2 support */
  Float_t v2rextent;    /* radial extension of V2 support */
  Float_t u1basez;    /* height of U1 base plate */
  Float_t v1basez;    /* height of V1 base plate */
  Float_t u2basez;    /* height of U2 base plate */
  Float_t v2basez;    /* height of V2 base plate */
  Float_t u1slotz;    /* height of U1 Al slots */
  Float_t v1slotz;    /* height of V1 Al slots */
  Float_t u2slotz;    /* height of U2 Al slots */
  Float_t v2slotz;    /* height of V2 Al slots */
  Float_t u1suppz;    /* height of U1 G10 support cards */
  Float_t v1suppz;    /* height of V1 G10 support cards */
  Float_t u2suppz;    /* height of U2 G10 support cards */
  Float_t v2suppz;    /* height of V2 G10 support cards */
  Float_t cfibinrad;    /* inner radius of C fiber support */
  Float_t cfiboutrad;    /* outer radius of C fiber support */

  Int_t   idateDC;     /* date of last Dch geometry change */
  Int_t   idatePC1;    /* date of PC1 Dch geometry change */

public:
   DchPISAPara() { }  // Default constructor (needed for arrays of objects)
   DchPISAPara(const Int_t i[], const Float_t f[]);  // main constructor
   void SetDateDch(const Int_t argidateDC) { idateDC = argidateDC; } 
   void SetDatePC1(const Int_t argidatePC1) { idatePC1 = argidatePC1; } 
   Int_t GetDateDch() { return idateDC; }
   Int_t GetDatePC1() { return idatePC1; }
   virtual ~DchPISAPara() { }

   ClassDef(DchPISAPara,1)  // A DCH parameter instance
};

#endif

