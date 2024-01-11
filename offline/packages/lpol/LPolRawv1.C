#include "LPolRawv1.h"
//INCLUDECHECKER: Removed this line: #include "LPolRawHit.h"
#include "LPolRawHitv1.h"
#include <iostream>
#include "TClonesArray.h"

using namespace std;

ClassImp(LPolRawv1)

static const int NPMTLPOLV1 = 4;

LPolRawv1::LPolRawv1()
{
  // LPolRawHit is class for raw hits (members: adc,tdc0), do not mix
  // with TClonesArray *LPolRawHits
  lpolRawHits = new TClonesArray("LPolRawHitv1",NPMTLPOLV1);
  lpolRawNpmt = 0;
  return;
}

LPolRawv1::~LPolRawv1()
{  
  Clear();
  return;
}

void LPolRawv1::Reset()
{
  Clear();
  lpolRawNpmt = 0;
  return;
}

void LPolRawv1::identify(ostream& out) const
{
  out << "identify yourself: I am a LPolRawv1 object" << endl;
  return;
}

int LPolRawv1::isValid() const
{
  if (lpolRawHits->GetEntries() ) 
    {
      return 1;	// valid
    }
  return 0; // not valid
}

void LPolRawv1::Clear(Option_t *option)
{  
  lpolRawHits->Clear();
  return;
}

short LPolRawv1::AddLPolRawHit(short adc, short tdc0, short tdc1, short ipmt)
{
  TClonesArray &lpolhits = *lpolRawHits;
  new(lpolhits[ipmt]) LPolRawHitv1(adc,tdc0,tdc1);
  lpolRawNpmt = lpolRawHits->GetLast()+1;
  return 0;
}

short LPolRawv1::get_Adc(short iPmt) const
{
  LPolRawHit *lpolhit = (LPolRawHit*) GetLPolRawHits()->UncheckedAt(iPmt);
  //  if LPolhit=nil (does not exist) return INVALID_SHORT, else Adc
   return((lpolhit) ? lpolhit->get_Adc() : INVALID_SHORT);
}

short LPolRawv1::get_Tdc0(short iPmt) const
{
  LPolRawHit *lpolhit = (LPolRawHit*) GetLPolRawHits()->UncheckedAt(iPmt);
  //  if LPolhit=nil (does not exist) return INVALID_SHORT, else Tdc0
   return((lpolhit) ? lpolhit->get_Tdc0() : INVALID_SHORT);
}

short LPolRawv1::get_Tdc1(short iPmt) const
{
  LPolRawHit *lpolhit = (LPolRawHit*) GetLPolRawHits()->UncheckedAt(iPmt);
  //  if LPolhit=nil (does not exist) return INVALID_SHORT, else Tdc0
   return((lpolhit) ? lpolhit->get_Tdc1() : INVALID_SHORT);
}
