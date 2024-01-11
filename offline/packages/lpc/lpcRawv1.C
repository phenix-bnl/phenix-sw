#include "lpcRawv1.h"
//INCLUDECHECKER: Removed this line: #include "lpcRawHit.h"
#include "lpcRawHitv1.h"
#include <iostream>
#include "TClonesArray.h"

using namespace std;

ClassImp(lpcRawv1)

static const int NPMTLPCV1 = 4;

lpcRawv1::lpcRawv1()
{
  // lpcRawHit is class for raw hits (members: adc,tdc0), do not mix
  // with TClonesArray *lpcRawHits
  lpcRawHits = new TClonesArray("lpcRawHitv1",NPMTLPCV1);
  lpcRawNpmt = 0;
  return;
}

lpcRawv1::~lpcRawv1()
{  
  Clear();
  return;
}

void lpcRawv1::Reset()
{
  Clear();
  lpcRawNpmt = 0;
  return;
}

void lpcRawv1::identify(ostream& out) const
{
  out << "identify yourself: I am a lpcRawv1 object" << endl;
  return;
}

int lpcRawv1::isValid() const
{
  if (lpcRawHits->GetEntries() ) 
    {
      return 1;	// valid
    }
  return 0; // not valid
}

void lpcRawv1::Clear(Option_t *option)
{  
  lpcRawHits->Clear();
  return;
}

short lpcRawv1::AddlpcRawHit(short adc, short tdc0 , short ipmt)
{
  TClonesArray &lpchits = *lpcRawHits;
  new(lpchits[ipmt]) lpcRawHitv1(adc,tdc0);
  lpcRawNpmt = lpcRawHits->GetLast()+1;
  return 0;
}

short lpcRawv1::get_Adc(short iPmt) const
{
  lpcRawHit *lpchit = (lpcRawHit*) GetlpcRawHits()->UncheckedAt(iPmt);
  //  if lpchit=nil (does not exist) return INVALID_SHORT, else Adc
   return((lpchit) ? lpchit->get_Adc() : INVALID_SHORT);
}

short lpcRawv1::get_Tdc0(short iPmt) const
{
  lpcRawHit *lpchit = (lpcRawHit*) GetlpcRawHits()->UncheckedAt(iPmt);
  //  if lpchit=nil (does not exist) return INVALID_SHORT, else Tdc0
   return((lpchit) ? lpchit->get_Tdc0() : INVALID_SHORT);
}

short lpcRawv1::get_Tdc(short iPmt, unsigned short itdc) const
{
  if (itdc==0) return get_Tdc0( iPmt );
  return INVALID_SHORT;
}
