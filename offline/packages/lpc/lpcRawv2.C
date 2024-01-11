#include "lpcRawv2.h"
//INCLUDECHECKER: Removed this line: #include "lpcRawHit.h"
#include "lpcRawHitv2.h"
#include <iostream>
#include "TClonesArray.h"

using namespace std;

ClassImp(lpcRawv2)

static const int NPMTLPCV2 = 4;

lpcRawv2::lpcRawv2()
{
  // lpcRawHit is class for raw hits (members: adc,tdc0), do not mix
  // with TClonesArray *lpcRawHits
  lpcRawHits = new TClonesArray("lpcRawHitv2",NPMTLPCV2);
  lpcRawNpmt = 0;
  return;
}

lpcRawv2::~lpcRawv2()
{  
  Clear();
  delete lpcRawHits;
  return;
}

void lpcRawv2::Reset()
{
  Clear();
  lpcRawNpmt = 0;
  return;
}

void lpcRawv2::identify(ostream& out) const
{
  out << "identify yourself: I am a lpcRawv2 object" << endl;
  return;
}

int lpcRawv2::isValid() const
{
  if (lpcRawHits->GetEntries() ) 
    {
      return 1;	// valid
    }
  return 0; // not valid
}

void lpcRawv2::Clear(Option_t *option)
{  
  lpcRawHits->Clear();
  return;
}

short lpcRawv2::AddlpcRawHit(short adc_post, short adc_pre, short tdc0 , short ipmt)
{
  TClonesArray &lpchits = *lpcRawHits;
  new(lpchits[ipmt]) lpcRawHitv2(adc_post,adc_pre,tdc0);
  lpcRawNpmt = lpcRawHits->GetLast()+1;
  return 0;
}

short lpcRawv2::get_AdcPost(short iPmt) const
{
  lpcRawHit *lpchit = (lpcRawHit*) GetlpcRawHits()->UncheckedAt(iPmt);
  //  if lpchit=nil (does not exist) return INVALID_SHORT, else Adc
   return((lpchit) ? lpchit->get_AdcPost() : INVALID_SHORT);
}

short lpcRawv2::get_AdcPre(short iPmt) const
{
  lpcRawHit *lpchit = (lpcRawHit*) GetlpcRawHits()->UncheckedAt(iPmt);
  //  if lpchit=nil (does not exist) return INVALID_SHORT, else Adc
   return((lpchit) ? lpchit->get_AdcPre() : INVALID_SHORT);
}

short lpcRawv2::get_Tdc0(short iPmt) const
{
  lpcRawHit *lpchit = (lpcRawHit*) GetlpcRawHits()->UncheckedAt(iPmt);
  //  if lpchit=nil (does not exist) return INVALID_SHORT, else Tdc0
   return((lpchit) ? lpchit->get_Tdc0() : INVALID_SHORT);
}

short lpcRawv2::get_Tdc(short iPmt, unsigned short itdc) const
{
  if (itdc==0) return get_Tdc0( iPmt );
  return INVALID_SHORT;
}
