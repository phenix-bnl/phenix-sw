#include "ZdcRawv1.h"
#include "ZdcRawHit.h"
#include <iostream>

using namespace std;

ClassImp(ZdcRawv1)

static const int NPMTZDCV1 = 40;

ZdcRawv1::ZdcRawv1()
{
  // ZdcRawHit is class for raw hits (members: adc,tdc0,tdc1), do not mix
  // with TClonesArray *ZdcRawHits
  ZdcRawHits = new TClonesArray("ZdcRawHit",NPMTZDCV1);
  ZdcRawNpmt = 0;
  return;
}

ZdcRawv1::~ZdcRawv1()
{  
  Clear();
  delete ZdcRawHits;
  return;
}

void ZdcRawv1::Reset()
{
  Clear();
  ZdcRawNpmt = 0;
  return;
}

void ZdcRawv1::identify(ostream& out) const
{
  out << "identify yourself: I am a ZdcRawv1 object" << endl;
  return;
}

int ZdcRawv1::isValid() const
{
  if (ZdcRawHits->GetEntries() ) 
    {
      return 1;	// valid
    }
  return 0; // not valid
}

void ZdcRawv1::Clear(Option_t *option)
{  
  ZdcRawHits->Clear();
  return;
}

short ZdcRawv1::AddZdcRawHit(short adc, short tdc0 ,short tdc1, short ipmt)
{
  TClonesArray &zdchits = *ZdcRawHits;
  new(zdchits[ipmt]) ZdcRawHit(adc,tdc0,tdc1);
  ZdcRawNpmt = ZdcRawHits->GetLast()+1;
  return 0;
}

short ZdcRawv1::get_Adc(short iPmt) const
{
  ZdcRawHit *zdchit = (ZdcRawHit*) GetZdcRawHits()->UncheckedAt(iPmt);
  //  if zdchit=nil (does not exist) return INVALID_SHORT, else Adc
   return((zdchit) ? zdchit->get_Adc() : INVALID_SHORT);
}

short ZdcRawv1::get_Tdc0(short iPmt) const
{
  ZdcRawHit *zdchit = (ZdcRawHit*) GetZdcRawHits()->UncheckedAt(iPmt);
  //  if zdchit=nil (does not exist) return INVALID_SHORT, else Tdc0
   return((zdchit) ? zdchit->get_Tdc0() : INVALID_SHORT);
}

short ZdcRawv1::get_Tdc1(short iPmt) const
{
  ZdcRawHit *zdchit = (ZdcRawHit*) GetZdcRawHits()->UncheckedAt(iPmt);
  //  if zdchit=nil (does not exist) return INVALID_SHORT, else Tdc1
   return((zdchit) ? zdchit->get_Tdc1() : INVALID_SHORT);
}

short ZdcRawv1::get_Tdc(short iPmt, unsigned short itdc) const
{
  if (itdc==0) return get_Tdc0( iPmt );
  if (itdc==1) return get_Tdc1( iPmt );
  return INVALID_SHORT;
}
