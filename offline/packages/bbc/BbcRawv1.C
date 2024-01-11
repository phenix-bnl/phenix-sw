#include "BbcRawv1.h"
#include "BbcRawHit.h"
#include "BbcReturncodes.h"
#include "TClonesArray.h"
#include <iostream>

using namespace std;

static const int NPMTBBCV1 = 128;

ClassImp(BbcRawv1)

BbcRawv1::BbcRawv1()
{
  // BbcRawHit is class for raw hits (members: pmt,adc,tdc0,tdc1), do not mix
  // with TClonesArray *BbcRawHits
  BbcRawHits = new TClonesArray("BbcRawHit", NPMTBBCV1);
  npmt = 0;
}

BbcRawv1::~BbcRawv1()
{
  if (BbcRawHits)
    {
      Clear();
      delete BbcRawHits;
    }
}

int
BbcRawv1::isValid() const
{
  if (npmt <= 0)
    {
      return 0;
    }
  return 1;
}

void
BbcRawv1::Reset()
{
  Clear();
  npmt = 0;
}

void
BbcRawv1::Clear(Option_t *option)
{
  BbcRawHits->Clear();
}

void
BbcRawv1::AddBbcRawHit(const short pmt, const short adc,
                       const short tdc0, const short tdc1,
                       const short ipmt)
{
  TClonesArray &Bbchits = *BbcRawHits;
  new(Bbchits[ipmt]) BbcRawHit(pmt, adc, tdc0, tdc1);
}

short
BbcRawv1::get_Adc(const short iPmt) const
{
  BbcRawHit *Bbchit = (BbcRawHit*) GetBbcRawHits()->UncheckedAt(iPmt);
  //  if Bbchit=nil (does not exist) return BBC_INVALID_SHORT, else Adc
  return ((Bbchit) ? Bbchit->get_Adc() : BBC_INVALID_SHORT);
}

short
BbcRawv1::get_Pmt(const short iPmt) const
{
  BbcRawHit *Bbchit = (BbcRawHit*) GetBbcRawHits()->UncheckedAt(iPmt);
  //  if Bbchit=nil (does not exist) return BBC_INVALID_SHORT, else Pmt
  return ((Bbchit) ? Bbchit->get_Pmt() : BBC_INVALID_SHORT);
}

short
BbcRawv1::get_Tdc0(const short iPmt) const
{
  BbcRawHit *Bbchit = (BbcRawHit*) GetBbcRawHits()->UncheckedAt(iPmt);
  //  if Bbchit=nil (does not exist) return BBC_INVALID_SHORT, else Tdc0
  return ((Bbchit) ? Bbchit->get_Tdc0() : BBC_INVALID_SHORT);
}

short
BbcRawv1::get_Tdc1(const short iPmt) const
{
  BbcRawHit *Bbchit = (BbcRawHit*) GetBbcRawHits()->UncheckedAt(iPmt);
  //  if Bbchit=nil (does not exist) return BBC_INVALID_SHORT, else Tdc1
  return ((Bbchit) ? Bbchit->get_Tdc1() : BBC_INVALID_SHORT);
}

void
BbcRawv1::identify(ostream& out) const
{
  out << "identify yourself: I am a BbcRawv1 object" << endl;
}
