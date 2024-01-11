//INCLUDECHECKER: Removed this line: #include <iostream>
#include "ZdcOutv1.h"
#include "ZdcHit.h"
#include "ZdcNorthSouth.h"

ClassImp(ZdcOutv1)

ZdcOutv1::ZdcOutv1()
{
  ZdcOutNpmt    = 0;
  ZdcVertex       = INVALID_FLOAT;
  ZdcVertexError  = INVALID_FLOAT;
  ZdcTimeZero      = INVALID_FLOAT;
  ZdcTimeZeroError = INVALID_FLOAT;
  // ZdcHit is class for  hits (members: charge,time0,time1), do not mix
  // with TClonesArray *ZdcHits
  ZdcHits = new TClonesArray("ZdcHit",NPMTZDCV1);
  ZdcNS = new TClonesArray("ZdcNorthSouth",NZDC);
  return;
}

ZdcOutv1::~ZdcOutv1()
{  
  Clear();
  delete ZdcHits;
  delete ZdcNS;
  return;
}

void ZdcOutv1::Clear(Option_t *option)
{  
  ZdcHits->Clear();
  ZdcNS->Clear();
  ZdcOutNpmt = 0;
  return;
}

void ZdcOutv1::Reset()
{
  ZdcOutNpmt       = 0;
  ZdcVertex        = INVALID_FLOAT;
  ZdcVertexError   = INVALID_FLOAT;
  ZdcTimeZero      = INVALID_FLOAT;
  ZdcTimeZeroError = INVALID_FLOAT;
  Clear();
  return;
}

void ZdcOutv1::identify(std::ostream& out) const
{
  out << "identify yourself: I am a ZdcOutv1 object" << std::endl;
  return;
}

short ZdcOutv1::set_TimeVertex(float t0, float t0err, float vtx, float vtxerr)
{
  ZdcTimeZero = t0;
  ZdcTimeZeroError = t0err;
  ZdcVertex = vtx;
  ZdcVertexError = vtxerr;
  return 0;
}

short ZdcOutv1::AddZdcHit(float charge, float time0, float time1, short ipmt)
{
  TClonesArray &zdchits = *ZdcHits;
  new(zdchits[ipmt]) ZdcHit(charge,time0,time1);
  ZdcOutNpmt = ipmt;
  return 0;
}

float ZdcOutv1::get_Charge(short iPmt) const
{

  ZdcHit *zdchit = (ZdcHit*) GetZdcHits()->UncheckedAt(iPmt);
  //  if zdchit=nil (does not exist) return INVALID_FLOAT, else Charge
   return((zdchit) ? zdchit->get_Charge() : INVALID_FLOAT);
}

float ZdcOutv1::get_Time0(short iPmt) const
{
  ZdcHit *zdchit = (ZdcHit*) GetZdcHits()->UncheckedAt(iPmt);
  //  if zdchit=nil (does not exist) return INVALID_FLOAT, else Time0
   return((zdchit) ? zdchit->get_Time0() : INVALID_FLOAT);
}

float ZdcOutv1::get_Time1(short iPmt) const
{
  ZdcHit *zdchit = (ZdcHit*) GetZdcHits()->UncheckedAt(iPmt);
  //  if zdchit=nil (does not exist) return INVALID_FLOAT, else Time1
   return((zdchit) ? zdchit->get_Time1() : INVALID_FLOAT);
}


void ZdcOutv1::AddZdcNS(float energy, float timing, short nzdc)
{
  TClonesArray &zdcnoso = *ZdcNS;
  new(zdcnoso[nzdc]) ZdcNorthSouth(energy,timing);
  return;
}

float ZdcOutv1::get_Energy(short nzdc) const
{
  ZdcNorthSouth *zdcnoso = (ZdcNorthSouth*) GetZdcNS()->UncheckedAt(nzdc);
  //  if zdcnoso=nil (does not exist) return INVALID_FLOAT, else Energy
   return((zdcnoso) ? zdcnoso->get_Energy() : INVALID_FLOAT);
}

float ZdcOutv1::get_Timing(short nzdc) const
{
  ZdcNorthSouth *zdcnoso = (ZdcNorthSouth*) GetZdcNS()->UncheckedAt(nzdc);
  //  if zdcnoso=nil (does not exist) return INVALID_FLOAT, else Timing
   return((zdcnoso) ? zdcnoso->get_Timing() : INVALID_FLOAT);
}

int ZdcOutv1::isValid() const
{
    return((ZdcTimeZero>-999) ? 1 : 0);
}
