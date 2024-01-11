//INCLUDECHECKER: Removed this line: #include <iostream>
//INCLUDECHECKER: Removed this line: #include "TClass.h"
#include "ZdcOutv2.h"
#include "ZdcHit.h"
#include "ZdcNorthSouth.h"

ClassImp(ZdcOutv2)

ZdcOutv2::ZdcOutv2()
{
  ZdcOutNpmt    = 0;
  ZdcVertex       = INVALID_FLOAT;
  ZdcVertexError  = INVALID_FLOAT;
  ZdcTimeZero      = INVALID_FLOAT;
  ZdcTimeZeroError = INVALID_FLOAT;
  // ZdcHit is class for  hits (members: charge,time0,time1), do not mix
  // with TClonesArray *ZdcHits
  ZdcHits = new TClonesArray("ZdcHit",NPMTZDCV2);
  ZdcNS = new TClonesArray("ZdcNorthSouth",NZDC);
  return;
}

ZdcOutv2::~ZdcOutv2()
{
  if (ZdcHits)
    {
      ZdcHits->Clear();
      delete ZdcHits;
    }
  if (ZdcNS)
    {
      ZdcNS->Clear();
      delete ZdcNS;
    }
  return;
}

void ZdcOutv2::Clear(Option_t *option)
{  
  ZdcHits->Clear();
  ZdcNS->Clear();
  ZdcOutNpmt = 0;
  return;
}

void ZdcOutv2::Reset()
{
  ZdcOutNpmt       = 0;
  ZdcVertex        = INVALID_FLOAT;
  ZdcVertexError   = INVALID_FLOAT;
  ZdcTimeZero      = INVALID_FLOAT;
  ZdcTimeZeroError = INVALID_FLOAT;
  Clear();
  return;
}

void ZdcOutv2::identify(std::ostream& out) const
{
  out << "identify yourself: I am a ZdcOutv2 object" << std::endl;
  out << "ZdcVtx: " << ZdcVertex << ", Error: " << ZdcVertexError << std::endl;
  out << "ZdcT0: " <<  ZdcTimeZero << ", Error: " <<  ZdcTimeZeroError 
      << std::endl;
  return;
}

short ZdcOutv2::set_TimeVertex(float t0, float t0err, float vtx, float vtxerr)
{
  ZdcTimeZero = t0;
  ZdcTimeZeroError = t0err;
  ZdcVertex = vtx;
  ZdcVertexError = vtxerr;
  return 0;
}

short ZdcOutv2::AddZdcHit(float charge, float time0, float time1, short ipmt)
{
  TClonesArray &zdchits = *ZdcHits;
  new(zdchits[ipmt]) ZdcHit(charge,time0,time1);
  ZdcOutNpmt = ipmt;
  return 0;
}

float ZdcOutv2::get_Charge(short iPmt) const
{

  ZdcHit *zdchit = (ZdcHit*) GetZdcHits()->UncheckedAt(iPmt);
  //  if zdchit=nil (does not exist) return INVALID_FLOAT, else Charge
   return((zdchit) ? zdchit->get_Charge() : INVALID_FLOAT);
}

float ZdcOutv2::get_Time0(short iPmt) const
{
  ZdcHit *zdchit = (ZdcHit*) GetZdcHits()->UncheckedAt(iPmt);
  //  if zdchit=nil (does not exist) return INVALID_FLOAT, else Time0
   return((zdchit) ? zdchit->get_Time0() : INVALID_FLOAT);
}

float ZdcOutv2::get_Time1(short iPmt) const
{
  ZdcHit *zdchit = (ZdcHit*) GetZdcHits()->UncheckedAt(iPmt);
  //  if zdchit=nil (does not exist) return INVALID_FLOAT, else Time1
   return((zdchit) ? zdchit->get_Time1() : INVALID_FLOAT);
}


void ZdcOutv2::AddZdcNS(float energy, float timing, short nzdc)
{
  TClonesArray &zdcnoso = *ZdcNS;
  new(zdcnoso[nzdc]) ZdcNorthSouth(energy,timing);
  return;
}

float ZdcOutv2::get_Energy(short nzdc) const
{
  ZdcNorthSouth *zdcnoso = (ZdcNorthSouth*) GetZdcNS()->UncheckedAt(nzdc);
  //  if zdcnoso=nil (does not exist) return INVALID_FLOAT, else Energy
   return((zdcnoso) ? zdcnoso->get_Energy() : INVALID_FLOAT);
}

float ZdcOutv2::get_Timing(short nzdc) const
{
  ZdcNorthSouth *zdcnoso = (ZdcNorthSouth*) GetZdcNS()->UncheckedAt(nzdc);
  //  if zdcnoso=nil (does not exist) return INVALID_FLOAT, else Timing
   return((zdcnoso) ? zdcnoso->get_Timing() : INVALID_FLOAT);
}

int ZdcOutv2::isValid() const
{
    return((ZdcTimeZero>-999) ? 1 : 0);
}

