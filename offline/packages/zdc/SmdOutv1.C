#include "SmdOutv1.h"
#include "SmdHit.h"
#include "SmdNorthSouth.h"

#include <iostream>

static const int NSMD = 2;		// 2 arms, 0 = south, 1 = north
static const int NSMDSLATS = 32;	// 2 arms, 0-15 = south, 16-31 = north

ClassImp(SmdOutv1)

SmdOutv1::SmdOutv1()
{
  SmdHits = new TClonesArray("SmdHit",NSMDSLATS);
  SmdNS = new TClonesArray("SmdNorthSouth",NSMD);
}

SmdOutv1::~SmdOutv1()
{  
  Clear();
  delete SmdHits;
  delete SmdNS;
}

void SmdOutv1::Clear(Option_t *option)
{  
  SmdHits->Clear();
  SmdNS->Clear();
}

void SmdOutv1::Reset()
{
  Clear();
  return;
}

void SmdOutv1::identify(std::ostream& out) const
{
  out << "identify yourself: I am a SmdOutv1 object" << std::endl;
  return;
}

int SmdOutv1::AddSmdHit(const float charge, const float time0, const float time1, const int islat)
{
  TClonesArray &smdhits = *SmdHits;
  new (smdhits[islat]) SmdHit(charge,time0,time1);
  return 0;
}

float SmdOutv1::get_Charge(const int islat) const
{

  SmdHit *smdhit = (SmdHit*) GetSmdHits()->UncheckedAt(islat);
  return((smdhit) ? smdhit->get_Charge() : INVALID_FLOAT);
}

float SmdOutv1::get_Time0(const int islat) const
{
  SmdHit *smdhit = (SmdHit*) GetSmdHits()->UncheckedAt(islat);
  return((smdhit) ? smdhit->get_Time0() : INVALID_FLOAT);
}

float SmdOutv1::get_Time1(const int islat) const
{
  SmdHit *smdhit = (SmdHit*) GetSmdHits()->UncheckedAt(islat);
  return((smdhit) ? smdhit->get_Time1() : INVALID_FLOAT);
}

void SmdOutv1::AddSmdNS(const float xpos, const float ypos, const float e, const int arm)
{
  TClonesArray &smdns = *SmdNS;
  new (smdns[arm]) SmdNorthSouth(xpos,ypos,e);
  return;
}

float SmdOutv1::get_Xpos(const int arm) const
{
  SmdNorthSouth *smdns = (SmdNorthSouth*) GetSmdNS()->UncheckedAt(arm);
  return ((smdns) ? smdns->get_Xpos() : INVALID_FLOAT);
}

float SmdOutv1::get_Ypos(const int arm) const
{
  SmdNorthSouth *smdns = (SmdNorthSouth*) GetSmdNS()->UncheckedAt(arm);
  return ((smdns) ? smdns->get_Ypos() : INVALID_FLOAT);
}

/*
float SmdOutv1::get_Xrms(const int arm) const
{
  SmdNorthSouth *smdns = (SmdNorthSouth*) GetSmdNS()->UncheckedAt(arm);
  return ((smdns) ? smdns->get_Xrms() : INVALID_FLOAT);
}

float SmdOutv1::get_Yrms(const int arm) const
{
  SmdNorthSouth *smdns = (SmdNorthSouth*) GetSmdNS()->UncheckedAt(arm);
  return ((smdns) ? smdns->get_Yrms() : INVALID_FLOAT);
}
*/

float SmdOutv1::get_Energy(const int arm) const
{
  SmdNorthSouth *smdns = (SmdNorthSouth*) GetSmdNS()->UncheckedAt(arm);
  return ((smdns) ? smdns->get_Energy() : INVALID_FLOAT);
}

int SmdOutv1::isValid() const
{
  if (SmdHits->GetEntries())
    {
      return 1;
    }
  return 0;
}
