#include "BbcReturncodes.h"
#include "BbcOutv1.h"
#include "BbcNorthSouth.h"

#include "TClonesArray.h"
#include <iostream>

static const int NPMTBBCV1 = 128;
static const int NBBC = 2;

using namespace std;

ClassImp(BbcOutv1)

//______________________________________
BbcOutv1::BbcOutv1()
{
  Init();
  BbcNS = new TClonesArray("BbcNorthSouth",NBBC);
}

//______________________________________
void BbcOutv1::Init()
{
  Bbc_ZVertex   = -99999.9;
  Bbc_dZVertex  = -99999.9;
  Bbc_TimeZero  = -99999.9;
  Bbc_dTimeZero = -99999.9;
}

//______________________________________
BbcOutv1::~BbcOutv1()
{  
  if (BbcNS)
    {
      Clear();
      delete BbcNS;
    }
}

//______________________________________
int BbcOutv1::isValid() const
{
  return((Bbc_TimeZero >-9999.) ? 1 : 0);
}

//______________________________________
void BbcOutv1::Clear(Option_t *option)
{  
  BbcNS->Clear();
}

//______________________________________
void BbcOutv1::Reset()
{
  Clear();
  Init();
}

//______________________________________
void BbcOutv1::identify(ostream& out) const
{
  out << "identify yourself: I am a BbcOutv1 object" << endl;
  out << "Vertex: " << Bbc_ZVertex << " Error: " << Bbc_dZVertex << endl;
  out << "T0: " << Bbc_TimeZero << " Error: " << Bbc_dTimeZero << endl;
}

//______________________________________
void BbcOutv1::set_TimeZero(const float t0, const float t0err )
{
  Bbc_TimeZero  = t0;
  Bbc_dTimeZero = t0err;
}

//______________________________________
void BbcOutv1::set_Vertex( const float vtx, const float vtxerr)
{
  Bbc_ZVertex   = vtx;
  Bbc_dZVertex  = vtxerr;
}

//______________________________________
void BbcOutv1::set_dZVertex( const float vtxerr)
{Bbc_dZVertex = vtxerr;}

//______________________________________
void BbcOutv1::AddBbcNS(const short npmt, const float energy, const float timing, const short nBbc)
{
  TClonesArray &Bbcnoso = *BbcNS;
  new(Bbcnoso[nBbc]) BbcNorthSouth(npmt, energy,timing);
}

//______________________________________
short BbcOutv1::get_nPmt(const short nBbc) const
{
  BbcNorthSouth *Bbcnoso = (BbcNorthSouth*) GetBbcNS()->UncheckedAt(nBbc);
  //  if Bbcnoso=nil (does not exist) return BBC_INVALID_SHORT, else nPmt
  return((Bbcnoso) ? Bbcnoso->get_nPmt() : BBC_INVALID_SHORT);
}

//______________________________________
float BbcOutv1::get_ChargeSum(const short nBbc) const
{
  BbcNorthSouth *Bbcnoso = (BbcNorthSouth*) GetBbcNS()->UncheckedAt(nBbc);
  //  if Bbcnoso=nil (does not exist) return BBC_INVALID_FLOAT, else Energy
  return((Bbcnoso) ? Bbcnoso->get_ChargeSum() : BBC_INVALID_FLOAT);
}

float 
BbcOutv1::get_Timing(const short nBbc) const
{
  BbcNorthSouth *Bbcnoso = (BbcNorthSouth*) GetBbcNS()->UncheckedAt(nBbc);
  //  if Bbcnoso=nil (does not exist) return BBC_INVALID_FLOAT, else Timing
  return((Bbcnoso) ? Bbcnoso->get_Timing() : BBC_INVALID_FLOAT);
}

