#include <MpcOutV1.h>
#include <iostream>

using namespace std;

ClassImp(MpcOutV1)

MpcOutV1::MpcOutV1()
{
  Clear();
}

int MpcOutV1::isValid() const
{
  Float_t etot = mpcesum[0] + mpcesum[1];
  return( (etot > 0.) ? 1 : 0 );
}

void MpcOutV1::Clear(Option_t *option)
{
  mpcntow[0]  = 0;
  mpcntow[1]  = 0;
  mpcesum[0]  = 0.;
  mpcesum[1]  = 0.;
  mpcz   = 0.;
  mpcdz  = -999.;
  mpct0  = 0.;
  mpcdt0 = -999.;
}

void MpcOutV1::Reset()
{
  Clear();
}

void MpcOutV1::identify(ostream& out) const
{
  out << "identify yourself: I am a MpcOutV1 object" << endl;
  out << "Esum: " << mpcesum[0] << " " << mpcesum[1] << endl;
  out << "Ntowers: " << mpcntow[0] << " " << mpcntow[1] << endl;
  out << "Vertex: " << mpcz << " Error: " << mpcdz << endl;
  out << "T0: " << mpct0 << " Error: " << mpcdt0 << endl;
}

