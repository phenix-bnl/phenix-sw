#include <CorrDataV1.h>
#include <recoConsts.h>

ClassImp(CorrDataV1)

using namespace std;

CorrDataV1::CorrDataV1()
{
  bbcq   = -9999;
  nchips = 24;
  nhits = new Short_t[nchips];
  for (int i=0; i<nchips; i++)
  {
    nhits[i] = -9999;
  }
}

CorrDataV1::CorrDataV1(const CorrData &m)
{
  bbcq = m.get_bbcq();
  nchips = 24;
  nhits = new Short_t[nchips];
  for (int i=0; i<nchips; i++)
  {
    nhits[i] = m.get_nhits(i);
  }
}

CorrData& CorrDataV1::operator=(const CorrData &rhs)
{
  if ( this == &rhs ) return *this;
  
  bbcq = rhs.get_bbcq();
  for (int i=0; i<nchips; i++)
  {
    nhits[i] = rhs.get_nhits(i);
  }

  return *this;
}

void CorrDataV1::print(std::ostream& out)
{
  out << bbcq;
  for (int i=0; i<nchips; i++)
  {
    out << "\t" << nhits[i];
  }
  cout << endl;
}
