#include <PdbMpcLed.hh>
#include <phool.h>
#include <iomanip>

using namespace std;

PdbMpcLed::PdbMpcLed()
{
  reset();
}

PdbMpcLed& PdbMpcLed::operator=(const PdbMpcLed& p)
{
  status=p.status;
  fee576ch=p.fee576ch;
  mean=p.mean;
  dmean=p.dmean;
  chisquare=p.chisquare;
  ndf=p.ndf;

  return *this;
}

void PdbMpcLed::reset()
{
  status=-9999;
  fee576ch=-999;
  mean=-9999.;
  dmean=-9999.;
  chisquare=-9999.;
  ndf=-9999;
}

void PdbMpcLed::print() const
{
  cout << "MpcLed status " << status << " chi2/ndf " << chisquare << "/" << ndf << " mean: " << mean << "+/-" << dmean << endl;
}

