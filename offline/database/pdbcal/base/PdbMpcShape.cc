#include <PdbMpcShape.hh>
#include <iostream>

using namespace std;

PdbMpcShape::~PdbMpcShape()
{
  reset();
  return;
}

void PdbMpcShape::print() const
{
  for( vector<float>::const_iterator iter = _valvec.begin(); iter != _valvec.end(); iter++ )
    { cout << *iter << endl; }
}

void PdbMpcShape::add_float(float fl){
  _valvec.push_back(fl);
}

float PdbMpcShape::getValue(int pos) const {
  return _valvec.at(pos);
}

void PdbMpcShape::reset() {
  nsamples = 0;
  start_time = -9999.;
  end_time = -9999.;
  _valvec.clear();
}
