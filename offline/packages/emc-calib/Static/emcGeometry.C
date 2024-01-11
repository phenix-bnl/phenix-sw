#include "emcGeometry.h"
#include <iostream>
#include <cassert>

size_t emcGeometry::fgNumberOfSectors = 8;

using std::cerr;
using std::endl;
using std::ostream;

//_____________________________________________________________________________
emcGeometry::emcGeometry() : emcManageable("","","emcGeometry")
{
  //  fSectors.reserve(fgNumberOfSectors);
}

//_____________________________________________________________________________
emcGeometry::~emcGeometry()
{

}

//_____________________________________________________________________________
emcGeometry::emcGeometry(const emcGeometry& obj) : emcManageable()
{
  obj.Copy(*this);
}

//_____________________________________________________________________________
emcGeometry& emcGeometry::operator=(const emcGeometry& obj)
{
  if ( this != &obj ) {
    obj.Copy(*this);
  }
  return *this;
}

//_____________________________________________________________________________
void emcGeometry::Copy(emcGeometry& obj) const
{
  emcManageable::Copy(static_cast<emcManageable&>(obj));
  obj.fSectors.clear();
  size_t i;
  for ( i = 0; i < fSectors.size(); i++ ) {
    obj.fSectors.push_back(fSectors[i]);
  }
  assert(obj.fSectors.size()==fSectors.size());
  obj.fStart = fStart;
  obj.fEnd = fEnd;
}

//_____________________________________________________________________________
bool
emcGeometry::IsValid(const PHTimeStamp& cwhen) const
{
  PHTimeStamp& when = const_cast<PHTimeStamp&>(cwhen);
  if (when.isInRange(fStart,fEnd)) return true;
  return false;
}

//_____________________________________________________________________________
const SecGeom& emcGeometry::Sector(size_t iS) const
{
  static SecGeom dummy;

  if ( iS < fSectors.size() ) {
    return fSectors[iS];
  }
  else {
    cerr << "emcGeometry::GetSector(size_t iS) : iS out of bounds (iS=" << iS
	 << ") max=" << fSectors.size() << endl;
    return dummy;
  }
}

//_____________________________________________________________________________
ostream& operator<<(ostream& out, const emcGeometry& obj) 
{
  size_t i;
  out << "emcGeometry:" << endl;
  for ( i = 0; i < obj.fSectors.size(); i++ ) {
    out << "Sector " << i << " : " << endl;
    out << obj.fSectors[i] << endl;
  }
  return out;
}


//_____________________________________________________________________________
void emcGeometry::PrintCorners(void) const
{
}

//_____________________________________________________________________________
void emcGeometry::Reset(void)
{
  fSectors.clear();
}


