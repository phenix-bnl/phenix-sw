#include <SvxHit.h>
#include <cstdlib>

// ***********************************************************************
// Implementation of the base class for SVX hits
// ---
// Created by V. L. Rykov on 09-Feb-2004
//
// Modified by V. L. Rykov on 09-May-2004: Sorting flag and methods added.
// ***********************************************************************

ClassImp(SvxHit)

// Static member(s) initialization
short SvxHit::sorting_switch = 0;     // -1: sort hitID; +1: sort sensorID

// Constructor(s)
// """"""""""""""
SvxHit::SvxHit(SvxHit *hit)
{
  if ( hit ) {
    hitID      =         hit->get_hitID()      ;
    svxSection = (short) hit->get_svxSection() ;
    layer      = (short) hit->get_layer()      ;
    ladder     = (short) hit->get_ladder()     ;
    sensor     = (short) hit->get_sensor()     ;
  } else {
    hitID      = -9999 ;
    svxSection = -9999 ;
    layer      = -9999 ;
    ladder     = -9999 ;
    sensor     = -9999 ;
  }

  return;
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxHit::Reset()
{
  hitID      = -9999 ;
  svxSection = -9999 ;
  layer      = -9999 ;
  ladder     = -9999 ;
  sensor     = -9999 ;
  return;
}

int SvxHit::isValid() const
{
  return (hitID < 0) ? 0 : 1;
}

void SvxHit::identify(std::ostream& os) const
{
  os << "Identify yourself: SvxHit object: hitID = " << hitID << std::endl;
}

// Sorting
// """""""
Bool_t SvxHit::IsSortable() const {
  return ( abs(sorting_switch) == 1 ) ? true : false;
}

Int_t SvxHit::Compare(const TObject* svxhit) const {
   const SvxHit* hit = dynamic_cast<const SvxHit*>(svxhit);
  if ( sorting_switch == -1 ) {
    int diffp = hitID - hit->get_hitID();
    if ( diffp < 0 ) return -1;
    if ( diffp > 0 ) return  1;
  } else if ( sorting_switch == 1 ) {
    int diffp = svxSection - hit->get_svxSection();
    if ( diffp < 0 ) return -1;
    if ( diffp > 0 ) return  1;
    diffp = layer - hit->get_layer();
    if ( diffp < 0 ) return -1;
    if ( diffp > 0 ) return  1;
    diffp = ladder - hit->get_ladder();
    if ( diffp < 0 ) return -1;
    if ( diffp > 0 ) return  1;
    diffp = sensor - hit->get_sensor();
    if ( diffp < 0 ) return -1;
    if ( diffp > 0 ) return  1;
  }
  return 0;
}    

// Methods
// """""""
bool SvxHit::check_hitID    (SvxHit* hit) const {
  return ( hit->get_hitID() == hitID ) ? true : false;
}

bool SvxHit::check_sensorID (SvxHit* hit) const {
  bool testresult = false;
  if ( hit->get_ladder() == ladder   ) {
    if ( hit->get_sensor() == sensor ) {
      if ( hit->get_layer() == layer ) {
	if ( hit->get_svxSection() == svxSection ) testresult = true;
      }
    }
  }
  return testresult;
}

void SvxHit::Copy(SvxHit* hit)
{
    hitID      =         hit->get_hitID()      ;
    svxSection = (short) hit->get_svxSection() ;
    layer      = (short) hit->get_layer()      ;
    ladder     = (short) hit->get_ladder()     ;
    sensor     = (short) hit->get_sensor()     ;
}

void SvxHit::print() const
{
  std::cout << "SvxHit: hitID = "    << hitID      << std::endl;
  std::cout << "  svxSection    = " << svxSection << std::endl;
  std::cout << "  layer         = " << layer      << std::endl;
  std::cout << "  ladder        = " << ladder     << std::endl;
  std::cout << "  sensor        = " << sensor     << std::endl;
  return;
}
