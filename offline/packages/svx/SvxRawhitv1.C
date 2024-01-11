// ===================
// FILE: SvxRawhitv1.C
// ===================

#include "SvxRawhitv1.h"

// **************************************************************
// Implementation of Silicon raw hit
// ---
// Created  by V. L. Rykov on 09-Feb-2004
//
// Modified by V. L. Rykov on 11-May-2004: Sorting related stuff is added.
// ***********************************************************************

ClassImp(SvxRawhitv1)

//  Comparison/sorting of sensorSection, sensorReadout & channel
unsigned int SvxRawhitv1::compareChan = 3;    //! varies from 0 to 3;

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxRawhitv1::SvxRawhitv1(SvxRawhitList* lst, SvxRawhit* rawhit)
  : SvxRawhit(rawhit)
{
  rawhitList = lst;
  if ( rawhit ) {
    sensorSection = (short) rawhit->get_sensorSection() ;
    sensorReadout = (short) rawhit->get_sensorReadout() ;
    sensorType    = (short) rawhit->get_sensorType()    ;
    channel       =         rawhit->get_channel()       ;
    adc           =         rawhit->get_adc()           ;
  } else {
    sensorSection = -9999 ;
    sensorReadout = -9999 ;
    sensorType    = -9999 ;
    channel       = -9999 ;
    adc           = -9999 ;
  }
  //std::cout << "SvxRawhitv1 object created" << std::endl;
}

SvxRawhitv1::SvxRawhitv1(SvxRawhit* rawhit) : SvxRawhit(rawhit)
{
  rawhitList = 0;
  if ( rawhit ) {
    sensorSection = (short) rawhit->get_sensorSection() ;
    sensorReadout = (short) rawhit->get_sensorReadout() ;
    sensorType    = (short) rawhit->get_sensorType()    ;
    channel       =         rawhit->get_channel()       ;
    adc           =         rawhit->get_adc()           ;
  } else {
    sensorSection = -9999 ;
    sensorReadout = -9999 ;
    sensorType    = -9999 ;
    channel       = -9999 ;
    adc           = -9999 ;
  }
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxRawhitv1::Reset()
{
  SvxHit::Reset();
  sensorSection = -9999 ;
  sensorReadout = -9999 ;
  sensorType    = -9999 ;
  channel       = -9999 ;
  adc           = -9999 ;
  if ( rawhitList ) rawhitList->unSort();
}

void SvxRawhitv1::identify(std::ostream& os) const
{
  os << "Identify yourself: SvxRawhitv1 object: hitID = "
     << hitID << std::endl;
}

// Sorting
// """""""
Int_t SvxRawhitv1::Compare(const TObject* svxhit) const {
  Int_t result = SvxHit::Compare(svxhit);
  if ( sorting_switch != 1 || result || !compareChan  ) return result;

  const SvxRawhit* rhit = dynamic_cast<const SvxRawhit*>(svxhit);
  int diffp = sensorSection - rhit->get_sensorSection();
  if ( diffp < 0 )       return -1;
  if ( diffp > 0 )       return  1;
  if ( compareChan < 2 ) return  0;
  diffp = sensorReadout - rhit->get_sensorReadout();
  if ( diffp < 0 )       return -1;
  if ( diffp > 0 )       return  1;
  if ( compareChan < 3 ) return  0;
  diffp = channel - rhit->get_channel();
  if ( diffp < 0 ) return -1;
  if ( diffp > 0 ) return  1;
  return 0;
}    

// Methods
// """""""
void SvxRawhitv1::Copy(SvxHit* hit)
{
    SvxHit::Copy(hit);
    SvxRawhit* rhit = dynamic_cast<SvxRawhit*>(hit);
    sensorSection = (short) rhit->get_sensorSection() ;
    sensorReadout = (short) rhit->get_sensorReadout() ;
    sensorType    = (short) rhit->get_sensorType()    ;
    channel       =         rhit->get_channel()       ;
    adc           =         rhit->get_adc()           ;
}

void SvxRawhitv1::print() const
{
    std::cout << "SvxRawhitv1 derived from ";
    SvxHit::print();
    std::cout << "  sensorSection = " << sensorSection << std::endl;
    std::cout << "  sensorReadout = " << sensorReadout << std::endl;
    std::cout << "  sensorType    = " << sensorType    << std::endl;
    std::cout << "  channel       = " << channel       << std::endl;
    std::cout << "  adc           = " << adc           << std::endl;
}
