// ===================
// FILE: SvxRawhitv3.C
// ===================

#include "SvxRawhitv3.h"

// **************************************************************
// Implementation of Silicon raw hit
// ---
// Created  by Sasha Lebedev <lebedev@iastate.edu> in June 2010
// v3: Add Hot/Dead Flag: Alex Shaver <alexshaver@gmail.com> in August 2011
//
// ***********************************************************************

ClassImp(SvxRawhitv3)

//  Comparison/sorting of sensorSection, sensorReadout & channel
unsigned int SvxRawhitv3::compareChan = 3;    //! varies from 0 to 3;

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxRawhitv3::SvxRawhitv3(SvxRawhitList* lst, SvxRawhit* rawhit)
  : SvxRawhit(rawhit)
{
  rawhitList = lst;
  if ( rawhit ) {
    sensorSection = (short) rawhit->get_sensorSection() ;
    sensorReadout = (short) rawhit->get_sensorReadout() ;
    sensorType    = (short) rawhit->get_sensorType()    ;
    channel       =         rawhit->get_channel()       ;
    adc           =         rawhit->get_adc()           ;
    pixelModule   =         rawhit->get_pixelModule()   ;
    pixelROC      =         rawhit->get_pixelROC   ()   ;
    HotDeadFlag   =         rawhit->get_HotDeadFlag()   ;
  } 
  else {
    sensorSection = -9999 ;
    sensorReadout = -9999 ;
    sensorType    = -9999 ;
    channel       = -9999 ;
    adc           = -9999 ;
    pixelModule   = -9999 ;
    pixelROC      = -9999 ;
    HotDeadFlag   = 0 ;
  }
  //std::cout << "SvxRawhitv2 object created" << std::endl;
}

SvxRawhitv3::SvxRawhitv3(SvxRawhit* rawhit) : SvxRawhit(rawhit)
{
  rawhitList = 0;
  if ( rawhit ) {
    sensorSection = (short) rawhit->get_sensorSection() ;
    sensorReadout = (short) rawhit->get_sensorReadout() ;
    sensorType    = (short) rawhit->get_sensorType()    ;
    channel       =         rawhit->get_channel()       ;
    adc           =         rawhit->get_adc()           ;
    pixelModule   =         rawhit->get_pixelModule()   ;
    pixelROC      =         rawhit->get_pixelROC   ()   ;
    HotDeadFlag   =         rawhit->get_HotDeadFlag()   ;
  } 
  else {
    sensorSection = -9999 ;
    sensorReadout = -9999 ;
    sensorType    = -9999 ;
    channel       = -9999 ;
    adc           = -9999 ;
    pixelModule   = -9999 ;
    pixelROC      = -9999 ;
    HotDeadFlag   = 0 ;
  }
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxRawhitv3::Reset()
{
  SvxHit::Reset();
  sensorSection = -9999 ;
  sensorReadout = -9999 ;
  sensorType    = -9999 ;
  channel       = -9999 ;
  adc           = -9999 ;
  pixelModule   = -9999 ;
  pixelROC      = -9999 ;
  HotDeadFlag   = 0 ;
  if ( rawhitList ) rawhitList->unSort();
}

void SvxRawhitv3::identify(std::ostream& os) const
{
  os << "SvxRawhitv3 object: hitID = "
     << hitID << std::endl;
}

// Sorting
// """""""
Int_t SvxRawhitv3::Compare(const TObject* svxhit) const {
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
void SvxRawhitv3::Copy(SvxHit* hit)
{
    SvxHit::Copy(hit);
    SvxRawhit* rhit = dynamic_cast<SvxRawhit*>(hit);
    sensorSection = (short) rhit->get_sensorSection() ;
    sensorReadout = (short) rhit->get_sensorReadout() ;
    sensorType    = (short) rhit->get_sensorType()    ;
    channel       =         rhit->get_channel()       ;
    adc           =         rhit->get_adc()           ;
    pixelModule   =         rhit->get_pixelModule()   ;
    pixelROC      =         rhit->get_pixelROC   ()   ;
    HotDeadFlag   =         rhit->get_HotDeadFlag()   ;
}

void SvxRawhitv3::print() const
{
    std::cout << "SvxRawhitv2 derived from ";
    SvxHit::print();
    std::cout << "  sensorSection = " << sensorSection << std::endl;
    std::cout << "  sensorReadout = " << sensorReadout << std::endl;
    std::cout << "  sensorType    = " << sensorType    << std::endl;
    std::cout << "  channel       = " << channel       << std::endl;
    std::cout << "  adc           = " << adc           << std::endl;
    std::cout << "  pixelModule   = " << pixelModule   << std::endl;
    std::cout << "  pixelROC      = " << pixelROC      << std::endl;
    std::cout << "  HotDeadFlag   = " << HotDeadFlag   << std::endl;
}
