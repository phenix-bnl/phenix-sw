// =================
// FILE: SvxGhitv1.C
// =================

#include "SvxGhitv1.h"

// ***********************************************************************
// Implementation of SVX GEANT/PISA hit
// Original version by Jeffery Mitchell as of 20-Nov-2003
// ---
// Modified by V. L. Rykov on 09-Feb-2004
//
// Modified by V. L. Rykov on 10-May-2004: Sorting related stuff is added.
// ***********************************************************************

ClassImp(SvxGhitv1)

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxGhitv1::SvxGhitv1(SvxGhitList* lst, SvxGhit* ghit) : SvxGhit(ghit)
{
  ghitList = lst;
  if ( ghit ) {
    isubevent  = ghit->get_isubevent()  ;
    nfile      = ghit->get_nfile()      ;
    mctrack    = ghit->get_mctrack()    ;
    idPart     = ghit->get_idPart()     ;
    track      = ghit->get_track()      ;
    dele       = ghit->get_dele()       ;
    tof        = ghit->get_tof()        ;
    for (int i=0; i<3; i++) {
      xyzglobal[i]   = ghit->get_xyzglobal(i)   ;
      xyzlocalin[i]  = ghit->get_xyzlocalin(i)  ;
      xyzlocalout[i] = ghit->get_xyzlocalout(i) ;
      pmomxyz[i]     = ghit->get_pmomxyz(i)     ;
    }
  } else {
    isubevent  = -9999 ;
    nfile      = -9999 ;
    mctrack    = -9999 ;
    idPart     = -9999 ;
    track      = -9999 ;
    dele       = -9999.;
    tof        = -9999.;
    for (int i=0; i<3; i++) {
      xyzglobal[i]   = -9999.;
      xyzlocalin[i]  = -9999.;
      xyzlocalout[i] = -9999.;
      pmomxyz[i]     = -9999.;
    }
  }
  //std::cout << "SvxGhitv1 object created" << std::endl;
}

SvxGhitv1::SvxGhitv1(SvxGhit* ghit) : SvxGhit(ghit)
{
  ghitList = 0;
  if ( ghit ) {
    isubevent  = ghit->get_isubevent()  ;
    nfile      = ghit->get_nfile()      ;
    mctrack    = ghit->get_mctrack()    ;
    idPart     = ghit->get_idPart()     ;
    track      = ghit->get_track()      ;
    dele       = ghit->get_dele()       ;
    tof        = ghit->get_tof()        ;
    for (int i=0; i<3; i++) {
      xyzglobal[i]   = ghit->get_xyzglobal(i)   ;
      xyzlocalin[i]  = ghit->get_xyzlocalin(i)  ;
      xyzlocalout[i] = ghit->get_xyzlocalout(i) ;
      pmomxyz[i]     = ghit->get_pmomxyz(i)     ;
    }
  } else {
    isubevent  = -9999 ;
    nfile      = -9999 ;
    mctrack    = -9999 ;
    idPart     = -9999 ;
    track      = -9999 ;
    dele       = -9999.;
    tof        = -9999.;
    for (int i=0; i<3; i++) {
      xyzglobal[i]   = -9999.;
      xyzlocalin[i]  = -9999.;
      xyzlocalout[i] = -9999.;
      pmomxyz[i]     = -9999.;
    }
  }
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxGhitv1::Reset()
{
  SvxHit::Reset();
  isubevent  = -9999 ;
  nfile      = -9999 ;
  mctrack    = -9999 ;
  idPart     = -9999 ;
  track      = -9999 ;
  dele       = -9999.;
  tof        = -9999.;
  for (int i=0; i<3; i++) {
    xyzglobal[i]   = -9999.;
    xyzlocalin[i]  = -9999.;
    xyzlocalout[i] = -9999.;
    pmomxyz[i] = -9999.;
  }
  if ( ghitList ) ghitList->unSort();
}

void SvxGhitv1::identify(std::ostream& os) const
{
  os << "Identify yourself: SvxGhitv1 object: hitID = " << hitID << std::endl;
}

// Methods
// """""""
void SvxGhitv1::Copy(SvxHit* hit)
{
    SvxHit::Copy(hit);
    SvxGhit* ghit = dynamic_cast<SvxGhit*>(hit);
    isubevent  = ghit->get_isubevent()  ;
    nfile      = ghit->get_nfile()      ;
    mctrack    = ghit->get_mctrack()    ;
    idPart     = ghit->get_idPart()     ;
    track      = ghit->get_track()      ;
    dele       = ghit->get_dele()       ;
    tof        = ghit->get_tof()        ;
    for (int i=0; i<3; i++) {
      xyzglobal[i]   = ghit->get_xyzglobal(i)   ;
      xyzlocalin[i]  = ghit->get_xyzlocalin(i)  ;
      xyzlocalout[i] = ghit->get_xyzlocalout(i) ;
      pmomxyz[i]     = ghit->get_pmomxyz(i)     ;
    }
 }

void SvxGhitv1::print() const
{
    std::cout << "SvxGhitv1 derived from ";
    SvxHit::print();
    std::cout << "  isubevent     = " << isubevent << std::endl;
    std::cout << "  nfile         = " << nfile     << std::endl;
    std::cout << "  mctrack       = " << mctrack   << std::endl;
    std::cout << "  idPart        = " << idPart    << std::endl;
    std::cout << "  track         = " << track     << std::endl;
    std::cout << "  dele          = " << dele      << std::endl;
    std::cout << "  tof           = " << tof       << std::endl;
    std::cout << "  xyzglobal     = " << xyzglobal[0]   << " "
	      << xyzglobal[1]   << " " << xyzglobal[2]   << std::endl;
    std::cout << "  xyzlocalin    = " << xyzlocalin[0]  << " "
	      << xyzlocalin[1]  << " " << xyzlocalin[2]  << std::endl;
    std::cout << "  xyzlocalout   = " << xyzlocalout[0] << " "
	      << xyzlocalout[1] << " " << xyzlocalout[2] << std::endl;
    std::cout << "  pmomxyz       = " << pmomxyz[0]     << " "
	      << pmomxyz[1]     << " " << pmomxyz[2]     << std::endl;
}

//Int_t SvxGhitv1::Compare(const TObject* svxhit) const {
//  if ( sorting_switch == -1 ) {
//    int diffp = hitID - ((SvxHit*) svxhit)->get_hitID();
//    if ( diffp < 0 ) return -1;
//    if ( diffp > 0 ) return  1;
//  } else if ( sorting_switch == 1 ) {
//    int diffp = svxSection - ((SvxHit*) svxhit)->get_svxSection();
//    if ( diffp < 0 ) return -1;
//    if ( diffp > 0 ) return  1;
//    diffp = layer - ((SvxHit*) svxhit)->get_layer();
//    if ( diffp < 0 ) return -1;
//    if ( diffp > 0 ) return  1;
//    diffp = ladder - ((SvxHit*) svxhit)->get_ladder();
//    if ( diffp < 0 ) return -1;
//    if ( diffp > 0 ) return  1;
//    diffp = sensor - ((SvxHit*) svxhit)->get_sensor();
//    if ( diffp < 0 ) return -1;
//    if ( diffp > 0 ) return  1;
//  }
//  return 0;
//}


