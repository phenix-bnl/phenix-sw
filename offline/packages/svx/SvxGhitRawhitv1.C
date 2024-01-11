// =======================
// FILE: SvxGhitRawhitv1.C
// =======================
// ***********************************************************************
// Implementation of SvxGhitRawhitv1
// ---
// Created  by V. L. Rykov on 09-Feb-2004
//
// Modified by V. L. Rykov on 09-May-2004: Sorting flag and methods added.
// ***********************************************************************

#include <SvxGhitRawhitv1.h>

#include <cstdlib>

ClassImp(SvxGhitRawhitv1)

// Static member(s) initialization
short SvxGhitRawhitv1::sorting_switch = 0;// -1: sort ghits; +1: sort rawhits

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxGhitRawhitv1::SvxGhitRawhitv1(SvxGhitRawhitList* g2r_list  ,
				 SvxGhitRawhit*     ghitrawhit)
{
  g2rList = g2r_list;
  if ( ghitrawhit ) {
    ghitID   = ghitrawhit->get_ghitID()   ;
    rawhitID = ghitrawhit->get_rawhitID() ;
  } else {
    ghitID   = -9999 ;
    rawhitID = -9999 ;
  }
  //std::cout << "SvxGhitRawhitv1 object created" << std::endl;
}

SvxGhitRawhitv1::SvxGhitRawhitv1(SvxGhitRawhit* ghitrawhit)
{
  g2rList = 0;
  if ( ghitrawhit ) {
    ghitID   = ghitrawhit->get_ghitID()   ;
    rawhitID = ghitrawhit->get_rawhitID() ;
  } else {
    ghitID   = -9999 ;
    rawhitID = -9999 ;
  }
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxGhitRawhitv1::Reset()
{
  ghitID   = -9999 ;
  rawhitID = -9999 ;
  if ( g2rList ) g2rList->unSort();
}

int SvxGhitRawhitv1::isValid() const
{
  return ( (ghitID >= 0) || (rawhitID >= 0) ) ? 1 : 0 ;
}

void SvxGhitRawhitv1::identify(std::ostream& os) const
{
  os << "Identify yourself: SvxGhitRawhitv1 object: isValid() = "
     << isValid() << std::endl;
}

// Sorting
// """""""
Bool_t SvxGhitRawhitv1::IsSortable() const {
  return ( abs(sorting_switch) == 1 ) ? true : false;
}

//Int_t SvxGhitRawhitv1::Compare(const PHObject* ghit2rawhit) const {
Int_t SvxGhitRawhitv1::Compare(const TObject* ghit2rawhit) const {
  int diffp;
  if ( sorting_switch == -1 ) {
    diffp = ghitID - ((SvxGhitRawhit*) ghit2rawhit)->get_ghitID()  ;
  } else if ( sorting_switch == 1 ) {
    diffp = rawhitID - ((SvxGhitRawhit*) ghit2rawhit)->get_rawhitID();
  } else {
    return 0;
  }
  if ( diffp < 0 ) return -1;
  if ( diffp > 0 ) return  1;
  return 0;
}

// Methods
// """""""
void SvxGhitRawhitv1::Copy(SvxGhitRawhit* hit)
{
   ghitID   = hit->get_ghitID()   ;
   rawhitID = hit->get_rawhitID() ;
}

void SvxGhitRawhitv1::print() const
{
  std::cout << "SvxGhitRawhitv1 relater of Ghit#" << ghitID
	    << " to Rawhit#" << rawhitID << std::endl;
}
