// ==========================
// FILE: SvxRawhitClusterv1.C
// ==========================
// ***********************************************************************
// Implementation of SvxRawhitClusterv1
// ---
// Created  by V. L. Rykov on 15-Feb-2004
//
// Modified by V. L. Rykov on 09-May-2004: Sorting flag and methods added.
// ***********************************************************************

#include <SvxRawhitClusterv1.h>

#include <cstdlib>

ClassImp(SvxRawhitClusterv1)

// Static member(s) initialization
short SvxRawhitClusterv1::sorting_switch = 0;  // initialized unsorted

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxRawhitClusterv1::SvxRawhitClusterv1(SvxRawhitClusterList* r2c_list     ,
				       SvxRawhitCluster*     rawhitcluster)
{
  r2cList = r2c_list;
  if ( rawhitcluster ) {
    rawhitID  = rawhitcluster->get_rawhitID()  ;
    clusterID = rawhitcluster->get_clusterID() ;
  } else {
    rawhitID  = -9999 ;
    clusterID = -9999 ;
  }
  //std::cout << "SvxRawhitClusterv1 object created" << std::endl;
}

SvxRawhitClusterv1::SvxRawhitClusterv1(SvxRawhitCluster* rawhitcluster)
{
  r2cList = 0;
  if ( rawhitcluster ) {
    rawhitID  = rawhitcluster->get_rawhitID()  ;
    clusterID = rawhitcluster->get_clusterID() ;
  } else {
    rawhitID  = -9999 ;
    clusterID = -9999 ;
  }
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxRawhitClusterv1::Reset()
{
  rawhitID  = -9999 ;
  clusterID = -9999 ;
  if ( r2cList ) r2cList->unSort();
}

int SvxRawhitClusterv1::isValid() const
{
  return ( (rawhitID >= 0) || (clusterID >= 0) ) ? 1 : 0 ;
}

void SvxRawhitClusterv1::identify(std::ostream& os) const
{
  os << "Identify yourself: SvxRawhitClusterv1 object: isValid() = "
     << isValid() << std::endl;
}

// Sorting
// """""""
Bool_t SvxRawhitClusterv1::IsSortable() const {
  return ( abs(sorting_switch) == 1 ) ? true : false;
}

//Int_t SvxRawhitClusterv1::Compare(const PHObject* rawhit2cluster) const {
Int_t SvxRawhitClusterv1::Compare(const TObject* rawhit2cluster) const {
  int diffp;
  if ( sorting_switch == -1 ) {
    diffp = rawhitID - ((SvxRawhitCluster*) rawhit2cluster)->get_rawhitID() ;
  } else if ( sorting_switch == 1 ) {
    diffp = clusterID - ((SvxRawhitCluster*) rawhit2cluster)->get_clusterID();
  } else {
    return 0;
  }
  if ( diffp < 0 ) return -1;
  if ( diffp > 0 ) return  1;
  return 0;
}

// Methods
// """""""
void SvxRawhitClusterv1::Copy(SvxRawhitCluster* hit)
{
   rawhitID  = hit->get_rawhitID()  ;
   clusterID = hit->get_clusterID() ;
}

void SvxRawhitClusterv1::print() const
{
  std::cout << "SvxRawhitClusterv1 relater of Rawhit#" << rawhitID
	    << " to Cluster#" << clusterID << std::endl;
}
