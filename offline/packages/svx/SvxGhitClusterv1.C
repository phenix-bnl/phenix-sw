// ========================
// FILE: SvxGhitClusterv1.C
// ========================
// ***********************************************************************
// Implementation of SvxGhitClusterv1
// ---
// Created  by V. L. Rykov on 15-Feb-2004
//
// Modified by V. L. Rykov on 09-May-2004: Sorting flag and methods added.
// ***********************************************************************

#include <SvxGhitClusterv1.h>

#include <cstdlib>

ClassImp(SvxGhitClusterv1)

// Static member(s) initialization
short SvxGhitClusterv1::sorting_switch = 0;// -1: sort ghits; +1: sort clusters

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxGhitClusterv1::SvxGhitClusterv1(SvxGhitClusterList* g2c_list   ,
				   SvxGhitCluster*     ghitcluster)
{
  g2cList = g2c_list;
  if ( ghitcluster ) {
    ghitID    = ghitcluster->get_ghitID()    ;
    clusterID = ghitcluster->get_clusterID() ;
  } else {
    ghitID    = -9999 ;
    clusterID = -9999 ;
  }
  //std::cout << "SvxGhitClusterv1 object created" << std::endl;
}

SvxGhitClusterv1::SvxGhitClusterv1(SvxGhitCluster* ghitcluster)
{
  g2cList = 0;
  if ( ghitcluster ) {
    ghitID    = ghitcluster->get_ghitID()    ;
    clusterID = ghitcluster->get_clusterID() ;
  } else {
    ghitID    = -9999 ;
    clusterID = -9999 ;
  }
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxGhitClusterv1::Reset()
{
  ghitID    = -9999 ;
  clusterID = -9999 ;
  if ( g2cList ) g2cList->unSort();
}

int SvxGhitClusterv1::isValid() const
{
  return ( (ghitID >= 0) || (clusterID >= 0) ) ? 1 : 0 ;
}

void SvxGhitClusterv1::identify(std::ostream& os) const
{
  os << "Identify yourself: SvxGhitClusterv1 object: isValid() = "
     << isValid() << std::endl;
}

// Sorting
// """""""
Bool_t SvxGhitClusterv1::IsSortable() const {
  return ( abs(sorting_switch) == 1 ) ? true : false;
}

//Int_t SvxGhitClusterv1::Compare(const PHObject* ghit2cluster) const {
Int_t SvxGhitClusterv1::Compare(const TObject* ghit2cluster) const {
  int diffp;
  if ( sorting_switch == -1 ) {
    diffp = ghitID - ((SvxGhitCluster*) ghit2cluster)->get_ghitID();
  } else if ( sorting_switch == 1 ) {
    diffp = clusterID - ((SvxGhitCluster*) ghit2cluster)->get_clusterID();
  } else {
    return 0;
  }
  if ( diffp < 0 ) return -1;
  if ( diffp > 0 ) return  1;
  return 0;
}

// Methods
// """""""
void SvxGhitClusterv1::Copy(SvxGhitCluster* hit)
{
   ghitID    = hit->get_ghitID()    ;
   clusterID = hit->get_clusterID() ;
}

void SvxGhitClusterv1::print() const
{
  std::cout << "SvxGhitClusterv1 relater of Ghit#" << ghitID
	    << " to Cluster#" << clusterID << std::endl;
}
