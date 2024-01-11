// ====================
// FILE: SvxClusterv5.C
// ====================

#include "SvxClusterv5.h"

// ***********************************************************************
// Implementation of Silicon cluster (version with size and associated track pointers)
// ---
// Created  by Sasha Lebedev <lebedev@iastate.edu> in December 2010
//
// ***********************************************************************

ClassImp(SvxClusterv5)

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxClusterv5::SvxClusterv5(SvxClusterList* lst, SvxCluster* cluster) 
: SvxCluster(cluster),
  clusterList(lst),
  size(0),
  circumference(0),
  AssociatedCGL(-1),
  AssociatedStandalone(-1),
  ambiguous(0),
  Nhot(0),
  Ncold(0)
{
  if ( cluster ) {
    sensorType = (short) cluster->get_sensorType() ;
    edgeflag   = (short) cluster->get_edgeflag()   ;
    for ( int i = 0; i < 2; i++ ) {
      adc[i] = cluster->get_adc(i);
    }
    for ( int i = 0; i < 3; i++ ) {
      xyz_local [i] = cluster->get_xyz_local (i);
      xyz_global[i] = cluster->get_xyz_global(i);
    }

  } else {
    sensorType = -9999 ;
    edgeflag   = -9999 ;
    for ( int i = 0; i < 2; i++ ) {
      adc[i] = -9999 ;
    }
    for ( int i = 0; i < 3; i++ ) {
      xyz_local [i] = -.9999e4;
      xyz_global[i] = -.9999e4;
    }
  }

  xz_size[0]=0;
  xz_size[1]=0;

}

SvxClusterv5::SvxClusterv5(SvxCluster* cluster) 
: SvxCluster(cluster),
  clusterList(0),
  size(0),
  circumference(0),
  AssociatedCGL(-1),
  AssociatedStandalone(-1),
  ambiguous(0),
  Nhot(0),
  Ncold(0)
{
  if ( cluster ) {
    sensorType = (short) cluster->get_sensorType() ;
    edgeflag   = (short) cluster->get_edgeflag()   ;
    for ( int i = 0; i < 2; i++ ) {
      adc[i] = cluster->get_adc(i);
    }
    for ( int i = 0; i < 3; i++ ) {
      xyz_local [i] = cluster->get_xyz_local (i);
      xyz_global[i] = cluster->get_xyz_global(i);
    }

  } else {
    sensorType = -9999 ;
    edgeflag   = -9999 ;
    for ( int i = 0; i < 2; i++ ) {
      adc[i] = -9999 ;
    }
    for ( int i = 0; i < 3; i++ ) {
      xyz_local [i] = -.9999e4;
      xyz_global[i] = -.9999e4;
    }
  }

  xz_size[0]=0;
  xz_size[1]=0;
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxClusterv5::Reset()
{
  SvxHit::Reset();
  sensorType = -9999 ;
  edgeflag   = -9999 ;
  for ( int i = 0; i < 2; i++ ) {
    adc[i] = -9999 ;
  }
  for ( int i = 0; i < 3; i++ ) {
    xyz_local [i] = -.9999e4;
    xyz_global[i] = -.9999e4;
  }
  if ( clusterList ) clusterList->unSort();
  size=0;
  circumference=0;
  AssociatedCGL=-1;
  AssociatedStandalone=-1;
  Nhot=0;
  Ncold=0;
}

void SvxClusterv5::identify(std::ostream& os) const
{
  os << "SvxClusterv5 object: hitID = "
     << hitID << std::endl;
}

// Methods
// """""""
void SvxClusterv5::Copy(SvxHit* hit) 
{
    SvxHit::Copy(hit);
    SvxCluster* cluster = dynamic_cast<SvxCluster*>(hit);
    sensorType = (short) cluster->get_sensorType() ;
    edgeflag   = (short) cluster->get_edgeflag()   ;
    for ( int i = 0; i < 2; i++ ) {
      adc[i] = cluster->get_adc(i);
    }
    for ( int i = 0; i < 3; i++ ) {
      xyz_local [i] = cluster->get_xyz_local (i);
      xyz_global[i] = cluster->get_xyz_global(i);
    }
  size=cluster->get_size();
  circumference=cluster->get_circumference();
  AssociatedCGL=cluster->get_AssociatedCGL();
  AssociatedStandalone=cluster->get_AssociatedStandalone();
  Ncold=cluster->get_Ncold();
  Nhot=cluster->get_Nhot();
}

void SvxClusterv5::print() const
{
  std::cout << "SvxClusterv5 derived from ";
  SvxHit::print();
  std::cout << "  sensorType    = " << sensorType    << std::endl;
  std::cout << "  edgeflag      = " << edgeflag      << std::endl;
  std::cout << "  adc           =";
  for ( int i = 0; i < 2; i++ ) {
    std::cout << " " << adc[i];
  }
  std::cout << std:: endl;
  std::cout << "  xyz_local       =";
  for ( int i = 0; i < 3; i++ ) {
    std::cout << " " << xyz_local[i];
  }
  std::cout << std::endl;
  std::cout << "  xyz_global      =";
  for ( int i = 0; i < 3; i++ ) {
    std::cout << " " << xyz_global[i];
  }
  std::cout << std::endl;
  std::cout << "  Size = " << size << std::endl;
  std::cout << "  Ncold = " << Ncold << std::endl;
  std::cout << "  Nhot = " << Nhot << std::endl;
  std::cout << "  Circumference = " << circumference << std::endl;
  std::cout << "  Associated CGL track = " << AssociatedCGL << std::endl;
  std::cout << "  Associated standalone track = " << AssociatedStandalone << std::endl;
}



