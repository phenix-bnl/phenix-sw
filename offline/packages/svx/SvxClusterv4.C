// ====================
// FILE: SvxClusterv4.C
// ====================

#include "SvxClusterv4.h"

// ***********************************************************************
// Implementation of Silicon cluster (version with size and associated track pointers)
// ---
// Created  by Sasha Lebedev <lebedev@iastate.edu> in December 2010
//
// ***********************************************************************

ClassImp(SvxClusterv4)

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxClusterv4::SvxClusterv4(SvxClusterList* lst, SvxCluster* cluster) 
: SvxCluster(cluster),
  clusterList(lst),
  size(0),
  circumference(0),
  AssociatedCGL(-1),
  AssociatedStandalone(-1),
  ambiguous(0)
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

SvxClusterv4::SvxClusterv4(SvxCluster* cluster) 
: SvxCluster(cluster),
  clusterList(0),
  size(0),
  circumference(0),
  AssociatedCGL(-1),
  AssociatedStandalone(-1),
  ambiguous(0)
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
void SvxClusterv4::Reset()
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
//    for ( int j = 0; j < 3; j++ ) {
//      size_xyz_local [i][j] = -.9999e4;
//      size_xyz_global[i][j] = -.9999e4;
//    }
  }
  if ( clusterList ) clusterList->unSort();
  size=0;
  circumference=0;
  AssociatedCGL=-1;
  AssociatedStandalone=-1;
}

void SvxClusterv4::identify(std::ostream& os) const
{
  os << "SvxClusterv4 object: hitID = "
     << hitID << std::endl;
}

// Methods
// """""""
void SvxClusterv4::Copy(SvxHit* hit) 
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
      for ( int j = 0; j < 3; j++ ) {
	//cov_xyz_local  [i][j] = cluster->get_cov_xyz_local  (i,j);
	//size_xyz_local [i][j] = cluster->get_size_xyz_local (i,j);
	//cov_xyz_global [i][j] = cluster->get_cov_xyz_global (i,j);
//	size_xyz_global[i][j] = cluster->get_size_xyz_global(i,j);
      }
    }
  size=cluster->get_size();
  circumference=cluster->get_circumference();
  AssociatedCGL=cluster->get_AssociatedCGL();
  AssociatedStandalone=cluster->get_AssociatedStandalone();
}

void SvxClusterv4::print() const
{
  std::cout << "SvxClusterv4 derived from ";
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
  /*
  std::cout << std::endl << "  cov_xyz_local   =";
  for ( int i = 0; i < 3; i++ ) {
    for ( int j = 0; j < 3; j++ ) {
      std::cout << " " << cov_xyz_local[i][j];
    }
    std::cout << std::endl;
    if ( i < 2 ) std::cout << "                   ";
  }
  */
//  std::cout << "  size_xyz_local  =";
//  for ( int i = 0; i < 3; i++ ) {
//    for ( int j = 0; j < 3; j++ ) {
//      std::cout << " " << size_xyz_local[i][j];
//    }
//    std::cout << std::endl;
//    if ( i < 2 ) std::cout << "                   ";
//  }
  std::cout << "  xyz_global      =";
  for ( int i = 0; i < 3; i++ ) {
    std::cout << " " << xyz_global[i];
  }
  std::cout << std::endl;
  std::cout << "  Size = " << size << std::endl;
  std::cout << "  Circumference = " << circumference << std::endl;
  std::cout << "  Associated CGL track = " << AssociatedCGL << std::endl;
  std::cout << "  Associated standalone track = " << AssociatedStandalone << std::endl;
  /*
  std::cout << std:: endl << "  cov_xyz_global  =";
  for ( int i = 0; i < 3; i++ ) {
    for ( int j = 0; j < 3; j++ ) {
      std::cout << " " << cov_xyz_global[i][j];
    }
    std::cout << std::endl;
    if ( i < 2 ) std::cout << "                   ";
  }
  */
//  std::cout << "  size_xyz_global =";
//  for ( int i = 0; i < 3; i++ ) {
//    for ( int j = 0; j < 3; j++ ) {
//      std::cout << " " << size_xyz_global[i][j];
//    }
//    std::cout << std::endl;
//    if ( i < 2 ) std::cout << "                   ";
//  }
}



