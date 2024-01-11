// ====================
// FILE: SvxClusterv2.C
// ====================

#include "SvxClusterv2.h"

// ***********************************************************************
// Implementation of Silicon cluster (compact version)
// ---
// Created  by Sasha Lebedev <lebedev@iastate.edu> in July 2010
//
// ***********************************************************************

ClassImp(SvxClusterv2)

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxClusterv2::SvxClusterv2(SvxClusterList* lst, SvxCluster* cluster)
  : SvxCluster(cluster)
{
  clusterList = lst;
  if ( cluster ) {
    sensorType = (short) cluster->get_sensorType() ;
    edgeflag   = (short) cluster->get_edgeflag()   ;
    for ( int i = 0; i < 2; i++ ) {
      adc[i] = cluster->get_adc(i);
    }
    for ( int i = 0; i < 3; i++ ) {
      xyz_local [i] = cluster->get_xyz_local (i);
      xyz_global[i] = cluster->get_xyz_global(i);
//      for ( int j = 0; j < 3; j++ ) {
//	size_xyz_local [i][j] = cluster->get_size_xyz_local (i,j);
//	size_xyz_global[i][j] = cluster->get_size_xyz_global(i,j);
//      }
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
//      for ( int j = 0; j < 3; j++ ) {
//	size_xyz_local [i][j] = -.9999e4;
//	size_xyz_global[i][j] = -.9999e4;
//      }
    }
  }
  //std::cout << "SvxClusterv2 object created" << std::endl;
}

SvxClusterv2::SvxClusterv2(SvxCluster* cluster) : SvxCluster(cluster)
{
  clusterList = 0;
  if ( cluster ) {
    sensorType = (short) cluster->get_sensorType() ;
    edgeflag   = (short) cluster->get_edgeflag()   ;
    for ( int i = 0; i < 2; i++ ) {
      adc[i] = cluster->get_adc(i);
    }
    for ( int i = 0; i < 3; i++ ) {
      xyz_local [i] = cluster->get_xyz_local (i);
      xyz_global[i] = cluster->get_xyz_global(i);
//      for ( int j = 0; j < 3; j++ ) {
//	size_xyz_local [i][j] = cluster->get_size_xyz_local (i,j);
//	size_xyz_global[i][j] = cluster->get_size_xyz_global(i,j);
//      }
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
//      for ( int j = 0; j < 3; j++ ) {
//	size_xyz_local [i][j] = -.9999e4;
//	size_xyz_global[i][j] = -.9999e4;
//      }
    }
  }
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxClusterv2::Reset()
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
}

void SvxClusterv2::identify(std::ostream& os) const
{
  os << "SvxClusterv2 object: hitID = "
     << hitID << std::endl;
}

// Methods
// """""""
void SvxClusterv2::Copy(SvxHit* hit) 
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
}

void SvxClusterv2::print() const
{
  std::cout << "SvxClusterv2 derived from ";
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



