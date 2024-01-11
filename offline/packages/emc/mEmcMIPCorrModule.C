//-------------------------------------------------------------------------
// 
// Package: offline/packages/emc
// 
// Copyright (C) PHENIX collaboration, 2000 
//
// Implementation of class : mEmcMIPCorrModule.h
//
// Hisayuki Torii
//-------------------------------------------------------------------------
#include "mEmcMIPCorrModule.h"
#include "dEmcCalibTower.h"
#include "dEmcCalibTowerWrapper.h"
#include "PHNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHDataNodeIterator.h"

#include <iostream>
#include <fstream>

using namespace std;

mEmcMIPCorrModule* mEmcMIPCorrModule::_instance = NULL;
//-------------------------------------------------------------------------
mEmcMIPCorrModule::mEmcMIPCorrModule()
{ 
  // default ctor
  fVerbose = 0 ;
  int isect,iz,iy;
  isect = 8;
  while( isect-- ){
    iz = 96;
    while( iz-- ){
      iy = 48;
      while( iy-- ){
	_corrfact[isect][iz][iy] = 1;
	_corrfact_err[isect][iz][iy] = 0;
	_corrfact_stat[isect][iz][iy] = 0;
      }
    }
  }
}
//-------------------------------------------------------------------------
mEmcMIPCorrModule* mEmcMIPCorrModule::instance(char* filename)
{ 
  if(! _instance )
    _instance = new mEmcMIPCorrModule();
  _instance->readfile(filename);
  return _instance;
}
//-------------------------------------------------------------------------
mEmcMIPCorrModule* mEmcMIPCorrModule::instance()
{ 
  if(! _instance )
    _instance = new mEmcMIPCorrModule();
  return _instance;
}
//-------------------------------------------------------------------------
void mEmcMIPCorrModule::readfile(char* filename)
{ 
  int isect,ismz,ismy,iz,iy,status,towerid;
  float correction,error_correction;
  char tmpline[128];
  char tmpc;
  ifstream fin(filename);
  if(!fin) {
    cerr<<" mEmcCIPcorrModule:: Can't open file: "<<filename<<endl;
    return;
  }
  fin.getline(tmpline,128);
  sscanf(tmpline,"#Sector %d MIP Correction table",&isect);
  if( isect < 0 || isect >= 8 ) {
    cerr<<" Error:: mEmcMIPCorrModule can't open the file "<<filename<<endl;
    return;
  }
  cout<<" mEmcMIPCorrModule read correction table of sector "<<isect<<endl;
  while( fin.get(tmpc) && tmpc != '\n' ); // Skip 1 line
  while( fin.get(tmpc) && tmpc != '\n' ); // Skip 1 line

  while( fin>>ismz>>ismy>>iz>>iy>>towerid>>correction>>error_correction>>status ){
    int itmpz = (isect>=6 ? 6*ismz+iz : 12*ismz+iz );
    int itmpy = (isect>=6 ? 4*ismy+iy : 12*ismy+iy );
    _corrfact[isect][itmpz][itmpy] = correction;
    _corrfact_err[isect][itmpz][itmpy] = error_correction;
    _corrfact_stat[isect][itmpz][itmpy] = status;
  }
  return;
}
//-------------------------------------------------------------------------
void mEmcMIPCorrModule::print()
{ 
  int isect,ismz,ismy,iz,iy;
  isect = 8;
  while( isect-- ){
    ismz = (isect>=6 ? 16 : 6 );
    while( ismz-- ){
      ismy = (isect>=6 ? 12 : 3 );
      while( ismy-- ){
	iz = (isect>=6 ? 4*ismz : 12*ismz );
	iy = (isect>=6 ? 4*ismy : 12*ismy );
	cout<<" isect, iz, iy = "<<isect<<","<<iz<<","<<iy<<"   : "<<_corrfact[isect][iz][iy]<<endl;
      }
    }
  }
  return;
}
//-------------------------------------------------------------------------
PHBoolean mEmcMIPCorrModule::event(PHCompositeNode *root) 
{  
  int arm,sect,yrow,zrow;
  int irow = 0 ;
  
  if( fVerbose > 0 )
    cout << "mEmcMIPCorrModule >>>  Starting..." << endl;
  
  // Set up dEmcCalibTower
  PHNodeIterator i(root);
  PHIODataNode<PHTable>* dEmcCalibTowerNode = (PHIODataNode<PHTable>*)i.findFirst("PHIODataNode","dEmcCalibTower");
  dEmcCalibTowerWrapper * dEmcCalibTower = static_cast<dEmcCalibTowerWrapper*>(dEmcCalibTowerNode->getData());

  irow = dEmcCalibTower->RowCount();
  while( irow -- ){
    arm = dEmcCalibTower->get_arm(irow);
    sect = dEmcCalibTower->get_sector(irow);
    zrow = dEmcCalibTower->get_ind(0,irow);
    yrow = dEmcCalibTower->get_ind(1,irow);
    if( arm == 1 )
      {
      if( sect == 0 || sect == 1 ) // PbGl E0,E1
	sect = sect + 6;
      else
	sect = sect + 4;           // PbSc E2,E3
      }
    // Apply MIP correction
    if( _corrfact_stat[sect][zrow][yrow] ){
      dEmcCalibTower->set_ecal( irow,  dEmcCalibTower->get_ecal(irow) * _corrfact[sect][zrow][yrow] );
    }
    //
  }
#ifdef SKIP_CLUSTER
  irow = dEmcClusterLocalExt->RowCount();
  while( irow -- ){
    arm = dEmcClusterLocalExt->get_arm(irow);
    sect = dEmcClusterLocalExt->get_sector(irow);
    zrow = dEmcClusterLocalExt->get_ind(0,irow);
    yrow = dEmcClusterLocalExt->get_ind(1,irow);
    if( arm == 1 )
      if( sect == 0 || sect == 1 ) // PbGl E0,E1
	sect = sect + 6;
      else
	sect = sect + 4;           // PbSc E2,E3
    // Apply MIP correction
    if( _corrfact_stat[sect][zrow][yrow] ){
      dEmcClusterLocalExt->set_e( irow, dEmcClusterLocalExt->get_e(irow) * _corrfact[sect][zrow][yrow] );
      dEmcClusterLocalExt->set_ecore( irow, dEmcClusterLocalExt->get_ecore(irow) * _corrfact[sect][zrow][yrow] );
      dEmcClusterLocalExt->set_ecorr( irow, dEmcClusterLocalExt->get_ecorr(irow) * _corrfact[sect][zrow][yrow] );
      dEmcClusterLocalExt->set_ecent( irow, dEmcClusterLocalExt->get_ecent(irow) * _corrfact[sect][zrow][yrow] );
    }
    //
  }
#endif
  return true;
  
}
//-------------------------------------------------------------------------
