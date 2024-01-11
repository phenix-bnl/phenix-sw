//-------------------------------------------------------------------------
// 
// Package: offline/packages/emc
// 
// Copyright (C) PHENIX collaboration, 2000 
//
// Implementation of class : mEmcMIPCorr3Module.C for Run2
//
// Hisayuki Torii
//-------------------------------------------------------------------------
#include "mEmcMIPCorr3Module.h"
#include "dEmcCalibTowerWrapper.h"
#include "dEmcCalibTower.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHDataNodeIterator.h"

#include "RunHeader.h"

#include <cstdlib>
#include <iostream>
#include <fstream>

using namespace std;

typedef PHIODataNode <RunHeader> RunHeaderNode_t;

mEmcMIPCorr3Module* mEmcMIPCorr3Module::_instance = NULL;
//-------------------------------------------------------------------------
mEmcMIPCorr3Module::mEmcMIPCorr3Module()
{ 
  // default ctor
  fVerbose = 0 ;
  int iarm,isect,iz,iy;
  iarm = 2;
  while( iarm-- ){
    isect = 4;
    while( isect-- ){
      _last_run[iarm][isect] = 0;
      _last_run_mip[iarm][isect] = 1.0;
      iz = 96;
      while( iz-- ){
	iy = 48;
	while( iy-- ){
	  _corr_twr_mip[iarm][isect][iz][iy] = 1;
	  _corr_twr_err[iarm][isect][iz][iy] = 0;
	  _corr_twr_stat[iarm][isect][iz][iy] = false;
	}
      }
    }
  }

  return;
}
//-------------------------------------------------------------------------
mEmcMIPCorr3Module* mEmcMIPCorr3Module::instance()
{ 
  if(! _instance )
    _instance = new mEmcMIPCorr3Module();
  return _instance;
}
//-------------------------------------------------------------------------
void mEmcMIPCorr3Module::readfile_twr(char* filename)
{ 
  int iarm,isect,iz,iy,status;
  int read_num;
  int swkey;
  float corr,corr_err;
  char tmpline[128];
  ifstream fin(filename);
  if(!fin) {
    cerr<<" mEmcMIPCorr3Module:: Can't open file: "<<filename<<endl;
    return;
  }
  cout<<" mEmcMIPCorr3Module:: Open file : "<<filename<<endl;
  while( fin.getline(tmpline,128) ){
    if( tmpline[0] != '#' ){
      read_num = sscanf(tmpline,"%d %f %f %d",&swkey,&corr,&corr_err,&status);
      if( read_num == 4 ){
	iarm = (int)( swkey / 100000 );
	isect = (int)( ( swkey - iarm * 100000 )/10000 );
	iy = (int)( ( swkey - iarm*100000 - isect*10000 )/100 );
	iz = (int)( swkey - iarm*100000 - isect*10000 - iy*100 );
	if( iarm >= 0 && iarm < 2 &&
	    isect >= 0 && isect < 4 &&
	    iy >= 0 && iy < 48 &&
	    iz >= 0 && iz < 96 ){
	  _corr_twr_mip[iarm][isect][iz][iy] = corr;
	  _corr_twr_err[iarm][isect][iz][iy] = corr_err;
	  if( status > 0 )
	    _corr_twr_stat[iarm][isect][iz][iy] = true;
	  else
	    _corr_twr_stat[iarm][isect][iz][iy] = false;
	} else {
	  cout<<" Error in mEmcMIPCorr3Module::readfile() : "<<endl;
	  cout<<"             -- Tower id is out of range : "<<filename<<endl;
	}
      } else {
      }
    }
  }
  fin.close();

  return;
}
//-------------------------------------------------------------------------
void mEmcMIPCorr3Module::readfile_run(char* filename)
{ 
  int irun,iarm,isect,status;
  int read_num;
  float corr,corr_err;
  char tmpline[128];
  ifstream fin(filename);
  if(!fin) {
    cerr<<" mEmcCIPCorr3Module:: Can't open file: "<<filename<<endl;
    return;
  }
  cout<<" mEmcMIPCorr3Module:: Open file : "<<filename<<endl;

  while( fin.getline(tmpline,128) ){
    if( tmpline[0] != '#' ){
      read_num = sscanf(tmpline,"%d %d %d %f %f %d",&irun,&iarm,&isect,&corr,&corr_err,&status);
      if( read_num == 6 ){
	if( status > 0 ){
	  _corr_run[iarm][isect].push_back(irun);
	  _corr_run_mip[iarm][isect].push_back(corr);
	  _corr_run_err[iarm][isect].push_back(corr_err);
	}
      } else {
      }
    }
  }
  fin.close();

  return;
}
//-------------------------------------------------------------------------
float mEmcMIPCorr3Module::get_corr_run(int iarm,int isect, int run){
  float corr_run;
  if( run == _last_run[iarm][isect] ){
    corr_run = _last_run_mip[iarm][isect];
  } else {
    vector<int>& vec_run = _corr_run[iarm][isect];
    vector<float>& vec_run_mip = _corr_run_mip[iarm][isect];
    //
    int n = vec_run.size();
    int min_n = 0;
    int rundiff;
    int min_rundiff = 500;
    while( n-- ){
      rundiff = abs( vec_run[n] - run );
      if( rundiff < min_rundiff ){
	min_rundiff = rundiff;
	min_n = n;
      }
    }
    if( min_rundiff < 500 ){
      _last_run[iarm][isect] = vec_run[min_n];
      _last_run_mip[iarm][isect] = vec_run_mip[min_n];
      corr_run = _last_run_mip[iarm][isect];
    } else
      corr_run = 1.0000000;
  }
  return corr_run;
  //
}
//-------------------------------------------------------------------------
void mEmcMIPCorr3Module::print()
{ 
  int iarm,isect,iz,iy,n;
  iarm = 2;
  while( iarm-- ){
    isect = 4;
    while( isect-- ){
      iz = ((iarm==1&&isect<2) ? 96 : 72 );
      while( iz-- ){
	iy = ((iarm==1&&isect<2)? 48 : 36 );
	while( iy-- ){
	  cout<<" isect, iz, iy = "<<isect<<","<<iz<<","<<iy<<"   : "<<_corr_twr_mip[iarm][isect][iz][iy]<<endl;
	}
      }
    }
  }
  iarm = 2;
  while( iarm-- ){
    isect = 4;
    while( isect-- ){
      n = _corr_run[iarm][isect].size();
      while( n-- ){
	cout<<" irun = "<<_corr_run[iarm][isect][n]<<" : "<<_corr_run_mip[iarm][isect][n]<<" +- "
	    <<_corr_run_err[iarm][isect][n]<<endl;
      }
    }
  }

  return;
}
//-------------------------------------------------------------------------
PHBoolean mEmcMIPCorr3Module::event(PHCompositeNode *root) 
{  
  int iarm,isect,iy,iz;
  float energy;
  int irow = 0 ;
  
  if( fVerbose > 0 )
    cout << "mEmcMIPCorr3Module >>>  Starting..." << endl;
  
  // Set up dEmcCalibTower
  PHNodeIterator i(root);
  PHIODataNode<PHTable>* dEmcCalibTowerNode = (PHIODataNode<PHTable>*)i.findFirst("PHIODataNode","dEmcCalibTower");
  dEmcCalibTowerWrapper * dEmcCalibTower = static_cast<dEmcCalibTowerWrapper*>(dEmcCalibTowerNode->getData());

  int irun = -9999;
  PHTypedNodeIterator<RunHeader> runiter(root);
  RunHeaderNode_t *RunHeaderNode = runiter.find("RunHeader");
  if (RunHeaderNode)
    {
      irun = RunHeaderNode->getData()->get_RunNumber();
    }
  else
    {
      cout << PHWHERE << "RunHeader Node missing" << endl;
    }
  irow = dEmcCalibTower->RowCount();
  while( irow-- ){
    iarm = dEmcCalibTower->get_arm(irow);
    isect = dEmcCalibTower->get_sector(irow);
    iz = dEmcCalibTower->get_ind(0,irow);
    iy = dEmcCalibTower->get_ind(1,irow);
    if( _corr_twr_stat[iarm][isect][iz][iy] ){
      energy = dEmcCalibTower->get_ecal(irow) * _corr_twr_mip[iarm][isect][iz][iy] * get_corr_run(iarm,isect,irun);
      dEmcCalibTower->set_ecal( irow, energy );
      
    }
  }
#ifdef SKIP_CLUSTER
  PHIODataNode<PHTable>* dEmcClusterLocalExtNode = (PHIODataNode<PHTable>*)i.findFirst("PHIODataNode","dEmcClusterLocalExt");
  dEmcClusterLocalExtWrapper * dEmcClusterLocalExt = static_cast<dEmcClusterLocalExtWrapper*>(dEmcClusterLocalExtNode->getData());

  irow = dEmcClusterLocalExt->RowCount();
  while( irow -- ){
    iarm = dEmcClusterLocalExt->get_arm(irow);
    isect = dEmcClusterLocalExt->get_sector(irow);
    iz = dEmcClusterLocalExt->get_ind(0,irow);
    iy = dEmcClusterLocalExt->get_ind(1,irow);
    if( _corr_twr_stat[isect][iz][iy] ){
      dEmcClusterLocalExt->set_e( irow, dEmcClusterLocalExt->get_e(irow) * _corr_twr_mip[iarm][isect][iz][iy] );
      dEmcClusterLocalExt->set_ecore( irow, dEmcClusterLocalExt->get_ecore(irow) * _corr_twr_mip[iarm][isect][iz][iy] );
      dEmcClusterLocalExt->set_ecorr( irow, dEmcClusterLocalExt->get_ecorr(irow) * _corr_twr_mip[iarm][isect][iz][iy] );
      dEmcClusterLocalExt->set_ecent( irow, dEmcClusterLocalExt->get_ecent(irow) * _corr_twr_mip[iarm][isect][iz][iy] );
    }
  }
#endif
  return true;
  
}
//-------------------------------------------------------------------------
