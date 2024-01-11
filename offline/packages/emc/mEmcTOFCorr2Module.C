//-------------------------------------------------------------------------
// 
// Package: offline/packages/emc
// 
// Copyright (C) PHENIX collaboration, 2000 
//
// Implementation of class : mEmcTOFCorr2Module
//
// Ken Oyama, CNS, University of Tokyo.
//          Modified by Hisayuki Torii Dec 2000
//          Modified for V05 production by H.Torii Aug/15/2001
//-------------------------------------------------------------------------
#include "mEmcTOFCorr2Module.h"
#include "emcCalibrator.h"
#include "emcRawDataCalibrator.h"
#include "emcRawDataAccessor.h"
#include "emcDataManager.h"
#include "emcPedestals.h"
#include "emcDBMS.h"

#include "dEmcCalibTowerWrapper.h"
#include "dEmcClusterLocalExtWrapper.h"
#include "RunHeader.h"

#include "Event.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"

#include "TFile.h"
#include "TH1.h"
#include "TROOT.h"

#include <iostream>
#include <fstream>
using namespace std;

typedef PHIODataNode <RunHeader> RunHeaderNode_t;

mEmcTOFCorr2Module* mEmcTOFCorr2Module::_instance = NULL;

//-------------------------------------------------------------------------
mEmcTOFCorr2Module::mEmcTOFCorr2Module()
{ 
  // Default Constructor
  fVerbose = 0;

  // Initialize run-by-run correction table to have no data.
  _pbsc_nruns = 0;
  _pbgl_nruns = 0;

  // Initialize tower by-tower tables with default value.
  for( int arm=0; arm<2; arm++ )
    for( int sec=0; sec<4; sec++ )
      for( int y=0; y<48; y++ )
        for( int z=0; z<96; z++ ){
	  _tbt_corr[arm][sec][y][z] = MEMCTOFCORR_DEF_TBT_CORR;
	  _tbt_lc[arm][sec][y][z] = 1.;

	}

  _pbsc_nruns = 0;
  _pbgl_nruns = 0;

  int irun;
  irun = MEMCTOFCORR_N_MAX_RUNS;
  while( irun-- ){
    _pbsc_runs[irun] = 0;
    _pbgl_runs[irun] = 0;
    _pbsc_corr[irun] = MEMCTOFCORR_DEF_RBR_CORR;
    _pbgl_corr[irun] = MEMCTOFCORR_DEF_RBR_CORR;
  }


}

//-------------------------------------------------------------------------
mEmcTOFCorr2Module* mEmcTOFCorr2Module::instance( char *pbsc_file, char *pbgl_file, char *tbt_file )
{ 
  if( _instance )
    delete _instance;
  _instance = new mEmcTOFCorr2Module();

  _instance->read_rbrsc_file( pbsc_file);   // Read PbSc run-by-run correction file.
  _instance->read_rbrgl_file( pbgl_file);   // Read PbGl run-by-run correction file.
  _instance->read_tbt_file( tbt_file  );    // Read tower-by-tower correction file.

  return _instance;
}

//-------------------------------------------------------------------------
mEmcTOFCorr2Module* mEmcTOFCorr2Module::instance()
{ 
  if( _instance )
    delete _instance;
  _instance = new mEmcTOFCorr2Module();
  return _instance;
}

//-------------------------------------------------------------------------
void mEmcTOFCorr2Module::read_t0_file( char *pbsc_file, char *pbgl_file, char *tbt_file )
{ 
  read_rbrsc_file( pbsc_file);   // Read PbSc run-by-run correction file.
  read_rbrgl_file( pbgl_file);   // Read PbGl run-by-run correction file.
  read_tbt_file( tbt_file  );    // Read tower-by-tower correction file.
  return;
};

//-------------------------------------------------------------------------
//
// It reads run-by-run correction data table. Called by instance.
//
void mEmcTOFCorr2Module::read_rbrsc_file( char *file)
{
  int suspicious_line = 0;
  ifstream fin( file );
  int nruns = 0;
  printf("mEmcTOFCorr2Module::read_rbrsc_file: Opening file <%s>\n", file);
  ifstream cfile( file );
  if( cfile == 0 ) {
    printf("mEmcTOFCorr2Module::read_rbrsc_file: Error: "
	   "Could not open file <%s>. Do not apply correction.\n", file);
    return;
  }
  while( cfile ){
    int run;
    float corr;
    float corr_err;
    int itmp,flag;
    cfile >> run >> corr >> corr_err >> itmp >> flag ;

    if( flag == 0 ){
      suspicious_line++;
      continue;
    }
    _pbsc_runs[ nruns ] = run;
    _pbsc_corr[ nruns ] = corr;
    nruns++;
    if( nruns >= MEMCTOFCORR_N_MAX_RUNS ) {
      printf("mEmcTOFCorr2Module::read_rbrsc_file: \n"
	     "                                  Too many lines.\n"
	     "                                  Probably you increase the maximum run numbers and re-compile me.\n"
	     "                                  Following lines will be ignored.\n");
      
      break;
    }
  }

  printf("mEmcTOFCorr2Module::read_rbrsc_file: %d lines have been read.\n", nruns);
  cfile.close();
  _pbsc_nruns = nruns;
}

//-------------------------------------------------------------------------
//
// It reads run-by-run correction data table. Called by instance.
//
void mEmcTOFCorr2Module::read_rbrgl_file( char *file)
{
  int suspicious_line = 0;
  ifstream fin( file );
  int nruns = 0;
  printf("mEmcTOFCorr2Module::read_rbrgl_file: Opening file <%s>\n", file);
  ifstream cfile( file );
  if( cfile == 0 ) {
    printf("mEmcTOFCorr2Module::read_rbrgl_file: Error: "
	   "Could not open file <%s>. Do not apply correction.\n", file);
    return;
  }
  while( cfile ){
    int run;
    float corr;
    float corr_err;
    int itmp,flag;
    cfile >> run >> corr >> corr_err >> itmp >> flag ;

    if( flag == 0 ){
      suspicious_line ++;
      continue;
    }
    _pbgl_runs[ nruns ] = run;
    _pbgl_corr[ nruns ] = corr;
    nruns++;
    if( nruns >= MEMCTOFCORR_N_MAX_RUNS ) {
      printf("mEmcTOFCorr2Module::read_rbrgl_file: \n"
	     "                                  Too many lines.\n"
	     "                                  Probably you increase the maximum run numbers and re-compile me.\n"
	     "                                  Following lines will be ignored.\n");
      
      break;
    }
  }
  printf("mEmcTOFCorr2Module::read_rbrgl_file: %d lines have been read.\n", nruns);
  cfile.close();
  _pbgl_nruns = nruns;
}


//-------------------------------------------------------------------------
void mEmcTOFCorr2Module::read_tbt_file( char *file )
//
// It reads tower-by-tower correction data table. Called by instance.
//
{
  int line = 0;
  int reject = 0;
  printf("mEmcTOFCorr2Module::read_tbt_file: Opening Tower-by-Tower TOF Correction file <%s>\n", file);
  ifstream cfile( file );
  if( cfile == 0 ) {
    printf("mEmcTOFCorr2Module::read_tbt_file: Error: Failed to open file. "
	   "Default value = %f is used for all tower.\n", MEMCTOFCORR_DEF_TBT_CORR );
    return;
  }
  while( cfile ){
    int y,z,s,a;
    float corr, reso;
    float lc,lc_err;
    int lc_flag;
    int flag;
    line++;
    cfile >> y >> z >> s >> a >> corr >> reso;
    cfile >> lc >> lc_err >> lc_flag >> flag;
    if( (a<0 || a>1) || (s<0 || s>3) || (y<0 || y>47) || (z<0 || z>95) ) {
      printf("mEmcTOFCorr2Module::read_tbt_file: Strange ARM/SEC/Y/Z number at line %d.\n", line );
      reject++;
    } else if (   !((a == 1) && (s < 2)) && ( (y>35) || (z>71) )  ) {
      printf("mEmcTOFCorr2Module::read_tbt_file: Hmm. it is PbSc but indexes are strange.... at line %d.\n", line );
      reject++;
    } else {
      _tbt_corr[a][s][y][z] = corr;
      _tbt_lc[a][s][y][z] = lc;
    }
  }
  printf("mEmcTOFCorr2Module::read_tbt_file: %d lines have been read. (%d lines were rejected.)\n", line, reject );
  cfile.close();
}

//-------------------------------------------------------------------------
//
// Find run number/index in the run-by-run table and return total correction includes tower-by-tower correction.
//
float mEmcTOFCorr2Module::get_correction( int run, int arm, int sec, int y, int z )
{
  static int   prev_run = -1;
  static float rbr_corr_pbsc = MEMCTOFCORR_DEF_RBR_CORR;
  static float rbr_corr_pbgl = MEMCTOFCORR_DEF_RBR_CORR;
  // If the run changed, re-fill the prev_rbr_corr_pb??.
  if( run != prev_run ) {
    printf("mEmcTOFCorr2Module::get_correction: New Run : %d, finding the correction table. \n", run);
    int i;
    for( i = 0 ; i < _pbsc_nruns-1 ; i++) {
      if( _pbsc_runs[i] <= run && _pbsc_runs[i+1] > run ) {
	rbr_corr_pbsc = _pbsc_corr[i];
	break;
      }
    }
    if( i == (_pbsc_nruns - 1) ) rbr_corr_pbsc = _pbsc_corr[i];  // If over run.
    for( i = 0 ; i < _pbgl_nruns-1 ; i++) {
      if( _pbgl_runs[i] <= run && _pbgl_runs[i+1] > run ) {
	rbr_corr_pbgl = _pbgl_corr[i];
	break;
      }
    }
    if( i == (_pbgl_nruns - 1) ) rbr_corr_pbgl = _pbgl_corr[i];  // If over run.
    prev_run = run;
    printf("mEmcTOFCorr2Module::get_correction:   New Values : PbSc: %f, PbGl: %f\n",
	   rbr_corr_pbsc, rbr_corr_pbgl);
  }
  if( !((arm == 1) && (sec < 2)) ) { // PbSc
    return rbr_corr_pbsc + _tbt_corr[arm][sec][y][z];
  } else {
    return rbr_corr_pbgl + _tbt_corr[arm][sec][y][z];
  }

}

//-------------------------------------------------------------------------
//void mEmcTOFCorr2Module::apply_tdcped(int runNum,char* dbfilename,PHCompositeNode* topNode,mEmcCalibratorModule* mEmcCalibrator)
PHBoolean mEmcTOFCorr2Module::apply_tdcped(int runNum,PHCompositeNode* topNode)
{
  bool status;
  char hname[128];
  static emcPedestals pedDriver ;

  if( fVerbose > 0 ) cout<<" mEmcTOFCorr2Module:: open db file "<<tdcped_file.Data()<<endl;
  TFile* file_pedDB = new TFile(tdcped_file.Data());
  file_pedDB->cd();
  sprintf(hname,"h_avetwr%d",runNum);
  TH1F* h1 = (TH1F*)gROOT->FindObject(hname);
  if( h1 !=  0 ) {
    if( fVerbose > 0 ) cout<<" mEmcTOFCorr2Module:: ... Found DB for run "<<runNum<<endl;
  } else {
    if( fVerbose > 0 ) cout<<" mEmcTOFCorr2Module:: ... Can't find pedestal DB for "<<runNum<<endl;
    int irun = runNum;
    while( irun-- > runNum - 1000 && h1 == 0 ){
      sprintf(hname,"h_avetwr%d",irun);
      h1 = (TH1F*)gROOT->FindObject(hname);
      if( h1 != 0 ){
	if( fVerbose > 0 ) cout<<"                        ... Found nearest run "<< irun <<endl;
      }
    }
  }
  //
  if( h1 != 0 ){
    PHNodeIterator i(topNode);
    PHDataNode<Event> * thisEventNode = 
      (PHDataNode<Event>*) (i.findFirst("PHDataNode","PRDF")) ;
    PHTimeStamp* when;
    when =  new PHTimeStamp(thisEventNode->getData()->getTime());
    emcPedestals* pdo = 0 ; 
    emcDataManager* dm = emcDataManager::GetInstance() ;
    pedDriver.SetSource(emcDBMS::get()) ;
    pdo = dynamic_cast<emcPedestals*>( dm->Collect(pedDriver, *when) ) ;
    //
    //-----------------------------------This is new part for v05 code..
    emcRawDataAccessor* fRda = emcRawDataAccessor::GetInstance();
    emcRawDataObject* fRdo = fRda->GetRawDataObject();
    int index;
    int softkey;
    int ibin,iarm,isect,itemp,iy,iz,itwr,iamutac;
    float tdc; //,tdc_err;
    for (index = 0; index < fRdo->GetSize(); index++ ) {
      if( index % 1000 == 0 ) {
	if( fVerbose > 0 ) cout<<" mEmcTOFCorr2Module ::... looping "<<index<<" / "<<fRdo->GetSize()<<" towers "<<endl;
      }
      softkey = fRdo->GetSoftwareKey(index) ;
      iarm = softkey / 100000 ;
      isect = (softkey - iarm * 100000) / 10000 ;
      itemp = softkey - iarm * 100000 - isect * 10000 ;
      iy = itemp / 100 ;
      iz = itemp - iy * 100 ;
      isect = isect + iarm * 6;
      itwr = (isect>=6 ?  15552+4608*(isect-6)+96*iy+iz : 2592*isect+72*iy+iz );
      ibin = h1->FindBin( itwr );
      tdc = h1->GetBinContent( ibin );
      //      tdc_err =  h1->GetBinError( ibin );
      iamutac = 64;
      while( iamutac-- ){
	ibin = 10;
	while( ibin-- )
	  pdo->updateValue(index, iamutac, tdc, "TAC");
      }
    }
    status = true;
    cout<<" mEmcTOFCorr2Module :: initialization completed."<<endl;
  } else {
    cout<<" mEmcTOFCorr2Module :: .. skip initialization "<<endl;
    status = false;
  }
  //=================================================== TDC Pedestal update END
  //
  return status;

};
//-------------------------------------------------------------------------
PHBoolean mEmcTOFCorr2Module::eventFirst(PHCompositeNode *root) 
{   
  if( fVerbose > 0 )
    cout<<"mEmcTOFCorr2Module >>>  Starting..." << endl;

  PHNodeIterator i(root);  

  int run = -9999;
  PHTypedNodeIterator<RunHeader> runiter(root);
  RunHeaderNode_t *RunHeaderNode = runiter.find("RunHeader");
  if (RunHeaderNode)
    {
      run = RunHeaderNode->getData()->get_RunNumber();
    }
  else
    {
      cout << PHWHERE << "RunHeader Node missing" << endl;
    }


  if( fVerbose > 0 )
    cout<<"mEmcTOFCorr2Module .......... finding run "<<run<<endl;
  bool status = apply_tdcped(run,root);
  return status;

};
//-------------------------------------------------------------------------
PHBoolean mEmcTOFCorr2Module::event(PHCompositeNode *root) 
{  
  int arm, sec, y, z;
  float tof;
  
  if( fVerbose > 0 )
    cout << "mEmcTOFCorr2Module >>>  Starting..." << endl;

  PHNodeIterator i(root);  

  int run = -9999;
  PHTypedNodeIterator<RunHeader> runiter(root);
  RunHeaderNode_t *RunHeaderNode = runiter.find("RunHeader");
  if (RunHeaderNode)
    {
      run = RunHeaderNode->getData()->get_RunNumber();
    }
  else
    {
      cout << PHWHERE << "RunHeader Node missing" << endl;
    }

  // === Patch dEmcCalibTower ===

  PHIODataNode<PHTable>* dEmcCalibTowerNode = (PHIODataNode<PHTable>*)i.findFirst("PHIODataNode","dEmcCalibTower");
  dEmcCalibTowerWrapper * dEmcCalibTower = static_cast<dEmcCalibTowerWrapper*>(dEmcCalibTowerNode->getData());
    
  for( size_t j = 0 ; j < dEmcCalibTower->RowCount(); j++) {
    arm = dEmcCalibTower->get_arm(j);
    sec = dEmcCalibTower->get_sector(j);
    y   = dEmcCalibTower->get_ind(1,j);
    z   = dEmcCalibTower->get_ind(0,j);
    tof = dEmcCalibTower->get_tof(j);
    dEmcCalibTower->set_tof( j , tof * _tbt_lc[arm][sec][y][z] - get_correction( run, arm, sec, y, z ) + 30);
  }
  
  // === Patch dEmcClusterLocalExt ===
  
  PHIODataNode<PHTable>* dEmcClusterLocalExtNode = (PHIODataNode<PHTable>*)i.findFirst("PHIODataNode","dEmcClusterLocalExt");
  dEmcClusterLocalExtWrapper * dEmcClusterLocalExt = static_cast<dEmcClusterLocalExtWrapper*>(dEmcClusterLocalExtNode->getData());
  
  for( size_t j = 0 ; j < dEmcClusterLocalExt->RowCount(); j++) {
    arm = dEmcClusterLocalExt->get_arm(j);
    sec = dEmcClusterLocalExt->get_sector(j);
    y   = dEmcClusterLocalExt->get_ind(1,j);
    z   = dEmcClusterLocalExt->get_ind(0,j);
    tof = dEmcClusterLocalExt->get_tof(j);
    dEmcClusterLocalExt->set_tofcorr( j , tof * _tbt_lc[arm][sec][y][z] - get_correction( run, arm, sec, y, z ) + 30) ;
  }

  return true;
  
}

// EOF
