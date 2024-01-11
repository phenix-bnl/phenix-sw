#include "MpcExSpin.h"
#include "recoConsts.h"
#include "SpinDBContent.hh"
#include "SpinDBOutput.hh"
#include "phool.h"
#include <iostream>
#include <cstdio>

using namespace std;

MpcExSpin* MpcExSpin::_instance = NULL;

MpcExSpin* MpcExSpin::instance() {
  if(_instance == NULL){
    _instance = new MpcExSpin();
  }
  return _instance;
}

MpcExSpin::MpcExSpin() {

  InitRun(); 
}

MpcExSpin::~MpcExSpin(){
}

void MpcExSpin::InitRun() {

  recoConsts* rc = recoConsts::instance();

  //initialize content
  _cross_shift = 5;
  _fillnumber = 10000;
  _qalevel = 1;

  memset(_bpol,0,sizeof(_bpol)); 
  memset(_bpolerr,0,sizeof(_bpolerr)); 
  memset(_bpolsys,0,sizeof(_bpolsys)); 
  memset(_ypol,0,sizeof(_ypol)); 
  memset(_ypolerr,0,sizeof(_ypolerr)); 
  memset(_ypolsys,0,sizeof(_ypolsys)); 
  memset(_bpat,0,sizeof(_bpat)); 
  memset(_ypat,0,sizeof(_ypat)); 
  memset(_pattern,0,sizeof(_pattern)); 
  memset(_scaler_bbc_vtxcut,0,sizeof(_scaler_bbc_vtxcut)); 
  memset(_scaler_bbc_nocut,0,sizeof(_scaler_bbc_nocut)); 
  memset(_scaler_zdc_wide,0,sizeof(_scaler_zdc_wide)); 
  memset(_scaler_zdc_narrow,0,sizeof(_scaler_zdc_narrow)); 

  // Get runnumber - bail if it can't be found!
  _runnumber = rc->get_IntFlag("RUNNUMBER");
  if(_runnumber == 0) {
    cout<<PHWHERE<<"runnumber  is not set in recoConsts !!!"<<endl;
    cout<<PHWHERE<<"The instance of this object is not useable."<<endl;
    return;
  }  
  SpinDBOutput spin_out;
  spin_out.StoreDBContent(_runnumber,_runnumber);
  if(spin_out.CheckRunRowStore(_runnumber)!=1){
    cout<<"read from data base failed !!!"<<endl;
    cout<<"The instance of this object is not useable."<<endl;
    return;
  }

  SpinDBContent spin_cont; 
  spin_out.GetDBContentStore(spin_cont,_runnumber);
  
  _cross_shift = spin_cont.GetCrossingShift();
  _qalevel = spin_cont.GetQALevel();
  _fillnumber = spin_cont.GetFillNumber();
  for(int i = 0;i < NCROSS;i++){
    spin_cont.GetPolarizationBlue(i,_bpol[i],_bpolerr[i],_bpolsys[i]);
    spin_cont.GetPolarizationYellow(i,_ypol[i],_ypolerr[i],_ypolsys[i]);
    _bpat[i] = spin_cont.GetSpinPatternBlue(i);
    _ypat[i] = spin_cont.GetSpinPatternYellow(i);
    _scaler_bbc_vtxcut[i] = spin_cont.GetScalerBbcVertexCut(i);
    _scaler_bbc_nocut[i] = spin_cont.GetScalerBbcNoCut(i);
    _scaler_zdc_wide[i] = spin_cont.GetScalerZdcWide(i);
    _scaler_zdc_narrow[i] = spin_cont.GetScalerZdcNarrow(i);

    //set pattern
     if(_bpat[i] == 1 && _ypat[i] == 1) {
        _pattern[i]=0;
     } else if( _bpat[i] == -1 && _ypat[i] == 1) { 
        _pattern[i]=1;
     } else if( _bpat[i] ==  1 && _ypat[i] ==-1) { 
        _pattern[i]=2;
     } else if( _bpat[i] == -1 && _ypat[i] ==-1) { 
       _pattern[i]=3;
     } else {                     
       _pattern[i]=4;
     }
  }

}

void MpcExSpin::Print() const {
  printf("Run number = %d\n",_runnumber);
  printf("QA Level = %d\n",_qalevel);
  printf("Fill number = %d\n",_fillnumber);
  printf("Crossing shift = %d\n",_cross_shift);
  
  for(int i=0; i<NCROSS; i++){
    printf("%3d : %12lld %12lld %12lld %12lld : %3d %3d ",i,
    _scaler_bbc_vtxcut[i],_scaler_bbc_nocut[i],
    _scaler_zdc_wide[i],_scaler_zdc_narrow[i],
    _bpat[i],_ypat[i]);
    printf(" : %6.3f +- %6.3f +- %6.3f %6.3f +- %6.3f +- %6.3f\n",
    _bpol[i],_bpolerr[i],_bpolsys[i],_ypol[i],_ypolerr[i],_ypolsys[i]);
  }
}

