#include <stdio.h>
#include <stream.h>
#include "CalibRunsTH2.hh"


ClassImp(CalibRunsTH2)
  
//------------------------------------------------------------
CalibRunsTH2::CalibRunsTH2(): CalibRuns(){
  int n = 2;
  while( n-- ){
    _nbins[n] = 10;
    _low[n] = 0;
    _high[n] = 1;
  }
  SetBuffersize(0);
  return;
}
//------------------------------------------------------------
CalibRunsTH2::CalibRunsTH2(const char* name, const char* title,int nch,
				 int xnbins,float xlow,float xhigh,int ynbins,float ylow,float yhigh): CalibRuns(name,title,nch){
  _nbins[0] = xnbins;
  _low[0] = xlow;
  _high[0] = xhigh;
  _nbins[1] = ynbins;
  _low[1] = ylow;
  _high[1] = yhigh;
  SetBuffersize(0);
  return;
};
//------------------------------------------------------------
CalibRunsTH2::~CalibRunsTH2(){
  SetBuffersize(0);
  return;
};
//------------------------------------------------------------
int CalibRunsTH2::Initialize(){
  char hname[256],htitle[256];
  sprintf(hname,"%s_%d_%d",GetName(),_run,_seq);
  sprintf(htitle,"%s run %d:%d",GetTitle(),_run,_seq);
  if(! _initialized ){
    _calibobj = (CalibObj*) new TH2Xch(hname,htitle,_nch,_nbins[0],_low[0],_high[0],_nbins[1],_low[1],_high[1]);
    _initialized = 1;
  }
  return _initialized;
};
//------------------------------------------------------------
//
//

