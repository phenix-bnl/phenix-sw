#include <stdio.h>
#include <stream.h>
#include "CalibRunsTH0.hh"

ClassImp(CalibRunsTH0)
  
//------------------------------------------------------------
CalibRunsTH0::CalibRunsTH0(): CalibRuns(){
  SetBuffersize(0);
  return;
}
//------------------------------------------------------------
CalibRunsTH0::CalibRunsTH0(const char* name, const char* title,int nch) : CalibRuns(name,title,nch){
  SetBuffersize(0);
  return;
};
//------------------------------------------------------------
CalibRunsTH0::~CalibRunsTH0(){
  SetBuffersize(0);
  return;
};
//------------------------------------------------------------
int CalibRunsTH0::Initialize(){
  char hname[256],htitle[256];
  sprintf(hname,"%s_%d_%d",GetName(),_run,_seq);
  sprintf(htitle,"%s run %d:%d",GetTitle(),_run,_seq);
  if(! _initialized ){
    _calibobj = (CalibObj*) new TH0Xch(hname,htitle,_nch);
    _initialized = 1;
  }
  return _initialized;
};
//------------------------------------------------------------
//
//

