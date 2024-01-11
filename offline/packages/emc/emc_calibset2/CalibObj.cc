#include <stdio.h>
#include <stream.h>
#include "CalibObj.hh"

ClassImp(CalibObj)
//------------------------------------------------------------
CalibObj::CalibObj(): TNamedDir(){
  _nch = 0;
};
//------------------------------------------------------------
CalibObj::CalibObj(const char* name, const char* title): TNamedDir(name,title){
  _nch = 0;
};
//------------------------------------------------------------
CalibObj::CalibObj(const char* name, const char* title,int nch): TNamedDir(name,title){
  _nch = nch;
};
//------------------------------------------------------------
CalibObj::~CalibObj(){
};
//------------------------------------------------------------
int CalibObj::Fill(int ch,float x,float y,float z){
  return 1;
};
//------------------------------------------------------------
void CalibObj::Reset(char* option){
  _nch = 0;
};
//------------------------------------------------------------
//
//
