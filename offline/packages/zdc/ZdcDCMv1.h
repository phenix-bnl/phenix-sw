/*
  implementation of Zdc DCM class version 1
*/
#ifndef ZDCDCMV1_H
#define ZDCDCMV1_H

#include <iostream>
#include "ZdcDCM.h"
#include "ZdcReturncodes.h"

class ZdcDCMv1 : public ZdcDCM //++CINT
{

 public:
  ZdcDCMv1();
  virtual ~ZdcDCMv1() { }

   long get_nWord(void) const {return nWord;}
   long get_scheme(void) const {return scheme;}
   long get_packetID(void) const {return packetID;}
   long get_DCM(short iword) const {return DCM[iword];}
   long get_adc(int ch) const {return DCM[ 7 + ch*3 ];}
   long get_tdc0(int ch) const {return DCM[ 7 + ch*3 + 1 ];}
   long get_tdc1(int ch) const {return DCM[ 7 + ch*3 + 2 ];}


   short set_nWord(long ival) {nWord=ival; return SUCCESS_SHORT;}
   short set_scheme(long ival) {scheme=ival; return SUCCESS_SHORT;}
   short set_packetID(long ival) {packetID=ival; return SUCCESS_SHORT;}
   short set_DCM(short iword, long ival);
   short set_DCM(long *ival);
  void identify(std::ostream& os = std::cout) const;

   void Reset();
  
 protected:
   long nWord;
   long scheme;
   long packetID;
   long DCM[33];

   ClassDef(ZdcDCMv1,1)

};


#endif


