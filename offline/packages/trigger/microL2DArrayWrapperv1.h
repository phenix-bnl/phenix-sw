////////////////////////
//microL2DArrayWrapper:
//    
//    
//    
//    structure to hold UINT array, so it can inherit 
//    from TObject and therefore be put in a TClonesArray
//    
//    
//    may replace with a TArrayI
//
//
//////////////////////////

#ifndef __MICROL2DECISIONARRAYWrapperv1_H
#define __MICROL2DECISIONARRAYWrapperv1_H

#include <PHObject.h> 
#include <phenixTypes.h>

static const unsigned short int  MICROL2MAXNUMALGORITHMSV1 = 64;

class microL2DArrayWrapperv1: public TObject 
{

 public:   

  microL2DArrayWrapperv1();
  microL2DArrayWrapperv1(microL2DArrayWrapperv1 *wrapper);
  virtual ~microL2DArrayWrapperv1() {}

  void set_theArrayVal(short int nalg, UINT ival) {theArray[nalg] = ival;}
  UINT get_theArrayVal(short int nalg) {return theArray[nalg];}

 protected:
  UINT theArray[MICROL2MAXNUMALGORITHMSV1 + 1]; // +1 for ordering insurance l1 ind

  ClassDef(microL2DArrayWrapperv1,1)

};

#endif



