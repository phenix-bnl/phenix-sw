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

#ifndef MICROL2DECISIONARRAYWRAPPER_H
#define MICROL2DECISIONARRAYWRAPPER_H

#include <PHObject.h> 
#include <phenixTypes.h>

#define MicroL2MaxNumLvl1Triggers 32
#define MicroL2MaxNumAlgorithms 64
//sizes should be replaced by some macro that contains MaxNumberOfLevel2Algs def

class microL2DArrayWrapper: public PHObject { 

 public:   
///
  microL2DArrayWrapper(){;}
///
  ~microL2DArrayWrapper() {;}
///
  void identify(ostream& os = std::cout) const {;}
///  
  UINT theArray[MicroL2MaxNumAlgorithms + 1]; // +1 for ordering insurance l1 ind

  ClassDef(microL2DArrayWrapper,1)

};

#endif
