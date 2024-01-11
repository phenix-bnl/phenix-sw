/*////////////////////////
//Lvl2DecisionOutMicrov1:
//    microDST Output object:
//    stores same info as Lvl2DecisionOut preco module/dst object
//    in a space efficient manner, and has methods to reconstruct
//    and make public data member of object of that class for use 
//    in analysis + some nicer user fristd::endly methods to access that
//    object's information.
//
//    Please refer to offline/packages/lvl2 for information about 
//    the class Lvl2DecisionOut.  ALSO PLEASE NOTE: when version
//    of Lvl2DecisionOutv1 -> v2 we may want to update this class
//    accordingly
//    
//    
//    author: jfrantz  
//
//////////////////////////
*/

#ifndef __Lvl2DecisionOutMicrov1_H
#define __Lvl2DecisionOutMicrov1_H

#include <Lvl2DecisionOut.h> // virtual base class
#include <microL2DArrayWrapperv1.h>
//#include <phenixTypes.h>
#include <iostream>

static const unsigned short int MICROL2MAXNUMLVL1TRIGGERSV1 = 32;

class microL2DArrayWrapperv1;
class TClonesArray;

class Lvl2DecisionOutMicrov1: public Lvl2DecisionOut {

public:
   
///
  Lvl2DecisionOutMicrov1();
  Lvl2DecisionOutMicrov1(Lvl2DecisionOut * inDstObj);

/// Lvl2DecisionOut functs

  void   setFullDecision(UINT decision);
  UINT   getFullDecision();
  void   setNumLevel1Triggers(UINT num);
  UINT   getNumLevel1Triggers();
  void   setLevel1TriggerDecision(UINT ilevel1, UINT decision);
  UINT   getLevel1TriggerDecision(UINT ilevel1);
  void   setAlgorithmDecision(UINT /*ialg*/, UINT /*decision*/) {return;}
  // setAlgorithDecision does nothing in microDST
  // if you want to use this and getAlgorithmDecision 
  // and use a DST obj from createDstObj(); 
  UINT   getAlgorithmDecision(UINT ialg);  // this works but is expensive
  void   setLvl1AlgorithmDecision(UINT ilevel1, UINT ialg, UINT decision);
  UINT   getLvl1AlgorithmDecision(UINT ilevel1, UINT ialg);
  UINT   getMaxNumLvl1Triggers() {return MICROL2MAXNUMLVL1TRIGGERSV1;}
  UINT   getMaxNumAlgorithms() {return MICROL2MAXNUMALGORITHMSV1;}

  int  locateLvl1L2DArray(UINT ilevel1);
  virtual ~Lvl2DecisionOutMicrov1();
  void Reset();
  void Clear(Option_t* ="");
  void identify(std::ostream& os = std::cout) const;
  void dump(std::ostream& os = std::cout) const // expensive time wise 
    {
      Lvl2DecisionOut* dstObj = createDstObj();
      dstObj->dump(os); 
      delete dstObj;
    }

  int getNumActiveLvl1Triggers() {return _numLvl1Triggers;}
  Lvl2DecisionOut* createDstObj() const; // you are responsible for deleting this obj
  void fillSelfFromDstObj(Lvl2DecisionOut *);

 protected:
  TClonesArray *getMicroL2DArrayWrappers() const 
    {return _microL2DArrayWrappers;}
  int addMicroL2DArrayWrapper(microL2DArrayWrapperv1 *wrapper) const;

  UINT _finalDecision;
  UINT _numLvl1Triggers;
  UINT _lvl1Decision[MICROL2MAXNUMLVL1TRIGGERSV1]; //microL2DarrayWrapper.h:MicroL2Max
  TClonesArray* _microL2DArrayWrappers;

  ClassDef(Lvl2DecisionOutMicrov1,1)

};

#endif



















