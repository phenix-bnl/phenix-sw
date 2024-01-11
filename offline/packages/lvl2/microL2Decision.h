/*////////////////////////
//microL2Decision:
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
//////////////////////////*/

#ifndef MICROL2DECISION_H
#define MICROL2DECISION_H

#include <iostream.h>
#include <TClonesArray.h>
#include <Lvl2DecisionOutv1.h>
#include <microL2DArrayWrapper.h>
#include <phenixTypes.h>

class microL2Decision: public PHObject {

public:
   
///
  microL2Decision();
  microL2Decision(Lvl2DecisionOutv1 * inDstObj);

///
  virtual ~microL2Decision();
///
  void Reset();
///
  void Clear();
///
  void identify(ostream& os = std::cout) const;
///
  void dump(ostream& os = std::cout) const {;}
///
  int addMicroL2DArrayWrapper(microL2DArrayWrapper *) const;
///
  TClonesArray *getMicroL2DArrayWrappers() const 
    {return _microL2DArrayWrappers;}
///
  int getNumActiveLvl1Triggers() {return _numLvl1Triggers;}
///
  void createDstObj();
  void fillSelfFromDstObj(Lvl2DecisionOutv1 *);

  //data members:  

  Lvl2DecisionOutv1* dstObj;

 protected:

  UINT _finalDecision;
  UINT _numLvl1Triggers;
  UINT _lvl1Decision[MicroL2MaxNumLvl1Triggers];
  TClonesArray* _microL2DArrayWrappers;

  ClassDef(microL2Decision,1)

};

#endif








