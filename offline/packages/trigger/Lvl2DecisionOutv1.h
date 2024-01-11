#ifndef __LVL2DECISIONOUTV1_H__
#define __LVL2DECISIONOUTV1_H__

#include <iostream>
#include <phenixTypes.h>
#include <Lvl2DecisionOut.h>

class Lvl2DecisionOutv1 : public Lvl2DecisionOut	//++CINT
{
public:

  enum {
    MaxNumLvl1Triggers = 32,
    MaxNumAlgorithms = 64
  };

  Lvl2DecisionOutv1();
  virtual ~Lvl2DecisionOutv1();

  void   setFullDecision(UINT decision) { _finalDecision = decision; }
  UINT   getFullDecision() { return _finalDecision; }
  void   setNumLevel1Triggers(UINT num) { _numLvl1Triggers = num; }
  UINT   getNumLevel1Triggers() { return _numLvl1Triggers; }
  void   setLevel1TriggerDecision(UINT ilevel1, UINT decision);
  UINT   getLevel1TriggerDecision(UINT ilevel1) { return _lvl1Decision[ilevel1]; };
  void   setAlgorithmDecision(UINT ialg, UINT decision);
  UINT   getAlgorithmDecision(UINT ialg) { return _algorithmDecision[ialg]; }
  void   setLvl1AlgorithmDecision(UINT ilevel1, UINT ialg, UINT decision);
  UINT   getLvl1AlgorithmDecision(UINT ilevel1, UINT ialg) {
           return _lvl1AlgorithmDecision[ilevel1][ialg];
         }
  UINT   getMaxNumAlgorithms() { return MaxNumAlgorithms; };
  UINT   getMaxNumLvl1Triggers() { return MaxNumLvl1Triggers; };
 
  void Reset();
  void Clear(Option_t *option = "");
  void identify(std::ostream& os = std::cout) const {
    os << "identify yourself: I am a Lvl2DecisionOutv1 object" << std::endl;
  }
  void dump(std::ostream& os = std::cout) const;

private:

  UINT _finalDecision;
  UINT _numLvl1Triggers;
  UINT _algorithmDecision[MaxNumAlgorithms];
  UINT _lvl1Decision[MaxNumLvl1Triggers];
  //UINT _lvl1AlgorithmDecision[MaxNumAlgorithms][MaxNumLvl1Triggers];
  UINT _lvl1AlgorithmDecision[MaxNumLvl1Triggers][MaxNumAlgorithms];

  ClassDef(Lvl2DecisionOutv1,2)

};

#endif	// __LVL2DECISIONOUTV1_H__
