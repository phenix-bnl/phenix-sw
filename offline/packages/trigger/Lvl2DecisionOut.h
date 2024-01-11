#ifndef __LVL2DECISIONOUT_H__
#define __LVL2DECISIONOUT_H__

#include <iostream>
#include <phool.h>
#include <PHObject.h>
#include <phenixTypes.h>

class Lvl2DecisionOut : public PHObject
{
public:
  virtual ~Lvl2DecisionOut() {}

  virtual void   setFullDecision(UINT decision) = 0;
  virtual UINT   getFullDecision() = 0;
  virtual void   setNumLevel1Triggers(UINT num) = 0;
  virtual UINT   getNumLevel1Triggers() = 0;
  virtual void   setLevel1TriggerDecision(UINT ilevel1, UINT decision) = 0;
  virtual UINT   getLevel1TriggerDecision(UINT ilevel1) = 0;
  virtual void   setAlgorithmDecision(UINT ialg, UINT decision) = 0;
  virtual UINT   getAlgorithmDecision(UINT ialg) = 0;
  virtual void   setLvl1AlgorithmDecision(UINT ilevel1, UINT ialg, UINT decision) = 0;
  virtual UINT   getLvl1AlgorithmDecision(UINT ilevel1, UINT ialg) = 0;
  virtual UINT   getMaxNumLvl1Triggers() {return 32;}
  virtual UINT   getMaxNumAlgorithms() {return 64;}
  virtual void fillSelfFromDstObj(Lvl2DecisionOut *)
    {
      std::cout << PHWHERE << "virtual fillSelfFromDstObj, doing nothing" << std::endl;
      return;
    }
  virtual void   identify(std::ostream& os = std::cout) const = 0;
  virtual void   dump(std::ostream& os = std::cout) const = 0;

  ClassDef(Lvl2DecisionOut,1)

};

#endif	// __LVL2DECISIONOUT_H__
