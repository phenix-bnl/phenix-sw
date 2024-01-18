#ifndef __PHG3TOG4ACTIVITYTRIGGER_H__
#define __PHG3TOG4ACTIVITYTRIGGER_H__


#include "PHG3toG4GeneralTrigger.h"

#include <map>
#include <string>                    // for string
#include <vector>

class PHG3toG4ActivityTrigger : public PHG3toG4GeneralTrigger
{

 public:

  PHG3toG4ActivityTrigger(const std::string &name = "PHG3toG4ActivityTrigger");
  virtual ~PHG3toG4ActivityTrigger();
  
  virtual bool Apply();

  void AddDetector(std::string det){_theDetectors.push_back(det);}
  void AddDetectorMinHits(std::string det,int minHits);

  virtual void CheckVectors();
  virtual void SetDoAbsParticlesOnly(){ _doAbsOnly = true;}                          
  virtual bool GetDoAbsParticlesOnly(){ return _doAbsOnly;}
  virtual void SetMinMom(float a) { minMom = a;}

  std::vector<int> GetParticles(){ return _theParticles;}
  std::vector<int> GetParents(){ return _theParents;}

  void PrintConfig();

 private:

  std::vector<std::string> _allDetectors;
  std::vector<std::string> _theDetectors;
  std::map<std::string,int> _theDetMinHits;
  bool _anyParticles;
  float minMom;

};

#endif	

