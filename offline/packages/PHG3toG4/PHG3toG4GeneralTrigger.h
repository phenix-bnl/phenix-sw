#ifndef __PHG3TOG4GENERALTRIGGER_H__
#define __PHG3TOG4GENERALTRIGGER_H__


#include <iostream>
#include <sstream>
#include <string>
#include <vector>

class PHG3toG4GeneralTrigger{

 protected:  
  //! constructor
  PHG3toG4GeneralTrigger(const std::string &name = "PHG3toG4GeneralTrigger");


 public:
  virtual ~PHG3toG4GeneralTrigger();
  virtual bool Apply()
  {std::cout << "PHG3toG4GeneralTrigger::Apply - in virtual function" << std::endl; return false; }

  virtual std::string GetName()
  { return _name;}

  virtual void AddParticles(std::string particles);
  virtual void AddParticles(int particle);
  virtual void AddParticles(std::vector<int> particles);

  virtual void AddParents(std::string parents);
  virtual void AddParents(int parent);
  virtual void AddParents(std::vector<int> parents);

  virtual std::vector<int> GetParticles(){ return _theParticles;}
  virtual std::vector<int> GetParents(){ return _theParents;}
  
  std::vector<int> convertToInts(std::string s);

  virtual void CheckVectors(){ std::cout << "PHG3toG4GeneralTrigger::CheckVectors - in virtual function" << std::endl;}
  virtual void SetDoAbsParticlesOnly(){ _doAbsOnly = true;}
  virtual bool GetDoAbsParticlesOnly(){ return _doAbsOnly;}

  void Verbosity(int v)
  { _verbosity = v; }

  int _verbosity;

 protected:

  std::string _name;
  std::vector<int> _theParents;
  std::vector<int> _theParticles;
  bool _doAbsOnly;
  

};

#endif	

