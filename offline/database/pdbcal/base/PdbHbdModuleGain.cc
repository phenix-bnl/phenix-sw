#include "PdbHbdModuleGain.hh"
#include <iostream>
#include "boost/foreach.hpp"

//____________________________________________________________
PdbHbdModuleGain::PdbHbdModuleGain( const PdbHbdModuleGain &c)
{
  unsigned int tmpMoreSignif,tmpLessSignif;
  c.getClockTick(tmpMoreSignif,tmpLessSignif);
  setClockTick(tmpMoreSignif,tmpLessSignif);
  std::vector<float> tmpConsts;
  c.getAllGains(tmpConsts);
  setAllGains(tmpConsts);
}

//_______________________________________________________________________
PdbHbdModuleGain& PdbHbdModuleGain::operator= (const PdbHbdModuleGain &c)
{
  unsigned int tmpMoreSignif,tmpLessSignif;
  c.getClockTick(tmpMoreSignif,tmpLessSignif);
  setClockTick(tmpMoreSignif,tmpLessSignif);
  std::vector<float> tmpConsts;
  c.getAllGains(tmpConsts);
  setAllGains(tmpConsts);
  return *this;
}

//____________________________________________________________________________________
PdbHbdModuleGain::PdbHbdModuleGain():ctMoreSignif(0),ctLessSignif(0),gainConsts()
{
  for (unsigned int ii=0; ii<NHBDMODULES; ii++) gainConsts.push_back(0.0);
}

// set the clock tick for start of validity range, as two ints for more & less significant bits
//____________________________________________________________________________________
int PdbHbdModuleGain::setClockTick(const unsigned int moreSignif, const unsigned int lessSignif)
{
  ctMoreSignif = moreSignif;
  ctLessSignif = lessSignif;
  return 0;
}

// set the gains individually for a given module identified by its module id
//________________________________________________
int PdbHbdModuleGain::setGain(const unsigned int module, const float constant)
{
  if (module>=NHBDMODULES) 
    {
      std::cout<< "PdbHbdModuleGain::setGain Supplied index argument " << module << " out of bounds" << std::endl;
      return -1;
    }
  gainConsts[module] = constant;
  return 0;
}

// set the gains for all modules as a vector of floats, (recommended) index = module id
//________________________________________________________________
int PdbHbdModuleGain::setAllGains(const std::vector<float> consts)
{
  if (consts.size()!=NHBDMODULES)
    {
      std::cout<< "PdbHbdModuleGain::setAllGains Vector supplied has wrong size " << consts.size() << ": Correct size = " << NHBDMODULES << std::endl;
      return -1;
    }
  gainConsts.clear();
  BOOST_FOREACH(float _const, consts)
    {
      gainConsts.push_back(_const);
    }
  return 0;
}

// get the clock ticks for the start of validity range 
//__________________________________________________________________________
int PdbHbdModuleGain::getClockTick(unsigned int &moreSignif, unsigned int &lessSignif) const
{
  moreSignif = ctMoreSignif;
  lessSignif = ctLessSignif;
  return 0;
}

// get the gain value for a given module (identifiedy by its module id)
//______________________________________________________________
int PdbHbdModuleGain::getGain(const unsigned int module, float &constant) const
{
  if (module>=NHBDMODULES)
    {
      std::cout << "PdbHbdModuleGain::getGain Supplied index argument " << module << " out of bounds" << std::endl;
      return -1;
    }
  constant = gainConsts.at(module);
  return 0;
}

// get the gain value for a given module (identifiedy by its module id)
//_____________________________________________________
float PdbHbdModuleGain::getGain(const unsigned int module) const
{
  if (module>=NHBDMODULES)
    {
      std::cout << "PdbHbdModuleGain::getGain Supplied index argument " << module << " out of bounds" << std::endl;
      return -9999.0;
    }
  return gainConsts.at(module);
}

// set all the gains together at the same time as a vector of floats
//___________________________________________________________
int PdbHbdModuleGain::getAllGains(std::vector<float> &consts) const
{
  BOOST_FOREACH(float _gain, gainConsts)
    {
      consts.push_back(_gain);
    }
  return 0;
}

//__________________________________
void PdbHbdModuleGain::print() const
{
  std::cout << "PdbHbdModuleGain::print() This is PdbHbdModuleGain";
  std::cout << " ctMoreSignif = " << ctMoreSignif;
  std::cout << " ctLessSignif = " << ctLessSignif << std::endl;
  std::cout << "PdbHbdModuleGain::print() Gains: ";
  BOOST_FOREACH(float _gain, gainConsts)
    {
      std::cout << _gain << " ";
    }
  std::cout << std::endl;
}
