#ifndef __EMCFEMLIST_H__
#define __EMCFEMLIST_H__

#include <iostream>
#include <set>
#include <string>

/** Small utility class to get the list of FEMs corresponding
    to a set of emcal sectors. 
@ingroup calibration
*/

class emcFEMList
{
 public:
  emcFEMList(const char* sectors);

  bool hasFEM(int ifem) const;
  bool hasSector(const char* sname) const;

  std::set<int> fems(const char* sname) const;

  void print(std::ostream& out = std::cout) const;

 private:

  std::set<std::string> fSectors;
  std::set<int> fFems;
};

#endif
