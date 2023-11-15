#ifndef PDBCALBANKSAVEV1_H
#define PDBCALBANKSAVEV1_H

#include "PdbCalBankSave.h"

#include <phool.h>

#include <map>
#include <set>
#include <string>

///
class PdbCalBankSavev1: public PdbCalBankSave
{
 public:

  /// dtor
  virtual ~PdbCalBankSavev1() {}

  /// Clear Event
  void Reset();

  /** identify Function from PHObject
      @param os Output Stream 
   */
  void identify(std::ostream& os = std::cout) const;

  /// isValid returns non zero if object contains valid data
  int isValid() const;

  int AddBank(const std::string &name, const int &rid);

 protected:

  std::map<std::string,std::set<int> > banksave;
  
 private: // prevent doc++ from showing ClassDef
  ClassDef(PdbCalBankSavev1,1)

};

#endif
