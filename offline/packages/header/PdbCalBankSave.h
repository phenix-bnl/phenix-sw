#ifndef __PDBCALBANKSAVE_H
#define __PDBCALBANKSAVE_H

#include <phool.h>
#include <PHObject.h>

#include <string>

///
class PdbCalBankSave: public PHObject
{
 public:

  /// dtor
  virtual ~PdbCalBankSave() {}

  /// Clear Event
  virtual void Reset();

  /** identify Function from PHObject
      @param os Output Stream 
   */
  virtual void identify(std::ostream& os = std::cout) const;

  /// isValid returns non zero if object contains valid data
  virtual int isValid() const;

  virtual int AddBank(const std::string &name, const int &rid);

 protected:
  void warning(const char *func) const;
  
 private: // prevent doc++ from showing ClassDef
  ClassDef(PdbCalBankSave,1)

};

#endif
