#ifndef __AGGROSNGLFILES_HH_
#define __AGGROSNGLFILES_HH_

#include <iostream>
#include <PHObject.h>
#include <phool.h>
#include <string>

//
//  The AggroSnglFiles is a deprecated piece of junk, we only keep it
//  so we can read our Run3 dsts

class AggroSnglFiles : public PHObject
{


 public:
  virtual ~AggroSnglFiles() {}

  // Set the values in the SnglFiles...
  // These virtual functions should ALL be overridden!
  // If the local version is called by mistake, the user sees a
  // warning on their screen.

  virtual void set_filename (const std::string val)   {warning("filename");}
  virtual void set_filesegm (const short  val)   {warning("filesegm");}

  // Get the values from the SnglFiles...
  // The virtual base class prints warning then returns crap...
  virtual std::string get_filename() const {warning("filename"); return "NONE";}
  virtual short  get_filesegm() const {warning("filesegm"); return -9999;}

 private:
  void warning(const char* field) const { 
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout << "Single AGGRO FILES Offending field == " << field << std::endl;
  }


  ClassDef(AggroSnglFiles,1)
};
#endif
