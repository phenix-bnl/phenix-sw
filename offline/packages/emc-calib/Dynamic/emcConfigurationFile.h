#ifndef __EMCCONFIGURATIONFILE_H__
#define __EMCCONFIGURATIONFILE_H__

#ifndef __EMCMANAGEABLE_H__
#include "emcManageable.h"
#endif
#ifndef __PHTIMESTAMP_H__
#include "PHTimeStamp.h"
#endif
#include <string>

/** A wrapper for an ASCII buffer that can be passed to the DataManager. */

class emcConfigurationFile : public emcManageable
{
 public:

  /// default ctor.
  emcConfigurationFile() : fCurrent(0),fIsValid(false),fContent(""),fStart(0),fEnd(0)
    { fEnd.setToFarFuture(); }

  /// ctor from a file.
  emcConfigurationFile(const char* filename); 

  /// dtor. does nothing.
  virtual ~emcConfigurationFile() {; }

  /// Get the start of validity period of this object.
  const PHTimeStamp& GetStartValTime(void) const { return fStart; }

  /// Get the end of validity period of this object.
  const PHTimeStamp& GetEndValTime(void) const { return fEnd; }

  /// How many characters in this (string) objects.
  size_t GetSize(void) const { return fContent.size(); }

  /// Tells if this object is OK or not.
  bool IsValid(void) const { return fIsValid; }

  /// Tells if this object is valid for a given time.
  bool IsValid(const PHTimeStamp& when) const; 

  /// Category = "CONFIG"
  virtual const char* GetCategory(void) const { return "CONFIG"; } 
 
  /// Set the validity period of this object.
  void SetValidityPeriod(const PHTimeStamp& t1, const PHTimeStamp& t2) {
    fStart = t1; fEnd = t2;
  }

  /// Replace the (string) content.
  void SetContent(const char* content) { fIsValid = true; fContent = content; }

  /// Get the content.
  const char* GetContent(void) const { return fContent.c_str(); }

  /// Reset (i.e. SetContent(""))
  virtual void Reset(void) { fContent=""; }

  /// Rewind the internal character buffer.
  void Rewind(void) { fCurrent = 0; }

  /// Get the next line from the internal char buffer.
  bool GetLine(char* s, int size);

 private:
  int fCurrent; 
  bool fIsValid; 
  std::string fContent;
  PHTimeStamp fStart;
  PHTimeStamp fEnd;
};

#endif
