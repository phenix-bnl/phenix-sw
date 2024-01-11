#ifndef TNamedDir_HH
#define TNamedDir_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TNamed.h>
#include <TDirectory.h>

class TNamedDir : public TNamed {
private:
  TDirectory* fDirectory;      //!Pointer to directory holding this histogram
  static bool fgAddDirectory;  //!flag to add histograms to the directory
public:
  TNamedDir();
  TNamedDir(const char* name, const char* title);
  ~TNamedDir();

  // Analysis method.
  static void AddDirectory(bool add = true);
  static bool AddDirectoryStatus();
  void SetDirectory(TDirectory *dir);
  virtual void SetName(const char *name);
  virtual void SetNameTitle(const char *name, const char *title);

  ClassDef(TNamedDir,1) //TNamed with append to gDirectory
};
//
#endif
//
