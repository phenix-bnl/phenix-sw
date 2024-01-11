#include <stdio.h>
#include <stream.h>
#include "TNamedDir.hh"
#include <TH1.h>

ClassImp(TNamedDir)
//
//------------------------------------------------------------
bool TNamedDir::fgAddDirectory = true;

//------------------------------------------------------------
TNamedDir::TNamedDir(): TNamed(){
  fDirectory = 0;
  return;
};
//------------------------------------------------------------
TNamedDir::TNamedDir(const char* name, const char* title) : TNamed(name,title){
  if (fgAddDirectory && gDirectory) {
    TNamedDir *hold = (TNamedDir*)gDirectory->GetList()->FindObject(GetName());
    if (hold) {
      Warning("Build","Replacing existing object: %s",GetName());
      gDirectory->GetList()->Remove(hold);
    }
    gDirectory->Append(this);
  }
  fDirectory = gDirectory;
  return;
};
//------------------------------------------------------------
TNamedDir::~TNamedDir(){
  if (fDirectory) {
    if (!fDirectory->TestBit(TDirectory::kCloseDirectory))
      fDirectory->GetList()->Remove(this);
  }
  fDirectory = 0;
  return;
};
//------------------------------------------------------------
void TNamedDir::AddDirectory(bool add = true){
  // Sets the flag controlling the automatic add of histograms in memory
  //
  // By default (fAddDirectory = kTRUE), histograms are automatically added
  // to the list of objects in memory.
  // Note that one histogram can be removed from its support directory
  // by calling h->SetDirectory(0) or h->SetDirectory(dir) to add it
  // to the list of objects in the directory dir.
  //
  //  NOTE that this is a static function. To call it, use;
  //     TH1::AddDirectory

  fgAddDirectory = add;
};
//------------------------------------------------------------
bool TNamedDir::AddDirectoryStatus(){
  //static function: cannot be inlined on Windows/NT
  return fgAddDirectory;
};
//------------------------------------------------------------
void TNamedDir::SetDirectory(TDirectory *dir){
  // By default when an histogram is created, it is added to the list
  // of histogram objects in the current directory in memory.
  // Remove reference to this histogram from current directory and add
  // reference to new directory dir. dir can be 0 in which case the
  // histogram does not belong to any directory.

  if (fDirectory == dir) return;
  if (fDirectory) fDirectory->GetList()->Remove(this);
  fDirectory = dir;
  if (fDirectory) fDirectory->GetList()->Add(this);
};
//------------------------------------------------------------
void TNamedDir::SetName(const char *name){
  // Change the name of this histogram
  //

  //  Histograms are named objects in a THashList.
  //  We must update the hashlist if we change the name
  if (fDirectory) fDirectory->GetList()->Remove(this);
  fName = name;
  if (fDirectory) fDirectory->GetList()->Add(this);
};
//------------------------------------------------------------
void TNamedDir::SetNameTitle(const char *name, const char *title){
  // Change the name and title of this histogram
  //

  //  Histograms are named objects in a THashList.
  //  We must update the hashlist if we change the name
  if (fDirectory) fDirectory->GetList()->Remove(this);
  fName  = name;
  fTitle = title;
  if (fDirectory) fDirectory->GetList()->Add(this);
};
//------------------------------------------------------------
