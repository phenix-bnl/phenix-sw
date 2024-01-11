#ifndef __AGGROFILESV1_H
#define __AGGROFILESV1_H

#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "AggroFiles.h"
#include "AggroSnglFilesv1.h" //must include to satisfy covariance.
#include <iostream>

//
//  This class is a so-called "versioned" object.  We are
//  inheriting from the AggroFiles.  That virtual base 
//  class has manipulators for AggroFiles objects that are
//  not specific to any particular verion of AggroFiles objects.
//  This container over-rides the methods of the virtual base class
//  making them specific to version 1 AggroFiless.
//
//  *Here* is where we actually make the TClonesArray that holds the
//  single AggroFilesv1 objects.
//                                 TKH 8-11-2003
//

class AggroFilesv1 : public AggroFiles
{
 public:
  AggroFilesv1();
  AggroFilesv1(const AggroFilesv1&);
  AggroFilesv1& operator=(const AggroFilesv1&);
  virtual ~AggroFilesv1();

  AggroFilesv1* clone() const;

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Actual implementations of the set/get methods...
  void set_nfiles      (const unsigned int NFILES) {nFiles = NFILES; return;}
  int  get_nfiles      () const {return nFiles;}

  // Routines to manipulate the particle array...
  int set_TClonesArraySize(const unsigned int ntrk);
  void AddFiles          (const unsigned int itrk);
  void RemoveFiles       (const unsigned int itrk);
  AggroSnglFilesv1* AddFiles (const unsigned int itrk, const AggroSnglFiles& sngl);
  AggroSnglFilesv1* get_files(const unsigned int itrk) const;

 protected:
  TClonesArray *GetFiles() const {return Files;}
  unsigned int nFiles;
  TClonesArray *Files;

private:
  // Copy this to dest.
  void copyto(AggroFilesv1& dest) const;
  ClassDef(AggroFilesv1,1)
};

#endif /* __AGGROFILESV1_H */






