#ifndef __AGGROSNGLFILESV1_H_
#define __AGGROSNGLFILESV1_H_

#include "PHObject.h"
#include "AggroSnglFiles.h"

class AggroSnglFilesv1 : public AggroSnglFiles
{
 public:
  AggroSnglFilesv1();
  AggroSnglFilesv1(AggroSnglFilesv1*track);  
  virtual ~AggroSnglFilesv1() {}

  // Here are the very explicit set routines...
  void set_filename (const std::string val) {filename = val; return;}
  void set_filesegm (const short  val) {filesegm = val; return;}

  // Here are the very explicit "get" routines...
  std::string get_filename () const  { return  filename;}
  short  get_filesegm () const  { return  filesegm;}

 protected:
  std::string filename;
  short  filesegm;

  ClassDef(AggroSnglFilesv1,1)
};

#endif

