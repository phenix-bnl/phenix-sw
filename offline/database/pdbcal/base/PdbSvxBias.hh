#ifndef __PDBSVXBIAS_HH__
#define __PDBSVXBIAS_HH__

#include <PdbCalChan.hh>
#include <iostream>
#include <ctime>
#include <phool.h>
#include <string>
#include <map>


class PdbSvxBias : public PdbCalChan {
public:
  PdbSvxBias();
  virtual ~PdbSvxBias() {}

  PdbSvxBias& operator = (const PdbSvxBias &p);

  int set(const std::string& channel,const int timestamp,const int flag);
  std::map<std::string,std::pair<int,int> > get() const { return badlist; };
  virtual void reset();
  virtual void print() const;

private:

  std::map<std::string,std::pair<int,int> > badlist;
  ClassDef(PdbSvxBias,1);
};

#endif // __PDBSVXBIAS_HH__ 

