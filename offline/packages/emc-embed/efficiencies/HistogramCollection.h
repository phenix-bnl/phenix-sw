#ifndef __HistogramCollection_h__
#define __HistogramCollection_h__

#include <map>
#include <iostream>
#include <cassert>
#include <string>

class TH1;
class TH2;
class TH3;
class TDirectory;

class HistogramCollection 
{
public:

  HistogramCollection() { assert(0==1); }

  HistogramCollection(TDirectory* dir);

  TH1* book(const std::string& name, const std::string& title,
	    int nbins, double xmin, double max);

  TH2* book(const std::string& name, const std::string& title,
	    int nbinx, double xmin, double xmax,
	    int nbiny, double ymin, double ymax);

  TH3* book(const std::string& name, const std::string& title,
	    int nbinx, double xmin, double xmax,
	    int nbiny, double ymin, double ymax,
	    int nbinz, double zmin, double zmax);

  const TDirectory* directory(void) const { return fDir; }

  bool exist(const std::string& name) const;

  void incrNevents1(void) { fNevents1++; }
  void incrNevents2(void) { fNevents2++; }

  bool fill(const std::string& name, double x) 
    { return fillW(name,x,1.0); }

  bool fillW(const std::string& name, double x, double weight);

  bool fill(const std::string& name, double x, double y) 
    { return fillW(name,x,y,1.0); }

  bool fillW(const std::string& name, double x, double y, double weight);

  bool fillW(const std::string& name, double x, double y, double z, double weight);

  TH1* get(const std::string& name) const;

  unsigned int getNevents1(void) const { return fNevents1; }
  unsigned int getNevents2(void) const { return fNevents2; }

  void print(std::ostream& out=std::cout) const;

  void write(void) const;

private:

  std::map<std::string, TH1*> fMap;
  std::map<std::string, TH1*>::iterator fIterator;
  std::map<std::string, TH1*>::const_iterator fCIterator;

  TDirectory* fDir;
  unsigned int fNevents1;
  unsigned int fNevents2;
};

#endif
