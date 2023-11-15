#ifndef PDBMAPINTMAPINT_HH__
#define PDBMAPINTMAPINT_HH__

#include "PdbCalChan.hh"

#include <map>
#include <vector>

class PdbMapIntMapIntInt : public PdbCalChan 
{
public:
  PdbMapIntMapIntInt(); 
  virtual ~PdbMapIntMapIntInt();

  std::map<int, std::map <int, int> > get_map() { return TheMap; }
  void set_map(std::map<int, std::map <int, int> > inmap) {TheMap = inmap;}
  void Clear(Option_t* ="");
  void set_runnumber(const int ir) {runnumber = ir;}
  int get_runnumber() const {return runnumber;}
  virtual void print() const;

private:
  int runnumber;
  std::map<int, std::map <int, int> > TheMap;

  ClassDef(PdbMapIntMapIntInt,1);

};

#endif 
