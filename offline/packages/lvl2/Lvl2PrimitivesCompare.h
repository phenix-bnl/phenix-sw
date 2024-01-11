#ifndef __LVL2PRIMITIVESCOMPARE_H__
#define __LVL2PRIMITIVESCOMPARE_H__

#include <iostream>
#include <SubsysReco.h>

class PHCompositeNode;
class Lvl2OutArray;

class Lvl2PrimitivesCompare: public SubsysReco
{

 public:

  Lvl2PrimitivesCompare(const char *name = "Lvl2PrimitivesCheck");
  virtual ~Lvl2PrimitivesCompare() {};

  int process_event(PHCompositeNode *topNode);
  int Init(PHCompositeNode *topNode);
  void identify(std::ostream& out = std::cout) const;
  int BeginRun(const int runno);
  int EndRun(const int runno);
	 
private:

  unsigned int nevt;

  char lengthfoundname[50][50];
  float lengthcount[50];
  int nfoundlength;

  char primfoundname[50][50];
  float primevtcount[50];
  float primprobcount[50];
  float primevtreset[50];
  int nfoundprims;

  void PrintL2EMCHighPtTilePrimitive(Lvl2OutArray *lvl2outarray,Lvl2OutArray * lvl2outarraycal);
  void PrintL2MuiPrimitives(Lvl2OutArray * lvl2outarray,Lvl2OutArray *lvloutarraycal);
  void PrintL2AuAuElectronCandidate(Lvl2OutArray *lvl2outarray,Lvl2OutArray *lvl2outarraycal);
  void PrintL2AuAuElectronLowMassPairs(Lvl2OutArray *lvl2outarray,Lvl2OutArray * lvl2outarraycal);
};

#endif /*__LVL2PRIMITIVESCOMPARE_H__ */
  






