#ifndef __LVL2PRIMITIVESCHECK_H__
#define __LVL2PRIMITIVESCHECK_H__

#include <iostream>
#include <SubsysReco.h>

class PHCompositeNode;

class Lvl2PrimitivesCheck: public SubsysReco
{

 public:

  Lvl2PrimitivesCheck(const char *name = "Lvl2PrimitivesCheck");
  virtual ~Lvl2PrimitivesCheck() {};

  int process_event(PHCompositeNode *topNode);
  int Init(PHCompositeNode *topNode);
  void identify(std::ostream& out = std::cout) const;
  int BeginRun(const int runno);
  int EndRun(const int runno);
	 
private:

  unsigned int nevt;
  char lvl2outarraynodename[100];

};

#endif /*__LVL2PRIMITIVESCHECK_H__ */
  






