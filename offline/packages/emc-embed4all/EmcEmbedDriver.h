#ifndef __EMCEMBEDDRIVER_H__
#define __EMCEMBEDDRIVER_H__

#include "SubsysReco.h"
#include <string>

class EmcEmbedDriver : public SubsysReco
{
 public:
  EmcEmbedDriver(const char* realnode="REAL",
		 const char* simunode="SIMU",
		 const char* mergednode="TOP",
		 const char* realinputmanagername="real");

  
  int InitRun(PHCompositeNode*);

  int process_event(PHCompositeNode*);

 private:
  
  std::string fRealNode;
  std::string fSimuNode;
  std::string fMergedNode;
  std::string fRealInputManager;
};
#endif
