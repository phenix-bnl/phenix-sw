#ifndef __EMCEMBEDREADER_H__
#define __EMCEMBEDREADER_H__

#include "SubsysReco.h"
#include <string>

class Counters;

/** This is an example of a module to handle both merged DST
    (read under topnode MERGED) and other DSTs (read under TOP)
*/

class EmcEmbedReader : public SubsysReco
{
 public:
  EmcEmbedReader(const char* merged="MERGED",
		 const char* top="TOP");

  virtual ~EmcEmbedReader();

  int Init(PHCompositeNode*);

  int process_event(PHCompositeNode*);

 private:
  std::string fMergedNode;
  std::string fTopNode;
  Counters* fCounters;
};

#endif
