#ifndef __MUTRGPACKPRDF__
#define __MUTRGPACKPRDF__

#include "SubsysReco.h"

#include <vector>

class PHCompositeNode;
class PHRawDataNode;
class MutrgHeaderArray;
class MutrgHitArray;

class MutrgPackPRDF : public SubsysReco{
public:
  MutrgPackPRDF(const char *name="MutrgPackPRDF");
  virtual ~MutrgPackPRDF(void){;}

  int InitRun(PHCompositeNode *top_node);
  int process_event(PHCompositeNode *top_node);

  const char *ClassName(void){return Name();}

protected:
  static const int BYTES_PER_WORD;

  PHCompositeNode *prdf_node;
  std::vector<PHRawDataNode*> data_node;

  MutrgHeaderArray *mutrg_headers;
  MutrgHitArray *mutrg_hits;
};

#endif /* __MUTRGPACKPRDF__ */
