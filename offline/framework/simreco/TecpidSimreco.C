#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeReset.h"
#include "recoConsts.h"

#include <cmath>

#include "TecpidSimreco.h"
#include "mTecPIDModule.h"

using namespace std;

TecpidSimreco::TecpidSimreco(const char *name)
{
  ThisName = name;

  mTecPID        = NULL;
}

int TecpidSimreco::Init(PHCompositeNode *topNode)
{
  return 0;
}

int TecpidSimreco::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);

  PHCompositeNode *tecNode;
  tecNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "TEC"));
  if (!tecNode)
    {
      cout << PHWHERE << "TecpidSimreco::TEC Node missing doing nothing" << endl;
      return -1;
    }

  mTecPID = new mTecPIDModule;

  return 0;
}

int TecpidSimreco::CreateNodeTree(PHCompositeNode *topNode)
{
  return 0;
}

int TecpidSimreco::process_event(PHCompositeNode *topNode)
{
  mTecPID->event(topNode);

  return 0;
}

int TecpidSimreco::ResetEvent(PHCompositeNode *topNode)
{

  return 0;
}
