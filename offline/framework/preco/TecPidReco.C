
#include <phool.h>
#include <PHNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeReset.h>
#include <PHCompositeNode.h>
#include <PHTypedNodeIterator.h>
#include <PHNodeReset.h>
#include <TecProj.hh>
#include <TecProjv1.hh>
#include <TecPidReco.h>
#include <mTecCglModule.h>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;

TecPidReco::TecPidReco(const string &name): SubsysReco(name) {
  mTecCgl = 0;
  return;
}

TecPidReco::~TecPidReco()
{
  delete mTecCgl;
  return;
}

int TecPidReco::InitRun(PHCompositeNode *topNode) {

  PHNodeIterator iter(topNode);
  PHCompositeNode *tecNode=0;
  tecNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "TEC"));
  if(!tecNode) {
      cout << PHWHERE << "TecPidReco ERROR: TEC Node missing doing nothing" << endl;
      return -1;
  }

  TecProj* tecproj[2];

  tecproj[0] = new TecProjv1();
  if (tecproj[0])
    {
      PHObjectNode_t *tecprojNode =
	new PHObjectNode_t(tecproj[0],"TecProj","PHObject");
      tecNode->addNode(tecprojNode);
    }
  
  tecproj[1] = new TecProjv1();
  if (tecproj[1])
    {
      PHObjectNode_t *tecprojNode =
	new PHObjectNode_t(tecproj[1],"TecProjBack","PHObject");
      tecNode->addNode(tecprojNode);
    }

  mTecCgl = new mTecCglModule;

  return 0;
}

int TecPidReco::process_event(PHCompositeNode *topNode) {
  mTecCgl->event(topNode);
  return 0;
}

int TecPidReco::ResetEvent(PHCompositeNode *topNode) {
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("TEC")) { mainIter.forEach(reset); }
  return 0;
}

