#include "McEvalSimreco.h"
#include <mNewDchEvaluator.hh>
#include <mCentralTrackEvaluator_v1.hh>
#include <McEvalSingleList_v1.h>

#include <Fun4AllReturnCodes.h>

#include <PHIODataNode.h>
#include <PHNodeIterator.h>

using namespace std;

McEvalSimreco::McEvalSimreco(const string &name): 
  SubsysReco(name),
  mEvaluate(NULL),
  mNewDchEvaluate(NULL)
{
  return;
}

McEvalSimreco::~McEvalSimreco()
{
  delete mEvaluate;
  delete mNewDchEvaluate;
  return;
}

int 
McEvalSimreco::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *evaNode = 
  dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","EVA"));  

 McEvalSingleList_v1* mcsingle = new McEvalSingleList_v1();
  PHIODataNode<PHObject>* mcNode =
    new PHIODataNode<PHObject>(mcsingle, "McSingle", "PHObject");
  evaNode->addNode(mcNode);

  mEvaluate       = new mCentralTrackEvaluator_v1();
  mEvaluate->set_verbose(verbosity);
  mNewDchEvaluate = new mNewDchEvaluator;
  return EVENT_OK;
}

int McEvalSimreco::process_event(PHCompositeNode *topNode) {
  /*  mNewDchEvaluate->event(topNode);
  mNewDchEvaluate->associatePC(topNode, 1);
  mNewDchEvaluate->associatePC(topNode, 2);
  mNewDchEvaluate->associatePC(topNode, 3);
  mNewDchEvaluate->associateTOF(topNode);
  mNewDchEvaluate->associateEMC(topNode);
  mNewDchEvaluate->associateCRK(topNode);
  mNewDchEvaluate->extrapolateToVertex(topNode);
  mNewDchEvaluate->associateMatching();
  mNewDchEvaluate->associateDchExt(topNode);
  */

  mEvaluate->event(topNode);
  mEvaluate->associatePC(topNode, 1);
  mEvaluate->associatePC(topNode, 2);
  mEvaluate->associatePC(topNode, 3);
  mEvaluate->associateTOF(topNode);
  mEvaluate->associateEMC(topNode);
  mEvaluate->associateCRK(topNode);
  return EVENT_OK;
}
