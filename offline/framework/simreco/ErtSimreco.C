#include "ErtSimreco.h"


using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> PHTableNode_t;
typedef PHIODataNode<TObject> TObjectNode_t;
typedef PHIODataNode<RunHeader> RunHeaderNode_t;
typedef PHIODataNode<ErtOut> ErtOutNode_t;
typedef PHIODataNode<CrkHit> CrkHitNode_t;

long ErtPutDCM(PHCompositeNode* topNode);


ErtSimreco::ErtSimreco(const char *name, const int run, const char *node_name )
{
  ThisName = name;
  RunNumber = run;
  NodeName = node_name;

  ertsim    = NULL;
}



int ErtSimreco::Init(PHCompositeNode *topNode)
{
  return 0;
}



int ErtSimreco::InitRun(PHCompositeNode *topNode)
{
  CreateNodeTree(topNode);

  return 0;
}



int ErtSimreco::CreateNodeTree(PHCompositeNode *topNode)
{
  //recoConsts *rc = recoConsts::instance();

  PHNodeIterator iter(topNode);

  PHCompositeNode *dstNode = 
    static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
  {
    cout << PHWHERE << "DST Node missing doing nothing" << endl;
    return -1;
  }

  PHCompositeNode *parNode = 
    static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PAR"));
  if (!parNode)
  {
    cout << PHWHERE << "PAR Node missing doing nothing" << endl;
    return -1;
  }

  PHCompositeNode *dcmNode = 
    static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DCM"));
  if (!dcmNode)
  {
    cout << PHWHERE << "DCM Node missing doing nothing" << endl;
    return -1;
  }

  PHCompositeNode *evaNode = 
    static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "EVA"));
  if (!evaNode)
  {
    cout << PHWHERE << "EVA Node missing doing nothing" << endl;
    return -1;
  }

  PHCompositeNode *geaNode = 
    static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "GEA"));
  if (!geaNode)
  {
    cout << PHWHERE << "GEA Node missing doing nothing" << endl;
    return -1;
  }
  
//___________________________________________________________ ErtNode
  PHCompositeNode* ertNode = new PHCompositeNode("ERT");
  topNode->addNode(ertNode);

  dErtFemDataWrapper *dErtFemData 
                 = new dErtFemDataWrapper("dErtFemData", 240);
  PHIODataNode<PHTable> *dErtFemDataNode 
                 = new PHIODataNode<PHTable>(dErtFemData , "dErtFemData");
  ertNode->addNode(dErtFemDataNode);

  ErtOut *ertout = new ErtOutv1();
  PHIODataNode<PHObject>* ErtOutNode =
    new PHIODataNode<PHObject>(ertout, NodeName.Data(), "PHObject");
  dstNode->addNode(ErtOutNode);

  ertsim = new ERTSimulator();
  ertsim->SetRunNumber(RunNumber);
  ertsim->SetNodeName(NodeName.Data());
  ertsim->FetchSMEff(RunNumber);
  ertsim->FetchSector(RunNumber);

  return 0;
}



int ErtSimreco::process_event(PHCompositeNode *topNode)
{
  //ertsim->DecisionMaking(topNode);
  //Function of process events 

  ertsim->EventLoopforCRK(topNode); 
  ertsim->EventLoopforEMC(topNode); 

  //ertsim->Decode(topNode); 

  ertsim->DstStore(topNode);
  //Storage intro DST 

  return 0;
}



int ErtSimreco::ResetEvent(PHCompositeNode *topNode)
{
  //  Nothing in main Reco, so nothing here...
  return 0;
}
