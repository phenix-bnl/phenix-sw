#include "PrdfSimreco.h"
#include "Fun4AllServer.h"

#include "PHCompositeNode.h"
#include "PHRawOManager.h"

#include <cstdlib>

using namespace std;


PrdfSimreco::PrdfSimreco(const string &name, const char *prdfOutputFile, const int runNumb): SubsysReco(name)
{

  prdfOut = new PHRawOManager(prdfOutputFile,runNumb,8*1024*1024/4);
  std::cout << "\n   PrdfSimreco <I>: Output PRDF name = " << prdfOutputFile;
  std::cout << ",    using Run Number " << runNumb << std::endl;

  //
  // Insure that SIMPRDF node is created before other subsystems try to use it
  //
  Fun4AllServer *se = Fun4AllServer::instance();
  PHCompositeNode *trueTop = se->topNode();
  if(!trueTop) {
    cout << "\n PrdfSimreco::Constructor <E>  cannot find topNode \n";
    exit(1);
  }

  prdfNode = new PHCompositeNode("SIMPRDF");
  trueTop->addNode(prdfNode);
}

PrdfSimreco::~PrdfSimreco() {

  if(prdfOut) {
    delete prdfOut;  // this is the way to close the output file
    std::cout << "\n   Closing simulation PRDF file" << std::endl;
  } // safety check
  else {
    std::cout << "\n   Simulation PRDF file is missing" << std::endl;
  }

}

int PrdfSimreco::process_event(PHCompositeNode *topNode)
{
  static int iEvent = 0;
  iEvent++;

  std::cout << "\n PrdfSimreco::process_event <I>: writing event " << iEvent;
  std::cout << ";  prdfNode = " << prdfNode << std::endl;

  PHBoolean prdfStatus = prdfOut->write(prdfNode);
  if(!prdfStatus) {
    std::cout << "\n PrdfSimreco::process_event <I>: prdfStatus problem at event " << iEvent << "\n" << std::endl;
  }

  return 0;
}

int PrdfSimreco::ResetEvent(PHCompositeNode *topNode)
{
  return 0;
}
