#include "EmcEmbedReader.h"

#include <cassert>
#include <iostream>
//INCLUDECHECKER: Removed this line: #include <iomanip>
#include <string>

//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "Fun4AllServer.h"
#include "getClass.h"
#include "TH1.h"
#include "Counters.h"

#include "fkinDumper.h"
#include "PHGlobal.h"
#include "VtxOut.h"

namespace
{
  void fill(const emcClusterContainer& cc, const char* histo)
  {
    Fun4AllServer* se = Fun4AllServer::instance();
    TH1* h = dynamic_cast<TH1*>(se->getHisto(histo));
    assert(h!=0);

    for ( size_t i = 0; i < cc.size(); ++i ) 
      {
	const emcClusterContent* c = cc.getCluster(i);
	assert(c!=0);
	h->Fill(c->ecore());
      }
  }
  
  const char* kEventsSeen = "Events seen";
}

//_____________________________________________________________________________
EmcEmbedReader::EmcEmbedReader(const char* merged /*="MERGED"*/,
			       const char* top /*="TOP"*/) 
  : SubsysReco("EmcEmbedReader"),
    fMergedNode(merged),
    fTopNode(top),
    fCounters(0)
{
  fCounters = new Counters;
}

EmcEmbedReader::~EmcEmbedReader()
{
  delete fCounters;
}

//_____________________________________________________________________________
int
EmcEmbedReader::Init(PHCompositeNode*)
{
  fCounters->add(kEventsSeen);

  Fun4AllServer* se = Fun4AllServer::instance();

  TH1* h = new TH1F("simEnergy","Simulated Clusters e() distribution",
		    100,0,10);

  se->registerHisto(h->GetName(),h);

  h = new TH1F("mergedEnergy","Merged Clusters e() distribution",
	       100,0,10);

  se->registerHisto(h->GetName(),h);

  return 0;
}

//_____________________________________________________________________________
int
EmcEmbedReader::process_event(PHCompositeNode*)
{
  Fun4AllServer* se = Fun4AllServer::instance();

  fCounters->incr(kEventsSeen);

  fCounters->print();

  PHCompositeNode* topNode = se->topNode(fTopNode.c_str());
  assert(topNode!=0);

  PHCompositeNode* mergedNode = se->topNode(fMergedNode.c_str());

  emcClusterContainer* mergedclusters = 
    findNode::getClass<emcClusterContainer>(mergedNode,"emcClusterContainer");
  assert(mergedclusters!=0);
 
  emcClusterContainer* simuclusters = 
    findNode::getClass<emcClusterContainer>(mergedNode,"emcSimClusterContainer");
  assert(simuclusters!=0);

  emcClusterContainer* realclusters = 
    findNode::getClass<emcClusterContainer>(topNode,"emcClusterContainer");
  assert(realclusters!=0);

 VtxOut* vtxout = findNode::getClass<VtxOut>(mergedNode,"VtxOut");
 assert(vtxout!=0);
 PHGlobal* phglobal = findNode::getClass<PHGlobal>(mergedNode,"PHGlobal");
 assert(phglobal!=0);

 std::cout << PHWHERE << std::endl
	   << " nclusters : real = " 
	   << realclusters->size() 
	   << " merged = " 
	   << mergedclusters->size()
	   << " simu = " 
	   << simuclusters->size()
	   << " zreal="
	   << phglobal->getBbcZVertex()
	   << " zsimu="
	   << vtxout->get_ZVertex()
	   << std::endl;

  fill(*simuclusters,"simEnergy");
  fill(*mergedclusters,"mergedEnergy");

  primaryWrapper* primary = findNode::getClass<primaryWrapper>(mergedNode,"primary");
  assert(primary!=0);
  fkinDumper::dump(*primary,"PRIMARY");
 
  fkinWrapper* fkin = findNode::getClass<fkinWrapper>(mergedNode,"fkin");
  assert(fkin!=0);
  fkinDumper::dump(*fkin,"FKIN");

//   std::cout << "simuclusters=" << std::endl;
//   simuclusters->print();

//   std::cout << "mergedlusters=" << std::endl;
//   mergedclusters->print();

  return 0;
}
