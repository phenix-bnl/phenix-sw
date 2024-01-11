#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <Lvl2Event.h>
#include <Lvl2OutArrayv2.h>
#include <Lvl2DecisionOutv1.h>
#include <TrigRunLvl2.h>
#include <recoConsts.h>
#include <Lvl2Reco.h>
#include <recoConsts.h>

using namespace std;

Lvl2Reco::Lvl2Reco(const string &name): SubsysReco(name)
{
  nevt = 0;
  initdone = 0;
  lvl2copyonly = 0;
  mLvl2Event = 0;
  return ;
}

Lvl2Reco::~Lvl2Reco()
{
  delete mLvl2Event;
  return;
}

int Lvl2Reco::Init(PHCompositeNode *topNode)
{
  // Objectivity is no longer available
  // Offline Lvl2 reads database files from an AFS  disk directory at present, make sure
  // that the appropriate flags are set

  // First implementation of Lvl2 DB access from postgres.
  // Xiaochun He, Jun Ying and Christopher Cleven
  // 11/15/2004

  recoConsts *rc = recoConsts::instance();

  // create the required DST nodes to write Lvl2 results to.
  // These are needed whether we run triggers or just get packets
  // from the PRDF file

  int iret = CreateNodeTree(topNode);
  if (iret)
    {
      cout << PHWHERE << "Unable to create node tree, quit!" << endl;
      return iret;
    }

  // Now initialize offline/packages/Lvl2Event
  // This initializes Lvl2 with triggers turned on if requested,
  // otherwise it just copies the L2Decision packet over

  if (rc->get_IntFlag("FORCE_LVL2") == 1)
    {
      mLvl2Event = new Lvl2Event(true);
    }
  else
    {
      mLvl2Event = new Lvl2Event(false);
    }

  return 0;
}

int Lvl2Reco::CreateNodeTree(PHCompositeNode *topNode)
{
  // find the dstNode
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = NULL;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << "Lvl2Reco::Init: Did not find a DST node, create one! " << endl;
      dstNode = new PHCompositeNode("DST");
      topNode->addNode(dstNode);
    }

  // The level 2 results go into two nodes, one to contain the decision
  // and one to contain the primitives
  // If the triggers are to be run in offline, make separate nodes for
  // the results. L2Decision and Lvl2OutArray are reserved for DAQ level
  // 2 decision and primitive data.

  // Lvl2 decision node
  recoConsts *rc = recoConsts::instance();
  if (!rc->FlagExist("LVL2_INITIALIZE"))
    {
      rc->set_IntFlag("LVL2_INITIALIZE", 1);
    }

  Lvl2DecisionOut *decisionOut = new Lvl2DecisionOutv1();
  PHIODataNode<PHObject> *Lvl2DecisionNode =
    new PHIODataNode<PHObject>(decisionOut, "L2Decision", "PHObject");
  cout << " Lvl2DecisionNode = " << Lvl2DecisionNode  << endl;
  dstNode->addNode(Lvl2DecisionNode);

  if (rc->get_IntFlag("FORCE_LVL2") == 1)
    {
      Lvl2DecisionOut *decisionOutCal = new Lvl2DecisionOutv1();
      PHIODataNode<PHObject> *Lvl2DecisionNodeCal =
        new PHIODataNode<PHObject>(decisionOutCal, "L2DecisionCal", "PHObject");
      cout << " Lvl2DecisionNodeCal = " << Lvl2DecisionNodeCal  << endl;
      dstNode->addNode(Lvl2DecisionNodeCal);
    }

  // Lvl2 array node
  cout << "Creating Lvl2OutArrayv2 object named Lvl2OutArray" << endl;
  Lvl2OutArray *lvl2out = new Lvl2OutArrayv2();
  PHIODataNode<PHObject> *Lvl2OutNode =
    new PHIODataNode<PHObject>(lvl2out, "Lvl2OutArray", "PHObject");
  dstNode->addNode(Lvl2OutNode);

  if (rc->get_IntFlag("FORCE_LVL2") == 1)
    {
      cout << "Creating Lvl2OutArrayv2 object named Lvl2OutArrayCal" << endl;
      Lvl2OutArray *lvl2outCal = new Lvl2OutArrayv2();
      PHIODataNode<PHObject> *Lvl2OutNodeCal =
        new PHIODataNode<PHObject>(lvl2outCal, "Lvl2OutArrayCal", "PHObject");
      dstNode->addNode(Lvl2OutNodeCal);
    }

  return 0;
}

int Lvl2Reco::process_event(PHCompositeNode *topNode)
{
  // verbosity is defined in the SubsysReco class

  if (verbosity > 1)
    {
      cout << "Entering Lvl2Reco::process_event" << " initdone = " << initdone << endl;
    }

  recoConsts *rc = recoConsts::instance();
  int dump_statistics_interval = 1000;

  nevt++;

  if (nevt % dump_statistics_interval == 0 && verbosity > 0)
    {
      cout << "Lvl2Reco: Nevts = " << nevt << endl;
    }

  if (initdone == 0)
    {
      lvl2copyonly = 1;

      if (rc->get_IntFlag("FORCE_LVL2") == 1)
        {
          // Run triggers in offline

          if (verbosity > 0)
            {
              cout << " Lvl2Reco: Will force running of lvl2 triggers in offline no matter what"
		   << endl;
            }
          // Make the Lvl2TrigRun object for running the triggers - it has to
          // have a different name because there will also be a Lvl2TrigRun from
          // the actual run in the database, so will call it Lvl2TrigRunCal.

          MakeLvl2TrigRunCal(topNode);

          // Set a flag so that triggers get run
          lvl2copyonly = 0;
        }

      // Will in any case Find lvl2 decision packet in PRDF and move lvl2 results to DST
      if (verbosity > 0)
        {
          cout << " Lvl2Reco: Will copy lvl2 results from PRDF to DST"
	       << endl;
        }

      initdone = 1;
    }

  // Transfer the Lvl2 results from the PRDF, if they are there
  // This writes to the L2Decision and Lvl2OutArray nodes

  if (mLvl2Event->SaveToDST(topNode))
    {
      if (verbosity > 0)
        {
          cout << " Copied lvl2 results from PRDF to DST" << endl;
        }
    }
  else
    {
      if (verbosity > 0)
        {
          cout << " This event had no Lvl2 dDecision packet in the PRDF!" << endl;
        }
    }

  if (lvl2copyonly == 0)
    {
      // Run the triggers in offline
      // The results will be written to L2DecisionCal and Lvl2OutArrayCal

      if (verbosity > 0)
        {
          cout << " Lvl2Reco: Run lvl2 triggers on the data" << endl;
        }

      mLvl2Event->RunL2TriggersPRDFToDST(topNode);

      // This is because we cannot call an end of run method!
      if (nevt % dump_statistics_interval == 0 && verbosity > 0)
        {
          mLvl2Event->DumpL2Statistics();
        }
    }

  return 0;
}

int Lvl2Reco::MakeLvl2TrigRunCal(PHCompositeNode *topNode)
{

  // find the runNode
  PHNodeIterator iter(topNode);
  PHCompositeNode *runNode = NULL;
  runNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "RUN"));
  if (!runNode)
    {
      cout << "Lvl2RunTrigSelect::process_event: No RUN node, abort!"
	   << endl;
      return 1;
    }

  // Is there a TrigRunLvl2Cal object? If not, add it.

  PHTypedNodeIterator<TrigRunLvl2> run2_uditer(runNode);
  PHIODataNode<TrigRunLvl2> * TrigRunLvl2Node = run2_uditer.find("TrigRunLvl2Cal");
  if (!TrigRunLvl2Node)
    {
      cout << " Adding TrigRunLvl2Cal object to RUN node" << endl;
      mLvl2Event->WriteTrigRunLvl2Cal(topNode);
    }
  return 0;
}

int Lvl2Reco::EndRun(const int runno)
{
  mLvl2Event->DumpL2Statistics();
  return 0;
}
