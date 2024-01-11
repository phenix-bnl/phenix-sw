#include <FillAccHits.h>
#include "setIntflag.h"

#include <AccCluster.h>
#include <AccSnglCluster.h>
#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>
#include <id_detector.h>
#include <CglTrack.h>
#include <Fun4AllReturnCodes.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <RunHeader.h>
#include <getClass.h>

#include <fstream>
#include <set>
#include <sstream>

#include <half/half.h>
#include <useInt.h>

using namespace std;

union floatint
{
  float    f32;
  int      i32;
};

FillAccHits::FillAccHits(const std::string &name): SubsysReco(name)
{
#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/fillacchits.dump");
#endif
  return;
}

int
FillAccHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

# ifdef useIntflag
  VariableArrayInt *acchit = new VariableArrayInt(6000);
#else
  VariableArray *acchit = new VariableArray(6000);
#endif

  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(acchit, "AccHit_VarArray", "PHObject");
  dstNode->addNode(PHObjectIONode);
  
  return EVENT_OK;
  
}

int
FillAccHits::process_event(PHCompositeNode *topNode)
{
  int isave=0;

  // Created in InitRun

#ifdef useIntflag
  VariableArrayInt *accarray = findNode::getClass<VariableArrayInt>(topNode, "AccHit_VarArray");
  vector<int> savethis;
#else
  VariableArray *accarray = findNode::getClass<VariableArray>(topNode, "AccHit_VarArray");
  vector<short int> savethis;
#endif

  if (!accarray)
    return 0;

  // Get the acc clusters - these are already associated with cgl tracks
  // The swapped tracks are in there too. cgl knows wich are real and swapped through
  // CglTrack::get_accrecid(icgl) and CglTrackBack::get_accrecid() which returns an acc
  // cluster index or -9999 if acc is not matched to the track. 
  // Will need to look at FillTrackHits.
  // AccCluster does not know which cgl track a cluster belongs to

  AccCluster* d_acccls = findNode::getClass<AccCluster> (topNode, "AccCluster");

  int nclusters = d_acccls->get_ncluster();

#ifdef DUMP
  dumpfile << "AccCluster has nclusters = " <<  nclusters << endl;
#endif

  for(int iclus=0;iclus<nclusters;iclus++)
    {

      AccSnglCluster *sngl = d_acccls->get_cluster(iclus);

      int hitid = sngl->get_aerhitid();
      int hitconfig = sngl->get_aerhitconfig();

      // Each individual cluster is loaded into the integer array in the format that AccHitMapEntry 
      // will expect on readback. The index isave keeps track of the entry number
      // We do not keep track of real or swapped here, that should be done in FillTrackHits?
      // To do it here we would need to check CglTrack and CglTrackBack to see if it is swapped or not

      //savethis.push_back(isave);
      savethis.push_back( hitid );
      savethis.push_back( hitconfig );

#ifdef DUMP
      dumpfile << "AccHitMapEntry: id: " << isave;
      dumpfile << "  Cluster " << iclus << " has hitid = " << hitid << " hitconfig " << hitconfig << endl;
#endif

      for (unsigned int ibox=0; ibox<4; ibox++)
	{
	  float ph1 = sngl->get_aerph1(ibox);
	  float ph2 = sngl->get_aerph2(ibox);
	  float t1 = sngl->get_aert1 (ibox);
	  float t2 = sngl->get_aert2 (ibox);

	  savethis.push_back(FloatToInt(ph1));
	  savethis.push_back(FloatToInt(ph2));
	  savethis.push_back(FloatToInt(t1));
	  savethis.push_back(FloatToInt(t2));
	}
      
      isave++;      
    }

  accarray->set_val(savethis);


  return EVENT_OK;
}

int
FillAccHits::End(PHCompositeNode *topNode)
{
#ifdef DUMP
  dumpfile.close();
#endif

  return 0;
}

#ifdef useIntflag
int
FillAccHits::FloatToInt(const float rval) const
{
  floatint fi;
  fi.f32 = rval;
  return fi.i32;
}
#else
short int
FillAccHits::FloatToInt(const float rval) const
{
  half ftoi(rval);
  return ftoi.bits();
}
#endif
