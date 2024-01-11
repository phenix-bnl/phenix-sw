#include <FillTecHits.h>
#include "setIntflag.h"

#include <vararray/VariableArray.h>
#include <vararray/VariableArrayIds.h>
#include <VariableArrayInt.h>
#include <TecClusterContainer.hh>
#include <TecProjv1.hh>

#include <Fun4AllReturnCodes.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <half/half.h>
#include <useInt.h>

#include <set>
#include <sstream>

using namespace std;

union floatint
{
  float    f32;
  int      i32;
};

FillTecHits::FillTecHits(const std::string &name): SubsysReco(name)
{
#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/filltechits.dump");
#endif

  return;
}

int
FillTecHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

  ostringstream tmpstream;
// defs are from VariableArrayIds.h, each detector should have a unique id
#ifdef useIntflag
  VariableArrayInt *techit = new VariableArrayInt(varids::TECHITV1);
#else
  VariableArray *techit = new VariableArray(varids::TECHITV1);
#endif

  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(techit, "TecHit_VarArray", "PHObject");
  dstNode->addNode(PHObjectIONode);

  return EVENT_OK;
}

int
FillTecHits::process_event(PHCompositeNode *topNode)
{

  TecProj *tecproj[2];
  tecproj[0] = findNode::getClass<TecProj>(topNode, "TecProj");
  tecproj[1] = findNode::getClass<TecProj>(topNode, "TecProjBack");

  set<int> tecid;

  // There can be multiple TEC planes associated with a given track
  // We want to capture the cluster info for every plane associated with every track
  // Make a list of all clusters that are associated with tracks
  // We can most conveniently get this from TecProj and TecProjBack, where it was written during 
  // track association

#ifdef DUMP
  dumpfile << "List of clusters from TecProj:" << endl;
#endif
  for (int k = 0; k < 2 ;k++)
    {
#ifdef DUMP
      dumpfile << "k " << k << " TecNProj " << tecproj[k]->get_TecNProj() << endl;
#endif
      for (unsigned int i = 0; i < tecproj[k]->get_TecNProj(); i++)
        {
	  for (int iplane=0; iplane<6; iplane++) 
	    if (tecproj[k]->get_teclusterid(i,iplane) >= 0)
	      {
		tecid.insert( tecproj[k]->get_teclusterid(i,iplane));
#ifdef DUMP
		dumpfile << "  k " << k
			 << " iproj " << i
			 << " iplane " << iplane
			 << " clusterid " << tecproj[k]->get_teclusterid(i,iplane)
			 << endl;
#endif
	      }
        }
    }

#ifdef DUMP
  dumpfile << "Add Tec hits to array:" << endl;
#endif 

#ifdef useIntflag
  VariableArrayInt *tecarray;
  tecarray = findNode::getClass<VariableArrayInt>(topNode, "TecHit_VarArray");
  vector<int> savethis;
#else
  VariableArray *tecarray;
  tecarray = findNode::getClass<VariableArray>(topNode, "TecHit_VarArray");
  vector<short int> savethis;
#endif

  TecClusterContainer *tecraw = findNode::getClass<TecClusterContainer>(topNode, "TecClusterContainer");

  if (tecarray && tecraw)
    {
      if (tecraw->isValid())
        {
          for (unsigned int ih = 0; ih < (unsigned int) tecraw->getNClusters(); ih++)
            {
              if (tecid.find(ih) != tecid.end())
                {
                  savethis.push_back(ih);
		  // the parameter "index" contains enough info to recover side and sector
                  savethis.push_back(tecraw->getTecCluster(ih)->get_index());
                  savethis.push_back(tecraw->getTecCluster(ih)->get_wire());
		  savethis.push_back(tecraw->getTecCluster(ih)->get_avgtime());
                  savethis.push_back(tecraw->getTecCluster(ih)->get_ntimebins());				    
                  //fi.f32 = tecraw->getTecCluster(ih)->get_charge();
		  float charge = tecraw->getTecCluster(ih)->get_charge();
                  savethis.push_back(FloatToInt(charge));
#ifdef DUMP
                  dumpfile << "TecHitMapEntry: id: " << ih;
		  dumpfile << " plane " << tecraw->getTecCluster(ih)->get_index()
			   << " wire=" << tecraw->getTecCluster(ih)->get_wire() 
			   << " <timebin>=" << tecraw->getTecCluster(ih)->get_avgtime()
			   << " n time bins=" << tecraw->getTecCluster(ih)->get_ntimebins()
			   << " charge=" << tecraw->getTecCluster(ih)->get_charge() << endl;
#endif
                }
            }
        }
      tecarray->set_val(savethis);
    }
  return EVENT_OK;
}

int
FillTecHits::End(PHCompositeNode *topNode)
{
#ifdef DUMP
  dumpfile.close();
#endif

  return 0;
}

#ifdef useIntflag
int
FillTecHits::FloatToInt(const float rval) const
{
  floatint fi;
  fi.f32 = rval;
  return fi.i32;
}
#else
short int
FillTecHits::FloatToInt(const float rval) const
{
  half ftoi(rval);
  return ftoi.bits();
}
#endif
