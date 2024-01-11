#include <FillEmcHits.h>
#include "setIntflag.h"

#include <emcClusterContainer.h>
#include <emcClusterContent.h>
#include <emcTowerContainer.h>
#include <emcTowerContent.h>
#include <EmcIndexer.h>

#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>


#include <CglTrack.h>


#include <Fun4AllReturnCodes.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <half/half.h>
#include <useInt.h>

#include <fstream>
#include <set>
#include <sstream>

using namespace std;

union floatint
{
  float    f32;
  int      i32;
};

FillEmcHits::FillEmcHits(const std::string &name): SubsysReco(name)
{
#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/fillemchits.dump");
#endif

  return;
}

int
FillEmcHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  
#ifdef useIntflag
  VariableArrayInt *emchit = new VariableArrayInt(6000);
#else
  VariableArray *emchit = new VariableArray(6000);
#endif
    
  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(emchit, "EmcHit_VarArray", "PHObject");
  dstNode->addNode(PHObjectIONode);

  return EVENT_OK;
}

int
FillEmcHits::process_event(PHCompositeNode *topNode)
{
  CglTrack *cgl[2];
  cgl[0] = findNode::getClass<CglTrack>(topNode, "CglTrack");
  cgl[1] = findNode::getClass<CglTrack>(topNode, "CglTrackBack");
  set<unsigned int> emcid;
  set<unsigned int>::const_iterator iter;
  for (int k = 0; k < 2 ;k++)
    {
      for (unsigned int i = 0; i < cgl[k]->get_CglNTrack(); i++)
        {
#ifdef DUMP
	  dumpfile << "k " << k << " emcclusid from cgl: " << cgl[k]->get_emcclusid(i) << endl;
#endif

          if (cgl[k]->get_emcclusid(i) >= 0)
            {

              emcid.insert( cgl[k]->get_emcclusid(i));
            }
        }
    }

#ifdef useIntflag
  VariableArrayInt *emcarray = findNode::getClass<VariableArrayInt>(topNode, "EmcHit_VarArray");
  vector<int> savethis;
#else
  VariableArray *emcarray = findNode::getClass<VariableArray>(topNode, "EmcHit_VarArray");
  vector<short int> savethis;
#endif

  emcClusterContainer *emccont = findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");
  emcTowerContainer *emctwr = findNode::getClass<emcTowerContainer>(topNode, "emcTowerContainer");
  if (emcarray && emccont)
    {
      if (emccont->isValid())
        {
          for (iter = emcid.begin(); iter != emcid.end(); iter++)
            {
              emcClusterContent *clus = emccont->getCluster(*iter);
              savethis.push_back(*iter);
              int emcarm = clus->arm   ();
              int emcsec = clus->sector();
              int emciy  = clus->iypos ();
              int emciz  = clus->izpos ();
              int TowerID = EmcIndexer::TowerID(emcarm, emcsec, emciy, emciz);
              emcTowerContent* twr = emctwr->findTower(TowerID);
	      savethis.push_back(twr->TDC());
	      if (twr->hasRaw())
		{
		  savethis.push_back(twr->HGPP());
		  savethis.push_back(twr->LGPP());
		}
	      else
		{
		  savethis.push_back(0);
		  savethis.push_back(0);
		}

#ifdef DUMP
              dumpfile << "EmcHitMapEntry: id: " << *iter;
              dumpfile << ", emcrawtdc: " << twr->TDC()
		       << ", emcrawadc: " << twr->HGPP()
		       << ", emcrawadclg: " << twr->LGPP()
		       << endl;
#endif
            }
        }
      emcarray->set_val(savethis);
    }
  return EVENT_OK;
}

int
FillEmcHits::End(PHCompositeNode *topNode)
{
#ifdef DUMP
  dumpfile.close();
#endif

  return 0;
}

#ifdef useIntflag
int FillEmcHits::FloatToInt(const float rval) const
{
  floatint fi;
  fi.f32 = rval;
  return fi.i32;
}
#else
short int FillEmcHits::FloatToInt(const float rval) const
{
  half ftoi(rval);
  return ftoi.bits();
}
#endif
