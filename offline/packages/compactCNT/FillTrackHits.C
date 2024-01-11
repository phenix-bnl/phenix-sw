#include <FillTrackHits.h>
#include <id_detector.h>
#include "setIntflag.h"

#include <vararray/VariableArray.h>
#include <CglTrack.h>
#include <CglTrackv6.h>

#include <Fun4AllReturnCodes.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

using namespace std;

FillTrackHits::FillTrackHits(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumpfile[0].open("/phenix/scratch/frawley/filltrackhits.dump");
  dumpfile[1].open("/phenix/scratch/frawley/filltrackbackhits.dump");
#endif

  outputnodename[0] = "CglTrackHits_VarArray";
  outputnodename[1] = "CglTrackBackHits_VarArray";
  return;
}

int
FillTrackHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  for (int i = 0; i < 2; i++)
    {
      VariableArray *cglhits = new VariableArray(10001);
      PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(cglhits, outputnodename[i].c_str(), "PHObject");
      dstNode->addNode(PHObjectIONode);
    }
  return EVENT_OK;
}

int
FillTrackHits::process_event(PHCompositeNode *topNode)
{
  static const string inputnodename[2] = {"CglTrack", "CglTrackBack"};

  for (int k = 0; k < 2; k++)
    {
      CglTrack *cgltrk = findNode::getClass<CglTrack>(topNode, inputnodename[k]);
      // We are storing a bunch of small integers here, can always use short ints
      VariableArray *trkhits = findNode::getClass<VariableArray>(topNode, outputnodename[k]);
      vector<short int> savethis;
      map<int, short> map_one_hit;
      map<int, short>::iterator iter;
      for (unsigned int i = 0; i < cgltrk->get_CglNTrack(); i++)
        {
          map_one_hit.clear();
          savethis.push_back(i);

#ifdef DUMP
          dumpfile[k] << "Track: " << i << " k " << k << endl;
#endif

	  if (cgltrk->get_accrecid(i) >= 0)
            {
              map_one_hit[id_detector::id_acc] = cgltrk->get_accrecid(i);
            }
          if (cgltrk->get_dctracksid(i) >= 0)
            {
              map_one_hit[id_detector::id_dch] = cgltrk->get_dctracksid(i);
            }
          if (cgltrk->get_emcclusid(i) >= 0)
            {
              map_one_hit[id_detector::id_emc] = cgltrk->get_emcclusid(i);
            }
          if (cgltrk->get_pc1clusid(i) >= 0)
            {
              map_one_hit[id_detector::id_pc1] = cgltrk->get_pc1clusid(i);
            }
          if (cgltrk->get_pc2clusid(i) >= 0)
            {
              map_one_hit[id_detector::id_pc2] = cgltrk->get_pc2clusid(i);
            }
          if (cgltrk->get_pc3clusid(i) >= 0)
            {
              map_one_hit[id_detector::id_pc3] = cgltrk->get_pc3clusid(i);
            }
          if (cgltrk->get_tectrackid(i) >= 0)
            {
              map_one_hit[id_detector::id_tec] = cgltrk->get_tectrackid(i);
            }
          if (cgltrk->get_tofrecid(i) >= 0)
            {
              map_one_hit[id_detector::id_tofe] = cgltrk->get_tofrecid(i);
            }
          if (cgltrk->get_tofwrecid(i) >= 0)
            {
              map_one_hit[id_detector::id_tofw] = cgltrk->get_tofwrecid(i);
            }
          if (cgltrk->get_richringid(i) >= 0)
            {
              map_one_hit[id_detector::id_crk] = cgltrk->get_richringid(i);
            }
	  // This svx call should only be made when cgltrk is a PHTrackv6 object
	  // Otherwise it produces a virtual method warning every time. The dynamic 
	  // cast returns a null pointer if it not a v6 object
	  CglTrackv6 *tmp = dynamic_cast <CglTrackv6 *> (cgltrk);
	  if(tmp)
	    {
	      if (cgltrk->get_svxclusid(i,0) >= 0)
		{
		  map_one_hit[id_detector::id_svx0] = cgltrk->get_svxclusid(i,0);
		}
	      if (cgltrk->get_svxclusid(i,1) >= 1)
		{
		  map_one_hit[id_detector::id_svx1] = cgltrk->get_svxclusid(i,1);
		}
	      if (cgltrk->get_svxclusid(i,2) >= 2)
		{
		  map_one_hit[id_detector::id_svx2] = cgltrk->get_svxclusid(i,2);
		}
	      if (cgltrk->get_svxclusid(i,3) >= 3)
		{
		  map_one_hit[id_detector::id_svx3] = cgltrk->get_svxclusid(i,3);
		}
	      // SVX tracks are special, they know with which cgl track they are associated.
	      // This association is done in svxcgl package.
	      // Thus, there is no need to save them here. 
	    }

	  savethis.push_back(map_one_hit.size());
          for (iter = map_one_hit.begin(); iter != map_one_hit.end(); iter++)
            {
              savethis.push_back(iter->first);
              savethis.push_back(iter->second);
#ifdef DUMP
              dumpfile[k] << "  id: " << iter->first << ", hitid: " << iter->second << endl;
#endif

            }
        }
      trkhits->set_val(savethis);
    }



  return EVENT_OK;
}

int
FillTrackHits::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  for (int k = 0; k < 2;k++)
    {
      dumpfile[k].close();
    }
#endif

  return 0;
}
