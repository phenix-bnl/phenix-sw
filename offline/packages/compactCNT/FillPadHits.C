#include <FillPadHits.h>
#include "setIntflag.h"

#include <PadHitMapEntry.h>
#include <PadCluster.h>
#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>


#include <CglTrack.h>
#include <PHTrackOut.h>


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

#define SAVEALLPC3HITS

using namespace std;

union floatint
{
  float    f32;
  int      i32;
};

FillPadHits::FillPadHits(const std::string &name): SubsysReco(name)
{
#ifdef DUMP
  dumpfile[0].open("/phenix/scratch/frawley/fillpc1hits.dump");
  dumpfile[1].open("/phenix/scratch/frawley/fillpc2hits.dump");
  dumpfile[2].open("/phenix/scratch/frawley/fillpc3hits.dump");
#endif

  return;
}

int
FillPadHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

  ostringstream tmpstream;

#ifdef useIntflag
  VariableArrayInt *padhit;
#else
  VariableArray *padhit;
#endif

  PHIODataNode<PHObject> *PHObjectIONode;
  for (short int j = 1; j <= 3 ; j++)
    {
      tmpstream.str(""); // reset tmpstream
      tmpstream << "Pc" << j << "Hit_VarArray";
      string NodeName = tmpstream.str();
#ifdef useIntflag
      padhit = new VariableArrayInt(4000);
#else
      padhit = new VariableArray(4000);
#endif
      
      PHObjectIONode = new PHIODataNode<PHObject>(padhit, NodeName.c_str(), "PHObject");
      dstNode->addNode(PHObjectIONode);

      //padhit->identify();
    }

#ifdef SAVEALLPC3HITS
  cout << "SAVEALLPC3HITS is set" << endl;
#endif

  return EVENT_OK;
}

int
FillPadHits::process_event(PHCompositeNode *topNode)
{
  CglTrack *cgl[2];
  cgl[0] = findNode::getClass<CglTrack>(topNode, "CglTrack");
  cgl[1] = findNode::getClass<CglTrack>(topNode, "CglTrackBack");
  set<int> pcid[3];
  for (int k =0; k<2;k++)
    {
      for (unsigned int i = 0; i < cgl[k]->get_CglNTrack(); i++)
	{
	  if (cgl[k]->get_pc1clusid(i) >= 0)
	    {
	      pcid[0].insert( cgl[k]->get_pc1clusid(i));
	    }
	  if (cgl[k]->get_pc2clusid(i) >= 0)
	    {
	      pcid[1].insert( cgl[k]->get_pc2clusid(i));
	    }
	  if (cgl[k]->get_pc3clusid(i) >= 0)
	    {
	      pcid[2].insert( cgl[k]->get_pc3clusid(i));
	    }
	}
    }
  ostringstream PHit, PhobjectNodeName;
#ifdef useIntflag
  VariableArrayInt *ph[3];
  vector<int> savethis;
#else
  VariableArray *ph[3];
  vector<short int> savethis;
#endif

  for (short int j = 1; j <= 3; j++)
    {
      savethis.clear();
      PHit.str("");
      PhobjectNodeName.str("");
      PhobjectNodeName << "Pc" << j << "Cluster";
      PHit << "Pc" << j << "Hit_VarArray";
#ifdef useIntflag 
     ph[j-1] = findNode::getClass<VariableArrayInt>(topNode, PHit.str().c_str());
#else
     ph[j-1] = findNode::getClass<VariableArray>(topNode, PHit.str());
#endif
      PadCluster *padcluster = findNode::getClass<PadCluster>(topNode, PhobjectNodeName.str());
      if (ph[j-1] && padcluster)
        {
          if (padcluster->isValid())
            {
              for (unsigned int ih = 0; ih < padcluster->get_PadNCluster(); ih++)
                {
                  if (pcid[j-1].find(padcluster->get_id(ih)) != pcid[j-1].end())
                    {
#ifdef DUMP
                      dumpfile[j-1] << "PadHitMapEntry: id: " << padcluster->get_id(ih);
                      string coo[3] = {", x: ", ", y: ", ", z: "};
#endif
                      savethis.push_back(padcluster->get_id(ih));
                      for (int k = 0;k < 3;k++)
                        {
			  float xyz = padcluster->get_xyz(ih, k);
#ifdef DUMP
                          dumpfile[j-1] << coo[k] << padcluster->get_xyz(ih, k);
#endif
                          savethis.push_back(FloatToInt(xyz));
                        }
#ifdef DUMP
                      dumpfile[j-1] << endl;
#endif
                    }
#ifdef SAVEALLPC3HITS
		  else if(j==3)    // take all PC3 hits regardless if this is defined
		    {
		      savethis.push_back(padcluster->get_id(ih));
                      for (int k = 0;k < 3;k++)
                        {
			  float xyz = padcluster->get_xyz(ih, k);
                          savethis.push_back(FloatToInt(xyz));
                        }
		    }
#endif
                }
            }
        }
      ph[j-1]->set_val(savethis);
    }
  return EVENT_OK;
}

int
FillPadHits::End(PHCompositeNode *topNode)
{
#ifdef DUMP
  for (int i = 0; i < 3;i++)
    {
      dumpfile[i].close();
    }
#endif

  return 0;
}

#ifdef useIntflag
int FillPadHits::FloatToInt(const float rval) const
{
  floatint fi;
  fi.f32 = rval;
  return fi.i32;
}
#else
short int FillPadHits::FloatToInt(const float rval) const
{
  half ftoi(rval);
  return ftoi.bits();
}
#endif
