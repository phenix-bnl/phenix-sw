#include <FillDchHits.h>
#include "setIntflag.h"

#include <DchTrack.h>
#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>

#include <CglTrack.h>

#include <Fun4AllReturnCodes.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
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

FillDchHits::FillDchHits(const std::string &name): SubsysReco(name)
{
#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/filldchhits.dump");
#endif

  return;
}

int
FillDchHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

  ostringstream tmpstream;

#ifdef useIntflag
  VariableArrayInt *dchhit = new VariableArrayInt(3001);
#else
  VariableArray *dchhit = new VariableArray(3001);
#endif

  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(dchhit, "DchHit_VarArray", "PHObject");
  dstNode->addNode(PHObjectIONode);
  
  return EVENT_OK;
}

int
FillDchHits::process_event(PHCompositeNode *topNode)
{
  CglTrack *cgl[2];
  cgl[0] = findNode::getClass<CglTrack>(topNode, "CglTrack");
  cgl[1] = findNode::getClass<CglTrack>(topNode, "CglTrackBack");

  set<unsigned int> dchid;
  set<unsigned int>::const_iterator iter;

  for (int k = 0; k < 2 ;k++)
    {
      for (unsigned int i = 0; i < cgl[k]->get_CglNTrack(); i++)
        {
          if (cgl[k]->get_dctracksid(i) >= 0)
            {
              dchid.insert( cgl[k]->get_dctracksid(i));
            }
        }
    }

#ifdef useIntflag
  VariableArrayInt *dcharray = findNode::getClass<VariableArrayInt>(topNode, "DchHit_VarArray");
  vector<int> savethis;
#else
  VariableArray *dcharray = findNode::getClass<VariableArray>(topNode, "DchHit_VarArray");
  vector<short int> savethis;
#endif

  DchTrack *dchtrk = findNode::getClass<DchTrack>(topNode, "DchTrack");
  if (dcharray && dchtrk)
    {
      if (dchtrk->isValid())
        {

          for (iter = dchid.begin(); iter != dchid.end(); iter++)
            {
              savethis.push_back(*iter);
              savethis.push_back(dchtrk->get_arm(*iter));
              savethis.push_back(dchtrk->get_side(*iter));
              savethis.push_back(dchtrk->get_quality(*iter));
              savethis.push_back(FloatToInt(dchtrk->get_zed(*iter)));
              savethis.push_back(FloatToInt(dchtrk->get_phi(*iter)));
              savethis.push_back(FloatToInt(dchtrk->get_alpha(*iter)));
              savethis.push_back(FloatToInt(dchtrk->get_beta(*iter)));
              savethis.push_back(FloatToInt(dchtrk->get_phi0(*iter)));
              savethis.push_back(FloatToInt(dchtrk->get_theta0(*iter)));
              savethis.push_back(FloatToInt(dchtrk->get_momentum(*iter)));
              savethis.push_back(dchtrk->get_nx1hits(*iter));
              savethis.push_back(dchtrk->get_nx2hits(*iter));

#ifdef DUMP
              dumpfile << "DchHitMapEntry: id: " << *iter;
              dumpfile << ", arm: " << dchtrk->get_arm(*iter)
		       << ", side: " << dchtrk->get_side(*iter)
		       << ", quality: " << dchtrk->get_quality(*iter)
		       << endl;
#endif
            }
        }
      dcharray->set_val(savethis);
    }
  return EVENT_OK;
}

int
FillDchHits::End(PHCompositeNode *topNode)
{
#ifdef DUMP
  dumpfile.close();
#endif

  return 0;
}

#ifdef useIntflag
int FillDchHits::FloatToInt(const float rval) const
{
  floatint fi;
  fi.f32 = rval;
  return fi.i32;
}
#else
short int FillDchHits::FloatToInt(const float rval) const
{
  half ftoi(rval);
  return ftoi.bits();
}
#endif
