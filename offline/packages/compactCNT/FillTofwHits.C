#include <FillTofwHits.h>
#include "setIntflag.h"

#include <TofwHit.h>
#include <T0Out.h>
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


FillTofwHits::FillTofwHits(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/filltofwhits.dump");
#endif

  return;
}

int
FillTofwHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

  ostringstream tmpstream;

#ifdef useIntflag
  VariableArrayInt *tofwhit = new VariableArrayInt(6000);
#else
  VariableArray *tofwhit = new VariableArray(6000);
#endif

  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(tofwhit, "TofwHit_VarArray", "PHObject");
  dstNode->addNode(PHObjectIONode);
  
  return EVENT_OK;
}

int
FillTofwHits::process_event(PHCompositeNode *topNode)
{
  CglTrack *cgl[2];
  cgl[0] = findNode::getClass<CglTrack>(topNode, "CglTrack");
  cgl[1] = findNode::getClass<CglTrack>(topNode, "CglTrackBack");
  T0Out *t0 = findNode::getClass<T0Out>(topNode, "T0Out");
  float TimeZero = t0->get_T0();
  set<int> tofwid;
  for (int k = 0; k < 2 ;k++)
    {
      for (unsigned int i = 0; i < cgl[k]->get_CglNTrack(); i++)
        {
          if (cgl[k]->get_tofwrecid(i) >= 0)
            {
              tofwid.insert( cgl[k]->get_tofwrecid(i));
            }
        }
    }

#ifdef useIntflag
  VariableArrayInt *tofwarray;
  tofwarray = findNode::getClass<VariableArrayInt>(topNode, "TofwHit_VarArray");
  vector<int> savethis;
#else
  VariableArray *tofwarray;
  tofwarray = findNode::getClass<VariableArray>(topNode, "TofwHit_VarArray");
  vector<short int> savethis;
#endif

  TofwHit *tofwhit = findNode::getClass<TofwHit>(topNode, "TofwHit");
  if (tofwarray && tofwhit)
    {
      if (tofwhit->isValid())
        {
          for (int ih = 0; ih < tofwhit->get_nhit(); ih++)
            {
              if (tofwid.find(ih) != tofwid.end())
                {
                  savethis.push_back(ih);
                  savethis.push_back(tofwhit->get_stripid(ih));
                  for (int i = 0; i < 3;i++)
                    {
                      float xyz = tofwhit->get_xyz(ih, i);
                      savethis.push_back(FloatToInt(xyz));
                    }
                  float time = tofwhit->get_time(ih) - TimeZero;
                  savethis.push_back(FloatToInt(time));
                  float charge = tofwhit->get_charge(ih);
                  savethis.push_back(FloatToInt(charge));
#ifdef DUMP
                  dumpfile << "TofwHitMapEntry: id: " << ih;
                  dumpfile << ", stripid: " << tofwhit->get_stripid(ih);
                  dumpfile << ", x: " << tofwhit->get_xyz(ih, 0)
			   << ", y: " << tofwhit->get_xyz(ih, 1)
			   << ", z: " << tofwhit->get_xyz(ih, 2);
                  dumpfile << ", ttofw: " << time;
                  dumpfile << ", qtofw: " << charge << endl;
#endif
                  for (int i = 0; i < 2; i++)
                    {
                      float rawadc = tofwhit->get_rawadc(ih, i);
                      savethis.push_back(FloatToInt(rawadc));
                    }
                  for (int i = 0; i < 2; i++)
                    {
                      float rawtdc = tofwhit->get_rawtdc(ih, i);
                      savethis.push_back(FloatToInt(rawtdc));
                    }

                }
            }
        }
      tofwarray->set_val(savethis);
    }
  return EVENT_OK;
}

int
FillTofwHits::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumpfile.close();
#endif

  return 0;
}

#ifdef useIntflag
int
  FillTofwHits::FloatToInt(const float rval) const
  {
    floatint fi;
    fi.f32 = rval;
    return fi.i32;
  }
#else
short int
FillTofwHits::FloatToInt(const float rval) const
{
  half ftoi(rval);
  return ftoi.bits();
}
#endif
