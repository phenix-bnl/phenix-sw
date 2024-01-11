#include <FillTofeHits.h>
#include "setIntflag.h"

#include <TofOut.h>
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

FillTofeHits::FillTofeHits(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/filltofehits.dump");
#endif

  return;
}

int
FillTofeHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

  ostringstream tmpstream;
#ifdef useIntflag
  VariableArrayInt *tofehit = new VariableArrayInt(6000);
#else
  VariableArray *tofehit = new VariableArray(6000);
#endif
  
  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(tofehit, "TofeHit_VarArray", "PHObject");
  dstNode->addNode(PHObjectIONode);
  
  return EVENT_OK;
}

int
FillTofeHits::process_event(PHCompositeNode *topNode)
{
  CglTrack *cgl[2];
  cgl[0] = findNode::getClass<CglTrack>(topNode, "CglTrack");
  cgl[1] = findNode::getClass<CglTrack>(topNode, "CglTrackBack");
  T0Out *t0 = findNode::getClass<T0Out>(topNode, "T0Out");
  float TimeZero = t0->get_T0();
  set<unsigned int> tofeid;
  for (int k = 0; k < 2 ;k++)
    {
      for (unsigned int i = 0; i < cgl[k]->get_CglNTrack(); i++)
        {
          if (cgl[k]->get_tofrecid(i) >= 0)
            {
              tofeid.insert( cgl[k]->get_tofrecid(i));
            }
        }
    }




#ifdef useIntflag
  VariableArrayInt *tofearray;
  tofearray = findNode::getClass<VariableArrayInt>(topNode, "TofeHit_VarArray");
  vector<int> savethis;
#else
  VariableArray *tofearray;
  tofearray = findNode::getClass<VariableArray>(topNode, "TofeHit_VarArray");
  vector<short int> savethis;
#endif

  TofOut *toferaw = findNode::getClass<TofOut>(topNode, "TofOut");
  if (tofearray && toferaw)
    {
      if (toferaw->isValid())
        {
          for (unsigned int ih = 0; ih < toferaw->get_TofNHit(); ih++)
            {
              if (tofeid.find(ih) != tofeid.end())
                {
                  float eloss = toferaw->get_eloss(ih);
                  float ttof = toferaw->get_tof(ih) - TimeZero;

                  savethis.push_back(ih);
                  savethis.push_back( toferaw->get_slatid(ih));
                  for (int i = 0; i < 3;i++)
                    {
                      float xtof = toferaw->get_xtof(ih, i);
                      savethis.push_back(FloatToInt(xtof));
                    }
                  savethis.push_back(FloatToInt(eloss));
                  savethis.push_back(FloatToInt(ttof));
#ifdef DUMP
                  dumpfile << "TofeHitMapEntry: id: " << ih;
                  dumpfile << ", x: " << toferaw->get_xtof(ih, 0)
			   << ", y: " << toferaw->get_xtof(ih, 1)
			   << ", z: " << toferaw->get_xtof(ih, 2)
			   << ", etof: " << eloss << ", ttof: " << ttof << endl;
#endif
                }
            }
        }
      tofearray->set_val(savethis);
    }
  return EVENT_OK;
}

int
FillTofeHits::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumpfile.close();
#endif

  return 0;
}

#ifdef useIntflag
int
FillTofeHits::FloatToInt(const float rval) const
{
  floatint fi;
  fi.f32 = rval;
  return fi.i32;
}
#else
short int
FillTofeHits::FloatToInt(const float rval) const
{
  half ftoi(rval);
  return ftoi.bits();
}
#endif
