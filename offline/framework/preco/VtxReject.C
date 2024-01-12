#include "VtxReject.h"

#include <BbcOut.h>
#include <Bbc.hh>
#include <PHGlobal.h>

#include <Fun4AllReturnCodes.h>
#include <getClass.h>


#include <TriggerHelper.h>
#include <TriggerUtilities.h>

#include <cmath>
#include <iostream>

using namespace std;

VtxReject::VtxReject(const char *name, const float a, const float b): SubsysReco(name)
{
  min_vtx = a;
  max_vtx = b;
  min_charge = 0;
  max_charge = 2500;
  return;
}

int VtxReject::process_event(PHCompositeNode *topNode)
{
  // reset trigger helper nodes
  _trigger_helper.setNodes( topNode );
  // first look if trigger should be ignored
  int iret = ABORTEVENT;
  set<string>::const_iterator iter;
  for (iter = ignoretrig.begin(); iter != ignoretrig.end(); iter++)
    {
      if (_trigger_helper.didLevel1TriggerGetScaled(*iter))
        {
          //	  cout << "found trigger " << *iter << endl;
          iret = DISCARDEVENT;
          break;
        }
    }
  float zvtx = NAN;
  float charge_sum = NAN;
  PHGlobal *glob = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  if (glob)
    {
      zvtx = glob->getBbcZVertex();
      charge_sum = glob->getBbcChargeN() + glob-> getBbcChargeS();
    }
  else
    {
      BbcOut* bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
      if (bbcout)
        {
          zvtx = bbcout->get_VertexPoint();
          charge_sum = bbcout->get_ChargeSum(Bbc::North) + bbcout->get_ChargeSum(Bbc::South);
        }
      else
        {
          cout << PHWHERE << "PHGlobal and BbcOut Object missing, aborting event" << endl;
          return ABORTEVENT;
        }
    }

  if (zvtx < min_vtx || zvtx > max_vtx)
    {
      if (verbosity > 0)
        {
          cout << "VtxReject: aborting for vtx " << zvtx
               << " limits: " << min_vtx << " to " << max_vtx << endl;
        }
      return iret;
    }
  if (charge_sum < min_charge || charge_sum > max_charge)
    {
      if(verbosity) cout << "VtxReject: aborting for charge sum " << charge_sum
           << " limits: " << min_charge << " to " << max_charge << endl;
      return iret;
    }

  return EVENT_OK;
}

