#include <FillCNT_DchHits.h>
#include "setIntflag.h"

#include <Fun4AllReturnCodes.h>
#include <DchHitMapEntry.h>
#include <DchHitMap.h>
#include <RunHeader.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>

#include <PHAngle.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <sstream>

using namespace std;

FillCNT_DchHits::FillCNT_DchHits(const std::string &name): SubsysReco(name)
{
  return;
}

int
FillCNT_DchHits::InitRun(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

int
FillCNT_DchHits::process_event(PHCompositeNode *topNode)
{
  DchHitMap *dchmap;
  dchmap  = findNode::getClass<DchHitMap>(topNode, "DchHit_comp");
  if (!dchmap)
    {
      return EVENT_OK;
    }
  PHCentralTrack *cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  RunHeader *run = findNode::getClass<RunHeader>(topNode, "RunHeader");
  int central_mag_current = run->get_currentCentral();
  int multiply_alpha;
  if (central_mag_current > 0)
    {
      multiply_alpha = -1;
    }
  else if (central_mag_current < 0)
    {
      multiply_alpha = 1;
    }
  else
    {
      multiply_alpha = -9999;
    }
  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      //      PHSnglCentralTrack *sngl = cnt->get_track(i);
    }
  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);
      short int dchid = sngl->get_dchid();
      if (dchid >= 0)
        {
          const DchHitMapEntry *pchit = dchmap->GetHit(dchid);

          sngl->set_dcarm(pchit->get_arm());
          sngl->set_dcside(pchit->get_side());
          sngl->set_quality(pchit->get_quality());
          sngl->set_nx1hits(pchit->get_nx1hits());
          sngl->set_nx2hits(pchit->get_nx2hits());
          sngl->set_zed(pchit->get_zed());
          sngl->set_phi(pchit->get_phi());
          float alpha = pchit->get_alpha();
          sngl->set_alpha(alpha);
          if (alpha > 0)
            {
              sngl->set_charge(multiply_alpha);
            }
          else if (alpha < 0)
            {
              sngl->set_charge(-multiply_alpha);
            }
          else
            {
              sngl->set_charge(0);
            }
          sngl->set_beta(pchit->get_beta());
          sngl->set_phi0(pchit->get_phi0());
          sngl->set_the0(pchit->get_theta0());
          sngl->set_mom(pchit->get_momentum());
        }
      //       short int sdchid = sngl->get_sdchid();
      //       if (sdchid >= 0)
      //         {
      //           const DchHitMapEntry *pchit = dchmap->GetHit(sdchid);
      //           float dphi = get_dphi(pchit->get_xyz(0), pchit->get_xyz(1), sngl->get_spdchx(), sngl->get_spdchy());
      //           float dz = get_dz(pchit->get_xyz(2), sngl->get_spdchz());
      //           sngl->set_stofdphi(dphi);
      //           sngl->set_stofdz(dz);
      // 	  sngl->set_setof(pchit->get_etof());
      // 	  sngl->set_sttof(pchit->get_ttof());
      //         }
    }

  return EVENT_OK;
}

