#include <FillCNT_TofwHits.h>
#include "setIntflag.h"

#include <Fun4AllReturnCodes.h>
#include <TofwHitMapEntry.h>
#include <TofwHitMap.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>

#include <PHAngle.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <sstream>

using namespace std;

FillCNT_TofwHits::FillCNT_TofwHits(const std::string &name): SubsysReco(name)
{
  return;
}

int
FillCNT_TofwHits::InitRun(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

int
FillCNT_TofwHits::process_event(PHCompositeNode *topNode)
{
  TofwHitMap *tofwmap;
  tofwmap  = findNode::getClass<TofwHitMap>(topNode, "TofwHit_comp");
  if (!tofwmap)
    {
      return EVENT_OK;
    }
  PHCentralTrack *cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);
      sngl->set_striptofw(-9999);
      sngl->set_ttofw(-9999);
      sngl->set_qtofw(-9999);
      sngl->set_tofwdphi(-9999);
      sngl->set_tofwdz(-9999);
      sngl->set_tofwsdphi(-9999);
      sngl->set_tofwsdz(-9999);
      sngl->set_tofwadcup(-9999);
      sngl->set_tofwadcdw(-9999);
      sngl->set_tofwtdcup(-9999);
      sngl->set_tofwtdcdw(-9999);
      sngl->set_m2tofw(-9999);
    }
  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);
      short int tofwid = sngl->get_tofwid();
      if (tofwid >= 0)
        {
          const TofwHitMapEntry *pchit = tofwmap->GetHit(tofwid);
          float dphi = get_dphi(pchit->get_xyz(0), pchit->get_xyz(1), sngl->get_ptofwx(), sngl->get_ptofwy());
          float dz = get_dz(pchit->get_xyz(2), sngl->get_ptofwz());
          sngl->set_striptofw(pchit->get_stripid());
          sngl->set_tofwdphi(dphi);
          sngl->set_tofwdz(dz);
	  sngl->set_ttofw(pchit->get_ttofw());
	  sngl->set_qtofw(pchit->get_qtofw());
          sngl->set_tofwadcdw(pchit->get_adc(0));
          sngl->set_tofwadcup(pchit->get_adc(1));
          sngl->set_tofwtdcdw(pchit->get_tdc(0));
          sngl->set_tofwtdcup(pchit->get_tdc(1));
        }
      short int stofwid = sngl->get_stofwid();
      if (stofwid >= 0)
        {
          const TofwHitMapEntry *pchit = tofwmap->GetHit(stofwid);
          float dphi = get_dphi(pchit->get_xyz(0), pchit->get_xyz(1), sngl->get_sptofwx(), sngl->get_sptofwy());
          float dz = get_dz(pchit->get_xyz(2), sngl->get_sptofwz());
          //	  sngl->set_sstriptofw(pchit->get_stripid());
          sngl->set_stofwdphi(dphi);
          sngl->set_stofwdz(dz);
	  // 	  sngl->set_setof(pchit->get_etof());
	  // 	  sngl->set_sttof(pchit->get_ttof());
        }
    }

  return EVENT_OK;
}

float
FillCNT_TofwHits::get_dphi(const float x1, const float y1, const float x2, const float y2) const
{
  PHAngle phiM, phiP, rawdiffphi;
  phiM = atan2(y1, x1);
  phiP = atan2(y2, x2);
  rawdiffphi = phiM - phiP;
  return rawdiffphi.getPhi();
}

float
FillCNT_TofwHits::get_dz(const float z1, const float z2) const
  {
    return (z1 - z2);
  }
