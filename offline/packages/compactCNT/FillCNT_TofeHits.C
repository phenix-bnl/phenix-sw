#include <FillCNT_TofeHits.h>
#include "setIntflag.h"

#include <Fun4AllReturnCodes.h>
#include <TofeHitMapEntry.h>
#include <TofeHitMap.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>

#include <PHAngle.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <sstream>

using namespace std;

FillCNT_TofeHits::FillCNT_TofeHits(const std::string &name): SubsysReco(name)
{
  return;
}

int
FillCNT_TofeHits::InitRun(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

int
FillCNT_TofeHits::process_event(PHCompositeNode *topNode)
{
  TofeHitMap *tofemap;
  tofemap  = findNode::getClass<TofeHitMap>(topNode, "TofeHit_comp");
  if (!tofemap)
    {
      return EVENT_OK;
    }
  PHCentralTrack *cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);
      sngl->set_slat(-9999);
      sngl->set_ttof(-9999.);
      sngl->set_etof(-9999.);
      sngl->set_sttof(-9999.);
      sngl->set_setof(-9999.);
      sngl->set_tofdphi(-9999.);
      sngl->set_tofdz(-9999.);
      sngl->set_tofsdphi(-9999.);
      sngl->set_tofsdz(-9999.);
      sngl->set_stofdphi(-9999.);
      sngl->set_stofdz(-9999.);
      sngl->set_stofsdphi(-9999.);
      sngl->set_stofsdz(-9999.);
      sngl->set_m2tof(-9999.);
      sngl->set_tofph1(-9999.);
      sngl->set_tofph2(-9999.);
      sngl->set_toftdc1(-9999.);
      sngl->set_toftdc2(-9999.);
    }
  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);
      short int tofeid = sngl->get_tofeid();
      if (tofeid >= 0)
        {
          const TofeHitMapEntry *pchit = tofemap->GetHit(tofeid);
          float dphi = get_dphi(pchit->get_xyz(0), pchit->get_xyz(1), sngl->get_ptofex(), sngl->get_ptofey());
          float dz = get_dz(pchit->get_xyz(2), sngl->get_ptofez());
	  sngl->set_slat(pchit->get_slatid());
          sngl->set_tofdphi(dphi);
          sngl->set_tofdz(dz);
	  sngl->set_etof(pchit->get_etof());
	  sngl->set_ttof(pchit->get_ttof());
        }
      short int stofeid = sngl->get_stofeid();
      if (stofeid >= 0)
        {
          const TofeHitMapEntry *pchit = tofemap->GetHit(stofeid);
          float dphi = get_dphi(pchit->get_xyz(0), pchit->get_xyz(1), sngl->get_sptofex(), sngl->get_sptofey());
          float dz = get_dz(pchit->get_xyz(2), sngl->get_sptofez());
          sngl->set_stofdphi(dphi);
          sngl->set_stofdz(dz);
	  sngl->set_setof(pchit->get_etof());
	  sngl->set_sttof(pchit->get_ttof());
        }
    }

  return EVENT_OK;
}

float
FillCNT_TofeHits::get_dphi(const float x1, const float y1, const float x2, const float y2) const
{
  PHAngle phiM, phiP, rawdiffphi;
  phiM = atan2(y1, x1);
  phiP = atan2(y2, x2);
  rawdiffphi = phiM - phiP;
  return rawdiffphi.getPhi();
}

float
FillCNT_TofeHits::get_dz(const float z1, const float z2) const
{
  return (z1 - z2);
  }
