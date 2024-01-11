#include <FillCNT_PadHits.h>
#include "setIntflag.h"

#include <Fun4AllReturnCodes.h>
#include <PadHitMapEntry.h>
#include <PadHitMap.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>

#include <PHAngle.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <sstream>

using namespace std;

FillCNT_PadHits::FillCNT_PadHits(const std::string &name): SubsysReco(name)
{
  return;
}

int
FillCNT_PadHits::InitRun(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

int
FillCNT_PadHits::process_event(PHCompositeNode *topNode)
{
  PadHitMap *padmap[3];
  ostringstream tmpstream;
  for (int k = 0; k < 3;k++)
    {
      tmpstream.str(""); // reset tmpstream
      tmpstream << "Pc" << k + 1 << "Hit_comp";
      padmap[k]  = findNode::getClass<PadHitMap>(topNode, tmpstream.str());
      if (!padmap[k])
	{
	  return EVENT_OK;
	}
    }
  PHCentralTrack *cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);
      sngl->set_pc1dphi(-9999.);
      sngl->set_pc2dphi(-9999.);
      sngl->set_pc3dphi(-9999.);
      sngl->set_spc1dphi(-9999.);
      sngl->set_spc2dphi(-9999.);
      sngl->set_spc3dphi(-9999.);
 
      sngl->set_pc1dz(-9999.);
      sngl->set_pc2dz(-9999.);
      sngl->set_pc3dz(-9999.);
      sngl->set_spc1dz(-9999.);
      sngl->set_spc2dz(-9999.);
      sngl->set_spc3dz(-9999.);

      sngl->set_pc2sdz(-9999.);
      sngl->set_pc3sdz(-9999.);
      sngl->set_pc2sdphi(-9999.);
      sngl->set_pc3sdphi(-9999.);

      sngl->set_spc2sdz(-9999.);
      sngl->set_spc3sdz(-9999.);
      sngl->set_spc2sdphi(-9999.);
      sngl->set_spc3sdphi(-9999.);

    }
  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);
      short int pc1id = sngl->get_pc1id();
      short int pc2id = sngl->get_pc2id();
      short int pc3id = sngl->get_pc3id();
      short int spc1id = sngl->get_spc1id();
      short int spc2id = sngl->get_spc2id();
      short int spc3id = sngl->get_spc3id();
       if (pc1id >= 0)
         {
           const PadHitMapEntry *pchit = padmap[0]->GetHit(pc1id);
           float dphi = get_dphi(pchit->get_xyz(0), pchit->get_xyz(1), sngl->get_ppc1x(), sngl->get_ppc1y());
           float dz = get_dz(pchit->get_xyz(2), sngl->get_ppc1z());
           sngl->set_pc1dphi(dphi);
           sngl->set_pc1dz(dz);
         }
      if (pc2id >= 0)
        {
          const PadHitMapEntry *pchit = padmap[1]->GetHit(pc2id);
          float dphi = get_dphi(pchit->get_xyz(0), pchit->get_xyz(1), sngl->get_ppc2x(), sngl->get_ppc2y());
          float dz = get_dz(pchit->get_xyz(2), sngl->get_ppc2z());
          sngl->set_pc2dphi(dphi);
          sngl->set_pc2dz(dz);
        }
      if (pc3id >= 0)
        {
          const PadHitMapEntry *pchit = padmap[2]->GetHit(pc3id);
          float dphi = get_dphi(pchit->get_xyz(0), pchit->get_xyz(1), sngl->get_ppc3x(), sngl->get_ppc3y());
          float dz = get_dz(pchit->get_xyz(2), sngl->get_ppc3z());
          sngl->set_pc3dphi(dphi);
          sngl->set_pc3dz(dz);
        }
       if (spc1id >= 0)
         {
           const PadHitMapEntry *pchit = padmap[0]->GetHit(spc1id);
           float dphi = get_dphi(pchit->get_xyz(0), pchit->get_xyz(1), sngl->get_sppc1x(), sngl->get_sppc1y());
           float dz = get_dz(pchit->get_xyz(2), sngl->get_sppc1z());
           sngl->set_spc1dphi(dphi);
           sngl->set_spc1dz(dz);
         }
      if (spc2id >= 0)
        {
          const PadHitMapEntry *pchit = padmap[1]->GetHit(spc2id);
	  if (!pchit)
	    {
	      cout << "found invalid pc2 hit with id: " << spc2id << endl;
	      padmap[1]->identify();
	    }
	  else
	    {
          float dphi = get_dphi(pchit->get_xyz(0), pchit->get_xyz(1), sngl->get_sppc2x(), sngl->get_sppc2y());
          float dz = get_dz(pchit->get_xyz(2), sngl->get_sppc2z());
          sngl->set_spc2dphi(dphi);
          sngl->set_spc2dz(dz);
	    }
        }
      if (spc3id >= 0)
        {
          const PadHitMapEntry *pchit = padmap[2]->GetHit(spc3id);
          float dphi = get_dphi(pchit->get_xyz(0), pchit->get_xyz(1), sngl->get_sppc3x(), sngl->get_sppc3y());
          float dz = get_dz(pchit->get_xyz(2), sngl->get_sppc3z());
          sngl->set_spc3dphi(dphi);
          sngl->set_spc3dz(dz);
        }
    }

  return EVENT_OK;
}

float
FillCNT_PadHits::get_dphi(const float x1, const float y1, const float x2, const float y2) const
{
  PHAngle phiM, phiP, rawdiffphi;
  phiM = atan2(y1, x1);
  phiP = atan2(y2, x2);
  rawdiffphi = phiM - phiP;
  return rawdiffphi.getPhi();
}

float
FillCNT_PadHits::get_dz(const float z1, const float z2) const
  {
    return (z1 - z2);
  }
