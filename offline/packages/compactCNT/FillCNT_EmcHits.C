#include <FillCNT_EmcHits.h>
#include "setIntflag.h"

#include <emcClusterContainer.h>
#include <emcClusterContent.h>
#include <EmcHitMap.h>
#include <EmcHitMapEntry.h>
#include <Fun4AllReturnCodes.h>

#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <PHGlobal.h>
#include <PHAngle.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <gsl/gsl_const.h>

#include <sstream>

using namespace std;

static const float lightSpeed = GSL_CONST_CGS_SPEED_OF_LIGHT / 1e9; // cm/ns

FillCNT_EmcHits::FillCNT_EmcHits(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumprecover.open("/phenix/scratch/frawley/fillcnt_emchits.dump");
#endif
  
  return;
}

int
FillCNT_EmcHits::InitRun(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

int
FillCNT_EmcHits::process_event(PHCompositeNode *topNode)
{
  emcClusterContainer *emccont  = findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");
  if (!emccont)
    {
      return EVENT_OK;
    }

  EmcHitMap *emcmap = findNode::getClass<EmcHitMap>(topNode, "EmcHit_comp");
  if (!emcmap)
    {
      return EVENT_OK;
    }

  PHCentralTrack *cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");

  PHGlobal *glb = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  float TimeZero = glb->getBbcTimeZero();
  float Zvtx = glb->getBbcZVertex();

#ifdef DUMP
  dumprecover << "Number of tracks " <<  cnt->get_npart() << endl;
#endif

  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);
      sngl->set_emcdphi(-9999);
      sngl->set_emcdz(-9999);
      sngl->set_semcdphi(-9999);
      sngl->set_semcdz(-9999);
      sngl->set_emcsdphi(-9999);
      sngl->set_emcsdz(-9999);
      sngl->set_semcsdphi(-9999);
      sngl->set_semcsdz(-9999);

      sngl->set_emcsdphi_e(-9999);
      sngl->set_emcsdz_e(-9999);
      sngl->set_semcsdphi_e(-9999);
      sngl->set_semcsdz_e(-9999);

      sngl->set_sect(-9999);
      sngl->set_ysect(-9999);
      sngl->set_zsect(-9999);
      sngl->set_ecore(-9999);
      sngl->set_emcdispy(-9999);
      sngl->set_emcdispz(-9999);
      sngl->set_emce(-9999);
      sngl->set_secore(-9999);
      sngl->set_semcdispy(-9999);
      sngl->set_semcdispz(-9999);
      sngl->set_semce(-9999);
      sngl->set_e9(-9999);
      sngl->set_se9(-9999);
      sngl->set_emcchi2(-9999);
      sngl->set_semcchi2(-9999);
      sngl->set_ecent(-9999);
      sngl->set_secent(-9999);
      sngl->set_prob(-9999);
      sngl->set_sprob(-9999);
      sngl->set_twrhit(-9999);
      sngl->set_stwrhit(-9999);
      sngl->set_temc(-9999);
      sngl->set_stemc(-9999);
      sngl->set_deadmap(-9999);
      sngl->set_warnmap(-9999);
      sngl->set_sdeadmap(-9999);
      sngl->set_swarnmap(-9999);
      sngl->set_m2emc(-9999);
      sngl->set_emcrawtdc(-9999);
      sngl->set_emcrawadc(-9999);
      sngl->set_emcrawadclg(-9999);
    }
  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);
      short int emcid = sngl->get_emcid();
      if (emcid >= 0)
        {
          emcClusterContent *emcclus = emccont->getCluster(emcid);
	  if(!emcclus)
	    {
	      cout << PHWHERE << " Failed to get emcClusterContent object for emcid " << emcid << " skip this emcid " << endl;
	      continue;
	    } 
	  const EmcHitMapEntry *emchit = emcmap->GetHit(emcid);
          float dphi = get_dphi(emcclus->x(), emcclus->y(), sngl->get_pemcx(), sngl->get_pemcy());
          float dz = get_dz(emcclus->z(), sngl->get_pemcz());
          sngl->set_emcdphi(dphi);
          sngl->set_emcdz(dz);
          sngl->set_sect(emcclus->sector());
          sngl->set_ysect(emcclus->iypos());
          sngl->set_zsect(emcclus->izpos());
          sngl->set_ecore(emcclus->ecore());
          sngl->set_emcdispy(emcclus->dispy());
          sngl->set_emcdispz(emcclus->dispz());
          sngl->set_emce(emcclus->e());
          sngl->set_e9(emcclus->e9());
          sngl->set_emcchi2(emcclus->chi2());
          sngl->set_ecent(emcclus->ecent());
          sngl->set_prob(emcclus->prob_photon());
          sngl->set_twrhit(emcclus->twrhit());
          float dist = sqrt(emcclus->x() * emcclus->x() + emcclus->y() * emcclus->y() + (emcclus->z() - Zvtx) * (emcclus->z() - Zvtx));
          sngl->set_temc(emcclus->tofcorr() - TimeZero + dist / lightSpeed);
          sngl->set_deadmap(emcclus->deadmap());
          sngl->set_warnmap(emcclus->warnmap());
	  sngl->set_emcrawtdc(emchit->get_emcrawtdc());
	  sngl->set_emcrawadc(emchit->get_emcrawadc());
	  sngl->set_emcrawadclg(emchit->get_emcrawadclg());

#ifdef DUMP
	  dumprecover << "Track i " << i << " emcid " << emcid 
		      << " ecore from emcclus " << emcclus->ecore()
		      << " ecore from sngl " << sngl->get_ecore()
		      << " emcrawtdc " << emchit->get_emcrawtdc()
		      << endl;
#endif

        }
      short int semcid = sngl->get_semcid();
      if (semcid >= 0)
        {
          emcClusterContent *emcclus = emccont->getCluster(semcid);
	  if(!emcclus)
	    {
	      cout << PHWHERE << " Failed to get emcClusterContent object for semcid " << emcid << " skip this semcid " << endl;
	      continue;
	    } 
          float dphi = get_dphi(emcclus->x(), emcclus->y(), sngl->get_spemcx(), sngl->get_spemcy());
          float dz = get_dz(emcclus->z(), sngl->get_spemcz());
          sngl->set_semcdphi(dphi);
          sngl->set_semcdz(dz);
          sngl->set_secore(emcclus->ecore());
          sngl->set_semcdispy(emcclus->dispy());
          sngl->set_semcdispz(emcclus->dispz());
          sngl->set_semce(emcclus->e());
          sngl->set_se9(emcclus->e9());
          sngl->set_semcchi2(emcclus->chi2());
          sngl->set_secent(emcclus->ecent());
          sngl->set_sprob(emcclus->prob_photon());
          sngl->set_stwrhit(emcclus->twrhit());
          float dist = sqrt(emcclus->x() * emcclus->x() + emcclus->y() * emcclus->y() + (emcclus->z() - Zvtx) * (emcclus->z() - Zvtx));
          sngl->set_stemc(emcclus->tofcorr() - TimeZero + dist / lightSpeed);

#ifdef DUMP
	  dumprecover << "Swapped: Track i " << i << " semcid " << semcid 
		      << " secore " << emcclus->ecore()  
		      << endl;
#endif
        }
    }

  return EVENT_OK;
}

float
FillCNT_EmcHits::get_dphi(const float x1, const float y1, const float x2, const float y2) const
{
  PHAngle phiM, phiP, rawdiffphi;
  phiM = atan2(y1, x1);
  phiP = atan2(y2, x2);
  rawdiffphi = phiM - phiP;
  return rawdiffphi.getPhi();
}

float
FillCNT_EmcHits::get_dz(const float z1, const float z2) const
  {
    return (z1 - z2);
  }
