#include <stdio.h>
#include <iostream>
#include "TObject.h"
#include "KCluster.h"
#include "KEvent.h"

KEvent::KEvent()
{
  g_multiplicity = 0;
  c_multiplicity = 0;
  centralityBin = -1;
}

KEvent::~KEvent()
{
  clear();
}

KCluster * KEvent::cluster_element(const unsigned int n)
{
  if ( n >= clusterVector.size() )
    return 0;
  return clusterVector[n];

}

// Clear contents of the event.
void KEvent::clear()
{

  for_each(clusterVector.begin(), clusterVector.end(), DeleteObject() );
  clusterVector.resize(0);
  c_multiplicity = 0;

}

void KEvent::add_multiplicity()
{
  c_multiplicity++;
}

Int_t KEvent::getEvent( emcClusterContainer *emccont,
                        float bbcT0,
                        float bbcVz,
                        mEmcGeometryModule *EmcGeo,
                        THmulf *tofc, THmulf *toftower,
                        THmulf *tower, THmulf *hcluster,
                        THmulf *ecompactness, THmulf *padisp_ratio)
{

  int k, j;

  KCluster *c;
  int nclusters = emccont->size();

  emcClusterContent *emc;
  const static unsigned int sccut3x3Map = 0xffe1ce70;
  const static unsigned int glcut3x3Map = 0x1ce70;

  unsigned int cut3x3Map;

  for ( j = 0; j < nclusters; j++)
    {

      emc = emccont->getCluster(j);

      if (emc->e() > 0.2) // && emc->deadmap()==0 && emc->warnmap()==0)
        {

          float tofEshift, corrtof, ecent;
          ecent = emc->ecent();
          if (ecent < 0.4)
            {
              tofEshift = -0.0039 - 0.076 / ecent + 0.019 / (ecent * ecent);
            }
          else if (ecent < 1.0)
            {
              tofEshift = 0.661 - 0.64 / ecent + 0.14 / (ecent * ecent);
            }
          else
            {
              tofEshift = 0.17 * (log(ecent) + 0.91) * (log(ecent) + 0.91);
            }
          corrtof = emc->tofcorr() - bbcT0; // - tofEshift;

          c = new KCluster();

          c->set_xyz(emc->x(), emc->y(), emc->z());
          c->set_e(emc->e());
          c->set_arm(emc->arm());
          c->set_sector(emc->sector());
          c->set_ipos(emc->iypos(), emc->izpos());
          c->set_ecore(emc->ecore());
          c->set_ecent(emc->ecent());
          c->set_tofcorr(corrtof);
          c->set_prob_photon(emc->prob_photon());
          c->set_chi2(emc->chi2());
          c->set_multiplicity(emc->multiplicity());
          for (k = 0;k < emc->multiplicity();k++)
            c->set_partesum(k, emc->partesum(k));
          c->set_disp(emc->dispy(), emc->dispz());
          c->set_padisp(emc->padispy(), emc->padispz());
          c->set_yz_cg(emc->ycg(), emc->zcg());
          c->setArmSecIyIz();
          c->setLocalPos(EmcGeo);
          c->setVtxZ(bbcVz);
          c->setTrackVector();
          c->set_maps(emc->deadmap(), emc->warnmap());

          int sector = c->getSec();
          if (sector < 6)
            {
              cut3x3Map = sccut3x3Map;
            }
          else
            {
              cut3x3Map = glcut3x3Map;
            }

          // add PID cuts and fiducial cuts here
          if (c->passFiducialCuts() && (c->warnmap() & cut3x3Map) == 0
              && (c->deadmap() & cut3x3Map) == 0 )
            {
              // for pi0 reconstruction

              c->passTOFCut();
              c->passCHI2Cut();

              tower->Fill(1.0, ecent, (float)sector, c->getIy(), c->getIz());
              hcluster->Fill(1.0, c->multiplicity(), c->e(), (float)sector);
              float partesum_1 = c->multiplicity() > 1 ? c->partesum(1) : 0 ;
              float partesum_2 = c->multiplicity() > 2 ? c->partesum(2) : 0 ;
              ecompactness->Fill(1.0, c->ecent() / c->ecore(), 1.0, (float)sector);
              ecompactness->Fill(1.0, partesum_1 / c->ecore(), 2.0, (float)sector);
              ecompactness->Fill(1.0, partesum_2 / c->ecore(), 3.0, (float)sector);
              padisp_ratio->Fill(1.0, c->padispy() / c->padispz(), (float)sector);

              clusterVector.push_back(c);
              g_multiplicity++;
              add_multiplicity();

              if (c->ecore() > 0.3 && c->getChi2Cut() >= 1)
                {
                  tofc->Fill(1.0, ecent, corrtof, (float)sector);
                  toftower->Fill(1.0, corrtof, (float)sector, c->getIy(), c->getIz());
                }

            }
          else
            {
              delete c;
            }
        }
    }

  if ( (c_multiplicity) == 0 )
    {
      return 0;
    }
  else
    {
      return c_multiplicity;
    }
}

float KEvent::getEtByMease()
{
  return 0;
}

