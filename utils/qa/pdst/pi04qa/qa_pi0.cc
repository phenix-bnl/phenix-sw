#include "qa_pi0.h"
#include <iostream>
#include <fstream>

#include <algorithm>
#include "TrigLvl1.h"
#include "RunHeader.h"
#include "utiCentrality.h"
#include "TriggerHelper.h"
#include "PHGlobal.h"
#include "recoConsts.h"

#include "Fun4AllServer.h"
#include "Fun4AllReturnCodes.h"

#include "mEmcGeometryModule.h"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "PHCentralTrack.h"

#include "KCluster.h"
#include "Combination.h"
#include "KEvent.h"
#include "THmulf.h"
#include "getClass.h"

#include "Fun4AllHistoManager.h"
#include "Fun4AllServer.h"

using namespace std;
using namespace findNode;
static const char *HistoManagerName = "QA";

int QA_pi0::process_event(PHCompositeNode *topNode)
{

  PHGlobal *phglobal = 0;
#ifndef QAROOT

  if (nevents % 100 == 0)
    cout << nevents << endl; //  fixed printout of zeros, cmv 11-22-05
#endif

  int iret = EVENT_OK;

  if (!phglobal)
    {
      phglobal = getClass<PHGlobal> (topNode, "PHGlobal");
    }

  float bbct0 = phglobal->getBbcTimeZero();
  if (bbct0 < -900)
    {
      return iret;
    }

  float bbcqs, bbcqn, bbcq;
  int bbcn, bbcs;
  bbcqs = phglobal->getBbcChargeS();
  bbcqn = phglobal->getBbcChargeN();
  bbcq = 0.5 * (bbcqs + bbcqn);
  float zdces, zdcen, zdce;
  zdcen = phglobal->getZdcEnergyN();
  zdces = phglobal->getZdcEnergyS();
  zdce = 0.5 * (zdcen + zdces);

  event_counter->Fill(1.0);

  float bbcz = phglobal->getBbcZVertex();
  if (fabs(bbcz) > 30.)
    {
      return iret;
    }

  bbcn = phglobal->getBbcMultN();
  bbcs = phglobal->getBbcMultS();

  emcClusterContainer *emccont = NULL;
  if (!emccont)
    {
      emccont = getClass<emcClusterContainer>(topNode, "emcClusterContainer");
    }

  PHCentralTrack *phtrk = NULL;
  if (!phtrk)
    {
      phtrk = getClass<PHCentralTrack>(topNode, "PHCentralTrack");
    }

  int ivtx = int( ( bbcz + 30. ) / 5 );

  if ( ivtx < 0 || ivtx >= NUM_VTX_CLASSES )
    {
      return iret;
    }

  //
  // Here, I will introduce a new centrality selection.
  // Very rough selection based on number of DC tracks.
  // Based upon the Au+Au 200GeV data.
  // We don't care if the centrality is flat or not.
  // What we want to do is to select high or low multiplicity events.
  //
  //  by T. Sakaguchi. Jan. 26, 2006
  //
  int nDC = phglobal->getNumberDchTracks();

  int cent;
  if (nDC >= 309)
    cent = 0;
  else if (nDC >= 219)
    cent = 1;
  else if (nDC >= 152)
    cent = 2;
  else if (nDC >= 103)
    cent = 3;
  else if (nDC >= 66)
    cent = 4;
  else if (nDC >= 40)
    cent = 5;
  else if (nDC >= 23)
    cent = 6;
  else if (nDC >= 12)
    cent = 7;
  else
    cent = 8;

  /*
  //  float percent = phglobal->getBbcPercentile();
  int percent = PhUtilities::getCentralityByClockRun4(topNode);
   
  if (percent<=0) percent = 0;
  if (percent>100) percent = -1;
  if (ignore_centrality) percent = 1; // This is just fake centrality for pp.
   
  if (!(percent > 0 && percent <= 100)) 
  {
  return iret;
  }
   
  nclus = emccont->size(); 
  int cent = evCent.getCentBin( percent, NUM_CENT_CLASSES);
  bbczdc->Fill(1.0,(float)cent,bbcq,zdce);
   
  if (!(cent >= 0))
  {
  return iret;
  }
  */

  if (cent > 8)
    cout << "ERR!cent: " << cent << endl;

#if(1)
  //
  // First Fill up electron related stuff
  //   Added by T. Sakaguchi on Dec.14, 2004
  //
  {
    // Dead or Warn Cut words
    unsigned int sccut3x3Map = 0xffe1ce70;
    unsigned int glcut3x3Map = 0x1ce70;

    int ntrk = phtrk->get_npart();

    for (int itrk = 0;itrk < ntrk;itrk++)
      {
        if (phtrk->get_quality(itrk) != 31 && phtrk->get_quality(itrk) != 63)
          continue;
        float trk_emcdphi = phtrk->get_emcdphi(itrk);
        float trk_emcdz = phtrk->get_emcdz(itrk);
        float trk_mom = phtrk->get_mom(itrk);
        int trk_emcid = phtrk->get_emcid(itrk);
        float trk_n1 = phtrk->get_n1(itrk);
        float trk_chi2 = phtrk->get_chi2(itrk);
        float trk_npe1 = phtrk->get_npe1(itrk);

        if (fabs(trk_emcdphi) < 0.01 && fabs(trk_emcdz) < 5
            && trk_n1 > 2 && trk_chi2 / trk_npe1 < 10)
          {

            //
            // These are electrons
            emcClusterContent *emcc = emccont->getCluster(trk_emcid);
            KCluster *cltmp = new KCluster();
            cltmp->set_xyz(emcc->x(), emcc->y(), emcc->z());
            cltmp->set_arm(emcc->arm());
            cltmp->set_sector(emcc->sector());
            cltmp->set_ipos(emcc->iypos(), emcc->izpos());
            cltmp->setArmSecIyIz();
            float emce = emcc->e();

            int sector = cltmp->getSec();
            unsigned int cut3x3Map;
            if (sector < 6)
              cut3x3Map = sccut3x3Map;
            else
              cut3x3Map = glcut3x3Map;

            /*
	      cout << "sector: " << sector << endl;
	      cout << "cent: " << cent << endl;
	      cout << "trk_mom: " << trk_mom << endl;
	      cout << "emce: " << emce << endl;
	      cout << "emcecore: " << emcecore << endl;
	      cout << "emcecent: " << emcecent << endl;
            */

            cltmp->setLocalPos(EmcGeo);

            //
            // add fiducial and deadmap cuts here
            if (cltmp->passFiducialCuts() && (emcc->warnmap() & cut3x3Map) == 0
                && (emcc->deadmap() & cut3x3Map) == 0 )
              {
                elecheck->Fill(1.0, (float)cent, (float)sector, trk_mom, emce / trk_mom);
              }
            delete cltmp;
          }
      }
  }
#endif



  int index = buf_add[cent][ivtx];

  //cout << "index: " << index << ", cent: " << cent << ", ivtx: " << ivtx << endl;

  evarray[index][cent][ivtx].centralityBin = cent;
  evarray[index][cent][ivtx].getEvent( emccont, bbct0, bbcz,
                                       EmcGeo, tofc, toftower, tower,
                                       hcluster, ecompactness, padisp_ratio);

  int nc = (int)(evarray[index][cent][ivtx].g_multiplicity);
  evts_cent->Fill((float)cent);
  evts_mult->Fill(1.0, (float)nc, (float)cent);
  if (nc == 0)
    return 0;
  cmbReal(nc, index, cent, ivtx);

  buf_add[cent][ivtx]++;

  if (buf_add[cent][ivtx] == MAX_EVBUF_DEPTH )
    {
      // make combinations in mixed events
      cmbMixed(cent, ivtx);

      buf_add[cent][ivtx] = 0;

      // release memory for event clusters
      for (int j = 0; j < MAX_EVBUF_DEPTH; j++)
        {
          evarray[j][cent][ivtx].clear();
          evarray[j][cent][ivtx].g_multiplicity = 0;
        }

    }


  nevents++;
  return EVENT_OK;

}

QA_pi0::QA_pi0(const char* outfile) : SubsysReco("PI04QA Analyzer")
{
  OutFileName = outfile;
  ignore_centrality = false;
  EmcGeo = 0;
  return ;
}

QA_pi0::~QA_pi0()
{
  delete EmcGeo;
  return;
}

int QA_pi0::InitRun(PHCompositeNode *topNode)
{
  std::cout << __LINE__ << "  " << __FILE__ << " in InitRun" << std::endl;

  se = Fun4AllServer::instance();

  PHGlobal *phglobal =
    getClass<PHGlobal> (topNode, "PHGlobal");

  emcClusterContainer *emccont =
    getClass<emcClusterContainer>(topNode, "emcClusterContainer");

  PHCentralTrack *phtrk =
    getClass<PHCentralTrack>(topNode, "PHCentralTrack");

  if (!phglobal || !emccont || !phtrk)
    {
      se->unregisterSubsystem(this);
      return 0;
    }

  nevents = 0;

  for (int i = 0; i < NUM_CENT_CLASSES; i++)
    {
      for (int j = 0; j < NUM_VTX_CLASSES; j++)
        {
          buf_add[i][j] = 0;
        }
    }

  /*
    evCent.setCentBins(0, 0., 10.); 
    evCent.setCentBins(1, 10., 20.); 
    evCent.setCentBins(2, 20., 30.); 
    evCent.setCentBins(3, 30., 40.); 
    evCent.setCentBins(4, 40., 50.); 
    evCent.setCentBins(5, 50., 60.); 
    evCent.setCentBins(6, 60., 70.); 
    evCent.setCentBins(7, 70., 80.); 
    evCent.setCentBins(8, 80., 100.); 
  */

  EmcGeo = new mEmcGeometryModule ();

  const float upper = NUM_CENT_CLASSES;
  int num_cent = NUM_CENT_CLASSES;

  // check for HistoManager - if it does not exist, create it
  // HistoManagerName is defined in QADefs.h
  Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
  if (!hm)
    {
      hm = new Fun4AllHistoManager(HistoManagerName);
      se->registerHistoManager(hm);
    }

  event_counter = new TH1F("event_counter", "Event counter histogram", 1, 0.5, 1.5);
  hm->registerHisto(event_counter);
  evts_cent = new TH1F("evts_cent", "Events in centrality classes", num_cent, -0.5, (upper - 0.5));
  hm->registerHisto(evts_cent);
#ifndef QAROOT

  se->registerHisto(event_counter->GetName(), event_counter );
  se->registerHisto(evts_cent->GetName(), evts_cent );
#endif

  hcluster = new THmulf("cluster", "cluster size vs energy");
  hcluster->AddAxis("twrhit", "cluster size (twrhit)", 25, -0.5, 24.5);
  hcluster->AddAxis("e", "cluster energy", 50, 0., 10.);
  hcluster->AddAxis("sec", "sector", 8, -0.5, 7.5);
  hm->registerHisto(hcluster);
#ifndef QAROOT

  se->registerHisto(hcluster->GetName(), hcluster );
#endif

  /*
    hstoch = new THmulf("stoch_vals","variables used for stochcut");
    hstoch->AddAxis("ecent_ecore","ecent/ecore", 50, 0., 1.);
    hstoch->AddAxis("partesum2_ecore","partesum2/ecore", 50, 0., 1.);
    hstoch->AddAxis("partesum3_ecore","partesum3/ecore", 50, 0., 1.);
    hstoch->AddAxis("ratio","padispy/padispz", 200, -10, 10);
    hstoch->AddAxis("sec","sector",8,-0.5,7.5);
    se->registerHisto(hstoch->GetName(), hstoch );
  */

  elecheck = new THmulf("elecheck", "E P dist for electrons");
  elecheck->AddAxis("cent", "cent", num_cent, -0.5, num_cent - 0.5);
  elecheck->AddAxis("sec", "sector", 8, -0.5, 7.5);
  elecheck->AddAxis("mom", "momentum", 50, 0., 5.);
  elecheck->AddAxis("e_mom", "e/momentum", 200, 0., 2.);
  hm->registerHisto(elecheck);

#ifndef QAROOT

  se->registerHisto(elecheck->GetName(), elecheck );
#endif


  ecompactness = new THmulf("ecomp", "ecompactness for stoch eval.");
  ecompactness->AddAxis("pe_ecore", "partesum/ecore", 50, 0., 1.);
  ecompactness->AddAxis("ntower", "number of tower", 3, 0.5, 3.5);
  ecompactness->AddAxis("sec", "sector", 8, -0.5, 7.5);
  hm->registerHisto(ecompactness);
#ifndef QAROOT

  se->registerHisto(ecompactness->GetName(), ecompactness );
#endif

  padisp_ratio = new THmulf("padisp_ratio", "padisp ratio for stoch eval.");
  padisp_ratio->AddAxis("ratio", "padispy/padispz", 200, -10, 10);
  padisp_ratio->AddAxis("sec", "sector", 8, -0.5, 7.5);
  hm->registerHisto(padisp_ratio);
#ifndef QAROOT

  se->registerHisto(padisp_ratio->GetName(), padisp_ratio );
#endif

  evts_mult = new THmulf("evts_mult", "Cluster Multiplicity");
  evts_mult->AddAxis("mult", "EMC cluster mult", 500, -0.5, 499.5);
  evts_mult->AddAxis("cent", "centrality", num_cent, -0.5, num_cent - 0.5);
  hm->registerHisto(evts_mult);
#ifndef QAROOT

  se->registerHisto(evts_mult->GetName(), evts_mult );
#endif

  gghs = new THmulf("gghs", "pairs per sector");
  gghs->AddAxis("mass", "Invariant Mass", 120, 0., 0.6);
  gghs->AddAxis("pt", "Momentum", 12, 0., 6.);
  gghs->AddAxis("mix", "Real or mixed", 2, -0.5, 1.5);
  gghs->AddAxis("cent", "Centrality Class", num_cent, -0.5, (upper - 0.5));
  gghs->AddAxis("sec", "sector", 8, -0.5, 7.5);
  gghs->AddAxis("asym", "Asymmetry cut", 2, -0.5, 1.5);
  gghs->AddAxis("chi2", "chi2 cut", 2, -0.5, 1.5);
  gghs->AddAxis("tof", "tof cut", 3, -0.5, 2.5);
  hm->registerHisto(gghs);
#ifndef QAROOT

  se->registerHisto(gghs->GetName(), gghs );
#endif

  tower = new THmulf("tower", "tower hits");
  tower->AddAxis("energy", "central tower energy", 40, 0., 20.);
  tower->AddAxis("sec", "sector", 8, -0.5, 7.5);
  tower->AddAxis("iy", "iy", 48, -0.5, 47.5);
  tower->AddAxis("iz", "iz", 96, -0.5, 95.5);
  hm->registerHisto(tower);
#ifndef QAROOT

  se->registerHisto(tower->GetName(), tower );
#endif

  toftower = new THmulf("toftower", "tower TOF");
  toftower->AddAxis("tof", "central toftower energy", 50, -5., 5.);
  toftower->AddAxis("sec", "sector", 8, -0.5, 7.5);
  toftower->AddAxis("iy", "iy", 48, -0.5, 47.5);
  toftower->AddAxis("iz", "iz", 96, -0.5, 95.5);
  hm->registerHisto(toftower);
#ifndef QAROOT

  se->registerHisto(toftower->GetName(), toftower );
#endif

  tofc = new THmulf("tofc", "Corrected tof vs ecent");
  tofc->AddAxis("ecent", "central tower energy", 16, 0., 8.);
  tofc->AddAxis("tof", "corrected tof", 100, -5., 5.);
  tofc->AddAxis("sec", "sector", 8, -0.5, 7.5);
  hm->registerHisto(tofc);
#ifndef QAROOT

  se->registerHisto(tofc->GetName(), tofc );
#endif

  bbczdc = new THmulf("bbczdc", "Bbc Zdc Distr");
  bbczdc->AddAxis("cent", "cent", num_cent, -0.5, num_cent - 0.5);
  bbczdc->AddAxis("bbcq", "bbc charge", 200, 0., 1000.);
  bbczdc->AddAxis("zdce", "zdc energy", 200, 0., 5000.);
  hm->registerHisto(bbczdc);
#ifndef QAROOT

  se->registerHisto(bbczdc->GetName(), bbczdc );
#endif

  cout << "Came to the end of Init" << endl;


  return 0;
}



void QA_pi0::cmbReal(int nc, int index, int cent, int ivtx)
{

  for ( int k = 0; k < nc - 1 ; k++ )
    {
      for ( int j = k + 1; j < nc ; j++ )
        {

          if ( cmb.calcCombination( (evarray[index][cent][ivtx]).cluster_element(k), (evarray[index][cent][ivtx]).cluster_element(j) ) )

            {

              mixed = 0;
              cmb.fillHist(gghs, mixed, cent);
            }
        }
    }
}

void QA_pi0::cmbMixed(int cent, int ivtx)
{

  for ( int e1 = 0; e1 < (MAX_EVBUF_DEPTH - 1); e1++)
    {

      for ( int e2 = (e1 + 1); e2 < MAX_EVBUF_DEPTH; e2++)
        {

          int n1 = (evarray[e1][cent][ivtx]).g_multiplicity;
          int n2 = (evarray[e2][cent][ivtx]).g_multiplicity;

          for ( int subi = 0; subi < n1 ; subi++ )
            {

              for ( int subj = 0; subj < n2 ; subj++ )
                {

                  if ( cmb.calcCombination( (evarray[e1][cent][ivtx]).cluster_element(subi), (evarray[e2][cent][ivtx]).cluster_element(subj) ) )

                    {

                      mixed = 1;
                      cmb.fillHist(gghs, mixed, cent);

                    }

                }  // loop over second event clusters

            }  // loop over first event clusters

        }  // loop over events for second clusters in combination

    }  // loop over events for first clusters in combination
}

#ifndef QAROOT
int QA_pi0::End(PHCompositeNode *topNode)
{
  cout << "Dumping histos" << endl;
  Fun4AllServer *se = Fun4AllServer::instance();
  se->dumpHistos(OutFileName.c_str());
  return 0;
}
#endif
