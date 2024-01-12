#include "Htypes.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "QADefs.h"

#include "Fun4AllHistoManager.h"
#include "Fun4AllServer.h"

#include "BbcOut.h"
#include "histElectron.h"
#include "TriggerHelper.h"
#include "TrigLvl1.h"
#include "ErtOut.h"
#include "PHCentralTrack.h"
#include "PHGlobal.h"
#include "getClass.h"
#include "vector"

#define eID_nPbins 38
#define minPhi -0.7
#define maxPhi 3.8
#define minAlpha -1
#define maxAlpha 1 
#define eID_minP 0.2
#define eID_maxP 4.8

using namespace std;

vector<int> shared_accept;

TH2 *elcut = NULL;
TH2 *eldep = NULL;
TH2 *elsdep = NULL;
TH2 *eln0 = NULL;
TH2 *elsn0 = NULL;
TH2 *elchi2npe0 = NULL;
TH2 *elschi2snpe0 = NULL;
TH3 *elemcmatch = NULL;
TH3 *elemcmatch_e = NULL;
TH3 *elemcmatch_se = NULL;
TH2 *elacceptance = NULL;
TH2 *elacceptance_X1orX2 = NULL;
TH2 *elacceptance_e = NULL;
TH2 *elacceptance_se = NULL;
TH2 *elep[8];
TH2 *elsep[8];
TH1 *elmom2 = NULL;
TH1 *elep2 = NULL;
TH2 *eldisp = NULL;
TH2 *elsdisp = NULL;

TH2 *elricht0 = NULL;
TH2 *elemct0 = NULL;
TH2 *elsricht0 = NULL;
TH2 *elsemct0 = NULL;

int QAElectron::InitRun(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  PHCentralTrack * phcentraltrack = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");

  if (!phcentraltrack)
    {
      se->unregisterSubsystem(this);
      return 0;
    }

  //  PHGlobal* global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");

  BbcOut *bbc = findNode::getClass<BbcOut>(topNode, "BbcOut");
  if (!bbc)
    {
      se->unregisterSubsystem(this);
      return 0;
    }

  zvertex_cut = 25;
  emcmatch_cut = 3;
  min_mom_cut = 0.2;
  max_mom_cut = 4.8;
  n0_cut = 2;
  chi2npe0_cut = 20;
  disp_cut = 5;
  min_ep_cut = -999.;
  max_ep_cut = 999.;
  min_dep_cut = -2.5; // in sigmas
  max_dep_cut = 5.0;
  quality_cut = 1;
  pfoa_cut = 2.5; //in degrees
  deltaZ_cut = 1.0;
  deltaPhi_cut = 0.1;
 
  // check for HistoManager - if it does not exist, create it
  // HistoManagerName is defined in QADefs.h
  Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
  if (!hm)
    {
      hm = new Fun4AllHistoManager(HistoManagerName);
      se->registerHistoManager(hm);
    }

  elcut = new TH2F("elcut", "Electron Cut ", 15, -0.5, 14.5, 50, 0, 5);
  hm->registerHisto(elcut);
  eldep = new TH2F("eldep", "#frac{E-P}{P} in #sigmas x P", 100, -5, 5, eID_nPbins, min_mom_cut, max_mom_cut);
  hm->registerHisto(eldep);
  elsdep = new TH2F("elsdep", "#frac{E-P}{P} in #sigmas x P (swapped)", 100, -5, 5, eID_nPbins, min_mom_cut, max_mom_cut);
  hm->registerHisto(elsdep);
  eln0 = new TH2F("eln0", "RICH n0 (Dep>-2)", 10, 0, 10, eID_nPbins, min_mom_cut, max_mom_cut);
  hm->registerHisto(eln0);
  elsn0 = new TH2F("elsn0", "RICH n0 (Dep>-2)", 10, 0, 10, eID_nPbins, min_mom_cut, max_mom_cut);
  hm->registerHisto(elsn0);
  elchi2npe0 = new TH2F("elchi2npe0", "chi2/npe0 (n0>2)", 100, 0, 30, eID_nPbins, min_mom_cut, max_mom_cut);
  hm->registerHisto(elchi2npe0);
  elschi2snpe0 = new TH2F("elschi2snpe0", "schi2/snpe0 (sn0>2)", 100, 0, 30, eID_nPbins, min_mom_cut, max_mom_cut);
  hm->registerHisto(elschi2snpe0);
  elemcmatch = new TH3F("elemcmatch", "EMC matching", 100, -10, 10, 100, -10, 10, eID_nPbins, min_mom_cut, max_mom_cut);
  hm->registerHisto(elemcmatch);
  elemcmatch_e = new TH3F("elemcmatch_e", "EMC matching (electron candidate)"
                          , 100, -10, 10, 100, -10, 10, eID_nPbins, min_mom_cut, max_mom_cut);
  hm->registerHisto(elemcmatch_e);
  elemcmatch_se = new TH3F("elemcmatch_se", "EMC matching (swapped electron)"
                           , 100, -10, 10, 100, -10, 10, eID_nPbins, min_mom_cut, max_mom_cut);
  hm->registerHisto(elemcmatch_se);
  elacceptance = new TH2F("elacceptance", "#phi x #alpha (X1 and X2)",
                          100, minPhi, maxPhi, 100, minAlpha, maxAlpha);
  hm->registerHisto(elacceptance);
  elacceptance_X1orX2 = new TH2F("elacceptance_X1orX2", "#phi x #alpha (X1 or X2)"
                                 , 100, minPhi, maxPhi, 100, minAlpha, maxAlpha);
  hm->registerHisto(elacceptance_X1orX2);
  elacceptance_e = new TH2F("elacceptance_e", "#phi x #alpha (eID)"
                            , 100, minPhi, maxPhi, 100, minAlpha, maxAlpha);
  hm->registerHisto(elacceptance_e);
  elacceptance_se = new TH2F("elacceptance_se", "#phi x #alpha (swapped eID)"
                             , 100, minPhi, maxPhi, 100, minAlpha, maxAlpha);
  hm->registerHisto(elacceptance_se);
  char name[10], title[40];
  for (int iarm = 0; iarm < 2; iarm++)
    for (int isector = 0; isector < 4; isector++)
      {
        int iarmsect = iarm * 4 + isector;
        sprintf(name, "elep_%d", iarmsect);
        if (iarm == 0)
          sprintf(title, "#frac{E}{P}-1 x pt E%d", isector);
        if (iarm == 1)
          sprintf(title, "#frac{E}{P}-1 x pt W%d", isector);
        elep[iarmsect] = new TH2F(name, title, 50, -1, 1, eID_nPbins, min_mom_cut, max_mom_cut);
	hm->registerHisto(elep[iarmsect]);
        sprintf(name, "elsep_%d", iarmsect);
        if (iarm == 0)
          sprintf(title, "Swapped #frac{E}{P}-1 x pt E%d", isector);
        if (iarm == 1)
          sprintf(title, "Swapped #frac{E}{P}-1 x pt W%d", isector);
        elsep[iarmsect] = new TH2F(name, title, 50, -1, 1, eID_nPbins, min_mom_cut, max_mom_cut);
	hm->registerHisto(elsep[iarmsect]);
      }
  eldisp = new TH2F("eldisp", "RICH ring displacement", 40, 0, 10, eID_nPbins, min_mom_cut, max_mom_cut);
  hm->registerHisto(eldisp);
  elsdisp = new TH2F("elsdisp", "RICH shared ring displacement", 40, 0, 10, eID_nPbins, min_mom_cut, max_mom_cut);
  hm->registerHisto(elsdisp);
  elep2 = new TH1F("elep2", "#frac{E}{P}  (pfoa cut)", 100, -5, 5);
  hm->registerHisto(elep2);
  elmom2 = new TH1F("elmom2", "Electron momentum (pfoa min>8)", 100, 0, 5);
  hm->registerHisto(elmom2);
  elricht0 = new TH2F("elricht0", "RICH T0 (eID)", 100, -5, 5, eID_nPbins, min_mom_cut, max_mom_cut);
  hm->registerHisto(elricht0);
  elsricht0 = new TH2F("elsricht0", "RICH T0 (swapped eID)", 100, -5, 5, eID_nPbins, min_mom_cut, max_mom_cut);
  hm->registerHisto(elsricht0);
  elemct0 = new TH2F("elemct0", "EmCal T0 (eID)", 100, -40, 40, eID_nPbins, min_mom_cut, max_mom_cut);
  hm->registerHisto(elemct0);
  elsemct0 = new TH2F("elsemct0", "EmCal T0 (swapped eID)", 100, -40, 40, eID_nPbins, min_mom_cut, max_mom_cut);
  hm->registerHisto(elsemct0);

  return 0;
}


int QAElectron::process_event(PHCompositeNode *topNode)
{
  TriggerHelper triggerhelper(topNode);
  ErtOut* ertout = findNode::getClass<ErtOut>(topNode, "ErtOut");

  PHCentralTrack * phcentraltrack = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");

  if (!phcentraltrack)
    {
      return 0;
    }

  //  PHGlobal* global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  //  if (!global)
  
  BbcOut *bbc = findNode::getClass<BbcOut>(topNode, "BbcOut");
  if (!bbc)
    {
      return 0;
    }
  
  bool isElectronCandidate, itrgBBCLL1, itrgERT_Electron;

  /*
    ------------------------------- elcut ----------------------------------------------------------
    bin 1 --> is BBCLL1>=1
    bin 2 --> is ERT_Electron fired event
    bin 3 --> |Z vertex| < 30 cm
    bin 4 --> total tracks
    bin 5 --> DCH X1 or X2 hits and PC1 found
    bin 6 --> DCH X1 and X2 found
    bin 7 --> EmCal |emcsdphi|<3 sigmas and |emcsdz|<3 sigmas
    bin8 -->  minP < P < maxP
    bin 9 --> RICH n0>2
    bin10 --> chi2/npe0<10
    bin11 --> disp<5
    bin12 --> Dep>-2
    bin13 --> minimum post field open angle > 8 degrees
    bin14 --> eID && ERT_Electron (no post field cut)
  */

  itrgERT_Electron = (0x1 << triggerhelper.getLevel1BitNumber("ERT_Electron"));
  itrgERT_Electron |= (0x1 << triggerhelper.getLevel1BitNumber("ERT_Electron&BBCLL1"));

  //  itrgBBCLL1 = triggerhelper.trigLive("BBCLL1>=1");
  //  itrgBBCLL1 |= triggerhelper.trigLive("BBCLL1");

  itrgBBCLL1 = triggerhelper.IsEventMinBias();

  //  int BBCLL1_live = itrgBBCLL1 * (1+triggerhelper.getLevel1Scaledown("BBCLL1>=1"));
  elcut->Fill(0., min_mom_cut, (Stat_t) itrgBBCLL1); // number of calls
  elcut->Fill(1., min_mom_cut, (Stat_t) itrgERT_Electron);  // fill if event is triggered by ERT_Electron
  float zvertex = bbc->get_VertexPoint();
  if (fabs(zvertex) > zvertex_cut)
    return 0;  // |Zvertex|<30 cm
  elcut->Fill(2., min_mom_cut, (Stat_t) itrgBBCLL1);  // |Zvertex|<30 cm

  shared_accept.clear();

  for (unsigned int i = 0; i < phcentraltrack->get_npart(); i++)
    {
      isElectronCandidate = 1;
      float p = phcentraltrack->get_mom(i);
      elcut->Fill(3., p, (Stat_t) (isElectronCandidate && itrgBBCLL1));
      float emcsdphi = phcentraltrack->get_emcsdphi_e(i);
      float emcsdz = phcentraltrack->get_emcsdz_e(i);
      bool emcmatchcut = fabs(emcsdphi) < emcmatch_cut && fabs(emcsdz) < emcmatch_cut;   // EmCal Match < 3 sigmas
      int quality = phcentraltrack->get_quality(i);

      if (quality_cut)
	if ((quality & 0x13) < 17)
	  continue;    // (DCH X1 or X2) and PC1

      elcut->Fill(4., p, (Stat_t) (isElectronCandidate && itrgBBCLL1));
      float phi = phcentraltrack->get_phi(i);
      float alpha = phcentraltrack->get_alpha(i);
      elacceptance_X1orX2->Fill(phi, alpha, (Stat_t)emcmatchcut);

      if (quality_cut)
	if ((quality & 0x13) < 18)
	  continue; // DCH X1 and X2 and PC1

      elacceptance->Fill(phi, alpha, (Stat_t)emcmatchcut);

      elcut->Fill(5., p, (Stat_t) (isElectronCandidate && itrgBBCLL1));
      // calculate minimum post field open angles of two tracks
      float E = phcentraltrack->get_ecore(i);
      int n0 = phcentraltrack->get_n0(i);
      float chi2 = phcentraltrack->get_chi2(i);
      float npe0 = phcentraltrack->get_npe0(i);
      float disp = phcentraltrack->get_disp(i);
      int sn0 = phcentraltrack->get_sn0(i);
      float schi2 = phcentraltrack->get_schi2(i);
      float snpe0 = phcentraltrack->get_snpe0(i);
      float sdisp = phcentraltrack->get_sdisp(i);
      int dcarm = phcentraltrack->get_dcarm(i);
      int sect = phcentraltrack->get_sect(i);
      //      float dep = dep_emc(E,p,dcarm, sect);
      float dep = phcentraltrack->get_dep(i);     // new function provided by Taku Gunshi 11/09/04
      int iarmsect = dcarm * 4 + sect;

      bool epcut = (dep > min_dep_cut && dep < max_dep_cut);

      elemcmatch->Fill(emcsdphi, emcsdz, p);
      elemcmatch_e->Fill(emcsdphi, emcsdz, p, (Stat_t) n0 > n0_cut && chi2 / npe0 < chi2npe0_cut && disp < disp_cut && epcut);
      elemcmatch_se->Fill(emcsdphi, emcsdz, p, (Stat_t) sn0 > n0_cut && schi2 / snpe0 < chi2npe0_cut && sdisp < disp_cut && epcut);
      if (!emcmatchcut)
        continue;  // EMCal match cut
      elcut->Fill(6., p, (Stat_t) (isElectronCandidate && itrgBBCLL1));

      if (p < min_mom_cut || p > max_mom_cut)
        continue;
      elcut->Fill(7., p, (Stat_t) (isElectronCandidate && itrgBBCLL1));
      eln0->Fill((float)n0 + 0.5, p, (Stat_t) (isElectronCandidate && itrgBBCLL1) && epcut);
      elsn0->Fill((float)sn0 + 0.5, p, (Stat_t) isElectronCandidate && itrgBBCLL1 && epcut); //select MinBias events && e/p cut

      bool isSwappedElectronCandidate = isElectronCandidate && itrgBBCLL1 && sn0 > n0_cut;  // Minimum Bias event and sn0>2

      isElectronCandidate &= n0 > n0_cut;
      elcut->Fill(8., p, (Stat_t) isElectronCandidate && itrgBBCLL1);
      elchi2npe0->Fill(chi2 / npe0, p, (Stat_t) (isElectronCandidate && itrgBBCLL1 && epcut));
      if (snpe0 > 0)
        elschi2snpe0->Fill(schi2 / snpe0, p, (Stat_t) isSwappedElectronCandidate && epcut);

      eldisp->Fill(disp, p, (Stat_t) isElectronCandidate && itrgBBCLL1 && epcut);
      elsdisp->Fill(sdisp, p, (Stat_t) isSwappedElectronCandidate && epcut);

      isElectronCandidate &= chi2 / npe0 < chi2npe0_cut;
      isSwappedElectronCandidate &= schi2 / snpe0 < chi2npe0_cut;
      elcut->Fill(9., p, (Stat_t) isElectronCandidate && itrgBBCLL1);

      isElectronCandidate &= disp < disp_cut;
      isSwappedElectronCandidate &= sdisp < disp_cut;
      elcut->Fill(10., p, (Stat_t) isElectronCandidate && itrgBBCLL1);

      eldep->Fill(dep, p, (Stat_t) isElectronCandidate && itrgBBCLL1);
      elsdep->Fill(dep, p, (Stat_t) isSwappedElectronCandidate);

      elep[iarmsect]->Fill(E / p - 1, p, (Stat_t) isElectronCandidate && itrgBBCLL1);
      elsep[iarmsect]->Fill(E / p - 1, p, (Stat_t) isSwappedElectronCandidate);

      int ghost_sharing = check_ghost_sharing(phcentraltrack, i);
      if (ghost_sharing == 0)      // fill with minimum post field open angles of two tracks > 8 degrees
        {
          elep2->Fill(dep, (Stat_t) isElectronCandidate && itrgBBCLL1);
        }

      isElectronCandidate &= epcut;

      elcut->Fill(11., p, (Stat_t) isElectronCandidate && itrgBBCLL1);
      elmom2->Fill(p, (Stat_t) isElectronCandidate && itrgBBCLL1);

      if (ghost_sharing==0) 
	elcut->Fill(12.,p,(Stat_t) isElectronCandidate && itrgBBCLL1); 

      int ert_fired = 0;
      if (ertout && isElectronCandidate && itrgERT_Electron)
        ert_fired = check_ert(phcentraltrack, ertout, i);
      elcut->Fill(13., p, (Stat_t) ert_fired);

      elacceptance_e->Fill(phi, alpha, (Stat_t) isElectronCandidate && itrgBBCLL1);
      elacceptance_se->Fill(phi, alpha, (Stat_t) isSwappedElectronCandidate && epcut);
      float emc_t0 = phcentraltrack->get_temc(i);
      float crk_t0 = phcentraltrack->get_tcrk(i);
      float scrk_t0 = phcentraltrack->get_stcrk(i);
      elricht0->Fill(crk_t0, p, (Stat_t) isElectronCandidate && itrgBBCLL1);
      elemct0->Fill(emc_t0, p, (Stat_t) isElectronCandidate && itrgBBCLL1);
      elsricht0->Fill(scrk_t0, p, (Stat_t) isSwappedElectronCandidate && epcut);
      elsemct0->Fill(emc_t0, p, (Stat_t) isSwappedElectronCandidate && epcut);
    }
  return 0;
}

float QAElectron::dep_emc(float E, float P, int dcarm, int sect)   // Taku Gunshi parametrization
{
	int isector = dcarm*4+sect;
	Float_t p0_dep_mean[8]={-0.0177,-0.02812, -0.03256, -0.0282, -0.07143, -0.07484, -0.05095,-0.04275};
	Float_t p1_dep_mean[8]={-1.584, -1.34, -1.148, -1.115, -1.372, -1.047, -1.148, -1.58};
	Float_t p2_dep_mean[8]={1.643, 1.982, 3.952, 4.182,  3.727, 4.742, 4.496, 3.19};
	Float_t p0_dep_sigma[8]={0.05562,0.05779, 0.06609, 0.0714, 0.06505, 0.07049, 0.06327,  0.06998};
	Float_t p1_dep_sigma[8]={0.07277,0.07212, 0.08418, 0.07752, 0.07756, 0.0785, 0.07987, 0.0817};
	Float_t p2_dep_sigma[8]={0.008906, 0.00864, 0.01075, 0.00871, 0.008168, 0.006238, 0.007226,0.008394};
	
	float Dep = ( (E/P-1.0)- ( p0_dep_mean[isector]-exp(p1_dep_mean[isector]-p2_dep_mean[isector]*P)));
	Dep /= sqrt(pow(p0_dep_sigma[isector],2)+pow(p1_dep_sigma[isector]/sqrt(P),2)+pow(p2_dep_sigma[isector]*P,2));
  return Dep;
}

float QAElectron::pfoa(float th1,float PH1,float th2,float PH2)
{
   // post field open angles of the two tracks for eID cut
  // where  thetaLAB=dch->get_beta(idch);
  //        phiLAB=dch->get_phi(idch) - alpha;
 
  // supposed if(costhetaAB<0.990268)  //  8 degrees cut
  //                        0.984808   // 10 degrees cut
 
  /*
    double Ax = sin(th1) * cos(PH1);
    double Ay = sin(th1) * sin(PH1);
    double Az = cos(th1);
    double Bx = sin(th2) * cos(PH2);
    double By = sin(th2) * sin(PH2);
    double Bz = cos(th2);
 
    //take the inner product of the two unit vectors
 
    //double xtmp = Ax*Bx + Ay*By + Az*Bz;
    */
 
  return        sin(th1) * cos(PH1) * sin(th2) * cos(PH2) +
    sin(th1) * sin(PH1) * sin(th2) * sin(PH2) +
    cos(th1) *            cos(th2);
 
  //return (float) xtmp;
 }


bool QAElectron::check_ert(PHCentralTrack *phcentraltrack, ErtOut *ertout, int itrck)
{
  // it checks if ERT_Electron was fired  -- code hacked from QAErt
  int side = phcentraltrack->get_dcside(itrck);
  int arm = phcentraltrack->get_dcarm(itrck);
  int sector = phcentraltrack->get_sect(itrck);
  int ertarm = 0;
  int ertside = 0;
  int ertsector = 0;

  int ertemc = 0;
  int ertrich = 0;

  int nerthits = ertout->get_ERThit_N();
  for (int ihit = 0;ihit < nerthits;ihit++)
    {
      int itrgert = ertout->get_ERTtrigmode(ihit);
      if (!(itrgert == 3 || itrgert == 4))
        continue;
      ertarm = ertout->get_ERTarm(ihit);
      if (ertarm != arm)
        continue;
      ertsector = ertout->get_ERTsector(ihit);
      if (ertsector != sector)
        continue;
      int sm = ertout->get_ERTsm(ihit);
      if (itrgert == 3)
        {		// emc
          if (ertarm == 0)
            {			// west pbsc
              if (sm == 0 || sm == 1 || sm == 2 ||
                  sm == 6 || sm == 7 || sm == 8 ||
                  sm == 12 || sm == 13 || sm == 14)
                ertside = 0;
              if (sm == 3 || sm == 4 || sm == 5 ||
                  sm == 9 || sm == 10 || sm == 11 ||
                  sm == 15 || sm == 16 || sm == 17)
                ertside = 1;
            }
          else
            {
              if (ertsector >= 2)
                {		// east pbsc
                  if (sm == 0 || sm == 1 || sm == 2 ||
                      sm == 6 || sm == 7 || sm == 8 ||
                      sm == 12 || sm == 13 || sm == 14)
                    ertside = 0;
                  if (sm == 3 || sm == 4 || sm == 5 ||
                      sm == 9 || sm == 10 || sm == 11 ||
                      sm == 15 || sm == 16 || sm == 17)
                    ertside = 1;
                }
              else
                {			// east pbgl
                  if (sm == 0 || sm == 1 || sm == 2 || sm == 3 ||
                      sm == 8 || sm == 9 || sm == 10 || sm == 11 ||
                      sm == 16 || sm == 17 || sm == 18 || sm == 19 ||
                      sm == 24 || sm == 25 || sm == 26 || sm == 27)
                    ertside = 0;
                  if (sm == 4 || sm == 5 || sm == 6 || sm == 7 ||
                      sm == 12 || sm == 13 || sm == 14 || sm == 15 ||
                      sm == 20 || sm == 21 || sm == 22 || sm == 23 ||
                      sm == 28 || sm == 29 || sm == 30 || sm == 31)
                    ertside = 1;
                }
            }
          if (ertside == side)
            ertemc = 1;
        }
      else if (itrgert == 4)
        {		//RICH
          if (sm == 0 || sm == 1 || sm == 2 || sm == 3 ||
              sm == 8 || sm == 9 || sm == 10 || sm == 11 ||
              sm == 16 || sm == 17 || sm == 18 || sm == 19 ||
              sm == 24 || sm == 25 || sm == 26 || sm == 27)
            ertside = 0;
          if (sm == 4 || sm == 5 || sm == 6 || sm == 7 ||
              sm == 12 || sm == 13 || sm == 14 || sm == 15 ||
              sm == 20 || sm == 21 || sm == 22 || sm == 23 ||
              sm == 28 || sm == 29 || sm == 30 || sm == 31)
            ertside = 1;
        }
      if (ertside == side)
        ertrich = 1;
    } // loop over ert hits
  return ertemc && ertrich;
}

bool QAElectron::check_ghost_sharing(PHCentralTrack *phcentraltrack, unsigned int i)
{
  int ishared = -1;
  float th1 = phcentraltrack->get_beta(i);
  float phi = phcentraltrack->get_phi(i);
  float alpha = phcentraltrack->get_alpha(i);
  float pfomin = 9999.;
  for (unsigned int gl2 = 0; gl2 < phcentraltrack->get_npart();gl2++)
    {
      if (phcentraltrack->get_dcarm(gl2) != phcentraltrack->get_dcarm(i))
        continue; //check only electrons in same arm
      if (phcentraltrack->get_n0(gl2) <= n0_cut)
        continue;
      if (phcentraltrack->get_mom(gl2) < min_mom_cut)
        continue;
      if (phcentraltrack->get_mom(gl2) > max_mom_cut)
        continue;
      if (phcentraltrack->get_dep(gl2) < min_dep_cut)
        continue;
      if (quality_cut && (phcentraltrack->get_quality(gl2) & 0x13) < 18)
        continue;
      if (fabs(phcentraltrack->get_emcsdphi_e(gl2)) > emcmatch_cut
          || fabs(phcentraltrack->get_emcsdz_e(gl2)) > emcmatch_cut)
        continue;
      if (phcentraltrack->get_chi2(gl2) / phcentraltrack->get_npe0(gl2) > chi2npe0_cut)
        continue;
      if (gl2 != i)    // we cannot compare the same track
        {
          float th2 = phcentraltrack->get_beta(gl2);
          float phi2 = phcentraltrack->get_phi(gl2) - phcentraltrack->get_alpha(gl2);
          float pfo = pfoa(th1, phi - alpha, th2, phi2);
          pfo = acos(pfo) * 180 / acos( -1.);   // conversion to degrees
          if (pfo < pfomin)
            {
              pfomin = pfo;
              ishared = gl2;
            }
        }
    }

  if (ishared == -1)
    return 0;

  int ret = fabs(phcentraltrack->get_zed(ishared) - phcentraltrack->get_zed(i)) < deltaZ_cut
            && fabs(phcentraltrack->get_phi(ishared) - phi) < deltaPhi_cut; //is it a ghost track ?
  ret |= pfomin < pfoa_cut;   // is it sharing RICH ring ?

  if (!ret)
    return 0;  // accept if it's not a ghost or sharing

  // procedure to reject only one of the electron sharing ring or ghost track
  int check_shared_accept = 0;
  if (ret)
    for (unsigned int j = 0; j < shared_accept.size(); j++)
      if (shared_accept[j] == ishared)
        {
          check_shared_accept = 1;
          break;
        }
  if (check_shared_accept == 1)
    ret = 1;  // if the shared electron was accepted reject this
  else
    {
      if (phcentraltrack->get_n0(i) > phcentraltrack->get_n0(ishared))
	{
	  shared_accept.push_back(i);
	  ret = 0;
	}
      if (phcentraltrack->get_n0(i) < phcentraltrack->get_n0(ishared))
	{
	  shared_accept.push_back(ishared);
	}
      if (phcentraltrack->get_n0(i) == phcentraltrack->get_n0(ishared))
	{
	  if (phcentraltrack->get_chi2(i) / phcentraltrack->get_npe0(i) > phcentraltrack->get_chi2(ishared) / phcentraltrack->get_npe0(ishared))
	    {
	      shared_accept.push_back(i);
	      ret = 0;
	    }
	  else
	    {
	      shared_accept.push_back(ishared);
	    }
	}
    }
  return (bool)ret; // 0 means accept, 1 means reject
}

