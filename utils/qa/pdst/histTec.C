/*
 * histTec.C
 * $Id: histTec.C,v 1.14 2006/03/01 14:12:01 phnxbld Exp $
 *
 * Book and fill TEC QA histograms.
 */

#include "histTec.h"  // includes qaDstReader.h
#include "QADefs.h"
#include "TecOut.hh"
#include "TecTrackV2.hh"
#include "TriggerHelper.h"
#include "getClass.h"
#include "PHCentralTrack.h"
#include "TecCalibrationObject.hh"
#include "mTecUtilities.h"
#include "Fun4AllReturnCodes.h"

#include "Fun4AllHistoManager.h"
#include "Fun4AllServer.h"

// Root header files
#include <TH1.h>
#include <TH2.h>
#include <TProfile.h>

// TEC histograms
TH1F *tecTrkMult;
TH1F *tecMBTrkMult;
TH1F *tecTrkNhits;
TH2F *tecYvsX;
TH2F *tecTrkMultvsCglMult;
TH2F *tecTrkMult2vsCglMult;
TProfile *tecNhitProf;
TProfile *tecNtrkProf;
TH1F *tecTrkMult2;
TH1F *tecMBTrkMult2;
TH1F *tecTrkNhits2;
TProfile *tecNhitProf2;
TProfile *tecNtrkProf2;
TH1F *tecHitPlane[48];
TH2F *tecChargeTimePions[48];
TH2F *tecChargeTimeElectrons[48];
TH2F *tecPlaneMatchCglTrack;
TH2F *tecPlaneElectronMatchCglTrack;
TH2F *tecPlaneAllCglTrack;
TH2F *tecPlaneElectronAllCglTrack;
TH2F *tecSectorMatchCglTrack;
TH2F *tecSectorElectronMatchCglTrack;
TH2F *tecSectorAllCglTrack;
TH2F *tecSectorElectronAllCglTrack;

const int iSector = 4;
const int iPlane = 6;
const int iSide = 2;

using namespace std;

int QATec::InitRun(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();

  TecOut * tecout = findNode::getClass<TecOut>(topNode, "TecOut");
  TecOut * techitout = findNode::getClass<TecOut>(topNode, "TecHitOut");
  PHCentralTrack *phcentraltrack = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");

  if (!tecout || !techitout || !phcentraltrack)
    {
      se->unregisterSubsystem(this);
      return 0;
    }

  Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
  if (!hm)
    {
      hm = new Fun4AllHistoManager(HistoManagerName);
      se->registerHistoManager(hm);
    }

  tecmatchphi_cut = 3.;
  tecmatchalpha_cut = 3.;
  tecTrkMult = new TH1F("tecTrkMult", "TEC tracks/event", 100, 0.0, 500.0);
  hm->registerHisto(tecTrkMult);
  tecMBTrkMult = new TH1F("tecMBTrkMult", "MB TEC tracks/event", 250, 0.0, 500.0);
  hm->registerHisto(tecMBTrkMult);
  tecTrkNhits = new TH1F("tecTrkNhits", "TEC hits/track", 80, 0.0, 400.0);
  hm->registerHisto(tecTrkNhits);
  tecYvsX = new TH2F("tecYvsX", "TEC Y vs. X",
                     200, -450.0, -280.0, 200, -300.0, 300.0);
  hm->registerHisto(tecYvsX);
  tecYvsX->SetMarkerStyle(4);
  tecYvsX->SetMarkerSize(0.2);
  
  tecTrkMultvsCglMult = new TH2F("tecTrkMultvsCglMult",
                                 "TEC Trk Mult vs. CGL Trk Mult",
                                 200, 0.0, 1000.0, 200, 0.0, 500.0);
  hm->registerHisto(tecTrkMultvsCglMult);
  tecTrkMultvsCglMult->SetMarkerStyle(4);
  tecTrkMultvsCglMult->SetMarkerSize(0.2);

  tecTrkMult2vsCglMult = new TH2F("tecTrkMult2vsCglMult",
                                  "TEC Trk Mult2 vs. CGL Trk Mult",
                                  200, 0.0, 1000.0, 200, 0.0, 500.0);
  hm->registerHisto(tecTrkMult2vsCglMult);
  tecTrkMult2vsCglMult->SetMarkerStyle(4);
  tecTrkMult2vsCglMult->SetMarkerSize(0.2);

  tecNhitProf = new TProfile("tecNhitProf", "TEC Profile nhits/track/plane/side",
                             48, -0.5, 47.5);
  hm->registerHisto(tecNhitProf);
  tecNtrkProf = new TProfile("tecNtrkProf", "TEC Profile ntrack/sector/side",
                             8, -0.5, 7.5);
  hm->registerHisto(tecNtrkProf);
  tecTrkMult2 = new TH1F("tecTrkMult2", "TEC tracks/event", 100, 0.0, 500.0);
  hm->registerHisto(tecTrkMult2);
  tecMBTrkMult2 = new TH1F("tecMBTrkMult2", "MB TEC tracks/event", 250, 0.0, 500.0);
  hm->registerHisto(tecMBTrkMult2);
  tecTrkNhits2 = new TH1F("tecTrkNhits2", "TEC hits/track", 80, 0.0, 400.0);
  hm->registerHisto(tecTrkNhits2);
  //
  tecNhitProf2 = new TProfile("tecNhitProf2", "TEC Profile nhits/track/plane/side",
                              48, -0.5, 47.5);
  hm->registerHisto(tecNhitProf2);
  tecNtrkProf2 = new TProfile("tecNtrkProf2", "TEC Profile ntrack/sector/side",
                              8, -0.5, 7.5);
  hm->registerHisto(tecNtrkProf2);
  tecPlaneMatchCglTrack = new TH2F("tecPlaneMatchCglTrack","CGL tracks matching TEC tracks in 3 sigmas",50,0,500, 48,-0.5, 47.5);
  hm->registerHisto(tecPlaneMatchCglTrack);
  tecPlaneElectronMatchCglTrack = new TH2F("tecPlaneElectronMatchCglTrack","CGL Electrons matching TEC tracks in 3 sigmas",50, 0, 500, 48,-0.5, 47.5);
  hm->registerHisto(tecPlaneElectronMatchCglTrack);
  tecPlaneAllCglTrack = new TH2F("tecPlaneAllCglTrack","CGL tracks in TEC fiduncial region",50, 0, 500, 48,-0.5, 47.5);
  hm->registerHisto(tecPlaneAllCglTrack);
  tecPlaneElectronAllCglTrack = new TH2F("tecPlaneElectronAllCglTrack","CGL Electrons in TEC fiduncial region",50, 0, 500, 48,-0.5, 47.5);
  hm->registerHisto(tecPlaneElectronAllCglTrack);
  tecPlaneMatchCglTrack->SetYTitle("sector*12+plane*2+side");
  tecPlaneElectronMatchCglTrack->SetYTitle("sector*12+plane*2+side");
  tecPlaneAllCglTrack->SetYTitle("sector*12+plane*2+side");
  tecPlaneElectronAllCglTrack->SetYTitle("sector*12+plane*2+side");

  tecSectorMatchCglTrack = new TH2F("tecSectorMatchCglTrack","CGL tracks matching TEC tracks in 3 sigmas",50,0,500, 8,-0.5, 7.5);
  hm->registerHisto(tecSectorMatchCglTrack);
  tecSectorElectronMatchCglTrack = new TH2F("tecSectorElectronMatchCglTrack","CGL Electrons matching TEC tracks in 3 sigmas",50, 0, 500, 8,-0.5, 7.5);
  hm->registerHisto(tecSectorElectronMatchCglTrack);
  tecSectorAllCglTrack = new TH2F("tecSectorAllCglTrack","CGL tracks in TEC fiduncial region",50, 0, 500, 8,-0.5, 7.5);
  hm->registerHisto(tecSectorAllCglTrack);
  tecSectorElectronAllCglTrack = new TH2F("tecSectorElectronAllCglTrack","CGL Electrons in TEC fiduncial region",50, 0, 500, 8,-0.5, 7.5);
  hm->registerHisto(tecSectorElectronAllCglTrack);
  tecSectorMatchCglTrack->SetYTitle("sector*2+side");
  tecSectorElectronMatchCglTrack->SetYTitle("sector*2+side");
  tecSectorAllCglTrack->SetYTitle("sector*2+side");
  tecSectorElectronAllCglTrack->SetYTitle("sector*2+side");

  Char_t hName[30];
  Char_t hTitle[50];

  for (int hNum = 0; hNum < 48; hNum++)
    {
      int sector = hNum/12;
      int side = hNum%2;
      int plane = (hNum%12) / 2;
      sprintf(hName, "tecHitPlane%d", hNum);
      sprintf(hTitle, "Number of Hits on TEC Track Plane %d", hNum);
      tecHitPlane[hNum] = new TH1F(hName, hTitle, 80, 0.5, 80.5);
      hm->registerHisto(tecHitPlane[hNum]);
      sprintf(hName,"tecChargeTimePions%d",hNum);
      if (side==0)
	sprintf(hTitle,"Charge : Time E%dS plane %d (Pions) ",sector, plane);
      if (side==1)
	sprintf(hTitle,"Charge : Time E%dN plane %d (Pions) ",sector, plane);
      tecChargeTimePions[hNum] = new TH2F(hName, hTitle, 80, 0, 79, 500, 0, 500);
      hm->registerHisto(tecChargeTimePions[hNum]);
      sprintf(hName,"tecChargeTimeElectrons%d", hNum);
      if (side==0)
	sprintf(hTitle,"Charge : Time E%dS plane %d (Electrons) ",sector, plane);
      if (side==1)
	sprintf(hTitle,"Charge : Time E%dN plane %d (Electrons) ",sector, plane);
      tecChargeTimeElectrons[hNum] = new TH2F(hName, hTitle , 80, 0, 79, 500, 0, 500);
      hm->registerHisto(tecChargeTimeElectrons[hNum]);
    }

  TCO = findNode::getClass<TecCalibrationObject>(topNode, "TecCalibration");
  if (!TCO)
    {
      TCO = new TecCalibrationObject();
      TCO->Fetch();
    }

  return 0;
}

int QATec::process_event(PHCompositeNode *topNode)
{
  //
  TecOut * tecout = findNode::getClass<TecOut>(topNode, "TecOut");
  TecOut * techitout = findNode::getClass<TecOut>(topNode, "TecHitOut");
  PHCentralTrack *phcentraltrack = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");

  using namespace TecUtilities;


  if (!tecout)
    {
      cout << PHWHERE << "TecOut object not found" << endl;
      return ABORTEVENT;
    }
  if (!techitout)
    {
      cout << PHWHERE << "TecHitOut object not found" << endl;
      return ABORTEVENT;
    }
  if (!phcentraltrack)
    {
      cout << PHWHERE << "PHCentralTrack object not found" << endl;
      return ABORTEVENT;
    }
  if (!TCO)
    {
      cout << PHWHERE << "TecCalibrationObject not found " << endl;
      return ABORTEVENT;
    }

  float profx;
  int itec, index;
  int ntectrack, ntechit, nhitplane, ncgltrack;

  int tecevtracks[8] = { 0, 0, 0, 0, 0, 0, 0, 0};
  int tecevtracks2[8] = { 0, 0, 0, 0, 0, 0, 0, 0};

  TriggerHelper trig(topNode);

  ntechit = tecout->getNHits();
  ntectrack = tecout->getNTracks();
  ncgltrack = phcentraltrack->get_npart();
  tecTrkMult->Fill ((float)ntectrack);
  tecTrkMultvsCglMult->Fill( (float)ncgltrack , (float)ntectrack) ;

  if (trig.IsEventMinBias())
    tecMBTrkMult->Fill ((float)ntectrack);

  int tecid, indcgl, ncgltec;
  ncgltec = 0;

  for (itec = 0; itec < ntectrack; itec++)
    {
      indcgl = -1;
      for (int icgl = 0; icgl < ncgltrack; icgl++)
        {
          tecid = phcentraltrack->get_tecid(icgl);
          if (tecid == itec)
            {
              indcgl = icgl;
            }
        }

      if (indcgl > -1)
        ncgltec++;
      tecTrkNhits->Fill((float)tecout->getTrackNhits(itec));
      if (indcgl > -1)
        tecTrkNhits2->Fill((float)tecout->getTrackNhits(itec));

      tecYvsX->Fill((float)tecout->getTrackXin(itec),
                    (float)tecout->getTrackYin(itec));
      index = tecout->getTrackIndex(itec);
      tecevtracks[index]++;
      if (indcgl > -1)
        tecevtracks2[index]++;
      for (int ipl = 0; ipl < iPlane; ipl++)
        {
          nhitplane = tecout->getTrackNhits(itec, ipl);
          profx = float(index * iPlane) + ipl;
          tecNhitProf->Fill(profx, (float)tecout->getTrackNhits(itec, ipl));
          if (indcgl > -1)
            {
              tecNhitProf2->Fill(profx, (float)tecout->getTrackNhits(itec, ipl));
              int hNum = (index * iPlane) + ipl;
              tecHitPlane[hNum]->Fill( (float)tecout->getTrackNhits(itec, ipl)
				       );
            }
        }
      float pc3dist = tecout->getTrackPc3Distance(itec);
      float emcdist = tecout->getTrackEmcDistance(itec);
      if (fabs(pc3dist)>4 && fabs(emcdist)>4) continue;
      float pt = tecout->getTrackPt(itec);

      if (pt<0.2) continue;
      
//       int isElectron = n0>=2 && crkdist<10 && ecore>0.2;
//       int isPion = n0<1;
      
    }

    for (unsigned int central=0; central<phcentraltrack->get_npart(); central++)
      {
	if (phcentraltrack->get_dcarm(central)==1) continue;  // only east arm
	float dchmom = phcentraltrack->get_mom(central);
	if (dchmom<0.2) continue;
	int quality = phcentraltrack->get_quality(central);
	if (!(quality==31 || quality==63)) continue;
	float emcsdphi = phcentraltrack->get_emcsdphi_e(central); 
	float emcsdz = phcentraltrack->get_emcsdz_e(central);    
	if (fabs(emcsdphi)>3. || fabs(emcsdz)>3.) continue;
	int sector = phcentraltrack->get_sect(central);
	float zed = phcentraltrack->get_zed(central);
	if (zed<-500) continue; 
	int side = 0;
	if (zed>0) side = 1;
	float emce = phcentraltrack->get_emce(central);
	int n0 = phcentraltrack->get_n0(central);
	float chi2 = phcentraltrack->get_chi2(central);
	float npe0 = phcentraltrack->get_npe0(central);
	float disp = phcentraltrack->get_disp(central);

	int isElectron = emce/dchmom> 0.6;
	isElectron &= emce/dchmom< 1.3;
	isElectron &= chi2/npe0<10;
	isElectron &= n0 > 2;
	isElectron &= disp<5;
	
	int isPion = emce/dchmom > 0.06 && emce/dchmom<0.5;
	isPion &= n0<0;

	int matchtec = fabs(phcentraltrack->get_tecsdphi(central))<tecmatchphi_cut;
	matchtec *= fabs(phcentraltrack->get_tecsdalpha(central))<tecmatchalpha_cut;
	int itec = phcentraltrack->get_tecid(central);
	int index2 = sector*2+side;
	tecSectorAllCglTrack->Fill((float)ncgltrack, index2);
	tecSectorElectronAllCglTrack->Fill((float)ncgltrack, index2, isElectron);
	if (matchtec)
	  {
	    tecSectorMatchCglTrack->Fill((float)ncgltrack, index2);
	    tecSectorElectronMatchCglTrack->Fill((float)ncgltrack, index2, isElectron);	
	    for (int ihit=0; ihit<techitout->getNHits(); ihit++)
	      {
		int index = techitout->getHitIndex(ihit);
		if (techitout->getHitTrackID(ihit) != itec) continue;
		if (TCO->getAbsoluteGain(index)==0) continue;
		int timebin = techitout->getHitTimeBin(ihit);
		int adc = techitout->getHitADC(ihit);
		float charge_ = Ampl2Charge(adc);
		float charge_cal = charge_*TCO->getAbsoluteGain(index);
		//	  charge_cal *= TCO->getTimingGain(index,timebin);
		if (isElectron)
		  tecChargeTimeElectrons[index]->Fill((float)timebin, charge_cal);
		if (isPion)
		  tecChargeTimePions[index]->Fill((float)timebin, charge_cal);
	      } 
	  }
	for (int iplane=0; iplane<6; iplane++)
	  {
	    int index = sector*12 + iplane*2 + side;
	    tecPlaneAllCglTrack->Fill((float)ncgltrack, index);
	    tecPlaneElectronAllCglTrack->Fill((float)ncgltrack, index, isElectron);
	    if (matchtec==0) continue;
	    if (tecout->getTrackNhits(itec, iplane) < 3) continue;  // minimum number of hits in plane = 3
	    tecPlaneMatchCglTrack->Fill((float)ncgltrack, index);
	    tecPlaneElectronMatchCglTrack->Fill((float)ncgltrack, index, isElectron);
	  }
      }

  tecTrkMult2->Fill ((float)ncgltec);
  tecTrkMult2vsCglMult->Fill( (float)ncgltrack , (float)ncgltec) ;
  if (trig.IsEventMinBias())
    tecMBTrkMult2->Fill ((float)ncgltec);

  for (int ind = 0; ind < 8; ind++)
    {
      tecNtrkProf->Fill(float(ind), float(tecevtracks[ind]));
      tecNtrkProf2->Fill(float(ind), float(tecevtracks2[ind]));
    }
  return 0;
}




