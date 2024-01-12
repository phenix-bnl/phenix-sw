#include <Fun4AllHistoManager.h>
#include <Fun4AllServer.h>
#include <TriggerHelper.h>
#include <getClass.h>
#include <ErtOut.h>
#include <ErtUtils.h>
#include <PHCentralTrack.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>
#include <CrkGeometryObject.hh>
#include <PHGeometry.h>
#include <RunHeader.h>

#include <TH1F.h>
#include <TH2F.h>
#include <TH3F.h>

#include <QADefs.h>

#include <vector>
#include "histErt.h"

using namespace std;

TH1F* ert_counter;

TH2F* ert_smhit_4x4a;
TH2F* ert_smhit_4x4b;
TH2F* ert_smhit_4x4c;
TH2F* ert_smhit_2x2;
TH2F* ert_smhit_RICH;

TH2F* ert_smhit_mb_4x4a;
TH2F* ert_smhit_mb_4x4b;
TH2F* ert_smhit_mb_4x4c;
TH2F* ert_smhit_mb_2x2;
TH2F* ert_smhit_mb_RICH;

TH3F* ert_sm_RICH_el;
TH3F* ert_sm_RICH_elert;
TH3F* ert_sm_EMC_el;
TH3F* ert_sm_4x4a_elert;
TH3F* ert_sm_4x4b_elert;
TH3F* ert_sm_4x4c_elert;
TH3F* ert_sm_2x2_elert;

TH2F* ert_gl1emu_4x4a;
TH2F* ert_gl1emu_4x4b;
TH2F* ert_gl1emu_4x4c;
TH2F* ert_gl1emu_2x2;
TH2F* ert_gl1emu_el;

const string QAErt::TRIG_NAME_BBC  = "BBCLL1(>0 tubes)";
const string QAErt::TRIG_NAME_4X4A = "ERTLL1_4x4a&BBCLL1";
const string QAErt::TRIG_NAME_4X4B = "ERTLL1_4x4b&BBCLL1";
const string QAErt::TRIG_NAME_4X4C = "ERTLL1_4x4c&BBCLL1";
const string QAErt::TRIG_NAME_2X2  = "ERTLL1_2x2&BBCLL1";
const string QAErt::TRIG_NAME_E    = "ERTLL1_E&BBCLL1";

int QAErt::InitRun(PHCompositeNode *topNode)
{
   Fun4AllServer *se = Fun4AllServer::instance();
//  TrigLvl1* triglvl1 = findNode::getClass<TrigLvl1>(topNode, "TrigLvl1");
   ErtOut* ertout = findNode::getClass<ErtOut>(topNode, "ErtOut");                                                                  
   if (! ertout) {
       se->unregisterSubsystem(this);
       return 0;
   }

   // check for HistoManager - if it does not exist, create it
   // HistoManagerName is defined in QADefs.h
   Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
   if (! hm) {
      hm = new Fun4AllHistoManager(HistoManagerName);
      se->registerHistoManager(hm);
   }
   ert_counter = new TH1F("ert_counter", "ert counter", 100, 0, 100);

   ert_smhit_4x4a = new TH2F("ert_smhit_4x4a","ert sm hit:armsect", 32, 0, 32, 8, 0, 8);
   ert_smhit_4x4b = new TH2F("ert_smhit_4x4b","ert sm hit:armsect", 32, 0, 32, 8, 0, 8);
   ert_smhit_4x4c = new TH2F("ert_smhit_4x4c","ert sm hit:armsect", 32, 0, 32, 8, 0, 8);
   ert_smhit_2x2  = new TH2F("ert_smhit_2x2", "ert sm hit:armsect", 32, 0, 32, 8, 0, 8);
   ert_smhit_RICH = new TH2F("ert_smhit_RICH","ert sm hit:armsect", 32, 0, 32, 8, 0, 8);

   ert_smhit_mb_4x4a = new TH2F("ert_smhit_mb_4x4a","ert sm hit:armsect", 32, 0, 32, 8, 0, 8);
   ert_smhit_mb_4x4b = new TH2F("ert_smhit_mb_4x4b","ert sm hit:armsect", 32, 0, 32, 8, 0, 8);
   ert_smhit_mb_4x4c = new TH2F("ert_smhit_mb_4x4c","ert sm hit:armsect", 32, 0, 32, 8, 0, 8);
   ert_smhit_mb_2x2  = new TH2F("ert_smhit_mb_2x2", "ert sm hit:armsect", 32, 0, 32, 8, 0, 8);
   ert_smhit_mb_RICH = new TH2F("ert_smhit_mb_RICH","ert sm hit:armsect", 32, 0, 32, 8, 0, 8);

   ert_gl1emu_4x4a = new TH2F("ert_gl1emu_4x4a","half sector:GL1, Emu, GL1&&Emu", 16, 0, 16, 3, 0, 3);
   ert_gl1emu_4x4b = new TH2F("ert_gl1emu_4x4b","half sector:GL1, Emu, GL1&&Emu", 16, 0, 16, 3, 0, 3);
   ert_gl1emu_4x4c = new TH2F("ert_gl1emu_4x4c","half sector:GL1, Emu, GL1&&Emu", 16, 0, 16, 3, 0, 3);
   ert_gl1emu_2x2  = new TH2F("ert_gl1emu_2x2", "half sector:GL1, Emu, GL1&&Emu", 16, 0, 16, 3, 0, 3);
   ert_gl1emu_el   = new TH2F("ert_gl1emu_el",  "half sector:GL1, Emu, GL1&&Emu", 16, 0, 16, 3, 0, 3);

   hm->registerHisto(ert_counter);

   hm->registerHisto(ert_smhit_4x4a);
   hm->registerHisto(ert_smhit_4x4b);
   hm->registerHisto(ert_smhit_4x4c);
   hm->registerHisto(ert_smhit_2x2);
   hm->registerHisto(ert_smhit_RICH);

   hm->registerHisto(ert_gl1emu_4x4a);
   hm->registerHisto(ert_gl1emu_4x4b);
   hm->registerHisto(ert_gl1emu_4x4c);
   hm->registerHisto(ert_gl1emu_2x2);
   hm->registerHisto(ert_gl1emu_el);

   //// other init
   TriggerHelper trig_help(topNode);
   unsigned int sd_bbc  = trig_help.getLevel1Scaledown(TRIG_NAME_BBC .c_str());
   unsigned int sd_4x4a = trig_help.getLevel1Scaledown(TRIG_NAME_4X4A.c_str());
   unsigned int sd_4x4b = trig_help.getLevel1Scaledown(TRIG_NAME_4X4B.c_str());
   unsigned int sd_4x4c = trig_help.getLevel1Scaledown(TRIG_NAME_4X4C.c_str());
   unsigned int sd_2x2  = trig_help.getLevel1Scaledown(TRIG_NAME_2X2 .c_str());
   unsigned int sd_el   = trig_help.getLevel1Scaledown(TRIG_NAME_E   .c_str());
   ert_counter->SetBinContent(1, sd_bbc);
   ert_counter->SetBinContent(2, sd_4x4a);
   ert_counter->SetBinContent(3, sd_4x4b);
   ert_counter->SetBinContent(4, sd_4x4c);
   ert_counter->SetBinContent(5, sd_2x2);
   ert_counter->SetBinContent(6, sd_el);

   ert_sm_RICH_el = new TH3F("ert_sm_RICH_el","ert RICH el:sm:armsect", 32, 0, 32, 8, 0, 8, 10, -0.5, 9.5);
   ert_sm_RICH_elert = new TH3F("ert_sm_RICH_elert","ert RICH elert match:sm:armsect", 32, 0, 32, 8, 0, 8, 10, -0.5, 9.5);

   float energybin[27] = {0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.5,3.0,3.5,4.0,4.5,5.0};
   float smbin[33];
   for (int ism=0; ism<=32; ism++)
     smbin[ism] = ism;
   float armsectbin[9];
   for (int iarmsect=0; iarmsect<9; iarmsect++)
       armsectbin[iarmsect] = iarmsect;

   ert_sm_EMC_el = new TH3F("ert_sm_EMC_el","ert EMC el:sm:armsect", 32, smbin, 8, armsectbin, 26, energybin);
   ert_sm_4x4a_elert = new TH3F("ert_sm_4x4a_elert","ert 4x4a elert match:sm:armsect", 32, smbin, 8, armsectbin, 26, energybin);
   ert_sm_4x4b_elert = new TH3F("ert_sm_4x4b_elert","ert 4x4b elert match:sm:armsect", 32, smbin, 8, armsectbin, 26, energybin);
   ert_sm_4x4c_elert = new TH3F("ert_sm_4x4c_elert","ert 4x4c elert match:sm:armsect", 32, smbin, 8, armsectbin, 26, energybin);
   ert_sm_2x2_elert = new TH3F("ert_sm_2x2_elert","ert 2x2 elert match:sm:armsect", 32, smbin, 8, armsectbin, 26, energybin);

   string sectname[8] = {"W0","W1","W2","W3","E0","E1","E2","E3"};
   for (int iarmsect=0; iarmsect<8; iarmsect++)
     {
       ert_sm_EMC_el->GetYaxis()->SetBinLabel(iarmsect+1,sectname[iarmsect].c_str());
       ert_sm_RICH_el->GetYaxis()->SetBinLabel(iarmsect+1,sectname[iarmsect].c_str());
       ert_sm_4x4a_elert->GetYaxis()->SetBinLabel(iarmsect+1,sectname[iarmsect].c_str());
       ert_sm_4x4b_elert->GetYaxis()->SetBinLabel(iarmsect+1,sectname[iarmsect].c_str());
       ert_sm_4x4c_elert->GetYaxis()->SetBinLabel(iarmsect+1,sectname[iarmsect].c_str());
       ert_sm_2x2_elert->GetYaxis()->SetBinLabel(iarmsect+1,sectname[iarmsect].c_str());
       ert_sm_RICH_elert->GetYaxis()->SetBinLabel(iarmsect+1,sectname[iarmsect].c_str());
     }

   hm->registerHisto(ert_sm_RICH_el);
   hm->registerHisto(ert_sm_RICH_elert);
   hm->registerHisto(ert_sm_EMC_el);
   hm->registerHisto(ert_sm_4x4a_elert);
   hm->registerHisto(ert_sm_4x4b_elert);
   hm->registerHisto(ert_sm_4x4c_elert);
   hm->registerHisto(ert_sm_2x2_elert);

   //this is the fastest way to retrieve Crk geometry.CLS
   CrkGeometryObject* d_cgo = new CrkGeometryObject;
   for(int pmtid = 0; pmtid<5120; pmtid++)
   {
       PHPoint pmt_pos = d_cgo->GetPmtPosition(d_cgo->IdToArm(pmtid),
					       d_cgo->IdToSide(pmtid),
					       d_cgo->IdToSm(pmtid),
					       d_cgo->IdToPmt(pmtid));
       xcrk[pmtid] = pmt_pos.getX();
       ycrk[pmtid] = pmt_pos.getY();
       zcrk[pmtid] = pmt_pos.getZ();
   }
   delete d_cgo;

   //dummy call for ErtUtils function just to make it call DB first time here
   int runnumber = 0;
   RunHeader *rh = findNode::getClass<RunHeader>(topNode, "RunHeader");
   if (rh)
     runnumber = rh->get_RunNumber();
   if (ErtUtils::is_included_in_ERT_LUT(runnumber, 0, 0, 0, 1))
     return 0;

   return 0;
}

int QAErt::process_event(PHCompositeNode *topNode)
{
//   TrigLvl1* triglvl1 = findNode::getClass<TrigLvl1>(topNode, "TrigLvl1");
   ErtOut* ertout = findNode::getClass<ErtOut>(topNode, "ErtOut");
   TriggerHelper trig_help(topNode);

   //// get trigger bit
   int trig_live_bbc  = trig_help.trigLive(TRIG_NAME_BBC .c_str());
   int trig_live_4x4a = trig_help.trigLive(TRIG_NAME_4X4A.c_str());
   int trig_live_4x4b = trig_help.trigLive(TRIG_NAME_4X4B.c_str());
   int trig_live_4x4c = trig_help.trigLive(TRIG_NAME_4X4C.c_str());
   int trig_live_2x2  = trig_help.trigLive(TRIG_NAME_2X2 .c_str());
   int trig_live_el   = trig_help.trigLive(TRIG_NAME_E   .c_str());
   
   int trig_scaled_bbc  = trig_help.trigScaled(TRIG_NAME_BBC .c_str());
   int trig_scaled_4x4a = trig_help.trigScaled(TRIG_NAME_4X4A.c_str());
   int trig_scaled_4x4b = trig_help.trigScaled(TRIG_NAME_4X4B.c_str());
   int trig_scaled_4x4c = trig_help.trigScaled(TRIG_NAME_4X4C.c_str());
   int trig_scaled_2x2  = trig_help.trigScaled(TRIG_NAME_2X2 .c_str());
   int trig_scaled_el   = trig_help.trigScaled(TRIG_NAME_E   .c_str());

   int trig_laser = trig_help.trigLive("PPG(Laser)");
   if (trig_laser) return 0;

   if (trig_live_bbc)  ert_counter->AddBinContent(11);
   if (trig_live_4x4a) ert_counter->AddBinContent(12);
   if (trig_live_4x4b) ert_counter->AddBinContent(13);
   if (trig_live_4x4c) ert_counter->AddBinContent(14);
   if (trig_live_2x2)  ert_counter->AddBinContent(15);
   if (trig_live_el)   ert_counter->AddBinContent(16);

   if (trig_scaled_bbc)  ert_counter->AddBinContent(21);
   if (trig_scaled_4x4a) ert_counter->AddBinContent(22);
   if (trig_scaled_4x4b) ert_counter->AddBinContent(23);
   if (trig_scaled_4x4c) ert_counter->AddBinContent(24);
   if (trig_scaled_2x2)  ert_counter->AddBinContent(25);
   if (trig_scaled_el)   ert_counter->AddBinContent(26);

   //// ErtOut analysis
   int hit_4x4a = 0;
   int hit_4x4b = 0;
   int hit_4x4c = 0;
   int hit_2x2  = 0;
   int hit_rich = 0;
   int hit_el   = 0;
   int ash_4x4a = -1; // ash = ArmSectorHalf (0 to 15)Project3D("z")->Clone()
   int ash_4x4b = -1;
   int ash_4x4c = -1;
   int ash_2x2  = -1;
   int ash_rich = -1;

   vector<int> arm_2x2hit, sectsm_2x2hit, arm_elhit, sectsm_elhit;
   arm_2x2hit   .clear();
   sectsm_2x2hit.clear();
   arm_elhit    .clear();
   sectsm_elhit .clear();

   //// count up the number of hits in each SM
   int n_ert = ertout->get_ERThit_N();
   for (int ihit = 0; ihit < n_ert; ihit++) {
      int arm  = ertout->get_ERTarm(ihit); // 0 = west, 1 = east
      int sect = ertout->get_ERTsector(ihit);
      int sm   = ertout->get_ERTsm(ihit);
      unsigned int trig_mode = ertout->get_ERTtrigmode(ihit);

      int armsect = arm == 0  ?  sect  :  7 - sect;
      int halfsect = GetHalfSectorNum(arm, sect, sm, trig_mode);
      int ash = armsect * 2 + halfsect;

      if (trig_mode == 0) {
         hit_4x4a++;
         ash_4x4a = ash; // may be overwritten
         ert_smhit_4x4a->Fill(sm, armsect);
         if (trig_scaled_bbc) ert_smhit_mb_4x4a->Fill(sm, armsect);
      } else if (trig_mode == 1) {
         hit_4x4b++;
         ash_4x4b = ash;
         ert_smhit_4x4b->Fill(sm, armsect);
         if (trig_scaled_bbc) ert_smhit_mb_4x4b->Fill(sm, armsect);
      } else if (trig_mode == 2) {
         hit_4x4c++;
         ash_4x4c = ash;
         ert_smhit_4x4c->Fill(sm, armsect);
         if (trig_scaled_bbc) ert_smhit_mb_4x4c->Fill(sm, armsect);
      } else if (trig_mode == 3) {
         hit_2x2++;
         ash_2x2 = ash;
         ert_smhit_2x2->Fill(sm, armsect);
         if (trig_scaled_bbc) ert_smhit_mb_2x2->Fill(sm, armsect);
         
         int sectsm = GetSectSM(arm, sect, sm, 0);
         arm_2x2hit.push_back(arm);
         sectsm_2x2hit.push_back(sectsm);
      } else if (trig_mode == 4) {
         hit_rich++;
         ash_rich = ash;
         ert_smhit_RICH->Fill(sm, armsect);
         if (trig_scaled_bbc) ert_smhit_mb_RICH->Fill(sm, armsect);
         
         int sectsm = GetSectSM(arm, sect, sm, 1);
         arm_elhit.push_back(arm);
         sectsm_elhit.push_back(sectsm);
      }
   }
   
   if (hit_4x4a > 1) ash_4x4a = -1; // put into underflow bin if multiple hits
   if (hit_4x4b > 1) ash_4x4b = -1;
   if (hit_4x4c > 1) ash_4x4c = -1;
   if (hit_2x2  > 1) ash_2x2  = -1;
   if (hit_rich > 1) ash_rich = -1;

   int runnumber = 0;
   RunHeader *rh = findNode::getClass<RunHeader>(topNode, "RunHeader");
   if (rh)
     runnumber = rh->get_RunNumber();

   //// ERTLL1_E emulation
   for (unsigned int i2x2 = 0; i2x2 < arm_2x2hit.size(); i2x2++) {
     for (unsigned int iel = 0; iel < arm_elhit.size(); iel++)
       {
	 if (arm_2x2hit.at(i2x2) != arm_elhit.at(iel)) continue;
 	 if (ErtUtils::is_included_in_ERT_LUT(runnumber,arm_2x2hit.at(i2x2),sectsm_2x2hit.at(i2x2),sectsm_elhit.at(iel)))
	   hit_el++;
       }
   }

   //// consistency check between GL1 bit and emulated bit
   if (trig_scaled_bbc) {
      if (trig_live_4x4a                  ) ert_gl1emu_4x4a->Fill((double)0,        0);
      if (                   hit_4x4a > 0 ) ert_gl1emu_4x4a->Fill((double)ash_4x4a, 1);
      if (trig_live_4x4a && (hit_4x4a > 0)) ert_gl1emu_4x4a->Fill((double)ash_4x4a, 2);
      
      if (trig_live_4x4b                  ) ert_gl1emu_4x4b->Fill((double)0,        0);
      if (                   hit_4x4b > 0 ) ert_gl1emu_4x4b->Fill((double)ash_4x4b, 1);
      if (trig_live_4x4b && (hit_4x4b > 0)) ert_gl1emu_4x4b->Fill((double)ash_4x4b, 2);

      if (trig_live_4x4c                  ) ert_gl1emu_4x4c->Fill((double)0,        0);
      if (                   hit_4x4c > 0 ) ert_gl1emu_4x4c->Fill((double)ash_4x4c, 1);
      if (trig_live_4x4c && (hit_4x4c > 0)) ert_gl1emu_4x4c->Fill((double)ash_4x4c, 2);

      if (trig_live_2x2                 ) ert_gl1emu_2x2->Fill((double)0,       0);
      if (                  hit_2x2 > 0 ) ert_gl1emu_2x2->Fill((double)ash_2x2, 1);
      if (trig_live_2x2 && (hit_2x2 > 0)) ert_gl1emu_2x2->Fill((double)ash_2x2, 2);

      if (trig_live_el                ) ert_gl1emu_el->Fill((double)0,        0);
      if (                 hit_el > 0 ) ert_gl1emu_el->Fill((double)ash_rich, 1);
      if (trig_live_el && (hit_el > 0)) ert_gl1emu_el->Fill((double)ash_rich, 2);
   }
   processEmCal(topNode);
   processRICH(topNode);
   return 0;
}

int QAErt::GetHalfSectorNum(int arm, int sect, int sm, int trig_mode)
{
   int halfsect = -1;
   if (trig_mode != 4) { // EMCal
      if (arm == 1 && sect < 2) { // PbGl
         halfsect = sm % 8 < 4  ?  0  :  1;
      } else { // PbSc
         halfsect = sm % 6 < 4  ?  0  :  1;
      }
   } else { // RICH
      halfsect = sm % 8 < 4  ?  0  :  1;
   }
   return halfsect;
}

// return Map Number defined as
// http://www.phenix.bnl.gov/WWW/trigger/pp/c-arm/Run3/LL1/numbering_scheme.pdf
int QAErt::GetSectSM(int arm, int sect, int sm, int is_rich)
{
     int sectsm = -1;
     if (is_rich) { // RICH
	  if (arm == 0) { // West
	       sectsm = sect * 32 + sm + (7 - 2 * (sm % 8));
	  } else { // East
	       sectsm = sect * 32 + sm;
	  }
     } else { // 2x2
	  if (arm == 0) { // West
	       if (sect < 2) { // bottom...
		    if      (sm >=  0 && sm <=  2) sectsm = sect * 32 + sm + 4;
		    else if (sm >=  3 && sm <=  5) sectsm = sect * 32 + sm - 3;
		    else if (sm ==  6)             sectsm = sect * 32 + sm + 1;
		    else if (sm >=  7 && sm <=  8) sectsm = sect * 32 + sm + 5;
		    else if (sm ==  9)             sectsm = sect * 32 + sm - 6;
		    else if (sm >= 10 && sm <= 11) sectsm = sect * 32 + sm - 2;
		    else if (sm >= 12 && sm <= 13) sectsm = sect * 32 + sm + 2;
		    else if (sm == 14)             sectsm = sect * 32 + sm + 6;
		    else if (sm >= 15 && sm <= 16) sectsm = sect * 32 + sm - 5;
		    else if (sm == 17)             sectsm = sect * 32 + sm - 1;
	       } else { // top
		    int sm_mod = sm + (sm % 6 < 3 ? 3 : -3); // 0,1,2 <=> 3,4,5
		    sectsm = sect * 32 + 4 * (sm_mod / 3) + (sm_mod % 3);
	       }
	  } else { // East
	       if (sect < 2) { // PbGl
		    sectsm = sect * 32 + sm;
	       } else { // PbSc
		    sectsm = sect * 32 + 4 * (sm / 3) + (sm % 3);
	       }
	  }
     }
     return sectsm;
}

int QAErt::CheckSectSM()
{
     //// RICH East & West
     for (int iarm = 0; iarm < 2; iarm++) {
	  if (iarm == 0) cout << "== RICH East ==" << endl;
	  else           cout << "== RICH West ==" << endl;
	  for (int isect = 0; isect < 4; isect++) {
	       cout << "Sector " << isect << endl;
	       for (int ism = 0; ism < 32; ism++) {
		    int sectsm = GetSectSM(iarm, isect, ism, 1);
		    printf("%3i-%3i   ", ism, sectsm);
		    if (ism % 8 == 7) cout << endl;
	       }
	  }
	  cout << endl;
     }
     //// 2x2 East PbGl
     cout << "== 2x2 East PbGl ==" << endl;
     for (int isect = 0; isect < 2; isect++) {
	  cout << "Sector " << isect << endl;
	  for (int ism = 0; ism < 32; ism++) {
	       int sectsm = GetSectSM(1, isect, ism, 0);
	       printf("%3i-%3i   ", ism, sectsm);
	       if (ism % 8 == 7) cout << endl;
	  }
     }
     cout << endl;
     //// 2x2 East PbSc
     cout << "== 2x2 East PbSc ==" << endl;
     for (int isect = 2; isect < 4; isect++) {
	  cout << "Sector " << isect << endl;
	  for (int ism = 0; ism < 18; ism++) {
	       int sectsm = GetSectSM(1, isect, ism, 0);
	       printf("%3i-%3i   ", ism, sectsm);
	       if (ism % 6 == 5) cout << endl;
	  }
     }
     cout << endl;
     //// 2x2 West
     cout << "== 2x2 West ==" << endl;
     for (int isect = 0; isect < 4; isect++) {
       cout << "Sector " << isect << endl;
       for (int ism = 0; ism < 18; ism++) {
	 int sectsm = GetSectSM(0, isect, ism, 0);
	 printf("%3i-%3i   ", ism, sectsm);
	 if (ism % 6 == 5) cout << endl;
       }
     }

     return 1;
}

void QAErt::processRICH(PHCompositeNode *topNode)
{
  ErtOut* ertout = findNode::getClass<ErtOut>(topNode, "ErtOut");
  PHCentralTrack* phcentraltrack = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  if (!phcentraltrack)
    phcentraltrack = findNode::getClass<PHCentralTrack>(topNode, "EWGCentralTrack");
  if (!phcentraltrack)
    {
      cout << "QAErt:: PHCentralTrack object not found " << endl;
      return;
    }
  for (unsigned int i = 0; i < phcentraltrack->get_npart(); i++)
    {
      if (phcentraltrack->get_n0(i)<1 && phcentraltrack->get_sn0(i)<1) continue;
      float p = phcentraltrack->get_mom(i);
      if (p<0.2) continue;
      float ep =  phcentraltrack->get_ecore(i)/p;
      if (ep<0.6 || ep>2.0) continue;
      if (fabs(phcentraltrack->get_emcsdz_e(i))>3) continue;
      if (fabs(phcentraltrack->get_emcsdphi_e(i))>3) continue;
      float n0 = phcentraltrack->get_n0(i);
      int arm = 1-phcentraltrack->get_dcarm(i);
      float cross_phi = phcentraltrack->get_cross_phi(i);
      float cross_z = phcentraltrack->get_cross_z(i);
      int pmt = getIDpmtAssoc(arm, cross_phi, cross_z);
      int sector, sm;
      ErtUtils::get_RICH_smID_FromPMT(pmt, arm, sector, sm);
      short armsect = (arm)*4+sector;
      short w = (phcentraltrack->get_sn0(i)>=1) ? -1 : 1;
      ert_sm_RICH_el->Fill(sm,armsect,n0,w);
      //      cout << arm << " " << sector << " " << sm << endl;
      for (int iert=0; iert<ertout->get_ERThit_N(); iert++)
	{
	  if (ertout->get_ERTtrigmode(iert)!=4) continue;
	  //	  cout << ertout->get_ERTarm(iert) << " " << ertout->get_ERTsector(iert) << " " << ertout->get_ERTsm(iert) << endl;
	  if (ertout->get_ERTarm(iert)!=arm) continue;
	  if (ertout->get_ERTsector(iert)!=sector) continue;
	  if (ertout->get_ERTsm(iert)!=sm) continue;
	  ert_sm_RICH_elert->Fill(sm,armsect,n0,w);
	}
      //      cout << endl;
    }
}

void QAErt::processEmCal(PHCompositeNode *topNode)
{
  ErtOut* ertout = findNode::getClass<ErtOut>(topNode, "ErtOut");
  emcClusterContainer* clusters =
    findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");
  for (size_t iemc = 0; iemc < clusters->size(); ++iemc)
    {
      emcClusterContent* clus = clusters->getCluster(iemc);
      if (clus->prob_photon()<0.01) continue;  // reject hadrons
      float energy = clus->ecore();
      int arm = clus->arm();
      int sector = clus->sector();
      short armsect = arm*4+sector;
      int iz = clus->izpos();
      int iy = clus->iypos();
      int towerkey = ErtUtils::get_EMC_Towerkey_FromIndex(arm, sector, iy, iz);
      int sm = -1;
      if (!ErtUtils::get_EMC_smID_FromTowerkey(towerkey, arm, sector, sm))
	continue;
      if (sm<0 || sm>31) continue;
      ert_sm_EMC_el->Fill(sm,armsect,energy);
      bool ert_4x4a_fired = false;
      bool ert_4x4b_fired = false;
      bool ert_4x4c_fired = false;
      bool ert_2x2_fired  = false;
      for (int iert=0; iert<ertout->get_ERThit_N(); iert++)
	{
	  short itrig = ertout->get_ERTtrigmode(iert);
	  if (itrig==4) continue;
	  if (ertout->get_ERTarm(iert)!=arm) continue;
	  if (ertout->get_ERTsector(iert)!=sector) continue;
	  if (ertout->get_ERTsm(iert)!=sm) continue;
	  if (itrig==0) ert_4x4a_fired = true;
	  if (itrig==1) ert_4x4b_fired = true;
	  if (itrig==2) ert_4x4c_fired = true;
	  if (itrig==3) ert_2x2_fired = true;
	}
	  if (ert_4x4a_fired) ert_sm_4x4a_elert->Fill(sm,armsect,energy);
	  if (ert_4x4b_fired) ert_sm_4x4b_elert->Fill(sm,armsect,energy);
	  if (ert_4x4c_fired) ert_sm_4x4c_elert->Fill(sm,armsect,energy);
	  if (ert_2x2_fired) ert_sm_2x2_elert->Fill(sm,armsect,energy);
    }
}


int QAErt::getIDpmtAssoc(int emc_arm, float cross_phi, float cross_z)
{//.. get pmt ID associated with the track

   if(cross_phi<-8000 || cross_z<-8000) return -1;

   int pmt_min = -1;
   float dist_min = 9999;

   PHPoint pmt_cross (265*cos(cross_phi), 265*sin(cross_phi), cross_z);

   int pmt_start = -1, pmt_end = -1;
   if(emc_arm==0) {//.. west arm
      if(cross_z<0) {
         pmt_start = 0;
         pmt_end = 1280;
      } else {
         pmt_start = 1280;
         pmt_end = 2560;
      }
   } else if(emc_arm==1) {
      if(cross_z<0) {
         pmt_start = 2560;
         pmt_end = 3840;
      } else {
         pmt_start = 3840;
         pmt_end = 5120;
      }
   }

   for(int pmtid = pmt_start; pmtid<pmt_end; pmtid++) {
       PHPoint pmt_pos(xcrk[pmtid], ycrk[pmtid], zcrk[pmtid]);
       float dist = PHGeometry::distancePointToPoint(pmt_pos, pmt_cross);
       if(dist<dist_min) {
	   dist_min = dist;
	   pmt_min = pmtid;
       }
   }
   
   return pmt_min;
}
