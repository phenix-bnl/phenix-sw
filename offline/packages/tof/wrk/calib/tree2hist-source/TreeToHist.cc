//-----------------------------------------------------------------------------
//  Implementation of class TreeToHist
//
//  Author: Akio Kiyomichi
//-----------------------------------------------------------------------------
#define __TREETOHIST_CC__
#include "TreeToHist.hh"
#include <string>
#include <fstream.h> 
#include <iostream.h>
#include "gsl/gsl_math.h"
void TreeToHist::Loop(){

//     This is the loop skeleton
//       To read only selected branches, Insert statements like:
// METHOD1:
//    fChain->SetBranchStatus("*",0);  // disable all branches
//    fChain->SetBranchStatus("branchname",1);  // activate branchname
// METHOD2: replace line
//    fChain->GetEntry(i);  // read all branches
//by  b_branchname->GetEntry(i); //read only this branch

  if (fChain == 0) return;

  Int_t nentries = Int_t(fChain->GetEntries());

  Int_t nbytes = 0, nb = 0;

  Int_t thisrun;
  //constants
  const Double_t Pi = M_PI;
  const Double_t light_v = 29.9792458;
  const Float_t Mpi  = 0.13957;
  const Float_t Mpi2 = 0.13957*0.13957;
  const Float_t Mk   = 0.49368;
  const Float_t Mk2  = 0.49368*0.49368;
  const Float_t Mp   = 0.93827;
  const Float_t Mp2  = 0.93827*0.93827;

  for (Int_t jentry=0; jentry<nentries;jentry++) {
    Int_t ientry = LoadTree(jentry); //in case of a TChain, ientry is the entry number in the current file
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    // if (Cut(ientry) < 0) continue;

    //=========== User defined part ===========
    trk_counter_all++;
    if(jentry == 0)thisrun = run;

    // Timing calculation using Calibration Parameter
    Float_t Timing[2], Time[2];
    TofCalib->setGlobalT(getTofGlobalT((Int_t)run));
    Float_t Toffset = TofCalib->getToffset(slatid) + TofCalib->getGlobalT();

    Float_t slatlength = TofGeometry->getSlatLength(slatid);
    Float_t slathalflength = slatlength/2.0;
    Float_t Yoffset  = TofCalib->getYoffset(slatid);
    Float_t velocity = TofCalib->getVelocity(slatid);

    // slewing effect
    Float_t slewpar[2];
    slewpar[0] = TofCalib->getSlewPar_a(0,slatid)
      + TofCalib->getSlewPar_b(0,slatid)/sqrt((Double_t)qvc[0]);
    slewpar[1] = TofCalib->getSlewPar_a(1,slatid)
      + TofCalib->getSlewPar_b(1,slatid)/sqrt((Double_t)qvc[1]);
    Float_t tofslew = (slewpar[0] + slewpar[1])/2.;

    //## slatpos: slat center point, tofpro : projection point by tracking
    Float_t ypos = sqrt((tofpro[0]-slatpos[0])*(tofpro[0]-slatpos[0])+
			(tofpro[1]-slatpos[1])*(tofpro[1]-slatpos[1]));
    if(tofpro[1]<slatpos[1]) ypos = -ypos;

    // Timing at PMT
    Timing[0] = tvc[0]*TofCalib->getTvcConv(0,slatid);
    Timing[1] = tvc[1]*TofCalib->getTvcConv(1,slatid);

    Time[0] = Timing[0] - Toffset - (Yoffset + ypos)/velocity;
    Time[1] = Timing[1] - Toffset + (Yoffset + ypos)/velocity;

    //Time[0] = Timing[0] - slewpar[0] - Toffset - (Yoffset + ypos)/velocity;
    //Time[1] = Timing[1] - slewpar[1] - Toffset + (Yoffset + ypos)/velocity;

    //----------------------------------------------------------------------
    //  tof = (Time[0] + Time[1])/2 - tofslew
    //      = (Timing[0] + Timing[1])/2 - tofslew - Toffset
    // ypos = (Time[0] - Time[1])/2 
    //      = (Timing[0] - Timing[1])/2*Velocity - Yoffset
    //----------------------------------------------------------------------

    Float_t tof_true, t0, t0corr;
    t0 = t0bbc; // no t0 correction
    //tof_true = tof - t0;  // dTofReconstructed[].tof
    tof_true = (Timing[0] + Timing[1])/2 - tofslew - Toffset - t0;
    //Float_t m2 = mass2;
    Float_t m2 = getMass2Tof(tof_true, flightpath, ptot);
 
    Int_t npc1  = pc1Nhits;
    Int_t panel = slatid/96;
    Int_t slat  = slatid%96;

    bool cut_tof = eloss>0.002&&eloss<0.004;
    //bool pimass = m2<0.1&&m2>-0.05;
    bool pimass = m2<0.15&&m2>-0.2;
    bool proj05 = sqrt(pow(xtof[1]-tofpro[1],2)+pow(xtof[2]-tofpro[2],2))<5.0;
    bool proj07 = sqrt(pow(xtof[1]-tofpro[1],2)+pow(xtof[2]-tofpro[2],2))<7.5;
    bool proj10 = sqrt(pow(xtof[1]-tofpro[1],2)+pow(xtof[2]-tofpro[2],2))<10.0;
    bool y10z05 = (fabs(xtof[1]-tofpro[1])<10.0&&fabs(xtof[2]-tofpro[2])<5.0);
    bool sector1 = (slatid<768);
    bool sector0 = (slatid>=768);

    //bool cut_bbc = bbcChargeSum[0]+bbcChargeSum[1]>330;
    bool cut_bbc = bbcChargeSum[0]+bbcChargeSum[1]>150;

    bool cut_dch = (dch_quality>20);
    //bool cut_dch = dch_quality==23||dch_quality==31; // x1&&x2

    bool cut_mom  = ptot>0.2&&ptot<2.0;
    bool cut_mom1 = ptot>0.2&&ptot<0.6;
    bool cut_mom2 = ptot>0.6&&ptot<1.2;
    bool cut_mom3 = ptot>1.2&&ptot<2.0;

    bool timecut = cut_bbc&&cut_dch&&cut_mom&&cut_tof&&proj05&&pimass;
    bool diffcut = cut_mom&&cut_tof&&proj07;

    tofSlat->Fill(slatid);
    if(diffcut) { //good event & track
      trk_counter_diff++;
      tofSlatD->Fill(slatid);
      //## Diff = (timing[0]-timing[1])/2 ##
      tofDiff2d->Fill(slatid, (Timing[0]-Timing[1])/2);
      if(sector1){ 
	tofDiff3d->Fill(slatid, (Timing[0]-Timing[1])/2, tofpro[1]-slatpos[1]);
	tofYpos2d->Fill(slatid, xtof[1]-tofpro[1]);
      }
      if(sector0){ 
	tofDiff3d->Fill(slatid, (Timing[0]-Timing[1])/2,
			(tofpro[1]-slatpos[1])/0.9238795); // ydiff/cos22.5
	tofYpos2d->Fill(slatid, (xtof[1]-tofpro[1])/0.9238795);
      }
      tofZpos2d->Fill(slatid, xtof[2]-tofpro[2]);
    }
    if(timecut){
      trk_counter_time++;
      tofSlatT->Fill(slatid);
      //## Time ##
      tofTime2d->Fill(slatid,(Timing[0]+Timing[1])/2-t0-flighttime); 

      tofT2d->Fill(slatid, tof_true-flighttime);
      tofTft3d->Fill(slatid, t0+flighttime, tof_true-flighttime);
      tofTbb3d->Fill(slatid, t0, tof_true-flighttime);

      //## Slewing ##
      tofQvcL3d->Fill(slatid, Time[0]-t0-flighttime, qvc[0]);
      tofQvcU3d->Fill(slatid, Time[1]-t0-flighttime, qvc[1]);
      tofSlew->Fill(qvc[0], Time[0]-t0-flighttime);
      tofSlew->Fill(qvc[1], Time[1]-t0-flighttime);

      //## rundep. ##
      if(run>7600&&run<8800){
	run2->Fill(run);
	tofTrun2->Fill(run, tof_true-flighttime);
      }
      if(run>8850&&run<10450){
	run4->Fill(run);
	tofTrun4->Fill(run, tof_true-flighttime);
      }
      if(run>10700&&run<11800){
	run6->Fill(run);
	tofTrun6->Fill(run, tof_true-flighttime);
      }
      if(run>11800&&run<12500){
	run8->Fill(run);
	tofTrun8->Fill(run, tof_true-flighttime);
      }
    }
    //+-------------------+
    //| checking histgram |
    //+-------------------+
    if(cut_bbc&&cut_dch&&cut_tof&&proj05&&pimass){ // timecut-cut_mom
    }
    if(cut_dch&&proj05){
      if(dch_alpha<0){ // positive
	t0pt_pos->Fill(pt, tof_true-flighttime);
	pt_mass2->Fill(m2, pt);
	t0npc1_pos->Fill(npc1, tof_true-flighttime);
	t0npc1pt_pos->Fill(pt, npc1, tof_true-flighttime);
	
	if(pt>1.0 && pt<1.5) 
	  t0npc1_pos_10pt15->Fill(npc1, tof_true-flighttime);
	if(pt>1.5 && pt<2.0) 
	  t0npc1_pos_15pt20->Fill(npc1, tof_true-flighttime);
      }
      if(dch_alpha>0){ // negative
	t0pt_neg->Fill(pt, tof_true-flighttime);
	pt_mass2->Fill(m2,-pt);
	t0npc1_neg->Fill(npc1, tof_true-flighttime);
	t0npc1pt_neg->Fill(pt, npc1, tof_true-flighttime);
	if(pt>1.0 && pt<1.5) 
	  t0npc1_neg_10pt15->Fill(npc1, tof_true-flighttime);
	if(pt>1.5 && pt<2.0) 
	  t0npc1_neg_15pt20->Fill(npc1, tof_true-flighttime);
      }
    }
  }
  cout<<" Run: "<<thisrun;
  cout<<" Track Count (all)   : " << trk_counter_all <<endl;
  cout<<"    for Timing : " << trk_counter_time;
  cout<<"  for Position : " << trk_counter_diff;
  cout<<endl;
}
//---------------------------------------------------------------
// MEMO: TOF calculation in TofEvent.cc
//
//   tof = (T_lower + T_upper)/2 - slew - Toffset - globalT
//  ypos = (T_lower - T_upper)/2*velocity;
//   xyz = TofGeometry->getSlatXYZ(islat) + 
//    (PHPoint)TofGeometry->getSlatVector(islat)*(ypos - Yoffset);
//
//     T_lower = tvc[0]*calib->getTvcConv(0,islat)
//
//    // slewing effect
//    slewpar[0] = calib->getSlewPar_a(0,islat)
//      + calib->getSlewPar_b(0,islat)/sqrt(fqvc[0]);
//      
//    slewpar[1] = calib->getSlewPar_a(1,islat)
//      + calib->getSlewPar_b(1,islat)/sqrt(fqvc[1]);
//    slew = (slewpar[0] + slewpar[1])/2.;
//---------------------------------------------------------------

void TreeToHist::Loop_a_file(char *filename){
  TFile *ntfile = TFile::Open(filename);
  TTree *trktree = (TTree*)ntfile->Get("trktree");
  if(trktree == 0) {
    cout << "trktree is not found in "<<filename<<endl;
    ntfile->Close();
    return;
  }
  cout << filename <<" is opened"<<endl;
  Init(trktree);
  Loop();
  ntfile->Close();
}

void TreeToHist::make_hist(void) {
  Int_t slatbin = 1000; Float_t slatmin = -19.5, slatmax = 980.5;
  Int_t diffbin =  100; Float_t diffmin = -10.0, diffmax =  10.0;
  Int_t yposbin =   50; Float_t yposmin = -50.0, yposmax =  50.0;
  Int_t dposbin =  100; Float_t dposmin = -10.0, dposmax =  10.0;

  Int_t timebin =  400; Float_t timemin =   5.0, timemax =  45.0;
  //Int_t toftbin =  100; Float_t toftmin =  -5.0, toftmax =   5.0;
  Int_t toftbin =   50; Float_t toftmin =  -2.5, toftmax =   2.5;
  Int_t ftimebin =  10; Float_t ftimemin = 10.0, ftimemax = 30.0;
  Int_t t0bbcbin =  10; Float_t t0bbcmin =-10.0, t0bbcmax = 10.0;

  Int_t qvcbin  =  100; Float_t qvcmin  =   0.0, qvcmax  =  2000;
  Int_t tof0bin =  100; Float_t tof0min =  -2.0, tof0max =   3.0;

  // Slat ID
  tofSlat = new TH1F("tofSlat","TOF hits/slatid",
		     slatbin,slatmin,slatmax);
  tofSlat->SetXTitle("Slat ID");
  tofSlat->SetFillColor(8);
  tofSlatD = new TH1F("tofSlatD","TOF hits/slatid",
		      slatbin,slatmin,slatmax);
  tofSlatD->SetXTitle("Slat ID");
  tofSlatD->SetFillColor(8);
  tofSlatT = new TH1F("tofSlatT","TOF hits/slatid",
		      slatbin,slatmin,slatmax);
  tofSlatT->SetXTitle("Slat ID");
  tofSlatT->SetFillColor(8);

  // Position
  tofDiff = new TH1F("tofDiff", "(T_l - T_u)/2", 
		     diffbin,diffmin,diffmax);
  tofDiff->SetXTitle("[ns]");
  tofDiff->SetFillColor(5);

  tofDiffY = new TH2F("tofDiffY", "Ypos:(T_l - T_u)/2", 
		      diffbin,diffmin,diffmax,yposbin,yposmin,yposmax);
  tofDiff->SetXTitle("[ns]");
  tofDiff->SetYTitle("[cm]");
  tofDiff->SetFillColor(5);

  tofDiff2d = new TH2F("tofDiff2d", "(T_l - T_u)/2:slatid",
		       slatbin,slatmin,slatmax,diffbin,diffmin,diffmax);
  tofDiff2d->SetXTitle("Slat ID");
  tofDiff2d->SetYTitle("[ns]");
  tofDiff2d->SetMarkerColor(4);

  tofDiff3d = new TH3F("tofDiff3d", "Ypos:(T_l - T_u)/2:slatid",
		       slatbin,slatmin,slatmax,
		       diffbin,diffmin,diffmax,
		       yposbin,yposmin,yposmax);
  tofDiff3d->SetXTitle("Slat ID");
  tofDiff3d->SetYTitle("[ns]");
  tofDiff3d->SetZTitle("[cm]");
  tofDiff3d->SetMarkerColor(4);

  tofYpos2d = new TH2F("tofYpos2d", "Ytof-Yproj:slatid",
		       slatbin,slatmin,slatmax,dposbin,dposmin,dposmax);
  tofYpos2d->SetXTitle("Slat ID");
  tofYpos2d->SetYTitle("[cm]");
  tofYpos2d->SetMarkerColor(4);

  tofZpos2d = new TH2F("tofZpos2d", "Ztof-Zproj:slatid",
		       slatbin,slatmin,slatmax,dposbin,dposmin,dposmax);
  tofZpos2d->SetXTitle("Slat ID");
  tofZpos2d->SetYTitle("[cm]");
  tofZpos2d->SetMarkerColor(4);

  // Timing
  tofTime = new TH1F("tofTime", "Time - flightTime",
		     timebin,timemin,timemax);
  tofTime->SetXTitle("[ns]");
  tofTime->SetFillColor(5);

  tofTime2d = new TH2F("tofTime2d", "Time - L/c/beta:slatid",
		       slatbin,slatmin,slatmax,timebin,timemin,timemax);
  tofTime2d->SetXTitle("Slat ID");
  tofTime2d->SetYTitle("[ns]");
  tofTime2d->SetMarkerColor(4);

  tofT2d = new TH2F("tofT2d", "TOF - T0 - L/c/beta:slatid",
		    slatbin,slatmin,slatmax,toftbin,toftmin,toftmax);
  tofT2d->SetXTitle("Slat ID");
  tofT2d->SetYTitle("[ns]");
  tofT2d->SetMarkerColor(4);

  tofTft3d = new TH3F("tofTft3d", "TOF - T0 - L/c/beta:T0 + L/c/beta:slatid",
		      slatbin, slatmin, slatmax,
		      ftimebin,ftimemin,ftimemax, 
		      toftbin, toftmin, toftmax);
  tofTft3d->SetXTitle("Slat ID");
  tofTft3d->SetYTitle("[ns]");
  tofTft3d->SetZTitle("[ns]");
  tofTft3d->SetMarkerColor(4);

  tofTbb3d = new TH3F("tofTbb3d", "TOF - T0 - L/c/beta:T0:slatid",
		      slatbin, slatmin, slatmax,
		      t0bbcbin,t0bbcmin,t0bbcmax, 
		      toftbin, toftmin, toftmax);
  tofTbb3d->SetXTitle("Slat ID");
  tofTbb3d->SetYTitle("[ns]");
  tofTbb3d->SetZTitle("[ns]");
  tofTbb3d->SetMarkerColor(4);

  tofQvcL3d = new TH3F("tofQvcL3d", "QVC_L:TOF:slatid",
		       slatbin,slatmin,slatmax,
		       tof0bin,tof0min,tof0max,
		       qvcbin,qvcmin,qvcmax);
  tofQvcL3d->SetXTitle("Slat ID");
  tofQvcL3d->SetYTitle("[ns]");
  tofQvcL3d->SetZTitle("[ch]");
  tofQvcL3d->SetMarkerColor(4);

  tofQvcU3d = new TH3F("tofQvcU3d", "QVC_U:TOF:slatid",
		       slatbin,slatmin,slatmax,
		       tof0bin,tof0min,tof0max,
		       qvcbin,qvcmin,qvcmax);
  tofQvcU3d->SetXTitle("Slat ID");
  tofQvcU3d->SetYTitle("[ns]");
  tofQvcU3d->SetZTitle("[ch]");
  tofQvcU3d->SetMarkerColor(4);

  tofSlew = new TH2F("tofSlew", "TOF:QVC",
		     qvcbin,qvcmin,qvcmax,tof0bin,tof0min,tof0max);
  tofSlew->SetXTitle("[ns]");
  tofSlew->SetYTitle("[ch]");
  tofSlew->SetMarkerColor(4);

  // rundep.
  run2 = new TH1F("run2","# of track run2", 1200, 7600.5,  8800.5);
  run4 = new TH1F("run4","# of track run4", 1600, 8850.5, 10450.5);
  run6 = new TH1F("run6","# of track run6", 1100,10700.5, 11800.5);
  run8 = new TH1F("run8","# of track run8", 700, 11800.5, 12500.5);
  tofTrun2 = new TH2F("tofTrun2","TOF - T0 - flightTime:run",
		      1200, 7600.5,  8800.5, 100,-2.0,2.0);
  tofTrun4 = new TH2F("tofTrun4","TOF - T0 - flightTime:run",
		      1600, 8850.5, 10450.5, 100,-2.0,2.0);
  tofTrun6 = new TH2F("tofTrun6","TOF - T0 - flightTime:run",
		      1100,10700.5, 11800.5, 100,-2.0,2.0);
  tofTrun8 = new TH2F("tofTrun8","TOF - T0 - flightTime:run",
		      700, 11800.5, 12500.5, 100,-2.0,2.0);

  //--------------------+
  //| checking histgram |
  //--------------------+
  Int_t mass2bin = 200; Float_t mass2min = -0.3, mass2max = 1.7;
  Int_t mombin   = 200; Float_t mommin   = -5.0, mommax   = 5.0; // 4.0=>5.0 
  Int_t ptbin    = 200; Float_t ptmin    = -5.0, ptmax    = 5.0;

  // pt vs mass2
  pt_mass2 = new TH2F("pt_mass2", "pt vs mass2", 
		      mass2bin, mass2min, mass2max, ptbin, ptmin, ptmax);

  // t0 vs something
  Int_t t0bin   =  80; Float_t t0min   = -2.0, t0max   =  2.0;
  Int_t t0ptbin =  80; Float_t t0ptmin =  0.0, t0ptmax =  4.0;
  Int_t npc1bin =  80; Float_t npc1min =  0.0, npc1max =  400;

  // tof-t0-flighttime vs pt
  t0pt_pos = new TH2F("t0pt_pos","tof-t0-flighttime vs pt", 
		      t0ptbin, t0ptmin, t0ptmax, t0bin, t0min, t0max);
  t0pt_neg = new TH2F("t0pt_neg","tof-t0-flighttime vs pt", 
		      t0ptbin, t0ptmin, t0ptmax, t0bin, t0min, t0max);
  // tof-t0-flighttime vs npc1
  t0npc1_pos = new TH2F("t0npc1_pos","tof-t0-flighttime vs npc1", 
			npc1bin, npc1min, npc1max, t0bin, t0min, t0max);
  t0npc1_neg = new TH2F("t0npc1_neg","tof-t0-flighttime vs npc1", 
			npc1bin, npc1min, npc1max, t0bin, t0min, t0max);
  t0npc1_pos_10pt15 = 
    new TH2F("t0npc1_pos_pt10",
	     "tof-t0-flighttime vs npc1 (1.0<pt<1.5GeV/c)", 
	     npc1bin, npc1min, npc1max, t0bin, t0min, t0max);
  t0npc1_neg_10pt15 =  
    new TH2F("t0npc1_neg_10pt15",
	     "tof-t0-flighttime vs npc1 (1.0<pt<1.5GeV/c)", 
	     npc1bin, npc1min, npc1max, t0bin, t0min, t0max);
  t0npc1_pos_15pt20 = 
    new TH2F("t0npc1_pos_15pt20",
	     "tof-t0-flighttime vs npc1 (1.5<pt<2.0GeV/c)", 
	     npc1bin, npc1min, npc1max, t0bin, t0min, t0max);
  t0npc1_neg_15pt20 = 
    new TH2F("t0npc1_neg_15pt20",
	     "tof-t0-flighttime vs npc1 (1.5<pt<2.0GeV/c)", 
	     npc1bin, npc1min, npc1max, t0bin, t0min, t0max);

  // tof-t0-flighttime vs npc1 vs pt
  t0npc1pt_pos = new TH3F("t0npc1pt_pos","tof-t0-flighttime vs npc1 vs pt", 
			  t0ptbin, t0ptmin, t0ptmax, 
			  npc1bin, npc1min, npc1max, 
			  t0bin, t0min, t0max);
  t0npc1pt_neg = new TH3F("t0npc1pt_neg","tof-t0-flighttime vs npc1 vs pt", 
			  t0ptbin, t0ptmin, t0ptmax, 
			  npc1bin, npc1min, npc1max, 
			  t0bin, t0min, t0max);
}

void TreeToHist::Write(char *ofile) {
  TFile hfile(ofile,"recreate");

  cout<<endl;
  cout<<"     Histgram file : " << ofile <<endl;
  cout<<" Track Count (all) : " << trk_counter_all <<endl;
  cout<<"      for Timing   : " << trk_counter_time<<endl;
  cout<<"      for Position : " << trk_counter_diff<<endl;
  cout<<endl;

  tofSlat->Write();
  tofSlatD->Write();
  tofSlatT->Write();
  // Position
  tofDiff->Write();
  tofDiffY->Write();
  tofDiff2d->Write();
  tofDiff3d->Write();
  tofYpos2d->Write();
  tofZpos2d->Write();
  // Timing
  tofTime->Write();
  tofTime2d->Write();
  tofT2d->Write();
  tofTft3d->Write();
  tofTbb3d->Write();
  tofQvcL3d->Write();
  tofQvcU3d->Write();
  tofSlew->Write();

  // rundep.
  run2->Write();
  run4->Write();
  run6->Write();
  run8->Write();
  tofTrun2->Write();
  tofTrun4->Write();
  tofTrun6->Write();
  tofTrun8->Write();

  // checking histgram
  pt_mass2->Write();

  t0pt_pos->Write();
  t0pt_neg->Write();

  t0npc1_pos->Write();
  t0npc1_neg->Write();
  t0npc1_pos_10pt15->Write();
  t0npc1_neg_10pt15->Write();
  t0npc1_pos_15pt20->Write();
  t0npc1_neg_15pt20->Write();
  t0npc1pt_pos->Write();
  t0npc1pt_neg->Write();

  hfile.Close();
}
// Function
//
void TreeToHist::setTofGlobalT(char *txtFile){
  ifstream inputfile; 
  inputfile.open(txtFile);
  Int_t runid = 0;
  if(!inputfile){ 
    cerr << "  Can not open "<< txtFile <<" file." << endl;
    cerr << endl;
  }else{ 
    while(!inputfile.eof()){
      inputfile >> run0[runid] >> globalT0[runid];
      runid++;
    }
    // final run#
    run0[runid] = 12470;  // for Year-1 only
    globalT0[runid] = 0;
    runid++;
  }
  runsetnum = runid;
}
Float_t TreeToHist::getTofGlobalT(Int_t run){
  Int_t runsetid;
  float globalT = 0.0;
  for(Int_t i = 0; i < runsetnum-1; i++){
    if(run >= run0[i] && run < run0[i+1]){
      globalT  = globalT0[i];
      runsetid = run0[i];
      break;
    }
  }
  return globalT;
}
Float_t TreeToHist::getMass2Tof(Float_t trueTof, Float_t pltof, Float_t mom){
  float mass2 = -1;
  if (pltof>0) {
    mass2   = mom*mom*(trueTof*trueTof*29.98*29.98/(pltof*pltof)-1);
  } 
  return (mass2);
}
