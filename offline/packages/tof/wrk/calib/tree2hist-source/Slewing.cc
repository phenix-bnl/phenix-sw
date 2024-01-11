//-----------------------------------------------------------------------------
//  Implementation of class Slewing
//
//  Author: Akio Kiyomichi
//-----------------------------------------------------------------------------
#define __SLEWING_CC__
#include "Slewing.hh"
#include <string>
#include <fstream.h> 
#include <iostream.h>
#include "gsl/gsl_math.h"
void Slewing::Loop(){

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

    Float_t sqvc = sqrt((Float_t)qvc[0]*(Float_t)qvc[1]);
    //## TEMP: slewing effect [0:slewing by sqvc, 1:slewing by eloss]
    //Float_t slew_sqvc, slew_eloss;
    //slew_sqvc = TofCalib->getSlewPar_a(0,slatid)
    //  + TofCalib->getSlewPar_b(0,slatid)/sqrt((Double_t)sqvc);
    //slew_eloss = TofCalib->getSlewPar_a(1,slatid)
    //  + TofCalib->getSlewPar_b(1,slatid)/sqrt((Double_t)eloss);

    //## slatpos: slat center point, tofpro : projection point by tracking
    Float_t yhit = sqrt((xtof[0]-slatpos[0])*(xtof[0]-slatpos[0])+
			(xtof[1]-slatpos[1])*(xtof[1]-slatpos[1]));
    if(xtof[1]<slatpos[1]) yhit = -yhit;
    Float_t dpos = tofpro[1]-slatpos[1];
    if(slatpos[1]<-100) dpos = (tofpro[1]-slatpos[1])/0.9238795;

    Float_t ypos = sqrt((tofpro[0]-slatpos[0])*(tofpro[0]-slatpos[0])+
			(tofpro[1]-slatpos[1])*(tofpro[1]-slatpos[1]));
    if(tofpro[1]<slatpos[1]) ypos = -ypos;

    // Timing at PMT
    Timing[0] = tvc[0]*TofCalib->getTvcConv(0,slatid);
    Timing[1] = tvc[1]*TofCalib->getTvcConv(1,slatid);

    //Time[0] = Timing[0] - Toffset - Yoffset/velocity;
    //Time[1] = Timing[1] - Toffset + Yoffset/velocity;

    Time[0] = Timing[0] - Toffset - (Yoffset + ypos)/velocity;
    Time[1] = Timing[1] - Toffset + (Yoffset + ypos)/velocity;

    //Time[0] = Timing[0] - slewpar[0] - Toffset - (Yoffset + ypos)/velocity;
    //Time[1] = Timing[1] - slewpar[1] - Toffset + (Yoffset + ypos)/velocity;

    //Time[0] = Time[1] = tof; // TEST

    //----------------------------------------------------------------------
    //  tof = (Time[0] + Time[1])/2 - tofslew
    //      = (Timing[0] + Timing[1])/2 - tofslew - Toffset
    // ypos = (Time[0] - Time[1])/2 
    //      = (Timing[0] - Timing[1])/2*Velocity - Yoffset
    //----------------------------------------------------------------------

    Float_t tof_true, t0;
    Float_t tof_sqvc, tof_eloss;

    t0 = t0bbc; // no t0 correction

    //tof_true = tof - t0; // dTofReconstructed[].tof
    tof_true  = (Time[0] + Time[1])/2 - t0; // no slewing correction
    //tof_true = (Time[0] + Time[1])/2 - tofslew - t0; // with slewing corr.

    //tof_sqvc  = (Time[0] + Time[1])/2 - slew_sqvc  - t0; // TEMP: by sqvc
    //tof_eloss = (Time[0] + Time[1])/2 - slew_eloss - t0; // TEMP: by eloss
    tof_sqvc  =  tof_eloss = tof_true;

    //Float_t m2 = mass2;
    Float_t m2 = getMass2Tof(tof_true, flightpath, ptot);

    Float_t cqvc[2]; // gain corrected QVC
    cqvc[0] = qvc[0]*getTofPmtFactor(slatid, 0);
    cqvc[1] = qvc[1]*getTofPmtFactor(slatid, 1);

    sqvc = sqrt(cqvc[0]*cqvc[1]);

    //bool cut_bbc = bbcChargeSum[0]+bbcChargeSum[1]>330;
    bool cut_bbc = bbcChargeSum[0]+bbcChargeSum[1]>150;
    bool cut_dch = (dch_quality>20);
    //bool cut_dch = dch_quality==23||dch_quality==31;
    bool cut_tof = eloss>0.002&&eloss<0.004;
    bool pimass  = m2<0.15&&m2>-0.2;
    bool proj05 = sqrt(pow(xtof[1]-tofpro[1],2)+pow(xtof[2]-tofpro[2],2))<5.0;
    bool proj07 = sqrt(pow(xtof[1]-tofpro[1],2)+pow(xtof[2]-tofpro[2],2))<7.5;
    bool proj10 = sqrt(pow(xtof[1]-tofpro[1],2)+pow(xtof[2]-tofpro[2],2))<10.0;
    bool y10z05 = (fabs(xtof[1]-tofpro[1])<10.0&&fabs(xtof[2]-tofpro[2])<5.0);
    bool sector1 = (slatid<768);
    bool sector0 = (slatid>=768);

    Int_t npc1 = pc1Nhits;
    Int_t panel = slatid/96;
    Int_t slat  = slatid%96;
    bool  slatL  = slat>=16&&slat<80;
    bool  slatLa = slatL&&((slat%16)/4==0);
    bool  slatLb = slatL&&((slat%16)/4==1);
    bool  slatLc = slatL&&((slat%16)/4==2);
    bool  slatLd = slatL&&((slat%16)/4==3);
    bool  slatSd = slat>=0&&slat<16;
    bool  slatSu = slat>=80&&slat<96;
    bool  slatS  = slatSd||slatSu;
    bool  slatSa = (slat>=0&&slat<8)||(slat>=80&&slat<88);
    bool  slatSb = (slat>=8&&slat<16)||(slat>=88&&slat<96);

    Int_t slatset;
    if(slatSa) slatset = 2;
    if(slatSb) slatset = 3;
    if(slatLa) slatset = 4;
    if(slatLb) slatset = 5;
    if(slatLc) slatset = 6;
    if(slatLd) slatset = 7;
    
    Float_t posFromLower = slathalflength + ypos;
    Float_t posFromUpper = slathalflength - ypos;
    Int_t ipos0 = 5, ipos1 = 5;

    if(posFromLower>=0.0&&posFromLower<11.0)  ipos0 = 0;
    if(posFromLower>=11.0&&posFromLower<22.0) ipos0 = 1;
    if(posFromLower>=22.0&&posFromLower<33.0) ipos0 = 2;
    if(posFromLower>=33.0&&posFromLower<44.0) ipos0 = 3;
    if(posFromLower>=44.0&&posFromLower<55.0) ipos0 = 4;
    if(posFromLower>=55.0&&posFromLower<66.0) ipos0 = 5;
    if(posFromUpper>=0.0&&posFromUpper<11.0)  ipos1 = 0;
    if(posFromUpper>=11.0&&posFromUpper<22.0) ipos1 = 1;
    if(posFromUpper>=22.0&&posFromUpper<33.0) ipos1 = 2;
    if(posFromUpper>=33.0&&posFromUpper<44.0) ipos1 = 3;
    if(posFromUpper>=44.0&&posFromUpper<55.0) ipos1 = 4;
    if(posFromUpper>=55.0&&posFromUpper<66.0) ipos1 = 5;

    bool cut_mom = pt<1.0&&pt>0.5;

    if(fabs(yhit)<3.0){
      tofSlatD->Fill(slatid);
      tofQvcL2d->Fill(slatid, qvc[0]);
      tofQvcU2d->Fill(slatid, qvc[1]);
    }

    //if(cut_bbc&&cut_dch&&proj05){
    if(cut_bbc&&cut_dch&&proj05&&fabs(ypos)<3.0){
      trk_counter_all++;
      tofSlewPt->Fill(pt, cqvc[0], Time[0]-t0-flighttime);
      tofSlewPt->Fill(pt, cqvc[1], Time[1]-t0-flighttime);
      if(slatL) tofSlew3d[panel][0]->Fill(pt,cqvc[0],Time[0]-t0-flighttime);
      if(slatL) tofSlew3d[panel][0]->Fill(pt,cqvc[1],Time[1]-t0-flighttime);
      if(slatS) tofSlew3d[panel][1]->Fill(pt,cqvc[0],Time[0]-t0-flighttime);
      if(slatS) tofSlew3d[panel][1]->Fill(pt,cqvc[1],Time[1]-t0-flighttime);
      if(slatSd)tofSlew3d[panel][3]->Fill(pt,cqvc[0],Time[0]-t0-flighttime);
      if(slatSd)tofSlew3d[panel][2]->Fill(pt,cqvc[1],Time[1]-t0-flighttime);
      if(slatSu)tofSlew3d[panel][2]->Fill(pt,cqvc[0],Time[0]-t0-flighttime);
      if(slatSu)tofSlew3d[panel][3]->Fill(pt,cqvc[1],Time[1]-t0-flighttime);
      if(slatLa)tofSlew3d[panel][4]->Fill(pt,cqvc[0],Time[0]-t0-flighttime);
      if(slatLa)tofSlew3d[panel][4]->Fill(pt,cqvc[1],Time[1]-t0-flighttime);
      if(slatLb)tofSlew3d[panel][5]->Fill(pt,cqvc[0],Time[0]-t0-flighttime);
      if(slatLb)tofSlew3d[panel][5]->Fill(pt,cqvc[1],Time[1]-t0-flighttime);
      if(slatLc)tofSlew3d[panel][6]->Fill(pt,cqvc[0],Time[0]-t0-flighttime);
      if(slatLc)tofSlew3d[panel][6]->Fill(pt,cqvc[1],Time[1]-t0-flighttime);
      if(slatLd)tofSlew3d[panel][7]->Fill(pt,cqvc[0],Time[0]-t0-flighttime);
      if(slatLd)tofSlew3d[panel][7]->Fill(pt,cqvc[1],Time[1]-t0-flighttime);

      // TOF vs sqrt(qvc[0]*qvc[1]) or eloss
      if(slatL)  tofSqvc3d[panel][0]->Fill(pt,sqvc, tof_sqvc-flighttime);
      if(slatL) tofEloss3d[panel][0]->Fill(pt,eloss,tof_eloss-flighttime);
      if(slatS)  tofSqvc3d[panel][1]->Fill(pt,sqvc, tof_sqvc-flighttime);
      if(slatS) tofEloss3d[panel][1]->Fill(pt,eloss,tof_eloss-flighttime);
      if(slatSa) tofSqvc3d[panel][2]->Fill(pt,sqvc, tof_sqvc-flighttime);
      if(slatSa)tofEloss3d[panel][2]->Fill(pt,eloss,tof_eloss-flighttime);
      if(slatSb) tofSqvc3d[panel][3]->Fill(pt,sqvc, tof_sqvc-flighttime);
      if(slatSb)tofEloss3d[panel][3]->Fill(pt,eloss,tof_eloss-flighttime);
      if(slatLa) tofSqvc3d[panel][4]->Fill(pt,sqvc, tof_sqvc-flighttime);
      if(slatLa)tofEloss3d[panel][4]->Fill(pt,eloss,tof_eloss-flighttime);
      if(slatLb) tofSqvc3d[panel][5]->Fill(pt,sqvc, tof_sqvc-flighttime);
      if(slatLb)tofEloss3d[panel][5]->Fill(pt,eloss,tof_eloss-flighttime);
      if(slatLc) tofSqvc3d[panel][6]->Fill(pt,sqvc, tof_sqvc-flighttime);
      if(slatLc)tofEloss3d[panel][6]->Fill(pt,eloss,tof_eloss-flighttime);
      if(slatLd) tofSqvc3d[panel][7]->Fill(pt,sqvc, tof_sqvc-flighttime);
      if(slatLd)tofEloss3d[panel][7]->Fill(pt,eloss,tof_eloss-flighttime);

      if(cut_mom){
	tofSlat->Fill(slatid);
	trk_counter_diff++;
	if(cut_tof&&pimass){
	  trk_counter_time++;
	  tofSlatT->Fill(slatid);
	  tofT2d->Fill(slatid, tof_true-flighttime);
	}
	//tofQvcL2d->Fill(slatid, qvc[0]);
	//tofQvcU2d->Fill(slatid, qvc[1]);
	tofQvcL3d->Fill(slatid, Time[0]-t0-flighttime, qvc[0]);
	tofQvcU3d->Fill(slatid, Time[1]-t0-flighttime, qvc[1]);
	tofSlew->Fill(cqvc[0], Time[0]-t0-flighttime);
	tofSlew->Fill(cqvc[1], Time[1]-t0-flighttime);

	// Time[] vs qvc[]
	if(slatL) tofSlew2d[panel][0]->Fill(cqvc[0],Time[0]-t0-flighttime);
	if(slatL) tofSlew2d[panel][0]->Fill(cqvc[1],Time[1]-t0-flighttime);
	if(slatS) tofSlew2d[panel][1]->Fill(cqvc[0],Time[0]-t0-flighttime);
	if(slatS) tofSlew2d[panel][1]->Fill(cqvc[1],Time[1]-t0-flighttime);
	if(slatSd)tofSlew2d[panel][3]->Fill(cqvc[0],Time[0]-t0-flighttime);//90
	if(slatSd)tofSlew2d[panel][2]->Fill(cqvc[1],Time[1]-t0-flighttime);//180
	if(slatSu)tofSlew2d[panel][2]->Fill(cqvc[0],Time[0]-t0-flighttime);//180
	if(slatSu)tofSlew2d[panel][3]->Fill(cqvc[1],Time[1]-t0-flighttime);//90
	if(slatLa)tofSlew2d[panel][4]->Fill(cqvc[0],Time[0]-t0-flighttime);
	if(slatLa)tofSlew2d[panel][4]->Fill(cqvc[1],Time[1]-t0-flighttime);
	if(slatLb)tofSlew2d[panel][5]->Fill(cqvc[0],Time[0]-t0-flighttime);
	if(slatLb)tofSlew2d[panel][5]->Fill(cqvc[1],Time[1]-t0-flighttime);
	if(slatLc)tofSlew2d[panel][6]->Fill(cqvc[0],Time[0]-t0-flighttime);
	if(slatLc)tofSlew2d[panel][6]->Fill(cqvc[1],Time[1]-t0-flighttime);
	if(slatLd)tofSlew2d[panel][7]->Fill(cqvc[0],Time[0]-t0-flighttime);
	if(slatLd)tofSlew2d[panel][7]->Fill(cqvc[1],Time[1]-t0-flighttime);

	// Time[] vs qvc[]
	if(slatL) 
	  tofSlewPos2d[panel][0][ipos0]->Fill(cqvc[0],Time[0]-t0-flighttime);
	if(slatL) 
	  tofSlewPos2d[panel][0][ipos1]->Fill(cqvc[1],Time[1]-t0-flighttime);
	if(slatS) 
	  tofSlewPos2d[panel][1][ipos0]->Fill(cqvc[0],Time[0]-t0-flighttime);
	if(slatS) 
	  tofSlewPos2d[panel][1][ipos1]->Fill(cqvc[1],Time[1]-t0-flighttime);
	if(slatSd)//90
	  tofSlewPos2d[panel][3][ipos0]->Fill(cqvc[0],Time[0]-t0-flighttime);
	if(slatSd)//180
	  tofSlewPos2d[panel][2][ipos1]->Fill(cqvc[1],Time[1]-t0-flighttime);
	if(slatSu)//180
	  tofSlewPos2d[panel][2][ipos0]->Fill(cqvc[0],Time[0]-t0-flighttime);
	if(slatSu)//90
	  tofSlewPos2d[panel][3][ipos1]->Fill(cqvc[1],Time[1]-t0-flighttime);
	if(slatLa)
	  tofSlewPos2d[panel][4][ipos0]->Fill(cqvc[0],Time[0]-t0-flighttime);
	if(slatLa)
	  tofSlewPos2d[panel][4][ipos1]->Fill(cqvc[1],Time[1]-t0-flighttime);
	if(slatLb)
	  tofSlewPos2d[panel][5][ipos0]->Fill(cqvc[0],Time[0]-t0-flighttime);
	if(slatLb)
	  tofSlewPos2d[panel][5][ipos1]->Fill(cqvc[1],Time[1]-t0-flighttime);
	if(slatLc)
	  tofSlewPos2d[panel][6][ipos0]->Fill(cqvc[0],Time[0]-t0-flighttime);
	if(slatLc)
	  tofSlewPos2d[panel][6][ipos1]->Fill(cqvc[1],Time[1]-t0-flighttime);
	if(slatLd)
	  tofSlewPos2d[panel][7][ipos0]->Fill(cqvc[0],Time[0]-t0-flighttime);
	if(slatLd)
	  tofSlewPos2d[panel][7][ipos1]->Fill(cqvc[1],Time[1]-t0-flighttime);

	// TOF vs qvc[]
	if(slatL) tofSlewT2d[panel][0]->Fill(cqvc[0],tof_true-flighttime);
	if(slatL) tofSlewT2d[panel][0]->Fill(cqvc[1],tof_true-flighttime);
	if(slatS) tofSlewT2d[panel][1]->Fill(cqvc[0],tof_true-flighttime);
	if(slatS) tofSlewT2d[panel][1]->Fill(cqvc[1],tof_true-flighttime);
	if(slatSd)tofSlewT2d[panel][3]->Fill(cqvc[0],tof_true-flighttime);//90
	if(slatSd)tofSlewT2d[panel][2]->Fill(cqvc[1],tof_true-flighttime);//180
	if(slatSu)tofSlewT2d[panel][2]->Fill(cqvc[0],tof_true-flighttime);//180
	if(slatSu)tofSlewT2d[panel][3]->Fill(cqvc[1],tof_true-flighttime);//90
	if(slatLa)tofSlewT2d[panel][4]->Fill(cqvc[0],tof_true-flighttime);
	if(slatLa)tofSlewT2d[panel][4]->Fill(cqvc[1],tof_true-flighttime);
	if(slatLb)tofSlewT2d[panel][5]->Fill(cqvc[0],tof_true-flighttime);
	if(slatLb)tofSlewT2d[panel][5]->Fill(cqvc[1],tof_true-flighttime);
	if(slatLc)tofSlewT2d[panel][6]->Fill(cqvc[0],tof_true-flighttime);
	if(slatLc)tofSlewT2d[panel][6]->Fill(cqvc[1],tof_true-flighttime);
	if(slatLd)tofSlewT2d[panel][7]->Fill(cqvc[0],tof_true-flighttime);
	if(slatLd)tofSlewT2d[panel][7]->Fill(cqvc[1],tof_true-flighttime);
	// Time[] vs sqrt(qvc[0]*qvc[1])
	if(slatL) tofSlewS2d[panel][0]->Fill(sqvc,Time[0]-t0-flighttime);
	if(slatL) tofSlewS2d[panel][0]->Fill(sqvc,Time[1]-t0-flighttime);
	if(slatS) tofSlewS2d[panel][1]->Fill(sqvc,Time[0]-t0-flighttime);
	if(slatS) tofSlewS2d[panel][1]->Fill(sqvc,Time[1]-t0-flighttime);
	if(slatSd)tofSlewS2d[panel][3]->Fill(sqvc,Time[0]-t0-flighttime);//90
	if(slatSd)tofSlewS2d[panel][2]->Fill(sqvc,Time[1]-t0-flighttime);//180
	if(slatSu)tofSlewS2d[panel][2]->Fill(sqvc,Time[0]-t0-flighttime);//180
	if(slatSu)tofSlewS2d[panel][3]->Fill(sqvc,Time[1]-t0-flighttime);//90
	if(slatLa)tofSlewS2d[panel][4]->Fill(sqvc,Time[0]-t0-flighttime);
	if(slatLa)tofSlewS2d[panel][4]->Fill(sqvc,Time[1]-t0-flighttime);
	if(slatLb)tofSlewS2d[panel][5]->Fill(sqvc,Time[0]-t0-flighttime);
	if(slatLb)tofSlewS2d[panel][5]->Fill(sqvc,Time[1]-t0-flighttime);
	if(slatLc)tofSlewS2d[panel][6]->Fill(sqvc,Time[0]-t0-flighttime);
	if(slatLc)tofSlewS2d[panel][6]->Fill(sqvc,Time[1]-t0-flighttime);
	if(slatLd)tofSlewS2d[panel][7]->Fill(sqvc,Time[0]-t0-flighttime);
	if(slatLd)tofSlewS2d[panel][7]->Fill(sqvc,Time[1]-t0-flighttime);

	// TOF vs sqrt(qvc[0]*qvc[1]) or eloss
	if(slatL)  tofSqvc2d[panel][0]->Fill(sqvc, tof_sqvc-flighttime);
	if(slatL) tofEloss2d[panel][0]->Fill(eloss,tof_eloss-flighttime);
	if(slatS)  tofSqvc2d[panel][1]->Fill(sqvc, tof_sqvc-flighttime);
	if(slatS) tofEloss2d[panel][1]->Fill(eloss,tof_eloss-flighttime);
	if(slatSa) tofSqvc2d[panel][2]->Fill(sqvc, tof_sqvc-flighttime);
	if(slatSa)tofEloss2d[panel][2]->Fill(eloss,tof_eloss-flighttime);
	if(slatSb) tofSqvc2d[panel][3]->Fill(sqvc, tof_sqvc-flighttime);
	if(slatSb)tofEloss2d[panel][3]->Fill(eloss,tof_eloss-flighttime);
	if(slatLa) tofSqvc2d[panel][4]->Fill(sqvc, tof_sqvc-flighttime);
	if(slatLa)tofEloss2d[panel][4]->Fill(eloss,tof_eloss-flighttime);
	if(slatLb) tofSqvc2d[panel][5]->Fill(sqvc, tof_sqvc-flighttime);
	if(slatLb)tofEloss2d[panel][5]->Fill(eloss,tof_eloss-flighttime);
	if(slatLc) tofSqvc2d[panel][6]->Fill(sqvc, tof_sqvc-flighttime);
	if(slatLc)tofEloss2d[panel][6]->Fill(eloss,tof_eloss-flighttime);
	if(slatLd) tofSqvc2d[panel][7]->Fill(sqvc, tof_sqvc-flighttime);
	if(slatLd)tofEloss2d[panel][7]->Fill(eloss,tof_eloss-flighttime);
      }

//        if(panel == 6){
//  	Float_t nttof[23], ntraw[22];
//  	nttof[0]  = ntraw[0]  = slatid;
//  	nttof[1]  = ntraw[1]  = panel;
//  	nttof[2]  = ntraw[2]  = slat;
//  	nttof[3]  = ntraw[3]  = slatset; // set
//  	nttof[4]  = ntraw[4]  = ptot;
//  	nttof[5]  = ntraw[5]  = pt;
//  	nttof[6]  = ntraw[6]  = charge;
//  	nttof[7]  = ntraw[7]  = t0;
//  	nttof[8]  = ntraw[8]  = flighttime;
//  	nttof[9]  = ntraw[9]  = Toffset;
//  	nttof[10] = ntraw[10] = Yoffset;
//  	nttof[11] = ntraw[11] = velocity;
//  	nttof[12] = ntraw[12] = yhit;
//  	nttof[13] = ntraw[13] = dpos;
//  	nttof[14] = ntraw[14] = ypos;
//  	nttof[15] = ntraw[15] = posFromLower;
//  	nttof[16] = ntraw[16] = posFromUpper;
//  	nttof[17] = Timing[0]; nttof[18] = Timing[1];
//  	nttof[19] = (Time[0] + Time[1])/2;
//  	nttof[20] = qvc[0]; nttof[21] = qvc[1]; nttof[22] = eloss;
//  	nt_tof->Fill(nttof);
//  	ntraw[20] = eloss; 
//  	ntraw[17] = Timing[0]; ntraw[18] = Time[0]; ntraw[19] = qvc[0];
//  	ntraw[21] = -1; if(slatSd) ntraw[3] = 3; if(slatSu) ntraw[3] = 2;
//  	nt_raw->Fill(ntraw);
//  	ntraw[17] = Timing[1]; ntraw[18] = Time[1]; ntraw[19] = qvc[1];
//  	ntraw[21] = 1; if(slatSd) ntraw[3] = 2; if(slatSu) ntraw[3] = 3;
//  	nt_raw->Fill(ntraw);
//        }
    }// if(cut_bbc....)
  }// jentry
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

void Slewing::Loop_a_file(char *filename){
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

void Slewing::make_hist(void) {
  Int_t slatbin = 1000; Float_t slatmin = -19.5, slatmax = 980.5;

  Int_t toftbin =  100; Float_t toftmin =  -5.0, toftmax =   5.0;

  //Int_t qvcbin  =  100; Float_t qvcmin  =   0.0, qvcmax  =  2000;
  Int_t qvcbin  =   50; Float_t qvcmin  =   0.0, qvcmax  =  2000;
  Int_t tof0bin =  100; Float_t tof0min =  -2.0, tof0max =   3.0;
  Int_t t0ptbin =   80; Float_t t0ptmin =   0.0, t0ptmax =   4.0;
  Int_t elossbin =  50; Float_t elossmin =  0.0, elossmax = 0.01;

  Char_t name[20], title[80];

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

  tofT2d = new TH2F("tofT2d", "TOF - T0 - L/C:slatid",
		    slatbin,slatmin,slatmax,toftbin,toftmin,toftmax);
  tofT2d->SetXTitle("Slat ID");
  tofT2d->SetYTitle("[ns]");
  tofT2d->SetMarkerColor(4);

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

  tofQvcL2d = new TH2F("tofQvcL2d", "qvc[0]:slatid",
		       slatbin,slatmin,slatmax,qvcbin,qvcmin,qvcmax);
  tofQvcL2d->SetXTitle("Slat ID");
  tofQvcL2d->SetYTitle("[ch]");
  tofQvcL2d->SetMarkerColor(4);
  
  tofQvcU2d = new TH2F("tofQvcU2d", "qvc[1]:slatid",
		       slatbin,slatmin,slatmax,qvcbin,qvcmin,qvcmax);
  tofQvcU2d->SetXTitle("Slat ID");
  tofQvcU2d->SetYTitle("[ch]");
  tofQvcU2d->SetMarkerColor(4);

  tofSlew = new TH2F("tofSlew", "TOF:QVC",
		     qvcbin,qvcmin,qvcmax,tof0bin,tof0min,tof0max);
  tofSlew->SetXTitle("[ch]");
  tofSlew->SetYTitle("[ns]");
  tofSlew->SetMarkerColor(4);

  cout<<"### Create Histgram ###"<<endl;
  for(Int_t ipanel = 0; ipanel<TOF_NPANEL_ALL; ipanel++){
    for(Int_t islat = 0; islat<8; islat++){
      sprintf(name, "tofSlewPt%d_%d", ipanel, islat);
      if(islat == 0) sprintf(title, "Time:QVC:pt (panel%d long)", ipanel);
      if(islat == 1) sprintf(title, "Time:QVC:pt (panel%d short)", ipanel);
      if(islat == 2) sprintf(title, "Time:QVC:pt (panel%d short 180)", ipanel);
      if(islat == 3) sprintf(title, "Time:QVC:pt (panel%d short 90)", ipanel);
      if(islat == 4) sprintf(title, "Time:QVC:pt (panel%d long-0)", ipanel);
      if(islat == 5) sprintf(title, "Time:QVC:pt (panel%d long-1)", ipanel);
      if(islat == 6) sprintf(title, "Time:QVC:pt (panel%d long-2)", ipanel);
      if(islat == 7) sprintf(title, "Time:QVC:pt (panel%d long-3)", ipanel);

      tofSlew3d[ipanel][islat] = new TH3F(name, title,
					  t0ptbin,t0ptmin,t0ptmax, 
					  qvcbin, qvcmin, qvcmax,
					  tof0bin,tof0min,tof0max);
      tofSlew3d[ipanel][islat]->SetXTitle("[GeV/c]");
      tofSlew3d[ipanel][islat]->SetYTitle("[ch]");
      tofSlew3d[ipanel][islat]->SetZTitle("[ns]");
      tofSlew3d[ipanel][islat]->SetMarkerColor(4);

      sprintf(name, "tofSqvcPt%d_%d", ipanel, islat);
      if(islat == 0) sprintf(title, "TOF:sqrtQVC:pt (panel%d long)", ipanel);
      if(islat == 1) sprintf(title, "TOF:sqrtQVC:pt (panel%d short)", ipanel);
      if(islat == 2) sprintf(title, "TOF:sqrtQVC:pt (panel%d short-0)",ipanel);
      if(islat == 3) sprintf(title, "TOF:sqrtQVC:pt (panel%d short-1)",ipanel);
      if(islat == 4) sprintf(title, "TOF:sqrtQVC:pt (panel%d long-0)", ipanel);
      if(islat == 5) sprintf(title, "TOF:sqrtQVC:pt (panel%d long-1)", ipanel);
      if(islat == 6) sprintf(title, "TOF:sqrtQVC:pt (panel%d long-2)", ipanel);
      if(islat == 7) sprintf(title, "TOF:sqrtQVC:pt (panel%d long-3)", ipanel);

      tofSqvc3d[ipanel][islat] = new TH3F(name, title,
					  t0ptbin,t0ptmin,t0ptmax, 
					  qvcbin, qvcmin, qvcmax,
					  tof0bin,tof0min,tof0max);
      tofSqvc3d[ipanel][islat]->SetXTitle("[GeV/c]");
      tofSqvc3d[ipanel][islat]->SetYTitle("[ch]");
      tofSqvc3d[ipanel][islat]->SetZTitle("[ns]");
      tofSqvc3d[ipanel][islat]->SetMarkerColor(4);

      sprintf(name, "tofElossPt%d_%d", ipanel, islat);
      if(islat == 0) sprintf(title, "TOF:ELOSS:pt (panel%d long)", ipanel);
      if(islat == 1) sprintf(title, "TOF:ELOSS:pt (panel%d short)", ipanel);
      if(islat == 2) sprintf(title, "TOF:ELOSS:pt (panel%d short-0)", ipanel);
      if(islat == 3) sprintf(title, "TOF:ELOSS:pt (panel%d short-1)", ipanel);
      if(islat == 4) sprintf(title, "TOF:ELOSS:pt (panel%d long-0)", ipanel);
      if(islat == 5) sprintf(title, "TOF:ELOSS:pt (panel%d long-1)", ipanel);
      if(islat == 6) sprintf(title, "TOF:ELOSS:pt (panel%d long-2)", ipanel);
      if(islat == 7) sprintf(title, "TOF:ELOSS:pt (panel%d long-3)", ipanel);

      tofEloss3d[ipanel][islat] = new TH3F(name, title,
					   t0ptbin,t0ptmin,t0ptmax, 
					   elossbin, elossmin, elossmax,
					   tof0bin,tof0min,tof0max);
      tofEloss3d[ipanel][islat]->SetXTitle("[GeV/c]");
      tofEloss3d[ipanel][islat]->SetYTitle("[GeV]");
      tofEloss3d[ipanel][islat]->SetZTitle("[ns]");
      tofEloss3d[ipanel][islat]->SetMarkerColor(4);

      sprintf(name, "tofSlew%d_%d", ipanel, islat);
      if(islat == 0) sprintf(title, "Time:QVC (panel%d long)", ipanel);
      if(islat == 1) sprintf(title, "Time:QVC (panel%d short)", ipanel);
      if(islat == 2) sprintf(title, "Time:QVC (panel%d short 180)", ipanel);
      if(islat == 3) sprintf(title, "Time:QVC (panel%d short 90)", ipanel);
      if(islat == 4) sprintf(title, "Time:QVC (panel%d long-0)", ipanel);
      if(islat == 5) sprintf(title, "Time:QVC (panel%d long-1)", ipanel);
      if(islat == 6) sprintf(title, "Time:QVC (panel%d long-2)", ipanel);
      if(islat == 7) sprintf(title, "Time:QVC (panel%d long-3)", ipanel);

      tofSlew2d[ipanel][islat] = new TH2F(name, title,
					  qvcbin, qvcmin, qvcmax,
					  tof0bin,tof0min,tof0max);
      tofSlew2d[ipanel][islat]->SetXTitle("[ch]");
      tofSlew2d[ipanel][islat]->SetYTitle("[ns]");
      tofSlew2d[ipanel][islat]->SetMarkerColor(4);
      cout<<"  "<<name<<" - "<<title<<endl;

      sprintf(name, "tofSlewT%d_%d", ipanel, islat);
      if(islat == 0) sprintf(title, "TOF:QVC (panel%d long)", ipanel);
      if(islat == 1) sprintf(title, "TOF:QVC (panel%d short)", ipanel);
      if(islat == 2) sprintf(title, "TOF:QVC (panel%d short 180)", ipanel);
      if(islat == 3) sprintf(title, "TOF:QVC (panel%d short 90)", ipanel);
      if(islat == 4) sprintf(title, "TOF:QVC (panel%d long-0)", ipanel);
      if(islat == 5) sprintf(title, "TOF:QVC (panel%d long-1)", ipanel);
      if(islat == 6) sprintf(title, "TOF:QVC (panel%d long-2)", ipanel);
      if(islat == 7) sprintf(title, "TOF:QVC (panel%d long-3)", ipanel);
      tofSlewT2d[ipanel][islat] = new TH2F(name, title,
					   qvcbin, qvcmin, qvcmax,
					   tof0bin,tof0min,tof0max);
      tofSlewT2d[ipanel][islat]->SetXTitle("[ch]");
      tofSlewT2d[ipanel][islat]->SetYTitle("[ns]");
      tofSlewT2d[ipanel][islat]->SetMarkerColor(4);

      sprintf(name, "tofSlewS%d_%d", ipanel, islat);
      if(islat == 0) sprintf(title,"Time:sqrtQVC (panel%d long)", ipanel);
      if(islat == 1) sprintf(title,"Time:sqrtQVC (panel%d short)", ipanel);
      if(islat == 2) sprintf(title,"Time:sqrtQVC (panel%d short 180)",ipanel);
      if(islat == 3) sprintf(title,"Time:sqrtQVC (panel%d short 90)", ipanel);
      if(islat == 4) sprintf(title,"Time:sqrtQVC (panel%d long-0)", ipanel);
      if(islat == 5) sprintf(title,"Time:sqrtQVC (panel%d long-1)", ipanel);
      if(islat == 6) sprintf(title,"Time:sqrtQVC (panel%d long-2)", ipanel);
      if(islat == 7) sprintf(title,"Time:sqrtQVC (panel%d long-3)", ipanel);
      tofSlewS2d[ipanel][islat] = new TH2F(name, title,
					   qvcbin, qvcmin, qvcmax,
					   tof0bin,tof0min,tof0max);
      tofSlewS2d[ipanel][islat]->SetXTitle("[ch]");
      tofSlewS2d[ipanel][islat]->SetYTitle("[ns]");
      tofSlewS2d[ipanel][islat]->SetMarkerColor(4);

      sprintf(name, "tofSqvc%d_%d", ipanel, islat);
      if(islat == 0) sprintf(title,"TOF:sqrtQVC (panel%d long)", ipanel);
      if(islat == 1) sprintf(title,"TOF:sqrtQVC (panel%d short)", ipanel);
      if(islat == 2) sprintf(title,"TOF:sqrtQVC (panel%d short-0)",ipanel);
      if(islat == 3) sprintf(title,"TOF:sqrtQVC (panel%d short-1)",ipanel);
      if(islat == 4) sprintf(title,"TOF:sqrtQVC (panel%d long-0)", ipanel);
      if(islat == 5) sprintf(title,"TOF:sqrtQVC (panel%d long-1)", ipanel);
      if(islat == 6) sprintf(title,"TOF:sqrtQVC (panel%d long-2)", ipanel);
      if(islat == 7) sprintf(title,"TOF:sqrtQVC (panel%d long-3)", ipanel);
      tofSqvc2d[ipanel][islat] = new TH2F(name, title,
					  qvcbin, qvcmin, qvcmax,
					  tof0bin,tof0min,tof0max);
      tofSqvc2d[ipanel][islat]->SetXTitle("[ch]");
      tofSqvc2d[ipanel][islat]->SetYTitle("[ns]");
      tofSqvc2d[ipanel][islat]->SetMarkerColor(4);

      sprintf(name, "tofEloss%d_%d", ipanel, islat);
      if(islat == 0) sprintf(title, "TOF:ELOSS (panel%d long)", ipanel);
      if(islat == 1) sprintf(title, "TOF:ELOSS (panel%d short)", ipanel);
      if(islat == 2) sprintf(title, "TOF:ELOSS (panel%d short-0)", ipanel);
      if(islat == 3) sprintf(title, "TOF:ELOSS (panel%d short-1)", ipanel);
      if(islat == 4) sprintf(title, "TOF:ELOSS (panel%d long-0)", ipanel);
      if(islat == 5) sprintf(title, "TOF:ELOSS (panel%d long-1)", ipanel);
      if(islat == 6) sprintf(title, "TOF:ELOSS (panel%d long-2)", ipanel);
      if(islat == 7) sprintf(title, "TOF:ELOSS (panel%d long-3)", ipanel);
      tofEloss2d[ipanel][islat] = new TH2F(name, title,
					   elossbin,elossmin,elossmax,
					   tof0bin, tof0min, tof0max);
      tofEloss2d[ipanel][islat]->SetXTitle("[GeV]");
      tofEloss2d[ipanel][islat]->SetYTitle("[ns]");
      tofEloss2d[ipanel][islat]->SetMarkerColor(4);

      // [panel][slatset][position]
      for(Int_t ipos = 0; ipos<6;ipos++){
	sprintf(name, "tofSlewPos%d_%d_%d", ipanel, islat, ipos);
	if(islat == 0) sprintf(title, "Time:QVC (panel%d long)", ipanel);
	if(islat == 1) sprintf(title, "Time:QVC (panel%d short)", ipanel);
	if(islat == 2) sprintf(title, "Time:QVC (panel%d short 180)", ipanel);
	if(islat == 3) sprintf(title, "Time:QVC (panel%d short 90)", ipanel);
	if(islat == 4) sprintf(title, "Time:QVC (panel%d long-0)", ipanel);
	if(islat == 5) sprintf(title, "Time:QVC (panel%d long-1)", ipanel);
	if(islat == 6) sprintf(title, "Time:QVC (panel%d long-2)", ipanel);
	if(islat == 7) sprintf(title, "Time:QVC (panel%d long-3)", ipanel);

	tofSlewPos2d[ipanel][islat][ipos] = 
	  new TH2F(name,title,qvcbin, qvcmin, qvcmax,tof0bin,tof0min,tof0max);
	tofSlewPos2d[ipanel][islat][ipos]->SetXTitle("[ch]");
	tofSlewPos2d[ipanel][islat][ipos]->SetYTitle("[ns]");
	tofSlewPos2d[ipanel][islat][ipos]->SetMarkerColor(4);
      }
    }
  }

  tofSlewPt = new TH3F("tofSlewPt", "QVC:TOF:pt",
		       t0ptbin,t0ptmin,t0ptmax, 
		       qvcbin, qvcmin, qvcmax,
		       tof0bin,tof0min,tof0max);
  tofSlewPt->SetXTitle("[GeV/c]");
  tofSlewPt->SetYTitle("[ch]");
  tofSlewPt->SetZTitle("[ns]");
  tofSlewPt->SetMarkerColor(4);
  cout<<"### Create Histgram - end ###"<<endl;

//    nt_tof = new TNtuple("nt_tof","Timing Study with Track",
//  		       "slatid:panel:slat:set:mom:pt:charge:"
//  		       "t0bbc:flighttime:Toffset:"
//  		       "Yoffset:velocity:yhit:dpos:ypos:posl:posu:"
//  		       "timing0:timing1:toft:qvc0:qvc1:eloss");
//    nt_raw = new TNtuple("nt_raw","Timing Study with Track",
//  		       "slatid:panel:slat:set:mom:pt:charge:"
//  		       "t0bbc:flighttime:Toffset:"
//  		       "Yoffset:velocity:yhit:dpos:ypos:posl:posu:"
//  		       "timing:time:qvc:eloss:lu");
//    cout<<"### Create Ntuple - end ###"<<endl;
}

void Slewing::Write(char *ofile) {
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
  // Timing
  tofT2d->Write();
  tofQvcL2d->Write();
  tofQvcU2d->Write();
  tofQvcL3d->Write();
  tofQvcU3d->Write();

  tofSlew->Write();
  for(Int_t ipanel = 0; ipanel<TOF_NPANEL_ALL; ipanel++){
    for(Int_t islat = 0; islat<8; islat++){ 
      tofSlew3d[ipanel][islat]->Write();
      tofSlew2d[ipanel][islat]->Write();
      tofSlewT2d[ipanel][islat]->Write();
      tofSlewS2d[ipanel][islat]->Write();
      tofSqvc2d[ipanel][islat]->Write();
      tofEloss2d[ipanel][islat]->Write();
      for(Int_t ipos = 0; ipos<6; ipos++) 
	tofSlewPos2d[ipanel][islat][ipos]->Write();
    }
  }
  tofSlewPt->Write();

  //nt_tof->Write();
  //nt_raw->Write();

  hfile.Close();
}
// Function
Float_t Slewing::getMass2Tof(Float_t trueTof, Float_t pltof, Float_t mom){
  float mass2 = -1;
  if (pltof>0) {
    mass2   = mom*mom*(trueTof*trueTof*29.98*29.98/(pltof*pltof)-1);
  } 
  return (mass2);
}
void Slewing::setTofGlobalT(char *txtFile){
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
Float_t Slewing::getTofGlobalT(Int_t run){
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
void Slewing::setTofPmtGain(char *txtFile){
  ifstream inputfile; 
  inputfile.open(txtFile);
  Int_t slatid;
  Float_t mip_mean[10][8], tmp_mean[2], tmp;
  Float_t mip_count[10][8];
  Float_t mipL, mipU, mipLsigma, mipUsigma;

  if(!inputfile){ 
    cerr << "  Can not open "<< txtFile <<" file." << endl;
    cerr << endl;
  }else{ 
    while(!inputfile.eof()){
      inputfile >> slatid >> mipL >> mipLsigma >> mipU >> mipUsigma;
      mip[slatid][0] = mipL;
      mip[slatid][1] = mipU;
    }
  }
  // Initialize
  for(Int_t ipanel = 0; ipanel<10; ipanel++){
    for(Int_t islat = 0; islat<8; islat++){
      mip_mean[ipanel][islat] = 0.0;
      mip_count[ipanel][islat] = 0;
    }
  }
  for(Int_t id = 0; id < 960; id++){
    slatid = id;
    Int_t panel = slatid/96;
    Int_t slat  = slatid%96;
    bool  slatL  = slat>=16&&slat<80;
    bool  slatLa = slatL&&((slat%16)/4==0);
    bool  slatLb = slatL&&((slat%16)/4==1);
    bool  slatLc = slatL&&((slat%16)/4==2);
    bool  slatLd = slatL&&((slat%16)/4==3);
    bool  slatSd = slat>=0&&slat<16;
    bool  slatSu = slat>=80&&slat<96;
    bool  slatS  = slatSd||slatSu;

    if(mip[slatid][0]>0){
      if(slatL) {mip_mean[panel][0] += mip[slatid][0]; mip_count[panel][0]++;}
      if(slatS) {mip_mean[panel][1] += mip[slatid][0]; mip_count[panel][1]++;}
      if(slatSd){mip_mean[panel][3] += mip[slatid][0]; mip_count[panel][3]++;}
      if(slatSu){mip_mean[panel][2] += mip[slatid][0]; mip_count[panel][2]++;}
      if(slatLa){mip_mean[panel][4] += mip[slatid][0]; mip_count[panel][4]++;}
      if(slatLb){mip_mean[panel][5] += mip[slatid][0]; mip_count[panel][5]++;}
      if(slatLc){mip_mean[panel][6] += mip[slatid][0]; mip_count[panel][6]++;}
      if(slatLd){mip_mean[panel][7] += mip[slatid][0]; mip_count[panel][7]++;}
    }
    if(mip[slatid][1]>0){
      if(slatL) {mip_mean[panel][0] += mip[slatid][1]; mip_count[panel][0]++;}
      if(slatS) {mip_mean[panel][1] += mip[slatid][1]; mip_count[panel][1]++;}
      if(slatSd){mip_mean[panel][2] += mip[slatid][1]; mip_count[panel][2]++;}
      if(slatSu){mip_mean[panel][3] += mip[slatid][1]; mip_count[panel][3]++;}
      if(slatLa){mip_mean[panel][4] += mip[slatid][1]; mip_count[panel][4]++;}
      if(slatLb){mip_mean[panel][5] += mip[slatid][1]; mip_count[panel][5]++;}
      if(slatLc){mip_mean[panel][6] += mip[slatid][1]; mip_count[panel][6]++;}
      if(slatLd){mip_mean[panel][7] += mip[slatid][1]; mip_count[panel][7]++;}
    }
  }
  for(Int_t ipanel = 0; ipanel<10; ipanel++){
    for(Int_t islat = 0; islat<8; islat++){
      if(mip_count[ipanel][islat]!=0){
	tmp = mip_mean[ipanel][islat]/(Float_t)mip_count[ipanel][islat];
	mip_mean[ipanel][islat] = tmp;
      }
    }
  }
  for(Int_t id = 0; id < 960; id++){
    slatid = id;
    Int_t panel = slatid/96;
    Int_t slat  = slatid%96;
    bool  slatL  = slat>=16&&slat<80;
    bool  slatLa = slatL&&((slat%16)/4==0);
    bool  slatLb = slatL&&((slat%16)/4==1);
    bool  slatLc = slatL&&((slat%16)/4==2);
    bool  slatLd = slatL&&((slat%16)/4==3);
    bool  slatSd = slat>=0&&slat<16;
    bool  slatSu = slat>=80&&slat<96;
    bool  slatS  = slatSd||slatSu;

    if(slatSd){tmp_mean[0] = mip_mean[panel][3];}//90
    if(slatSd){tmp_mean[1] = mip_mean[panel][2];}//180
    if(slatSu){tmp_mean[0] = mip_mean[panel][2];}//180
    if(slatSu){tmp_mean[1] = mip_mean[panel][3];}//90
    if(slatLa){tmp_mean[0] = tmp_mean[1] = mip_mean[panel][4];}
    if(slatLb){tmp_mean[0] = tmp_mean[1] = mip_mean[panel][5];}
    if(slatLc){tmp_mean[0] = tmp_mean[1] = mip_mean[panel][6];}
    if(slatLd){tmp_mean[0] = tmp_mean[1] = mip_mean[panel][7];}

    pmtfactor[slatid][0] = pmtfactor[slatid][1] = 1.0; // Initialize;
    if(mip[slatid][0]>0) pmtfactor[slatid][0] = tmp_mean[0]/mip[slatid][0];
    if(mip[slatid][1]>0) pmtfactor[slatid][1] = tmp_mean[1]/mip[slatid][1];

    cout<<"  slatid = "<<slatid;
    cout<<"  L: "<<mip[slatid][0]<<" "<<tmp_mean[0]<<" "<<pmtfactor[slatid][0];
    cout<<"\tU: "<<mip[slatid][1]<<" "<<tmp_mean[1]<<" "<<pmtfactor[slatid][1];
    cout<<endl;
  }
}
Float_t Slewing::getTofPmtGain(Int_t slatid, Int_t lu){
  return mip[slatid][lu];
}
Float_t Slewing::getTofPmtFactor(Int_t slatid, Int_t lu){
  return pmtfactor[slatid][lu];
}
