//-----------------------------------------------------------------------------
//  Implementation of class TreeToNtpl
//
//  Author: Akio Kiyomichi
//-----------------------------------------------------------------------------
#define __TREETONTPL_CC__
#include "TreeToNtpl.hh"
#include <string>
#include <fstream.h> 
#include <iostream.h>
#include "gsl/gsl_math.h"
void TreeToNtpl::Loop(){

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

    //==================
    // Centrality Class
    //==================
    // Define Centrality class (bbc vs. zdc)
    int iclass;
    float npart, ncoll;
    int icent = centBin(ncoll,npart);
    if(icent==0)      iclass = 1;
    else if(icent<3)  iclass = 2;
    else if(icent<6)  iclass = 3;
    else if(icent<12) iclass = 4;
    else if(icent<16) iclass = 5;
    else              iclass = 6;


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

    Float_t tof_true, toft, t0, t0corr;
    //toft = tof;  // dTofReconstructed[].tof
    toft = (Timing[0] + Timing[1])/2 - tofslew - Toffset;

    t0corr = getBbcT0();
    t0 = t0bbc; // no t0 correction
    //t0 = t0corr; // BBC correction for Year-1 only

    tof_true = toft - t0;

    //Float_t m2 = mass2;
    Float_t m2 = getMass2Tof(tof_true, flightpath, ptot);

    Float_t nstofp, nstofz;
    Float_t dtof = surviveMatchTof(5.0,0,nstofp,nstofz); // rough-cut 5sigma
    Float_t dr = sqrt(pow(xtof[0]-tofpro[0],2)+
		      pow(xtof[1]-tofpro[1],2)+
		      pow(xtof[2]-tofpro[2],2));

    //bool cut_bbc = bbcChargeSum[0]+bbcChargeSum[1]>330;
    bool cut_bbc = bbcChargeSum[0]+bbcChargeSum[1]>150;
    //bool cut_dch = (dch_quality>20);
    bool cut_dch = dch_quality==23||dch_quality==31;
    bool cut_mom = ptot<2.0&&ptot>0.2;
    //bool cut_mom = ptot<2.0&&ptot>0.5;
    //bool cut_mom = ptot<2.0&&ptot>1.0;
    bool cut_tof = eloss>0.002&&eloss<0.004;
    bool pimass  = m2<0.1&&m2>-0.05;
    //bool pimass  = m2<0.15&&m2>-0.2;
    bool proj05 = sqrt(pow(xtof[1]-tofpro[1],2)+pow(xtof[2]-tofpro[2],2))<5.0;
    bool proj07 = sqrt(pow(xtof[1]-tofpro[1],2)+pow(xtof[2]-tofpro[2],2))<7.5;
    bool proj10 = sqrt(pow(xtof[1]-tofpro[1],2)+pow(xtof[2]-tofpro[2],2))<10.0;
    bool y10z05 = (fabs(xtof[1]-tofpro[1])<10.0&&fabs(xtof[2]-tofpro[2])<5.0);
    bool sector1 = (slatid<768);
    bool sector0 = (slatid>=768);

    Int_t npc1  = pc1Nhits;
    Int_t panel = slatid/96;
    Int_t slat  = slatid%96;

    bool timecut = cut_bbc&&cut_dch&&cut_mom&&cut_tof&&proj05&&pimass;
    bool diffcut = cut_dch&&cut_mom&&cut_tof&&proj07;

    tofSlat->Fill(slatid);
    if(timecut){
      trk_counter_time++;
      tofSlatT->Fill(slatid);
      tofT2d->Fill(slatid, tof_true-flighttime);
    }

    if(dtof!=-1){ // timecut-cut_mom
      if(dtof<2.5){
	tofSlatD->Fill(slatid);
	if(dch_alpha<0) pt_mass2->Fill(m2, pt); // positive
	if(dch_alpha>0) pt_mass2->Fill(m2,-pt); // negative
      }
      Float_t nttof[39];
      nttof[0]  = run;
      nttof[1]  = event;
      nttof[2]  = z0bbc;
      nttof[3]  = t0bbc;
      nttof[4]  = bbcChargeSum[0]+bbcChargeSum[1];
      nttof[5]  = zdcEnergy[0]+zdcEnergy[1];
      nttof[6]  = pc1Nhits;
      nttof[7]  = t0corr;
      nttof[8]  = icent;
      nttof[9]  = npart;
      nttof[10] = ncoll;
      nttof[11] = dch_quality;
      nttof[12] = ptot;
      nttof[13] = pt;
      nttof[14] = charge;
      nttof[15] = flightpath;                // path
      nttof[16] = ptot/sqrt(ptot*ptot+Mpi2); // pion beta
      nttof[17] = flighttime;                // pion flighttime
      nttof[18] = tofpro[0];
      nttof[19] = tofpro[1];
      nttof[20] = tofpro[2];
      nttof[21] = xtof[0];
      nttof[22] = xtof[1];
      nttof[23] = xtof[2];
      nttof[24] = dr;
      nttof[25] = dtof;
      nttof[26] = m2;
      nttof[27] = slatid;
      nttof[28] = toft;
      nttof[29] = eloss;
      nttof[30] = ypos;
      nttof[31] = Timing[0];
      nttof[32] = Timing[1];
      nttof[33] = qvc[0];
      nttof[34] = qvc[1];
      nttof[35] = tofslew;
      nttof[36] = Toffset;
      nttof[37] = Yoffset;
      nttof[38] = velocity;
      nt_tof->Fill(nttof);
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

void TreeToNtpl::Loop_a_file(char *filename){
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

void TreeToNtpl::make_ntpl(void) {
  // Ntuple
  nt_tof = new TNtuple("nt_tof","Timing Study with Track",
		       "run:evt:z0bbc:t0bbc:bbcq:zdce:npc1:t0corr:"
		       "cent:npart:ncoll:"
		       "quality:mom:pt:charge:path:beta:flighttime:"
		       "prox:proy:proz:tofx:tofy:tofz:dr:dtof:"
		       "mass2:slatid:tof:eloss:ypos:"
		       "timing0:timing1:qvc0:qvc1:slew:"
		       "Toffset:Yoffset:velocity");

  // Histgram
  Int_t slatbin = 1000; Float_t slatmin = -19.5, slatmax = 980.5;
  Int_t toftbin =  100; Float_t toftmin =  -5.0, toftmax =   5.0;

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

  //--------------------+
  //| checking histgram |
  //--------------------+
  Int_t mass2bin = 200; Float_t mass2min = -0.3, mass2max = 1.7;
  Int_t mombin   = 200; Float_t mommin   = -5.0, mommax   = 5.0; // 4.0=>5.0 
  Int_t ptbin    = 200; Float_t ptmin    = -5.0, ptmax    = 5.0;
  // pt vs mass2
  pt_mass2 = new TH2F("pt_mass2", "pt vs mass2", 
		      mass2bin, mass2min, mass2max, ptbin, ptmin, ptmax);

}

void TreeToNtpl::Write(char *ofile) {
  TFile hfile(ofile,"recreate");

  cout<<endl;
  cout<<"     Histgram file : " << ofile <<endl;
  cout<<" Track Count (all) : " << trk_counter_all <<endl;
  cout<<"      for Timing   : " << trk_counter_time<<endl;
  cout<<"      for Position : " << trk_counter_diff<<endl;
  cout<<endl;

  // Ntuple
  nt_tof->Write();

  // Histgram
  tofSlat->Write();
  tofSlatD->Write();
  tofSlatT->Write();
  tofT2d->Write();
  // checking histgram
  pt_mass2->Write();

  hfile.Close();
}
//
// Function
void TreeToNtpl::setTofGlobalT(char *txtFile){
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
Float_t TreeToNtpl::getTofGlobalT(Int_t run){
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
//-------------------------------------------------------------------------
// Function -- copy from $CVS/offline/analysis/microdst/classMicroTrack.C
//   following functions work only Year-1 Analysis
//-------------------------------------------------------------------------
Int_t TreeToNtpl::centBin(Float_t& ncoll,Float_t& npart)
  // Julia Velkovska e-mail julia.velkovska@sunysb.edu
{
  Float_t bbcqs = bbcChargeSum[0];
  Float_t bbcqn = bbcChargeSum[1];
  Float_t zdce0 = zdcEnergy[0];
  Float_t zdce1 = zdcEnergy[1];
  Int_t cent=-999;
  // Akiba's corrections for runs in which BBC and ZDC HV was changed
  //modify the value of bbc,zdc for special runs

  if(run==12010) {
    bbcqs*=1.17;
    bbcqn*=1.17;
  }
  if(run==12026) {
    bbcqs*=1.26;
    bbcqn*=1.26;
  }
  if(run==12156) {
    zdce0*=2.0;
    zdce1*=2.0;
  }
  if(run==12404) {
    bbcqs*=2.7;
    bbcqn*=2.7;
  }
  Float_t BBC=bbcqs+bbcqn;
  Float_t ZDC=zdce0+zdce1;
  Float_t BBC0=0.2*bbcMax;
  Float_t phi_bbczdc = atan2((BBC-BBC0)/bbcMax, ZDC/zdcMax);
 
  if (run<8884) cent=-1;
  if(zdce0==0)  cent=-2;
  if(cent!=-1&&cent!=-2)
    // no error condition - get the centrality bin 
    {
      for (Int_t j=0;j<18;j++){
	if(phi_bbczdc>phiCut[j+1]&&phi_bbczdc<phiCut[j])
	  cent=j;
      }
    }
  if (cent == 0) {
    npart = 346.7;
    ncoll = 945.2;  
  }else if (cent ==1) {
    npart = 292.8;
    ncoll = 748.6;  
  }else if (cent ==2) {
    npart = 248.7;
    ncoll = 598.2;  
  }else if (cent ==3) {
    npart = 210.2;
    ncoll = 476.6;  
  }else if (cent ==4) {
    npart = 176.7;
    ncoll = 377.1;  
  }else if (cent ==5) {
    npart = 148.1;
    ncoll = 295.2;  
  }else if (cent ==6) {
    npart = 122.7;
    ncoll = 228;  
  }else if (cent ==7) {
    npart = 100.3;
    ncoll = 172.9;  
  }else if (cent ==8) {
    npart = 81.4;
    ncoll = 129.9;  
  }else if (cent ==9) {
    npart = 64.1;
    ncoll = 93.6;  
  }else if (cent ==10) {
    npart = 49.4;
    ncoll = 65.8;  
  }else if (cent ==11) {
    npart = 38.1;
    ncoll = 46.4;  
  }else if (cent ==12) {
    npart = 28.7;
    ncoll = 32;  
  }else if (cent ==13) {
    npart = 21.2;
    ncoll = 21.6;  
  }else if (cent ==14) {
    npart = 15.3;
    ncoll = 14.3;  
  }else if (cent ==15) {
    npart = 10.9;
    ncoll = 9.4;  
  }else if (cent ==16) {
    npart = 7.7;
    ncoll = 6.1;  
  }else if (cent ==17) {
    npart = 5.5;
    ncoll = 4.1;  
  }else if (cent >=18) {
    npart = 3;
    ncoll = 2.;  
  }
return cent;
}

void TreeToNtpl::LoadCentAngles(Int_t run)
  // Julia Velkovska e-mail julia.velkovska@sunysb.edu
{
  if (run<0) {

    bbcMax=400;
    zdcMax=4500;
 
    phiCut[0]=1.57;
    phiCut[1]=1.208;    //0-5 %
    phiCut[2]=1.048;    //5-10%
    phiCut[3]=0.92;     //10-15%  
    phiCut[4]= 0.8112;  //15-20%  
    phiCut[5]= 0.712;   //20-25% 
    phiCut[6]= 0.616;   //25-30%
    phiCut[7]= 0.5136;  //30-35% 
    phiCut[8]= 0.4112;  //35-40% 
    phiCut[9]= 0.312;   //40-45%
    phiCut[10]= 0.2096;  //45-50%
    phiCut[11]= 0.1136; //50-55%
    phiCut[12]= 0.0272; //55-60%
    phiCut[13]= -0.0528;//60-65%
    phiCut[14]= -0.1264;//65-70% 
    phiCut[15]= -0.1968;//70-75%
    phiCut[16]= -0.2736;//75-80% 
    phiCut[17]= -0.408 ;//80-85%
    phiCut[18]= -1.57  ;// >85%
    phiCut[19]= -1.57  ;// >85%

    // efficiency parameters for track-by-track efficiency correction

    p0x1orx2=56.5589;    
    p1x1orx2=12.0604;    
    p2x1orx2=-0.837913;    
    p0x1andx2=77.1046;    
    p1x1andx2=6.22799;    
    p2x1andx2=-.421016;    

  }else {
    printf("not implemented yet \n");
  }
}

Float_t TreeToNtpl::surviveMatchTof(Float_t sigma,Int_t type,Float_t& nsP, Float_t& nsZ)
{
  // TOF
  Float_t APtof = 2.4568*(0.01);
  Float_t BPtof = 5.6122;
  Float_t CPtof = 5.0*(0.001); 
  Float_t DPtof = -1.092*(0.001);
  
  Float_t AZtof = 1.2621*(10) ; 
  Float_t BZtof = 4.5384;       
  Float_t CZtof = 1.19;         
  Float_t DZtof = 9.502*(0.1);  

 // parametrization function (for the sigma vs mom)  is  A*exp(-B*x) + C
  //Float_t momentum = mom;
  Float_t momentum = ptot;
  Float_t tofx = xtof[0], tofy = xtof[1], tofz = xtof[2];
  Float_t ptofx = tofpro[0], ptofy = tofpro[1], ptofz = tofpro[2];

  Float_t stofx = xtof[0], stofy = xtof[1], stofz = -xtof[2];
  Float_t sptofz = -tofpro[2];

  
  Float_t pmatchTof = APtof*exp(-BPtof*momentum) + CPtof;
  Float_t poffTof = DPtof;
  Float_t zmatchTof = AZtof*exp(-BZtof*momentum) + CZtof;
  Float_t zoffTof = DZtof;

  //------------------------------------------------------------
  Float_t deltaZ, deltaP;
  Float_t ph,php;
  
  if (type == 0) { // signal
    deltaZ = tofz - ptofz;
    ph  = atan2(tofy,tofx);
    php = atan2(ptofy,ptofx);
    deltaP = ph-php;
    if (deltaP > M_PI) {
      deltaP = 2* M_PI- deltaP; // 
    }else if (deltaP <- M_PI) {
      deltaP = deltaP + 2* M_PI;
    }
  }else {
    deltaZ = stofz - sptofz;
    ph  = atan2(stofy,stofx);
    php = atan2(ptofy,ptofx);
    deltaP = ph-php;
    if (deltaP > M_PI) {
      deltaP = 2* M_PI- deltaP; // 
    }else if (deltaP <- M_PI) {
      deltaP = deltaP + 2* M_PI;
    }
  }
  
  Float_t nsigmaZ = (deltaZ - zoffTof)/zmatchTof;
  Float_t nsigmaP = (deltaP - poffTof)/pmatchTof;
  Float_t biggestSigma ;
  nsP = nsigmaP;
  nsZ = nsigmaZ;
  nsigmaZ = fabs(nsigmaZ);
  nsigmaP = fabs(nsigmaP);

  //printf("Delta z : %f , Delta phi: %f \n",deltaZ, deltaP);
  //printf("N sigmaz %f , Nsimga phi %f \n",nsigmaZ, nsigmaP);
    
  Float_t nSigmaRadius = sqrt(nsigmaP*nsigmaP + nsigmaZ*nsigmaZ);
  if (nSigmaRadius > sigma) {
    return -1;
  }else {
    return nSigmaRadius;
  }
}

Float_t  TreeToNtpl::getBbcT0() { // for Year1 only
  Float_t bbct0 = t0bbc;
  Float_t npc1  = (Float_t)pc1Nhits;

  Float_t bbcMultDepend = -8.35895e-04*npc1 + 1.53058e-01; // AK [Mar.13,2001]

  Float_t trueT0;
  trueT0 = bbct0-bbcMultDepend;
  return(trueT0);
}
Float_t  TreeToNtpl::getToF(){
  Float_t t0  = getBbcT0();
  Float_t ToF = tof - t0; // AK [Mar.13,2001]

  return ( ToF );
}
Float_t TreeToNtpl::getMass2Tof(Float_t trueTof, Float_t pltof, Float_t mom){
  float mass2 = -1;
  if (pltof>0) {
    mass2   = mom*mom*(trueTof*trueTof*29.98*29.98/(pltof*pltof)-1);
  } 
  return (mass2);
}
