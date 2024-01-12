#include "TFile.h"
#include "TF1.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "summaryQA.h"
#include <fstream>
#include <iostream>

using namespace std;

int QASummary::processMut()
{
  cout << "MuTr..." << endl;
  fstream textFile(outputName1, ios::in); if (textFile) { textFile.close(); textFile.open(outputName1,ios::app|ios::out); }
  fstream statusFile(outputName2, ios::in); if (statusFile) {statusFile.close();  statusFile.open(outputName2,ios::app|ios::out); }
  
  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- MUTR QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;
  
  // define a bunch of constants
  const int NUMARM      = 2;
  const int NUMSTATION  = 3;
  const int NUMOCTANT   = 8;
  const int NUMHALFOCT  = 2;
  const int NUMGAPST[NUMSTATION] = { 3, 3, 2};
  const int NUMPLANEST[NUMSTATION] = { 6, 6, 4};
  //onst int TOTPLANEST[NUMSTATION] = { 48, 48, 32};
  const int NUMSAMPLES  = 4;
  const int MAXTOTPLANE = 48;
  
  const int NUMLANDAU = 3; // number of Landau parameters
  const int NUMADDR  = 2; // used for keeping track of octant and plane info for hot/dead regions
  
  const float CLUSWIDMIN = 1.75;
  const float CLUSWIDMAX = 3.25;
  const int CLUSMINST[NUMSTATION] = { 0, 0, 0 };
  const int CLUSMAXST[NUMSTATION] = { 40, 40, 30 };
  const int NUMTRKMIN =    0;
  const int NUMTRKMAX =  30;  // For run4 AuAu limit set
  const float PTOTMIN =    1.;
  const float PTOTMAX =   10.; // For run4 AuAu limit set
  const int NUMSTATBITS = 15;
  // packet
  const int MAXNUMPACKET = 192;
  
  //#define NNOHITSTX TOTPLANESSTX/8
  const int NNOHITST[NUMSTATION] = { 6, 6, 4 }; 
  //#define NHOTSTX TOTPLANESSTX/4
  const int NHOTST[NUMSTATION] = { 12, 12, 8 }; 
  
  // datatypes: 0 = Run2 AuAu, 1 = Run2(&3) pp, 2 = Run3 dAu, 3 = Run4 AuAu,
  // dA values are set at about the mean between AA and pp for now..
  const int NDATATYPES = 4;
  const int CLUSQFITMIN[NDATATYPES] = { 50, 30, 40, 50};
  const int CLUSQFITMAX[NDATATYPES] = { 100, 100, 100, 100};
  
  
  // cluster peak charge in ADC value, so all limit times 10(gain value)
  const int CLUSQPEAKMIN[NDATATYPES] = { 300, 200, 250, 350};
  const int CLUSQPEAKMAX[NDATATYPES] = { 900, 900, 900, 900};
  
  const int PULSEHEIGHTMIN[NDATATYPES] = { 15, 2, 8, 15};

  const int HOTPACKST[NDATATYPES][NUMSTATION] = { {50, 40, 30},
                                                  {50, 40, 30},       
                                                  {50, 40, 30},
                                                  {20, 16, 12}};
  
  int QPEAKRANGEMINST[NDATATYPES][NUMSTATION][NUMOCTANT][NUMHALFOCT];			
  int QPEAKRANGEMAXST[NDATATYPES][NUMSTATION][NUMOCTANT][NUMHALFOCT];
  
  double HOTPLANEST[NDATATYPES][NUMSTATION];
  
  // note that ordering of an array starts with last index, i.e
  // int test[2][2] = { 0, 1, 2, 3};
  // cout << test[0][1] << endl; - gives 1
  // cout << test[1][0] << endl; - gives 2
  // Thus [NDATATYPES][NUMSTATION] means that the first 3 values
  // are for the first datatype (one for each station), and so on
  
  // current comment the QFIT limitation valure.
  //const int QFITRANGEMINST[NDATATYPES][NUMSTATION] = { 1, 1, 1,  // datatype 0 
  //  					                1, 1, 1,  // datatype 1
  //					                1, 1, 1 };// datatype 2 
  //const int QFITRANGEMAXST[NDATATYPES][NUMSTATION] = { 125, 125, 125,  // datatype 0 
  //					                130, 130, 120,  // datatype 1
  //                                                     125, 125, 125};  // datatype 1
  
  
  for (int datatpcons = 0; datatpcons<NDATATYPES; datatpcons++){
    for (int stacons = 0; stacons<NUMSTATION; stacons++){
      if (datatpcons == 0){
        if (stacons !=  2) HOTPLANEST[datatpcons][stacons] = 5;
        else  HOTPLANEST[datatpcons][stacons] = 3;                     //datatype 0;      
      } 
      else if (datatpcons == 1) HOTPLANEST[datatpcons][stacons] = 0.3; //datatype 1; 
      else if (datatpcons == 2) HOTPLANEST[datatpcons][stacons] = 1.0; //datatype 2; 
      else {
        if (stacons !=  2) HOTPLANEST[datatpcons][stacons] = 6;
        else  HOTPLANEST[datatpcons][stacons] = 5;                     //datatype 3;     
      }
      for (int octcons = 0; octcons<NUMOCTANT; octcons++){
        for (int hfoctcons = 0; hfoctcons<NUMHALFOCT; hfoctcons++){
          QPEAKRANGEMINST[datatpcons][stacons][octcons][hfoctcons] = 1;
          if (datatpcons == 0) {
            if (stacons == 0) QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 1100;         
            else if (stacons == 1) QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 1050;      
            else  QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 850;}                    //datatype 0;
          if (datatpcons == 1) {
            if (stacons == 0) QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 1100;
            else if (stacons == 1) QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 1100;
            else QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 900;}                     //datatype 1;
          if (datatpcons == 2) {
            if (stacons == 0) QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 1100;
            else if (stacons == 1) QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 1100;
            else QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 900;}                     //datatype 2;
          if (datatpcons == 3) {
            if (stacons == 0) QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 1100;
            else if (stacons == 1) QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 1050;
            else QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 900;}                     //datatype 3;
        }
      }
    }
  }
  
  // determine datatype from the runNumber
  Int_t datatype = 0; // assume we're dealing with gold
  if(runNumber < 35000)
  { // Au-Au Run2
    datatype = 0;
  }
  else if ( (runNumber > 35000) && (runNumber < 45000) )
  { // pp Run-2, with some margin around start and stop
    datatype = 1;
  }
  else if ( (runNumber > 60000) && (runNumber < 80500) )
  { // dAu Run-3, with some margin around start and stop
    datatype = 2;
  }
  else if ( (runNumber > 80500) && (runNumber < 92500) )
  { // pp Run-3, with some margin around start and stop
    // same values as for Run-2, for now
    datatype = 1;
  }
  else if ( (runNumber > 100000) && (runNumber < 126000) )
  { // Au-Au Run-4, with some margin around start and stop
    datatype = 3;
  }
  else if ( (runNumber > 126000) && (runNumber < 140000) )
  { // pp Run-4, with some margin around start and stop
    // same values as for Run-2, for now
    datatype = 1;
  }
  else if ( (runNumber > 358000) && (runNumber < 364000) )
  { // pp Run-12, with some margin around start and stop
    // same values as for Run-2, for now
    datatype = 1;
  }
  else
  {
    cerr << " summaryMutr.C - unknown run period; will use"
         << " default Run-2 AuAu limit values " << endl; 
    datatype = 0;
  }
  
  // histogram objects
  TH1F *MutNumCathClustersSt[NUMARM][NUMSTATION];
  //TH1F *MutCathClustQFitSt[NUMARM][NUMSTATION];
  TH1F *MutCathClustQPeakSt[NUMARM][NUMSTATION][NUMOCTANT][NUMHALFOCT];
  TProfile *MutHitsPerPlaneSt[NUMARM][NUMSTATION];
  
  TH1F *MutCathClustWidth[NUMARM][NUMSTATION];
  //TH1F *MutCathClustQFit[NUMARM];
  TH1F *MutCathClustQPeak[NUMARM];
  TH1F *MutNumTracks[NUMARM];
  TH1F *MutTrackMom[NUMARM];
  TProfile *MutPulseSamples[NUMARM][NUMSTATION][NUMOCTANT][3] = {{{{0}}}};

  TProfile *MutHitsPerPacketArm[NUMARM];
  TProfile *MutAmuErrorPerPacketArm[NUMARM];

  
  // check if Mutr objects are into the qa file
  if (!qafile->Get("MutNumCathClustersSt[0][0]"))
  {
    textFile << "Mutr histograms don't exist." 
             << "Status set to 2" << endl;
    statusFile << 2 << " " << 2 << " ";
    return -1;
  }
  
  // loop over our two arms 
  for (int arm = 0; arm < NUMARM; arm++)
  { 
    textFile << "MUTR - arm " << arm << endl;
    char id[128];
    int sta, oct, halfoct, totalp, addr;
    // arm and sta histograms
    for (sta = 0; sta<NUMSTATION; sta++)
    {
      sprintf(id,"MutNumCathClustersSt[%d][%d]", arm, sta);
      MutNumCathClustersSt[arm][sta] = (TH1F *)qafile->Get(id);
      //  sprintf(id,"MutCathClustQFitSt[%d][%d]", arm, sta);
      // MutCathClustQFitSt[arm][sta] = (TH1F *)qafile->Get(id);

      sprintf(id,"MutCathClustWidth[%d][%d]", arm, sta);
      MutCathClustWidth[arm][sta] = (TH1F *)qafile->Get(id);

      for (oct = 0; oct<NUMOCTANT; oct++){
        for (halfoct = 0; halfoct<NUMHALFOCT; halfoct++){
          sprintf(id,"MutCathClustQPeakSt[%d][%d][%d][%d]", arm, sta, oct, halfoct);
          MutCathClustQPeakSt[arm][sta][oct][halfoct] = (TH1F *)qafile->Get(id);
        }	  
      }
      sprintf(id,"MutHitsPerPlaneSt[%d][%d]", arm, sta);
      MutHitsPerPlaneSt[arm][sta] = (TProfile *)qafile->Get(id);
    }

    // arm histograms
    sprintf(id,"MutTrackMom[%d]", arm);
    MutTrackMom[arm] = (TH1F *)qafile->Get(id);
    // sprintf(id,"MutCathClustQFit[%d]", arm);
    //MutCathClustQFit[arm] = (TH1F *)qafile->Get(id);
    sprintf(id,"MutCathClustQPeak[%d]", arm);
    MutCathClustQPeak[arm] = (TH1F *)qafile->Get(id);
    sprintf(id,"MutNumTracks[%d]", arm);
    MutNumTracks[arm] = (TH1F *)qafile->Get(id);

    for (sta = 0; sta<NUMSTATION; sta++){
      for (oct = 0; oct<NUMOCTANT; oct++){
        for (int gap = 0; gap<NUMGAPST[sta]; gap++){
          sprintf(id,"MutPulseSamples[%d][%d][%d][%d]", arm, sta, oct, gap);
          MutPulseSamples[arm][sta][oct][gap] = (TProfile *)qafile->Get(id);
        }
      }
    }

    // TF1 *qfitfunc[NUMSTATION];
    TF1 *qpeakfunc[NUMSTATION][NUMOCTANT][NUMHALFOCT];
    for (sta = 0; sta<NUMSTATION; sta++)
    {
      // sprintf(id,"qfitfunc[%d]", sta);
      // qfitfunc[sta] = new TF1(id,"landau",QFITRANGEMINST[datatype][sta],
      //		      QFITRANGEMAXST[datatype][sta]);
      for (oct = 0; oct<NUMOCTANT; oct++){
        for (halfoct = 0; halfoct<NUMHALFOCT; halfoct++){
          sprintf(id,"qpeakfunc[%d][%d][%d]", sta, oct, halfoct);
          qpeakfunc[sta][oct][halfoct] = new TF1(id,"landau",QPEAKRANGEMINST[datatype][sta][oct][halfoct], QPEAKRANGEMAXST[datatype][sta][oct][halfoct]);
        }
      }
    }

    sprintf(id,"MutHitsPerPacketArm[%d]", arm);
    MutHitsPerPacketArm[arm] = (TProfile *)qafile->Get(id);
    sprintf(id,"MutAmuErrorPerPacketArm[%d]", arm);
    MutAmuErrorPerPacketArm[arm] = (TProfile *)qafile->Get(id);
      
    Int_t mutrstatus = 0;
    Float_t meanp, meancathclustST[NUMSTATION];
    Float_t meancluswid[NUMSTATION], meanqfit,meanqpeak,meannumtracks;
    Float_t notrack,nevt,percnotrack,percwithtrack;
    Float_t samp[NUMSTATION][NUMOCTANT][3][NUMSAMPLES] = {{{{0}}}};
    Double_t params[NUMLANDAU] = {0.0};
    Double_t returned_params[NUMLANDAU] = {0.0};
    // Float_t qfitMPVST[NUMSTATION] = {0};
    Float_t qpeakMPVST[NUMSTATION][NUMOCTANT][NUMHALFOCT]; 
    Int_t numhotST[NUMSTATION] = {0};
    Int_t ndeadST[NUMSTATION] = {0};
    Int_t howmanyplanes;
    Float_t hitavg = 0.0;
    Int_t hotST[NUMSTATION][MAXTOTPLANE][NUMADDR];
    Int_t nohitST[NUMSTATION][MAXTOTPLANE][NUMADDR];
    Int_t plane;
    Int_t i;
    Float_t HitPlaSt1=0.0;
    Float_t HitPlaSt2=0.0;
    Float_t HitPlaSt3=0.0;

    for (sta = 0; sta<NUMSTATION; sta++){
      for (oct = 0; oct<NUMOCTANT; oct++){
        for (halfoct = 0; halfoct<NUMHALFOCT; halfoct++){ 
          qpeakMPVST[sta][oct][halfoct] = 0.0;
        }
      }
      for (totalp = 0; totalp<MAXTOTPLANE; totalp++){
        for (addr = 0; addr<NUMADDR; addr++){ 
          hotST[sta][totalp][addr] = 0;
          nohitST[sta][totalp][addr] = 0;
        }
      }
    }
      
    for (sta = 0; sta<NUMSTATION; sta++)
    {
      //  qfitfunc[sta]->SetParameters(params);
      // MutCathClustQFitSt[arm][sta]->Fit(qfitfunc[sta],"RQN");
      //qfitfunc[sta]->GetParameters(returned_params);
      //qfitMPVST[sta] = returned_params[1];
      for (oct = 0; oct<NUMOCTANT; oct++){
        for (halfoct = 0; halfoct<NUMHALFOCT; halfoct++){ 
          qpeakfunc[sta][oct][halfoct]->SetParameters(params);
          MutCathClustQPeakSt[arm][sta][oct][halfoct]->Fit(qpeakfunc[sta][oct][halfoct],"RQN");
          qpeakfunc[sta][oct][halfoct]->GetParameters(returned_params);
          qpeakMPVST[sta][oct][halfoct] = returned_params[1];
        }

        for (int gap=0; gap<NUMGAPST[sta]; gap++) {
          samp[sta][oct][gap][0] = MutPulseSamples[arm][sta][oct][gap]->GetBinContent(1);
          samp[sta][oct][gap][1] = MutPulseSamples[arm][sta][oct][gap]->GetBinContent(6);
          samp[sta][oct][gap][2] = MutPulseSamples[arm][sta][oct][gap]->GetBinContent(7);
          samp[sta][oct][gap][3] = MutPulseSamples[arm][sta][oct][gap]->GetBinContent(8);
        }
      }

      howmanyplanes = MutHitsPerPlaneSt[arm][sta]->GetNbinsX();
      for (i=1; i<=howmanyplanes; i++)
      {
        hitavg = MutHitsPerPlaneSt[arm][sta]->GetBinContent(i);

        if(sta == 0)  HitPlaSt1+=hitavg;
        if(sta == 1)  HitPlaSt2+=hitavg;
        if(sta == 2)  HitPlaSt3+=hitavg;
	      
        if (hitavg > HOTPLANEST[datatype][sta])  
        {
          oct = (int)((i-1)/NUMPLANEST[sta]);
          plane = (i-1) - oct*NUMPLANEST[sta];
          hotST[sta][numhotST[sta]][0] = oct;
          hotST[sta][numhotST[sta]][1] = plane;
          numhotST[sta]++;
        }
        if(hitavg == 0)
        {
          oct = (int)((i-1)/NUMPLANEST[sta]);
          plane = (i-1) - oct*NUMPLANEST[sta];
          nohitST[sta][ndeadST[sta]][0] = oct;
          nohitST[sta][ndeadST[sta]][1] = plane;
          ndeadST[sta]++;
        }
      }
      meancathclustST[sta] = MutNumCathClustersSt[arm][sta]->GetMean();
    }   
    
    HitPlaSt1 = HitPlaSt1/48.0;
    HitPlaSt2 = HitPlaSt2/48.0;
    HitPlaSt3 = HitPlaSt3/32.0;
    //write out hits/event/plane for station 1,2,3 , south or north arm
    textFile << "MUTR hits/event/plane station1 =  " << HitPlaSt1 << endl;
    textFile << "MUTR hits/event/plane station2 =  " << HitPlaSt2 << endl;
    textFile << "MUTR hits/event/plane station3 =  " << HitPlaSt3 << endl;
      
    // end of loop over stations 
      
    meanp = MutTrackMom[arm]->GetMean();
    for(int sta=0; sta<NUMSTATION; sta++)
      meancluswid[sta] = MutCathClustWidth[arm][sta]->GetMean();
    // In Run4 Au-Au, we don't check QFIT information, set defallut 70 and do not change the original set up of status words. /quhai 
    meanqfit = 70;
    meanqpeak = MutCathClustQPeak[arm]->GetMean();
    meannumtracks = MutNumTracks[arm]->GetMean(); 
    notrack = MutNumTracks[arm]->GetBinContent(1);
    nevt = MutNumTracks[arm]->GetEntries();

    if(nevt!=0)
    {
      percnotrack = notrack/nevt;
    }
    else 
    {
      percnotrack = 1.0;
    }
    percwithtrack = 100*(1 - percnotrack); 

    //check paket related
    Int_t howmanypacks, namuerr,  numhotp, numdeadp;
    Float_t amuerrpack;
    Float_t  hitavgp = 0.0;
    Int_t wrnpack[MAXNUMPACKET]={0};
    Int_t hotp[MAXNUMPACKET] = {0};
    Int_t deadp[MAXNUMPACKET] = {0};
    Float_t HitPackSt1=0.0;
    Float_t HitPackSt2=0.0;
    Float_t HitPackSt3=0.0;
    namuerr = 0;
    numhotp = 0;
    numdeadp = 0;
     
    howmanypacks = MutHitsPerPacketArm[arm]->GetNbinsX();
    for (i=1; i<=howmanypacks; i++)
    {
      amuerrpack = MutAmuErrorPerPacketArm[arm]->GetBinContent(i);
      if (amuerrpack >= 0.1){
        namuerr++;
        wrnpack[i-1] = 1;
      }
      hitavgp = MutHitsPerPacketArm[arm]->GetBinContent(i);

      if(arm == 0){
        if(i<= 40)  HitPackSt1+=hitavgp;
        if(i<= 104 && i > 40)   HitPackSt2+=hitavgp;
        if(i<= 186 && i > 104)  HitPackSt3+=hitavgp;
      }
      else{
        if(i<= 40)  HitPackSt1+=hitavgp;
        if(i<= 112 && i > 40)   HitPackSt2+=hitavgp;
        if(i<= 192 && i > 112)  HitPackSt3+=hitavgp;
      }
	      
      if (arm == 0){
        if (i <= 40){ 
          if (hitavgp > HOTPACKST[datatype][0]){
            numhotp++;
            hotp[i-1] = 1;
          }
        }
        else if (i <= 104) {
          if (hitavgp > HOTPACKST[datatype][1]){
            numhotp++;
            hotp[i-1] = 1;
          }
        }
        else {
          if (hitavgp > HOTPACKST[datatype][2]){
            numhotp++;
            hotp[i-1] = 1;
          }
        }
      }
      else {
        if (i <= 40){ 
          if (hitavgp > HOTPACKST[datatype][0]){
            numhotp++;
            hotp[i-1] = 1;
          }
        }
        else if (i <= 112) {
          if (hitavgp > HOTPACKST[datatype][1]){
            numhotp++;
            hotp[i-1] = 1;
          }
        }
        else {
          if (hitavgp > HOTPACKST[datatype][2]){
            numhotp++;
            hotp[i-1] = 1;
          }
        }
      }
      if(hitavgp == 0)
	    {
	      numdeadp++;
	      deadp[i-1] = 1; 
	    }
	       
    }
      
    if(arm == 0){
      HitPackSt1 = HitPackSt1/40.0;
      HitPackSt2 = HitPackSt2/64.0;
      HitPackSt3 = HitPackSt3/64.0;
    }
    else {
      HitPackSt1 = HitPackSt1/40.0;
      HitPackSt2 = HitPackSt2/72.0;
      HitPackSt3 = HitPackSt3/80.0;
    }
      
    //write out hits/event/packet for station 1,2,3 , south or north arm
    textFile << "MUTR hits/event/packet station1 =  " << HitPackSt1 << endl;
    textFile << "MUTR hits/event/packet station2 =  " << HitPackSt2 << endl;
    textFile << "MUTR hits/event/packet station3 =  " << HitPackSt3 << endl;
      
    // ok, now we have all the info, start filling in the error code
    // first a check on the things above station level (i.e. for the whole arm)
    for (sta = 0; sta<NUMSTATION; sta++){
      for (oct = 0; oct<NUMOCTANT; oct++){
        for (int gap = 0; gap<NUMGAPST[sta]; gap++){
          if ( samp[sta][oct][gap][2] < PULSEHEIGHTMIN[datatype] ) 
            mutrstatus = mutrstatus|4;
        }
      }
    }

    // comment out bit set 8 for temporary
    // if ( (samp[sta][oct][gap][2]-samp[sta][oct][gap][1])<0 || (samp[sta][oct][gap][2]-samp[sta][oct][gap][3])<0 ) 
    //	mutrstatus = mutrstatus|8;
      
    if ( meanqfit < CLUSQFITMIN[datatype] || meanqfit > CLUSQFITMAX[datatype])  
      mutrstatus = mutrstatus|32;
    if ( meanqpeak < CLUSQPEAKMIN[datatype] || meanqpeak > CLUSQPEAKMAX[datatype] )  
      mutrstatus = mutrstatus|64;
      
    // now we move on to the station level
    Int_t status_bit = 128;
    for (sta = 0; sta<NUMSTATION; sta++)
    {
      if ( meancluswid[sta] < CLUSWIDMIN || meancluswid[sta] > CLUSWIDMAX )
        mutrstatus = mutrstatus|16;

      if ( meancathclustST[sta] <= CLUSMINST[sta] || 
           (meancathclustST[sta] >= CLUSMAXST[sta]) )  
      {
        mutrstatus = mutrstatus|status_bit;
      }
      status_bit *= 2;
    }
    for (sta = 0; sta<NUMSTATION; sta++)
    {
      if ( ndeadST[sta] >= NNOHITST[sta] || numhotST[sta]>=NHOTST[sta]) 
      {
        mutrstatus = mutrstatus|status_bit;
	    }
      status_bit *= 2;
    }
    // back to above station level checks
    if (percwithtrack <= NUMTRKMIN || percwithtrack >= NUMTRKMAX)  
      mutrstatus = mutrstatus|8192;
    if (meanp < PTOTMIN || meanp > PTOTMAX)  
      mutrstatus = mutrstatus|16384;


    //get to packet level check
    if (namuerr > 10) mutrstatus = mutrstatus|32768; // set limit for run4 AuAu that amu cell error < 10 packets for both arm


    //set limit for hotpackets < 25% and deadpackets < 12.5% (for run4 AuAu) 
    if (arm == 0){		
      if (numhotp>42 || numdeadp>21) mutrstatus = mutrstatus|65536;
    }       
    else {
      if (numhotp>48 || numdeadp>24) mutrstatus = mutrstatus|65536;
    }
      
    if(mutrstatus != 0) mutrstatus = mutrstatus|1; // overall status bit
      
    for (sta = 0; sta<NUMSTATION; sta++){
      for (oct = 0; oct<NUMOCTANT; oct++){
        for (int gap = 0; gap<NUMGAPST[sta]; gap++){
          textFile << "MUTR mean pulse sample 3 amplitude =  " << samp[sta][oct][gap][2] << endl;
          textFile << "MUTR pulse timing: samp3-samp2 = " << samp[sta][oct][gap][2]-samp[sta][oct][gap][1]
                   << "  samp3-samp4 = " << samp[sta][oct][gap][2]-samp[sta][oct][gap][3] << endl;
        }
      }
    }

    textFile << "MUTR mean cathode cluster width =  " << meancluswid[0]
             << ", " << meancluswid[0] << ", " << meancluswid[2] << endl;
    // textFile << "MUTR mean cathode cluster fitted charge =  " << meanqfit<<endl;
    textFile << "MUTR mean cathode cluster peak charge =  " << meanqpeak<<endl;
    textFile << "MUTR mean number fitted cathode clusters (station 1) = " 
             << meancathclustST[0] << endl;
    textFile << "MUTR mean number fitted cathode clusters (station 2) = " 
             << meancathclustST[1] << endl;
    textFile << "MUTR mean number fitted cathode clusters (station 3) = " 
             << meancathclustST[2] << endl;
    textFile << "MUTR Sta. 1: # of hot planes = " << numhotST[0] 
             << " # dead planes = " << ndeadST[0]  << endl;
    textFile << "MUTR Sta. 2: # of hot planes = " << numhotST[1] 
             << " # dead planes = " << ndeadST[1] << endl;
    textFile << "MUTR Sta. 3: # of hot planes = " << numhotST[2] 
             << " # dead planes = " << ndeadST[2]  << endl;
    textFile << "MUTR percentage of events with fitted tracks = " << percwithtrack << " %" <<endl;
    textFile << "MUTR mean total momentum of fitted tracks =  " << meanp << " GeV/c" << endl;
    textFile <<  "MUTR number packets with cell error  rate > 10% = " << namuerr << endl; 
    textFile <<  "MUTR number of hot packets = " << numhotp << endl; 

    textFile <<  "MUTR number of dead packets = " << numdeadp << endl; 
 
    //  textFile <<  "MUTR station 1 qfit landau MPV = " << qfitMPVST[0] << endl;
    // textFile <<  "MUTR station 2 qfit landau MPV = " << qfitMPVST[1] << endl;
    // textFile <<  "MUTR station 3 qfit landau MPV = " << qfitMPVST[2] << endl;
    for (sta = 0; sta<NUMSTATION; sta++){
      for (oct = 0; oct<NUMOCTANT; oct++){
        for (halfoct = 0; halfoct<NUMHALFOCT; halfoct++){  
          textFile <<  "MUTR arm " << arm << "  station  "<< sta << "  octant   " << oct << "  halfoctant  " << halfoct << " qpeak landau MPV = " << qpeakMPVST[sta][oct][halfoct] << endl;
        }
      }
    }
    textFile << endl;
    // Station by station reporting
    // hot
    for (sta = 0; sta<NUMSTATION; sta++)
    {
      if (numhotST[sta]>0)
	    {
	      textFile <<  "List of hot planes on station " << sta+1 << ":" << endl;
	      textFile << "station   octant   plane" << endl;
	      textFile << "-------   ------   -----" << endl;
	      for(i=0;i<numhotST[sta];i++)
        {
          textFile<<"   " << sta
                  <<"         " << hotST[sta][i][0]
                  <<"       " << hotST[sta][i][1]<<endl;
        }
	    }
      else
	    { 
	      textFile << "No hot planes on station " << sta+1 << ":" << endl;
	    }
    }
      
    // dead
    for (sta = 0; sta<NUMSTATION; sta++)
    {
      if (ndeadST[sta]>0)
	    {
	      textFile <<  "List of dead planes on station " << sta+1 << ":" << endl;
	      textFile << "station   octant   plane" << endl;
	      textFile << "-------   ------   -----" << endl;
	      for(i=0;i<ndeadST[sta];i++)
        {
          textFile<<"   " << sta
                  <<"         " << nohitST[sta][i][0]
                  <<"       " << nohitST[sta][i][1]<<endl;
        }
	    }
      else
	    { 
	      textFile << "No dead planes on station " << sta+1 << ":" << endl;
	    }
    }

    textFile << endl;
    // packet by packet reporting
    // amu cell error
    if (namuerr > 0){
      textFile <<  "List of wrong packets with error > 10% "<< endl;
      for (i = 0; i<howmanypacks; i++)
      {	
        if (wrnpack[i]>0)
	      {
          if (arm == 0)textFile << " packet " << 11001+i << endl;
          else textFile << " packet " << 11171+i << endl;
	      }
	 
      }
    }
    else
    { 
      textFile << "No wrong packets for amu cell difference check." << endl;
    }


    // hot and dead packets reporting

    if ( numhotp > 0){
      textFile <<  "List of hot packets "<< endl;
      for (i = 0; i<howmanypacks; i++)
      {
        if (hotp[i]>0)
	      {
          if (arm == 0)textFile << " packet " << 11001+i << endl;
          else textFile << " packet " << 11171+i << endl;
	      }
      }
    }
    else textFile << "No hot packets for this arm." << endl;
     
    //dead
    if ( numdeadp > 0){
      textFile <<  "List of dead packets "<< endl;
      for (i = 0; i<howmanypacks; i++)
      {
        if (deadp[i]>0)
	      {
          if (arm == 0)textFile << " packet " << 11001+i << endl;
          else textFile << " packet " << 11171+i << endl;
	      }
      }
    }
    else  textFile << "No dead packets for this arm." << endl;

    
    // output summary info
    textFile << "MUTR arm " << arm << " status = " << mutrstatus;
    statusFile << mutrstatus << " ";
      
    textFile << " = (Bitwise) ";
    for(Int_t bit=NUMSTATBITS-1; bit>=0; bit--){
      status_bit = mutrstatus & (0x1<<bit);
      textFile << (status_bit>>bit);
    }
    textFile << endl;
      
    char armname[10], name[40];
    if (arm==0) sprintf(armname,"South: ");
    if (arm==1) sprintf(armname,"North: ");
      
    for (sta = 0; sta<NUMSTATION; sta++){
      for (oct = 0; oct<NUMOCTANT; oct++){
        for (int gap = 0; gap<NUMGAPST[sta]; gap++){

          sprintf(name,"%s %s S%d O%d G%d",armname,"<Pulse Sample3 Ampl.>", sta+1, oct, gap);
          CommitToQADatabase("Mutr", name, samp[sta][oct][gap][2], 0.0);
          
          sprintf(name,"%s %s S%d O%d G%d",armname,"<Pulse Timing:samp3-samp2>", sta+1, oct, gap);
          CommitToQADatabase("Mutr", name, samp[sta][oct][gap][2]-samp[sta][oct][gap][1], 0.0);
          
          sprintf(name,"%s %s S%d O%d G%d",armname,"<Pulse Timing:samp3-samp4>", sta+1, oct, gap);
          CommitToQADatabase("Mutr", name, samp[sta][oct][gap][2]-samp[sta][oct][gap][3], 0.0);
        }
      }
    }

    sprintf(name,"%s %s",armname,"<Cath.Cluster Width> S1");
    CommitToQADatabase("Mutr", name, meancluswid[0], 0.0);

    sprintf(name,"%s %s",armname,"<Cath.Cluster Width> S2");
    CommitToQADatabase("Mutr", name, meancluswid[1], 0.0);

    sprintf(name,"%s %s",armname,"<Cath.Cluster Width> S3");
    CommitToQADatabase("Mutr", name, meancluswid[2], 0.0);

    sprintf(name,"%s %s",armname,"<Cath.Cluster Peak Charge>");
    CommitToQADatabase("Mutr", name, meanqpeak, 0.0);
      
    sprintf(name,"%s %s",armname,"<Fitted Cath.Cluster> S1");
    CommitToQADatabase("Mutr", name, meancathclustST[0], 0.0);

    sprintf(name,"%s %s",armname,"<Fitted Cath.Cluster> S2");
    CommitToQADatabase("Mutr", name, meancathclustST[1], 0.0);

    sprintf(name,"%s %s",armname,"<Fitted Cath.Cluster> S3");
    CommitToQADatabase("Mutr", name, meancathclustST[2], 0.0);

    sprintf(name,"%s %s",armname,"No. Hits/event/plane S1");
    CommitToQADatabase("Mutr", name, (float)HitPlaSt1, 0.0);
      
    sprintf(name,"%s %s",armname,"No. Hits/event/plane S2");
    CommitToQADatabase("Mutr", name, (float)HitPlaSt2, 0.0);
      
    sprintf(name,"%s %s",armname,"No. Hits/event/plane S3");
    CommitToQADatabase("Mutr", name, (float)HitPlaSt3, 0.0);
      
    sprintf(name,"%s %s",armname,"No. Hits/event/packet S1");
    CommitToQADatabase("Mutr", name, (float)HitPackSt1, 0.0);
      
    sprintf(name,"%s %s",armname,"No. Hits/event/packet S2");
    CommitToQADatabase("Mutr", name, (float)HitPackSt2, 0.0);
      
    sprintf(name,"%s %s",armname,"No. Hits/event/packet S3");
    CommitToQADatabase("Mutr", name, (float)HitPackSt3, 0.0);

    sprintf(name,"%s %s",armname,"Events w/ fitted tracks(%)");
    CommitToQADatabase("Mutr", name, percwithtrack, 0.0);

    sprintf(name,"%s %s",armname,"<Momentum> fitted tracks");
    CommitToQADatabase("Mutr", name, meanp, 0.0);
      
    sprintf(name,"%s %s",armname,"NPackets w/ error rate>10%");
    CommitToQADatabase("Mutr", name, (float)namuerr, 0.0);

    sprintf(name,"%s %s",armname,"No. Hot Packets");
    CommitToQADatabase("Mutr", name, (float)numhotp, 0.0);
      
    sprintf(name,"%s %s",armname,"No. Dead Packets");
    CommitToQADatabase("Mutr", name, (float)numdeadp, 0.0);

    sprintf(name,"%s %s",armname,"No. Hot Plane S1");
    CommitToQADatabase("Mutr", name, (float)numhotST[0], 0.0);
      
    sprintf(name,"%s %s",armname,"No. Hot Plane S2");
    CommitToQADatabase("Mutr", name, (float)numhotST[1], 0.0);
      
    sprintf(name,"%s %s",armname,"No. Hot Plane S3");
    CommitToQADatabase("Mutr", name, (float)numhotST[2], 0.0);
      
    sprintf(name,"%s %s",armname,"No. Dead Plane S1");
    CommitToQADatabase("Mutr", name, (float)ndeadST[0], 0.0);
      
    sprintf(name,"%s %s",armname,"No. Dead Plane S2");
    CommitToQADatabase("Mutr", name, (float)ndeadST[1], 0.0);
      
    sprintf(name,"%s %s",armname,"No. Dead Plane S3");
    CommitToQADatabase("Mutr", name, (float)ndeadST[2], 0.0);
      

  } // arm
  
  cout << "    ...done." << endl;  
  
  return 0;
}
