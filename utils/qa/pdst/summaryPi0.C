#include "TFile.h"
#include "TCanvas.h"
#include "TF1.h"
#include "TH1.h"
#include "TH2.h"
#include "THmulf.h"
#include "TProfile.h"
#include "summaryQA.h"
#include "summaryPi0.h"

#include <fstream>
#include <iostream>
#include <cmath>

#define CommitPi0QAToQADatabase(subsystem, parname, parvalue, parerror)  { cout << subsystem << ": " << parname << ", Val= " << parvalue << ", Err= " << parerror << endl;   textFile << subsystem << ": " << parname << ", Val= " << parvalue << ", Err= " << parerror << endl;  CommitToQADatabase(subsystem, parname, parvalue, parerror); }

//#define CommitPi0QAToQADatabase(subsystem, parname, parvalue, parerror)  { cout << subsystem << ": " << parname << ", Val= " << parvalue << ", Err= " << parerror << endl;   textFile << subsystem << ": " << parname << ", Val= " << parvalue << ", Err= " << parerror << endl; }

using namespace std;
int QASummary::processPi0()
{
  cout << "Pi0..." << endl;
  fstream textFile(outputName1, ios::in); if (textFile) { textFile.close(); textFile.open(outputName1,ios::app|ios::out); }
  fstream statusFile(outputName2, ios::in); if (statusFile) {statusFile.close();  statusFile.open(outputName2,ios::app|ios::out); }

  textFile << " -- EMC QA Summary for AuAu --" << endl;
  textFile << " ----------------------------------------------------" << endl;
  textFile << " Run number: " << runNumber << endl;
  
//  TH1F* event_counter = (TH1F *) qafile->Get ("event_counter");
  TH1F* evts_cent = (TH1F *) qafile->Get ("evts_cent");
  THmulf* cluster = (THmulf *) qafile->Get ("cluster");
  THmulf* elecheck = (THmulf *) qafile->Get ("elecheck");
  THmulf* ecomp = (THmulf *) qafile->Get ("ecomp");
  THmulf* evts_mult = (THmulf *) qafile->Get ("evts_mult");
  THmulf* padisp_ratio = (THmulf *) qafile->Get ("padisp_ratio");
  THmulf* gghs = (THmulf *) qafile->Get ("gghs");
//  THmulf* tower = (THmulf *) qafile->Get ("tower");
//  THmulf* toftower = (THmulf *) qafile->Get ("toftower");
  THmulf* tofc = (THmulf *) qafile->Get ("tofc");
//  THmulf* bbczdc = (THmulf *) qafile->Get ("bbczdc");

  Pi0Util pi0util;

  //
  // add here analysis and printout results to textFile
  //

  //
  // First of all, setup basic parameters:
  Int_t nevent= (Int_t)evts_cent->Integral(3,9);
  textFile <<"Nevents subject to analyze: " << nevent << endl;
  Int_t nevent2= (Int_t)evts_cent->Integral(1,2);

  //
  // Counts up for central and peripheral events
  //
  CommitPi0QAToQADatabase("Pi0Emc","nCentralEvents",nevent2,0.0);
  CommitPi0QAToQADatabase("Pi0Emc","nPeripheralEvents",nevent,0.0);
  

  //
  // Pi0 and e/p ratio QA eval
  //
  TH1D *hrealsec[8];
  TH1D *hrealSc[3];
  TH1D *hrealGl[3];
  TH1D *hmixsec[8];
  TH1D *hmixSc[3];
  TH1D *hmixGl[3];
  TH1D *hsubsec[8];
  TH1D *hsubSc[3];
  TH1D *hsubGl[3];
  TH1D *EPhistsec[8];
  TH1D *EPhistSc[3];
  TH1D *EPhistGl[3];
  char Plotword[100];
  char Cutword[100];
  char title[100];
  char Committitle[100];

  //
  // Initialize Histogram
  //
  for(Int_t i=0;i<8;i++){
     sprintf(title,"hrealsec%d",i);
     hrealsec[i] = new TH1D(title,title,120,0,0.6);
     hrealsec[i]->Sumw2();
     sprintf(title,"hmixsec%d",i);
     hmixsec[i] = new TH1D(title,title,120,0,0.6);
     hmixsec[i]->Sumw2();
     sprintf(title,"hsubsec%d",i);
     hsubsec[i] = new TH1D(title,title,120,0,0.6);
     hsubsec[i]->Sumw2();

     sprintf(title,"EPhistsec%d",i);
     EPhistsec[i] = new TH1D(title,title,40,0,2);
     EPhistsec[i]->Sumw2();
  }
  for(Int_t i=0;i<3;i++){
     sprintf(title,"hrealSc%d",i);
     hrealSc[i] = new TH1D(title,title,120,0,0.6);
     hrealSc[i]->Sumw2();
     sprintf(title,"hrealGl%d",i);
     hrealGl[i] = new TH1D(title,title,120,0,0.6);
     hrealGl[i]->Sumw2();
     sprintf(title,"hmixSc%d",i);
     hmixSc[i] = new TH1D(title,title,120,0,0.6);
     hmixSc[i]->Sumw2();

     sprintf(title,"hmixGl%d",i);
     hmixGl[i] = new TH1D(title,title,120,0,0.6);
     hmixGl[i]->Sumw2();
     sprintf(title,"hsubSc%d",i);
     hsubSc[i] = new TH1D(title,title,120,0,0.6);
     hsubSc[i]->Sumw2();
     sprintf(title,"hsubGl%d",i);
     hsubGl[i] = new TH1D(title,title,120,0,0.6);
     hsubGl[i]->Sumw2();

     sprintf(title,"EPhistSc%d",i);
     EPhistSc[i] = new TH1D(title,title,40,0,2);
     EPhistSc[i]->Sumw2();
     sprintf(title,"EPhistGl%d",i);
     EPhistGl[i] = new TH1D(title,title,40,0,2);
     EPhistGl[i]->Sumw2();
  }


  //
  // Projection Histogram
  //
  for(int i=0;i<8;i++){
     sprintf(Plotword,"mass>>hrealsec%d",i);
     sprintf(Cutword,"chi2==1&&cent>=2&&pt>2&&sec==%d&&mix==0",i);
     gghs->Draw(Plotword,Cutword);
     sprintf(Plotword,"mass>>hmixsec%d",i);
     sprintf(Cutword,"chi2==1&&cent>=2&&pt>2&&sec==%d&&mix==1",i);
     gghs->Draw(Plotword,Cutword);

     sprintf(Plotword,"e_mom>>EPhistsec%d",i);
     sprintf(Cutword,"cent>=2&&mom>1.5&&sec==%d",i);
     elecheck->Draw(Plotword,Cutword);
  }

  gghs->Draw("mass>>hrealSc0","chi2==1&&cent>=2&&pt>1.5&&pt<2.0&&mix==0&&sec<6");
  gghs->Draw("mass>>hrealSc1","chi2==1&&cent>=2&&pt>2.0&&pt<2.5&&mix==0&&sec<6");
  gghs->Draw("mass>>hrealSc2","chi2==1&&cent>=2&&pt>2.5&&mix==0&&sec<6");

  gghs->Draw("mass>>hmixSc0", "chi2==1&&cent>=2&&pt>1.5&&pt<2.0&&mix==1&&sec<6");
  gghs->Draw("mass>>hmixSc1", "chi2==1&&cent>=2&&pt>2.0&&pt<2.5&&mix==1&&sec<6");
  gghs->Draw("mass>>hmixSc2", "chi2==1&&cent>=2&&pt>2.5&&mix==1&&sec<6");

  elecheck->Draw("e_mom>>EPhistSc0","cent>=2&&mom>0.8&&mom<1.0&&sec<6");
  elecheck->Draw("e_mom>>EPhistSc1","cent>=2&&mom>1.0&&mom<1.5&&sec<6");
  elecheck->Draw("e_mom>>EPhistSc2","cent>=2&&mom>1.5&&sec<6");

  gghs->Draw("mass>>hrealGl0","chi2==1&&cent>=2&&pt>1.5&&pt<2.0&&mix==0&&sec>=6");
  gghs->Draw("mass>>hrealGl1","chi2==1&&cent>=2&&pt>2.0&&pt<2.5&&mix==0&&sec>=6");
  gghs->Draw("mass>>hrealGl2","chi2==1&&cent>=2&&pt>2.5&&mix==0&&sec>=6");

  gghs->Draw("mass>>hmixGl0", "chi2==1&&cent>=2&&pt>1.5&&pt<2.0&&mix==1&&sec>=6");
  gghs->Draw("mass>>hmixGl1", "chi2==1&&cent>=2&&pt>2.0&&pt<2.5&&mix==1&&sec>=6");
  gghs->Draw("mass>>hmixGl2", "chi2==1&&cent>=2&&pt>2.5&&mix==1&&sec>=6");

  elecheck->Draw("e_mom>>EPhistGl0","cent>=2&&mom>0.8&&mom<1.0&&sec>=6");
  elecheck->Draw("e_mom>>EPhistGl1","cent>=2&&mom>1.0&&mom<1.5&&sec>=6");
  elecheck->Draw("e_mom>>EPhistGl2","cent>=2&&mom>1.5&&sec>=6");

  double fitpar[8];
  double fiterr[8];
  double countpi0[2];

  //
  // First Pi0 Check
  //
  for(int i=0;i<8;i++){
    for(int j=0;j<120;j++) hrealsec[i]->SetBinError(j+1,sqrt(hrealsec[i]->GetBinContent(j+1)));
    for(int j=0;j<120;j++) hmixsec[i]->SetBinError(j+1,sqrt(hmixsec[i]->GetBinContent(j+1)));
    Float_t norm_factor = ((Float_t)(hrealsec[i]->Integral(10,20)+hrealsec[i]->Integral(40,60)))/((Float_t)(hmixsec[i]->Integral(10,20)+hmixsec[i]->Integral(40,60)));
    hsubsec[i]->Add(hrealsec[i],hmixsec[i],1,-1*norm_factor);
    pi0util.Pi0FitAndCount(hsubsec[i],fitpar,fiterr,countpi0);

    sprintf(Committitle,"Pi0PeakSec%d",i);
    CommitPi0QAToQADatabase("Pi0Emc",Committitle,fitpar[1],fiterr[1]);
    sprintf(Committitle,"Pi0WidthSec%d",i);
    CommitPi0QAToQADatabase("Pi0Emc",Committitle,fitpar[2],fiterr[2]);
    sprintf(Committitle,"Pi0YieldPerEventSec%d",i);
    CommitPi0QAToQADatabase("Pi0Emc",Committitle,countpi0[0]/nevent,countpi0[1]/nevent);
  }

  for(int i=0;i<3;i++){
    for(int j=0;j<120;j++) hrealSc[i]->SetBinError(j+1,sqrt(hrealSc[i]->GetBinContent(j+1)));
    for(int j=0;j<120;j++) hmixSc[i]->SetBinError(j+1,sqrt(hmixSc[i]->GetBinContent(j+1)));
    Float_t norm_factor = ((Float_t)(hrealSc[i]->Integral(10,20)+hrealSc[i]->Integral(40,60)))/((Float_t)(hmixSc[i]->Integral(10,20)+hmixSc[i]->Integral(40,60)));
    hsubSc[i]->Add(hrealSc[i],hmixSc[i],1,-1*norm_factor);
    pi0util.Pi0FitAndCount(hsubSc[i],fitpar,fiterr,countpi0);

    sprintf(Committitle,"Pi0PeakSc (pT%d)",i);
    CommitPi0QAToQADatabase("Pi0Emc",Committitle,fitpar[1],fiterr[1]);
    sprintf(Committitle,"Pi0WidthSc (pT%d)",i);
    CommitPi0QAToQADatabase("Pi0Emc",Committitle,fitpar[2],fiterr[2]);
    sprintf(Committitle,"Pi0YieldPerEventSc (pT%d)",i);
    CommitPi0QAToQADatabase("Pi0Emc",Committitle,countpi0[0]/nevent,countpi0[1]/nevent);
  }

  for(int i=0;i<3;i++){
    for(int j=0;j<120;j++) hrealGl[i]->SetBinError(j+1,sqrt(hrealGl[i]->GetBinContent(j+1)));
    for(int j=0;j<120;j++) hmixGl[i]->SetBinError(j+1,sqrt(hmixGl[i]->GetBinContent(j+1)));
    Float_t norm_factor = ((Float_t)(hrealGl[i]->Integral(10,20)+hrealGl[i]->Integral(40,60)))/((Float_t)(hmixGl[i]->Integral(10,20)+hmixGl[i]->Integral(40,60)));
    hsubGl[i]->Add(hrealGl[i],hmixGl[i],1,-1*norm_factor);
    pi0util.Pi0FitAndCount(hsubGl[i],fitpar,fiterr,countpi0);

    sprintf(Committitle,"Pi0PeakGl (pT%d)",i);
    CommitPi0QAToQADatabase("Pi0Emc",Committitle,fitpar[1],fiterr[1]);
    sprintf(Committitle,"Pi0WidthGl (pT%d)",i);
    CommitPi0QAToQADatabase("Pi0Emc",Committitle,fitpar[2],fiterr[2]);
    sprintf(Committitle,"Pi0YieldPerEventGl (pT%d)",i);
    CommitPi0QAToQADatabase("Pi0Emc",Committitle,countpi0[0]/nevent,countpi0[1]/nevent);
  }


  //
  // Electron E/P ratio check
  //
  TF1 *f1 = new TF1("f1","gaus",-10,10);

  Stat_t stats[4];
  for(int i=0;i<8;i++){
    f1->SetParameter(0,EPhistsec[i]->Integral(15,30));
    f1->SetParameter(1,0.97);
    f1->SetParameter(2,0.1);
    Int_t Fstatus = EPhistsec[i]->Fit("f1","NQ","",0.7,1.2);
    Fstatus = EPhistsec[i]->Fit("f1","NMQ","",0.7,1.2);
    cout << "Fstatus: " << Fstatus;
    cout << ", Chi2: " << f1->GetChisquare();
    cout << ", NDF: " << f1->GetNDF();
    cout << ", Chi2/NDF: " << f1->GetChisquare()/f1->GetNDF() << endl;
    Float_t Chi2_NDF = f1->GetChisquare()/f1->GetNDF();
    if(Fstatus!=0 || Chi2_NDF<0.1 || Chi2_NDF>100 || f1->GetParameter(1)<0.75 || f1->GetParameter(1)>1.15){
      if(f1->GetNDF()<=2){
        sprintf(Committitle,"epPeakSec%d",i);
        CommitPi0QAToQADatabase("Pi0Emc",Committitle,0.0,0.0);
        sprintf(Committitle,"epWidthSec%d",i);
        CommitPi0QAToQADatabase("Pi0Emc",Committitle,0.0,0.0);
      }
      else{
        EPhistsec[i]->SetAxisRange(0.7,1.3);
        EPhistsec[i]->GetStats(stats);
        sprintf(Committitle,"epPeakSec%d",i);
        CommitPi0QAToQADatabase("Pi0Emc",Committitle,EPhistsec[i]->GetMean(),EPhistsec[i]->GetMean()/sqrt(stats[0]));
        sprintf(Committitle,"epWidthSec%d",i);
        CommitPi0QAToQADatabase("Pi0Emc",Committitle,EPhistsec[i]->GetRMS(),EPhistsec[i]->GetRMS()/sqrt(2*stats[0]));
      }
    }
    else{
      sprintf(Committitle,"epPeakSec%d",i);
      CommitPi0QAToQADatabase("Pi0Emc",Committitle,f1->GetParameter(1),f1->GetParError(1));
      sprintf(Committitle,"epWidthSec%d",i);
      CommitPi0QAToQADatabase("Pi0Emc",Committitle,f1->GetParameter(2),f1->GetParError(2));
    }
  }

  for(int i=0;i<3;i++){
    f1->SetParameter(0,EPhistSc[i]->Integral(15,30));
    f1->SetParameter(1,0.97);
    f1->SetParameter(2,0.1);
    Int_t Fstatus = EPhistSc[i]->Fit("f1","NQ","",0.7,1.2);
    Fstatus = EPhistSc[i]->Fit("f1","NMQ","",0.7,1.2);
    cout << "Fstatus: " << Fstatus;
    cout << ", Chi2: " << f1->GetChisquare();
    cout << ", NDF: " << f1->GetNDF();
    cout << ", Chi2/NDF: " << f1->GetChisquare()/f1->GetNDF() << endl;
    Float_t Chi2_NDF = f1->GetChisquare()/f1->GetNDF();
    if(Fstatus!=0 || Chi2_NDF<0.1 || Chi2_NDF>100 || f1->GetParameter(1)<0.75 || f1->GetParameter(1)>1.15){
      if(f1->GetNDF()<=2){
        sprintf(Committitle,"epPeakSc (pT%d)",i);
        CommitPi0QAToQADatabase("Pi0Emc",Committitle,0.0,0.0);
        sprintf(Committitle,"epWidthSc (pT%d)",i);
        CommitPi0QAToQADatabase("Pi0Emc",Committitle,0.0,0.0);
      }
      else{
        EPhistSc[i]->SetAxisRange(0.7,1.3);
        EPhistSc[i]->GetStats(stats);
        sprintf(Committitle,"epPeakSc (pT%d)",i);
        CommitPi0QAToQADatabase("Pi0Emc",Committitle,EPhistSc[i]->GetMean(),EPhistSc[i]->GetMean()/sqrt(stats[0]));
        sprintf(Committitle,"epWidthSc (pT%d)",i);
        CommitPi0QAToQADatabase("Pi0Emc",Committitle,EPhistSc[i]->GetRMS(),EPhistSc[i]->GetRMS()/sqrt(2*stats[0]));
      }
    }
    else{
      sprintf(Committitle,"epPeakSc (pT%d)",i);
      CommitPi0QAToQADatabase("Pi0Emc",Committitle,f1->GetParameter(1),f1->GetParError(1));
      sprintf(Committitle,"epWidthSc (pT%d)",i);
      CommitPi0QAToQADatabase("Pi0Emc",Committitle,f1->GetParameter(2),f1->GetParError(2));
    }
  }

  for(int i=0;i<3;i++){
    f1->SetParameter(0,EPhistGl[i]->Integral(15,30));
    f1->SetParameter(1,0.97);
    f1->SetParameter(2,0.1);
    Int_t Fstatus = EPhistGl[i]->Fit("f1","NQ","",0.7,1.2);
    Fstatus = EPhistGl[i]->Fit("f1","NMQ","",0.7,1.2);
    cout << "Fstatus: " << Fstatus;
    cout << ", Chi2: " << f1->GetChisquare();
    cout << ", NDF: " << f1->GetNDF();
    cout << ", Chi2/NDF: " << f1->GetChisquare()/f1->GetNDF() << endl;
    Float_t Chi2_NDF = f1->GetChisquare()/f1->GetNDF();
    if(Fstatus!=0 || Chi2_NDF<0.1 || Chi2_NDF>100 || f1->GetParameter(1)<0.75 || f1->GetParameter(1)>1.15){
      if(f1->GetNDF()<=2){
        sprintf(Committitle,"epPeakGl (pT%d)",i);
        CommitPi0QAToQADatabase("Pi0Emc",Committitle,0.0,0.0);
        sprintf(Committitle,"epWidthGl (pT%d)",i);
        CommitPi0QAToQADatabase("Pi0Emc",Committitle,0.0,0.0);
      }
      else{
        EPhistGl[i]->SetAxisRange(0.7,1.3);
        EPhistGl[i]->GetStats(stats);
        sprintf(Committitle,"epPeakGl (pT%d)",i);
        CommitPi0QAToQADatabase("Pi0Emc",Committitle,EPhistGl[i]->GetMean(),EPhistGl[i]->GetMean()/sqrt(stats[0]));
        sprintf(Committitle,"epWidthGl (pT%d)",i);
        CommitPi0QAToQADatabase("Pi0Emc",Committitle,EPhistGl[i]->GetRMS(),EPhistGl[i]->GetRMS()/sqrt(2*stats[0]));
      }
    }
    else{
      sprintf(Committitle,"epPeakGl (pT%d)",i);
      CommitPi0QAToQADatabase("Pi0Emc",Committitle,f1->GetParameter(1),f1->GetParError(1));
      sprintf(Committitle,"epWidthGl (pT%d)",i);
      CommitPi0QAToQADatabase("Pi0Emc",Committitle,f1->GetParameter(2),f1->GetParError(2));
    }
  }


   //
   // TOF Check part
   //
   char plotcondition[16][100];
   sprintf(plotcondition[0],"ecent>0.5&&sec<6");
   sprintf(plotcondition[1],"ecent<=0.5&&sec<6");
   sprintf(plotcondition[2],"ecent>0.5&&ecent<=1.0&&sec<6");
   sprintf(plotcondition[3],"ecent>1.0&&sec<6");
   sprintf(plotcondition[4],"ecent>0.5&&sec>=6");
   sprintf(plotcondition[5],"ecent<=0.5&&sec>=6");
   sprintf(plotcondition[6],"ecent>0.5&&ecent<=1.0&&sec>=6");
   sprintf(plotcondition[7],"ecent>1.0&&sec>=6");

   sprintf(plotcondition[8],"ecent>0.5&&sec==0");
   sprintf(plotcondition[9],"ecent>0.5&&sec==1");
   sprintf(plotcondition[10],"ecent>0.5&&sec==2");
   sprintf(plotcondition[11],"ecent>0.5&&sec==3");
   sprintf(plotcondition[12],"ecent>0.5&&sec==4");
   sprintf(plotcondition[13],"ecent>0.5&&sec==5");
   sprintf(plotcondition[14],"ecent>0.5&&sec==6");
   sprintf(plotcondition[15],"ecent>0.5&&sec==7");
   TH1D *thist[16];

   for(int j=0;j<16;j++){
     sprintf(title,"htof""%d",10+j);
     textFile << title << endl;
     thist[j] = new TH1D(title,title,100,-5.0,5.0);
     sprintf(Plotword,"tof>>""%s",title);
     thist[j]->Sumw2();
     tofc->Draw(Plotword,plotcondition[j]);
//     for(i=0;i<100;i++) thist[j]->SetBinError(i+1,sqrt(thist[j]->GetBinContent(i+1)));
     f1->SetParameter(0,thist[j]->GetEntries());
     f1->SetParameter(1,thist[j]->GetMean());
     f1->SetParameter(2,thist[j]->GetRMS());
     int Fstatus = thist[j]->Fit("f1","NQ","",-2.0,3.0);
     Fstatus = thist[j]->Fit("f1","NMQ","",-2.0,3.0);
    cout << "Fstatus: " << Fstatus;
    cout << ", Chi2: " << f1->GetChisquare();
    cout << ", NDF: " << f1->GetNDF();
    cout << ", Chi2/NDF: " << f1->GetChisquare()/f1->GetNDF() << endl;
     if(f1->GetParameter(1)<1.0){
       Fstatus = thist[j]->Fit("f1","NMQ","",-1.0,1.0);
     }
     if(Fstatus!=0 || f1->GetParameter(0)>100.0){
       sprintf(Committitle,"TofPeak (%s)",plotcondition[j]);
       CommitPi0QAToQADatabase("Pi0Emc",Committitle,f1->GetParameter(1),f1->GetParError(1));
       sprintf(Committitle,"TofWidth (%s)",plotcondition[j]);
       CommitPi0QAToQADatabase("Pi0Emc",Committitle,f1->GetParameter(2),f1->GetParError(2));
     }
     else{
       sprintf(Committitle,"TofPeak (%s)",plotcondition[j]);
       CommitPi0QAToQADatabase("Pi0Emc",Committitle,-0.8,0.0);
       sprintf(Committitle,"TofWidth (%s)",plotcondition[j]);
       CommitPi0QAToQADatabase("Pi0Emc",Committitle,-0.1,0.0);
     }
   }


  //
  // Cluster Mutiplicity as a function of centrality
  //
  TH1D *mulH = new TH1D("mulH","mulH",500,-0.5,500);
  TH1D *mulM = new TH1D("mulM","mulM",500,-0.5,500);
  TH1D *mulL = new TH1D("mulL","mulL",500,-0.5,500);
  mulH->Sumw2();
  mulM->Sumw2();
  mulL->Sumw2();
  evts_mult->Draw("mult>>mulH","cent<=2");
  evts_mult->Draw("mult>>mulM","cent>=3&&cent<=5");
  evts_mult->Draw("mult>>mulL","cent>=6&&cent>=8");
  CommitPi0QAToQADatabase("Pi0Emc", "ClusterMultH: <clusters/(cent0-cent2>", mulH->GetMean(), mulH->GetRMS()/sqrt(mulH->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "ClusterMultHWidth: <clusters/(cent0-cent2>", mulH->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "ClusterMultM: <clusters/(cent3-cent5>", mulM->GetMean(), mulM->GetRMS()/sqrt(mulM->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "ClusterMultMWidth: <clusters/(cent3-cent5>", mulM->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "ClusterMultL: <clusters/(cent6-cent8>", mulL->GetMean(), mulL->GetRMS()/sqrt(mulL->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "ClusterMultLWidth: <clusters/(cent6-cent8>", mulL->GetRMS(),0.0);


  //
  // Tower Energy Slope Check
  //
  TH1D *ClusEnergy0 = new TH1D("ClusEnergy0","ClusEnergy0",50,0,10);
  TH1D *ClusEnergy1 = new TH1D("ClusEnergy1","ClusEnergy1",50,0,10);
  TH1D *ClusEnergy2 = new TH1D("ClusEnergy2","ClusEnergy2",50,0,10);
  TH1D *ClusEnergy3 = new TH1D("ClusEnergy3","ClusEnergy3",50,0,10);
  TH1D *ClusEnergy4 = new TH1D("ClusEnergy4","ClusEnergy4",50,0,10);
  TH1D *ClusEnergy5 = new TH1D("ClusEnergy5","ClusEnergy5",50,0,10);
  TH1D *ClusEnergy6 = new TH1D("ClusEnergy6","ClusEnergy6",50,0,10);
  TH1D *ClusEnergy7 = new TH1D("ClusEnergy7","ClusEnergy7",50,0,10);
  TH1D *ClusEnergySc = new TH1D("ClusEnergySc","ClusEnergySc",50,0,10);
  TH1D *ClusEnergyGl = new TH1D("ClusEnergyGl","ClusEnergyGl",50,0,10);
  ClusEnergy0->Sumw2();
  ClusEnergy1->Sumw2();
  ClusEnergy2->Sumw2();
  ClusEnergy3->Sumw2();
  ClusEnergy4->Sumw2();
  ClusEnergy5->Sumw2();
  ClusEnergy6->Sumw2();
  ClusEnergy7->Sumw2();
  ClusEnergySc->Sumw2();
  ClusEnergyGl->Sumw2();

  cluster->Draw("e>>ClusEnergy0","sec==0");
  cluster->Draw("e>>ClusEnergy1","sec==1");
  cluster->Draw("e>>ClusEnergy2","sec==2");
  cluster->Draw("e>>ClusEnergy3","sec==3");
  cluster->Draw("e>>ClusEnergy4","sec==4");
  cluster->Draw("e>>ClusEnergy5","sec==5");
  cluster->Draw("e>>ClusEnergy6","sec==6");
  cluster->Draw("e>>ClusEnergy7","sec==7");
  cluster->Draw("e>>ClusEnergySc","sec<6");
  cluster->Draw("e>>ClusEnergyGl","sec>=6");

  TF1 *f2 = new TF1("f2","expo",-10,10);
  f2->SetParameter(0,ClusEnergy0->GetEntries());
  f2->SetParameter(1,-0.3);

  ClusEnergy0->Fit("f2","NQ","",1,4);
  CommitPi0QAToQADatabase("Pi0Emc", "ClusEnergySlope0(1-4GeV)", f2->GetParameter(1),f2->GetParError(1));
  ClusEnergy1->Fit("f2","NQ","",1,4);
  CommitPi0QAToQADatabase("Pi0Emc", "ClusEnergySlope1(1-4GeV)", f2->GetParameter(1),f2->GetParError(1));
  ClusEnergy2->Fit("f2","NQ","",1,4);
  CommitPi0QAToQADatabase("Pi0Emc", "ClusEnergySlope2(1-4GeV)", f2->GetParameter(1),f2->GetParError(1));
  ClusEnergy3->Fit("f2","NQ","",1,4);
  CommitPi0QAToQADatabase("Pi0Emc", "ClusEnergySlope3(1-4GeV)", f2->GetParameter(1),f2->GetParError(1));
  ClusEnergy4->Fit("f2","NQ","",1,4);
  CommitPi0QAToQADatabase("Pi0Emc", "ClusEnergySlope4(1-4GeV)", f2->GetParameter(1),f2->GetParError(1));
  ClusEnergy5->Fit("f2","NQ","",1,4);
  CommitPi0QAToQADatabase("Pi0Emc", "ClusEnergySlope5(1-4GeV)", f2->GetParameter(1),f2->GetParError(1));
  ClusEnergy6->Fit("f2","NQ","",1,4);
  CommitPi0QAToQADatabase("Pi0Emc", "ClusEnergySlope6(1-4GeV)", f2->GetParameter(1),f2->GetParError(1));
  ClusEnergy7->Fit("f2","NQ","",1,4);
  CommitPi0QAToQADatabase("Pi0Emc", "ClusEnergySlope7(1-4GeV)", f2->GetParameter(1),f2->GetParError(1));

  ClusEnergySc->Fit("f2","NQ","",0.2,1);
  CommitPi0QAToQADatabase("Pi0Emc", "ClusEnergySlopeSc(0.2-1.0GeV)", f2->GetParameter(1),f2->GetParError(1));
  ClusEnergySc->Fit("f2","NQ","",1,2);
  CommitPi0QAToQADatabase("Pi0Emc", "ClusEnergySlopeSc(1.0-2.0GeV)", f2->GetParameter(1),f2->GetParError(1));
  ClusEnergySc->Fit("f2","NQ","",2,4);
  CommitPi0QAToQADatabase("Pi0Emc", "ClusEnergySlopeSc(2.0-4.0GeV)", f2->GetParameter(1),f2->GetParError(1));

  ClusEnergyGl->Fit("f2","NQ","",0.2,1);
  CommitPi0QAToQADatabase("Pi0Emc", "ClusEnergySlopeGl(0.2-1.0GeV)", f2->GetParameter(1),f2->GetParError(1));
  ClusEnergyGl->Fit("f2","NQ","",1,2);
  CommitPi0QAToQADatabase("Pi0Emc", "ClusEnergySlopeGl(1.0-2.0GeV)", f2->GetParameter(1),f2->GetParError(1));
  ClusEnergyGl->Fit("f2","NQ","",2,4);
  CommitPi0QAToQADatabase("Pi0Emc", "ClusEnergySlopeGl(2.0-4.0GeV)", f2->GetParameter(1),f2->GetParError(1));

  //
  // Number of Tower Hits
  //
  TH1D *TowerHits0 = new TH1D("TowerHits0","TowerHits0",25,-0.5,24.5);
  TH1D *TowerHits1 = new TH1D("TowerHits1","TowerHits1",25,-0.5,24.5);
  TH1D *TowerHits2 = new TH1D("TowerHits2","TowerHits2",25,-0.5,24.5);
  TH1D *TowerHits3 = new TH1D("TowerHits3","TowerHits3",25,-0.5,24.5);
  TH1D *TowerHits4 = new TH1D("TowerHits4","TowerHits4",25,-0.5,24.5);
  TH1D *TowerHits5 = new TH1D("TowerHits5","TowerHits5",25,-0.5,24.5);
  TH1D *TowerHits6 = new TH1D("TowerHits6","TowerHits6",25,-0.5,24.5);
  TH1D *TowerHits7 = new TH1D("TowerHits7","TowerHits7",25,-0.5,24.5);
  TH1D *TowerHitsSc = new TH1D("TowerHitsSc","TowerHitsSc",25,-0.5,24.5);
  TH1D *TowerHitsGl = new TH1D("TowerHitsGl","TowerHitsGl",25,-0.5,24.5);
  cluster->Draw("twrhit>>TowerHits0","sec==0");
  cluster->Draw("twrhit>>TowerHits1","sec==1");
  cluster->Draw("twrhit>>TowerHits2","sec==2");
  cluster->Draw("twrhit>>TowerHits3","sec==3");
  cluster->Draw("twrhit>>TowerHits4","sec==4");
  cluster->Draw("twrhit>>TowerHits5","sec==5");
  cluster->Draw("twrhit>>TowerHits6","sec==6");
  cluster->Draw("twrhit>>TowerHits7","sec==7");
  cluster->Draw("twrhit>>TowerHitsSc","sec<6");
  cluster->Draw("twrhit>>TowerHitsGl","sec>=6");
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHits0", TowerHits0->GetMean(),TowerHits0->GetRMS()/sqrt(TowerHits0->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHits1", TowerHits1->GetMean(),TowerHits1->GetRMS()/sqrt(TowerHits1->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHits2", TowerHits2->GetMean(),TowerHits2->GetRMS()/sqrt(TowerHits2->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHits3", TowerHits3->GetMean(),TowerHits3->GetRMS()/sqrt(TowerHits3->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHits4", TowerHits4->GetMean(),TowerHits4->GetRMS()/sqrt(TowerHits4->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHits5", TowerHits5->GetMean(),TowerHits5->GetRMS()/sqrt(TowerHits5->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHits6", TowerHits6->GetMean(),TowerHits6->GetRMS()/sqrt(TowerHits6->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHits7", TowerHits7->GetMean(),TowerHits7->GetRMS()/sqrt(TowerHits7->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHitsSc", TowerHitsSc->GetMean(),TowerHitsSc->GetRMS()/sqrt(TowerHitsSc->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHitsGl", TowerHitsGl->GetMean(),TowerHitsGl->GetRMS()/sqrt(TowerHitsGl->GetEntries()));

  CommitPi0QAToQADatabase("Pi0Emc","nTowerHitsWidth0",TowerHits0->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHitsWidth1",TowerHits1->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHitsWidth2",TowerHits2->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHitsWidth3",TowerHits3->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHitsWidth4",TowerHits4->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHitsWidth5",TowerHits5->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHitsWidth6",TowerHits6->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHitsWidth7",TowerHits7->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHitsWidthSc",TowerHitsSc->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc","nTowerHitsWidthGl",TowerHitsGl->GetRMS(),0.0);


  //
  // padisp_ratio
  //
  TH1D *padispR0 = new TH1D("padispR0","padispR0",200,-10,10);
  TH1D *padispR1 = new TH1D("padispR1","padispR1",200,-10,10);
  TH1D *padispR2 = new TH1D("padispR2","padispR2",200,-10,10);
  TH1D *padispR3 = new TH1D("padispR3","padispR3",200,-10,10);
  TH1D *padispR4 = new TH1D("padispR4","padispR4",200,-10,10);
  TH1D *padispR5 = new TH1D("padispR5","padispR5",200,-10,10);
  TH1D *padispR6 = new TH1D("padispR6","padispR6",200,-10,10);
  TH1D *padispR7 = new TH1D("padispR7","padispR7",200,-10,10);
  TH1D *padispRSc = new TH1D("padispRSc","padispRSc",200,-10,10);
  TH1D *padispRGl = new TH1D("padispRGl","padispRGl",200,-10,10);

  padisp_ratio->Draw("ratio>>padispR0","sec==0");
  padisp_ratio->Draw("ratio>>padispR1","sec==1");
  padisp_ratio->Draw("ratio>>padispR2","sec==2");
  padisp_ratio->Draw("ratio>>padispR3","sec==3");
  padisp_ratio->Draw("ratio>>padispR4","sec==4");
  padisp_ratio->Draw("ratio>>padispR5","sec==5");
  padisp_ratio->Draw("ratio>>padispR6","sec==6");
  padisp_ratio->Draw("ratio>>padispR7","sec==7");
  padisp_ratio->Draw("ratio>>padispRSc","sec<6");
  padisp_ratio->Draw("ratio>>padispRGl","sec>=6");

  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratio0", padispR0->GetMean(),padispR0->GetRMS()/sqrt(padispR0->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratio1", padispR1->GetMean(),padispR1->GetRMS()/sqrt(padispR1->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratio2", padispR2->GetMean(),padispR2->GetRMS()/sqrt(padispR2->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratio3", padispR3->GetMean(),padispR3->GetRMS()/sqrt(padispR3->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratio4", padispR4->GetMean(),padispR4->GetRMS()/sqrt(padispR4->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratio5", padispR5->GetMean(),padispR5->GetRMS()/sqrt(padispR5->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratio6", padispR6->GetMean(),padispR6->GetRMS()/sqrt(padispR6->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratio7", padispR7->GetMean(),padispR7->GetRMS()/sqrt(padispR7->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratioSc", padispRSc->GetMean(),padispRSc->GetRMS()/sqrt(padispRSc->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratioGl", padispRGl->GetMean(),padispRGl->GetRMS()/sqrt(padispRGl->GetEntries()));

  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratioWidth0", padispR0->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratioWidth1", padispR1->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratioWidth2", padispR2->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratioWidth3", padispR3->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratioWidth4", padispR4->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratioWidth5", padispR5->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratioWidth6", padispR6->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratioWidth7", padispR7->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratioWidthSc", padispRSc->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "padisp_ratioWidthGl", padispRGl->GetRMS(),0.0);


  //
  // partesum/ecore
  //
  TH1D *peR0 = new TH1D("peR0","peR0",50,0,1);
  TH1D *peR1 = new TH1D("peR1","peR1",50,0,1);
  TH1D *peR2 = new TH1D("peR2","peR2",50,0,1);
  TH1D *peR3 = new TH1D("peR3","peR3",50,0,1);
  TH1D *peR4 = new TH1D("peR4","peR4",50,0,1);
  TH1D *peR5 = new TH1D("peR5","peR5",50,0,1);
  TH1D *peR6 = new TH1D("peR6","peR6",50,0,1);
  TH1D *peR7 = new TH1D("peR7","peR7",50,0,1);
  TH1D *peRSc = new TH1D("peRSc","peRSc",50,0,1);
  TH1D *peRGl = new TH1D("peRGl","peRGl",50,0,1);
  TH1D *peRSc1 = new TH1D("peRSc1","peRSc1",50,0,1);
  TH1D *peRGl1 = new TH1D("peRGl1","peRGl1",50,0,1);
  TH1D *peRSc2 = new TH1D("peRSc2","peRSc2",50,0,1);
  TH1D *peRGl2 = new TH1D("peRGl2","peRGl2",50,0,1);
  TH1D *peRSc3 = new TH1D("peRSc3","peRSc3",50,0,1);
  TH1D *peRGl3 = new TH1D("peRGl3","peRGl3",50,0,1);
  ecomp->Draw("pe_ecore>>peR0","sec==0");
  ecomp->Draw("pe_ecore>>peR1","sec==1");
  ecomp->Draw("pe_ecore>>peR2","sec==2");
  ecomp->Draw("pe_ecore>>peR3","sec==3");
  ecomp->Draw("pe_ecore>>peR4","sec==4");
  ecomp->Draw("pe_ecore>>peR5","sec==5");
  ecomp->Draw("pe_ecore>>peR6","sec==6");
  ecomp->Draw("pe_ecore>>peR7","sec==7");
  ecomp->Draw("pe_ecore>>peRSc","sec<6");
  ecomp->Draw("pe_ecore>>peRGl","sec>=6");
  ecomp->Draw("pe_ecore>>peRSc1","sec<6&&ntower==1");
  ecomp->Draw("pe_ecore>>peRGl1","sec>=6&&ntower==1");
  ecomp->Draw("pe_ecore>>peRSc2","sec<6&&ntower==2");
  ecomp->Draw("pe_ecore>>peRGl2","sec>=6&&ntower==2");
  ecomp->Draw("pe_ecore>>peRSc3","sec<6&&ntower==3");
  ecomp->Draw("pe_ecore>>peRGl3","sec>=6&&ntower==3");

  CommitPi0QAToQADatabase("Pi0Emc", "PEOverEC0", peR0->GetMean(),peR0->GetRMS()/sqrt(peR0->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverEC1", peR1->GetMean(),peR1->GetRMS()/sqrt(peR1->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverEC2", peR2->GetMean(),peR2->GetRMS()/sqrt(peR2->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverEC3", peR3->GetMean(),peR3->GetRMS()/sqrt(peR3->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverEC4", peR4->GetMean(),peR4->GetRMS()/sqrt(peR4->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverEC5", peR5->GetMean(),peR5->GetRMS()/sqrt(peR5->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverEC6", peR6->GetMean(),peR6->GetRMS()/sqrt(peR6->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverEC7", peR7->GetMean(),peR7->GetRMS()/sqrt(peR7->GetEntries()));

  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECSc", peRSc->GetMean(),peRSc->GetRMS()/sqrt(peRSc->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECGl", peRGl->GetMean(),peRGl->GetRMS()/sqrt(peRGl->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECScTw1", peRSc1->GetMean(),peRSc1->GetRMS()/sqrt(peRSc1->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECGlTw1", peRGl1->GetMean(),peRGl1->GetRMS()/sqrt(peRGl1->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECScTw2", peRSc2->GetMean(),peRSc2->GetRMS()/sqrt(peRSc2->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECGlTw2", peRGl2->GetMean(),peRGl2->GetRMS()/sqrt(peRGl2->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECScTw3", peRSc3->GetMean(),peRSc3->GetRMS()/sqrt(peRSc3->GetEntries()));
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECGlTw3", peRGl3->GetMean(),peRGl3->GetRMS()/sqrt(peRGl3->GetEntries()));

  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECWidth0", peR0->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECWidth1", peR1->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECWidth2", peR2->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECWidth3", peR3->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECWidth4", peR4->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECWidth5", peR5->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECWidth6", peR6->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECWidth7", peR7->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECScWidth", peRSc->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECGlWidth", peRGl->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECScTwWidth1", peRSc1->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECGlTwWidth1", peRGl1->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECScTwWidth2", peRSc2->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECGlTwWidth2", peRGl2->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECScTwWidth3", peRSc3->GetRMS(),0.0);
  CommitPi0QAToQADatabase("Pi0Emc", "PEOverECGlTwWidth3", peRGl3->GetRMS(),0.0);

  for(int i=0;i<8;i++){
    delete hrealsec[i];
    delete hmixsec[i];
    delete hsubsec[i];
    delete EPhistsec[i];
  }
  for(int i=0;i<3;i++){
    delete hrealSc[i];
    delete hrealGl[i];
    delete hmixSc[i];
    delete hmixGl[i];
    delete hsubSc[i];
    delete hsubGl[i];
    delete EPhistSc[i];
    delete EPhistGl[i];
  }

  cout << "done..." << endl;
  return 0;
}

Double_t MyFitBack(Double_t *x, Double_t *par)
{
  if(x[0]>0.10 && x[0]<0.20){
    TF1::RejectPoint();
    return 0;
  }
  return par[0]+par[1]*x[0];
}

Double_t Pi0Util::Pi0FitAndCount(TH1D *hist, double *parameter, double *errors, double *npi0)
{

  //to get fit parameters from subtracted histrogram
  //0 gaussian
  //1 gaussian
  //2 gaussian
  //3 polynominal3 0
  //4 polynominal3 1

// pi0 peak fit range
//double fitrange_low = 0.06;
//double fitrange_up = 0.3;
double fitrange_low = 0.1;
double fitrange_up = 0.2;

//char dummy[10];


  Float_t fitmean = 0.135;
  Float_t fitsigma = 0.013;
//  Float_t testwidth = 0.06;

  for(Int_t i=0;i<7;i++){
    parameter[i] = 0; errors[i] = 0;
  }

  TF1 *mygaus = new TF1("mygaus","gaus",fitrange_low,fitrange_up);
  mygaus->SetParameter(0,hist->GetEntries());
  mygaus->SetParameter(1,fitmean);
  mygaus->SetParameter(2,fitsigma);

  Int_t Fstatus= hist->Fit("mygaus","NRQ");
  Fstatus= hist->Fit("mygaus","NMRQ");

  Stat_t stats[4];
  mygaus->GetParameters(parameter);
  for(Int_t i=0;i<3;i++) errors[i] = mygaus->GetParError(i);
    cout << "Fstatus: " << Fstatus;
    cout << ", Chi2: " << mygaus->GetChisquare();
    cout << ", NDF: " << mygaus->GetNDF();
    cout << ", Chi2/NDF: " << mygaus->GetChisquare()/mygaus->GetNDF() << endl;
    Float_t Chi2_NDF = mygaus->GetChisquare()/mygaus->GetNDF();
    if(Chi2_NDF<0.1 || Chi2_NDF>100 || Fstatus!=0 || parameter[0]<0.0 ||
    parameter[1]<0.110 || parameter[1]>0.160 ||
    parameter[2]>0.030 || parameter[2]<0.0 ||
    parameter[1]<errors[1]){
    if(mygaus->GetNDF()<=2 || parameter[0]<=0){
      parameter[0]=0.0;
      parameter[1]=0.0;
      parameter[2]=0.0;
      errors[1]=0.0;
      errors[2]=0.0;
    }
    else{
      hist->SetAxisRange(0.1,0.2);
      parameter[0]=0.0;
      parameter[1]=hist->GetMean();
      parameter[2]=hist->GetRMS();
      hist->GetStats(stats);
      errors[1]=hist->GetMean()/sqrt(stats[0]);
      errors[2]=hist->GetRMS()/sqrt(2*stats[0]);
    }
  }


  // check from figure.
/*
  TF1 *fitpol = new TF1("fitpol","pol1",fitrange_low,fitrange_up);
  fitpol->SetParameter(0,fitback->GetParameter(0) );
  fitpol->SetParameter(1,fitback->GetParameter(1) );
  fitpol->SetLineColor(4);
*/

/*
  fitpol->Draw("same");
  c1->Update();
  cin >> dummy;
  delete c1;
*/

  //  cout << "Chisq/NDF: " << eval << endl;

  ////////////// counting Pi0 ////////////////

  Float_t tmp_npi0 = 0;
  Float_t tmp_npi0err = 0;

  npi0[0]=0;npi0[1]=0;

    Float_t countrange_low = parameter[1] - 2*parameter[2];
    Float_t countrange_hi = parameter[1] + 2*parameter[2];

  for(Int_t i=hist->FindBin(countrange_low);i<hist->FindBin(countrange_hi);i++)
    {
      Double_t cont = hist->GetBinContent(i);
      Float_t tmperr  = (Float_t)(hist->GetBinError(i));

      tmp_npi0 += cont;
      tmp_npi0err += tmperr * tmperr;
    }

  tmp_npi0err = sqrt(tmp_npi0err);

  npi0[0] = tmp_npi0;
  npi0[1] = tmp_npi0err;
  return 0.0;
}
