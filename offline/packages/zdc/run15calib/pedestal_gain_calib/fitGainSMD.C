#include <iostream>
#include <stdlib.h>
#include <fstream>
#include "TFile.h"
#include "TTree.h"
#include "TH1F.h"
#include "TChain.h"
#include "TStyle.h"
#include "TGraph.h"
#include "TCanvas.h"
#include "TF1.h"

using namespace std;


//argv[1]: fillnumber
const float adcmin = -0.5; // min adc
const float adcmax = 4000.5; // max adc
const int nbinmerge = 10; 
const int NF=1;
//int main(int argc, char** argv){

void fitGainSMD(const int runnumber, const int ch=1){

  char inname[255];
  sprintf(inname,"adchisto/adc%d.root",runnumber);
  TFile* in = new TFile(inname);

  double bestrange_max_offset[32]={0,};
  double bestrange_min_offset[32]={0,};
 
  
  bestrange_min_offset[3]=-100;
  bestrange_min_offset[19]=-100;

  bestrange_max_offset[1]=-500;
  bestrange_max_offset[2]=-500;
  bestrange_max_offset[9]=100;
  bestrange_max_offset[17]=-500;

  bestrange_max_offset[19]=00;
  bestrange_max_offset[31]=100;

  const double fitmax=2500+bestrange_max_offset[ch];

  gStyle->SetOptStat(0);
  gStyle->SetOptFit();

  char pedname[255];
  if(ch<10)
    sprintf(pedname,"parameters/pedestal_s0%d.txt",ch);
  else
    sprintf(pedname,"parameters/pedestal_s%d.txt",ch);

  ifstream ifs(pedname);
  char line[255];
  float ped,sigma;
  ifs.getline(line,255);
  sscanf(line,"%f %f", &ped, &sigma);
  
  cout<<"pedfile: ped="<<ped<<", sigma="<<sigma<<endl;
 
 
  char name[16];
  sprintf(name,"s%d",ch);
  TH1F* h = (TH1F*)in->Get(name); 
  h->Draw();
  gPad->SetLogy();
  char fitfunc[200];
  sprintf(fitfunc,"[0]*exp(-1.0*(x-%lf)*[1]) + [2]*exp(-1.0*(x-%lf)*[3])", ped, ped);
  
  //fit parameter setting**********************
  double p0, p1, p2, p3;
  double p0err, p1err, p2err, p3err;

  h->Rebin(nbinmerge);
  const int maxbin = h->GetMaximumBin();
  const double peak = h->GetBinContent(maxbin);
  const double fitmin=ped+200.0+bestrange_min_offset[ch];
  //const double fitmin=ped+250.0;
  
  double peakforgain=peak;
  double p0min=peakforgain/10.0;
  double p2min=peakforgain/2.0;
  double p0max=peakforgain/2.0;
  double p2max=peakforgain;
  
  //  double fitmin=1100;
  //double fitmax=ped[ch]+2000.0;
  
  double q0, q1, q2, q3;
    
  int fitLimit =1; //0: RQB, 1: RQ
  char option[16];
  q0=peakforgain* 0.03;
  q2=peakforgain* 0.13;
  q1=0.0015;
  q3=0.005;

  if(fitLimit==0)
    sprintf(option,"RQBL");
  else
    sprintf(option,"RQL");

  
    //h->Rebin(128);

    //fit parameter setting*****************************


  TF1 gfit("twoexp", fitfunc ,fitmin,fitmax);
  gfit.SetLineColor(2);
  gfit.SetRange(fitmin,fitmax);
  gfit.SetParameters(q0,q1,q2,q3);

  if(fitLimit==0){
    gfit.SetParLimits(0,p0min,p0max);
    gfit.SetParLimits(2,p2min,p2max);
      //  gfit.SetParLimits(1,0.0007,0.0026);
      //  gfit.SetParLimits(3,0.0025,0.01);
  }

  h->Fit(&gfit,option);

  q0=gfit.GetParameter(0);
  q1=gfit.GetParameter(1);
  q2=gfit.GetParameter(2);
  q3=gfit.GetParameter(3);

  if( p0> p2 )
    {
      double temp=p1;
      p1=p3;
      p3=temp;

      temp=p0;
      p0=p2;
      p2=temp;
    }
  
  gfit.SetParameters(q0,q1,q2,q3);
  h->Fit(&gfit,option);
  
  q0=gfit.GetParameter(0);
  q1=gfit.GetParameter(1);
  q2=gfit.GetParameter(2);
  q3=gfit.GetParameter(3);
  
  gfit.SetParameters(q0,q1,q2,q3);
  h->Fit(&gfit,option);
  
  p1=gfit.GetParameter(1);
  p1err=gfit.GetParError(1);
  p3=gfit.GetParameter(3);
  p3err=gfit.GetParError(3);
 
  p0=gfit.GetParameter(0);
  p0err=gfit.GetParError(0);
  p2=gfit.GetParameter(2);
  p2err=gfit.GetParError(2);
  h->GetXaxis()->SetTitle("ADC (ch)");
  h->GetYaxis()->SetTitle("Count");
  h->GetXaxis()->SetTitleSize(0.046);
  h->GetYaxis()->SetTitleSize(0.046);
  h->Draw();
  
  if(ch<10)
    sprintf(name,"s0%d",ch);
  else
    sprintf(name,"s%d",ch);

  char cname[128];
  sprintf(cname,"resultsfitpng/gain%s.png",name);
  gPad->Print(cname);
 
  sprintf(cname,"parameters/gain_%s.txt",name);
  FILE* fp;
  fp=fopen(cname,"w");
 




  cout<<p0<<" "<<p1<<" "<<p2<<" "<<p3<<endl;
  fprintf(fp,"%.1f\t%.8f\t%.1f\t%.8f\n",p0,p1,p2,p3);
  fclose(fp);

}
