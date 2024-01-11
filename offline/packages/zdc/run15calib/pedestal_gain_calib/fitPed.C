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
const float adcmax = 1000.5; // max adc
const int nbins = 128; 
const int NF=1;
//int main(int argc, char** argv){
void fitPed(const int runnumber, const int num=2){

  char inname[255];
  sprintf(inname,"adchisto/adc%d.root",runnumber);

  //2 8 9 10 11 13 16 17 20 22 25 26 27 29 
  char sname[128];
  sprintf(sname,"s%d",num);
  
  gStyle->SetOptStat(0);
  gStyle->SetOptFit();

  TFile* in = new TFile(inname);
  TCanvas* c=new TCanvas();
  
  TH1F* h=(TH1F*)in->Get(sname);
  TF1 fit("gaus","gaus",adcmin,adcmax);

  fit.SetLineColor(2);
    
	
  int nbins = h->GetNbinsX();
  double pedmean = h->GetMean();
  double pedrms = h->GetRMS();


    double firstnonzero = 0.;
    for (int ibin=1; ibin<=nbins; ibin++)
      {
 	if ( h->GetBinContent(ibin) > 0.0 )
	  {
	    firstnonzero = h->GetBinLowEdge(ibin);
	    break;
	  }
      }
 	
    int maxbin = h->GetMaximumBin();
    double peakvalue = h->GetBinContent(maxbin);
    double meanguess = h->GetBinLowEdge(maxbin+1);
  
    cout<<peakvalue<<endl;
    cout<<maxbin<<endl;
    cout<<meanguess<<endl;
  
    if( num!=2 && num!=8  && num!=9  && num!=10  && num!=11 && num!= 13  && num!=16  && num!=17  && num!=20  && num!=22  && num!=25  && num!=26  && num!=27  && num!=29 ){
      fit.SetRange(meanguess-40,meanguess+15);
      fit.SetParameters(peakvalue,meanguess,7);
    }
    else{
      // h->Rebin(5);
      fit.SetRange(meanguess-40,meanguess+15);
      fit.SetParameters(peakvalue,meanguess,7);
    }
 
    c->cd();
    gPad->SetLogy();
    
    h->Fit(&fit,"RQL");
    h->GetXaxis()->SetRangeUser(400,1200);

    float ped=fit.GetParameter(1);
    float sigma=fit.GetParameter(2);

    sprintf(sname,"SMD Pedestal Fit ADC Channel %d",num);
    h->SetTitle(sname);
    h->GetXaxis()->SetTitle("ADC (ch)");
    h->GetYaxis()->SetTitle("Count");
    h->GetXaxis()->SetTitleSize(0.046);
    h->GetYaxis()->SetTitleSize(0.046);
    
    h->Draw();

    if(num<10)
      sprintf(sname,"s0%d",num);
    else
      sprintf(sname,"s%d",num);
  
    char cname[128];
    // sprintf(cname,"resultsfitpng/pedestal%s.png",sname);
    sprintf(cname,"resultsfitpng/pedestal%s.root",sname);
    c->Print(cname);

    //   sprintf(cname,"resultsfitpng/pedestal%s.eps",sname);
    //    c->Print(cname);

    sprintf(cname,"parameters/pedestal_%s.txt",sname);
    FILE* fp;
    fp=fopen(cname,"w");
    
    fprintf(fp,"%.1f\t%.1f\t0\n",ped,sigma);
    fclose(fp);
   

}
  
