//macro to calculate the hitrate chip by chip
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <iomanip>
#include <list>

#include <TROOT.h>
#include <TDirectory.h>
#include <TSystem.h>
#include <TStyle.h>
#include <TFile.h>
#include <TCanvas.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TGraphErrors.h>
#include <TText.h>
#include <TPaveText.h>
#include <TGaxis.h>
#include <TRegexp.h>

using namespace std;

void plot_stabilityQA(std::string file="test.root")
{
  gROOT->SetBatch(true);
  gSystem->Load("libsvx.so");
  
  gStyle->SetOptStat(1);
  gStyle->SetTitleXOffset(0.9);
  gStyle->SetTitleYOffset(0.8);
  svxAddress *d_svxadr = new svxAddress();

  const int nbinevtseq=60000;//up to 60M events can be filled
  const int binwidth=1000;
  const int rebin=100;
  const int maxrange_evtseq=20000;
  const int size_canvasX=1600;
  const int size_canvasY=1200;

  const int nladder[4]={10,20,16,24};
  const int nsensor[4]={4,4,5,6};
  TFile *fqa = TFile::Open(file.c_str());
  TH2I *h2_bbc_evt_clone=NULL;
  h2_bbc_evt_clone= (TH2I*)fqa->Get("hzbbc_vs_evtseq_svxstabilityQA");
  h2_bbc_evt_clone->ProjectionX("pjx_bbcevent");
  //need to add sqrt(N) error before dividing 2 histograms. 
  //see .http://root.cern.ch/root/html532/TH1.html#TH1:Divide%2
  pjx_bbcevent->Sumw2();

  //create canvas ladder by ladder
  TCanvas *c[30];//for pixel
  for(int ilr=0;ilr<2;ilr++){
    for(int ild=0;ild<nladder[ilr];ild++){
      char canvas[256];
      sprintf(canvas,"B%dL%d rawhitrate",ilr,ild);
      c[ild]= new TCanvas(canvas,canvas,size_canvasX,size_canvasY);
      c[ild]->Divide(1,nsensor[ilr]);
      for(int isn=0;isn<nsensor[ilr];isn++){
        c[ild]->cd(isn+1);
        int imod=-1;
        imod = d_svxadr->getPixelModuleID(ilr,ild,isn);
        TH1F *h_hitrate[4];
        for(int icp=0;icp<4;icp++){
          int chipid= (isn%2)*4+icp;
          cout << "pixel module:"<< imod << "chip(0-7):"<< chipid <<"\tlayer:ladder:sensor:chip\t"<< ilr<<'\t' <<ild<<'\t'<<isn<<'\t' << icp << endl;
          char hname[256];
          sprintf(hname,"h1_pixelchiprawhit_mod%d_roc%d",imod,chipid);
          TH1F *h_tmp=NULL;
          h_tmp = (TH1F*)fqa->Get(hname);

          h_hitrate[icp] =(TH1F*) h_tmp->Clone();   
          h_hitrate[icp]->Sumw2();
          h_hitrate[icp]->Divide(h_tmp,pjx_bbcevent,1.0,1.0,"");
          h_hitrate[icp]->SetLineColor(icp+1);
          h_hitrate[icp]->SetXTitle("evt. sequence#*1000");
          h_hitrate[icp]->SetYTitle("rawhit rate");
          h_hitrate[icp]->Rebin(rebin);
          h_hitrate[icp]->GetXaxis()->SetRangeUser(0,maxrange_evtseq);
          if(ilr==0) h_hitrate[icp]->GetYaxis()->SetRangeUser(0,200);
          else       h_hitrate[icp]->GetYaxis()->SetRangeUser(0,100);
          if(icp==0){
            char histname[256];
            sprintf(histname,"pixel rawhitrate B%dLadder%dSensor%d",ilr,ild,isn);
            h_hitrate[icp]->SetTitle(histname);
            h_hitrate[icp]->Draw("E");
          }else{
            h_hitrate[icp]->Draw("Esame");
          }
        }
      }//isn
      char pname[256];
      sprintf(pname,"B%dL%drawhitrate.png",ilr,ild);
      c[ild]->SaveAs(pname,"PNG");
      sprintf(pname,"B%dL%drawhitrate.pdf",ilr,ild);
      c[ild]->SaveAs(pname,"PDF");
    }//ild
  }//pixel
  
  int h_index=0;
  TCanvas *c2[40];//for strip
  for(int ilr=2;ilr<4;ilr++){
    for(int ild=0;ild<nladder[ilr];ild++){
      char canvas[256];
      sprintf(canvas,"B%dL%d rawhit rate",ilr,ild);
      c2[ild]= new TCanvas(canvas,canvas,size_canvasX,size_canvasY);
      c2[ild]->Divide(1,nsensor[ilr]);
      for(int isn=0;isn<nsensor[ilr];isn++){
        c2[ild]->cd(isn+1);
        TH1F *h_hitrate[6];
        for(int iss=0;iss<2;iss++){
          for(int iro=0;iro<2;iro++){
            sprintf(hname,"h1_striphybridrawhit_B%dL%dS%d_ss%dro%d",ilr,ild,isn,iss,iro);
            TH1F *h_temp=NULL;
            h_temp = (TH1F*)fqa->Get(hname);
            h_hitrate[iss*2+iro] = (TH1F*) h_temp->Clone();
            h_hitrate[iss*2+iro]->Sumw2();
            h_hitrate[iss*2+iro]->Divide(h_temp,pjx_bbcevent,1.,1.,"");
            h_hitrate[iss*2+iro]->SetLineColor(iss*2+iro+1);
            h_hitrate[iss*2+iro]->Rebin(rebin);
            h_hitrate[iss*2+iro]->SetXTitle("evt. sequence#*1000");
            h_hitrate[iss*2+iro]->SetYTitle("rawhit rate");
            h_hitrate[iss*2+iro]->GetXaxis()->SetRangeUser(0,maxrange_evtseq);
            if(ilr==2)h_hitrate[iss*2+iro]->GetYaxis()->SetRangeUser(0,200);
            else      h_hitrate[iss*2+iro]->GetYaxis()->SetRangeUser(0,120);
            if(iss*2+iro==0){
              char histname[256];
              sprintf(histname,"strip rawhitrate B%dLadder%dSensor%d",ilr,ild,isn);
              h_hitrate[iss*2+iro]->Draw("E");
            }else{
              h_hitrate[iss*2+iro]->Draw("Esame");   
            }
            cout << "strip layer:ladder:sensor:section:readout\t" << ilr << '\t' << ild << '\t'<< isn << '\t' << iss << '\t' << iro << endl;
            h_index++;
          }//iro
        }//iss
      }//isn
      char pname[256];
      sprintf(pname,"B%dL%drawhitrate.png",ilr,ild);
      c2[ild]->SaveAs(pname,"PNG");
      sprintf(pname,"B%dL%drawhitrate.pdf",ilr,ild);
      c2[ild]->SaveAs(pname,"PDF");
    }//ild
  }//strip

  TCanvas *cbbc = new TCanvas("cbbcz","bbcz",size_canvasX,size_canvasY);
  h2_bbc_evt_clone->GetXaxis()->SetRangeUser(0,maxrange_evtseq);
  h2_bbc_evt_clone->ProjectionX("pjx_bbcevent2");
  cbbc->Divide(1,2);
  cbbc->cd(1);
  h2_bbc_evt_clone->Draw("colz");
  cbbc->cd(2);
  pjx_bbcevent2->SetTitle("projectionX");
  pjx_bbcevent2->Draw("");
  cbbc->SaveAs("bbc.png","PNG");
  cbbc->SaveAs("bbc.pdf","PDF");
  fqa->Close();
  cout << "number of strip histogram " << h_index << endl;


}

