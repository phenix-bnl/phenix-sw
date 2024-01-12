//############### Fits Beam Center Postion
//## See Below for fixing fit parameters 
#include <TGaxis.h>
#include <TROOT.h>
#include <TH1.h>
#include <TF1.h>
#include <TStyle.h>
#include <string>
#include <iostream>
#include <sstream>

using namespace std;

char output[512];
vector<string> outfiles;


string MakeOutput(int runnumber, int segment, const char *file ="")
{
  sprintf(output,"%s/%s-%010d-%04d.png",file,file,runnumber,segment);
  char mkdircmd[100];
  sprintf(mkdircmd,"mkdir -p %s",file);
  gSystem->Exec(mkdircmd);
  outfiles.push_back(output);
 
  return output;
}


Double_t glfit(Double_t *x,Double_t *par)
{
//  Double_t fitval = par[0]/(sqrt(2.*TMath::Pi())*par[2])*TMath::Exp(-(x[0]-par[1])*(x[0]-par[1])/2./par[2]/par[2]) + par[3];
  Double_t fitval = par[0]/(sqrt(2.*TMath::Pi())*par[2])*TMath::Exp(-(x[0]-par[1])*(x[0]-par[1])/2./par[2]/par[2]) ;

  return fitval;
}


bool plot_beamcenter_xy(const char *filename,  const char *outdir="anaoutdir/",float outval[10],float outerr[10],int runinfo[2]){
  //comment the following line if you want to print the histos to the screen
  gROOT->SetBatch(kTRUE);

  gROOT->SetStyle("Plain");
  // gStyle->SetOptStat(0);
  //  gStyle->SetOptTitle(0);

  cout<<"input file name  : "<< filename<< endl;
  cout<<"outdir : "<< outdir << endl;

  //  char ifile[strlen(filename)+1]; // + 1 for the \0 which marks the end of string
 // strcpy(ifile,filename);
////  strtok(ifile, "run");
  char filepath[512];
  sprintf(filepath,"/phenix/hhj2/asano/run11_pro98_qa/run%%06d.root"); //_pro98-%%010d-%%04d.root"); 
  int run=-1;
  int fnumscan = sscanf(filename,filepath,&run);
  runinfo[0] = run;
  runinfo[1] = 0;
  cout << "runnumber:  " << run << endl;

  TH1F *h_primx_clone=NULL;
  TH1F *h_primy_clone=NULL;
  //  TH1F *h_primz_clone=NULL;
  
  TDirectory *gDir = gDirectory;
  TFile *f = TFile::Open(filename);
  //  cout << "open file" << endl;
  
  gDirectory = gDir;
  h_primx_clone = (TH1F*)f->Get("h_primary_x_minus_dbcenter")->Clone();
  h_primy_clone = (TH1F*)f->Get("h_primary_y_minus_dbcenter")->Clone();
  //  h_primz_clone = (TH1F*)f->Get("h_primary_z")->Clone();
  
  if(h_primx_clone == NULL) return false;

  f->Close();
  //define Canvas
  TCanvas *c1 = new TCanvas("c1","c1",800,600);
  c1->Divide(2,1);
  //rebin (if necessary);
  h_primx_clone->Rebin(2);//20um per bin
  int hidebin = h_primy_clone->GetMaximumBin();
  h_primy_clone->SetBinContent(hidebin,1);
  h_primy_clone->Rebin(2);//20um per bin
  //  h_priz_clone->Rebin(10);
  //draw
  
  c1->cd(1);
  h_primx_clone->Draw();
  //fitting
  int h_entry_x = h_primx_clone->GetEntries();
  if(h_entry_x < 1000){
    cout << "Entry is not enough !! " << endl;
    return false;
  }
  float hist_primx_max = h_primx_clone->GetMaximum(); 
  float hist_primx_mean = h_primx_clone->GetMean();
  float hist_primx_sigma = h_primx_clone->GetRMS();
  //TF1 *f1x = new TF1("f1x",glfit,-0.1,0.1,3);
  TF1 *f1x = new TF1("f1x","gaus",-0.5,0.5);
  f1x->SetLineColor(3);
  f1x->SetParameters(hist_primx_max,hist_primx_mean,hist_primx_sigma);
  f1x->SetParLimits(0,10,10000000000);//const.
  f1x->SetParLimits(1,-0.3,0.3);//mean 
  f1x->SetParLimits(2,-0.2,0.2);//sigma

  //################################
  //######  If you need to fix the fit for a specific 
  //######     run/segment insert that here:
  //#####  if you insert a run. be sure to make a default
  //#####  entry 
//    switch(runnumber)
 //     {
	//INSERT Entry here for Runnumber
  //    case 372524: 
//	switch(segnumber)
//	  {
//	    //INSERT Entry here for Segnumber
//	  case 0:
//	    cout<<"Fitting Run "<<runnumber<<"-"<<segnumber<<endl;
//	    f1x->SetParameters(20,hist_primx_mean,hist_primx_sigma);
//	    f1x->SetParLimits(0,10,10000000000);//const.
//	    f1x->SetParLimits(1,-0.3,0.3);//mean 
//	    f1x->SetParLimits(2,-0.2,0.2);//sigma
//	    break;//segnum==0

//	  default:
//	      break;//default
//	  }//switch(segnumber)
//	break;//case 372524: 

 //     default:
//	cout<<"Using default fit parameters"<<endl;
//	f1x->SetParameters(20,hist_primx_mean,hist_primx_sigma);
//	f1x->SetParLimits(0,10,100000000000000000);//const.
//	f1x->SetParLimits(1,-0.2,0.2);//mean 
//	f1x->SetParLimits(2,-0.2,0.2);//sigma
//	break;//default
//      }//switch(runnumber)

//  f1x->SetParNames("const","mean","sigma");

  h_primx_clone->Fit(f1x,"r","",hist_primx_mean - 3.*hist_primx_sigma, hist_primx_mean + 3.*hist_primx_sigma);
 //   h_primy_clone->Fit("gaus");
  outval[0] = f1x->GetParameter(0);
  outval[1] = f1x->GetParameter(1);
  outval[2] = f1x->GetParameter(2);
  outerr[0] = f1x->GetParError(0);
  outerr[1] = f1x->GetParError(1);
  outerr[2] = f1x->GetParError(2);

  c1->cd(2);
  h_primy_clone->Draw();
  int h_entry_y = h_primy_clone->GetEntries();
  if(h_entry_y < 1000){
    cout << "Entry is not enough !! " << endl;
    return false;
  }
  float hist_primy_max = h_primy_clone->GetMaximum(); 
  float hist_primy_mean = h_primy_clone->GetMean();
  float hist_primy_sigma = 0.02;
//  TF1 *f1y = new TF1("f1y",glfit,-0.1,0.1,3);
  TF1 *f1y = new TF1("f1y","gaus",-0.5,0.5);
  f1y->SetLineColor(3);
  f1y->SetParameters(hist_primy_max,hist_primy_mean,hist_primy_sigma);
  f1y->SetParLimits(0,10,hist_primy_max*10);//const.
  f1y->SetParLimits(1,-0.3,0.3);//mean 
  f1y->SetParLimits(2,-0.2,0.2);//sigma
//  f1y->SetParNames("const","mean","sigma");

  //################################
  //######  If you need to fix the fit for a specific 
  //######     run/segment insert that here:
  //#####  if you insert a run. be sure to make a default
  //#####  entry 
 //   switch(runnumber)
 //     {
	//INSERT Entry here for Runnumber
 //     case 372524: 
//	switch(segnumber)
//	  {
	    //INSERT Entry here for Segnumber
//	  case 0:
//	    cout<<"Fitting Run "<<runnumber<<"-"<<segnumber<<endl;
//	    f1y->SetParameters(hist_primy_max,hist_primy_mean,hist_primy_sigma);
//	    f1y->SetParLimits(0,10,hist_primy_max*1.5);//const.
//	    f1y->SetParLimits(1,-0.2,0.2);//mean 
//	    f1y->SetParLimits(2,-0.2,0.2);//sigma
//	    f1y->SetParNames("const","mean","sigma");
//	    break;//segnum==0

	//  default:
	//      break;//default
	//  }//switch(segnumber)
//	break;//case 372524: 

  //    default:
//	cout<<"Using default fit parameters"<<endl;
	f1y->SetParameters(hist_primy_max,hist_primy_mean,hist_primy_sigma);
	f1y->SetParLimits(0,10,hist_primy_max*15);//const.
	f1y->SetParLimits(1,-0.3,0.3);//mean 
	f1y->SetParLimits(2,-0.2,0.2);//sigma
//	f1y->SetParNames("const","mean","sigma");
//	break;//default
//      }//switch(runnumber)

  h_primy_clone->Fit(f1y,"rq","",hist_primy_mean - 3.*hist_primy_sigma, hist_primy_mean + 3.*hist_primy_sigma);
   
  outval[3] = f1y->GetParameter(0);  
  outval[4] = f1y->GetParameter(1);
  outval[5] = f1y->GetParameter(2);
  outerr[3] = f1y->GetParError(0);  
  outerr[4] = f1y->GetParError(1);
  outerr[5] = f1y->GetParError(2);

  outval[6] = h_entry_x;
  outval[7] = h_entry_y;
//  std::string outfit = MakeOutput(runnumber,segnumber,"Beamcenter");
  std::string outfit = MakeOutput(run,0,"Beamcenter");
  c1->SaveAs(outfit.c_str());


  //clean up
  delete c1;
  delete h_primx_clone;
  delete h_primy_clone;

  //  delete h_primz_clone;
  delete f1x;
  delete f1y;
  
  return true;
}
