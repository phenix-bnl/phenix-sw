#include <TString.h>
#include <TH1.h>
#include <TFile.h>
#include <TGraphErrors.h>
#include <TCanvas.h>
#include <iostream>
#include <fstream>

using namespace std;

void plotped(const char *fname = "mpcped.root", int run_number = 0)
{
  gROOT->Reset();
  gSystem->Load("libnanoDST.so");
  gSystem->Load("libtrigger.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libmpc.so");

  MpcMap *mpcmap = MpcMap::instance();

  TString run = "0";

  if ( run_number == 0 )
    {
      run = fname;
      run.ReplaceAll("mpcped_","");
      run.ReplaceAll(".root","");
    }

  bool saveflag = true;
  //bool saveflag = false;
  bool printflag = true;
  const int MAXFEM = 4;
  //const int MAXFEM = 1;
  const int MAXCH = 144;	// max channels per fem

  TFile *infile = new TFile(fname,"READ");

  // now get the necessary ntuples
  TNtuple *tdc = (TNtuple*)infile->Get("tdc");
  TNtuple *lopre = (TNtuple*)infile->Get("lopre");
  TNtuple *lopost = (TNtuple*)infile->Get("lopost");
  TNtuple *hipre = (TNtuple*)infile->Get("hipre");
  TNtuple *hipost = (TNtuple*)infile->Get("hipost");
  TNtuple *lo = (TNtuple*)infile->Get("lo");
  TNtuple *hi = (TNtuple*)infile->Get("hi");

  TH1F *htdc_all[MAXFEM][MAXCH] = {{0}};
  TH1F *hlopost_all[MAXFEM][MAXCH] = {{0}};
  TH1F *hlopre_all[MAXFEM][MAXCH] = {{0}};
  TH1F *hlo_all[MAXFEM][MAXCH] = {{0}};
  TH1F *hhipost_all[MAXFEM][MAXCH] = {{0}};
  TH1F *hhipre_all[MAXFEM][MAXCH] = {{0}};
  TH1F *hhi_all[MAXFEM][MAXCH] = {{0}};

  // Generate 
  TH1F *hemptyfem[4] = {0};
  hemptyfem[0] = new TH1F("nfem0", "nfem0", 144, -0.5, 143.5);
  hemptyfem[1] = new TH1F("nfem1", "nfem1", 144, 143.5, 287.5);
  hemptyfem[2] = new TH1F("nfem2", "nfem2", 144, 287.5, 431.5);
  hemptyfem[3] = new TH1F("nfem3", "nfem3", 144, 431.5, 575.5);

  for (int ifem = 0; ifem < MAXFEM; ifem++)
    {
      for (int ich = 0; ich < 144; ich++)
	{
          int ifee576ch = 144*ifem + ich;
	  if ( mpcmap->getGridX(ifee576ch)<0 )
            {
              hemptyfem[ifem]->Fill(ifee576ch,4000.);
            }
	}
      hemptyfem[ifem]->SetFillColor(5);
      //hemptyfem[ifem]->SetFillStyle(4100);
    }

  Double_t fee567ch[MAXFEM][MAXCH] = {{0}};
  Double_t tdcmean[MAXFEM][MAXCH] = {{0}};
  Double_t tdcmeanerr[MAXFEM][MAXCH] = {{0}};
  Double_t tdcrms[MAXFEM][MAXCH] = {{0}};
  Double_t tdcrmserr[MAXFEM][MAXCH] = {{0}};
  Double_t lopostmean[MAXFEM][MAXCH] = {{0}};
  Double_t lopostmeanerr[MAXFEM][MAXCH] = {{0}};
  Double_t lopostrms[MAXFEM][MAXCH] = {{0}};
  Double_t lopostrmserr[MAXFEM][MAXCH] = {{0}};
  Double_t lopremean[MAXFEM][MAXCH] = {{0}};
  Double_t lopremeanerr[MAXFEM][MAXCH] = {{0}};
  Double_t loprerms[MAXFEM][MAXCH] = {{0}};
  Double_t loprermserr[MAXFEM][MAXCH] = {{0}};
  Double_t lomean[MAXFEM][MAXCH] = {{0}};
  Double_t lomeanerr[MAXFEM][MAXCH] = {{0}};
  Double_t lorms[MAXFEM][MAXCH] = {{0}};
  Double_t lormserr[MAXFEM][MAXCH] = {{0}};
  Double_t hipostmean[MAXFEM][MAXCH] = {{0}};
  Double_t hipostmeanerr[MAXFEM][MAXCH] = {{0}};
  Double_t hipostrms[MAXFEM][MAXCH] = {{0}};
  Double_t hipostrmserr[MAXFEM][MAXCH] = {{0}};
  Double_t hipremean[MAXFEM][MAXCH] = {{0}};
  Double_t hipremeanerr[MAXFEM][MAXCH] = {{0}};
  Double_t hiprerms[MAXFEM][MAXCH] = {{0}};
  Double_t hiprermserr[MAXFEM][MAXCH] = {{0}};
  Double_t himean[MAXFEM][MAXCH] = {{0}};
  Double_t himeanerr[MAXFEM][MAXCH] = {{0}};
  Double_t hirms[MAXFEM][MAXCH] = {{0}};
  Double_t hirmserr[MAXFEM][MAXCH] = {{0}};

  TString histfname;
  TFile *histfile[4] = {0};
  TString name;
  int nch = 0;
  for (int ifem=0; ifem<MAXFEM; ifem++)
    {
      histfname = fname;
      histfname.ReplaceAll(".root","_");
      histfname += ifem; histfname += ".root";
      histfile[ifem] = new TFile(histfname,"READ");
      if ( histfile[ifem]->IsZombie() )
        {
          cout << "Couldn't open file " << histfname << endl;
          continue;
        }

      for (int ich=0; ich<MAXCH; ich++)
        {
          int ifee576ch = ifem*MAXCH + ich;
	  name = "htdc_"; name += ifee576ch;
          htdc_all[ifem][ich] = (TH1F*)histfile[ifem]->Get(name);

          if ( htdc_all[ifem][ich]==0 ) break;

	  name = "hlopost_"; name += ifee576ch;
          hlopost_all[ifem][ich] = (TH1F*)histfile[ifem]->Get(name);
          name = "hlopre_"; name += ifee576ch;
          hlopre_all[ifem][ich] = (TH1F*)histfile[ifem]->Get(name);
	  name = "hlo_"; name += ifee576ch;
          hlo_all[ifem][ich] = (TH1F*)histfile[ifem]->Get(name);
	  name = "hhipost_"; name += ifee576ch;
          hhipost_all[ifem][ich] = (TH1F*)histfile[ifem]->Get(name);
	  name = "hhipre_"; name += ifee576ch;
          hhipre_all[ifem][ich] = (TH1F*)histfile[ifem]->Get(name);
          name = "hhi_"; name += ifee576ch;
          hhi_all[ifem][ich] = (TH1F*)histfile[ifem]->Get(name);

          // Process the histograms
          fee567ch[ifem][ich] = ifee576ch;

          tdcmean[ifem][ich] = htdc_all[ifem][ich]->GetMean();
          //tdcmeanerr[ifem][ich] = htdc_all[ifem][ich]->GetMeanError();
          tdcrms[ifem][ich] = htdc_all[ifem][ich]->GetRMS();
          //tdcrmserr[ifem][ich] = htdc_all[ifem][ich]->GetRMSError();

          lopostmean[ifem][ich] = hlopost_all[ifem][ich]->GetMean();
          lopostrms[ifem][ich] = hlopost_all[ifem][ich]->GetRMS();
//cout << ich << "\t" << lopostrms[ifem][ich] << "\t" << hlopost_all[ifem][ich]->GetName() << "\t" << hlopost_all[ifem][ich]->GetRMS() << endl;
          lopremean[ifem][ich] = hlopre_all[ifem][ich]->GetMean();
          loprerms[ifem][ich] = hlopre_all[ifem][ich]->GetRMS();
          lomean[ifem][ich] = hlo_all[ifem][ich]->GetMean();
          lorms[ifem][ich] = hlo_all[ifem][ich]->GetRMS();

          hipostmean[ifem][ich] = hhipost_all[ifem][ich]->GetMean();
          hipostrms[ifem][ich] = hhipost_all[ifem][ich]->GetRMS();
          hipremean[ifem][ich] = hhipre_all[ifem][ich]->GetMean();
          hiprerms[ifem][ich] = hhipre_all[ifem][ich]->GetRMS();
          himean[ifem][ich] = hhi_all[ifem][ich]->GetMean();
          hirms[ifem][ich] = hhi_all[ifem][ich]->GetRMS();

          ++nch;
        }
    }

  int nfem = nch/MAXCH;
//cout << nfem << "\t" << nch << "\t" << MAXCH << endl;

  // get the mean and rms with amu dependence

  TFile *savefile = 0;
  if ( saveflag )
    {
      savefile = new TFile("plotped2.root","RECREATE");
    }

  TH2F *lo_mean_vs_ch = new TH2F("lo_mean_vs_ch","Low Post-Pre Mean vs Ch",nch,-0.5,nch-0.5,2000,-100,100);
  TH2F *lo_rms_vs_ch = new TH2F("lo_rms_vs_ch","Low Post-Pre RMS vs Ch",nch,-0.5,nch-0.5,1000,0,100);
  TH2F *lo_mean_vs_amu = new TH2F("lo_mean_vs_amu","Low Post-Pre Mean vs AMU",64,-0.5,63.5,200,-100,100);
  TH2F *lo_rms_vs_amu = new TH2F("lo_rms_vs_amu","Low Post-Pre RMS vs AMU",64,-0.5,63.5,100,0,100);
  TH2F *lopost_mean_vs_ch = new TH2F("lopost_mean_vs_ch","Low Post Mean vs Ch",nch,-0.5,nch-0.5,8000,0,800);
  TH2F *lopost_rms_vs_ch = new TH2F("lopost_rms_vs_ch","Low Post RMS vs Ch",nch,-0.5,nch-0.5,1000,0,100);
  TH2F *lopost_mean_vs_amu = new TH2F("lopost_mean_vs_amu","Low Post Mean vs AMU",64,-0.5,63.5,8000,0,800);
  TH2F *lopost_rms_vs_amu = new TH2F("lopost_rms_vs_amu","Low Post RMS vs AMU",64,-0.5,63.5,100,0,100);
  TH2F *lopre_mean_vs_ch = new TH2F("lopre_mean_vs_ch","Low Pre Mean vs Ch",nch,-0.5,nch-0.5,8000,0,800);
  TH2F *lopre_rms_vs_ch = new TH2F("lopre_rms_vs_ch","Low Pre Mean vs Ch",nch,-0.5,nch-0.5,1000,0,100);
  TH2F *lopre_mean_vs_amu = new TH2F("lopre_mean_vs_amu","Low Pre Mean vs AMU",64,-0.5,63.5,8000,0,800);
  TH2F *lopre_rms_vs_amu = new TH2F("lopre_rms_vs_amu","Low Pre RMS vs AMU",64,-0.5,63.5,100,0,100);

  TH2F *lo_mean_chamu = new TH2F("lo_mean_chamu","Low Mean vs Ch and AMU",nch,-0.5,nch-0.5,64,-0.5,63.5);
  TH2F *lo_rms_chamu = new TH2F("lo_rms_chamu","Low RMS vs Ch and AMU",nch,-0.5,nch-0.5,64,-0.5,63.5);

  lo->Draw("mean:ch>>lo_mean_vs_ch");
  lo->Draw("rms:ch>>lo_rms_vs_ch");
  lo->Draw("mean:amu>>lo_mean_vs_amu");
  lo->Draw("rms:amu>>lo_rms_vs_amu");
  lopost->Draw("mean:ch>>lopost_mean_vs_ch");
  lopost->Draw("rms:ch>>lopost_rms_vs_ch");
  lopost->Draw("mean:amu>>lopost_mean_vs_amu");
  lopost->Draw("rms:amu>>lopost_rms_vs_amu");
  lopre->Draw("mean:ch>>lopre_mean_vs_ch");
  lopre->Draw("rms:ch>>lopre_rms_vs_ch");
  lopre->Draw("mean:amu>>lopre_mean_vs_amu");
  lopre->Draw("rms:amu>>lopre_rms_vs_amu");
  lo->Draw("amu:ch>>lo_mean_chamu","mean");
  lo->Draw("amu:ch>>lo_rms_chamu","rms");

  TH2F *hi_mean_vs_ch = new TH2F("hi_mean_vs_ch","High Post-Pre Mean vs Ch",nch,-0.5,nch-0.5,2000,-100,100);
  TH2F *hi_rms_vs_ch = new TH2F("hi_rms_vs_ch","High Post-Pre RMS vs Ch",nch,-0.5,nch-0.5,6000,0,600);
  TH2F *hi_mean_vs_amu = new TH2F("hi_mean_vs_amu","High Post-Pre Mean vs AMU",64,-0.5,63.5,2000,-100,100);
  TH2F *hi_rms_vs_amu = new TH2F("hi_rms_vs_amu","High Post-Pre RMS vs AMU",64,-0.5,63.5,600,0,600);
  TH2F *hipost_mean_vs_ch = new TH2F("hipost_mean_vs_ch","High Post Mean vs Ch",nch,-0.5,nch-0.5,8000,0,2000);
  TH2F *hipost_rms_vs_ch = new TH2F("hipost_rms_vs_ch","High Post RMS vs Ch",nch,-0.5,nch-0.5,6000,0,600);
  TH2F *hipost_mean_vs_amu = new TH2F("hipost_mean_vs_amu","High Post Mean vs AMU",64,-0.5,63.5,800,0,800);
  TH2F *hipost_rms_vs_amu = new TH2F("hipost_rms_vs_amu","High Post RMS vs AMU",64,-0.5,63.5,600,0,600);
  TH2F *hipre_mean_vs_ch = new TH2F("hipre_mean_vs_ch","High Pre Mean vs Ch",nch,-0.5,nch-0.5,8000,0,800);
  TH2F *hipre_rms_vs_ch = new TH2F("hipre_rms_vs_ch","High Pre Mean vs Ch",nch,-0.5,nch-0.5,600,0,600);
  TH2F *hipre_mean_vs_amu = new TH2F("hipre_mean_vs_amu","High Pre Mean vs AMU",64,-0.5,63.5,800,0,800);
  TH2F *hipre_rms_vs_amu = new TH2F("hipre_rms_vs_amu","High Pre RMS vs AMU",64,-0.5,63.5,600,0,600);

  TH2F *hi_mean_chamu = new TH2F("hi_mean_chamu","High Mean vs Ch and AMU",nch,-0.5,nch-0.5,64,-0.5,63.5);
  TH2F *hi_rms_chamu = new TH2F("hi_rms_chamu","High RMS vs Ch and AMU",nch,-0.5,nch-0.5,64,-0.5,63.5);

  hi->Draw("mean:ch>>hi_mean_vs_ch");
  hi->Draw("rms:ch>>hi_rms_vs_ch");
  hi->Draw("mean:amu>>hi_mean_vs_amu");
  hi->Draw("rms:amu>>hi_rms_vs_amu");
  hipost->Draw("mean:ch>>hipost_mean_vs_ch");
  hipost->Draw("rms:ch>>hipost_rms_vs_ch");
  hipost->Draw("mean:amu>>hipost_mean_vs_amu");
  hipost->Draw("rms:amu>>hipost_rms_vs_amu");
  hipre->Draw("mean:ch>>hipre_mean_vs_ch");
  hipre->Draw("rms:ch>>hipre_rms_vs_ch");
  hipre->Draw("mean:amu>>hipre_mean_vs_amu");
  hipre->Draw("rms:amu>>hipre_rms_vs_amu");
  hi->Draw("amu:ch>>hi_mean_chamu","mean");
  hi->Draw("amu:ch>>hi_rms_chamu","rms");

  TH2F *tdc_mean_vs_ch = new TH2F("tdc_mean_vs_ch","TDC Mean vs Ch",nch,-0.5,nch-0.5,3000,0,3000);
  TH2F *tdc_rms_vs_ch = new TH2F("tdc_rms_vs_ch","TDC RMS vs Ch",nch,-0.5,nch-0.5,6000,0,600);

  tdc->Draw("mean:ch>>tdc_mean_vs_ch");
  tdc->Draw("rms:ch>>tdc_rms_vs_ch");

  TGraphErrors *gtdcmean[MAXFEM] = {0};
  TGraphErrors *gtdcrms[MAXFEM] = {0};
  TGraphErrors *glopostmean[MAXFEM] = {0};
  TGraphErrors *glopostrms[MAXFEM] = {0};
  TGraphErrors *glopremean[MAXFEM] = {0};
  TGraphErrors *gloprerms[MAXFEM] = {0};
  TGraphErrors *glomean[MAXFEM] = {0};
  TGraphErrors *glorms[MAXFEM] = {0};
  TGraphErrors *ghipostmean[MAXFEM] = {0};
  TGraphErrors *ghipostrms[MAXFEM] = {0};
  TGraphErrors *ghipremean[MAXFEM] = {0};
  TGraphErrors *ghiprerms[MAXFEM] = {0};
  TGraphErrors *ghimean[MAXFEM] = {0};
  TGraphErrors *ghirms[MAXFEM] = {0};
  TGraphErrors *ghivslomean[MAXFEM] = {0};
  TGraphErrors *ghivslorms[MAXFEM] = {0};

  TCanvas *cvpostpremean[MAXFEM] = {0};
  TCanvas *cvpostprerms[MAXFEM] = {0};
  TCanvas *cvtdc[MAXFEM] = {0};
  TCanvas *cvhilomean[MAXFEM] = {0};
  TCanvas *cvhilorms[MAXFEM] = {0};
  TCanvas *cvhivslo[MAXFEM] = {0};

  for (int ifem=0; ifem<nfem; ifem++)
    {
      gtdcmean[ifem] = new TGraphErrors(MAXCH,fee567ch[ifem],tdcmean[ifem],0,tdcmeanerr[ifem]);
      name = "run_"; name +=run; name+="_"; name += "gtdcmean"; name += ifem;
      //name = "gtdcmean"; name += ifem;
      gtdcmean[ifem]->SetName(name);
      gtdcmean[ifem]->SetTitle(name);
      gtdcmean[ifem]->SetMarkerStyle(20);
      gtdcmean[ifem]->SetMarkerSize(0.5);
      gtdcrms[ifem] = new TGraphErrors(MAXCH,fee567ch[ifem],tdcrms[ifem],0,tdcrmserr[ifem]);
      name = "run_"; name +=run; name+="_";
      name += "gtdcrms"; name += ifem;
      gtdcrms[ifem]->SetName(name);
      gtdcrms[ifem]->SetTitle(name);
      gtdcrms[ifem]->SetMarkerStyle(21);
      gtdcrms[ifem]->SetMarkerSize(0.5);
    
      glopostmean[ifem] = new TGraphErrors(MAXCH,fee567ch[ifem],lopostmean[ifem],0,lopostmeanerr[ifem]);
      name = "run_"; name +=run; name+="_";     
      name += "glopostmean"; name += ifem;
      glopostmean[ifem]->SetName(name);
      glopostmean[ifem]->SetTitle(name);
      glopostmean[ifem]->SetMarkerStyle(20);
      glopostmean[ifem]->SetMarkerSize(0.5);
      glopostmean[ifem]->SetMarkerColor(ifem+1);
      glopostrms[ifem] = new TGraphErrors(MAXCH,fee567ch[ifem],lopostrms[ifem],0,lopostrmserr[ifem]);
      name = "run_"; name +=run; name+="_";
      name += "glopostrms"; name += ifem;
      glopostrms[ifem]->SetName(name);
      glopostrms[ifem]->SetTitle(name);
      glopostrms[ifem]->SetMarkerStyle(21);
      glopostrms[ifem]->SetMarkerSize(0.5);
      glopostrms[ifem]->SetMarkerColor(ifem+1);
    
      glopremean[ifem] = new TGraphErrors(MAXCH,fee567ch[ifem],lopremean[ifem],0,lopremeanerr[ifem]);
      name = "run_"; name +=run; name+="_";
      name += "glopremean"; name += ifem;
      glopremean[ifem]->SetName(name);
      glopremean[ifem]->SetTitle(name);
      glopremean[ifem]->SetMarkerStyle(20);
      glopremean[ifem]->SetMarkerSize(0.5);
      glopremean[ifem]->SetMarkerColor(ifem+1);
      gloprerms[ifem] = new TGraphErrors(MAXCH,fee567ch[ifem],loprerms[ifem],0,loprermserr[ifem]);
      name = "run_"; name +=run; name+="_";
      name += "gloprerms"; name += ifem;
      gloprerms[ifem]->SetName(name);
      gloprerms[ifem]->SetTitle(name);
      gloprerms[ifem]->SetMarkerStyle(21);
      gloprerms[ifem]->SetMarkerSize(0.5);
      gloprerms[ifem]->SetMarkerColor(ifem+1);
    
      glomean[ifem] = new TGraphErrors(MAXCH,fee567ch[ifem],lomean[ifem],0,lomeanerr[ifem]);
      name = "run_"; name +=run; name+="_";
      name += "glomean"; name += ifem;
      glomean[ifem]->SetName(name);
      glomean[ifem]->SetTitle(name);
      glomean[ifem]->SetMarkerStyle(20);
      glomean[ifem]->SetMarkerSize(0.5);
      glorms[ifem] = new TGraphErrors(MAXCH,fee567ch[ifem],lorms[ifem],0,lormserr[ifem]);
      name = "run_"; name +=run; name+="_";
      name += "glorms"; name += ifem;
      glorms[ifem]->SetName(name);
      glorms[ifem]->SetTitle(name);
      glorms[ifem]->SetMarkerStyle(21);
      glorms[ifem]->SetMarkerSize(0.5);
    
      ghipostmean[ifem] = new TGraphErrors(MAXCH,fee567ch[ifem],hipostmean[ifem],0,hipostmeanerr[ifem]);
      name = "run_"; name +=run; name+="_";
      name += "ghipostmean"; name += ifem;
      ghipostmean[ifem]->SetName(name);
      ghipostmean[ifem]->SetTitle(name);
      ghipostmean[ifem]->SetMarkerStyle(20);
      ghipostmean[ifem]->SetMarkerSize(0.5);
      ghipostmean[ifem]->SetMarkerColor(ifem+1);
      ghipostrms[ifem] = new TGraphErrors(MAXCH,fee567ch[ifem],hipostrms[ifem],0,hipostrmserr[ifem]);
      name = "run_"; name +=run; name+="_";
      name += "ghipostrms"; name += ifem;
      ghipostrms[ifem]->SetName(name);
      ghipostrms[ifem]->SetTitle(name);
      ghipostrms[ifem]->SetMarkerStyle(21);
      ghipostrms[ifem]->SetMarkerSize(0.5);
      ghipostrms[ifem]->SetMarkerColor(ifem+1);
    
      ghipremean[ifem] = new TGraphErrors(MAXCH,fee567ch[ifem],hipremean[ifem],0,hipremeanerr[ifem]);
      name = "run_"; name +=run; name+="_";
      name += "ghipremean"; name += ifem;
      ghipremean[ifem]->SetName(name);
      ghipremean[ifem]->SetTitle(name);
      ghipremean[ifem]->SetMarkerStyle(20);
      ghipremean[ifem]->SetMarkerSize(0.5);
      ghipremean[ifem]->SetMarkerColor(ifem+1);
      ghiprerms[ifem] = new TGraphErrors(MAXCH,fee567ch[ifem],hiprerms[ifem],0,hiprermserr[ifem]);
      name = "run_"; name +=run; name+="_";
      name += "ghiprerms"; name += ifem;
      ghiprerms[ifem]->SetName(name);
      ghiprerms[ifem]->SetTitle(name);
      ghiprerms[ifem]->SetMarkerStyle(21);
      ghiprerms[ifem]->SetMarkerSize(0.5);
      ghiprerms[ifem]->SetMarkerColor(ifem+1);
    
      ghimean[ifem] = new TGraphErrors(MAXCH,fee567ch[ifem],himean[ifem],0,himeanerr[ifem]);
      name = "run_"; name +=run; name+="_";
      name += "ghimean"; name += ifem;
      ghimean[ifem]->SetName(name);
      ghimean[ifem]->SetTitle(name);
      ghimean[ifem]->SetMarkerStyle(20);
      ghimean[ifem]->SetMarkerSize(0.5);
      ghimean[ifem]->SetMarkerColor(ifem+1);
      ghirms[ifem] = new TGraphErrors(MAXCH,fee567ch[ifem],hirms[ifem],0,hirmserr[ifem]);
      name = "run_"; name +=run; name+="_";
      name += "ghirms"; name += ifem;
      ghirms[ifem]->SetName(name);
      ghirms[ifem]->SetTitle(name);
      ghirms[ifem]->SetMarkerStyle(21);
      ghirms[ifem]->SetMarkerSize(0.5);
      ghirms[ifem]->SetMarkerColor(ifem+1);
 
      ghivslomean[ifem] = new TGraphErrors(MAXCH,himean[ifem],lomean[ifem],himeanerr[ifem],lomeanerr[ifem]);
      name = "run_"; name +=run; name+="_";
      name += "ghivslomean"; name += ifem;
      ghivslomean[ifem]->SetName(name);
      ghivslomean[ifem]->SetTitle(name);
      ghivslomean[ifem]->SetMarkerStyle(20);
      ghivslomean[ifem]->SetMarkerSize(0.5);
      ghivslorms[ifem] = new TGraphErrors(MAXCH,hirms[ifem],lorms[ifem],hirmserr[ifem],lormserr[ifem]);
      name = "run_"; name +=run; name+="_";
      name += "ghivslorms"; name += ifem;
      ghivslorms[ifem]->SetName(name);
      ghivslorms[ifem]->SetTitle(name);
      ghivslorms[ifem]->SetMarkerStyle(21);
      ghivslorms[ifem]->SetMarkerSize(0.5);
 
      // Draw on Canvas
      name = "run_"; name +=run; name+="_";      
      name += "cvpostpremean"; name += ifem;
      //name = "cvpostpremean"; name += ifem;
      cvpostpremean[ifem] = new TCanvas(name,name,480,640);
      cvpostpremean[ifem]->Divide(1,2);
      cvpostpremean[ifem]->cd(1);
      glopostmean[ifem]->SetMarkerColor(1);
      glopremean[ifem]->SetMarkerColor(2);
      glopostmean[ifem]->Draw("ap");
      lopost_mean_vs_ch->SetMarkerColor(4);
      lopost_mean_vs_ch->DrawCopy("same");
      hemptyfem[ifem]->Draw("same");
      glopremean[ifem]->Draw("p");
      glopostmean[ifem]->Draw("p");
      //gPad->SetLogy(1);
/*
      if ( (glopostmean[ifem]->GetHistogram()->GetMaximum()-
            glopostmean[ifem]->GetHistogram()->GetMinimum()) > 100 ) gPad->SetLogy(1);
*/
      cvpostpremean[ifem]->cd(2);
      ghipostmean[ifem]->SetMarkerColor(1);
      ghipremean[ifem]->SetMarkerColor(2);
      ghipostmean[ifem]->Draw("ap");
      hipost_mean_vs_ch->SetMarkerColor(4);
      hipost_mean_vs_ch->DrawCopy("same");
      hemptyfem[ifem]->Draw("same");
      ghipremean[ifem]->Draw("p");
      ghipostmean[ifem]->Draw("p");
      name = "run_"; name +=run; name+="_";
      name += "cvpostprerms"; name += ifem;
      cvpostprerms[ifem] = new TCanvas(name,name,480,640);
      cvpostprerms[ifem]->Divide(1,2);
      cvpostprerms[ifem]->cd(1);
      glopostrms[ifem]->SetMarkerColor(1);
      gloprerms[ifem]->SetMarkerColor(2);
      glopostrms[ifem]->Draw("ap");
      lopost_rms_vs_ch->SetMarkerColor(4);
      lopost_rms_vs_ch->DrawCopy("same");
      gloprerms[ifem]->Draw("p");
      hemptyfem[ifem]->Draw("same");
      cvpostprerms[ifem]->cd(2);
      ghipostrms[ifem]->SetMarkerColor(1);
      ghiprerms[ifem]->SetMarkerColor(2);
      ghiprerms[ifem]->Draw("ap");
      hipost_rms_vs_ch->SetMarkerColor(4);
      hipost_rms_vs_ch->DrawCopy("same");
      hemptyfem[ifem]->Draw("same");
      ghipostrms[ifem]->Draw("p");
      ghiprerms[ifem]->Draw("p");
      name = "run_"; name +=run; name+="_";
      name += "cvtdc"; name += ifem;
      cvtdc[ifem] = new TCanvas(name,name,480,640);
      cvtdc[ifem]->Divide(1,2);
      cvtdc[ifem]->cd(1);
      gtdcmean[ifem]->SetMarkerStyle(20);
      gtdcmean[ifem]->Draw("ap");
      tdc_mean_vs_ch->SetMarkerColor(4);
      tdc_mean_vs_ch->DrawCopy("same");
      hemptyfem[ifem]->Draw("same");
      gtdcmean[ifem]->Draw("p");
      cvtdc[ifem]->cd(2);
      gtdcrms[ifem]->Draw("ap");
      tdc_rms_vs_ch->SetMarkerColor(4);
      tdc_rms_vs_ch->DrawCopy("same");
      hemptyfem[ifem]->Draw("same");
      gtdcrms[ifem]->Draw("p");
      name = "run_"; name +=run; name+="_";
      name += "cvhilomean"; name += ifem;
      cvhilomean[ifem] = new TCanvas(name,name,480,640);
      cvhilomean[ifem]->Divide(1,2);
      cvhilomean[ifem]->cd(1);
      glomean[ifem]->Draw("ap");
      lo_mean_vs_ch->SetMarkerColor(4);
      lo_mean_vs_ch->DrawCopy("same");
      hemptyfem[ifem]->Draw("same");
      glomean[ifem]->Draw("p");
      cvhilomean[ifem]->cd(2);
      ghimean[ifem]->Draw("ap");
      hi_mean_vs_ch->SetMarkerColor(4);
      hi_mean_vs_ch->DrawCopy("same");
      hemptyfem[ifem]->Draw("same");
      ghimean[ifem]->Draw("p");
      name = "run_"; name +=run; name+="_";
      name += "cvhilorms"; name += ifem;
      cvhilorms[ifem] = new TCanvas(name,name,480,640);
      cvhilorms[ifem]->Divide(1,2);
      cvhilorms[ifem]->cd(1);
      glorms[ifem]->Draw("ap");
      lo_rms_vs_ch->SetMarkerColor(4);
      lo_rms_vs_ch->DrawCopy("same");
      hemptyfem[ifem]->Draw("same");
      glorms[ifem]->Draw("p");
      cvhilorms[ifem]->cd(2);
      ghirms[ifem]->Draw("ap");
      hi_rms_vs_ch->SetMarkerColor(4);
      hi_rms_vs_ch->DrawCopy("same");
      hemptyfem[ifem]->Draw("same");
      ghirms[ifem]->Draw("p");
      name = "run_"; name +=run; name+="_";
      name += "cvhivslo"; name += ifem;
      cvhivslo[ifem] = new TCanvas(name,name,480,640);
      cvhivslo[ifem]->Divide(1,2);
      cvhivslo[ifem]->cd(1);
      ghivslomean[ifem]->Draw("ap");
      hemptyfem[ifem]->Draw("same");
      cvhivslo[ifem]->cd(2);
      ghivslorms[ifem]->Draw("ap");
      hemptyfem[ifem]->Draw("same");
      // save to postscript files
      if ( printflag )
        {
	  name = cvpostpremean[ifem]->GetName(); name += ".ps";
          //name = cvpostpremean[ifem]->GetName(); name += ".ps";
          cvpostpremean[ifem]->SaveAs(name);
          name = cvpostprerms[ifem]->GetName(); name += ".ps";
          cvpostprerms[ifem]->SaveAs(name);
          name = cvtdc[ifem]->GetName(); name += ".ps";
          cvtdc[ifem]->SaveAs(name);
          name = cvhilomean[ifem]->GetName(); name += ".ps";
          cvhilomean[ifem]->SaveAs(name);
          name = cvhilorms[ifem]->GetName(); name += ".ps";
          cvhilorms[ifem]->SaveAs(name);
          name = cvhivslo[ifem]->GetName(); name += ".ps";
          cvhivslo[ifem]->SaveAs(name);
        }

      // save TGraphs to file
      if ( saveflag )
        {
          gtdcmean[ifem]->Write();
          gtdcrms[ifem]->Write();
          glopostmean[ifem]->Write();
          glopostrms[ifem]->Write();
          glopremean[ifem]->Write();
          gloprerms[ifem]->Write();
          glomean[ifem]->Write();
          glorms[ifem]->Write();
          ghipostmean[ifem]->Write();
          ghipostrms[ifem]->Write();
          ghipremean[ifem]->Write();
          ghiprerms[ifem]->Write();
          ghimean[ifem]->Write();
          ghirms[ifem]->Write();
        }

  }

  // dump pedestal files
  ofstream tacfile("tac");
  for (int ifem=0; ifem<nfem; ifem++)
    {
      for (int ich=0; ich<144; ich++)
        {
          if ( tdcrms[ifem][ich] > 12 ) tdcrms[ifem][ich] = 12;
          tacfile << tdcmean[ifem][ich] << "\t" << tdcrms[ifem][ich] << endl;
        }
    }
  tacfile.close();

  // write out root file
  if ( saveflag )
    {
      savefile->Write();
      savefile->Close();
    }
}

