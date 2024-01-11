#include <iostream>
#include <sstream>
#include <string>
#include <iomanip>

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
//#include <TGraph2D.h>
#include <TText.h>
#include <TPaveText.h>
#include <TGaxis.h>
#include <TRegexp.h>

//#include "utilfuncs.C"


#include <cstdlib>
#include <fstream>
#include <sstream>
#include <iostream>

using namespace std;

const int NMODULE=60;
const int NCHIP=8;
const double DEADTH0 = 0.0004;
const double DEADTH1 = 0.00015;
const double HOTTH0 = 0.0034;
const double HOTTH1 = 0.0023;
const char chipdir[256]="ratechip_data_temp";
/// chipdir : directory for input files (chipdir/ratechip_....)
const char rundir[256]="raterun_data_temp";
/// rundir  : directory for output files
const int maxrun = 6000;
const int maxranges = 300;
//  const int maxranges = 10;

void raterun(Int_t runstart=0, Int_t runend=0, char *ratecutfile=NULL) {
  
  gROOT->SetStyle("Plain");
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);
  
  //def  TH1::AddDirectory(kFALSE);
  bool merge = true;
  bool plot = false;
  
  char *char_tmp = chipdir;
  char command[256];
  sprintf(command,"ls %s/ratechip_??????_????.root > root.files",char_tmp);
  gSystem->Exec(command);//def
  
  ifstream infile;
  
  infile.open("root.files");
  if (!infile) {
    std::cerr << "Can't open input file " << filename << std::endl;
    exit(-1);
  }
  
  //def
  ifstream fcut;
  fcut.open(ratecutfile);
  if (fcut.fail()) {
    // if runmode=false, make temporary ratecut file
    cout << "can't open ratecut file. ratecut_tmp.txt is used." << endl;
    fcut.close();
    ofstream *outfcut = NULL;
    outfcut = new ofstream("ratecut_tmp.txt");
    for (Int_t im=0;im<NMODULE;im++) {
      for (Int_t ic=0;ic<NCHIP;ic++) {
	float minrate = (im<20) ? DEADTH0 : DEADTH1;
	float maxrate = (im<20) ? HOTTH0 : HOTTH1;
	float rate = (maxrate+minrate)*0.5;
	float sigma = (maxrate-minrate)*0.1;
	(*outfcut) << im <<"\t"<< ic <<"\t"<< runstart <<"\t"<< runend <<"\t"
		   << minrate <<"\t"<< maxrate <<"\t"<< rate <<"\t"<< sigma <<endl;
      }
    }
    outfcut->close();
    delete outfcut;
    fcut.open("ratecut_tmp.txt");
  }
  
  string rline;
  Float_t ratescut[maxranges][NMODULE][NCHIP];
  Float_t sigmascut[maxranges][NMODULE][NCHIP];
  Float_t minratescut[maxranges][NMODULE][NCHIP];
  Float_t maxratescut[maxranges][NMODULE][NCHIP];
  Int_t nranges[NMODULE][NCHIP];
  Int_t minruns[maxranges][NMODULE][NCHIP];
  Int_t maxruns[maxranges][NMODULE][NCHIP];

  for (Int_t im=0;im<NMODULE;im++) {
    for (Int_t ic=0;ic<NCHIP;ic++) {
      nranges[im][ic]=0;
    }
  }

  while (getline(fcut, rline)) {
    // read cut parameters from a file
    float minrate,maxrate;
    float rate,sigma;
    int im, ic,run0,run1;
    int nscan = sscanf(rline.c_str(),"%d\t%d\t%d\t%d\t%f\t%f\t%f\t%f",&im,&ic,&run0,&run1,&minrate,&maxrate,&rate,&sigma);
    
    if (nscan==8) {
      // if lower threshold is abnormally low, change the threshold
      if (im<20) {
	minrate = (minrate>DEADTH0/8192.) ? minrate : DEADTH0/8192.;
      } else {
	minrate = (minrate>DEADTH1/8192.) ? minrate : DEADTH1/8192.;
      }
      minratescut[nranges[im][ic]][im][ic] = minrate;
      maxratescut[nranges[im][ic]][im][ic] = maxrate;
      ratescut[nranges[im][ic]][im][ic] = rate;
      sigmascut[nranges[im][ic]][im][ic] = sigma;
      minruns[nranges[im][ic]][im][ic] = run0;
      maxruns[nranges[im][ic]][im][ic] = run1;
      nranges[im][ic]++;
    } else {
      cerr << "nscan = " << nscan << " !=8" <<endl;
      cerr << "rline = " << rline << endl;
    }
  }
  cout << "finish to get run range" << endl;
  
  Double_t runs[maxrun];//all
  int seqs[maxrun];
  Double_t rates[NMODULE][NCHIP][maxrun];

  TH1F *hevt[NMODULE][NCHIP][maxranges];
  TH2F *hmap[NMODULE][NCHIP][maxranges];
  TH1F *hrate1d[NMODULE][NCHIP];
  for (int im=0;im<NMODULE;im++) {
    for (int ic=0;ic<NCHIP;ic++) {
      hrate1d[im][ic] = NULL;
      for (int ir=0;ir<maxranges;ir++) {
	hevt[im][ic][ir] = NULL;
	hmap[im][ic][ir] = NULL;
      }
    }
  }
  for (Int_t im=0;im<NMODULE;++im) {
    for (Int_t ic=0;ic<NCHIP;++ic) {
      ostringstream ostr;
      ostr << "hrate1d_" << im << "_" << ic;
      hrate1d[im][ic] = new TH1F(ostr.str().c_str(), ostr.str().c_str(), 4000,0.000,0.200);
    }
  }
  
  string line;
  Int_t nrun=0;
  while(getline(infile, line)) {
    int run=-1;
    int seq=-1;
    char format[256];
    char_tmp = chipdir;
    sprintf(format,"%s/ratechip_%%06d_%%04d.root",char_tmp);
    int nscan = sscanf(line.data(), format, &run, &seq);
    if (nscan<=0) {
      cerr << "scan failed. skip to next run" << endl;
      continue;
    }
    if ((run<runstart)||(runend<run)) {
      //cout << "skip run " << run << endl;
      continue;
    }
    cout << line.c_str() << endl;
    runs[nrun] = run;
    seqs[nrun] = 0;
    if (nrun>=1) {
      if (runs[nrun-1]==runs[nrun]) {
	seqs[nrun] = seqs[nrun-1]+1;
      }
    }
    char hotdeadchip[200];
    char_tmp = rundir;
    sprintf(hotdeadchip,"%s/hotdeadchip_%06d_%04d.txt",char_tmp,run,seq);
    ofstream *fchiprun=NULL;
    fchiprun = new ofstream(hotdeadchip);
    TFile *fin = new TFile(line.data());
    Int_t status[NMODULE][NCHIP];
    TH1F *hrate_chip_all = (TH1F *)gROOT->FindObject("hrate_chip_all");
    Float_t *ratehist = hrate_chip_all->GetArray();
    for (int im=0;im<NMODULE;im++) {
      for (int ic=0;ic<NCHIP;ic++) {
	int chipseq = im*NCHIP+ic;
	rates[im][ic][nrun] = ratehist[chipseq+1];
	hrate1d[im][ic]->Fill(ratehist[chipseq+1],1.);
	Int_t irange=-1;
	for (Int_t ir=0;ir<nranges[im][ic];ir++) {
	  if ((minruns[ir][im][ic]<=run)&&(run<=maxruns[ir][im][ic])) {
	    irange = ir;
	    break;
	  }
	}
	if (irange==-1) {
	  cerr << "Failed to find irange. im, ic = " << im << " " << ic << endl;
	  //exit(-1);
	  continue;
	}
	
	status[im][ic]=0;//dead
	if ((minratescut[irange][im][ic]<ratehist[chipseq+1])&&(ratehist[chipseq+1]<maxratescut[irange][im][ic])){
	  status[im][ic] = 1;//normal
	} else if (ratehist[chipseq+1]>=maxratescut[irange][im][ic])  {
	  status[im][ic] = 2;//hot
	}
	
	if (ratescut[irange][im][ic]==0) {
	  status[im][ic]=0;
	}
	
	if ((status[im][ic]==0)||(status[im][ic]==2)) {
	  (*fchiprun) << run << "\t" << seq << "\t" << im << "\t" << ic << "\t" << status[im][ic]-1 << endl;
	}
      }
    }
    
    if (plot) {
      TH2F *hists[NMODULE][NCHIP];//hot/deadmap
      TH2F *hhists[NMODULE][NCHIP];//hitmap
       
      char hname[100];
      char hhname[100];
      
      for(int imod=0; imod<NMODULE; imod++){
	for(int ichip=0; ichip<NCHIP; ichip++){
	  sprintf(hname,"hotdeadmap_%d_%d",imod,ichip);
	  hists[imod][ichip] = (TH2F *)gROOT->FindObject(hname);
	  sprintf(hhname,"hitmap_%d_%d",imod,ichip);
	  hhists[imod][ichip] = (TH2F *)gROOT->FindObject(hhname);
	}
      }
      
      TH1F *h_evt = (TH1F *)gROOT->FindObject("h_pxl_evt");
      int nevt = h_evt->GetEntries();
      
      gStyle->SetPalette(1);
      TCanvas* c2 = new TCanvas("c2", "c2", 1000, 700);
      
      drawPixelHotDeadmap(c2, run, seq, nevt, hists, status);
      c2->Clear();
      drawPixelHitmap(c2, run, seq, nevt, hhists, status);
      c2->Clear();
      delete c2;
    }
    
    //merge hitmap
    //def
    if (merge) {
      TH1F *h_evt = (TH1F *)gROOT->FindObject("h_pxl_evt");
      //TH1F *h_evt = (TH1F *)fin->Get("h_pxl_evt");
      if (h_evt == NULL) {
	exit(-1);
      }
      char hname[300];
      for(Int_t im=0;im<NMODULE;im++){
	for(Int_t ic=0;ic<NCHIP;ic++){
	  Int_t irange=-1;
	  for (Int_t ir=0;ir<nranges[im][ic];ir++) {
	    if ((minruns[ir][im][ic]<=run)&&(run<=maxruns[ir][im][ic])) {
	      irange = ir;
	      break;
	    }
	  }
	  if (irange==-1) {
	    //cerr << "Failed to find irange. run, im, ic = " << run << " " << im << " " << ic << endl;
	    //exit(-1);
	    continue;
	  }
	  if (status[im][ic]!=1) {
	    continue;
	  }

	  // cout << "irange = " << irange << endl;
	  sprintf(hname,"hitmap_%d_%d",im,ic);
	  TH2F *h_hitmap = (TH2F *)gROOT->FindObject(hname);
	  if (h_hitmap == NULL) {
	    cerr << hname << " == NULL for " << tsname << endl;
	    exit(-1);
	  }
	  
	  if (hevt[im][ic][irange] == NULL) {
	    ostringstream oevt;
	    oevt << "h_pxl_evt"<<"_" << im << "_" << ic<<"_"<<minruns[ir][im][ic]<<"_"<<maxruns[ir][im][ic];
	    hevt[im][ic][irange] = (TH1F *)h_evt->Clone(oevt.str().c_str());
	    hevt[im][ic][irange]->SetName(oevt.str().c_str());
	    hevt[im][ic][irange]->SetDirectory(0);
	  } else {
	    if (h_evt != NULL) {
	      hevt[im][ic][irange]->Add(h_evt);
	    } else {
	      cerr << "h_pxl_evt == NULL for " << oevt.str().c_str() << endl;
	    }
	  }
	  
	  if (h_hitmap->GetEntries()>0) {
	    if (hmap[im][ic][irange] == NULL) {
	      ostringstream omap;
	      omap << "h_pxl_hitmap"<<"_" << im << "_" << ic<<"_"<<minruns[ir][im][ic]<<"_"<<maxruns[ir][im][ic];
	      hmap[im][ic][irange] = (TH2F *)h_hitmap->Clone(omap.str().c_str());
	      hmap[im][ic][irange]->SetName(omap.str().c_str());
	      hmap[im][ic][irange]->SetDirectory(0);
	    } else {
	      hmap[im][ic][irange]->Add(h_hitmap);
	    }
	  }
	}
      }
      //cout << "end of merge" << endl;
    }
    fin->Close();
    delete fin;

    nrun ++;
    if (nrun>=maxrun) {
      cout << "maxrun reached. breaking." << endl;
      break;
    }

    //def 
    if (fchiprun != NULL) {
      fchiprun->close();
      delete fchiprun;
    } else {
      cerr << "fchiprun = NULL" << endl;
    }
  }//end run loop
  
  ostringstream foutstr;
  //def
  foutstr << rundir << "/raterun_" << runstart << "_" << runend <<".root";
  TFile *fout = new TFile(foutstr.str().c_str(),"RECREATE");
  TGraph *grate[NMODULE][NCHIP];
  
  for (Int_t im=0;im<NMODULE;++im) {
    for (Int_t ic=0;ic<NCHIP;++ic) {
      grate[im][ic] = new TGraph(nrun, runs, rates[im][ic]);
      grate[im][ic]->SetMarkerStyle(25);
      ostringstream ostr;
      ostr << "grate_" << im << "_" << ic;
      grate[im][ic]->Write(ostr.str().c_str());
      hrate1d[im][ic]->Write();
      delete hrate1d[im][ic];
      delete grate[im][ic];
      
      for (Int_t ir=0;ir<nranges[im][ic];ir++) {
	if (hevt[im][ic][ir]==NULL) {
	  //write empty histos
	  ostringstream oevt;
	  oevt << "h_pxl_evt"<<"_" << im << "_" << ic<<"_"<<minruns[ir][im][ic]<<"_"<<maxruns[ir][im][ic];
	  hevt[im][ic][ir] = new TH1F(oevt.str().c_str(),oevt.str().c_str(),2,-1.0,1.0);
	  
	  ostringstream omap;
	  omap << "h_pxl_hitmap"<<"_" << im << "_" << ic<<"_"<<minruns[ir][im][ic]<<"_"<<maxruns[ir][im][ic];
	  hmap[im][ic][ir] = new TH2F(omap.str().c_str(),omap.str().c_str(),32,-0.5,31.5,256,-0.5,255.5);
	}
	ostringstream oevt;
	oevt << "h_pxl_evt"<<"_" << im << "_" << ic<<"_"<<minruns[ir][im][ic]<<"_"<<maxruns[ir][im][ic];
	hevt[im][ic][ir]->Write(oevt.str().c_str());
	
	ostringstream omap;
	omap << "h_pxl_hitmap"<<"_" << im << "_" << ic<<"_"<<minruns[ir][im][ic]<<"_"<<maxruns[ir][im][ic];
	hmap[im][ic][ir]->Write(omap.str().c_str());
	
	delete hevt[im][ic][ir];
	delete hmap[im][ic][ir];
      }
    }
  }
  fout->Close();
  delete fout;
  cout << "end to create raterun.root"<<endl;
}

void drawPixelHotDeadmap(TCanvas *c1, int run, int seq, int nevt, TH2F* h_hist[NMODULE][NCHIP],Int_t status[NMODULE][NCHIP]){
  if(c1==NULL){
    return;
  }
  //cout<<"srun : "<<s_run->Data()<<endl;

  TText *sSide[4];
  sSide[0] = new TText(0.132, 0.59, "South");
  sSide[1] = new TText(0.35, 0.59, "North");
  sSide[2] = new TText(0.58, 0.59, "North");
  sSide[3] = new TText(0.83, 0.59, "South");
  for(int i=0; i<4; i++){
    sSide[i]->SetNDC();
    sSide[i]->SetTextSize(0.03);
  }


  TGaxis *ax[4];
  ax[0] = new TGaxis(0.255, 0.03, 0.065, 0.03, 0, 7, 10, "BS");
  ax[1] = new TGaxis(0.475, 0.03, 0.285, 0.03, 0, 7, 10, "BS");
  ax[2] = new TGaxis(0.715, 0.03, 0.525, 0.03, 0, 7, 10, "BS");
  ax[3] = new TGaxis(0.935, 0.03, 0.74,  0.03, 0, 7, 10, "BS");
  for(int ia=0; ia<4; ia++){
    ax[ia]->SetTickSize(0.0);
    ax[ia]->SetLabelSize(0.03);
  }

  //y-axis
  //  TGaxis *ax3[4];
  //  ax3[0] = new TGaxis(0.03, 0.65,  0.0301, 0.845,  0,  4,  5, "BSI");
  //  ax3[1] = new TGaxis(0.98, 0.845, 0.9801, 0.65,  5,  9,  5, "BSI");
  //  ax3[2] = new TGaxis(0.03, 0.08,  0.0301, 0.545,  0,  9, 10, "BSI");
  //  ax3[3] = new TGaxis(0.98, 0.545, 0.9801, 0.08, 10, 19, 10, "BSI");
  //  for(int ia=0; ia<4; ia++){
  //    ax3[ia]->SetTickSize(0.0);
  //    ax3[ia]->SetLabelSize(0.03);
  //  }

  TGaxis *ax3[8];
  ax3[0] = new TGaxis(0.03, 0.65,  0.0301, 0.845, 5,  9,  5, "BSI");
  ax3[1] = new TGaxis(0.48, 0.65,  0.4801, 0.845, 0,  4,  5, "BSI");
  ax3[2] = new TGaxis(0.52, 0.845, 0.5201, 0.650,10, 14,  5, "BSI");
  ax3[3] = new TGaxis(0.98, 0.845, 0.9801, 0.650,15, 19,  5, "BSI");

  ax3[4] = new TGaxis(0.03, 0.08,  0.0301, 0.545,30, 39, 10, "BSI");
  ax3[5] = new TGaxis(0.48, 0.08,  0.4801, 0.545,20, 29, 10, "BSI");
  ax3[6] = new TGaxis(0.52, 0.545, 0.5201, 0.080,40, 49, 10, "BSI");
  ax3[7] = new TGaxis(0.98, 0.545, 0.9801, 0.080,50, 59, 10, "BSI");

  for(int ia=0; ia<8; ia++){
    ax3[ia]->SetTickSize(0.0);
    ax3[ia]->SetLabelSize(0.03);
  }

  // 
  TString s[4]={"WestB0", "EastB0", "WestB1", "EastB1"};

  c1->cd();
  TPad *p[4];

  //  float padx1[4]={0.05, 0.51, 0.05, 0.51};
  //  float padx2[4]={0.49, 0.95, 0.49, 0.95};
  //  float pady1[4]={0.61, 0.61, 0.05, 0.05};
  //  float pady2[4]={0.87, 0.87, 0.59, 0.59};

  float padx1[4]={0.05, 0.53, 0.05, 0.53};
  float padx2[4]={0.47, 0.95, 0.47, 0.95};
  float pady1[4]={0.61, 0.61, 0.05, 0.05};
  float pady2[4]={0.87, 0.87, 0.59, 0.59};

  for(int i=0; i<4; i++){
    c1->cd();
    p[i] = new TPad(Form("p%d",i), s[i].Data(), padx1[i], pady1[i], padx2[i], pady2[i]);
    if(i<2) p[i]->Divide(16, 5, 0.0, 0.0);
    else    p[i]->Divide(16, 10, 0.0, 0.0);
    p[i]->Draw();

    if(i<2){
      float x1 = (i==0)? 0.07 : 0.53;
      TPaveText *ptitle = new TPaveText(x1, 0.92, x1+0.4, 0.95);
      if (i==0) {
	ptitle->AddText(Form("West"));
      } else {
	ptitle->AddText(Form("East"));
      }
      if (i==0) {
	TPaveText *ptitle0 = new TPaveText(0.30, 0.96, 0.70, 0.99);
	ptitle0->SetTextSize(0.03);
	ptitle0->AddText(Form("Run: %d  Seq: %04d   Nevent : %d", run, seq, nevt));
	ptitle0->SetFillColor(0);
	ptitle0->SetLineColor(0);
	ptitle0->SetBorderSize(0);
	ptitle0->Draw();
      }

      ptitle->SetFillColor(0);
      ptitle->SetLineColor(0);
      ptitle->SetBorderSize(0);
      ptitle->Draw();

      TPaveText *ptitleh = new TPaveText(x1, 0.90, x1+0.4, 0.92);
      ptitleh->SetTextSize(0.02);
      ptitleh->SetFillColor(0);
      ptitleh->SetLineColor(0);
      ptitleh->SetBorderSize(0);
      ptitleh->Draw();

      TPaveText *ptitled = new TPaveText(x1, 0.88, x1+0.4, 0.90);
      ptitled->SetTextSize(0.02);
      ptitled->SetFillColor(0);
      ptitled->SetLineColor(0);
      ptitled->SetBorderSize(0);
      ptitled->Draw();
    }

    //-- decolation
    for(int is=0; is<4; is++) sSide[is]->Draw();
    for(int ia=0; ia<4; ia++){ ax[ia]->Draw(); }
    for(int ia=0; ia<8; ia++){ ax3[ia]->Draw(); }
    //-- decolation end
  }

  gStyle->SetPalette(1);


  for(int imod=0; imod<NMODULE; imod++){

    int ip = 0;
    if     (imod<10){ ip = 0;}
    else if(imod<20){ ip = 1;}
    else if(imod<40){ ip = 2;}
    else            { ip = 3;}

    int ican=0; 
    if     (imod< 5) ican = 10-(2*(imod-0));
    else if(imod<10) ican =  9-(2*(imod-5));

    else if(imod<15) ican = 2*(imod-10)+1;
    else if(imod<20) ican = 2*(imod-15)+2;

    else if(imod<30) ican = 20-(2*(imod-20));
    else if(imod<40) ican = 19-(2*(imod-30));

    else if(imod<50) ican = 2*(imod-40)+1;
    else             ican = 2*(imod-50)+2;

    for(int ichip=0; ichip<NCHIP; ichip++){
      TPad *pp = (TPad*)p[ip]->cd(8*ican-ichip);


      h_hist[imod][ichip]->SetMaximum(2.);
      h_hist[imod][ichip]->Draw("colz");

      if (status != 0) {
      if (status[imod][ichip]==0) {
	//	cout <<"Found status 0, color is black" << endl;
	TBox *b = new TBox(1,1,30,252);
	b->SetFillStyle(0);
	b->SetFillColor(0);
	b->SetLineColor(4);
	b->SetLineWidth(3);
	b->Draw();
	//	pp->SetLineWidth(1);
	//	pp->SetLineColor(1);
	//	pp->SetFillColor(1);
	
      } else if (status[imod][ichip]==2) {
	//	cout <<"Found status 2, color is yellow" << endl;
	TBox *b = new TBox(1,1,30,252);
	b->SetFillStyle(0);
	b->SetFillColor(0);
	b->SetLineColor(5);
	b->SetLineWidth(3);
	b->Draw();
	//	pp->SetLineWidth(5);
	//	pp->SetLineColor(5);
	//	pp->SetFillColor(5);
      }
      }
      pp->Modified();
      pp->Update();
    }
    p[ip]->Modified();
    p[ip]->Update();
  }

  //c1->Print(Form("raterun_data_temp/hotdeadmap_pixel_%06d_%04d.png", run, seq));
  c1->Print(Form("raterun_data_temp_new2/hotdeadmap_pixel_%06d_%04d.png", run, seq));
}


//hitmap
void drawPixelHitmap(TCanvas *c1, int run, int seq, int nevt, TH2F* h_hist[NMODULE][NCHIP],Int_t status[NMODULE][NCHIP]){
  if(c1==NULL){
    return;
  }
  //cout<<"srun : "<<s_run->Data()<<endl;

  TText *sSide[4];
  sSide[0] = new TText(0.132, 0.59, "South");
  sSide[1] = new TText(0.35, 0.59, "North");
  sSide[2] = new TText(0.58, 0.59, "North");
  sSide[3] = new TText(0.83, 0.59, "South");
  for(int i=0; i<4; i++){
    sSide[i]->SetNDC();
    sSide[i]->SetTextSize(0.03);
  }


  TGaxis *ax[4];
  ax[0] = new TGaxis(0.255, 0.03, 0.065, 0.03, 0, 7, 10, "BS");
  ax[1] = new TGaxis(0.475, 0.03, 0.285, 0.03, 0, 7, 10, "BS");
  ax[2] = new TGaxis(0.715, 0.03, 0.525, 0.03, 0, 7, 10, "BS");
  ax[3] = new TGaxis(0.935, 0.03, 0.74,  0.03, 0, 7, 10, "BS");
  for(int ia=0; ia<4; ia++){
    ax[ia]->SetTickSize(0.0);
    ax[ia]->SetLabelSize(0.03);
  }

  //y-axis
  //  TGaxis *ax3[4];
  //  ax3[0] = new TGaxis(0.03, 0.65,  0.0301, 0.845,  0,  4,  5, "BSI");
  //  ax3[1] = new TGaxis(0.98, 0.845, 0.9801, 0.65,  5,  9,  5, "BSI");
  //  ax3[2] = new TGaxis(0.03, 0.08,  0.0301, 0.545,  0,  9, 10, "BSI");
  //  ax3[3] = new TGaxis(0.98, 0.545, 0.9801, 0.08, 10, 19, 10, "BSI");
  //  for(int ia=0; ia<4; ia++){
  //    ax3[ia]->SetTickSize(0.0);
  //    ax3[ia]->SetLabelSize(0.03);
  //  }

  TGaxis *ax3[8];
  ax3[0] = new TGaxis(0.03, 0.65,  0.0301, 0.845, 5,  9,  5, "BSI");
  ax3[1] = new TGaxis(0.48, 0.65,  0.4801, 0.845, 0,  4,  5, "BSI");
  ax3[2] = new TGaxis(0.52, 0.845, 0.5201, 0.650,10, 14,  5, "BSI");
  ax3[3] = new TGaxis(0.98, 0.845, 0.9801, 0.650,15, 19,  5, "BSI");

  ax3[4] = new TGaxis(0.03, 0.08,  0.0301, 0.545,30, 39, 10, "BSI");
  ax3[5] = new TGaxis(0.48, 0.08,  0.4801, 0.545,20, 29, 10, "BSI");
  ax3[6] = new TGaxis(0.52, 0.545, 0.5201, 0.080,40, 49, 10, "BSI");
  ax3[7] = new TGaxis(0.98, 0.545, 0.9801, 0.080,50, 59, 10, "BSI");

  for(int ia=0; ia<8; ia++){
    ax3[ia]->SetTickSize(0.0);
    ax3[ia]->SetLabelSize(0.03);
  }

  // 
  TString s[4]={"WestB0", "EastB0", "WestB1", "EastB1"};

  c1->cd();
  TPad *p[4];

  //  float padx1[4]={0.05, 0.51, 0.05, 0.51};
  //  float padx2[4]={0.49, 0.95, 0.49, 0.95};
  //  float pady1[4]={0.61, 0.61, 0.05, 0.05};
  //  float pady2[4]={0.87, 0.87, 0.59, 0.59};

  float padx1[4]={0.05, 0.53, 0.05, 0.53};
  float padx2[4]={0.47, 0.95, 0.47, 0.95};
  float pady1[4]={0.61, 0.61, 0.05, 0.05};
  float pady2[4]={0.87, 0.87, 0.59, 0.59};

  for(int i=0; i<4; i++){
    c1->cd();
    p[i] = new TPad(Form("p%d",i), s[i].Data(), padx1[i], pady1[i], padx2[i], pady2[i]);
    if(i<2) p[i]->Divide(16, 5, 0.0, 0.0);
    else    p[i]->Divide(16, 10, 0.0, 0.0);
    p[i]->Draw();

    if(i<2){
      float x1 = (i==0)? 0.07 : 0.53;
      TPaveText *ptitle = new TPaveText(x1, 0.92, x1+0.4, 0.95);
      if (i==0) {
	ptitle->AddText(Form("West"));
      } else {
	ptitle->AddText(Form("East"));
      }
      if (i==0) {
	TPaveText *ptitle0 = new TPaveText(0.30, 0.96, 0.70, 0.99);
	ptitle0->SetTextSize(0.03);
	ptitle0->AddText(Form("Run: %d  Seq: %04d   Nevent : %d", run, seq, nevt));
	ptitle0->SetFillColor(0);
	ptitle0->SetLineColor(0);
	ptitle0->SetBorderSize(0);
	ptitle0->Draw();
      }

      ptitle->SetFillColor(0);
      ptitle->SetLineColor(0);
      ptitle->SetBorderSize(0);
      ptitle->Draw();

      TPaveText *ptitleh = new TPaveText(x1, 0.90, x1+0.4, 0.92);
      ptitleh->SetTextSize(0.02);
      ptitleh->SetFillColor(0);
      ptitleh->SetLineColor(0);
      ptitleh->SetBorderSize(0);
      ptitleh->Draw();

      TPaveText *ptitled = new TPaveText(x1, 0.88, x1+0.4, 0.90);
      ptitled->SetTextSize(0.02);
      ptitled->SetFillColor(0);
      ptitled->SetLineColor(0);
      ptitled->SetBorderSize(0);
      ptitled->Draw();
    }

    //-- decolation
    for(int is=0; is<4; is++) sSide[is]->Draw();
    for(int ia=0; ia<4; ia++){ ax[ia]->Draw(); }
    for(int ia=0; ia<8; ia++){ ax3[ia]->Draw(); }
    //-- decolation end
  }

  gStyle->SetPalette(1);


  for(int imod=0; imod<NMODULE; imod++){

    int ip = 0;
    if     (imod<10){ ip = 0;}
    else if(imod<20){ ip = 1;}
    else if(imod<40){ ip = 2;}
    else            { ip = 3;}

    int ican=0; 
    if     (imod< 5) ican = 10-(2*(imod-0));
    else if(imod<10) ican =  9-(2*(imod-5));

    else if(imod<15) ican = 2*(imod-10)+1;
    else if(imod<20) ican = 2*(imod-15)+2;

    else if(imod<30) ican = 20-(2*(imod-20));
    else if(imod<40) ican = 19-(2*(imod-30));

    else if(imod<50) ican = 2*(imod-40)+1;
    else             ican = 2*(imod-50)+2;

    for(int ichip=0; ichip<NCHIP; ichip++){
      //      cout << "imod, ichip = " << imod << " " << ichip << endl;
      TPad *pp = (TPad*)p[ip]->cd(8*ican-ichip);

      Double_t scale = 1.;
      if (nevt>0) {
	scale = 1./(double)nevt;
      }
      h_hist[imod][ichip]->Scale(scale);
      if (imod<20) {
	h_hist[imod][ichip]->SetMaximum(0.0034);
      } else {
	h_hist[imod][ichip]->SetMaximum(0.0023);
      }
      h_hist[imod][ichip]->Draw("colz");

      if (status != 0) {
      if (status[imod][ichip]==0) {
	//	cout <<"Found status 0, color is black" << endl;
	TBox *b = new TBox(1,1,30,252);
	b->SetFillStyle(0);
	b->SetFillColor(0);
	b->SetLineColor(4);
	b->SetLineWidth(3);
	b->Draw();
	//	pp->SetLineWidth(1);
	//	pp->SetLineColor(1);
	//	pp->SetFillColor(1);
	
      } else if (status[imod][ichip]==2) {
	//	cout <<"Found status 2, color is yellow" << endl;
	TBox *b = new TBox(1,1,30,252);
	b->SetFillStyle(0);
	b->SetFillColor(0);
	b->SetLineColor(5);
	b->SetLineWidth(3);
	b->Draw();
	//	pp->SetLineWidth(5);
	//	pp->SetLineColor(5);
	//	pp->SetFillColor(5);
      }
      }
      pp->Modified();
      pp->Update();
    }
    p[ip]->Modified();
    p[ip]->Update();
  }

  //  cout << "going to write " << Form("raterun_data_temp/hitmap_pixel_%06d_%04d.png", run, seq) << endl;
  cout << "going to write " << Form("raterun_data_temp_new2/hitmap_pixel_%06d_%04d.png", run, seq) << endl;
  //  c1->Print(Form("raterun_data_temp/hitmap_pixel_%06d_%04d.png", run, seq));
  //  c1->SaveAs(Form("raterun_data_temp/hitmap_pixel_%06d_%04d.png", run, seq));
  c1->SaveAs(Form("raterun_data_temp_new2/hitmap_pixel_%06d_%04d.png", run, seq));
  //  cout << "end to write " << Form("raterun_data_temp/hitmap_pixel_%06d_%04d.png", run, seq) << endl;
  cout << "end to write " << Form("raterun_data_temp_new2/hitmap_pixel_%06d_%04d.png", run, seq) << endl;

  delete [] sSide;
  delete [] ax;
  delete [] ax3;
  //  delete [] p;

}




void drawPixelHitmapRun(int run, int seq){

  char name[300];
  //def
  sprintf(name,"svxdstqa2/SvxDstQA_Merge_%06d_%04d.root",run,seq);
  TFile *fhist = new TFile(name);
  
  TH2F *hists[NMODULE][NCHIP];
  char hname[100];
  for(int imod=0; imod<NMODULE; imod++){
    for(int ichip=0; ichip<NCHIP; ichip++){
      sprintf(hname,"h_pxl_hitmap_%d_%d",imod,ichip);
      hists[imod][ichip] = (TH2F *)gROOT->FindObject(hname);
    }
  }
  
  TH1F *h_evt = (TH1F *)gROOT->FindObject("h_pxl_evt");
  int nevt = h_evt->GetEntries();
  
  gStyle->SetPalette(1);
  TCanvas* c2 = new TCanvas("c2", "c2", 1000, 700);
  
  drawPixelHitmap(c2, run, seq, nevt, hists,0);
  fhist->Close();

}
