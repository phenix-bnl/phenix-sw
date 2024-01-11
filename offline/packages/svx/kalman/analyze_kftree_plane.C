
#include <TFile.h>
#include <TTree.h>
#include <TGraphErrors.h>
#include <TH2.h>
#include <TF1.h>
#include <TMath.h>
#include <TROOT.h>
#include <TCanvas.h>
#include <TLatex.h>
#include <TStyle.h>

#include <iostream>
#include <cmath>
using namespace std;

// //#ifndef __CINT__
// #include <boost/multi_array.hpp>
// typedef boost::multi_array<float, 2> array2d;
// typedef boost::multi_array<float, 3> array3d;
// typedef boost::multi_array<float, 4> array4d;
// //#endif


#define NLAYERS 5

struct Arrays
{
  float kfmom[NLAYERS][3];
  float initmom[NLAYERS][3];
  float mcpos[NLAYERS][3];
  float mcmom[NLAYERS][3];
  int mcpid[NLAYERS];
  float chisq[NLAYERS];
  float clusterpos[NLAYERS][3];
  float state[NLAYERS][3][5];
  float covar[NLAYERS][3][5][5];
  float statemc[NLAYERS][5];
};

double sqr(double x) {return x*x;}
void fill_geom_hists(Arrays &, TH1*);
TF1* fit_chisq(TH1* h, float ndf);

void analyze_kftree_plane(const char* infilename)
{
  bool stepthrough = false;
  const int n(12);
  double bins[n] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};
  //   const int n(23);
  //   double bins[n] = {0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5,
  //                     6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11};
  
  vector<double> ptbins(n);
  std::copy(&(bins[0]), &(bins[n]), ptbins.begin());
  
  gROOT->Reset();
  TCanvas* c15 = 0;
  if(stepthrough) {
    c15 = new TCanvas("c15", "c15", 800, 800);
    TH2F* h = new TH2F("h", "Circle Fit", 100, -20, 20, 100, -20, 20);
    h->SetXTitle("x (cm)");
    h->SetYTitle("y (cm)");
    h->SetStats(0);
    h->Draw();
  }
  
  TFile *f = new TFile(infilename);
  TTree *kftree = (TTree*) gDirectory->Get("kftree");

  Arrays arr;
  
  //Declaration of leaves types
  // float kfmom[NLAYERS][3] = {{0}};
  // float initmom[NLAYERS][3] = {{0}};
  float mcvtxmom[3];
  float mcvtxpos[3];
  int nsegments;
  int nclusters;
  // float chisq;
  int ndf;
  float initcharge;
  float kfcharge;
  // float mcpos[NLAYERS][3] = {{0}};
  // float clusterpos[NLAYERS][3] = {{0}};
  // float state[NLAYERS][3][5] = {{{0}}};
  // float covar[NLAYERS][3][5][5] = {{{{0}}}};
  // float statemc[NLAYERS][5] = {{0}};
  float vtx[3];
  float pca[3];

  float dcmom;
  float dcphi;
  float dctheta;
  float dcalpha;
  float simdcmom;
  float simdcphi;
  float simdctheta;
  float simdcalpha;
  
  // Set branch addresses.
  kftree->SetBranchAddress("kfmom", arr.kfmom);
  kftree->SetBranchAddress("initmom", arr.initmom);
  kftree->SetBranchAddress("mcmom", arr.mcmom);
  kftree->SetBranchAddress("mcvtxmom", mcvtxmom);
  kftree->SetBranchAddress("mcvtxpos", mcvtxpos);
  kftree->SetBranchAddress("mcpid", arr.mcpid);
  kftree->SetBranchAddress("nsegments", &nsegments);
  kftree->SetBranchAddress("nclusters", &nclusters);
  kftree->SetBranchAddress("chisq", arr.chisq);
  kftree->SetBranchAddress("initcharge", &initcharge);
  kftree->SetBranchAddress("kfcharge", &kfcharge);
  kftree->SetBranchAddress("mcpos", arr.mcpos);
  kftree->SetBranchAddress("clusterpos", arr.clusterpos);
  kftree->SetBranchAddress("state", arr.state);
  kftree->SetBranchAddress("covar", arr.covar);
  kftree->SetBranchAddress("statemc", arr.statemc);
  kftree->SetBranchAddress("vtx", vtx);
  kftree->SetBranchAddress("pca", pca);

  
  Long64_t nentries = kftree->GetEntries();
  
  char temp[1000];
  TH1F* hkfout[100] = {0};
  // TH1F* hkfrout[100] = {0};
  TH1F* hinitout[100] = {0};
  for(unsigned int ipt=0; ipt<ptbins.size()-1; ++ipt) {
    sprintf(temp, "hkfout%d", ipt);
    hkfout[ipt] = new TH1F(temp, "p_{T} (KF-MC_{out})/MC_{out}",
                           500, -4, 4);
    // sprintf(temp, "hkfrout%d", ipt);
    // hkfrout[ipt] = new TH1F(temp, "p_{T} (KF-MC_{out})/MC_{out}",
    //                         500, -4, 4);
    sprintf(temp, "hinitout%d", ipt);
    hinitout[ipt] = new TH1F(temp, "p_{T} (Init-MC_{out})/MC_{out}",
                             500, -4, 4);
  }

  TAxis* pt_axis = new TAxis(ptbins.size()-1, &(ptbins[0]));
  TH1F* hkfchisq = new TH1F("hkfchisq", "Kalman #chi^{2}",
                            100, 0, 100);
  TH1F* hmcout = new TH1F("hmcout", "p_{T} (MC_{in}-MC_{out})/MC_{in}",
                          500, -1, 1);
  TH1F* hkfinit = new TH1F("hkfinit", "p_{T} (KF-Init)/Init",
                           500, -1, 1);

  TH1F* hhelixout = new TH1F("hhelixout", "p_{T} (helix-MC_{out})/MC_{out}",
                             500, -1, 1);

  TH1F* hkfoutz = new TH1F("hkfoutz", "tan(#lambda)=dp_{z}/dp_{T} (KF-MC_{out})/MC_{out}",
                           500, -.5, .5);
  TH1F* hinitoutz = new TH1F("hinitoutz", "tan(#lambda)=dp_{z}/dp_{T} (Init-MC_{out})/MC_{out}",
                             500, -.5, .5);
  TH1F* hhelixoutz = new TH1F("hhelixoutz", "tan(#lambda)=dp_{z}/dp_{T} (helix-MC_{out})/MC_{out}",
                              500, -.5, .5);
  
  TH1F* hhelixchisq = new TH1F("hhelixchisq", "helix #chi^{2}",
                               500, 0, 100);
  TH1F* hkfhelix = new TH1F("hkfhelix", "p_{T} (KF-helix)/helix",
                            500, -1, 1);

  TH2F* hinitmc = new TH2F("hinitmc", "p_{T} (Init-MC_{out})/MC_{out}",
                           500, -1, 1, 500, -1, 1);

  TH1F* geomhist = new TH1F("geomhist", "",
                            500, -1, 1);

  TH1F* hpull[5][5] = {{0}};
  TH1F* hres[5][5] = {{0}};
  for(int ilayer=0; ilayer<5; ++ilayer) {
    sprintf(temp, "hpull%d0", ilayer);
    hpull[ilayer][0] = new TH1F(temp, "1/p (KF-MC)/sigma",
                                100, -8, 8);
    sprintf(temp, "hpull%d1", ilayer);
    hpull[ilayer][1] = new TH1F(temp, "dv/du (KF-MC)/sigma",
                                100, -4, 4);
    sprintf(temp, "hpull%d2", ilayer);
    hpull[ilayer][2] = new TH1F(temp, "dw/du (KF-MC)/sigma",
                                100, -4, 4);
    sprintf(temp, "hpull%d3", ilayer);
    hpull[ilayer][3] = new TH1F(temp, "v (KF-MC)/sigma",
                                100, -4, 4);
    sprintf(temp, "hpull%d4", ilayer);
    hpull[ilayer][4] = new TH1F(temp, "w (KF-MC)/sigma",
                                100, -4, 4);

    sprintf(temp, "hres%d0", ilayer);
    hres[ilayer][0] = new TH1F(temp, "1/p (KF-MC)",
                               1000, -2, 2);
    sprintf(temp, "hres%d1", ilayer);
    hres[ilayer][1] = new TH1F(temp, "dv/du (KF-MC)",
                               100, -.01, .01);
    sprintf(temp, "hres%d2", ilayer);
    hres[ilayer][2] = new TH1F(temp, "dw/du (KF-MC)",
                               100, -.02, .02);
    sprintf(temp, "hres%d3", ilayer);
    hres[ilayer][3] = new TH1F(temp, "v (KF-MC)",
                               100, -.01, .01);
    sprintf(temp, "hres%d4", ilayer);
    hres[ilayer][4] = new TH1F(temp, "w (KF-MC)",
                               100, -.1, .1);
  }

  TH1F* hdca[4];
  hdca[0] = new TH1F("hdca0", "x_{DCA}", 1000, -500, 500);
  hdca[1] = new TH1F("hdca1", "y_{DCA}", 1000, -500, 500);
  hdca[2] = new TH1F("hdca2", "z_{DCA}", 1000, -500, 500);
  hdca[3] = new TH1F("hdca3", "3D DCA", 500, 0, 500);
  //hdca[3] = new TH1F("hdca3", "2D (x-y) DCA", 500, 0, 500);
  hdca[0]->SetXTitle("x_{DCA} (#mum)");
  hdca[1]->SetXTitle("y_{DCA} (#mum)");
  hdca[2]->SetXTitle("z_{DCA} (#mum)");
  hdca[3]->SetXTitle("DCA (#mum)");

  TH1F* hclusres[8] = {0};
  for(int i=0; i<8; ++i) {
    sprintf(temp, "clusterres%d", i);
    if(i<4)
      hclusres[i] = new TH1F(temp, temp, 100, -100, 100);
    else
      hclusres[i] = new TH1F(temp, temp, 100, -1000, 1000);
  }

  TGraphErrors* clus_graph = new TGraphErrors(4);
  clus_graph->SetMarkerStyle(20);
  TGraphErrors* clus_graph2 = new TGraphErrors(4);
  clus_graph2->SetMarkerStyle(20);
  TF1* fhelix = new TF1("fhelix", "[3]*sqrt(pow(333.*[2]/.92,2)-pow(x-[0],2))+[1]", -20, 20);
  fhelix->SetLineColor(2);
  TF1* fline = new TF1("fline", "[1]*x-[0]", 0, 20);
  fhelix->SetLineColor(2);

  for(Long64_t i=0; i<nentries;i++) {
    if(i%10000==0) cout << i << endl;
    kftree->GetEntry(i);

    float kfpt = sqrt(sqr(arr.kfmom[0][0]) + sqr(arr.kfmom[0][1]));
    float initpt = sqrt(sqr(arr.initmom[0][0]) + sqr(arr.initmom[0][1]));
    float mcptin = sqrt(sqr(arr.mcmom[0][0]) + sqr(arr.mcmom[0][1]));
    float mcptout = sqrt(sqr(arr.mcmom[3][0]) + sqr(arr.mcmom[3][1]));
    float mcdpzdpt = arr.mcmom[3][2]/mcptout;
    
    hmcout->Fill((mcptin-mcptout)/mcptin);

    for(int iclus=0; iclus<4; ++iclus) {
      float diff = -10000000;

      if(arr.mcpos[iclus][0]<-9000) continue;

      diff = sqrt(sqr(arr.clusterpos[iclus][0] - arr.mcpos[iclus][0]) +
                  sqr(arr.clusterpos[iclus][1] - arr.mcpos[iclus][1]));
      if(atan2(arr.clusterpos[iclus][1], arr.clusterpos[iclus][0]) < atan2(arr.mcpos[iclus][1], arr.mcpos[iclus][0]))
         diff = -diff;
      hclusres[iclus]->Fill(1.e4*diff);

      diff = arr.clusterpos[iclus][2] - arr.mcpos[iclus][2];
      hclusres[iclus+4]->Fill(1.e4*diff);
    }
    
    //if((nsegments==1) && (nclusters==4)) {// && ((mcptin-mcptout)/mcptin < 0.05)) {
    if((nclusters==4) && (fabs((mcptin-mcptout)/mcptin) < 0.05)) {
      float phi = atan2(arr.clusterpos[0][1], arr.clusterpos[0][0]);
      //if(!(fabs(phi)<TMath::Pi()*3./4. && fabs(phi)>TMath::Pi()/4.)) continue;
      //if((fabs(phi)<TMath::Pi()*.8 || fabs(phi)>TMath::Pi()*.2)) continue;

      float DCA_um[3] = {0};
      for(int j=0; j<3; j++) {
        DCA_um[j] = 10000.*(pca[j]-vtx[j]);
        hdca[j]->Fill(DCA_um[j]);
      }

      hdca[3]->Fill(sqrt(sqr(DCA_um[0])+sqr(DCA_um[1])));//+sqr(DCA_um[2])));

      fill_geom_hists(arr, geomhist);

      float total_chisq = 0.;
      for(int j=0; j<NLAYERS; ++j)
        total_chisq += arr.chisq[j];
      hkfchisq->Fill(total_chisq);
      //if(chisq>10.) continue;

      int ptbin = pt_axis->FindBin(mcptout) - 1;
      //cout << mcptout << " => " << ptbin << endl;
      hkfout[ptbin]->Fill((kfpt-mcptout)/mcptout);
      hinitout[ptbin]->Fill((initpt-mcptout)/mcptout);
      hkfinit->Fill((kfpt-initpt)/initpt);
      hinitmc->Fill(initpt,mcptout);

      hkfoutz->Fill((arr.kfmom[0][2]/kfpt-mcdpzdpt)/mcdpzdpt);
      hinitoutz->Fill((arr.initmom[0][2]/initpt-mcdpzdpt)/mcdpzdpt);

      // state variables
//       float mcinvp = sqrt(sqr(mc_mom_out[0])+sqr(mc_mom_out[1])+sqr(mc_mom_out[2]));
//       float mclambda = atan2(mc_mom_out[2], mcptout);
//       float mcphi = atan2(mc_mom_out[1], mc_mom_out[0]);

      //int layer=4;
      for(unsigned int ilayer=0; ilayer<NLAYERS; ++ilayer) {
        for(unsigned int ivar=0; ivar<5; ++ivar) {
          hres[ilayer][ivar]->Fill( (arr.state[ilayer][2][ivar]-arr.statemc[ilayer][ivar]));
          hpull[ilayer][ivar]->Fill( (arr.state[ilayer][2][ivar]-arr.statemc[ilayer][ivar]) / sqrt(arr.covar[ilayer][2][ivar][ivar]) );
//         cout << "lambda(reco-MC): " << (state[2][ivar]-statemc[2][ivar]) 
//              << "  sigma_{lambda}: " << sqrt(covar[2][1][1]) << endl;
        }
      }

      // Do helix fit
      for(int j=0; j<4; j++) {
        clus_graph->SetPoint(j, arr.clusterpos[j][0], arr.clusterpos[j][1]);
        //clus_graph->SetPoint(j, mc_pos[3*j+0], mc_pos[3*j+1]);

        // float orient = cluster_orient[j];
        // float sigma = cluster_sigma[2*j];
        // clus_graph->SetPointError(j, 0., fabs(sigma/sin(orient)));

        clus_graph2->SetPoint(j, sqrt(sqr(arr.clusterpos[j][0])+sqr(arr.clusterpos[j][1])),
                              arr.clusterpos[j][2]);
        //clus_graph2->SetPointError(j, 0., cluster_sigma[2*j+1]);

//         cout << sigma << " " << atan2(cluster_pos[3*j+1],cluster_pos[3*j+0])
//              << " " << orient << endl;
      }

//       clus_graph->SetPoint(4, 0, 0);
//       clus_graph->SetPointError(4, 0, 10);

      double initpt = .5;

      double phi1 = atan2(arr.clusterpos[1][1], arr.clusterpos[1][0]);
      double a = 0.5*sqrt(sqr(arr.clusterpos[1][0])+sqr(arr.clusterpos[1][1]));
      double c = sqrt(sqr(arr.clusterpos[0][0])+sqr(arr.clusterpos[0][1]));
      double b = (a<c) ? sqrt(sqr(c)-sqr(a)) : 0.;
//       cout << "a b c: " << a << " " << b << " " << c << endl;
      double R = initpt*333./0.92;
      double x0 = a*cos(phi1)-(R-b)*sin(phi1);
      double y0 = -a*sin(phi1)+(R-b)*cos(phi1);

//       cout << "x0, y0: " << x0 << " " << y0 << endl;


      x0 = -330.*initpt*sin(phi);
      y0 = 330.*initpt*cos(phi);
      fhelix->SetParameter(0, x0);
      fhelix->SetParameter(1, y0);
      fhelix->SetParameter(2, initpt);
      if(fabs(phi)<TMath::Pi()/2)
        fhelix->FixParameter(3, -1.);
      else
        fhelix->FixParameter(3, 1.);

      if(kfcharge<0)
        fhelix->FixParameter(3, -fhelix->GetParameter(3));
      
      clus_graph->Fit(fhelix, "Q0");
      clus_graph2->Fit(fline, "Q0");
      
      float helixpt = fhelix->GetParameter(2);
      float helixdpzdpt = fline->GetParameter(1);

      //if(fhelix->GetChisquare()<5)
      hhelixout->Fill((helixpt-mcptout)/mcptout);
      hhelixoutz->Fill((helixdpzdpt-mcdpzdpt)/mcdpzdpt);

      hkfhelix->Fill( (kfpt-helixpt)/helixpt );

      hhelixchisq->Fill( fhelix->GetChisquare() );
//       if(fabs(fhelix->GetParameter(2)-0.825)<0.005) 
//         cout << "pT: " << fhelix->GetParameter(2)
//              << " chisq: " << fhelix->GetChisquare() << endl;

      if(stepthrough) {
        clus_graph->Draw("pe,same");
        fhelix->Draw("same");

        c15->Update();
        cout << "\"quit\" to exit loop... ";
        cout.flush();
        //char temp[1000];
        cin.getline(temp, 1000);
        if(strcmp(temp,"quit")==0)
          stepthrough=false;
      }
    }
  }
  cout << "Done with tree loop!" << endl;

  TCanvas* c = new TCanvas("c0", "c0", 1000, 800);
  c->Divide(2,2);
  
  c->cd(1);
  hkfout[0]->Draw();
  
  c->cd(2);
  hinitout[0]->Draw();
  
  c->cd(3);
  //hmcout->Draw();
  hhelixout->Draw();
  
  c->cd(4);
  //hkfinit->Draw();
  //hinitmc->Draw();

  hkfchisq->Draw();
  fit_chisq(hkfchisq, (4+2*4-5));
  hkfhelix->Draw("same");
  c->Print("pT.eps");

  TCanvas* c2[5] = {0};
  for(int ilayer=0; ilayer<NLAYERS; ++ilayer) {
    cout << "c2." << ilayer+1 << "..." << endl;
    sprintf(temp, "c2%d", ilayer+1);
    c2[ilayer] = new TCanvas(temp, temp, 800, 1000);
    c2[ilayer]->Divide(2,3);
    c2[ilayer]->cd(2)->SetPad(.99, .99, 1, 1);
    
    gStyle->SetOptFit(1);
    c2[ilayer]->cd(1)->SetPad(0, .66, 1, 1);
    hpull[ilayer][0]->Fit("gausn", "EMI");
    hpull[ilayer][0]->Draw();
    for(unsigned int i=1; i<5; ++i) {
      c2[ilayer]->cd(i+2);
      hpull[ilayer][i]->Fit("gausn", "EMI");
      hpull[ilayer][i]->Draw();
    }
  }

  TCanvas* c3[5] = {0};
  for(int ilayer=0; ilayer<NLAYERS; ++ilayer) {
    cout << "c3." << ilayer+1 << "..." << endl;
    sprintf(temp, "c3%d", ilayer+1);
    c3[ilayer] = new TCanvas(temp, temp, 800, 1000);
    c3[ilayer]->Divide(2,3);
    c3[ilayer]->cd(2)->SetPad(.99, .99, 1, 1);
    
    c3[ilayer]->cd(1)->SetPad(0, .66, 1, 1);
    hres[ilayer][0]->Draw();
    for(unsigned int i=1; i<5; ++i) {
      c3[ilayer]->cd(i+2);
      hres[ilayer][i]->Draw();
    }
  }

  cout << "c4..." << endl;
  TCanvas* c4 = new TCanvas("c4", "c4", 1000, 800);
  c4->Divide(2,2);
  c4->cd(1);
  hkfoutz->Draw();

  c4->cd(2);
  hinitoutz->Draw();

  c4->cd(3);
  hhelixoutz->Draw();

  c4->cd(4);
  geomhist->Draw();

  c4->Print("pz.eps");

  cout << "c5..." << endl;
  TCanvas* c5 = new TCanvas("c5", "c5", 1000, 800);
  c5->Divide(2,2);
  c5->cd(1);
  hdca[0]->Draw();

  c5->cd(2);
  hdca[1]->Draw();

  c5->cd(3);
  hdca[2]->Draw();

  c5->cd(4);
  hdca[3]->Draw();

  c5->Print("dca.eps");


  cout << "c6..." << endl;
  unsigned int nptbins = ptbins.size()-2;
  TH1F* ptres = new TH1F("ptres", "p_{T} resolution", nptbins, &(ptbins[0]));
  ptres->SetXTitle("MC p_{T} (GeV/c)");
  ptres->SetYTitle("RMS[(p_{T}^{Reco}-p_{T}^{MC})/p_{T}^{MC}]");
  ptres->SetMarkerStyle(20);
  ptres->SetMarkerColor(2);
  ptres->SetStats(0);
  ptres->GetYaxis()->SetRangeUser(0, 0.3);

  // TH1F* rptres = new TH1F("rptres", "p_{T} resolution", nptbins, &(ptbins[0]));
  // rptres->SetXTitle("MC p_{T} (GeV/c)");
  // rptres->SetYTitle("RMS[(p_{T}^{Reco}-p_{T}^{MC})/p_{T}^{MC}]");
  // rptres->SetMarkerStyle(20);
  // rptres->SetMarkerColor(8);
  // rptres->SetStats(0);
  // rptres->GetYaxis()->SetRangeUser(0, 0.3);
 
  TH1F* oldptres = new TH1F("oldptres", "p_{T} resolution", nptbins, &(ptbins[0]));
  oldptres->SetXTitle("MC p_{T} (GeV/c)");
  oldptres->SetYTitle("RMS[(p_{T}^{Reco}-p_{T}^{MC})/p_{T}^{MC}]");
  oldptres->SetMarkerStyle(20);
  oldptres->SetStats(0);
  oldptres->GetYaxis()->SetRangeUser(0, 0.3);

  TF1* lfit = new TF1("lfit", "landaun", -10, 10);
  lfit->SetParameter(0, ptres->Integral());
  lfit->SetParameter(1, 0.);
  lfit->SetParameter(2, 0.2);
  lfit->SetLineColor(2);

  TF1* gfit = new TF1("gfit", "gausn", -0.3, 0.3);
  gfit->SetParameter(0, ptres->Integral());
  gfit->SetParameter(1, 0.);
  gfit->SetParameter(2, 0.2);
  gfit->SetLineColor(2);

  TCanvas* cbig = new TCanvas("cbig", "cbig", 1200, 1000);
  cbig->Divide(4,3);

  TLatex latex;
  latex.SetNDC(1);

  for(unsigned int ipt=0; ipt<nptbins; ++ipt) {
    float fit_range[2] = {-.3, .3};
    if(ipt<1) {
      fit_range[0] = -.1;
      fit_range[1] = .2;
    }

    cbig->cd(ipt+1);
    hkfout[ipt]->Fit(gfit, "IMRQ0", "", fit_range[0], fit_range[1]);
    hkfout[ipt]->SetLineColor(2);
    hkfout[ipt]->Draw();
    hkfout[ipt]->GetXaxis()->SetRangeUser(-1,1);

    ptres->SetBinContent(ipt+1, gfit->GetParameter(2));
    ptres->SetBinError(ipt+1, gfit->GetParError(2));
    //ptres->SetBinContent(ipt+1, hkfout[ipt]->GetRMS());
    //ptres->SetBinError(ipt+1, hkfout[ipt]->GetRMSError());

    // hkfrout[ipt]->Fit(gfit, "IMRQ0", "", fit_range[0], fit_range[1]);
    // hkfrout[ipt]->SetLineColor(8);
    // hkfrout[ipt]->Draw("same");
    // rptres->SetBinContent(ipt+1, gfit->GetParameter(2));
    // rptres->SetBinError(ipt+1, gfit->GetParError(2));
    // // rptres->SetBinContent(ipt+1, hkfrout[ipt]->GetRMS());
    // // rptres->SetBinError(ipt+1, hkfrout[ipt]->GetRMSError());

    hinitout[ipt]->Fit(gfit, "IMR0Q", "", fit_range[0], fit_range[1]);
    hinitout[ipt]->Draw("same");
    oldptres->SetBinContent(ipt+1, gfit->GetParameter(2));
    //oldptres->SetBinContent(ipt+1, hinitout[ipt]->GetRMS());
    //oldptres->SetBinError(ipt+1, hinitout[ipt]->GetRMSError());
    oldptres->SetBinError(ipt+1, gfit->GetParError(2));

    sprintf(temp, "p_{T}=%.1f-%.1f", ptres->GetBinLowEdge(ipt+1), 
            ptres->GetBinLowEdge(ipt+1) + ptres->GetBinWidth(ipt+1) );
    latex.DrawLatex(0.15, 0.8, temp);
  }

  TCanvas* c6 = new TCanvas("c6", "c6", 800, 600);
  c6->cd();
  ptres->Draw("p");
  //rptres->Draw("p,same");
  oldptres->Draw("p,same");

  TCanvas* c7 = new TCanvas("c7", "c7", 1500, 1000);
  c7->Divide(4,2);

  for(int i=0; i<8; ++i) {
    c7->cd(i+1);
    hclusres[i]->Draw();
  }
  
  return;
}

void fill_geom_hists(Arrays &a, TH1* hist)
{
  static TF1* fitfunc = new TF1("linearfit", "[1]*x+[0]", 0, 20);
  static TGraph* graph = new TGraph(3);
  static int counter=0;

  double tanlam = 0;
  double r2 = 0;
  double z2 = 0;
  for(unsigned int i=0; i<4; ++i) {
    double r = sqrt(sqr(a.clusterpos[i][0])+sqr(a.clusterpos[i][1]));
    double z = a.clusterpos[i][2];
    if(i!=2)
      tanlam += r/z;
    else
      r2 = r;
    graph->SetPoint( ((i<2)?i:2), r, z);
  }
  tanlam /= 3;
  z2 = r2/tanlam;

  graph->Fit(fitfunc, "Q0");
  z2 = fitfunc->Eval(r2);

  hist->Fill(a.clusterpos[2][2] - z2);

  if(counter<10) {
    graph->Print();
    fitfunc->Print();
    ++counter;
  }

  return;
}

TF1* fit_chisq(TH1* h, float ndf)
{
  TF1* f = 0;
  gROOT->GetObject("chisq", f);
  if(!f)
    f = new TF1("chisq", "[0]*TMath::GammaDist(x/2., [1]/2.)",0, 50);

  f->SetParameter(0, h->Integral());
  f->FixParameter(1, ndf);

  f->SetLineColor(2);

  h->Fit(f, "IMQ");
  //f->Draw("Same");
  return f;
}

