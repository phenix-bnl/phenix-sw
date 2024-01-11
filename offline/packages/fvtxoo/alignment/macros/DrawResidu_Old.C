/*
 * DrawResidu.C
 *
 *  Created on: Aug 11, 2012
 *      Author: jinhuang
 */

#include <cassert>
#include "SaveCanvas.C"
#include "SetOKStyle.C"
using namespace std;

void
DrawResidu(TString infile = "./FvtxGlobalAlign_eval.root")
{
  SetOKStyle();
  gStyle->SetOptStat(0);
  gStyle->SetOptFit(1111);
  TVirtualFitter::SetDefaultFitter("MINUIT2");

  DrawResidu_All(infile, 0);
//  DrawResidu_SameSec(infile, false);
//
  DrawResidu_All(infile, 1);
//  DrawResidu_SameSec(infile, true);

  DrawResidu_All(infile, 2);
//  Get1DFitDiff(infile);

  DrawVTX(infile);
}

void
DrawResidu_All(TString infile = "./FvtxGlobalAlign_eval.root",
    const int UseSpecialFit = 0)
{

  const TString prefix =
      UseSpecialFit ?
          (UseSpecialFit == 1 ? "W-Z-Fit" : "3-D_LateralConstraint") :
          "KalmanFit";
  const TString residu =
      UseSpecialFit ?
          (UseSpecialFit == 1 ? "residu_1D" : "residu_vzcon") : "residu";
  const double ylimit = UseSpecialFit >= 1 ? 400 : 100;

  TFile *_file0 = TFile::Open(infile);

  assert(_file0);

  TTree * alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  TCanvas *c1 = new TCanvas("DrawResidu_All_" + prefix,
      "DrawResidu_All_" + prefix, 1800, 900);
  c1->Divide(4, 2);
  int idx = 1;

  for (int arm = 0; arm < 2; arm++)
    for (int station = 0; station < 4; station++)
      {
        TString name = residu + Form("%1d%1d", arm, station);
        TH2F * h = new TH2F(name, name, 48, -.5, 47.5, 100, -ylimit, ylimit);

        TF1 * myfit1_1 = new TF1(name + "myfit1_1",
            "[0]+[1]*sin(-pi/2+x*3.14159/24)+[2]*cos(-pi/2+x*3.14159/24)", -.5,
            23.5);
        TF1 * myfit1_2 = new TF1(name + "myfit1_2",
            "[0]+[1]*sin(pi/2+x*3.14159/24)+[2]*cos(pi/2+x*3.14159/24)", 23.5,
            47.5);

        alignment->Project(name, residu + "*1e4*sign(w_det):sector_det",
            Form("flag==1 && arm == %d && (int(Iteration$/2)==%d )", arm,
                station));

        h->SetTitle(
            Form("Arm %d Station %d;Sector;w-Residual Meas-Fit (#mum)", arm,
                station));

        TPad * p = c1->cd(idx++);
        c1->Update();

//        h->Fit(myfit1_1, "MR"); // fit St 1 left
//        h->Fit(myfit1_2, "MR"); // fit St 1 right
        p->SetLogz();
        h->Fit("pol0");
        h->Draw("COLZ");
      }

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);
}

void
DrawResidu_SameSec(TString infile = "./FvtxGlobalAlign_eval.root",
    const bool Use1D = false)
{

  const TString prefix = Use1D ? "W-Z-Fit" : "KalmanFit";
  const TString residu = Use1D ? "residu_1D" : "residu";
  const double ylimit = Use1D ? 400 : 100;

  SetOKStyle();
  gStyle->SetOptStat(0);
  TVirtualFitter::SetDefaultFitter("MINUIT2");

  TFile *_file0 = TFile::Open(infile);

  assert(_file0);

  TTree * alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  TCanvas *c1 = new TCanvas("DrawResidu_SameSec_" + prefix,
      "DrawResidu_SameSec_" + prefix, 1800, 900);
  c1->Divide(4, 2);
  int idx = 1;

  for (int arm = 0; arm < 2; arm++)
    for (int station = 0; station < 4; station++)
      {
        TString name = residu + Form("%1d%1d", arm, station);
        TH2F * h = new TH2F(name, name, 48, -.5, 47.5, 100, -ylimit, ylimit);

        TF1 * myfit1_1 = new TF1(name + "myfit1_1",
            "[0]+[1]*sin(-pi/2+x*3.14159/24)+[2]*cos(-pi/2+x*3.14159/24)", -.5,
            23.5);
        TF1 * myfit1_2 = new TF1(name + "myfit1_2",
            "[0]+[1]*sin(pi/2+x*3.14159/24)+[2]*cos(pi/2+x*3.14159/24)", 23.5,
            47.5);

        alignment->Project(name, residu + "*1e4*sign(w_det):sector_det",
            Form(
                "flag==1 && arm == %d && int(Iteration$/2)==%d && Sum$(sector_det*sector_det*flag)/Sum$(flag)==Sum$(sector_det*flag)*Sum$(sector_det*flag)/Sum$(flag)/Sum$(flag)",
                arm, station));

        h->SetTitle(
            Form("Arm %d Station %d;Sector;w-Residual Meas-Fit (#mum)", arm,
                station));

        TPad * p = c1->cd(idx++);
        c1->Update();

//        h->Fit(myfit1_1, "MR"); // fit St 1 left
//        h->Fit(myfit1_2, "MR"); // fit St 1 right
        p->SetLogz();
        h->Draw("COLZ");

      }

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);
}

void
Get1DFitDiff(TString infile = "./FvtxGlobalAlign_eval.root")
{

  SetOKStyle();
  gStyle->SetOptStat(0);
  TVirtualFitter::SetDefaultFitter("MINUIT2");

  TFile *_file0 = TFile::Open(infile);

  assert(_file0);

  TTree * alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  TCanvas *c1 = new TCanvas("DrawResidu_Get1DFitDiff",
      "DrawResidu_Get1DFitDiff", 800, 900);
  c1->Divide(1, 3);
  int idx = 1;
  TVirtualPad * p;

  c1->Update();
  p = c1->cd(idx++);

  TH2F * hdiff =
      new TH2F("hdiff",
          "Diff between KalmanFit and W-Z-Fit;z of hit (cm);w_{KalmanFit} - w_{W-Z-Fit} (cm)",
          200, 15, 40, 1000, -.05, .05);

  alignment->Draw("(w_fit-w_fit_1D)*sign(w_det):abs(z_det)>>hdiff", "flag ",
      "colz");

  c1->Update();
  p = c1->cd(idx++);

  TProfile * hdiff_prx = hdiff->ProfileX();
  hdiff_prx->Draw();

  c1->Update();
  p = c1->cd(idx++);

  const int maxID = (1 * 4 + 3) * 4 + 3 + 1;
  TH2F * hdiff2 = new TH2F("hdiff2",
      "Diff between KalmanFit and W-Z-Fit;ID;w_{KalmanFit} - w_{W-Z-Fit} (cm)",
      maxID, -.5, maxID - .5, 1000, -.05, .05);
  alignment->Draw(
      "(w_fit-w_fit_1D)*sign(w_det):(arm*4+int(Iteration$/2))*4+fmod(sector_det,4)>>hdiff2",
      "flag ", "goff");

  TProfile * hdiff2_prx = hdiff2->ProfileX();
  hdiff2_prx->Draw();

  cout << "static const int maxID = " << maxID << " ;" << endl;

  cout << "static const double FitDiff[maxID+1] = {" << " " << endl;
  for (int i = 1; i <= hdiff2_prx->GetNbinsX(); i++)
    {
      cout << "\t" << hdiff2_prx->GetBinContent(i) << ", // +/- "
          << hdiff2_prx->GetBinError(i) << " @ ID = "
          << hdiff2_prx->GetBinCenter(i) << endl;
    }

  cout << "0};" << endl;

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);

}

void
DrawVTX(TString infile = "./FvtxGlobalAlign_eval.root")
{

  gStyle->SetOptStat(1110);

  TFile *_file0 = TFile::Open(infile);

  assert(_file0);

  TTree * alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  TCanvas *c1 = new TCanvas("DrawVTX", "DrawVTX", 1300, 900);
  c1->Divide(2, 2);
  int idx = 1;
  TPad * p ;

  for (int arm = 0; arm < 2; arm++)
    {

      p = (TPad *)c1->cd(idx++);
      c1->Update();

      TString name = Form("dispVSphi_arm%d",arm);
      TH2F * h2 = new TH2F(name, name, 48*2, 0, 2*3.14159, 100, -.5, .5);
      alignment->Draw("disp_pt_vtx_latcon:fmod(phi_acpt_cent+2*pi,2*pi)>>"+name,
          Form("arm == %d",arm), "COLZ");

      h2 -> SetTitle(Form("Arm %d displacement VS phi;central #phi (rad);vertex displacement along p_{T} (cm)",arm));

      p->SetLogz();


      p = (TPad *)c1->cd(idx++);
      c1->Update();


      TString name = Form("vertexdiff_arm%d",arm);
      TH1F * h1 = new TH1F(name, name, 300, -6, 6);
      alignment->Draw("z0reco_latcon-vtxzp>>"+name,
          Form("arm == %d",arm), "");

      TF1 * f1 = new TF1(Form("gaus_%d",arm), "gaus", -5, 5);
      TF1 * f2 = new TF1(Form("dgaus_%d",arm), "gaus + [3]*exp(-0.5*((x-[1])/[4])**2) + [5]",
          -5, 5);

      f1->SetLineColor(kGreen);
      f2->SetLineColor(kRed);
      f2->SetNpx(1000);

      h1->Fit(f1, "MR");

      f2->SetParameters(f1->GetParameter(0) / 2, f1->GetParameter(1),
          f1->GetParameter(2), f1->GetParameter(0) / 2, f1->GetParameter(2) / 2, 0);

      f2->SetParName(1,"Mean");
      f2->SetParName(2,"#sigma (W)");
      f2->SetParName(4,"#sigma (N)");

      h1->Fit(f2, "MR");


      h1 -> SetTitle(Form("Arm %d vertex diff;FVTX - VTX vertex (cm)",arm));

    }

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);
}
