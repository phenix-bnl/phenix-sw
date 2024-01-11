/*
 * DrawResidu.C
 *
 *  Created on: Aug 11, 2012
 *      Author: jinhuang
 */

#include <cassert>
#include <TGraphErrors.h>
#include "SaveCanvas.C"
#include "SetOKStyle.C"
using namespace std;

void
FitQualityCheck(TString infile = "./FvtxGlobalAlign.root")
{
  FitQualityCheck_General(infile);
  FitQualityCheck_ShiftInspection(infile);
  FitQualityCheck_ShiftInspectionInputs(infile);
  FitQualityCheck_StationInspection(infile);
}

void
FitQualityCheck_General(TString infile = "./FvtxGlobalAlign.root")
{

  SetOKStyle();
  gStyle->SetOptStat(1);
  TVirtualFitter::SetDefaultFitter("MINUIT2");

  TFile *_file0 = TFile::Open(infile);

  assert(_file0);

  TTree * misalignment = (TTree *) _file0->GetObjectChecked("misalignment",
      "TTree");
  assert(misalignment);
  misalignment->SetAlias("central_angle", "angle+3.2725e-02*(1-2*arm)");

  TCanvas *c1 = new TCanvas("FitQualityCheck_General",
      "FitQualityCheck_General", 1200, 900);
  c1->Divide(3, 2);
  int idx = 1;
  TVirtualPad * p;

  c1->Update();
  p = c1->cd(idx++);
  misalignment->Draw("nb_tracks", "error_w>0");
  p->SetLogy();

  c1->Update();
  p = c1->cd(idx++);
  misalignment->Draw("log10(error_w*1e4)", "error_w>0");
  p->SetLogy();

  c1->Update();
  p = c1->cd(idx++);
  misalignment->Draw("delta_w_millepede*1e4", "error_w>0");
  p->SetLogy();

  c1->Update();
  p = c1->cd(idx++);
  misalignment->Draw("delta_w_millepede*1e4:delta_w*1e4", "error_w>0", "*");

  c1->Update();
  p = c1->cd(idx++);
  misalignment->Draw(
      "(delta_x_millepede*cos(central_angle+pi/2)+delta_y_millepede*sin(central_angle+pi/2))*1e4>>h1",
      "half == 1 && error_w>0");
  h1->SetTitle("Sensitive Shift;Shift (#mum)");
  p->SetLogy();

  c1->Update();
  p = c1->cd(idx++);
  misalignment->Draw(
      "(delta_x_millepede*cos(central_angle)+delta_y_millepede*sin(central_angle))*1e4>>h2",
      "half == 1 && error_w>0");
  h2->SetTitle("Insensitive Shift;Shift (#mum)");
  p->SetLogy();

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);
}

void
FitQualityCheck_ShiftInspection(TString infile = "./FvtxGlobalAlign.root")
{

  SetOKStyle();
  gStyle->SetOptStat(0);
  TVirtualFitter::SetDefaultFitter("MINUIT2");

  const double ylimit = 1000;

  TFile *_file0 = TFile::Open(infile);

  assert(_file0);

  TTree * misalignment = (TTree *) _file0->GetObjectChecked("misalignment",
      "TTree");
  assert(misalignment);
  misalignment->SetAlias("central_angle", "angle+3.2725e-02*(1-2*arm)");

  TCanvas *c1 = new TCanvas("FitQualityCheck_ShiftInspection",
      "FitQualityCheck_ShiftInspection", 1800, 900);
  c1->Divide(4, 2);
  int idx = 1;
  TVirtualPad * p;

  for (int arm = 0; arm < 2; arm++)
    for (int station = 0; station < 4; station++)
      {
        c1->Update();
        p = c1->cd(idx++);

        TString name = Form("w_Shift%1d%1d", arm, station);
        TH2F * h = new TH2F(name, name, 48, -.5, 47.5, 100, -ylimit, ylimit);

        misalignment->Draw(
            "(delta_w+delta_w_millepede)*1e4:sector + cage*24>>" + name,
            Form("error_w>0 && error_w<100e-4 && half==1 && arm == %d && station==%d", arm,
                station), "COL");
      }

  cout <<"************************************************************************************"<<endl;
  cout << "Inspection for large outputs" <<endl;
  misalignment->Scan("arm:cage:station:sector:half:delta_w*1e4:(delta_w+delta_w_millepede)*1e4:error_w*1e4",
      "error_w>0 && half == 1 && (abs(delta_w+delta_w_millepede)>500e-4 || error_w>50e-4)");

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);
}

void
FitQualityCheck_ShiftInspectionInputs(TString infile = "./FvtxGlobalAlign.root")
{

  SetOKStyle();
  gStyle->SetOptStat(0);
  TVirtualFitter::SetDefaultFitter("MINUIT2");

  const double ylimit = 1000;

  TFile *_file0 = TFile::Open(infile);

  assert(_file0);

  TTree * misalignment = (TTree *) _file0->GetObjectChecked("misalignment",
      "TTree");
  assert(misalignment);
  misalignment->SetAlias("central_angle", "angle+3.2725e-02*(1-2*arm)");

  TCanvas *c1 = new TCanvas("FitQualityCheck_ShiftInspectionInputs",
      "FitQualityCheck_ShiftInspectionInputs", 1800, 900);
  c1->Divide(4, 2);
  int idx = 1;
  TVirtualPad * p;

  for (int arm = 0; arm < 2; arm++)
    for (int station = 0; station < 4; station++)
      {
        c1->Update();
        p = c1->cd(idx++);

        TString name = Form("w_Shift%1d%1d", arm, station);
        TH2F * h = new TH2F(name, name, 48, -.5, 47.5, 100, -ylimit, ylimit);
        h->SetTitle(
            Form("Arm%1d Station%1d;sector;shift in w (#mum)", arm, station));

        misalignment->Draw("(delta_w)*1e4:sector + cage*24>>" + name,
            Form("error_w>0 && half==1 && arm == %d && station==%d", arm,
                station), "COL");
      }

  cout << "Inspection for large inputs" <<endl;
  misalignment->Scan("arm:cage:station:sector:half:delta_w*1e4","abs(delta_w*1e4)>500");

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);
}

void
FitQualityCheck_StationInspection(TString infile = "./FvtxGlobalAlign.root")
{

  SetOKStyle();
  gStyle->SetOptStat(0);
  TVirtualFitter::SetDefaultFitter("MINUIT2");

//  const double ylimit = 200;
  const double ylimit = 3000;

  TFile *_file0 = TFile::Open(infile);

  assert(_file0);

  TTree * misalignment = (TTree *) _file0->GetObjectChecked("misalignment",
      "TTree");
  assert(misalignment);

  TCanvas *c1 = new TCanvas("FitQualityCheck_StationInspection",
      "FitQualityCheck_StationInspection", 1800 * .5, 900);
  c1->Divide(2, 2);
  int idx = 1;
  TVirtualPad * p;

  for (int arm = 0; arm < 2; arm++)
    {

      for (int cage = 0; cage < 2; cage++)
        {
          c1->Update();
          p = c1->cd(idx++);

          TString name = Form("w_Shift%1d%1d", arm, cage);
          TH2F * h = new TH2F(name, name, 4, -.5, 3.5, 100, -ylimit, ylimit);
          h->SetTitle(Form("Arm = %d, Cage = %d;Station;Shift (#mum)", arm, cage));
          h->Draw();
          TLine * l = new TLine(-.5, 0, 3.5, 0);
          l -> SetLineColor(kBlack);
          l -> Draw();

          int n = misalignment->Draw(
              "station:(delta_x+delta_x_millepede)*1e4:error_x*1e4",
              Form("is_fvtx_wedge == 0 &&  arm == %d && cage ==%d", arm, cage),
              "goff");

          TGraphErrors * ge = new TGraphErrors(n, misalignment->GetV1(),
              misalignment->GetV2(), 0, misalignment->GetV3());

          ge->SetLineColor(kRed);
          ge->SetMarkerColor(kRed);


          ge->Draw("ep*");

          int n = misalignment->Draw(
              "station+.2:(delta_y+delta_y_millepede)*1e4:error_y*1e4",
              Form("is_fvtx_wedge == 0 &&  arm == %d && cage ==%d", arm, cage),
              "goff");

          TGraphErrors * ge = new TGraphErrors(n, misalignment->GetV1(),
              misalignment->GetV2(), 0, misalignment->GetV3());

          ge->SetLineColor(kBlue);
          ge->SetMarkerColor(kBlue);

          ge->Draw("ep*");

        }

    }

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);
}

