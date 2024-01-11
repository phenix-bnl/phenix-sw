// $Id: Draw.C,v 1.1 2013/10/16 22:34:21 jinhuang Exp $                                                                                             

/*!
 * \file Draw.C
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.1 $
 * \date $Date: 2013/10/16 22:34:21 $
 */

#include <cmath>
#include <TFile.h>
#include <TTree.h>
#include <cassert>
#include "SaveCanvas.C"
#include "SetOKStyle.C"
using namespace std;

TFile * _file0 = NULL;
TTree * T = NULL;
TString selection_name;
void
Draw(
//    const TString infile =
//        "/direct/phenix+subsys+fvtx/jinhuang/GoldenDimuon/AlignDST_GD_DST_VTX_FVTX_VTXCor_pDST/result/ALL.singlemuon_pdst.root" //
//    const TString infile =
//        "/direct/phenix+scratch/jinhuang/miliped_work/AlignDST_GD_DST_VTXCor_pDST_read/result/ALL.singlemuon_pdst.root" //
//        const TString infile =
//            "/direct/phenix+scratch/jinhuang/miliped_work/AlignDST_GD_DST_VTXCor_pDST_read/result/ALL.dimuon_pdst.root" //
    //        const TString infile =
    //            "/direct/phenix+scratch/jinhuang/miliped_work/AlignDST_GD_DST_VTXCor_pDST_read/result/GOLDENEVENT_DIMUON_run12_online_muon-0000368789-0000.root.singlemuon_pdst.root" //
//    const TString infile =
//        "/direct/phenix+scratch/jinhuang/miliped_work/AlignDST_GD_DST_VTXCor_pDST_read/result/GOLDENEVENT_DIMUON_run12_online_muon-0000368789-0000.root.dimuon_pdst.root" //
//    const TString infile =
//        "/direct/phenix+scratch/jinhuang/miliped_work/AlignDST_GD_DST_VTXCor_pDST_read/result/GOLDENEVENT_DIMUON_run12_online_muon-0000368789-0000.root.singlemuon_pdst.root" //
//    const TString infile =
//        "/phenix/u/jinhuang/links/fvtx_tmp/miliped_work/run13_online_muon/analyze_run13_online_muon/pDST_dimuons_393000.root" //
        const TString infile =
            "/phenix/u/jinhuang/links/fvtx_tmp/miliped_work/Filter_Run13_DiMu_1/ALL_nDST.root" //
        )
{
  SetOKStyle();
  gStyle->SetOptStat(0);
  gStyle->SetOptFit(1111);
  TVirtualFitter::SetDefaultFitter("MINUIT2");

  if (!_file0)
    {
      TString chian_str = infile;
      chian_str.ReplaceAll("ALL", "*");

      TChain * t = new TChain("T");
      const int n = t->Add(chian_str);

      cout << "Loaded " << n << " root files with " << chian_str << endl;

      T = t;

      _file0 = new TFile;
      _file0->SetName(infile);
    }

  assert(_file0);

  T->SetAlias("cut_charge", "DiMuons.charge==0");
  T->SetAlias("cut_eta",
      "abs(DiMuons.rapidity) > 1.2 && abs(DiMuons.rapidity)<2.4");
  T->SetAlias("cut_pz",
      "abs(DiMuons.Tr0_pz) > 2.5 && abs(DiMuons.Tr1_pz) > 2.5");

  T->SetAlias("cut_DG0", "(DiMuons.Tr0_DG0) <20 && (DiMuons.Tr1_DG0)<20");
  T->SetAlias("cut_DDG0", "(DiMuons.Tr0_DDG0) <9 && (DiMuons.Tr1_DDG0)<9");
  T->SetAlias("cut_chi2", "Tr0_trchi2<23 && Tr1_trchi2<23");
  T->SetAlias("cut_muid",
      "( (Tr0_lastgap>=3 && Tr1_lastgap>3) || (Tr0_lastgap>3 && Tr1_lastgap>=3) ) && Tr0_idhits>8 && Tr1_idhits>8");

  T->SetAlias("asym_cut",
      "abs( (sqrt(Tr0_px * Tr0_px + Tr0_py*Tr0_py + Tr0_pz*Tr0_pz ) - sqrt(Tr1_px * Tr1_px + Tr1_py*Tr1_py + Tr1_pz*Tr1_pz ))/ (sqrt(Tr0_px * Tr0_px + Tr0_py*Tr0_py + Tr0_pz*Tr0_pz ) + sqrt(Tr1_px * Tr1_px + Tr1_py*Tr1_py + Tr1_pz*Tr1_pz )) ) < 0.75 ");

  T->SetAlias("angle_diff","fmod(atan2(Tr0_py,Tr0_px)-atan2(Tr1_py,Tr1_px)+2*pi,2*pi)");

//  const TCut event_sel =
//      "cut_eta && cut_pz && cut_DG0 && cut_DDG0 && cut_chi2 && cut_muid && asym_cut && Tr0_pz<0 && Tr1_pz<0";
//  selection_name = "_south";
//  const TCut event_sel =
//      "cut_eta && cut_pz && cut_DG0 && cut_DDG0 && cut_chi2 && cut_muid && asym_cut && Tr0_pz>0 && Tr1_pz>0";
//  selection_name = "_north";
    const TCut event_sel =
        "1";

  cout << "Build event selection of " << (const char *) event_sel << endl;

  T->Draw(">>EventList", event_sel);
  TEventList * elist = gDirectory->GetObjectChecked("EventList", "TEventList");
  cout << elist->GetN() << " / " << T->GetEntriesFast() << " events selected"
      << endl;

  T->SetEventList(elist);

//  DCA_Check();
//  DCA_Check_dimuon();

//  InvMass(true);
}

void
InvMass(bool DrawRawMass = false, bool DrawBadDCACut = false)
{

  TH1F * h_mass_sign0 = new TH1F("h_mass_sign0", "Inv. Mass; Inv. Mass (GeV)",
      240, .5, 12);
  TH1F * h_mass_signed = h_mass_sign0->Clone("h_mass_signed");

  TH1F * h_mass_fvtx_sign0 = h_mass_sign0->Clone("h_mass_fvtx_sign0");
  TH1F * h_mass_fvtx_signed = h_mass_sign0->Clone("h_mass_fvtx_signed");

  T->Project("h_mass_sign0", "DiMuons.mass", "DiMuons.charge==0");
  T->Project("h_mass_signed", "DiMuons.mass", "abs(DiMuons.charge)>0");

  T->Project("h_mass_fvtx_sign0", "DiMuons.mass_fvtx",
      "DiMuons.charge==0 && Tr0_dr_fvtx<2 && Tr1_dr_fvtx<2");
  T->Project("h_mass_fvtx_signed", "DiMuons.mass_fvtx",
      "abs(DiMuons.charge)>0 && Tr0_dr_fvtx<2 && Tr1_dr_fvtx<2");

  h_mass_sign0->Sumw2();
  h_mass_signed->Sumw2();
  h_mass_fvtx_sign0->Sumw2();
  h_mass_fvtx_signed->Sumw2();

  TH1F * h_mass_sign0_sub = h_mass_sign0->Clone("h_mass_sign0_sub");
  h_mass_sign0_sub->Add(h_mass_signed, -1);

  TH1F * h_mass_fvtx_sign0_sub = h_mass_fvtx_sign0->Clone("h_mass_sign0_sub");
  h_mass_fvtx_sign0_sub->Add(h_mass_fvtx_signed, -1);

  h_mass_sign0->SetLineColor(kMagenta + 2);
  h_mass_sign0->SetMarkerColor(kMagenta + 2);
  h_mass_sign0->SetMarkerStyle(21);
  h_mass_sign0_sub->SetLineColor(kBlue + 2);
  h_mass_sign0_sub->SetMarkerColor(kBlue + 2);
  h_mass_sign0_sub->SetMarkerStyle(20);
  h_mass_signed->SetLineColor(kRed + 2);
  h_mass_signed->SetMarkerColor(kRed + 2);
  h_mass_signed->SetMarkerStyle(22);

  h_mass_fvtx_sign0->SetLineColor(kMagenta + 2);
  h_mass_fvtx_sign0->SetMarkerColor(kMagenta + 2);
  h_mass_fvtx_sign0->SetMarkerStyle(21);
  h_mass_fvtx_sign0_sub->SetLineColor(kBlue + 2);
  h_mass_fvtx_sign0_sub->SetMarkerColor(kBlue + 2);
  h_mass_fvtx_sign0_sub->SetMarkerStyle(20);
  h_mass_fvtx_signed->SetLineColor(kRed + 2);
  h_mass_fvtx_signed->SetMarkerColor(kRed + 2);
  h_mass_fvtx_signed->SetMarkerStyle(22);

  TCanvas *c1 = new TCanvas("Draw_InvMass", "Draw_InvMass", 1800, 900);
  c1->Divide(2, 1);
  int idx = 1;
  TPad * p;

  p = (TPad *) c1->cd(idx++);
  c1->Update();

  p->SetLogy();

  h_mass_sign0->Draw();
  h_mass_signed->Draw("same");
  h_mass_sign0_sub->Draw("same");
  Fit_InvMass(h_mass_sign0_sub);

  p = (TPad *) c1->cd(idx++);
  c1->Update();

  p->SetLogy();

  h_mass_fvtx_sign0->Draw();
  h_mass_fvtx_signed->Draw("same");
  h_mass_fvtx_sign0_sub->Draw("same");
  Fit_InvMass(h_mass_fvtx_sign0_sub);

  SaveCanvas(c1,
       TString(_file0->GetName()) + TString("_")
          + TString(c1->GetName() + selection_name ), kFALSE);

}

void Fit_InvMass(TH1F * h)
{
  TString name = h->GetName();

  TF1 * f = new TF1(name + "_gaus","gaus",3.1-.2,3.1+.2);
  TF1 * f2 = new TF1(name + "_gaus_bgd","gaus + [3] + [4]*x",3.1-.7,3.1+.7);

  h -> Fit(f,"MR0");

  f2->SetParameters(f->GetParameter(0), f->GetParameter(1), f->GetParameter(2));

  h -> Fit(f2,"MR0");

  f2 -> Draw("same");
}


void
DCA_Check_dimuon()
{

}

void
DCA_Check()
{

  gStyle->SetOptStat(0);
  gStyle->SetOptFit(1111);

  assert(T);

  TCut cut = "abs(SingleMuons.pz_fvtxmutr) > 4 && SingleMuons.dtheta_fvtx>-10";

  T->SetAlias("SingleMuons_arm", "1*(SingleMuons.pz>0)");

  T->SetAlias("disp_x_kalman", "SingleMuons.x0_fvtxmutr - Evt_vtxX");
  T->SetAlias("disp_y_kalman", "SingleMuons.y0_fvtxmutr - Evt_vtxY");
  T->SetAlias("phi",
      "1*atan2(SingleMuons.py_fvtxmutr,SingleMuons.px_fvtxmutr)");

  T->SetAlias("disp_pt_vtx_kalman",
      "disp_x_kalman * cos(phi) + disp_y_kalman * sin(phi)");
  T->SetAlias("disp_lateral_vtx_kalman",
      "disp_x_kalman * cos(phi+pi/2) + disp_y_kalman * sin(phi+pi/2)");

  // DCA corrections - FVTX workshop Oct 2012
  T->SetAlias("disp_pt_vtx_kalman_cor",
      "disp_pt_vtx_kalman + 0.1201 * disp_lateral_vtx_kalman * disp_lateral_vtx_kalman");

  TCanvas *c1 = new TCanvas("DrawVTX", "DrawVTX", 1800, 900);
  c1->Divide(3, 2);
  int idx = 1;
  TPad * p;

  for (int arm = 0; arm < 2; arm++)
    {

      p = (TPad *) c1->cd(idx++);
      c1->Update();

      p->SetLogy();

      TString name = Form("disp_pt_vtx_kalman_arm%d", arm);
      TH1F * h1 = new TH1F(name, name, 300, -1, 1);
//      T->Draw("track.z0reco_no_VTX_DCA-track.vtxzp>>" + name,
//          Form("arm == %d", arm), "");
      T->Draw("disp_pt_vtx_kalman>>" + name,
          Form("SingleMuons_arm == %d", arm) && cut, "");

      TF1 * f1 = new TF1(Form("gaus1_%d", arm), "gaus", -1, 1);
      TF1 * f2 = new TF1(Form("dgaus1_%d", arm),
          "gaus + [3]*exp(-0.5*((x-[1])/[4])**2) + [5]", -1, 1);

      f1->SetLineColor(kGreen);
      f2->SetLineColor(kRed);
      f2->SetNpx(1000);

      h1->Fit(f1, "MR");

      f2->SetParameters(f1->GetParameter(0) / 2, f1->GetParameter(1),
          f1->GetParameter(2), f1->GetParameter(0) / 2, f1->GetParameter(2) / 4,
          0);

      f2->SetParName(1, "Mean");
      f2->SetParName(5, "Const");

      f2->SetParName(2, "#sigma (W)");
      f2->SetParName(4, "#sigma (N)");

      f2->SetParName(0, "Amp (W)");
      f2->SetParName(3, "Amp (N)");

      f2->SetParName(2, "#sigma (W)");
      f2->SetParName(4, "#sigma (N)");

      h1->Fit(f2, "MR");

      h1->SetTitle(
          Form(
              "Arm %d vertex diff #rightarrow Pt;vertex (Kalman fit - VTX) #upoint #hat{p_{T}} (cm)",
              arm));

      p = (TPad *) c1->cd(idx++);
      c1->Update();

      p->SetLogy();

      TString name = Form("disp_lateral_vtx_kalman_arm%d", arm);
      TH1F * h1 = new TH1F(name, name, 300, -1, 1);
//      T->Draw("track.z0reco_no_VTX_DCA-track.vtxzp>>" + name,
//          Form("arm == %d", arm), "");
      T->Draw("disp_lateral_vtx_kalman>>" + name,
          Form("SingleMuons_arm == %d", arm) && cut, "");

      TF1 * f1 = new TF1(Form("gaus2_%d", arm), "gaus", -1, 1);
      TF1 * f2 = new TF1(Form("dgaus2_%d", arm),
          "gaus + [3]*exp(-0.5*((x-[1])/[4])**2) + [5]", -1, 1);

      f1->SetLineColor(kGreen);
      f2->SetLineColor(kRed);
      f2->SetNpx(1000);

      h1->Fit(f1, "MR");

      f2->SetParameters(f1->GetParameter(0) / 2, f1->GetParameter(1),
          f1->GetParameter(2), f1->GetParameter(0) / 2, f1->GetParameter(2) / 4,
          0);

      f2->SetParName(1, "Mean");
      f2->SetParName(5, "Const");

      f2->SetParName(2, "#sigma (W)");
      f2->SetParName(4, "#sigma (N)");

      f2->SetParName(0, "Amp (W)");
      f2->SetParName(3, "Amp (N)");

      f2->SetParName(2, "#sigma (W)");
      f2->SetParName(4, "#sigma (N)");

      h1->Fit(f2, "MR");

      h1->SetTitle(
          Form(
              "Arm %d vertex diff #rightarrow Pt #times z;vertex (Kalman fit - VTX) #upoint #hat{(p_{T}#timesz)}  (cm)",
              arm));

      p = (TPad *) c1->cd(idx++);
      c1->Update();

      p->SetLogy();

      TString name = Form("disp_pt_vtx_kalman_cor_arm%d", arm);
      TH1F * h1 = new TH1F(name, name, 300, -1, 1);
//      T->Draw("track.z0reco_no_VTX_DCA-track.vtxzp>>" + name,
//          Form("arm == %d", arm), "");
      T->Draw("disp_pt_vtx_kalman_cor>>" + name,
          Form("SingleMuons_arm == %d", arm) && cut, "");

      TF1 * f1 = new TF1(Form("gaus3_%d", arm), "gaus", -1, 1);
      TF1 * f2 = new TF1(Form("dgaus3_%d", arm),
          "gaus + [3]*exp(-0.5*((x-[1])/[4])**2) + [5]", -1, 1);

      f1->SetLineColor(kGreen);
      f2->SetLineColor(kRed);
      f2->SetNpx(1000);

      h1->Fit(f1, "MR");

      f2->SetParameters(f1->GetParameter(0) / 2, f1->GetParameter(1),
          f1->GetParameter(2), f1->GetParameter(0) / 2, f1->GetParameter(2) / 4,
          0);

      f2->SetParName(1, "Mean");
      f2->SetParName(5, "Const");

      f2->SetParName(2, "#sigma (W)");
      f2->SetParName(4, "#sigma (N)");

      f2->SetParName(0, "Amp (W)");
      f2->SetParName(3, "Amp (N)");

      f2->SetParName(2, "#sigma (W)");
      f2->SetParName(4, "#sigma (N)");

      h1->Fit(f2, "MR");

      h1->SetTitle(
          Form(
              "Arm %d vertex diff #rightarrow Pt with Pt #times z correction;vertex (Kalman fit - VTX) #upoint #hat{p_{T}} + cor. (cm)",
              arm));

    }

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()),
      kFALSE);
}

//! calculate a/b with binominal error
TH1F *
GetRatio(TH1F * ha, TH1F * hb)
{
  TH1F * h = ha->Clone(TString(ha->GetName()) + "_GetRatio");

  assert(ha->GetNbinsX() == hb->GetNbinsX());

  for (int bin = 1; bin <= ha->GetNbinsX(); bin++)
    {
      const double a = ha->GetBinContent(bin);
      const double b = hb->GetBinContent(bin);

      assert(a>=0);
      assert(b>=0);
      assert(b>=a);

      if (b > 0)
        {
          const double v = a / b;
          const double err = TMath::Sqrt(v * (1 - v) / b);

          h->SetBinContent(bin, v);
          h->SetBinError(bin, err);
        }
      else
        {
          h->SetBinContent(bin, 0);
          h->SetBinError(bin, 0);
        }
    }

  return h;
}

