/*
 * DrawResidu.C
 *
 *  Created on: Aug 11, 2012
 *      Author: jinhuang
 */

#include <cmath>
#include <TFile.h>
#include <TTree.h>
#include <cassert>
#include "SaveCanvas.C"
#include "SetOKStyle.C"
using namespace std;

TFile * _file0 = NULL;
TTree * alignment = NULL;
TString select_name = "";

void
Do_DrawResidu()
{
  DrawResidu();
}

void
//DrawResidu(TString infile = "./FvtxGlobalAlign_AlignDST.root")
//DrawResidu(TString infile =
//    "DST/ZEROFDATA_P00-ALL.PRDFF_AlignDST.root")
//DrawResidu(TString infile =
//    "data/fvtx_data/zero_field/fileter_MuTrVTX/AlignDST_1/ALL.AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+subsys+fvtx/jinhuang/zero_field/fileter_MuTrVTX/AlignDST_3_MuTr/result//ALL.AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+subsys+fvtx/jinhuang/GoldenDimuon/AlignDST_GD1/result//ALL.AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+subsys+fvtx/jinhuang/GoldenDimuon/AlignDST_GD_DST/result/ALL.AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+subsys+fvtx/jinhuang/GoldenDimuon/AlignDST_GD_DST_MuTr_VTXCor/result/ALL.AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+subsys+fvtx/jinhuang/GoldenDimuon/AlignDST_GD_DST_VTX_FVTX_VTXCor/result/ALL.AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+subsys+fvtx/jinhuang/GoldenDimuon/AlignDST_GD_DST_MuTr/result/ALL.AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+subsys+fvtx/jinhuang/GoldenDimuon/AlignDST_GD_DST_VTX/result/ALL.AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+scratch/jinhuang/miliped_work/AlignDST_GD_DST_VTX_FVTX_VTXCor_Test2/result/ALL.AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+subsys+fvtx/jinhuang/GoldenDimuon/AlignDST_GD_DST_VTX_FVTX_VTXCor_pDST/result/ALL.AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+user05/phnxreco/FVTX_Jin/jinhuang/miliped_work/Filter_MuTr/ZEROFDATA_P00-ALL.PRDFF.root_AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+user05/phnxreco/FVTX_Jin/jinhuang/miliped_work/Filter_Run13_Mu_1/GLOBAL_ALIGN_PRDF_run13_zf_online_fvtx-ALL.PRDFF_AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+user05/phnxreco/FVTX_Jin/jinhuang/miliped_work/Filter_Run13_Mu_2/GLOBAL_ALIGN_PRDF_run13_zf_online_fvtx-ALL.PRDFF_AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+user05/phnxreco/FVTX_Jin/jinhuang/miliped_work/Filter_Run13_Mu_4/GLOBAL_ALIGN_PRDF_run13_zf_online_fvtx-ALL.PRDFF_AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+user05/phnxreco/FVTX_Jin/jinhuang/miliped_work/Filter_Run13_Mu_5_UpdatedRMuTr/GLOBAL_ALIGN_PRDF_run13_zf_online_fvtx-ALL.PRDFF_AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+user05/phnxreco/FVTX_Jin/jinhuang/miliped_work/Filter_Run13_Mu_MuTrAloneTrk_2_UpdatedRMuTr/GLOBAL_ALIGN_PRDF_run13_zf_online_fvtx-ALL.PRDFF_AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+user05/phnxreco/FVTX_Jin/jinhuang/miliped_work/Filter_Run13_Mu_MuTrAloneTrk_1/ALL_AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/phenix/u/jinhuang/links/fvtx_tmp/miliped_work/Simulation/MB_ZF_MuonTrig_VRMS10_Reco_1.ZRef80cm/MB_ZF_MuonTrig_VRMS10_ALL_PISAEvent.root_AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/phenix/u/jinhuang/links/fvtx_tmp/miliped_work/Filter_MB_ZF_MuonTrig_MuTrAloneTrk/MB_ZF_MuonTrig_VRMS10_ALL_PISAEvent.root_AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/phenix/u/jinhuang/links/fvtx_tmp/miliped_work/Filter_Run13_HighPz_FieldON_1/ALL_AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/phenix/u/jinhuang/links/fvtx_tmp/miliped_work/Filter_Run13_HighPz_FieldON_2/ALL_AlignDST.root")
//    DrawResidu(
//    TString infile =
//        "/phenix/u/jinhuang/links/fvtx_tmp/miliped_work/Filter_Run13_DiMu_FieldON_1/ALL_AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/phenix/u/jinhuang/links/fvtx_tmp/miliped_work/Filter_MB_ZF_MuonTrig/MB_ZF_MuonTrig_ALL_PISAEvent.root_AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+user05/phnxreco/FVTX_Jin/jinhuang/miliped_work/Filter_MB_ZF_MuonTrig/ALL_AlignDST.root")
//DrawResidu(
//    TString infile =
////        "/direct/phenix+user05/phnxreco/FVTX_Jin/jinhuang/miliped_work/Filter_MB_ZF_MuonTrig_MuTrAloneTrk/ALL_AlignDST.root")
//DrawResidu(
//    TString infile =
//        "/direct/phenix+user05/phnxreco/FVTX_Jin/jinhuang/miliped_work/Filter_Run13_HighPz_FieldON_NIM/ALL_AlignDST.root")
//DrawResidu(
//    TString infile = "FvtxGlobalAlign_AlignDST_SimJPsi_VTXIdeal.root")
//DrawResidu(
//    TString infile = "DST/ALL_AlignDST.root")
//DrawResidu(
//    TString infile = "DST/ZEROFDATA_P00-0000386885-0000.PRDFF_AlignDST.root")
//DrawResidu(
//    TString infile = "data/ALL_AlignDST.root")
DrawResidu(
    TString infile = "data/ALL_DCAMode_AlignDST.root")
//DrawResidu(
//    TString infile = "data/ALL_NonDCAMode_AlignDST.root")
//DrawResidu(
//    TString infile = "/phenix/u/jinhuang/work/FVTX/miliped_work/Simulation/PISA_PartGun/PISAEvent_1-15GeV_mu+-_10cmVzRMS.root_FvtxGlobalAlign_AlignDST.root")
//DrawResidu(
//    TString infile = "/phenix/u/jinhuang/work/FVTX/miliped_work/Simulation/PISA_PartGun/PISAEvent_1-15GeV_mu+-_10cmVzRMS.root_AlignDST_IncMuTrFit.root")
{
  SetOKStyle();
  gStyle->SetOptStat(0);
  gStyle->SetOptFit(0111);
  TVirtualFitter::SetDefaultFitter("Minuit2");

  gSystem->Load("libfvtxoo_alignment");

  if (!_file0)
    {
//    _file0 = TFile::Open(infile);
      TChain * T = new TChain("alignment");
      TString chian_str = infile;
      chian_str.ReplaceAll("ALL", "*");

      const int n = T->Add(chian_str);
      cout << "Loaded " << n << " root files with " << chian_str << endl;

      assert(n>0);
      _file0 = new TFile();
      alignment = T;
    }

  assert(_file0);

  if (!alignment)
    alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  //-----------------------------------------------------------
  // Event switches
  //-----------------------------------------------------------

//  const TCut event_sel =
//      "track.reject_track == 0 && track.nNodes_MuTr>=11 && track.chi_square_mutr/track.ndf_mutr<30";
//  select_name = "_CrossOct_mutr";
//  const TCut event_sel =
//      "track.reject_track == 0 && track.nNodes_MuTr>=11 && track.chi_square_mutr/track.ndf_mutr<30 && fmod(Sum$(track.Nodes_MuTr.octant*2 + track.Nodes_MuTr.half_octant)/Length$(track.Nodes_MuTr.octant), 1)>0";
//  const TCut event_sel = "(Sum$(track.Nodes_FVTX.cage*track.Nodes_FVTX.cage) - Sum$(track.Nodes_FVTX.cage)*Sum$(track.Nodes_FVTX.cage)/track.nNodes_FVTX)>0";
//  const TCut event_sel = "track.nNodes_VTX > 0 && track.nNodes_VTX<=4 && track.nNodes_MuTr == 0 && track.nNodes_FVTX >=4";
//  const TCut event_sel = "track.nNodes_VTX > 0 && track.nNodes_MuTr>6 && track.nNodes_FVTX >=4";
//    const TCut event_sel = "track.nNodes_VTX == 0";
//  const TCut event_sel = "track.reject_track == 0&& track.nNodes_VTX > 0";
  //  const TCut event_sel = "track.reject_track == 0&& track.nNodes_VTX == 0";
//  const TCut event_sel = "track.nNodes_MuTr == 0&& track.nNodes_VTX == 0";
//  const TCut event_sel = "track.reject_track == 0 && track.nNodes_MuTr == 0 && track.nNodes_VTX == 0";
  select_name = "_good_trk";
  const TCut event_sel =
      "track.reject_track == 0";
//  select_name = "_good_trk4_station";
//  const TCut event_sel =
//      "track.reject_track == 0 && track.fvtx_one_hit_per_station(0,0) && track.muid_lastgap==4 && track.nNodes_Constraint==2 && abs(track.trk_par_kalman_mutr_zref._pz)<5";
//  select_name = "_good_trk4_station_highp";
//  const TCut event_sel =
//      "track.reject_track == 0 && track.fvtx_one_hit_per_station(0,0) && abs(track.trk_par_kalman_mutr._pz)>7";
//  select_name = "_good_trk4_station_highp_Zf";
//  const TCut event_sel =
//      "track.reject_track == 0 && track.fvtx_one_hit_per_station(0,0) && track.muid_lastgap==4 && track.nNodes_Constraint==2 && abs(track.mutr_disp_pt_z_ref - -3.42139e-01)<2.35496e+00*.5 && abs(track.mutr_disp_lateral_z_ref - 2.89630e-02)<1.77377e+00*.5 && abs(track.mutr_disp_tangent_theta - 3.12159e-03)<4.15889e-02*.5";
////  select_name = "_good_trk_vtx";
//  const TCut event_sel =
//      "track.reject_track == 0 && track.nNodes_VTX ==2 && track.nNodes_FVTX>=4";
//  select_name = "_valid_vtx_good_trk";
//  const TCut event_sel =
//      "track.reject_track == 0 && abs(abs(track.vtxzp)<10)<10 && track.nNodes_FVTX >= 4";

  return;

  cout << "Build event selection of " << (const char *) event_sel << endl;

  alignment->Draw(">>EventList", event_sel);
  TEventList * elist = gDirectory->GetObjectChecked("EventList", "TEventList");
  assert(elist);
  cout << elist->GetN() << " / " << alignment->GetEntriesFast()
      << " events selected" << endl;

  alignment->SetEventList(elist);

  //-----------------------------------------------------------
  // Function switches
  //-----------------------------------------------------------

  cout << "Plotting ..." << endl;

//  DrawResidu_All(infile, 0); // Kalman fit
//  DrawResidu_All(infile, 1); // W-Z fit
  DrawResidu_All(infile, 2); // 3D - lateral constraint fits
//  DrawResidu_RZDiff(infile,0);
//  DrawResidu_RZDiff(infile,1);
//  DrawResidu_RZDiff(infile,1,"X");
//  DrawResidu_RZDiff(infile,1,"Y");
//  DrawResidu_Proj(infile); // 3D - lateral constraint fits, projection then calculate residual
//  DrawResidu_All_Scale(infile, 2); // 3D - lateral constraint fits
//
//  DrawResidu_MuTr(infile + select_name, 0, 0);
//  DrawResidu_MuTr(infile + select_name, 0, 1);
//  DrawResidu_MuTr(infile + select_name, 1, 0);
//  DrawResidu_MuTr(infile + select_name, 1, 1);
////
//  DrawZRef_MuTr(infile, 0, 1);
//  DrawZRef_MuTr(infile, 0);

////  DrawResidu_SameSec(infile, false);
////
//  DrawResidu_All(infile, 1); // 1D fit
////  DrawResidu_SameSec(infile, true);
//

  DrawVTX(infile);
  DrawVTX_Kalman(infile);
//////
//  DrawResidu_VTX(infile, 0, 0);
//  DrawResidu_VTX(infile, 0, 1);
//  DrawResidu_VTX(infile, 1, 0);
//  DrawResidu_VTX(infile, 1, 1);

//  Get1DFitDiff(infile);

//  DrawVTX_MuTr(infile);
//  DrawZRef_MuTr(infile, 1);
//  DrawMuTr_Azimutthal(infile);

//  DrawZRefdR_MuTr(infile, 0);
//  DrawZRefdR_MuTr(infile, 1);

//  FVTXFitCheck(infile);

}

void
DrawResidu_All(TString infile = "./FvtxGlobalAlign_AlignDST.root",
    const int UseSpecialFit = 0)
{

  const TString prefix =
      UseSpecialFit ?
          (UseSpecialFit == 1 ? "W-Z-Fit" : "3-D_LateralConstraint") :
          "KalmanFit";
  const TString residu =
      UseSpecialFit ?
          (UseSpecialFit == 1 ?
              "track.Nodes_FVTX.residu_1D" : "track.Nodes_FVTX.residu") :
          "track.Nodes_FVTX.residu_kalman";
  const double ylimit = UseSpecialFit >= 1 ? 400 : 100;

  TH1F * h_mean_sum = new TH1F("h_mean_sum",
      "Summary for means;Residual Mean Fit (#mum)", 100, -30, 30);

  if (!_file0)
    _file0 = TFile::Open(infile);

  assert(_file0);

  if (!alignment)
    alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  TCanvas *c1 = new TCanvas("DrawResidu_All_" + prefix,
      "DrawResidu_All_" + prefix, 1800, 900);
  c1->Divide(4, 2);
  int idx = 1;

  TCanvas *c2 = new TCanvas("DrawResidu_All_Proj_" + prefix,
      "DrawResidu_All_Proj_" + prefix, 1800, 900);
  c2->Divide(4, 2);
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
        TF1 * myfit1_3 = new TF1(name + "_Gauss_Res_Fit", "gaus", -100, 100);
        myfit1_3->SetNpx(1000);

        alignment->Project(name,
            residu
                + "*1e4*sign(track.Nodes_FVTX.meas):(track.Nodes_FVTX.sector + track.Nodes_FVTX.cage * 24)",
            Form(
                "track.Nodes_FVTX.arm == %d && (track.Nodes_FVTX.station==%d )",
                arm, station));

        h->FitSlicesY();
        TH1D * h_mean = (TH1D *) gDirectory->GetObjectChecked(name + "_1",
            "TH1D");
        assert(h_mean);
        TH1D * h_proj = h->ProjectionY(name + "_ProjY");
        assert(h_proj);

        TProfile * h_prof = h->ProfileX();
        h_prof->SetLineColor(kMagenta);
        assert(h_prof);

        h->SetTitle(
            Form("Arm %d Station %d;Sector;w-Residual Meas-Fit (#mum)", arm,
                station));

        TPad * p = c1->cd(idx);
        c1->Update();

//        h->Fit(myfit1_1, "MR"); // fit St 1 left
//        h->Fit(myfit1_2, "MR"); // fit St 1 right
        p->SetLogz();
        h->Fit("pol0");
        h->Draw("COLZ");
        h_mean->Draw("same");
//        h_prof -> Draw("same");

        for (int bin = 1; bin <= h_mean->GetNbinsX(); bin++)
          {
            if (h_mean->GetBinError(bin) != 0)
              h_mean_sum->Fill(h_mean->GetBinContent(bin));
          }

        c2->cd(idx);
        c2->Update();
//        return;

        h_proj->Draw();
        h_proj->Fit(myfit1_3, "R");
        h_proj->SetTitle(
            Form("Arm %d Station %d;w-Residual (#mum)", arm, station));

        idx++;
      }

  SaveCanvas(c1,
      TString(infile) + select_name + TString("_") + TString(c1->GetName()),
      kFALSE);
  SaveCanvas(c2,
      TString(infile) + select_name + TString("_") + TString(c2->GetName()));

  TCanvas *c1 = new TCanvas("DrawResidu_All_Sum_" + prefix,
      "DrawResidu_All_Sum_" + prefix, 800, 700);

  h_mean_sum->Draw();
  h_mean_sum->Fit("gaus");

  SaveCanvas(c1,
      TString(infile) + select_name + TString("_") + TString(c1->GetName()),
      kFALSE);
}

void
DrawResidu_All_Scale(TString infile = "./FvtxGlobalAlign_AlignDST.root",
    const int UseSpecialFit = 0)
{

  const TString prefix =
      UseSpecialFit ?
          (UseSpecialFit == 1 ? "W-Z-Fit" : "3-D_LateralConstraint") :
          "KalmanFit";
  const TString residu =
      UseSpecialFit ?
          (UseSpecialFit == 1 ?
              "track.Nodes_FVTX.residu_1D" : "track.Nodes_FVTX.residu") :
          "track.Nodes_FVTX.residu_kalman";
  const double ylimit = UseSpecialFit >= 1 ? 400 : 100;

  const double scale_down[4] =
    { 0.5484, 0.8359, 0.8359, 0.5484 };

  TH1F * h_mean_sum = new TH1F("h_mean_sum",
      "Summary for means;Residual Mean Fit (#mum)", 100, -30, 30);

  if (!_file0)
    _file0 = TFile::Open(infile);

  assert(_file0);

  if (!alignment)
    alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  TCanvas *c1 = new TCanvas("DrawResidu_All_Scale_" + prefix,
      "DrawResidu_All_Scale_" + prefix, 1800, 900);
  c1->Divide(4, 2);
  int idx = 1;

  TCanvas *c2 = new TCanvas("DrawResidu_All_Scale_Proj_" + prefix,
      "DrawResidu_All_Scale_Proj_" + prefix, 1800, 900);
  c2->Divide(4, 2);
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
        TF1 * myfit1_3 = new TF1(name + "_Gauss_Res_Fit", "gaus", -50, 50);
        myfit1_3->SetNpx(1000);

        alignment->Project(name,
            residu
                + Form(
                    "/%f*1e4*sign(track.Nodes_FVTX.meas):(track.Nodes_FVTX.sector + track.Nodes_FVTX.cage * 24)",
                    scale_down[station]),
            Form(
                "track.Nodes_FVTX.arm == %d && (track.Nodes_FVTX.station==%d )",
                arm, station));

        h->FitSlicesY();
        TH1D * h_mean = (TH1D *) gDirectory->GetObjectChecked(name + "_1",
            "TH1D");
        assert(h_mean);
        TH1D * h_proj = h->ProjectionY(name + "_ProjY");
        assert(h_proj);

        TProfile * h_prof = h->ProfileX();
        h_prof->SetLineColor(kMagenta);
        assert(h_prof);

        h->SetTitle(
            Form("Arm %d Station %d;Sector;w-Residual Meas-Fit (#mum)", arm,
                station));

        TPad * p = c1->cd(idx);
        c1->Update();

//        h->Fit(myfit1_1, "MR"); // fit St 1 left
//        h->Fit(myfit1_2, "MR"); // fit St 1 right
        p->SetLogz();
        h->Fit("pol0");
        h->Draw("COLZ");
        h_mean->Draw("same");
//        h_prof -> Draw("same");

        for (int bin = 1; bin <= h_mean->GetNbinsX(); bin++)
          {
            if (h_mean->GetBinError(bin) != 0)
              h_mean_sum->Fill(h_mean->GetBinContent(bin));
          }

        c2->cd(idx);
        c2->Update();
//        return;

        h_proj->Draw();
        h_proj->Fit(myfit1_3, "R");
        h_proj->SetTitle(
            Form("Arm %d Station %d;w-Residual (#mum)", arm, station));

        idx++;
      }

  SaveCanvas(c1,
      TString(infile) + select_name + TString("_") + TString(c1->GetName()),
      kFALSE);
  SaveCanvas(c2,
      TString(infile) + select_name + TString("_") + TString(c2->GetName()));

  TCanvas *c1 = new TCanvas("DrawResidu_All_Sum_" + prefix,
      "DrawResidu_All_Sum_" + prefix, 800, 700);

  h_mean_sum->Draw();
  h_mean_sum->Fit("gaus");

  SaveCanvas(c1,
      TString(infile) + select_name + TString("_") + TString(c1->GetName()),
      kFALSE);
}

void
DrawResidu_Proj(TString infile = "./FvtxGlobalAlign_AlignDST.root")
{

  const TString prefix = "3-D_LateralConstraint";
  const double ylimit = 400;

  const double scale_down[4] =
    { TMath::Sqrt(10. / 3.), (2. * TMath::Sqrt(17)) / 7., (2. * TMath::Sqrt(17))
        / 7., TMath::Sqrt(10. / 3.) };
  const char * s_scale_down[4] = {
      "#sqrt{3/10}",
      "7/2/#sqrt{17}",
      "7/2/#sqrt{17}",
      "#sqrt{3/10}"
  };

  TH1F * h_mean_sum = new TH1F("h_mean_sum",
      "Summary for means;Residual Mean Fit (#mum)", 100, -30, 30);

  if (!_file0)
    _file0 = TFile::Open(infile);

  assert(_file0);

  if (!alignment)
    alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  TCanvas *c1 = new TCanvas("DrawResidu_Proj_" + prefix,
      "DrawResidu_Proj_" + prefix, 1800, 900);
  c1->Divide(4, 2);
  int idx = 1;

  TCanvas *c2 = new TCanvas("DrawResidu_Proj_Proj_" + prefix,
      "DrawResidu_Proj_Proj_" + prefix, 1800, 900);
  c2->Divide(4, 2);
  int idx = 1;

  for (int arm = 0; arm < 2; arm++)
    for (int station = 0; station < 4; station++)
      {
        TString name = Form("h%1d%1d", arm, station);
        TH2F * h = new TH2F(name, name, 48, -.5, 47.5, 100, -ylimit, ylimit);

        TF1 * myfit1_1 = new TF1(name + "myfit1_1",
            "[0]+[1]*sin(-pi/2+x*3.14159/24)+[2]*cos(-pi/2+x*3.14159/24)", -.5,
            23.5);
        TF1 * myfit1_2 = new TF1(name + "myfit1_2",
            "[0]+[1]*sin(pi/2+x*3.14159/24)+[2]*cos(pi/2+x*3.14159/24)", 23.5,
            47.5);
        TF1 * myfit1_3 = new TF1(name + "_Gauss_Res_Fit", "gaus", -50, 50);
        myfit1_3->SetNpx(1000);

        TString draw =
            Form(
                "track.interanl_fit_FVTX_self_proj(%d)*1e4*sign(track.Nodes_FVTX.meas)/%f:(track.Nodes_FVTX[0].sector+ track.Nodes_FVTX.cage * 24)",
                station, scale_down[station]);
        alignment->Project(name, draw,
            Form("track.Nodes_FVTX.arm == %d ", arm));

        h->FitSlicesY();
        TH1D * h_mean = (TH1D *) gDirectory->GetObjectChecked(name + "_1",
            "TH1D");
        assert(h_mean);
        TH1D * h_proj = h->ProjectionY(name + "_ProjY");
        assert(h_proj);

        TProfile * h_prof = h->ProfileX();
        h_prof->SetLineColor(kMagenta);
        assert(h_prof);

        h->SetTitle(
            Form("Arm %d Station %d;Sector;w-Residual Meas-Fit (#mum)", arm,
                station));

        TPad * p = c1->cd(idx);
        c1->Update();

//        h->Fit(myfit1_1, "MR"); // fit St 1 left
//        h->Fit(myfit1_2, "MR"); // fit St 1 right
        p->SetLogz();
        h->Fit("pol0");
        h->Draw("COLZ");
        h_mean->Draw("same");
//        h_prof -> Draw("same");

        for (int bin = 1; bin <= h_mean->GetNbinsX(); bin++)
          {
            if (h_mean->GetBinError(bin) != 0)
              h_mean_sum->Fill(h_mean->GetBinContent(bin));
          }

        c2->cd(idx);
        c2->Update();
//        return;

        h_proj->Draw();
        h_proj->Fit(myfit1_3, "R");
        h_proj->SetTitle(
            Form("Arm %d Station %d;(Projection Residual) #times %s (#mum)", arm, station, s_scale_down[station]));

        idx++;
      }

  SaveCanvas(c1,
      TString(infile) + select_name + TString("_") + TString(c1->GetName()),
      kFALSE);
  SaveCanvas(c2,
      TString(infile) + select_name + TString("_") + TString(c2->GetName()));

}

void
DrawResidu_RZDiff(TString infile = "./FvtxGlobalAlign_AlignDST.root",
    const int UseSpecialFit = 0, const TString Var = "Mag")
{

  const TString prefix = (UseSpecialFit ? "PhiConstFit_" : "StripCenter_")
      + Var;
  const TString point = UseSpecialFit ? "p_fit" : "p_det";
  const double ylimit = 400;

  TH1F * h_mean_sum = new TH1F("h_mean_sum",
      "Summary for means;Residual Mean Fit (#mum)", 100, -30, 30);

  if (!_file0)
    _file0 = TFile::Open(infile);

  assert(_file0);

  if (!alignment)
    alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  alignment->SetAlias("R0",
      "Sum$((track.Nodes_FVTX[].station==0)*(track.Nodes_FVTX[]." + point + "."
          + Var + "()))/Sum$(track.Nodes_FVTX[].station==0)");
  alignment->SetAlias("Z0",
      "Sum$((track.Nodes_FVTX[].station==0)*(track.Nodes_FVTX[]." + point
          + ".fZ))/Sum$(track.Nodes_FVTX[].station==0)");

  alignment->SetAlias("R1",
      "Sum$((track.Nodes_FVTX[].station==1)*(track.Nodes_FVTX[]." + point + "."
          + Var + "()))/Sum$(track.Nodes_FVTX[].station==1)");
  alignment->SetAlias("Z1",
      "Sum$((track.Nodes_FVTX[].station==1)*(track.Nodes_FVTX[]." + point
          + ".fZ))/Sum$(track.Nodes_FVTX[].station==1)");

  alignment->SetAlias("R2",
      "Sum$((track.Nodes_FVTX[].station==2)*(track.Nodes_FVTX[]." + point + "."
          + Var + "()))/Sum$(track.Nodes_FVTX[].station==2)");
  alignment->SetAlias("Z2",
      "Sum$((track.Nodes_FVTX[].station==2)*(track.Nodes_FVTX[]." + point
          + ".fZ))/Sum$(track.Nodes_FVTX[].station==2)");

  alignment->SetAlias("R3",
      "Sum$((track.Nodes_FVTX[].station==3)*(track.Nodes_FVTX[]." + point + "."
          + Var + "()))/Sum$(track.Nodes_FVTX[].station==3)");
  alignment->SetAlias("Z3",
      "Sum$((track.Nodes_FVTX[].station==3)*(track.Nodes_FVTX[]." + point
          + ".fZ))/Sum$(track.Nodes_FVTX[].station==3)");

  cout << "R0 = " << alignment->GetAlias("R0") << endl;

  TCanvas *c1 = new TCanvas("DrawResidu_RZDiff_" + prefix,
      "DrawResidu_RZDiff_" + prefix, 1800, 900);
  c1->Divide(4, 2);
  int idx = 1;

  for (int arm = 0; arm < 2; arm++)
    for (int station = 1; station < 3; station++)
      {
        TString name = Form("RZDiff_%1d%1d", arm, station);
        TH2F * h = new TH2F(name, name, 48, -.5, 47.5, 100, -ylimit, ylimit);

        TString draw = Form("(R%d+(R%d-R%d)/(Z%d-Z%d)*(Z%d-Z%d)-R%d)*1e4", //
            station - 1, //
            station + 1, station - 1, //
            station + 1, station - 1, //
            station, station - 1, //
            station);

        cout << "Drawing " << draw << endl;

        alignment->Project(name,
            draw
                + TString(
                    ":(track.Nodes_FVTX[0].sector + track.Nodes_FVTX[0].cage * 24)"),
            Form(
                "track.Nodes_FVTX[0].arm == %d && (track.Nodes_FVTX.station==%d )",
                arm, station));

        h->FitSlicesY();

        TH1D * h_mean = (TH1D *) gDirectory->GetObjectChecked(name + "_1",
            "TH1D");
        assert(h_mean);

        TProfile * h_prof = h->ProfileX();
        h_prof->SetLineColor(kMagenta);
        assert(h_prof);

        h->SetTitle(
            Form("Arm %d Station %d;Sector;R Project - R Meas", arm, station));

        TPad * p = c1->cd(idx++);
        c1->Update();

//        h->Fit(myfit1_1, "MR"); // fit St 1 left
//        h->Fit(myfit1_2, "MR"); // fit St 1 right
        p->SetLogz();
        h_mean->Fit("pol0");
        h->Draw("COLZ");
        h_mean->Draw("same");
//        h_prof -> Draw("same");
//        return;

        c1->cd(idx++);
        c1->Update();

        h->ProjectionY()->Draw();
      }

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);

}

void
DrawResidu_SameSec(TString infile = "./FvtxGlobalAlign_AlignDST.root",
    const bool Use1D = false)
{

  const TString prefix = Use1D ? "W-Z-Fit" : "KalmanFit";
  const TString residu =
      Use1D ? "track.Nodes_FVTX.residu_1D" : "track.Nodes_FVTX.residu_kalman";
  const double ylimit = Use1D ? 400 : 100;

  SetOKStyle();
  gStyle->SetOptStat(0);
  TVirtualFitter::SetDefaultFitter("Minuit2");

  if (!_file0)
    _file0 = TFile::Open(infile);

  assert(_file0);

  if (!alignment)
    alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

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

        alignment->Project(name,
            residu
                + "*1e4*sign(track.Nodes_FVTX.meas):(track.Nodes_FVTX.sector + track.Nodes_FVTX.cage * 24)",
            Form(
                "track.Nodes_FVTX.arm == %d && track.Nodes_FVTX.station==%d && Sum$((track.Nodes_FVTX.sector + track.Nodes_FVTX.cage * 24)*(track.Nodes_FVTX.sector + track.Nodes_FVTX.cage * 24))/track.nNodes_FVTX==Sum$((track.Nodes_FVTX.sector + track.Nodes_FVTX.cage * 24))*Sum$((track.Nodes_FVTX.sector + track.Nodes_FVTX.cage * 24))/track.nNodes_FVTX/track.nNodes_FVTX",
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
Get1DFitDiff(TString infile = "./FvtxGlobalAlign_AlignDST.root")
{

  SetOKStyle();
  gStyle->SetOptStat(0);
  TVirtualFitter::SetDefaultFitter("Minuit2");

  if (!_file0)
    _file0 = TFile::Open(infile);

  assert(_file0);

  if (!alignment)
    alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

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

  alignment->Draw(
      "(track.Nodes_FVTX.fit_kalman-track.Nodes_FVTX.w_fit_1D)*sign(track.Nodes_FVTX.meas):abs(track.Nodes_FVTX.p_det.fZ)>>hdiff",
      "1", "colz");

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
      "(track.Nodes_FVTX.fit_kalman-track.Nodes_FVTX.w_fit_1D)*sign(track.Nodes_FVTX.meas):(track.Nodes_FVTX.arm*4+track.Nodes_FVTX.station)*4+fmod((track.Nodes_FVTX.sector + track.Nodes_FVTX.cage * 24),4)>>hdiff2",
      "1", "goff");

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
DrawVTX(TString infile = "./FvtxGlobalAlign_AlignDST.root")
{

  gStyle->SetOptStat(0);
  gStyle->SetOptFit(1111);

  if (!_file0)
    _file0 = TFile::Open(infile);

  assert(_file0);

  if (!alignment)
    alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  TCanvas *c1 = new TCanvas("DrawVTX", "DrawVTX", 1300, 900);
  c1->Divide(2, 2);
  int idx = 1;
  TPad * p;

  TF1 * fmods[100] =
    { NULL };

  for (int arm = 0; arm < 2; arm++)
    {

      p = (TPad *) c1->cd(idx++);
      c1->Update();

      TString name = Form("dispVSphi_arm%d", arm);
      TH2F * h2 = new TH2F(name, name, 48 * 2, 0, 2 * 3.14159, 100, -.5, .5);
      alignment->Draw(
          "track.disp_pt_vtx:fmod(track.phi_acpt_cent+2*pi,2*pi)>>" + name,
          Form("track.Nodes_FVTX.arm[0] == %d", arm), "goff");

      h2->SetTitle(
          Form(
              "Arm %d displacement VS phi;central #phi (rad);vertex displacement along p_{T} (cm)",
              arm));

      TGraphErrors * ge = FitProfile(h2);
      ge->SetMarkerStyle(20);

      TF1 * fmod = new TF1("fmod", "cos(x) ++ sin(x)", 0, 2 * 3.14159);
      ge->Fit(fmod, "M");
      fmods[arm] = fmod;

      h2->Draw("colz");
      ge->Draw("p");
      p->SetLogz();

      p = (TPad *) c1->cd(idx++);
      c1->Update();

      TString name = Form("vertexdiff_arm%d", arm);
      TH1F * h1 = new TH1F(name, name, 300, -1, 1);
//      alignment->Draw("track.z0reco_no_VTX_DCA-track.vtxzp>>" + name,
//          Form("track.Nodes_FVTX.arm[0] == %d", arm), "");
      alignment->Draw("track.disp_pt_vtx>>" + name,
          Form("track.Nodes_FVTX.arm[0] == %d", arm), "");

      TF1 * f1 = new TF1(Form("gaus_%d", arm), "gaus", -1, 1);
      TF1 * f2 = new TF1(Form("dgaus_%d", arm),
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
              "Arm %d vertex diff #rightarrow Pt;vertex (VTX - Kalman fit) #upoint #hat{p_{T}} (cm)",
              arm));

    }

  // get average beam position off set
  const double mod_cos = (fmods[0]->GetParameter(0) + fmods[1]->GetParameter(0))
      / 2;
  const double mod_sin = (fmods[0]->GetParameter(1) + fmods[1]->GetParameter(1))
      / 2;

  cout << Form("Average VTX shift VS FVTX = (%f %f) cm", mod_cos, mod_sin)
      << endl;

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);
}

void
DrawVTX_Kalman(TString infile = "./FvtxGlobalAlign_AlignDST.root")
{

  gStyle->SetOptStat(0);
  gStyle->SetOptFit(1111);

  if (!_file0)
    _file0 = TFile::Open(infile);

  assert(_file0);

  if (!alignment)
    alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  TCanvas *c1 = new TCanvas("DrawVTX_Kalman", "DrawVTX_Kalman", 1800, 900);
  c1->Divide(3, 2);

  int idx = 1;
  TPad * p;

  alignment->SetAlias("disp_x_kalman",
      "track.vtx_point.fX - track.vtx_point_kalman.fX");
  alignment->SetAlias("disp_y_kalman",
      "track.vtx_point.fY - track.vtx_point_kalman.fY");
  alignment->SetAlias("phi",
      "1*atan2(track.trk_par_kalman._py,track.trk_par_kalman._px)");

  alignment->SetAlias("disp_pt_vtx_kalman",
      "disp_x_kalman * cos(phi) + disp_y_kalman * sin(phi)");
  alignment->SetAlias("disp_lateral_vtx_kalman",
      "disp_x_kalman * cos(phi+pi/2) + disp_y_kalman * sin(phi+pi/2)");

  // DCA corrections - FVTX workshop Oct 2012
  alignment->SetAlias("disp_pt_vtx_kalman_cor",
      "disp_pt_vtx_kalman - 0.1201 * disp_lateral_vtx_kalman * disp_lateral_vtx_kalman");

  alignment->SetAlias("tan_theta",
      "sqrt(track.trk_par_kalman._py**2 + track.trk_par_kalman._px**2)/(track.trk_par_kalman._pz)");
  alignment->SetAlias("vtxz_fvtxmutr",
      "track.vtxzp - disp_pt_vtx_kalman_cor/tan_theta");

  TH1F * hbbc_diffs[100] =
    { NULL };

  for (int arm = 0; arm < 2; arm++)
    {

      p = (TPad *) c1->cd(idx++);
      c1->Update();

      TString name = Form("disp_pt_vtx_kalman_arm%d", arm);
      TH1F * h1 = new TH1F(name, name, 300, -1, 1);
//      alignment->Draw("track.z0reco_no_VTX_DCA-track.vtxzp>>" + name,
//          Form("track.Nodes_FVTX.arm[0] == %d", arm), "");
      alignment->Draw("disp_pt_vtx_kalman>>" + name,
          Form("track.Nodes_FVTX.arm[0] == %d", arm), "");

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
              "Arm %d vertex diff #rightarrow Pt;vertex (VTX - Kalman fit) #upoint #hat{p_{T}} (cm)",
              arm));

      p = (TPad *) c1->cd(idx++);
      c1->Update();

      TString name = Form("disp_lateral_vtx_kalman_arm%d", arm);
      TH1F * h1 = new TH1F(name, name, 300, -1, 1);
//      alignment->Draw("track.z0reco_no_VTX_DCA-track.vtxzp>>" + name,
//          Form("track.Nodes_FVTX.arm[0] == %d", arm), "");
      alignment->Draw("disp_lateral_vtx_kalman>>" + name,
          Form("track.Nodes_FVTX.arm[0] == %d", arm), "");

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
              "Arm %d vertex diff #rightarrow Pt #times z;vertex (VTX - Kalman fit) #upoint #hat{(p_{T}#timesz)}  (cm)",
              arm));

      p = (TPad *) c1->cd(idx++);
      c1->Update();

      TString name = Form("disp_pt_vtx_kalman_cor_arm%d", arm);
      TH1F * h1 = new TH1F(name, name, 300, -1, 1);
//      alignment->Draw("track.z0reco_no_VTX_DCA-track.vtxzp>>" + name,
//          Form("track.Nodes_FVTX.arm[0] == %d", arm), "");
      alignment->Draw("disp_pt_vtx_kalman_cor>>" + name,
          Form("track.Nodes_FVTX.arm[0] == %d", arm), "");

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
              "Arm %d vertex diff #rightarrow Pt with Pt #times z correction;vertex (VTX - Kalman fit) #upoint #hat{p_{T}} + cor. (cm)",
              arm));

    }

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);

  TCanvas *c2 = new TCanvas("DrawVTX_Kalman_BBC", "DrawVTX_Kalman_BBC", 1100,
      600);
  c2->Divide(2, 1);

  for (int arm = 0; arm < 2; arm++)
    {

      c2->cd(arm + 1);
      c2->Update();

      TString name = Form("hbbc_diff%d", arm);
      TH1F * hbbc_diff = new TH1F(name, name, 300, -6, 6);
//      alignment->Draw("track.z0reco_no_VTX_DCA-track.vtxzp>>" + name,
//          Form("track.Nodes_FVTX.arm[0] == %d", arm), "");
      alignment->Draw("track.bbcz - vtxz_fvtxmutr>>" + name,
          Form("track.Nodes_FVTX.arm[0] == %d", arm), "");

      hbbc_diff->SetTitle(
          Form(
              "Arm %d vertex diff (z_{BBC} - z_{FVTX+MuTr});z_{BBC} - z_{FVTX+MuTr} (cm)",
              arm));

      TF1 * fbbc_diff = new TF1(Form("gaus4_%d", arm), "gaus", -1, 1);

      fbbc_diff->SetLineColor(kRed);
      fbbc_diff->SetNpx(1000);

      hbbc_diff->Fit(fbbc_diff, "M");

      hbbc_diffs[arm] = hbbc_diff;

    }

  SaveCanvas(c2, TString(infile) + TString("_") + TString(c2->GetName()),
      kFALSE);
}

void
DrawVTX_MuTr(TString infile = "./FvtxGlobalAlign_AlignDST.root")
{

  gStyle->SetOptStat(1110);

  if (!_file0)
    _file0 = TFile::Open(infile);

  assert(_file0);

  if (!alignment)
    alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  TCanvas *c1 = new TCanvas("DrawVTX_MuTr", "DrawVTX_MuTr", 1800, 900);
  c1->Divide(4, 2);
  int idx = 1;
  TPad * p;

  for (int arm = 0; arm < 2; arm++)
    {

      p = (TPad *) c1->cd(idx++);
      c1->Update();

      TString name = Form("momentum_arm%d_chargePos", arm);
      TH1F * h1 = new TH1F(name, name, 200, 2, 202);
      alignment->Draw(
          "sqrt(track.trk_par_kalman._px^2 + track.trk_par_kalman._py^2 + track.trk_par_kalman._pz^2)>>"
              + name,
          Form(
              "track.Nodes_MuTr.arm[0] == %d && track.trk_par_kalman._charge == 1",
              arm), "");
      h1->SetLineColor(kRed);
      h1->SetTitle(
          Form("Count VS momentum Arm %d;Momentum (GeV/c);Count (1/0.25 GeV)",
              arm));

      TString name = Form("momentum_arm%d_chargeNeg", arm);
      TH1F * h1 = new TH1F(name, name, 200, 0, 200);
      alignment->Draw(
          "sqrt(track.trk_par_kalman._px^2 + track.trk_par_kalman._py^2 + track.trk_par_kalman._pz^2)>>"
              + name,
          Form(
              "track.Nodes_MuTr.arm[0] == %d && track.trk_par_kalman._charge == -1",
              arm), "same");
      h1->SetLineColor(kBlue);

      h1->SetTitle(
          Form("Count VS momentum Arm %d;Momentum (GeV/c);Count (1/0.25 GeV)",
              arm));

      p->SetLogy();
      p->SetLogx();

      p = (TPad *) c1->cd(idx++);
      c1->Update();

//      TString name = Form("mutr_disp_pt_vtxVSphi_arm%d", arm);
//      TH2F * h2 = new TH2F(name, name, 48 * 2, -3.14159, 3.14159, 200, -20, 20);
//      alignment->Draw(
//          "track.mutr_disp_pt_vtx:atan2(track.trk_par_kalman._py,track.trk_par_kalman._px)>>"
//              + name, Form("track.Nodes_MuTr.arm[0] == %d", arm), "COLZ");
//
//      h2->SetTitle(
//          Form(
//              "MuTr Arm %d R displacement VS phi;central #phi (rad);vertex displacement along p_{T} (cm)",
//              arm));
//
//      p->SetLogz();
//
//      TProfile * p2 = h2->ProfileX();
//      p2->SetLineWidth(5);
//      p2->SetLineColor(kBlack);
//      p2->Draw("same");

      DrawVTX_MuTr_Sub(
          //
          "mutr_disp_pt_vtx",//
//          "mutr_disp_pt_z_ref", //
          arm,
          "MuTr Arm %d R displacement VS #phi;azimtuhal #phi (rad); displacement (cm)",
          7);

      p = (TPad *) c1->cd(idx++);
      c1->Update();

//      TString name = Form("mutr_disp_lateral_vtxVSphi_arm%d", arm);
//      TH2F * h2 = new TH2F(name, name, 48 * 2, -3.14159, 3.14159, 200, -20, 20);
//      alignment->Draw(
//          "track.mutr_disp_lateral_vtx:atan2(track.trk_par_kalman._py,track.trk_par_kalman._px)>>"
//              + name, Form("track.Nodes_MuTr.arm[0] == %d", arm), "COLZ");
//
//      h2->SetTitle(
//          Form(
//              "MuTr Arm %d Phi displacement VS phi;central #phi (rad);vertex displacement along p_{T} (cm)",
//              arm));
//
//      p->SetLogz();
//
//      TProfile * p2 = h2->ProfileX();
//      p2->SetLineWidth(5);
//      p2->SetLineColor(kBlack);
//      p2->Draw("same");

      DrawVTX_MuTr_Sub(
          //
          "mutr_disp_lateral_vtx",//
//          "mutr_disp_lateral_z_ref",//
          arm,
          "MuTr Arm %d Phi displacement VS #phi;azimtuhal #phi (rad); displacement (cm)",
          7);

      p = (TPad *) c1->cd(idx++);
      c1->Update();

//      TString name = Form("mutr_disp_tangent_thetaVSphi_arm%d", arm);
//      TH2F * h2 = new TH2F(name, name, 48 * 2, -3.14159, 3.14159, 100, -.5, .5);
//      alignment->Draw(
//          "track.mutr_disp_tangent_theta:atan2(track.trk_par_kalman._py,track.trk_par_kalman._px)>>"
//              + name, Form("track.Nodes_MuTr.arm[0] == %d", arm), "COLZ");
//
//      h2->SetTitle(
//          Form(
//              "MuTr Arm %d Theta displacement VS phi;central #phi (rad);vertex displacement along p_{T} (cm)",
//              arm));
//
//      p->SetLogz();
//
//      TProfile * p2 = h2->ProfileX();
//      p2->SetLineWidth(5);
//      p2->SetLineColor(kBlack);
//      p2->Draw("same");

      DrawVTX_MuTr_Sub("mutr_disp_tangent_theta", arm,
          "MuTr Arm %d Theta displacement VS #phi;azimtuhal #phi (rad); dr/dz slope difference",
          .1);

    }

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);
}

void
DrawZRef_MuTr(TString infile = "./FvtxGlobalAlign_AlignDST.root",
    const int charged = 0, const bool use_kalman_fit = false)
{

  gStyle->SetOptStat(1110);

  if (!_file0)
    _file0 = TFile::Open(infile);

  assert(_file0);

  if (!alignment)
    alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  TString sufix = "";
  if (charged)
    sufix += "_Charged";
  if (use_kalman_fit)
    {
      sufix += "_KalmanFitOnly";

      alignment->SetAlias("phi_zref",
          "1*atan2(track.trk_par_kalman_zref._y,track.trk_par_kalman_zref._x)");

      alignment->SetAlias("mutr_disp_pt_z_ref_kalman",
          "(track.trk_par_kalman_zref._x - track.trk_par_kalman_mutr_zref._x)*cos(phi_zref) + (track.trk_par_kalman_zref._y - track.trk_par_kalman_mutr_zref._y)*sin(phi_zref)");
      alignment->SetAlias("mutr_disp_lateral_z_ref_kalman",
          "(track.trk_par_kalman_zref._x - track.trk_par_kalman_mutr_zref._x)*cos(phi_zref + pi/2) + (track.trk_par_kalman_zref._y - track.trk_par_kalman_mutr_zref._y)*sin(phi_zref + pi/2)");
      alignment->SetAlias("mutr_disp_tangent_theta_kalman",
          "(track.trk_par_kalman_zref._px/track.trk_par_kalman_zref._pz - track.trk_par_kalman_mutr_zref._px/track.trk_par_kalman_mutr_zref._pz)*cos(phi_zref) + (track.trk_par_kalman_zref._py/track.trk_par_kalman_zref._pz - track.trk_par_kalman_mutr_zref._py/track.trk_par_kalman_mutr_zref._pz)*sin(phi_zref)");

    }

  TCanvas *c1 = new TCanvas("DrawZRef_MuTr" + sufix, "DrawZRef_MuTr" + sufix,
      1800, 900);
  c1->Divide(4, 2);
  int idx = 1;
  TPad * p;

  for (int arm = 0; arm < 2; arm++)
    {

      p = (TPad *) c1->cd(idx++);
      c1->Update();

      TString name = Form("momentum_arm%d_chargePos", arm);
      TH1F * h1 = new TH1F(name, name, 200, 2, 202);
      alignment->Draw(
          "sqrt(track.trk_par_kalman._px^2 + track.trk_par_kalman._py^2 + track.trk_par_kalman._pz^2)>>"
              + name,
          Form(
              "track.Nodes_MuTr.arm[0] == %d && track.trk_par_kalman._charge == 1",
              arm), "");
      h1->SetLineColor(kRed);
      h1->SetTitle(
          Form("Count VS momentum Arm %d;Momentum (GeV/c);Count (1/0.25 GeV)",
              arm));

      TString name = Form("momentum_arm%d_chargeNeg", arm);
      TH1F * h1 = new TH1F(name, name, 200, 0, 200);
      alignment->Draw(
          "sqrt(track.trk_par_kalman._px^2 + track.trk_par_kalman._py^2 + track.trk_par_kalman._pz^2)>>"
              + name,
          Form(
              "track.Nodes_MuTr.arm[0] == %d && track.trk_par_kalman._charge == -1",
              arm), "same");
      h1->SetLineColor(kBlue);

      h1->SetTitle(
          Form("Count VS momentum Arm %d;Momentum (GeV/c);Count (1/0.25 GeV)",
              arm));

      p->SetLogy();
      p->SetLogx();

      p = (TPad *) c1->cd(idx++);
      c1->Update();

//      TString name = Form("mutr_disp_pt_vtxVSphi_arm%d", arm);
//      TH2F * h2 = new TH2F(name, name, 48 * 2, -3.14159, 3.14159, 200, -20, 20);
//      alignment->Draw(
//          "track.mutr_disp_pt_vtx:atan2(track.trk_par_kalman._py,track.trk_par_kalman._px)>>"
//              + name, Form("track.Nodes_MuTr.arm[0] == %d", arm), "COLZ");
//
//      h2->SetTitle(
//          Form(
//              "MuTr Arm %d R displacement VS phi;central #phi (rad);vertex displacement along p_{T} (cm)",
//              arm));
//
//      p->SetLogz();
//
//      TProfile * p2 = h2->ProfileX();
//      p2->SetLineWidth(5);
//      p2->SetLineColor(kBlack);
//      p2->Draw("same");

      DrawVTX_MuTr_Sub(
          //
//          "mutr_disp_pt_vtx", //
          use_kalman_fit ? "mutr_disp_pt_z_ref_kalman" : "mutr_disp_pt_z_ref", //
          arm,
          "MuTr Arm %d R displacement VS phi;azimuthal #phi (rad); displacement (cm)",
          7, charged) //
//      ->Draw("*") //
          ;

      p = (TPad *) c1->cd(idx++);
      c1->Update();

//      TString name = Form("mutr_disp_lateral_vtxVSphi_arm%d", arm);
//      TH2F * h2 = new TH2F(name, name, 48 * 2, -3.14159, 3.14159, 200, -20, 20);
//      alignment->Draw(
//          "track.mutr_disp_lateral_vtx:atan2(track.trk_par_kalman._py,track.trk_par_kalman._px)>>"
//              + name, Form("track.Nodes_MuTr.arm[0] == %d", arm), "COLZ");
//
//      h2->SetTitle(
//          Form(
//              "MuTr Arm %d Phi displacement VS phi;central #phi (rad);vertex displacement along p_{T} (cm)",
//              arm));
//
//      p->SetLogz();
//
//      TProfile * p2 = h2->ProfileX();
//      p2->SetLineWidth(5);
//      p2->SetLineColor(kBlack);
//      p2->Draw("same");

      DrawVTX_MuTr_Sub(
          //
//          "mutr_disp_lateral_vtx",//
          use_kalman_fit ?
              "mutr_disp_lateral_z_ref_kalman" : "mutr_disp_lateral_z_ref", //
          arm,
          "MuTr Arm %d Phi displacement VS phi;azimuthal #phi (rad); displacement (cm)",
          7, charged) //
//      ->Draw("*") //
          ;

      p = (TPad *) c1->cd(idx++);
      c1->Update();

//      TString name = Form("mutr_disp_tangent_thetaVSphi_arm%d", arm);
//      TH2F * h2 = new TH2F(name, name, 48 * 2, -3.14159, 3.14159, 100, -.5, .5);
//      alignment->Draw(
//          "track.mutr_disp_tangent_theta:atan2(track.trk_par_kalman._py,track.trk_par_kalman._px)>>"
//              + name, Form("track.Nodes_MuTr.arm[0] == %d", arm), "COLZ");
//
//      h2->SetTitle(
//          Form(
//              "MuTr Arm %d Theta displacement VS phi;central #phi (rad);vertex displacement along p_{T} (cm)",
//              arm));
//
//      p->SetLogz();
//
//      TProfile * p2 = h2->ProfileX();
//      p2->SetLineWidth(5);
//      p2->SetLineColor(kBlack);
//      p2->Draw("same");

      DrawVTX_MuTr_Sub(
          use_kalman_fit ?
              "mutr_disp_tangent_theta_kalman" : "mutr_disp_tangent_theta", arm,
          "MuTr Arm %d Theta displacement VS phi;azimuthal #phi (rad);dr/dz slope difference",
          .1, charged) //
//          ->Draw("*")//
          ;

//      return;
    }

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);
}

void
DrawZRefdR_MuTr(TString infile = "./FvtxGlobalAlign_AlignDST.root", int arm = 0)
{

  gStyle->SetOptStat(1110);

  if (!_file0)
    _file0 = TFile::Open(infile);

  assert(_file0);

  if (!alignment)
    alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  TString sufix = Form("_Arm%d", arm);

  TCanvas *c1 = new TCanvas("DrawZRefdR_MuTr" + sufix,
      "DrawZRefdR_MuTr" + sufix, 1800, 900);
  c1->Divide(8, 2);
  int idx = 1;
  TPad * p;

  fstream f(TString(infile) + TString("_") + TString(c1->GetName()) + ".log",
      ios_base::out);
  assert(f.is_open());

  for (int oct = 0; oct < 8; oct++)
    {

      for (int half = 0; half < 2; half++)
        {

          p = (TPad *) c1->cd(idx++);
          c1->Update();

          TString name = Form("dR_arm%d_oct%d_half%d", arm, oct, half);

          TH1F * h = new TH1F(name,
              Form("#deltaR arm%d oct%d half%d;#deltaR (cm)", arm, oct, half),
              100, -10, 10);

          alignment->Project(name,
              "mutr_disp_pt_z_ref", //
              Form(
                  "track.Nodes_MuTr.arm[0] == %d && Sum$(track.Nodes_MuTr.octant*2 + track.Nodes_MuTr.half_octant)/Length$(track.Nodes_MuTr.octant) == %d",
                  arm, oct * 2 + half));

          TF1 * f1 = new TF1(name + "_gaus", "gaus", -10, 10);
          TF1 * f2 = new TF1(name + "_gaus2",
              "gaus + [3]*exp(-0.5*((x-[1])/[4])**2) ", -10, 10);

          f1->SetLineColor(kGreen);
          f2->SetLineColor(kRed);
          f2->SetNpx(1000);

          h->Fit(f1, "M0");

          f2->SetParameters(f1->GetParameter(0) / 2, f1->GetParameter(1),
              f1->GetParameter(2), f1->GetParameter(0) / 2,
              f1->GetParameter(2) / 4, 0);

          f2->SetParName(1, "Mean");

          f2->SetParName(2, "#sigma (W)");
          f2->SetParName(4, "#sigma (N)");

          f2->SetParName(0, "Amp (W)");
          f2->SetParName(3, "Amp (N)");

          f2->SetParName(2, "#sigma (W)");
          f2->SetParName(4, "#sigma (N)");

          h->Fit(f2, "M");

          f << arm << "\t";
          f << oct << "\t";
          f << half << "\t";
          f << f2->GetParameter(1) << endl;
        }
    }

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);

  f.close();

  cout << "Saved to "
      << TString(infile) + TString("_") + TString(c1->GetName()) + ".log"
      << endl;

}

void
DrawResidu_VTX(TString infile = "./FvtxGlobalAlign_AlignDST.root",
    const int UseSpecialFit = 0, const int switch_r_phi = 1)
{

  const TString prefix = UseSpecialFit ? "3-D_LateralConstraint" : "KalmanFit";
  const TString prefix2 = switch_r_phi ? "_R_Direction" : "_Phi_Direction";
  const TString residu =
      UseSpecialFit ?
          "track.Nodes_VTX.residu" : "track.Nodes_VTX.residu_kalman";
  const double ylimit = UseSpecialFit >= 1 ? 2000 : 2000;

  TCanvas *c1 = new TCanvas("DrawResidu_VTX_" + prefix + prefix2,
      "DrawResidu_VTX_" + prefix + prefix2, 1800, 900);
  c1->Divide(5, 2);
  int idx = 1;

  for (int layer = 0; layer < 2; layer++)
    {
      for (int sensor = -2; sensor < 2; sensor++)
        {
          const int N_LADDER = layer == 0 ? 10 : 18;

          TString name = residu + Form("_%1d_%2d", layer, sensor) + prefix2;
          TH2F * h = new TH2F(name, name, N_LADDER, -.5, N_LADDER - .5, 100,
              -ylimit, ylimit);

//        TF1 * myfit1_1 = new TF1(name + "myfit1_1",
//            "[0]+[1]*sin(-pi/2+x*3.14159/24)+[2]*cos(-pi/2+x*3.14159/24)", -.5,
//            23.5);
//        TF1 * myfit1_2 = new TF1(name + "myfit1_2",
//            "[0]+[1]*sin(pi/2+x*3.14159/24)+[2]*cos(pi/2+x*3.14159/24)", 23.5,
//            47.5);

//        alignment->Project(name, residu + "*1e4:track.Nodes_VTX.ladder",
//            Form(
//                "(track.Nodes_VTX.layer == %d) && (track.Nodes_VTX.sensor==%d ) && (track.Nodes_VTX.switch_r_phi==%d)",
//                layer, sensor, switch_r_phi));
//
//        h->SetTitle(
//            Form("Layer %d Sensor %d;Ladder;w-Residual Meas-Fit (#mum)", layer,
//                sensor) + prefix2);

          alignment->Project(name, residu + "*1e4:track.Nodes_VTX.ladder",
              Form(
                  "(track.Nodes_VTX.layer == %d) && (floor(track.Nodes_VTX.p_det.fZ/5.72)==%d ) && (track.Nodes_VTX.switch_r_phi==%d)",
                  layer, sensor, switch_r_phi));

          h->SetTitle(
              Form(
                  "Layer %d Sensor z = [%.1f - %.1f] cm;Ladder;w-Residual Meas-Fit (#mum)", //
                  layer, sensor * 5.72, (sensor + 1) * 5.72) + prefix2);

          TLine * l = new TLine(N_LADDER / 2 - .5, -ylimit, N_LADDER / 2 - .5,
              ylimit);
          l->SetLineColor(kBlack);
          l->SetLineWidth(2);
          TLine * l2 = new TLine(-.5, 0, N_LADDER - .5, 0);
          l2->SetLineColor(kBlack);
          l2->SetLineWidth(2);

          TPad * p = c1->cd(idx++);
          c1->Update();

//        h->Fit(myfit1_1, "MR"); // fit St 1 left
//        h->Fit(myfit1_2, "MR"); // fit St 1 right
          p->SetLogz();
//        h->Fit("pol0");
          h->Draw("COLZ");

          l->Draw();
          l2->Draw();
        }

      TPad * p = (TPad *) (c1->cd(idx++));
      c1->Update();

      assert(p);
      p->SetLogy();

      TString name = Form("residual_layer%d", layer);
      TH1F * h1 = new TH1F(name, name, 300, -1000, 1000);
//      T->Draw("track.z0reco_no_VTX_DCA-track.vtxzp>>" + name,
//          Form("arm == %d", arm), "");
      alignment->Draw(residu + "*1e4 >> " + name,
          Form(
              "track.Nodes_VTX.layer == %d && (track.Nodes_VTX.switch_r_phi==%d)",
              layer, switch_r_phi), "");

      h1->GetXaxis()->SetTitle("Residual" + prefix2 + " (#mum)");

      TF1 * f1 = new TF1(Form("gaus1_%d", layer), "gaus ", -200, 200);

      f1->SetLineColor(kGreen);
      f1->SetNpx(1000);

      h1->Fit(f1, "M");

    }

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);
}

void
DrawResidu_MuTr(TString infile = "./FvtxGlobalAlign_AlignDST.root",
    const int UseSpecialFit = 0, const int arm = 0)
{

  const TString prefix = UseSpecialFit ? "3-D_LateralConstraint" : "KalmanFit";
  const TString prefix2 = arm ? "_North" : "_South";
  const TString residu =
      UseSpecialFit ?
          "track.Nodes_MuTr.residu" : "track.Nodes_MuTr.residu_kalman";
  const double ylimit = UseSpecialFit >= 1 ? 5000 : 5000;

  TH1F * h_mean_sum = new TH1F("h_mean_sum",
      "Summary for means;Residual Mean Fit (#mum)", 100, -300, 300);

  TCanvas *c1 = new TCanvas("DrawResidu_MuTr_" + prefix + prefix2,
      "DrawResidu_MuTr_" + prefix + prefix2, 1800, 1000);
  c1->Divide(6, 3);
  int idx = 1;

  for (int station = 0; station < 3; station++)
    {
//      const int N_GAP = station == 2 ? 2 : 3;
      for (int gap = 0; gap < 3; gap++)
        for (int cathode = 0; cathode < 2; cathode++)
          {

            TString name = residu
                + Form("_Arm%1d_Sta%1d_Gap%d_Cat%d", arm, station, gap, cathode)
                + prefix2;
            TH2F * h = new TH2F(name, name, 16, -.5, 16 - .5, 200, -ylimit,
                ylimit);

//        TF1 * myfit1_1 = new TF1(name + "myfit1_1",
//            "[0]+[1]*sin(-pi/2+x*3.14159/24)+[2]*cos(-pi/2+x*3.14159/24)", -.5,
//            23.5);
//        TF1 * myfit1_2 = new TF1(name + "myfit1_2",
//            "[0]+[1]*sin(pi/2+x*3.14159/24)+[2]*cos(pi/2+x*3.14159/24)", 23.5,
//            47.5);

            alignment->Project(name,
                residu
                    + "*1e4:track.Nodes_MuTr.octant*2 + track.Nodes_MuTr.half_octant",
                Form(
                    "(track.Nodes_MuTr.arm == %d) && (track.Nodes_MuTr.station==%d ) && (track.Nodes_MuTr.gap==%d) && (track.Nodes_MuTr.cathode==%d)",
                    arm, station, gap, cathode));

            h->FitSlicesY();
            TH1D * h_mean = (TH1D *) gDirectory->GetObjectChecked(name + "_1",
                "TH1D");
            assert(h_mean);
            h_mean->SetLineColor(kMagenta);
            h_mean->SetLineWidth(2);

            for (int bin = 1; bin <= h_mean->GetNbinsX(); bin++)
              {
                if (h_mean->GetBinContent(bin) != 0)
                  h_mean_sum->Fill(h_mean->GetBinContent(bin));
              }

            h->SetTitle(
                Form(
                    "Arm%1d Sta%1d Gap%d Cat%d;(octant #times 2+half_octant);w-Residual Meas-Fit (#mum)",
                    arm, station, gap, cathode) + prefix2);

            TLine * l2 = new TLine(-.5, 0, 16 - .5, 0);
            l2->SetLineColor(kBlack);
            l2->SetLineWidth(1);

            TPad * p = c1->cd(idx++);
            c1->Update();

            p->SetLogz();

            h->Draw("COLZ");
            h_mean->Draw("same");

//            l->Draw();
            l2->Draw();
          }
    }

  h_mean_sum->Draw();
  h_mean_sum->Fit("gaus");

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);
}

void
DrawFit_MuTr(TString var, TCut cut, TH2F * h2)
{
  TH2F * h2s[8] =
    { NULL };
  for (int i = 0; i < 8; i++)
    {

      h2s[i] = (TH2F *) h2->Clone(Form("%s_%d", h2->GetName(), i));
    }

  for (int i = 0; i < 8; i++)
    {

      cout << "DrawFit_MuTr - process " << h2s[i]->GetName() << endl;

      const double phi_cent = i * TMath::Pi() / 4;

      TCut phi_cut =
          Form(
              "abs(fmod(atan2(track.trk_par_kalman._py,track.trk_par_kalman._px) - %f + 3 * pi, 2 * pi) - pi)<pi/8",
              phi_cent);

      alignment->Project(h2s[i]->GetName(), var.Data(),
          (const char *) (cut && phi_cut));

      TF1 * f1 = new TF1(h2s[i]->GetName() + TString("_fit"), "cos(x)++sin(x)",
          -3.14, 3.14);
      h2s[i]->Fit(f1, "MQ");
      f1->Print();

      h2->Add(h2s[i]);
    }

}

TMultiGraph *
DrawVTX_MuTr_Sub(TString var = "mutr_disp_pt_vtx", int arm, TString title =
    "MuTr Arm %d R displacement VS phi;central #phi (rad); displacement (cm)",
    const double ylim = 15, const int charged = 0)
{

  TString name = var + Form("VSphi_arm%d", arm);
  TH2F * h2 = new TH2F(name, name, //
      48, -3.14159, 3.14159, 30, -ylim, ylim);
  TH2F * h2_pos = new TH2F(name + "Pos", name + "Pos", //
  48, -3.14159, 3.14159, 30, -ylim, ylim);
  TH2F * h2_neg = new TH2F(name + "Neg", name + "Neg", //
  48, -3.14159, 3.14159, 30, -ylim, ylim);

  if (charged == 0)
    {

      alignment->Draw(
          var + ":atan2(track.trk_par_kalman._py,track.trk_par_kalman._px)>>"
              + name, Form("track.Nodes_MuTr.arm[0] == %d", arm), "goff");
//      DrawFit_MuTr(
//          "track." + var
//              + ":atan2(track.trk_par_kalman._py,track.trk_par_kalman._px)",
//          Form("track.Nodes_MuTr.arm[0] == %d", arm), h2);
    }
  else
    {
      alignment->Draw(
          var + ":atan2(track.trk_par_kalman._py,track.trk_par_kalman._px)>>"
              + name + "Pos",
          Form(
              "track.Nodes_MuTr.arm[0] == %d && track.trk_par_kalman._charge > 0",
              arm), "goff");
      alignment->Draw(
          var + ":atan2(track.trk_par_kalman._py,track.trk_par_kalman._px)>>"
              + name + "Neg",
          Form(
              "track.Nodes_MuTr.arm[0] == %d  && track.trk_par_kalman._charge < 0",
              arm), "goff");
    }

  h2->SetTitle(Form(title.Data(), arm));

  TMultiGraph * mg = new TMultiGraph();

  TGraphErrors * p2 = NULL;
  TGraphErrors * p2_pos = NULL;
  TGraphErrors * p2_neg = NULL;

  if (charged == 0)
    {
      p2 = FitProfile(h2);
      p2->SetLineWidth(2);
      p2->SetLineColor(kBlack);
      p2->SetMarkerColor(kBlack);
      p2->SetMarkerStyle(20);
      //p2->Draw("*");
      mg->Add(p2);
//
    }
  else
    {
      p2_pos = FitProfile(h2_pos);
      p2_pos->SetLineWidth(2);
      p2_pos->SetLineColor(kRed);
      p2_pos->SetMarkerColor(kRed);
      p2_pos->SetMarkerStyle(20);
//  p2->Draw("*");
      mg->Add(p2_pos);
//
      p2_neg = FitProfile(h2_neg);
      p2_neg->SetLineWidth(2);
      p2_neg->SetLineColor(kBlue);
      p2_neg->SetMarkerColor(kBlue);
      p2_neg->SetMarkerStyle(21);
//  p2_neg->Draw("*");
      mg->Add(p2_neg);

    }

  TH1 * frame = gPad->DrawFrame(-3.14159, -ylim, 3.14159, ylim);
  frame->SetTitle(Form(title.Data(), arm));

  if (charged == 0)
    {
      h2->Draw("colz same");
      p2->Draw("ep");
    }
  else
    {
      p2_pos->Draw("ep");
      p2_neg->Draw("ep");
    }
  DrawMuTrBoundary(ylim, arm);

  gPad->Update();

  return mg;
}

void
DrawMuTrBoundary(const double ylim, int arm)
{

  TLine * l2 = new TLine(-3.14159, 0, 3.14159, 0);
  l2->SetLineColor(kMagenta);
  l2->SetLineWidth(1);
  l2->Draw();

  const double pi = 3.14159265358979323846;
  const double phi0 = arm == 0 ? pi / 8 : -pi / 8;

  for (int q = -4; q < 4; q++)
    {
        {
          const double phi = phi0 + pi / 2 * q;

          TLine * l2 = new TLine(phi, -ylim, phi, ylim);
          l2->SetLineColor(kMagenta);
          l2->SetLineWidth(3);
          l2->Draw();
        }
        {
          const double phi = phi0 + pi / 2 * q + pi / 4;

          TLine * l2 = new TLine(phi, -ylim, phi, ylim);
          l2->SetLineColor(kMagenta);
          l2->SetLineWidth(1);
          l2->Draw();
        }
    }

}

TGraphErrors *
FitProfile(const TH2F * h2)
{

  TProfile * p2 = h2->ProfileX();

  int n = 0;
  double x[1000];
  double ex[1000];
  double y[1000];
  double ey[1000];

  for (int i = 1; i < h2->GetNbinsX(); i++)
    {
      TH1D * h1 = h2->ProjectionY("htmp", i, i);

      if (h1->GetSum() < 30)
        continue;

      TF1 fgaus("fgaus", "gaus", -p2->GetBinError(i) * 2,
          p2->GetBinError(i) * 2);
      fgaus.SetParameter(1, p2->GetBinContent(i));
      fgaus.SetParameter(2, p2->GetBinError(i));

      h1->Fit(&fgaus, "MQ");

//      new TCanvas;
//      h1->Draw();
//      fgaus.Draw("same");
//      break;

      x[n] = p2->GetBinCenter(i);
      ex[n] = (p2->GetBinCenter(2) - p2->GetBinCenter(1)) / 2;
      y[n] = fgaus.GetParameter(1);
      ey[n] = fgaus.GetParError(1);

//      p2->SetBinContent(i, fgaus.GetParameter(1));
//      p2->SetBinError(i, fgaus.GetParameter(2));

      n++;
      delete h1;
    }

  return new TGraphErrors(n, x, y, ex, ey);
}

void
DrawMuTr_Azimutthal(TString infile = "./FvtxGlobalAlign_AlignDST.root")
{

  gStyle->SetOptStat(1110);

  if (!_file0)
    _file0 = TFile::Open(infile);

  assert(_file0);

  if (!alignment)
    alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  TCanvas *c1 = new TCanvas("DrawMuTr_Azimutthal", "DrawMuTr_Azimutthal", 1300,
      700);
  c1->Divide(2, 1);
  int idx = 1;
  TPad * p;

  for (int arm = 0; arm < 2; arm++)
    {

      p = (TPad *) c1->cd(idx++);
      c1->Update();

      alignment->Draw(
          "atan2(track.trk_par_kalman._py,track.trk_par_kalman._px)",
          Form("track.Nodes_MuTr.arm[0] == %d", arm));
    }

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);
}

void
FVTXFitCheck(TString infile = "./FvtxGlobalAlign_AlignDST.root")
{

  alignment->SetAlias("phi_fit",
      "1*atan2(track.Nodes_FVTX.p_kalman.fY, track.Nodes_FVTX.p_kalman.fX)");

  alignment->SetAlias("phi_begin",
      "1*atan2(track.Nodes_FVTX.p_strip_begin.fY, track.Nodes_FVTX.p_strip_begin.fX)");
  alignment->SetAlias("phi_end",
      "1*atan2(track.Nodes_FVTX.p_strip_end.fY, track.Nodes_FVTX.p_strip_end.fX)");

  alignment->SetAlias("L2",
      "(track.Nodes_FVTX.p_strip_begin.fY - track.Nodes_FVTX.p_strip_end.fY)**2 + (track.Nodes_FVTX.p_strip_begin.fX - track.Nodes_FVTX.p_strip_end.fX)**2");
  alignment->SetAlias("Lb2",
      "(track.Nodes_FVTX.p_strip_begin.fY - track.Nodes_FVTX.p_kalman.fY)**2 + (track.Nodes_FVTX.p_strip_begin.fX - track.Nodes_FVTX.p_kalman.fX)**2");
  alignment->SetAlias("Le2",
      "(track.Nodes_FVTX.p_kalman.fY - track.Nodes_FVTX.p_strip_end.fY)**2 + (track.Nodes_FVTX.p_kalman.fX - track.Nodes_FVTX.p_strip_end.fX)**2");

  alignment->SetAlias("x", "(Le2 - Lb2)/(2*L2)");

  TCanvas *c1 = new TCanvas("FVTXFitCheck", FVTXFitCheck, 1800, 900);
  c1->Divide(5, 2);
  int idx = 1;

  for (int arm = 0; arm < 2; arm++)
    {
      for (int station = 0; station < 4; station++)
        {
          const double ylimit = 1.5;

          TString name = Form("FVTXFitCheck%1d%1d", arm, station);
          TH2F * h = new TH2F(name, name, 4, -.5, 3.5, 100, -ylimit, ylimit);

          alignment->Project(name, "x:fmod(track.Nodes_FVTX.sector,4)",
              Form(
                  "track.Nodes_FVTX.arm == %d && (track.Nodes_FVTX.station==%d )",
                  arm, station));

          h->SetTitle(
              Form(
                  "Arm %d Station %d;Wedge location (Sector %% 4);Normalized fit projection",
                  arm, station));

          TPad * p = c1->cd(idx++);
          c1->Update();

//          p->SetLogz();
          h->Draw("COLZ");

          TLine * l2 = new TLine(-.5, .5, 3.5, .5);
          l2->SetLineColor(kMagenta);
          l2->SetLineWidth(2);
          l2->Draw();

          TLine * l2 = new TLine(-.5, -.5, 3.5, -.5);
          l2->SetLineColor(kMagenta);
          l2->SetLineWidth(2);
          l2->Draw();
        }

      TString name = Form("FVTXFitCheck%1d", arm, station);
      TH1F * hg = new TH1F(name + "G", name, 50, -.02, .05);
      TH1F * ha = new TH1F(name + "A", name, 50, -.02, .05);

      alignment->Project(name + "G", "track.phi_acpt_width",
          Form(
              "track.Nodes_FVTX.arm == %d && (Sum$(abs(x)<=.5) == track.nNodes_FVTX)",
              arm));
      alignment->Project(name + "A", "track.phi_acpt_width",
          Form("track.Nodes_FVTX.arm == %d", arm));

      hg->Sumw2();
      hg->Divide(ha);

      hg->SetTitle(
          Form(
              "Ratio of Good Track - Arm %d;#phi acpt. window (rad);Ratio of Good Track",
              arm));

      c1->cd(idx++);
      c1->Update();

      hg->GetYaxis()->SetRangeUser(0, 1);
      hg->Draw();

    }

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);
}

