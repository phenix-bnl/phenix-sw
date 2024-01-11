// $Id: GetBeamPos.C,v 1.3 2016/03/10 19:09:26 jinhuang Exp $

/*!
 * \file Test_SpinAna.C
 * \brief Example how to draw the result of spin analyzer
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.3 $
 * \date $Date: 2016/03/10 19:09:26 $
 */

#include "SaveCanvas.C"
#include "SetOKStyle.C"
#include <cassert>
using namespace std;

TFile *_file0 = NULL;

void
GetBeamPos()
{
  SetOKStyle();
  gStyle->SetOptStat(0);
  gStyle->SetOptFit(1111);
  TVirtualFitter::SetDefaultFitter("Minuit2");

  gSystem->Load("libspin_analyzer");

  //Run15 pA pro.105
//  Merge(
//      "/gpfs/mnt/gpfs02/phenix/fvtx/subsys/fvtx/jinhuang/taxi/Run15pAu200MuonsMBPro105/8326/dca_hist.root.lst");
  Merge(
      "/gpfs/mnt/gpfs02/phenix/fvtx/subsys/fvtx/jinhuang/taxi/Run15pAu200MuonsMBPro105/8326/dca_hist.root.Test.lst");
  //  GetBeamPos_Fit_WithSlope_Run15pAu(
    //      "/gpfs/mnt/gpfs02/phenix/fvtx/subsys/fvtx/jinhuang/taxi/Run15pAu200MuonsMBPro105/8326/dca_hist.root.lst.root");
//    GetBeamPos_Fit_WithSlope_Run15pAu(
//          "/gpfs/mnt/gpfs02/phenix/fvtx/subsys/fvtx/jinhuang/taxi/Run15pAu200MuonsMBPro105/8326/dca_hist.root.lst.Iter1.root");
//  GetBeamPos_Fit_WithSlope_Run15pAu(
//      "/gpfs/mnt/gpfs02/phenix/fvtx/subsys/fvtx/jinhuang/taxi/Run15pAu200MuonsMBPro105/8326/dca_hist.root.lst.Iter1.root");


//    Merge(
//        "/phenix/u/jinhuang/links/fvtx_data/taxi/Run15pAu200MuonsMUPro105/8327_Iter1/dca_hist.root.lst");
//    _file0 =
//        TFile::Open(
//            "/phenix/u/jinhuang/links/fvtx_data/taxi/Run15pAu200MuonsMUPro105/8327_Iter1/dca_hist.root.lst.root");
//    DrawDCA_Z();


  //    Merge(
  //        "/phenix/u/jinhuang/links/fvtx_data/taxi/Run15pAu200MuonsMUPro105/8327_BeamAvg/dca_hist.root.lst");
  //    _file0 =
  //        TFile::Open(
  //            "/phenix/u/jinhuang/links/fvtx_data/taxi/Run15pAu200MuonsMUPro105/8327_BeamAvg/dca_hist.root.lst.root");
  //    DrawDCA_Z();

  //Run15 pp pro.105
//  Merge(
//      "/gpfs/mnt/gpfs02/phenix/fvtx/subsys/fvtx/jinhuang/taxi/Run15pp200MuonsMUPro105/8065_Iter2/dca_hist.root.lst");
//    Merge(
//        "/gpfs/mnt/gpfs02/phenix/fvtx/subsys/fvtx/jinhuang/taxi/Run15pp200MuonsMUPro105/8065_Iter2/dca_hist_VTX.root.lst");
//    Merge(
//        "/gpfs/mnt/gpfs02/phenix/fvtx/subsys/fvtx/jinhuang/taxi/Run15pp200MuonsMUPro105/8065_Iter2/dca_hist_noVTX.root.lst");
//  GetBeamPos_Fit_WithSlope(
//      "/gpfs/mnt/gpfs02/phenix/fvtx/subsys/fvtx/jinhuang/taxi/Run15pp200MuonsMUPro105/8065/dca_hist.root.lst.root");
//  GetBeamPos_Fit_WithSlope(
//      "/gpfs/mnt/gpfs02/phenix/fvtx/subsys/fvtx/jinhuang/taxi/Run15pp200MuonsMUPro105/8065_Iter1/dca_hist.root.lst.root");

//  Merge(
//      "/phenix/u/jinhuang/links/fvtx_data/taxi/Run15pp200MuonsMUPro104/6729/Pass2/se-ALL.lst");
//  GetBeamPos_Fit_Run15pp(
//      "/phenix/u/jinhuang/links/fvtx_data/taxi/Run15pp200MuonsMUPro104/6729/Pass2/se-ALL.lst.root");

//  Merge("/phenix/u/jinhuang/links/fvtx_data/taxi/Run15pp200MuonsMUPro104/6729/ReviewPass/se-ALL.lst");
//  GetBeamPos_Fit_Run15pp("/phenix/u/jinhuang/links/fvtx_data/taxi/Run15pp200MuonsMUPro104/6729/ReviewPass/se-ALL.lst.root");
//  GetBeamPos_Fit_Run15pp(
//      "/gpfs/mnt/gpfs02/phenix/fvtx/subsys/fvtx/jinhuang/taxi/Run15pp200MuonsMUPro105/8065/dca_hist.root.lst.root");

//  _file0 =
//      TFile::Open(
//          "/gpfs/mnt/gpfs02/phenix/fvtx/subsys/fvtx/jinhuang/taxi/Run15pp200MuonsMUPro105/8065_Iter2/dca_hist.root.lst.root");
//  _file0 =
//      TFile::Open(
//          "/gpfs/mnt/gpfs02/phenix/fvtx/subsys/fvtx/jinhuang/taxi/Run15pp200MuonsMUPro105/8065_Iter2/dca_hist_VTX.root.lst.root");
//  _file0 =
//      TFile::Open(
//          "/gpfs/mnt/gpfs02/phenix/fvtx/subsys/fvtx/jinhuang/taxi/Run15pp200MuonsMUPro105/8065_Iter2/dca_hist_noVTX.root.lst.root");
//  DrawDCA_Z();
//  DrawDCA_FVTX();
//  DrawDCA();
//  DrawDCAPHI();
//  DrawDCAPHI_FVTX();
//  DrawVertex_XZ_YZ();

}

void
Merge(
    const TString list_file =
        "/phenix/u/jinhuang/links/fvtx_data/taxi/Run15pp200MuonsMUPro104/6729/ReviewPass/se-ALL.lst")
{

//    _file0 = TFile::Open(
//        "/phenix/u/jinhuang/links/fvtx_data/taxi/Run12pp510MuonPro100/4117/dca_hist/dca_hist.lst.root");
//    _file0 = TFile::Open(
//        "../data_pro100_VTXv16_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
  //  _file0 = TFile::Open(
  //      "../data_pro100_VTXv16_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
//  _file0 = TFile::Open(
//      "../data_pro100_VTXv18_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
//  _file0 = TFile::Open(
//      "../data_pro100_VTXv16_LooseVTX/MWG_MU_pDST.lst_vtx_dca_hist.root");
//  _file0 = TFile::Open(
//      "../data_pro100_VTXv18_LooseVTX/MWG_MU_pDST.lst_vtx_dca_hist.root");
//  _file0 = TFile::Open(
//      "../data_pro100_VTXv17_FVTXv10_VTXSeg_masterdb/MWG_MU_pDST.lst_dca_hist.root");
//    _file0 = TFile::Open(
//        "../data_pro100_VTXv17_FVTXv10_VTXSeg_masterdb_2stfix_pro100patch/MWG_MU_pDST.lst_dca_hist.root");
//    _file0 = TFile::Open(
//        "../data_pro100_VTXv17_FVTXv10_VTXSeg_masterdb_2stfix_pro100patch/MWG_MU_pDST.lst_vtx_dca_hist.root");

//  const char * hist_names[] =
//    { "VTX_XY" };
//  //  MergeHist(
//  //      "/phenix/u/jinhuang/links/fvtx_data/taxi/Run12pp510MuonPro100/4117/se-368692.lst",
//  //      1, hist_names);
//      MergeHist(
//          "/phenix/u/jinhuang/links/fvtx_data/taxi/Run12pp510MuonPro100/4117/se-ALL.lst",
//          1, hist_names);
  const char * hist_names[] =
    { "VTX_XY", "VTX_YZ", "VTX_XZ", "DCA_Phi_z_Arm0", "DCA_Phi_z_Arm1",
        "Normalization", "DCA_Phi_Arm" };
  MergeHist(
//      "/phenix/u/jinhuang/links/fvtx_data/taxi/Run15pp200MuonsMUPro104/6729/FirstPass/se-ALL.lst",
      list_file, 7, hist_names);

////  const char * hist_names[] =
////    { "VTX_XY", "VTX_YZ", "VTX_XZ", "DCA_Phi_Arm", "DCA_Pz_Arm",
////        "FVTX_DCA_Phi_Arm", "FVTX_DCA_Pz_Arm", "DCAR_Diff_Dimuon",
////        "DCAR_Sum_Dimuon", "DCAZ_Diff_Dimuon", "DCAZ_Sum_Dimuon" };
//  const char * hist_names[] =
//    { "VTX_XY", "VTX_YZ", "VTX_XZ", "DCA_Phi_Arm", "DCA_Pz_Arm",
//        "FVTX_DCA_Phi_Arm", "FVTX_DCA_Pz_Arm" };
////      MergeHistSUM(
////          "/phenix/u/jinhuang/links/fvtx_data/taxi/Run12pp510MuonPro100/4117/dca_hist/dca_hist2.lst",
////          4, hist_names);
//  MergeHistSUM(list_file, 7, hist_names);

//    GetBeamPos();

//  DrawDCA();
//  DrawDCA_Pz();
//  DrawDCA_Dimuon();

//  DrawDCA_FVTX();
//  DrawDCA_Pz_FVTX();

//  DrawDCAPHI_FVTX();
//  DrawDCAPHI();
}

void
GetBeamPos_Fit_Run12pp()
{

  TFile *_file0 =
      TFile::Open(
          "/phenix/u/jinhuang/links/fvtx_data/taxi/Run12pp510MuonPro100/4117/se-ALL.lst.root");
//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv18_LooseVTX/MWG_MU.lst_vtx_hist.root");
//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv16_LooseVTX/MWG_MU.lst_vtx_hist.root");
  const double shift_x = 3.86259e-03;
  const double shift_y = 4.26060e-03;

  saHist * VTX_XY_saHist = (saHist *) _file0->GetObjectChecked("VTX_XY_saHist",
      "saHist");
  assert(VTX_XY_saHist);

  VTX_XY_saHist->AutoLoad();

  double run[10000] =
    { 0 };
  double bx[10000] =
    { 0 };
  double bxe[10000] =
    { 0 };
  double by[10000] =
    { 0 };
  double bye[10000] =
    { 0 };

  saHist::n_list run_list = VTX_XY_saHist->get_run_list();

  int cnt = 0;
  int cnt_total = 0;
  for (saHist::n_list::const_iterator it = run_list.begin();
      it != run_list.end(); ++it)
    {
      const int run_num = *it;
      cout << "Processing run " << run_num << endl;

      cnt_total++;

      saHist * saRun = VTX_XY_saHist->getsaHist_run(run_num);
      TH2F * hsum = (TH2F *) saRun->getHisto("SUM");

      assert(hsum);

      TF1 * fx = new TF1(TString("gausx_") + hsum->GetTitle(), "gaus",
          0.12 - .1, 0.12 + .1);
      fx->SetParameters(1, .12, .02);

      TF1 * fy = new TF1(TString("gausy_") + hsum->GetTitle(), "gaus",
          -.17 - .1, -.17 + .1);
      fy->SetParameters(1, -.17, .02);

      // For X/Y swapped plots
      hsum->ProjectionY()->Fit(fx, "MRQ");
      hsum->ProjectionX()->Fit(fy, "MRQ");

      if (fx->GetParError(1) > 0 && fx->GetParError(1) < 30e-4
          && fy->GetParError(1) > 0 && fy->GetParError(1) < 30e-4)
        {
          run[cnt] = run_num;
          bx[cnt] = fx->GetParameter(1);
          bxe[cnt] = fx->GetParError(1);
          by[cnt] = fy->GetParameter(1);
          bye[cnt] = fy->GetParError(1);

          cnt++;
        }
      else
        {
          cout << "Low stat run " << run_num << " : fx->GetParError(1) = "
              << fx->GetParError(1) << " , fy->GetParError(1) = "
              << fy->GetParError(1) << endl;
        }
    }

  TGraphErrors * gex = new TGraphErrors(cnt, run, bx, 0, bxe);
  TGraphErrors * gey = new TGraphErrors(cnt, run, by, 0, bye);

  string f_pos_name = string(_file0->GetName()) + string(".BeamPosTest");
  fstream f_pos(f_pos_name.c_str(), ios_base::out);
  for (int i = 0; i < cnt; i++)
    {

      cout << (run[i]) << "\t" << bx[i] << "\t" << by[i] << "\t" << bxe[i]
          << "\t" << bye[i] << endl;
      f_pos << (run[i]) << "\t" << bx[i] + shift_x << "\t" << by[i] + shift_y
          << "\t" << bxe[i] << "\t" << bye[i] << endl;

    }

  cout << "Processed run " << cnt << "/" << cnt_total << endl;

  f_pos.close();

  ///////////////////////

  TCanvas *c1 = new TCanvas("GetBeamPos", "GetBeamPos", 1800, 900);
  c1->Divide(2, 1);
  int idx = 1;
  TVirtualPad * p;

  p = c1->cd(idx++);
  c1->Update();

  gex->Draw("*al");

  p = c1->cd(idx++);
  c1->Update();

  gey->Draw("*al");

  c1->Paint();

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()),
      kFALSE);

  cout << "Beam pos file: " << f_pos_name << endl;
}

void
GetBeamPos_Fit_Run15pp(
    TString hist_file =
        "/phenix/u/jinhuang/links/fvtx_data/taxi/Run15pp200MuonsMUPro104/6729/ReviewPass/se-ALL.lst.root")
{

  TFile *_file0 = TFile::Open(hist_file);
//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv18_LooseVTX/MWG_MU.lst_vtx_hist.root");
//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv16_LooseVTX/MWG_MU.lst_vtx_hist.root");
  const double shift_x = 0;
  const double shift_y = 0;

  TH1F * h_xerr = new TH1F("h_xerr", "Error Distribution; log_{10}(Err X (cm))",
      100, -7, 0);
  TH1F * h_yerr = new TH1F("h_yerr", "Error Distribution; log_{10}(Err Y (cm))",
      100, -7, 0);

  const double beamcenter_x = 0.17;
  const double beamcenter_y = 0.07;

  saHist * VTX_XY_saHist = (saHist *) _file0->GetObjectChecked("VTX_XY_saHist",
      "saHist");
  assert(VTX_XY_saHist);

  VTX_XY_saHist->AutoLoad();

  double run[10000] =
    { 0 };
  double bx[10000] =
    { 0 };
  double bxe[10000] =
    { 0 };
  double by[10000] =
    { 0 };
  double bye[10000] =
    { 0 };

  saHist::n_list run_list = VTX_XY_saHist->get_run_list();

  int cnt = 0;
  int cnt_total = 0;
  for (saHist::n_list::const_iterator it = run_list.begin();
      it != run_list.end(); ++it)
    {
      const int run_num = *it;
      cout << "Processing run " << run_num << endl;

      cnt_total++;

      saHist * saRun = VTX_XY_saHist->getsaHist_run(run_num);
      TH2F * hsum = (TH2F *) saRun->getHisto("SUM");

      assert(hsum);

      TF1 * fx = new TF1(TString("gausx_") + hsum->GetTitle(), "gaus",
          beamcenter_x - .1, beamcenter_x + .1);
      fx->SetParameters(1, beamcenter_x, .02);

      TF1 * fy = new TF1(TString("gausy_") + hsum->GetTitle(), "gaus",
          beamcenter_y - .1, beamcenter_y + .1);
      fy->SetParameters(1, beamcenter_y, .02);

//      // For X/Y swapped plots
//      hsum->ProjectionY()->Fit(fx, "MRQ");
//      hsum->ProjectionX()->Fit(fy, "MRQ");

      hsum->ProjectionX()->Fit(fx, "MRQ");
      hsum->ProjectionY()->Fit(fy, "MRQ");

      TF1 * fx2 = new TF1(TString("gausx2_") + hsum->GetTitle(),
          "[0]*exp(-0.5*((x-[1])/[2])**2) + [3]*exp(-0.5*((x-[1])/[4])**2) ",
          fx->GetParameter(1) - .1, fx->GetParameter(1) + .1);
      fx2->SetParameters(fx->GetParameter(0) / 2, fx->GetParameter(1),
          fx->GetParameter(2) / 2, fx->GetParameter(0) / 2,
          fx->GetParameter(2) * 2);

      TF1 * fy2 = new TF1(TString("gausy2_") + hsum->GetTitle(),
          "[0]*exp(-0.5*((x-[1])/[2])**2) + [3]*exp(-0.5*((x-[1])/[4])**2)",
          fy->GetParameter(1) - .1, fy->GetParameter(1) + .1);
      fy2->SetParameters(fy->GetParameter(0) / 2, fy->GetParameter(1),
          fy->GetParameter(2) / 2, fy->GetParameter(0) / 2,
          fy->GetParameter(2) * 2);

      hsum->ProjectionX()->Fit(fx2, "MRQ");
      hsum->ProjectionY()->Fit(fy2, "MRQ");

      h_xerr->Fill(log10(fx2->GetParError(1)));
      h_yerr->Fill(log10(fy2->GetParError(1)));

      if (fx2->GetParError(1) > 0 && fx2->GetParError(1) < 30e-4
          && fy2->GetParError(1) > 0 && fy2->GetParError(1) < 30e-4)
        {
          run[cnt] = run_num;
          bx[cnt] = fx2->GetParameter(1);
          bxe[cnt] = fx2->GetParError(1);
          by[cnt] = fy2->GetParameter(1);
          bye[cnt] = fy2->GetParError(1);

          cnt++;

          if (fx2->GetParError(1) > 5e-4 || fy2->GetParError(1) > 5e-4)
            cout << "Slightly Low stat run " << run_num
                << " : fx2->GetParError(1) = " << fx2->GetParError(1)
                << " , fy2->GetParError(1) = " << fy2->GetParError(1)
                << " : fx->GetParError(1) = " << fx->GetParError(1)
                << " , fy->GetParError(1) = " << fy->GetParError(1) << endl;
        }
      else
        {
          cout << "Low stat run " << run_num << " : fx2->GetParError(1) = "
              << fx2->GetParError(1) << " , fy2->GetParError(1) = "
              << fy2->GetParError(1) << " : fx->GetParError(1) = "
              << fx->GetParError(1) << " , fy->GetParError(1) = "
              << fy->GetParError(1) << endl;
        }

//            fx2->Print();
//            return;
    }

  TGraphErrors * gex = new TGraphErrors(cnt, run, bx, 0, bxe);
  TGraphErrors * gey = new TGraphErrors(cnt, run, by, 0, bye);

  string f_pos_name = string(_file0->GetName()) + string(".BeamPosTest");
  fstream f_pos(f_pos_name.c_str(), ios_base::out);
  for (int i = 0; i < cnt; i++)
    {

      cout << (run[i]) << "\t" << bx[i] << "\t" << by[i] << "\t" << bxe[i]
          << "\t" << bye[i] << endl;
      f_pos << (run[i]) << "\t" << bx[i] + shift_x << "\t" << by[i] + shift_y
          << "\t" << bxe[i] << "\t" << bye[i] << endl;

    }

  cout << "Processed run " << cnt << "/" << cnt_total << endl;

  f_pos.close();

  ///////////////////////

  TCanvas *c1 = new TCanvas("GetBeamPos", "GetBeamPos", 1800, 1000);
  c1->Divide(2, 2);
  int idx = 1;
  TVirtualPad * p;

  p = c1->cd(idx++);
  c1->Update();

  gex->Draw("*al");

  p = c1->cd(idx++);
  c1->Update();

  gey->Draw("*al");

  c1->Paint();

  p = c1->cd(idx++);
  c1->Update();
  p->SetLogy();
  h_xerr->Draw("");
  p = c1->cd(idx++);
  c1->Update();
  p->SetLogy();
  h_yerr->Draw("");

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()),
      kFALSE);

  cout << "Beam pos file: " << f_pos_name << endl;
}

void
GetBeamPos_Fit_WithSlope_Run15pAu(
    TString hist_file =
        "/phenix/u/jinhuang/links/fvtx_data/taxi/Run15pp200MuonsMUPro104/6729/ReviewPass/se-ALL.lst.root")
{

  TFile *_file0 = TFile::Open(hist_file);

  // how much z bin are rebined
  const int z_rebin = 10;

//  //after burner corrections OFF
//  bool constraint_beam_slope = false;
//  const double shift_x = 0;
//  const double shift_y = 0;
//  const double shift_beam_dxdz = -0;
//  const double shift_beam_dydz = 0;
  //

  bool constraint_beam_slope = false;
  // Correction based on /phenix/u/jinhuang/links/fvtx_data/taxi/Run15pAu200MuonsMUPro105/8327_BeamAvg/dca_hist.root.lst.root
  const double shift_x = 0.00365842;
  const double shift_y = 0.000786397;
  const double shift_beam_dxdz = 5.36731e-06;
  const double shift_beam_dydz = 0.000351887;

//  // For un15 pp
//  bool constraint_beam_slope = true;
//  const double beam_dxdz = 0.000354242; //  +/-   1.09664e-06 ;
//  const double beam_dydz = -0.00029179; //  +/-   8.45421e-07 ;
//
//  // Correction based on /gpfs/mnt/gpfs02/phenix/fvtx/subsys/fvtx/jinhuang/taxi/Run15pp200MuonsMUPro105/8065_Iter1/dca_hist.root.lst.root
//  const double shift_x = 0.00422933;
//  const double shift_y = 0.0010499;
//  const double shift_beam_dxdz = -0.000379005;
//  const double shift_beam_dydz = 0.000295145;
//

//  //seed
//  const double beamcenter_x = 0.17;
//  const double beamcenter_y = 0.07;
  //seed
  const double beamcenter_x = 0.201362;
  const double beamcenter_y = 0.07;

  TH1F * h_xerr = new TH1F("h_xerr", "Error Distribution; log_{10}(Err X (cm))",
      100, -7, 0);
  TH1F * h_yerr = new TH1F("h_yerr", "Error Distribution; log_{10}(Err Y (cm))",
      100, -7, 0);
  TH1F * h_dxerr = new TH1F("h_dxerr",
      "Error Distribution; log_{10}(Err dX/dZ (cm))", 100, -7, 0);
  TH1F * h_dyerr = new TH1F("h_dyerr",
      "Error Distribution; log_{10}(Err dY/dZ (cm))", 100, -7, 0);

  saHist * VTX_XZ_saHist = (saHist *) _file0->GetObjectChecked("VTX_XZ_saHist",
      "saHist");
  assert(VTX_XZ_saHist);
  saHist * VTX_YZ_saHist = (saHist *) _file0->GetObjectChecked("VTX_YZ_saHist",
      "saHist");
  assert(VTX_YZ_saHist);

  VTX_XZ_saHist->AutoLoad();
  VTX_YZ_saHist->AutoLoad();

  double run[10000] =
    { 0 };
  double bx[10000] =
    { 0 };
  double bxe[10000] =
    { 0 };
  double by[10000] =
    { 0 };
  double bye[10000] =
    { 0 };
  double bdx[10000] =
    { 0 };
  double bdxe[10000] =
    { 0 };
  double bdy[10000] =
    { 0 };
  double bdye[10000] =
    { 0 };

  saHist::n_list run_list = VTX_XY_saHist->get_run_list();

  int cnt = 0;
  int cnt_total = 0;
  for (saHist::n_list::const_iterator it = run_list.begin();
      it != run_list.end(); ++it)
    {
      const int run_num = *it;
//  {
//    const int run_num =434340;
//    const int run_num =433028;

      cout << "Processing run " << run_num << endl;

      cnt_total++;

      saHist * saRun_XZ = VTX_XZ_saHist->getsaHist_run(run_num);
      saHist * saRun_YZ = VTX_YZ_saHist->getsaHist_run(run_num);

      assert(saRun_XZ);
      assert(saRun_YZ);

      TH2F * hsum_XZ = (TH2F *) saRun_XZ->getHisto("SUM");
      TH2F * hsum_YZ = (TH2F *) saRun_YZ->getHisto("SUM");

      assert(hsum_XZ);
      assert(hsum_YZ);

      hsum_XZ->Rebin2D(z_rebin, 1);
      hsum_YZ->Rebin2D(z_rebin, 1);

      const
      int N_zbins = 200 / z_rebin;
      TGraphErrors * gex = new TGraphErrors(N_zbins);
      TGraphErrors * gey = new TGraphErrors(N_zbins);

      int i_zbins = 0;
      for (int i = 1; i <= hsum_XZ->GetNbinsX(); ++i)
        {
          const double z = hsum_XZ->GetXaxis()->GetBinCenter(i);
          if (fabs(z) > 10)
            continue;

          assert(i_zbins<N_zbins);

          TH1D * projx = hsum_XZ->ProjectionY("projx", i, i);
          TH1D * projy = hsum_YZ->ProjectionY("projy", i, i);

          TF1 * fx = new TF1(TString("gausx_") + hsum_XZ->GetTitle(), "gaus",
              beamcenter_x - .1, beamcenter_x + .1);
          fx->SetParameters(1, beamcenter_x, .02);

          TF1 * fy = new TF1(TString("gausy_") + hsum_YZ->GetTitle(), "gaus",
              beamcenter_y - .1, beamcenter_y + .1);
          fy->SetParameters(1, beamcenter_y, .02);

//          cout <<"Process first fit z = "<<z<<" at bin "<<i<<" projx = "<<projx->GetSum()<<" projy = "<<projy->GetSum()<<endl;
          projx->Fit(fx, "MRQ");
          projy->Fit(fy, "MRQ");

          TF1 * fx2 =
              new TF1(TString("gausx2_") + hsum_XZ->GetTitle(),
                  "[0]*exp(-0.5*((x-[1])/[2])**2) + [3]*exp(-0.5*((x-[1])/[4])**2) ",
                  fx->GetParameter(1) - .1, fx->GetParameter(1) + .1);
          fx2->SetParameters(fx->GetParameter(0) / 2, fx->GetParameter(1),
              fx->GetParameter(2) / 2, fx->GetParameter(0) / 2,
              fx->GetParameter(2) * 2);

          TF1 * fy2 = new TF1(TString("gausy2_") + hsum_YZ->GetTitle(),
              "[0]*exp(-0.5*((x-[1])/[2])**2) + [3]*exp(-0.5*((x-[1])/[4])**2)",
              fy->GetParameter(1) - .1, fy->GetParameter(1) + .1);
          fy2->SetParameters(fy->GetParameter(0) / 2, fy->GetParameter(1),
              fy->GetParameter(2) / 2, fy->GetParameter(0) / 2,
              fy->GetParameter(2) * 2);

//          cout <<"Process second fit z = "<<z<<" at bin "<<i<<endl;
          projx->Fit(fx2, "MRQ");
          projy->Fit(fy2, "MRQ");

          (gex->GetX())[i_zbins] = z;
          (gey->GetX())[i_zbins] = z;
          (gex->GetY())[i_zbins] = fx2->GetParameter(1);
          (gex->GetEY())[i_zbins] = fx2->GetParError(1);
          (gey->GetY())[i_zbins] = fy2->GetParameter(1);
          (gey->GetEY())[i_zbins] = fy2->GetParError(1);

          i_zbins++;
        }
      assert(i_zbins==N_zbins);

      TF1 * fx = new TF1(TString("pol1x_") + hsum_XZ->GetTitle(), "pol1", -10,
          10);
      fx->SetParameters(beamcenter_x, .0);

      TF1 * fy = new TF1(TString("pol1y_") + hsum_YZ->GetTitle(), "pol1", -10,
          10);
      fy->SetParameters(beamcenter_y, .0);

      if (constraint_beam_slope)
        {
          fx->FixParameter(1, beam_dxdz);
          fy->FixParameter(1, beam_dydz);
        }

      gex->Fit(fx, "MRQ");
      gey->Fit(fy, "MRQ");

      h_xerr->Fill(log10(fx->GetParError(0)));
      h_yerr->Fill(log10(fy->GetParError(0)));
      h_dxerr->Fill(log10(fx->GetParError(1)));
      h_dyerr->Fill(log10(fy->GetParError(1)));
//
      if (fx->GetParError(0) > 0 && fx->GetParError(0) < 30e-4
          && fy->GetParError(0) > 0 && fy->GetParError(0) < 30e-4)
        {
          run[cnt] = run_num;
          bx[cnt] = fx->GetParameter(0);
          bxe[cnt] = fx->GetParError(0);
          by[cnt] = fy->GetParameter(0);
          bye[cnt] = fy->GetParError(0);
          bdx[cnt] = fx->GetParameter(1);
          bdxe[cnt] = fx->GetParError(1);
          bdy[cnt] = fy->GetParameter(1);
          bdye[cnt] = fy->GetParError(1);

          cnt++;

          if (fx->GetParError(0) > 5e-4 || fy->GetParError(0) > 5e-4)
            cout << "Slightly Low stat run " << run_num
                << " : fx->GetParError(0) = " << fx->GetParError(0)
                << " , fy->GetParError(0) = " << fy->GetParError(0) << endl;
        }
      else
        {
          cout << "Low stat run " << run_num << " : fx->GetParError(0) = "
              << fx->GetParError(0) << " , fy->GetParError(0) = "
              << fy->GetParError(0) << endl;
        }
      
      if (0)
        { // run single fit and check
          new TCanvas("hsum_XZ", "hsum_XZ");
          hsum_XZ->Draw("colz");
          gex->Draw("*e");
          fx->Draw("same");
          new TCanvas("hsum_YZ", "hsum_YZ");
          hsum_YZ->Draw("colz");
          gey->Draw("*e");
          fy->Draw("same");

          fx->Print();
          fy->Print();
          return;
        }
    }

  TGraphErrors * gex = new TGraphErrors(cnt, run, bx, 0, bxe);
  TGraphErrors * gey = new TGraphErrors(cnt, run, by, 0, bye);
  TGraphErrors * gedx = new TGraphErrors(cnt, run, bdx, 0, bdxe);
  TGraphErrors * gedy = new TGraphErrors(cnt, run, bdy, 0, bdye);

  string f_pos_name = string(_file0->GetName()) + string(".BeamPosTest");
  fstream f_pos(f_pos_name.c_str(), ios_base::out);
  for (int i = 0; i < cnt; i++)
    {

      cout << (run[i]) << "\t" << bx[i] << "\t" << by[i] << "\t" << bxe[i]
          << "\t" << bye[i] << endl;
      f_pos << (run[i]) << "\t"
      //
          << bx[i] + shift_x << "\t"
          //
          << by[i] + shift_y << "\t"
          //
          << bxe[i] << "\t" << bye[i] << "\t" //
          << bdx[i] + shift_beam_dxdz << "\t" //
          << bdy[i] + shift_beam_dydz << "\t" //
          << bdxe[i] << "\t" << bdye[i] << endl;

    }

  cout << "Processed run " << cnt << "/" << cnt_total << endl;

  f_pos.close();

  ///////////////////////

  TCanvas *c1 = new TCanvas("GetBeamPos", "GetBeamPos", 1800, 1000);
  c1->Divide(4, 2);
  int idx = 1;
  TVirtualPad * p;

  p = c1->cd(idx++);
  c1->Update();
  gex->Draw("*al");
  gex->Fit("pol0");

  p = c1->cd(idx++);
  c1->Update();
  gey->Draw("*al");
  gey->Fit("pol0");

  p = c1->cd(idx++);
  c1->Update();
  gedx->Draw("*al");
  gedx->Fit("pol0");

  p = c1->cd(idx++);
  c1->Update();
  gedy->Draw("*al");
  gedy->Fit("pol0");

  p = c1->cd(idx++);
  c1->Update();
  p->SetLogy();
  h_xerr->Draw("");
  p = c1->cd(idx++);
  c1->Update();
  p->SetLogy();
  h_yerr->Draw("");

  p = c1->cd(idx++);
  c1->Update();
  p->SetLogy();
  h_dxerr->Draw("");
  p = c1->cd(idx++);
  c1->Update();
  p->SetLogy();
  h_dyerr->Draw("");

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()),
      kFALSE);

  cout << "Beam pos file: " << f_pos_name << endl;
}


void
GetBeamPos_Fit_WithSlope_Run15pp(
    TString hist_file =
        "/phenix/u/jinhuang/links/fvtx_data/taxi/Run15pp200MuonsMUPro104/6729/ReviewPass/se-ALL.lst.root")
{

  TFile *_file0 = TFile::Open(hist_file);

  //after burner corrections OFF
  bool constraint_beam_slope = false;
  const double shift_x = 0;
  const double shift_y = 0;
  const double shift_beam_dxdz = -0;
  const double shift_beam_dydz = 0;
  //

//  // For un15 pp
//  bool constraint_beam_slope = true;
//  const double beam_dxdz = 0.000354242; //  +/-   1.09664e-06 ;
//  const double beam_dydz = -0.00029179; //  +/-   8.45421e-07 ;
//
//  // Correction based on /gpfs/mnt/gpfs02/phenix/fvtx/subsys/fvtx/jinhuang/taxi/Run15pp200MuonsMUPro105/8065_Iter1/dca_hist.root.lst.root
//  const double shift_x = 0.00422933;
//  const double shift_y = 0.0010499;
//  const double shift_beam_dxdz = -0.000379005;
//  const double shift_beam_dydz = 0.000295145;
//

  //seed
  const double beamcenter_x = 0.17;
  const double beamcenter_y = 0.07;

  TH1F * h_xerr = new TH1F("h_xerr", "Error Distribution; log_{10}(Err X (cm))",
      100, -7, 0);
  TH1F * h_yerr = new TH1F("h_yerr", "Error Distribution; log_{10}(Err Y (cm))",
      100, -7, 0);
  TH1F * h_dxerr = new TH1F("h_dxerr",
      "Error Distribution; log_{10}(Err dX/dZ (cm))", 100, -7, 0);
  TH1F * h_dyerr = new TH1F("h_dyerr",
      "Error Distribution; log_{10}(Err dY/dZ (cm))", 100, -7, 0);

  saHist * VTX_XZ_saHist = (saHist *) _file0->GetObjectChecked("VTX_XZ_saHist",
      "saHist");
  assert(VTX_XZ_saHist);
  saHist * VTX_YZ_saHist = (saHist *) _file0->GetObjectChecked("VTX_YZ_saHist",
      "saHist");
  assert(VTX_YZ_saHist);

  VTX_XZ_saHist->AutoLoad();
  VTX_YZ_saHist->AutoLoad();

  double run[10000] =
    { 0 };
  double bx[10000] =
    { 0 };
  double bxe[10000] =
    { 0 };
  double by[10000] =
    { 0 };
  double bye[10000] =
    { 0 };
  double bdx[10000] =
    { 0 };
  double bdxe[10000] =
    { 0 };
  double bdy[10000] =
    { 0 };
  double bdye[10000] =
    { 0 };

  saHist::n_list run_list = VTX_XY_saHist->get_run_list();

  int cnt = 0;
  int cnt_total = 0;
  for (saHist::n_list::const_iterator it = run_list.begin();
      it != run_list.end(); ++it)
    {
      const int run_num = *it;

      cout << "Processing run " << run_num << endl;

      cnt_total++;

      saHist * saRun_XZ = VTX_XZ_saHist->getsaHist_run(run_num);
      saHist * saRun_YZ = VTX_YZ_saHist->getsaHist_run(run_num);

      assert(saRun_XZ);
      assert(saRun_YZ);

      TH2F * hsum_XZ = (TH2F *) saRun_XZ->getHisto("SUM");
      TH2F * hsum_YZ = (TH2F *) saRun_YZ->getHisto("SUM");

      assert(hsum_XZ);
      assert(hsum_YZ);

      hsum_XZ->Rebin2D(50, 1);
      hsum_YZ->Rebin2D(50, 1);

      const
      int N_zbins = 200 / 50;
      TGraphErrors * gex = new TGraphErrors(N_zbins);
      TGraphErrors * gey = new TGraphErrors(N_zbins);

      int i_zbins = 0;
      for (int i = 1; i <= hsum_XZ->GetNbinsX(); ++i)
        {
          const double z = hsum_XZ->GetXaxis()->GetBinCenter(i);
          if (fabs(z) > 10)
            continue;

          assert(i_zbins<N_zbins);

          TH1D * projx = hsum_XZ->ProjectionY("projx", i, i);
          TH1D * projy = hsum_YZ->ProjectionY("projy", i, i);

          TF1 * fx = new TF1(TString("gausx_") + hsum_XZ->GetTitle(), "gaus",
              beamcenter_x - .1, beamcenter_x + .1);
          fx->SetParameters(1, beamcenter_x, .02);

          TF1 * fy = new TF1(TString("gausy_") + hsum_YZ->GetTitle(), "gaus",
              beamcenter_y - .1, beamcenter_y + .1);
          fy->SetParameters(1, beamcenter_y, .02);

//          cout <<"Process first fit z = "<<z<<" at bin "<<i<<" projx = "<<projx->GetSum()<<" projy = "<<projy->GetSum()<<endl;
          projx->Fit(fx, "MRQ");
          projy->Fit(fy, "MRQ");

          TF1 * fx2 =
              new TF1(TString("gausx2_") + hsum_XZ->GetTitle(),
                  "[0]*exp(-0.5*((x-[1])/[2])**2) + [3]*exp(-0.5*((x-[1])/[4])**2) ",
                  fx->GetParameter(1) - .1, fx->GetParameter(1) + .1);
          fx2->SetParameters(fx->GetParameter(0) / 2, fx->GetParameter(1),
              fx->GetParameter(2) / 2, fx->GetParameter(0) / 2,
              fx->GetParameter(2) * 2);

          TF1 * fy2 = new TF1(TString("gausy2_") + hsum_YZ->GetTitle(),
              "[0]*exp(-0.5*((x-[1])/[2])**2) + [3]*exp(-0.5*((x-[1])/[4])**2)",
              fy->GetParameter(1) - .1, fy->GetParameter(1) + .1);
          fy2->SetParameters(fy->GetParameter(0) / 2, fy->GetParameter(1),
              fy->GetParameter(2) / 2, fy->GetParameter(0) / 2,
              fy->GetParameter(2) * 2);

//          cout <<"Process second fit z = "<<z<<" at bin "<<i<<endl;
          projx->Fit(fx2, "MRQ");
          projy->Fit(fy2, "MRQ");

          (gex->GetX())[i_zbins] = z;
          (gey->GetX())[i_zbins] = z;
          (gex->GetY())[i_zbins] = fx2->GetParameter(1);
          (gex->GetEY())[i_zbins] = fx2->GetParError(1);
          (gey->GetY())[i_zbins] = fy2->GetParameter(1);
          (gey->GetEY())[i_zbins] = fy2->GetParError(1);

          i_zbins++;
        }
      assert(i_zbins==N_zbins);

      TF1 * fx = new TF1(TString("pol1x_") + hsum_XZ->GetTitle(), "pol1", -10,
          10);
      fx->SetParameters(beamcenter_x, .0);

      TF1 * fy = new TF1(TString("pol1y_") + hsum_YZ->GetTitle(), "pol1", -10,
          10);
      fy->SetParameters(beamcenter_y, .0);

      if (constraint_beam_slope)
        {
          fx->FixParameter(1, beam_dxdz);
          fy->FixParameter(1, beam_dydz);
        }

      gex->Fit(fx, "MRQ");
      gey->Fit(fy, "MRQ");

      h_xerr->Fill(log10(fx->GetParError(0)));
      h_yerr->Fill(log10(fy->GetParError(0)));
      h_dxerr->Fill(log10(fx->GetParError(1)));
      h_dyerr->Fill(log10(fy->GetParError(1)));
//
      if (fx->GetParError(0) > 0 && fx->GetParError(0) < 30e-4
          && fy->GetParError(0) > 0 && fy->GetParError(0) < 30e-4)
        {
          run[cnt] = run_num;
          bx[cnt] = fx->GetParameter(0);
          bxe[cnt] = fx->GetParError(0);
          by[cnt] = fy->GetParameter(0);
          bye[cnt] = fy->GetParError(0);
          bdx[cnt] = fx->GetParameter(1);
          bdxe[cnt] = fx->GetParError(1);
          bdy[cnt] = fy->GetParameter(1);
          bdye[cnt] = fy->GetParError(1);

          cnt++;

          if (fx->GetParError(0) > 5e-4 || fy->GetParError(0) > 5e-4)
            cout << "Slightly Low stat run " << run_num
                << " : fx->GetParError(0) = " << fx->GetParError(0)
                << " , fy->GetParError(0) = " << fy->GetParError(0) << endl;
        }
      else
        {
          cout << "Low stat run " << run_num << " : fx->GetParError(0) = "
              << fx->GetParError(0) << " , fy->GetParError(0) = "
              << fy->GetParError(0) << endl;
        }

        { // run single fit and check
          new TCanvas("hsum_XZ", "hsum_XZ");
          hsum_XZ->Draw("colz");
          gex->Draw("*e");
          fx->Draw("same");
          new TCanvas("hsum_YZ", "hsum_YZ");
          hsum_YZ->Draw("colz");
          gey->Draw("*e");
          fy->Draw("same");

          fx->Print();
          fy->Print();
          return;
        }
    }

  TGraphErrors * gex = new TGraphErrors(cnt, run, bx, 0, bxe);
  TGraphErrors * gey = new TGraphErrors(cnt, run, by, 0, bye);
  TGraphErrors * gedx = new TGraphErrors(cnt, run, bdx, 0, bdxe);
  TGraphErrors * gedy = new TGraphErrors(cnt, run, bdy, 0, bdye);

  string f_pos_name = string(_file0->GetName()) + string(".BeamPosTest");
  fstream f_pos(f_pos_name.c_str(), ios_base::out);
  for (int i = 0; i < cnt; i++)
    {

      cout << (run[i]) << "\t" << bx[i] << "\t" << by[i] << "\t" << bxe[i]
          << "\t" << bye[i] << endl;
      f_pos << (run[i]) << "\t"
      //
          << bx[i] + shift_x << "\t"
          //
          << by[i] + shift_y << "\t"
          //
          << bxe[i] << "\t" << bye[i] << "\t" //
          << bdx[i] + shift_beam_dxdz << "\t" //
          << bdy[i] + shift_beam_dydz << "\t" //
          << bdxe[i] << "\t" << bdye[i] << endl;

    }

  cout << "Processed run " << cnt << "/" << cnt_total << endl;

  f_pos.close();

  ///////////////////////

  TCanvas *c1 = new TCanvas("GetBeamPos", "GetBeamPos", 1800, 1000);
  c1->Divide(4, 2);
  int idx = 1;
  TVirtualPad * p;

  p = c1->cd(idx++);
  c1->Update();
  gex->Draw("*al");
  gex->Fit("pol0");

  p = c1->cd(idx++);
  c1->Update();
  gey->Draw("*al");
  gey->Fit("pol0");

  p = c1->cd(idx++);
  c1->Update();
  gedx->Draw("*al");
  gedx->Fit("pol0");

  p = c1->cd(idx++);
  c1->Update();
  gedy->Draw("*al");
  gedy->Fit("pol0");

  p = c1->cd(idx++);
  c1->Update();
  p->SetLogy();
  h_xerr->Draw("");
  p = c1->cd(idx++);
  c1->Update();
  p->SetLogy();
  h_yerr->Draw("");

  p = c1->cd(idx++);
  c1->Update();
  p->SetLogy();
  h_dxerr->Draw("");
  p = c1->cd(idx++);
  c1->Update();
  p->SetLogy();
  h_dyerr->Draw("");

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()),
      kFALSE);

  cout << "Beam pos file: " << f_pos_name << endl;
}



void
DrawVertex_XZ_YZ()
{
  const double beamcenter_x = 0.17;
  const double beamcenter_y = 0.07;

//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv18_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
  assert(_file0);

  TH2F * VTX_XZ_SUM = (TH2F *) _file0->GetObjectChecked("VTX_XZ_SUM", "TH2F");
  assert(VTX_XZ_SUM);

  TH2F * VTX_YZ_SUM = (TH2F *) _file0->GetObjectChecked("VTX_YZ_SUM", "TH2F");
  assert(VTX_YZ_SUM);

///////////////////////

  TCanvas *c1 = new TCanvas("DrawVertex_XZ_YZ", "DrawVertex_XZ_YZ", 1800, 900);
  c1->Divide(2, 1);
  int idx = 1;
  TVirtualPad * p;

  p = c1->cd(idx++);
  c1->Update();

  TGraphErrors * ge = FitProfile(VTX_XZ_SUM);
  VTX_XZ_SUM->Draw("colz");
  ge->Draw("*");
  VTX_XZ_SUM->GetYaxis()->SetRangeUser(beamcenter_x - .05, beamcenter_x + .05);

  p = c1->cd(idx++);
  c1->Update();

  TGraphErrors * ge = FitProfile(VTX_YZ_SUM);
  VTX_YZ_SUM->Draw("colz");
  ge->Draw("*");
  VTX_YZ_SUM->GetYaxis()->SetRangeUser(beamcenter_y - .05, beamcenter_y + .05);

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()),
      kFALSE);

}

void
DrawDCA_Z()
{

//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv16_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv18_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
  assert(_file0);

  TH3F * DCA_Phi_z_Arm0_SUM = (TH3F *) _file0->GetObjectChecked(
      "DCA_Phi_z_Arm0_SUM", "TH3F");
  assert(DCA_Phi_z_Arm0_SUM);

  TH3F * DCA_Phi_z_Arm1_SUM = (TH3F *) _file0->GetObjectChecked(
      "DCA_Phi_z_Arm1_SUM", "TH3F");
  assert(DCA_Phi_z_Arm1_SUM);

  TF1 * fup = new TF1("up", "pol0", -3.14159, 3.14159);
  TF1 * fdown = new TF1("up", "pol0", -3.14159, 3.14159);

  fup->SetParameter(0, +80e-4);
  fdown->SetParameter(0, -80e-4);
  fup->SetLineColor(kMagenta);
  fdown->SetLineColor(kMagenta);

  TH3F * DCA_Phi_z_Arm_SUM[2] =
    { DCA_Phi_z_Arm0_SUM, DCA_Phi_z_Arm1_SUM };

  double zs[2][100] =
    { 0 };
  double xs[2][100] =
    { 0 };
  double exs[2][100] =
    { 0 };
  double ys[2][100] =
    { 0 };
  double eys[2][100] =
    { 0 };

///////////////////////

  TCanvas *c1 = new TCanvas("DrawDCA_Z", "DrawDCA_Z", 1800, 950);
  c1->Divide(4, 2);
  int idx = 1;
  TVirtualPad * p;

  for (int arm = 0; arm < 2; arm++)
    {

      for (int iz = 1; iz <= 4; iz++)
        {
          p = c1->cd(idx++);
          c1->Update();

          zs[arm][iz - 1] = DCA_Phi_z_Arm_SUM[arm]->GetZaxis()->GetBinCenter(
              iz);
          DCA_Phi_z_Arm_SUM[arm]->GetZaxis()->SetRange(iz, iz);
          TH2F * DCA_Phi_Arm_SUM = DCA_Phi_z_Arm_SUM[arm]->Project3D("xy");
          DCA_Phi_Arm_SUM->SetName(Form("DCA_Phi_Arm%d_z%d", arm, iz));

          TGraphErrors * ge = FitProfile(DCA_Phi_Arm_SUM);
          ge->SetMarkerStyle(20);
          DCA_Phi_Arm_SUM->GetYaxis()->SetRangeUser(-.1, .1);
          DCA_Phi_Arm_SUM->Draw("colz");

          //      TF1 * fmod = new TF1("fmod", "cos(x) ++ sin(x)", 0, 2 * 3.14159);
          //      TF1 * fmod = new TF1("fmod", "1 ++ cos(x) ++ sin(x)", 0, 2 * 3.14159);
          TF1 * fmod = new TF1("fmod", "1 ++ cos(x) ++ sin(x)", -3.14159 + .1,
              3.14159 - .1);
          fmod->SetParName(0, "Const Shift");
          fmod->SetParName(1, "Cosine mod");
          fmod->SetParName(2, "Sine mod");
          ge->Fit(fmod, "MR");

          xs[arm][iz - 1] = fmod->GetParameter(1);
          exs[arm][iz - 1] = fmod->GetParError(1);
          ys[arm][iz - 1] = fmod->GetParameter(2);
          eys[arm][iz - 1] = fmod->GetParError(2);

          fup->Draw("same");
          fdown->Draw("same");
          ge->Draw("p");
        }
    }

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()),
      kFALSE);

  TCanvas *c1 = new TCanvas("DrawDCA_Z_Slope", "DrawDCA_Z_Slope", 1800, 950);
  c1->Divide(2, 1);
  int idx = 1;
  TVirtualPad * p;

  p = c1->cd(idx++);
  c1->Update();
  p->DrawFrame(-15, -.05, 15, .05, "X DST slope;Z (cm);DCA (cm)");

  fup->Draw("same");
  fdown->Draw("same");

  TGraphErrors * gex0 = new TGraphErrors(4, zs[0], xs[0], 0, exs[0]);
  gex0->SetLineColor(kRed);
  gex0->SetMarkerColor(kRed);
  gex0->SetMarkerStyle(kFullCircle);
  gex0->Fit("pol1", "M");
  gex0->Draw("pe");

  TGraphErrors * gex1 = new TGraphErrors(4, zs[1], xs[1], 0, exs[1]);
  gex1->SetLineColor(kBlue);
  gex1->SetMarkerColor(kBlue);
  gex1->SetMarkerStyle(kFullSquare);
  gex1->Fit("pol1", "M");
  gex1->Draw("pe");

  p = c1->cd(idx++);
  c1->Update();
  p->DrawFrame(-15, -.05, 15, .05, "Y DST slope;Z (cm);DCA (cm)");

  fup->Draw("same");
  fdown->Draw("same");

  TGraphErrors * gey0 = new TGraphErrors(4, zs[0], ys[0], 0, eys[0]);
  gey0->SetLineColor(kRed);
  gey0->SetMarkerColor(kRed);
  gey0->SetMarkerStyle(kFullCircle);
  gey0->Fit("pol1", "M");
  gey0->Draw("pe");

  TGraphErrors * gey1 = new TGraphErrors(4, zs[1], ys[1], 0, eys[1]);
  gey1->SetLineColor(kBlue);
  gey1->SetMarkerColor(kBlue);
  gey1->SetMarkerStyle(kFullSquare);
  gey1->Fit("pol1", "M");
  gey1->Draw("pe");

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()),
      kFALSE);

  cout << "// Correction based on " << _file0->GetName() << endl;
  cout << "const double shift_x = "
      << 0.5
          * (((TF1 *) (gex0->GetListOfFunctions()->At(0)))->GetParameter(0)
              + ((TF1 *) (gex1->GetListOfFunctions()->At(0)))->GetParameter(0))
      << "; " << endl;
  cout << "const double shift_y = "
      << 0.5
          * (((TF1 *) (gey0->GetListOfFunctions()->At(0)))->GetParameter(0)
              + ((TF1 *) (gey1->GetListOfFunctions()->At(0)))->GetParameter(0))
      << "; " << endl;
  cout << "const double shift_beam_dxdz = "
      << 0.5
          * (((TF1 *) (gex0->GetListOfFunctions()->At(0)))->GetParameter(1)
              + ((TF1 *) (gex1->GetListOfFunctions()->At(0)))->GetParameter(1))
      << "; " << endl;
  cout << "const double shift_beam_dydz = "
      << 0.5
          * (((TF1 *) (gey0->GetListOfFunctions()->At(0)))->GetParameter(1)
              + ((TF1 *) (gey1->GetListOfFunctions()->At(0)))->GetParameter(1))
      << "; " << endl;

}

TGraphErrors *
FitProfile(const TH2F * h2)
{

  assert(h2);

  TProfile * p2 = h2->ProfileX();
  assert(p2);

  int n = 0;
  double x[1000];
  double ex[1000];
  double y[1000];
  double ey[1000];

  for (int i = 1; i <= h2->GetNbinsX(); i++)
    {
      TH1D * h1 = h2->ProjectionY(Form("htmp_%d", rand()), i, i);

      if (h1->GetSum() < 30)
        continue;

      TF1 fgaus("fgaus", "gaus", -p2->GetBinError(i) * 4,
          p2->GetBinError(i) * 4);

      TF1 f2(Form("dgaus"), "gaus + [3]*exp(-0.5*((x-[1])/[4])**2) + [5]",
          -p2->GetBinError(i) * 4, p2->GetBinError(i) * 4);

      fgaus.SetParameter(1, p2->GetBinContent(i));
      fgaus.SetParameter(2, p2->GetBinError(i));

      h1->Fit(&fgaus, "MQ0");

      f2.SetParameters(fgaus.GetParameter(0) / 2, fgaus.GetParameter(1),
          fgaus.GetParameter(2), fgaus.GetParameter(0) / 2,
          fgaus.GetParameter(2) / 4, 0);

      h1->Fit(&f2, "MQ0");

//      new TCanvas;
//      h1->Draw();
//      fgaus.Draw("same");
//      break;

      x[n] = p2->GetBinCenter(i);
      ex[n] = (p2->GetBinCenter(2) - p2->GetBinCenter(1)) / 2;
//      y[n] = fgaus.GetParameter(1);
//      ey[n] = fgaus.GetParError(1);
      y[n] = f2.GetParameter(1);
      ey[n] = f2.GetParError(1);

//      p2->SetBinContent(i, fgaus.GetParameter(1));
//      p2->SetBinError(i, fgaus.GetParameter(2));

      n++;
      delete h1;
    }

  TGraphErrors * ge = new TGraphErrors(n, x, y, ex, ey);
  ge->SetName(TString(h2->GetName()) + "_FitProfile");
  return ge;
}

void
DrawDCA()
{

//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv16_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv18_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
  assert(_file0);

  TH3F * DCA_Phi_Arm_SUM = (TH3F *) _file0->GetObjectChecked("DCA_Phi_Arm_SUM",
      "TH3F");
  assert(DCA_Phi_Arm_SUM);

  TH3F * DCA_Pz_Arm_SUM = (TH3F *) _file0->GetObjectChecked("DCA_Pz_Arm_SUM",
      "TH3F");
  assert(DCA_Pz_Arm_SUM);

  TH3F * FVTX_DCA_Phi_Arm_SUM = (TH3F *) _file0->GetObjectChecked(
      "FVTX_DCA_Phi_Arm_SUM", "TH3F");
  assert(FVTX_DCA_Phi_Arm_SUM);

  TH3F * FVTX_DCA_Pz_Arm_SUM = (TH3F *) _file0->GetObjectChecked(
      "FVTX_DCA_Pz_Arm_SUM", "TH3F");
  assert(FVTX_DCA_Pz_Arm_SUM);

//  DCA_Phi_Arm_SUM->GetZaxis()->
//  DCA_Phi_Arm_SUM

  TF1 * fup = new TF1("up", "pol0", -3.14159, 3.14159);
  TF1 * fdown = new TF1("up", "pol0", -3.14159, 3.14159);

  fup->SetParameter(0, +80e-4);
  fdown->SetParameter(0, -80e-4);
  fup->SetLineColor(kMagenta);
  fdown->SetLineColor(kMagenta);

///////////////////////

  TCanvas *c1 = new TCanvas("DrawDCA", "DrawDCA", 1800, 900);
  c1->Divide(2, 1);
  int idx = 1;
  TVirtualPad * p;

  p = c1->cd(idx++);
  c1->Update();

  DCA_Phi_Arm_SUM->GetZaxis()->SetRange(1, 1);
  TH2F * DCA_Phi_Arm_SUM_arm0 = DCA_Phi_Arm_SUM->Project3D("xy");
  DCA_Phi_Arm_SUM_arm0->SetName("DCA_Phi_Arm_SUM_arm0");

  TGraphErrors * ge = FitProfile(DCA_Phi_Arm_SUM_arm0);
  DCA_Phi_Arm_SUM_arm0->Draw("colz");
  ge->Draw("*");
  DCA_Phi_Arm_SUM_arm0->GetYaxis()->SetRangeUser(-.05, .05);

  TF1 * fmod0 = new TF1("fmod0", "1 ++ cos(x) ++ sin(x)", -3.14159, 3.14159);
  fmod0->SetParName(0, "Const Shift");
  fmod0->SetParName(1, "Cosine mod");
  fmod0->SetParName(2, "Sine mod");
  ge->Fit(fmod0, "MRq");

  fup->Draw("same");
  fdown->Draw("same");

  p = c1->cd(idx++);
  c1->Update();

  DCA_Phi_Arm_SUM->GetZaxis()->SetRange(2, 2);
  TH2F * DCA_Phi_Arm_SUM_arm1 = DCA_Phi_Arm_SUM->Project3D("xy");
  DCA_Phi_Arm_SUM_arm1->SetName("DCA_Phi_Arm_SUM_arm1");

  TGraphErrors * ge = FitProfile(DCA_Phi_Arm_SUM_arm1);
  DCA_Phi_Arm_SUM_arm1->Draw("colz");
  ge->Draw("*");
  DCA_Phi_Arm_SUM_arm1->GetYaxis()->SetRangeUser(-.05, .05);

  TF1 * fmod0 = new TF1("fmod0", "1 ++ cos(x) ++ sin(x)", -3.14159, 3.14159);
  fmod0->SetParName(0, "Const Shift");
  fmod0->SetParName(1, "Cosine mod");
  fmod0->SetParName(2, "Sine mod");
  ge->Fit(fmod0, "MRq");

  fup->Draw("same");
  fdown->Draw("same");

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()),
      kFALSE);

}

void
DrawDCA_Dimuon()
{

//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv16_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv18_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
  assert(_file0);

  TH2F * DCAR_Diff_Dimuon = (TH2F *) _file0->GetObjectChecked(
      "DCAR_Diff_Dimuon", "TH2F");
  assert(DCAR_Diff_Dimuon);

  TH2F * DCAR_Sum_Dimuon = (TH2F *) _file0->GetObjectChecked("DCAR_Sum_Dimuon",
      "TH2F");
  assert(DCAR_Sum_Dimuon);

  TH2F * DCAZ_Diff_Dimuon = (TH2F *) _file0->DCAZ_Diff_Dimuon(
      "FVTX_DCA_Phi_Arm_SUM", "TH2F");
  assert(DCAZ_Diff_Dimuon);

  TH2F * DCAZ_Sum_Dimuon = (TH2F *) _file0->GetObjectChecked("DCAZ_Sum_Dimuon",
      "TH2F");
  assert(DCAZ_Sum_Dimuon);

///////////////////////

  TCanvas *c1 = new TCanvas("DrawDCA_Dimuon", "DrawDCA_Dimuon", 1800, 900);
  c1->Divide(2, 1);
  int idx = 1;
  TVirtualPad * p;

  p = c1->cd(idx++);
  c1->Update();

  p = c1->cd(idx++);
  c1->Update();

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()),
      kFALSE);

}

void
DrawDCA_FVTX()
{

//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv18_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
  assert(_file0);

  TH3F * DCA_Phi_Arm_SUM = (TH3F *) _file0->GetObjectChecked("DCA_Phi_Arm_SUM",
      "TH3F");
  assert(DCA_Phi_Arm_SUM);

  TH3F * DCA_Pz_Arm_SUM = (TH3F *) _file0->GetObjectChecked("DCA_Pz_Arm_SUM",
      "TH3F");
  assert(DCA_Pz_Arm_SUM);

  TH3F * FVTX_DCA_Phi_Arm_SUM = (TH3F *) _file0->GetObjectChecked(
      "FVTX_DCA_Phi_Arm_SUM", "TH3F");
  assert(FVTX_DCA_Phi_Arm_SUM);

  TH3F * FVTX_DCA_Pz_Arm_SUM = (TH3F *) _file0->GetObjectChecked(
      "FVTX_DCA_Pz_Arm_SUM", "TH3F");
  assert(FVTX_DCA_Pz_Arm_SUM);

//  DCA_Phi_Arm_SUM->GetZaxis()->
//  DCA_Phi_Arm_SUM

  TF1 * fup = new TF1("up", "pol0", -3.14159, 3.14159);
  TF1 * fdown = new TF1("up", "pol0", -3.14159, 3.14159);

  fup->SetParameter(0, +80e-4);
  fdown->SetParameter(0, -80e-4);
  fup->SetLineColor(kMagenta);
  fdown->SetLineColor(kMagenta);

///////////////////////

  TCanvas *c1 = new TCanvas("DrawDCA_FVTX", "DrawDCA_FVTX", 1800, 900);
  c1->Divide(2, 1);
  int idx = 1;
  TVirtualPad * p;

  p = c1->cd(idx++);
  c1->Update();

  FVTX_DCA_Phi_Arm_SUM->GetZaxis()->SetRange(1, 1);
  TH2F * FVTX_DCA_Phi_Arm_SUM_arm0 = FVTX_DCA_Phi_Arm_SUM->Project3D("xy");
  FVTX_DCA_Phi_Arm_SUM_arm0->SetName("FVTX_DCA_Phi_Arm_SUM_arm0");

  TGraphErrors * ge = FitProfile(FVTX_DCA_Phi_Arm_SUM_arm0);
  FVTX_DCA_Phi_Arm_SUM_arm0->Draw("colz");
  ge->Draw("*");
  FVTX_DCA_Phi_Arm_SUM_arm0->GetYaxis()->SetRangeUser(-.05, .05);

  TF1 * fmod0 = new TF1("fmod0", "1 ++ cos(x) ++ sin(x)", -3.14159, 3.14159);
  fmod0->SetParName(0, "Const Shift");
  fmod0->SetParName(1, "Cosine mod");
  fmod0->SetParName(2, "Sine mod");
  ge->Fit(fmod0, "MRq");

  fup->Draw("same");
  fdown->Draw("same");

  p = c1->cd(idx++);
  c1->Update();

  FVTX_DCA_Phi_Arm_SUM->GetZaxis()->SetRange(2, 2);
  TH2F * FVTX_DCA_Phi_Arm_SUM_arm1 = FVTX_DCA_Phi_Arm_SUM->Project3D("xy");
  FVTX_DCA_Phi_Arm_SUM_arm1->SetName("FVTX_DCA_Phi_Arm_SUM_arm1");

  TGraphErrors * ge = FitProfile(FVTX_DCA_Phi_Arm_SUM_arm1);
  FVTX_DCA_Phi_Arm_SUM_arm1->Draw("colz");
  ge->Draw("*");
  FVTX_DCA_Phi_Arm_SUM_arm1->GetYaxis()->SetRangeUser(-.05, .05);

  TF1 * fmod1 = new TF1("fmod1", "1 ++ cos(x) ++ sin(x)", -3.14159, 3.14159);
  fmod1->SetParName(0, "Const Shift");
  fmod1->SetParName(1, "Cosine mod");
  fmod1->SetParName(2, "Sine mod");
  ge->Fit(fmod1, "MRq");
  fup->Draw("same");
  fdown->Draw("same");

  c1->Paint();

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()),
      kFALSE);

}

void
DrawDCAPHI_FVTX()
{

//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv16_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv18_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
  assert(_file0);

  TH3F * DCA_Phi_Arm_SUM = (TH3F *) _file0->GetObjectChecked("DCA_Phi_Arm_SUM",
      "TH3F");
  assert(DCA_Phi_Arm_SUM);

  TH3F * DCA_Pz_Arm_SUM = (TH3F *) _file0->GetObjectChecked("DCA_Pz_Arm_SUM",
      "TH3F");
  assert(DCA_Pz_Arm_SUM);

  TH3F * FVTX_DCA_Phi_Arm_SUM = (TH3F *) _file0->GetObjectChecked(
      "FVTX_DCA_Phi_Arm_SUM", "TH3F");
  assert(FVTX_DCA_Phi_Arm_SUM);

  TH3F * FVTX_DCA_Pz_Arm_SUM = (TH3F *) _file0->GetObjectChecked(
      "FVTX_DCA_Pz_Arm_SUM", "TH3F");
  assert(FVTX_DCA_Pz_Arm_SUM);

//  DCA_Phi_Arm_SUM->GetZaxis()->
//  DCA_Phi_Arm_SUM

  TF1 * fup = new TF1("up", "pol0", -3.14159, 3.14159);
  TF1 * fdown = new TF1("up", "pol0", -3.14159, 3.14159);

  fup->SetParameter(0, +80e-4);
  fdown->SetParameter(0, -80e-4);
  fup->SetLineColor(kMagenta);
  fdown->SetLineColor(kMagenta);

///////////////////////

  TCanvas *c1 = new TCanvas("DrawDCAPHI_FVTX", "DrawDCAPHI_FVTX", 1800, 900);
  c1->Divide(2, 1);
  int idx = 1;
  TVirtualPad * p;

  p = c1->cd(idx++);
  c1->Update();

  FVTX_DCA_Phi_Arm_SUM->GetZaxis()->SetRange(1 + 2, 1 + 2);
  TH2F * FVTX_DCA_Phi_Arm_SUM_arm0 = FVTX_DCA_Phi_Arm_SUM->Project3D("xy");
  FVTX_DCA_Phi_Arm_SUM_arm0->SetName("FVTX_DCAPHI_Phi_Arm_SUM_arm0");

  TGraphErrors * ge = FitProfile(FVTX_DCA_Phi_Arm_SUM_arm0);
  FVTX_DCA_Phi_Arm_SUM_arm0->Draw("colz");
  ge->Draw("*");
//  FVTX_DCA_Phi_Arm_SUM_arm0->GetYaxis()->SetRangeUser(-.05, .05);
  FVTX_DCA_Phi_Arm_SUM_arm0->Rebin2D(1, 4);

  TF1 * fmod0 = new TF1("fmod0", "1 ++ cos(x) ++ sin(x)", -3.14159, 3.14159);
  fmod0->SetParName(0, "Const Shift");
  fmod0->SetParName(1, "Cosine mod");
  fmod0->SetParName(2, "Sine mod");
//  ge->Fit(fmod0, "MRq");

  fup->Draw("same");
  fdown->Draw("same");

  p = c1->cd(idx++);
  c1->Update();

  FVTX_DCA_Phi_Arm_SUM->GetZaxis()->SetRange(2 + 2, 2 + 2);
  TH2F * FVTX_DCA_Phi_Arm_SUM_arm1 = FVTX_DCA_Phi_Arm_SUM->Project3D("xy");
  FVTX_DCA_Phi_Arm_SUM_arm1->SetName("FVTX_DCAPHI_Phi_Arm_SUM_arm1");

  TGraphErrors * ge = FitProfile(FVTX_DCA_Phi_Arm_SUM_arm1);
  FVTX_DCA_Phi_Arm_SUM_arm1->Draw("colz");
  ge->Draw("*");
//  FVTX_DCA_Phi_Arm_SUM_arm1->GetYaxis()->SetRangeUser(-.05, .05);
  FVTX_DCA_Phi_Arm_SUM_arm1->Rebin2D(1, 4);

  TF1 * fmod1 = new TF1("fmod1", "1 ++ cos(x) ++ sin(x)", -3.14159, 3.14159);
  fmod1->SetParName(0, "Const Shift");
  fmod1->SetParName(1, "Cosine mod");
  fmod1->SetParName(2, "Sine mod");
//  ge->Fit(fmod1, "MRq");
  fup->Draw("same");
  fdown->Draw("same");

  c1->Paint();

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()),
      kFALSE);

}

void
DrawDCAPHI()
{

//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv16_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv18_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
  assert(_file0);

  TH3F * DCA_Phi_Arm_SUM = (TH3F *) _file0->GetObjectChecked("DCA_Phi_Arm_SUM",
      "TH3F");
  assert(DCA_Phi_Arm_SUM);

  TH3F * DCA_Pz_Arm_SUM = (TH3F *) _file0->GetObjectChecked("DCA_Pz_Arm_SUM",
      "TH3F");
  assert(DCA_Pz_Arm_SUM);

  TH3F * FVTX_DCA_Phi_Arm_SUM = (TH3F *) _file0->GetObjectChecked(
      "FVTX_DCA_Phi_Arm_SUM", "TH3F");
  assert(FVTX_DCA_Phi_Arm_SUM);

  TH3F * FVTX_DCA_Pz_Arm_SUM = (TH3F *) _file0->GetObjectChecked(
      "FVTX_DCA_Pz_Arm_SUM", "TH3F");
  assert(FVTX_DCA_Pz_Arm_SUM);

//  DCA_Phi_Arm_SUM->GetZaxis()->
//  DCA_Phi_Arm_SUM

  TF1 * fup = new TF1("up", "pol0", -3.14159, 3.14159);
  TF1 * fdown = new TF1("up", "pol0", -3.14159, 3.14159);

  fup->SetParameter(0, +80e-4);
  fdown->SetParameter(0, -80e-4);
  fup->SetLineColor(kMagenta);
  fdown->SetLineColor(kMagenta);

///////////////////////

  TCanvas *c1 = new TCanvas("DrawDCAPHI", "DrawDCAPHI", 1800, 900);
  c1->Divide(2, 1);
  int idx = 1;
  TVirtualPad * p;

  p = c1->cd(idx++);
  c1->Update();

  DCA_Phi_Arm_SUM->GetZaxis()->SetRange(1 + 2, 1 + 2);
  TH2F * DCA_Phi_Arm_SUM_arm0 = DCA_Phi_Arm_SUM->Project3D("xy");
  DCA_Phi_Arm_SUM_arm0->SetName("DCAPHI_Phi_Arm_SUM_arm0");

  TGraphErrors * ge = FitProfile(DCA_Phi_Arm_SUM_arm0);
  DCA_Phi_Arm_SUM_arm0->Draw("colz");
  ge->Draw("*");
//  DCA_Phi_Arm_SUM_arm0->GetYaxis()->SetRangeUser(-.05, .05);
  DCA_Phi_Arm_SUM_arm0->Rebin2D(1, 4);

  TF1 * fmod0 = new TF1("fmod0", "1 ++ cos(x) ++ sin(x)", -3.14159, 3.14159);
  fmod0->SetParName(0, "Const Shift");
  fmod0->SetParName(1, "Cosine mod");
  fmod0->SetParName(2, "Sine mod");
//  ge->Fit(fmod0, "MRq");

  fup->Draw("same");
  fdown->Draw("same");

  p = c1->cd(idx++);
  c1->Update();

  DCA_Phi_Arm_SUM->GetZaxis()->SetRange(2 + 2, 2 + 2);
  TH2F * DCA_Phi_Arm_SUM_arm1 = DCA_Phi_Arm_SUM->Project3D("xy");
  DCA_Phi_Arm_SUM_arm1->SetName("DCAPHI_Phi_Arm_SUM_arm1");

  TGraphErrors * ge = FitProfile(DCA_Phi_Arm_SUM_arm1);
  DCA_Phi_Arm_SUM_arm1->Draw("colz");
  ge->Draw("*");
//  DCA_Phi_Arm_SUM_arm1->GetYaxis()->SetRangeUser(-.05, .05);
  DCA_Phi_Arm_SUM_arm1->Rebin2D(1, 4);

  TF1 * fmod1 = new TF1("fmod1", "1 ++ cos(x) ++ sin(x)", -3.14159, 3.14159);
  fmod1->SetParName(0, "Const Shift");
  fmod1->SetParName(1, "Cosine mod");
  fmod1->SetParName(2, "Sine mod");
//  ge->Fit(fmod1, "MRq");
  fup->Draw("same");
  fdown->Draw("same");

  c1->Paint();

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()),
      kFALSE);

}

void
DrawDCA_Pz_FVTX_Old()
{

//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv16_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv18_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
  assert(_file0);

  TH3F * DCA_Phi_Arm_SUM = (TH3F *) _file0->GetObjectChecked("DCA_Phi_Arm_SUM",
      "TH3F");
  assert(DCA_Phi_Arm_SUM);

  TH3F * DCA_Pz_Arm_SUM = (TH3F *) _file0->GetObjectChecked("DCA_Pz_Arm_SUM",
      "TH3F");
  assert(DCA_Pz_Arm_SUM);

  TH3F * FVTX_DCA_Phi_Arm_SUM = (TH3F *) _file0->GetObjectChecked(
      "FVTX_DCA_Phi_Arm_SUM", "TH3F");
  assert(FVTX_DCA_Phi_Arm_SUM);

  TH3F * FVTX_DCA_Pz_Arm_SUM = (TH3F *) _file0->GetObjectChecked(
      "FVTX_DCA_Pz_Arm_SUM", "TH3F");
  assert(FVTX_DCA_Pz_Arm_SUM);

//  DCA_Phi_Arm_SUM->GetZaxis()->
//  DCA_Phi_Arm_SUM

  TF1 * fup = new TF1("up", "pol0", -3.14159, 3.14159);
  TF1 * fdown = new TF1("up", "pol0", -3.14159, 3.14159);

  fup->SetParameter(0, +80e-4);
  fdown->SetParameter(0, -80e-4);
  fup->SetLineColor(kMagenta);
  fdown->SetLineColor(kMagenta);

  TCanvas *c1 = new TCanvas("DrawDCA_Pz_FVTX", "DrawDCA_Pz_FVTX", 1800, 900);
  c1->Divide(3, 2);
  int idx = 1;
  TVirtualPad * p;

  p = c1->cd(idx++);
  c1->Update();

  FVTX_DCA_Pz_Arm_SUM->GetZaxis()->SetRange(1, 1);
  TH2F * FVTX_DCA_Pz_Arm_SUM_arm0 = FVTX_DCA_Pz_Arm_SUM->Project3D("xy");
  FVTX_DCA_Pz_Arm_SUM_arm0->SetName("FVTX_DCA_Pz_Arm_SUM_arm0");

  FVTX_DCA_Pz_Arm_SUM_arm0->Draw("colz");

  p = c1->cd(idx++);
  c1->Update();
//  FVTX_DCA_Pz_Arm_SUM_arm0->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm0_5GeV",FVTX_DCA_Pz_Arm_SUM_arm0->GetXaxis()->FindBin(5),-1)->Draw("");
  FVTX_DCA_Pz_Arm_SUM_arm0->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm0_1", 1, 1);
  FVTX_DCA_Pz_Arm_SUM_arm0->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm0_2", 2, 2);
  FVTX_DCA_Pz_Arm_SUM_arm0->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm0_3", 3, 3);
  FVTX_DCA_Pz_Arm_SUM_arm0->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm0_4", 4, -1);
  FVTX_DCA_Pz_Arm_SUM_arm0_1->Scale(1. / FVTX_DCA_Pz_Arm_SUM_arm0_1->GetSum());
  FVTX_DCA_Pz_Arm_SUM_arm0_2->Scale(1. / FVTX_DCA_Pz_Arm_SUM_arm0_2->GetSum());
  FVTX_DCA_Pz_Arm_SUM_arm0_3->Scale(1. / FVTX_DCA_Pz_Arm_SUM_arm0_3->GetSum());
  FVTX_DCA_Pz_Arm_SUM_arm0_4->Scale(1. / FVTX_DCA_Pz_Arm_SUM_arm0_4->GetSum());

  FVTX_DCA_Pz_Arm_SUM_arm0_4->GetXaxis()->SetRangeUser(-.2, .2);

  FVTX_DCA_Pz_Arm_SUM_arm0_4->SetLineColor(kRed);
  FVTX_DCA_Pz_Arm_SUM_arm0_3->SetLineColor(kMagenta);
  FVTX_DCA_Pz_Arm_SUM_arm0_2->SetLineColor(kBlue);
  FVTX_DCA_Pz_Arm_SUM_arm0_1->SetLineColor(kGreen);

  FVTX_DCA_Pz_Arm_SUM_arm0_4->Draw();
  FVTX_DCA_Pz_Arm_SUM_arm0_3->Draw("same");
  FVTX_DCA_Pz_Arm_SUM_arm0_2->Draw("same");
  FVTX_DCA_Pz_Arm_SUM_arm0_1->Draw("same");

  p = c1->cd(idx++);
  c1->Update();

  TF1 * fgaus = new TF1("fgaus0", "gaus", -.1, .1);

  TF1 * f2 = new TF1(Form("dgaus0"),
      "gaus + [3]*exp(-0.5*((x-[1])/[4])**2) + [5]", -.1, .1);

  FVTX_DCA_Pz_Arm_SUM_arm0_4->Fit(fgaus, "RMQ");

  f2->SetParameters(fgaus->GetParameter(0) / 2, fgaus->GetParameter(1),
      fgaus->GetParameter(2), fgaus->GetParameter(0) / 2,
      fgaus->GetParameter(2) / 4, 0);
  f2->SetParNames("Amp1", "Mean", "#sigma_{1}", "Amp2", "#sigma_{2}", "Const");

  FVTX_DCA_Pz_Arm_SUM_arm0_4->Fit(f2, "RM");

  p = c1->cd(idx++);
  c1->Update();

  FVTX_DCA_Pz_Arm_SUM->GetZaxis()->SetRange(2, 2);
  TH2F * FVTX_DCA_Pz_Arm_SUM_arm1 = FVTX_DCA_Pz_Arm_SUM->Project3D("xy");
  FVTX_DCA_Pz_Arm_SUM_arm1->SetName("FVTX_DCA_Pz_Arm_SUM_arm1");

  FVTX_DCA_Pz_Arm_SUM_arm1->Draw("colz");

  p = c1->cd(idx++);
  c1->Update();
//  FVTX_DCA_Pz_Arm_SUM_arm0->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm0_5GeV",FVTX_DCA_Pz_Arm_SUM_arm0->GetXaxis()->FindBin(5),-1)->Draw("");
  FVTX_DCA_Pz_Arm_SUM_arm1->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm1_1", 1, 1);
  FVTX_DCA_Pz_Arm_SUM_arm1->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm1_2", 2, 2);
  FVTX_DCA_Pz_Arm_SUM_arm1->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm1_3", 3, 3);
  FVTX_DCA_Pz_Arm_SUM_arm1->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm1_4", 4, -1);
  FVTX_DCA_Pz_Arm_SUM_arm1_1->Scale(1. / FVTX_DCA_Pz_Arm_SUM_arm1_1->GetSum());
  FVTX_DCA_Pz_Arm_SUM_arm1_2->Scale(1. / FVTX_DCA_Pz_Arm_SUM_arm1_2->GetSum());
  FVTX_DCA_Pz_Arm_SUM_arm1_3->Scale(1. / FVTX_DCA_Pz_Arm_SUM_arm1_3->GetSum());
  FVTX_DCA_Pz_Arm_SUM_arm1_4->Scale(1. / FVTX_DCA_Pz_Arm_SUM_arm1_4->GetSum());

  FVTX_DCA_Pz_Arm_SUM_arm1_4->GetXaxis()->SetRangeUser(-.2, .2);

  FVTX_DCA_Pz_Arm_SUM_arm1_4->SetLineColor(kRed);
  FVTX_DCA_Pz_Arm_SUM_arm1_3->SetLineColor(kMagenta);
  FVTX_DCA_Pz_Arm_SUM_arm1_2->SetLineColor(kBlue);
  FVTX_DCA_Pz_Arm_SUM_arm1_1->SetLineColor(kGreen);

  FVTX_DCA_Pz_Arm_SUM_arm1_4->Draw();
  FVTX_DCA_Pz_Arm_SUM_arm1_3->Draw("same");
  FVTX_DCA_Pz_Arm_SUM_arm1_2->Draw("same");
  FVTX_DCA_Pz_Arm_SUM_arm1_1->Draw("same");

  p = c1->cd(idx++);
  c1->Update();

  TF1 * fgaus = new TF1("fgaus0", "gaus", -.1, .1);

  TF1 * f2 = new TF1(Form("dgaus0"),
      "gaus + [3]*exp(-0.5*((x-[1])/[4])**2) + [5]", -.1, .1);

  FVTX_DCA_Pz_Arm_SUM_arm1_4->Fit(fgaus, "RMQ");

  f2->SetParameters(fgaus->GetParameter(0) / 2, fgaus->GetParameter(1),
      fgaus->GetParameter(2), fgaus->GetParameter(0) / 2,
      fgaus->GetParameter(2) / 4, 0);
  f2->SetParNames("Amp1", "Mean", "#sigma_{1}", "Amp2", "#sigma_{2}", "Const");

  FVTX_DCA_Pz_Arm_SUM_arm1_4->Fit(f2, "RM");

  c1->Paint();

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()),
      kFALSE);

}

void
DrawDCA_Pz()
{

//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv16_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv18_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
  assert(_file0);

  TH3F * DCA_Phi_Arm_SUM = (TH3F *) _file0->GetObjectChecked("DCA_Phi_Arm_SUM",
      "TH3F");
  assert(DCA_Phi_Arm_SUM);

  TH3F * DCA_Pz_Arm_SUM = (TH3F *) _file0->GetObjectChecked("DCA_Pz_Arm_SUM",
      "TH3F");
  assert(DCA_Pz_Arm_SUM);

  TH3F * FVTX_DCA_Phi_Arm_SUM = (TH3F *) _file0->GetObjectChecked(
      "FVTX_DCA_Phi_Arm_SUM", "TH3F");
  assert(FVTX_DCA_Phi_Arm_SUM);

  TH3F * FVTX_DCA_Pz_Arm_SUM = (TH3F *) _file0->GetObjectChecked(
      "FVTX_DCA_Pz_Arm_SUM", "TH3F");
  assert(FVTX_DCA_Pz_Arm_SUM);

//  DCA_Phi_Arm_SUM->GetZaxis()->
//  DCA_Phi_Arm_SUM

  TF1 * fup = new TF1("up", "pol0", -3.14159, 3.14159);
  TF1 * fdown = new TF1("up", "pol0", -3.14159, 3.14159);

  fup->SetParameter(0, +80e-4);
  fdown->SetParameter(0, -80e-4);
  fup->SetLineColor(kMagenta);
  fdown->SetLineColor(kMagenta);

  TCanvas *c1 = new TCanvas("DrawDCA_Pz", "DrawDCA_Pz", 1800, 900);
  c1->Divide(3, 2);
  int idx = 1;
  TVirtualPad * p;

  p = c1->cd(idx++);
  c1->Update();

  DCA_Pz_Arm_SUM->GetZaxis()->SetRange(1, 1);
  TH2F * DCA_Pz_Arm_SUM_arm0 = DCA_Pz_Arm_SUM->Project3D("xy");
  DCA_Pz_Arm_SUM_arm0->SetName("DCA_Pz_Arm_SUM_arm0");

  DCA_Pz_Arm_SUM_arm0->Draw("colz");

  p = c1->cd(idx++);
  c1->Update();
//  DCA_Pz_Arm_SUM_arm0->ProjectionY("DCA_Pz_Arm_SUM_arm0_5GeV",DCA_Pz_Arm_SUM_arm0->GetXaxis()->FindBin(5),-1)->Draw("");
  DCA_Pz_Arm_SUM_arm0->ProjectionY("DCA_Pz_Arm_SUM_arm0_1", 1, 1);
  DCA_Pz_Arm_SUM_arm0->ProjectionY("DCA_Pz_Arm_SUM_arm0_2", 2, 2);
  DCA_Pz_Arm_SUM_arm0->ProjectionY("DCA_Pz_Arm_SUM_arm0_3", 3, 3);
  DCA_Pz_Arm_SUM_arm0->ProjectionY("DCA_Pz_Arm_SUM_arm0_4", 4, -1);
  DCA_Pz_Arm_SUM_arm0_1->Scale(1. / DCA_Pz_Arm_SUM_arm0_1->GetSum());
  DCA_Pz_Arm_SUM_arm0_2->Scale(1. / DCA_Pz_Arm_SUM_arm0_2->GetSum());
  DCA_Pz_Arm_SUM_arm0_3->Scale(1. / DCA_Pz_Arm_SUM_arm0_3->GetSum());
  DCA_Pz_Arm_SUM_arm0_4->Scale(1. / DCA_Pz_Arm_SUM_arm0_4->GetSum());

  DCA_Pz_Arm_SUM_arm0_4->GetXaxis()->SetRangeUser(-.2, .2);

  DCA_Pz_Arm_SUM_arm0_4->SetLineColor(kRed);
  DCA_Pz_Arm_SUM_arm0_3->SetLineColor(kMagenta);
  DCA_Pz_Arm_SUM_arm0_2->SetLineColor(kBlue);
  DCA_Pz_Arm_SUM_arm0_1->SetLineColor(kGreen);

  DCA_Pz_Arm_SUM_arm0_4->Draw();
  DCA_Pz_Arm_SUM_arm0_3->Draw("same");
  DCA_Pz_Arm_SUM_arm0_2->Draw("same");
  DCA_Pz_Arm_SUM_arm0_1->Draw("same");

  p = c1->cd(idx++);
  c1->Update();

  TF1 * fgaus = new TF1("fgaus0", "gaus", -.1, .1);

//  TF1 * f2 = new TF1(Form("dgaus0"),
//      "gaus + [3]*exp(-0.5*((x-[1])/[4])**2) + [5]", -.1, .1);
  TF1 * f2 = new TF1(Form("dgaus0"), "gaus + [3]", -.05, .05);

  DCA_Pz_Arm_SUM_arm0->ProjectionY("DCA_Pz_Arm_SUM_arm0_4_new", 4, -1);
  DCA_Pz_Arm_SUM_arm0_4_new->GetXaxis()->SetRangeUser(-.2, .2);
  DCA_Pz_Arm_SUM_arm0_4_new->Sumw2();
  DCA_Pz_Arm_SUM_arm0_4_new->Fit(fgaus, "RMQ");

  f2->SetParameters(fgaus->GetParameter(0) / 2, fgaus->GetParameter(1),
      fgaus->GetParameter(2), fgaus->GetParameter(0) / 2,
      fgaus->GetParameter(2) / 4, 0);
//  f2->SetParNames("Amp1","Mean","#sigma_{1}","Amp2","#sigma_{2}","Const");
  f2->SetParNames("Amp1", "Mean", "#sigma_{1}", "Const");

  DCA_Pz_Arm_SUM_arm0_4_new->Fit(f2, "RM");

  p = c1->cd(idx++);
  c1->Update();

  DCA_Pz_Arm_SUM->GetZaxis()->SetRange(2, 2);
  TH2F * DCA_Pz_Arm_SUM_arm1 = DCA_Pz_Arm_SUM->Project3D("xy");
  DCA_Pz_Arm_SUM_arm1->SetName("DCA_Pz_Arm_SUM_arm1");

  DCA_Pz_Arm_SUM_arm1->Draw("colz");

  p = c1->cd(idx++);
  c1->Update();
//  DCA_Pz_Arm_SUM_arm0->ProjectionY("DCA_Pz_Arm_SUM_arm0_5GeV",DCA_Pz_Arm_SUM_arm0->GetXaxis()->FindBin(5),-1)->Draw("");
  DCA_Pz_Arm_SUM_arm1->ProjectionY("DCA_Pz_Arm_SUM_arm1_1", 1, 1);
  DCA_Pz_Arm_SUM_arm1->ProjectionY("DCA_Pz_Arm_SUM_arm1_2", 2, 2);
  DCA_Pz_Arm_SUM_arm1->ProjectionY("DCA_Pz_Arm_SUM_arm1_3", 3, 3);
  DCA_Pz_Arm_SUM_arm1->ProjectionY("DCA_Pz_Arm_SUM_arm1_4", 4, -1);
  DCA_Pz_Arm_SUM_arm1_1->Scale(1. / DCA_Pz_Arm_SUM_arm1_1->GetSum());
  DCA_Pz_Arm_SUM_arm1_2->Scale(1. / DCA_Pz_Arm_SUM_arm1_2->GetSum());
  DCA_Pz_Arm_SUM_arm1_3->Scale(1. / DCA_Pz_Arm_SUM_arm1_3->GetSum());
  DCA_Pz_Arm_SUM_arm1_4->Scale(1. / DCA_Pz_Arm_SUM_arm1_4->GetSum());

  DCA_Pz_Arm_SUM_arm1_4->GetXaxis()->SetRangeUser(-.2, .2);

  DCA_Pz_Arm_SUM_arm1_4->SetLineColor(kRed);
  DCA_Pz_Arm_SUM_arm1_3->SetLineColor(kMagenta);
  DCA_Pz_Arm_SUM_arm1_2->SetLineColor(kBlue);
  DCA_Pz_Arm_SUM_arm1_1->SetLineColor(kGreen);

  DCA_Pz_Arm_SUM_arm1_4->Draw();
  DCA_Pz_Arm_SUM_arm1_3->Draw("same");
  DCA_Pz_Arm_SUM_arm1_2->Draw("same");
  DCA_Pz_Arm_SUM_arm1_1->Draw("same");

  p = c1->cd(idx++);
  c1->Update();

  TF1 * fgaus = new TF1("fgaus0", "gaus", -.1, .1);

//  TF1 * f2 = new TF1(Form("dgaus0"),
//      "gaus + [3]*exp(-0.5*((x-[1])/[4])**2) + [5]", -.1, .1);
  TF1 * f2 = new TF1(Form("dgaus1"), "gaus + [3]", -.05, .05);

  DCA_Pz_Arm_SUM_arm1->ProjectionY("DCA_Pz_Arm_SUM_arm1_4_new", 4, -1);
  DCA_Pz_Arm_SUM_arm1_4_new->Sumw2();
  DCA_Pz_Arm_SUM_arm1_4_new->GetXaxis()->SetRangeUser(-.2, .2);
  DCA_Pz_Arm_SUM_arm1_4_new->Fit(fgaus, "RMQ");

  f2->SetParameters(fgaus->GetParameter(0) / 2, fgaus->GetParameter(1),
      fgaus->GetParameter(2), fgaus->GetParameter(0) / 2,
      fgaus->GetParameter(2) / 4, 0);
//  f2->SetParNames("Amp1","Mean","#sigma_{1}","Amp2","#sigma_{2}","Const");
  f2->SetParNames("Amp1", "Mean", "#sigma_{1}", "Const");

  DCA_Pz_Arm_SUM_arm1_4_new->Fit(f2, "RM");

  c1->Paint();

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()),
      kFALSE);

  gStyle->SetOptFit(0);

  TCanvas *c1 = new TCanvas("DrawDCA_Pz_Print", "DrawDCA_Pz_Print", 1000, 800);
//  c1->Divide(3, 2);
  int idx = 1;
  TVirtualPad * p;

  p = c1->cd(idx++);
  c1->Update();

  c1->SetGridx(0);
  c1->SetGridy(0);

  TH1F * DCA_Pz_Arm_SUM_armAll_4_new = DCA_Pz_Arm_SUM_arm1_4_new->Clone(
      "DCA_Pz_Arm_SUM_armAll_4_new");

  DCA_Pz_Arm_SUM_armAll_4_new->Add(DCA_Pz_Arm_SUM_arm0_4_new);

  TF1 * f2 = new TF1(Form("dgaus"),
      "gaus + [3]*exp(-0.5*((x-[1])/[4])**2) + [5]", -.3, .3);
  f2->SetParameters(10, 0, 160e-4, 1, .1, 0);
  f2->SetNpx(1000);
  f2->SetLineWidth(3);
  f2->SetLineColor(kRed);
  DCA_Pz_Arm_SUM_armAll_4_new->Fit(f2, "RM");

//  f2->Draw("same");
  DCA_Pz_Arm_SUM_armAll_4_new->SetLineColor(kBlack);
  DCA_Pz_Arm_SUM_armAll_4_new->SetLineWidth(2);
  DCA_Pz_Arm_SUM_armAll_4_new->GetXaxis()->SetRangeUser(-.3, .3);
  DCA_Pz_Arm_SUM_armAll_4_new->GetXaxis()->SetTitleSize(0.05);
  DCA_Pz_Arm_SUM_armAll_4_new->GetXaxis()->SetLabelSize(0.05);
  DCA_Pz_Arm_SUM_armAll_4_new->GetYaxis()->SetLabelSize(0.05);
  DCA_Pz_Arm_SUM_armAll_4_new->SetTitle(";DCA_{#perp}     [cm]");

  TLatex L;
  L.SetTextSize(.05);
  double line = 65;
  double dline = 8;
  L.DrawLatex(-.25, line,
      Form("#sigma_{1} = %.0f #pm %.0f #mum", f2->GetParameter(2) * 1e4,
          f2->GetParError(2) * 1e4));
  line -= dline;
  L.DrawLatex(-.25, line,
      Form("#sigma_{2} = %.0f #pm %.0f #mum", f2->GetParameter(4) * 1e4,
          f2->GetParError(4) * 1e4));
  line -= dline;

  double line = 65 + dline / 2;
  L.DrawLatex(0.06, line, Form("Run12 p+p"));
  line -= dline;
  L.DrawLatex(0.06, line, Form("#sqrt{s} = 510 GeV"));
  line -= dline;
  L.DrawLatex(0.06, line, Form("J/#Psi#rightarrow#mu^{+}#mu^{-} in FVTX"));
//  line -= dline;
//  L.DrawLatex(0.06,line,Form("Muon measured in FVTX + MuTr"));
//  line -= dline;
//  L.DrawLatex(0.06,line,Form("Vertex z VTX"));
//  line -= dline;
//  L.DrawLatex(0.06,line,Form("Vertex x,y averaged over run"));

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()));
}

void
DrawDCA_Pz_FVTX()
{

//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv16_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
//  TFile *_file0 = TFile::Open(
//      "../data_pro100_VTXv18_LooseVTX/MWG_MU_pDST.lst_dca_hist.root");
  assert(_file0);

  TH3F * DCA_Phi_Arm_SUM = (TH3F *) _file0->GetObjectChecked("DCA_Phi_Arm_SUM",
      "TH3F");
  assert(DCA_Phi_Arm_SUM);

  TH3F * DCA_Pz_Arm_SUM = (TH3F *) _file0->GetObjectChecked("DCA_Pz_Arm_SUM",
      "TH3F");
  assert(DCA_Pz_Arm_SUM);

  TH3F * FVTX_DCA_Phi_Arm_SUM = (TH3F *) _file0->GetObjectChecked(
      "FVTX_DCA_Phi_Arm_SUM", "TH3F");
  assert(FVTX_DCA_Phi_Arm_SUM);

  TH3F * FVTX_DCA_Pz_Arm_SUM = (TH3F *) _file0->GetObjectChecked(
      "FVTX_DCA_Pz_Arm_SUM", "TH3F");
  assert(FVTX_DCA_Pz_Arm_SUM);

//  DCA_Phi_Arm_SUM->GetZaxis()->
//  DCA_Phi_Arm_SUM

  TF1 * fup = new TF1("up", "pol0", -3.14159, 3.14159);
  TF1 * fdown = new TF1("up", "pol0", -3.14159, 3.14159);

  fup->SetParameter(0, +80e-4);
  fdown->SetParameter(0, -80e-4);
  fup->SetLineColor(kMagenta);
  fdown->SetLineColor(kMagenta);

  TCanvas *c1 = new TCanvas("DrawDCA_Pz_FVTX", "DrawDCA_Pz_FVTX", 1800, 900);
  c1->Divide(3, 2);
  int idx = 1;
  TVirtualPad * p;

  p = c1->cd(idx++);
  c1->Update();

  FVTX_DCA_Pz_Arm_SUM->GetZaxis()->SetRange(1, 1);
  TH2F * FVTX_DCA_Pz_Arm_SUM_arm0 = FVTX_DCA_Pz_Arm_SUM->Project3D("xy");
  FVTX_DCA_Pz_Arm_SUM_arm0->SetName("FVTX_DCA_Pz_Arm_SUM_arm0");

  FVTX_DCA_Pz_Arm_SUM_arm0->Draw("colz");

  p = c1->cd(idx++);
  c1->Update();
//  FVTX_DCA_Pz_Arm_SUM_arm0->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm0_5GeV",FVTX_DCA_Pz_Arm_SUM_arm0->GetXaxis()->FindBin(5),-1)->Draw("");
  FVTX_DCA_Pz_Arm_SUM_arm0->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm0_1", 1, 1);
  FVTX_DCA_Pz_Arm_SUM_arm0->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm0_2", 2, 2);
  FVTX_DCA_Pz_Arm_SUM_arm0->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm0_3", 3, 3);
  FVTX_DCA_Pz_Arm_SUM_arm0->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm0_4", 4, -1);
  FVTX_DCA_Pz_Arm_SUM_arm0_1->Scale(1. / FVTX_DCA_Pz_Arm_SUM_arm0_1->GetSum());
  FVTX_DCA_Pz_Arm_SUM_arm0_2->Scale(1. / FVTX_DCA_Pz_Arm_SUM_arm0_2->GetSum());
  FVTX_DCA_Pz_Arm_SUM_arm0_3->Scale(1. / FVTX_DCA_Pz_Arm_SUM_arm0_3->GetSum());
  FVTX_DCA_Pz_Arm_SUM_arm0_4->Scale(1. / FVTX_DCA_Pz_Arm_SUM_arm0_4->GetSum());

  FVTX_DCA_Pz_Arm_SUM_arm0_4->GetXaxis()->SetRangeUser(-.2, .2);

  FVTX_DCA_Pz_Arm_SUM_arm0_4->SetLineColor(kRed);
  FVTX_DCA_Pz_Arm_SUM_arm0_3->SetLineColor(kMagenta);
  FVTX_DCA_Pz_Arm_SUM_arm0_2->SetLineColor(kBlue);
  FVTX_DCA_Pz_Arm_SUM_arm0_1->SetLineColor(kGreen);

  FVTX_DCA_Pz_Arm_SUM_arm0_4->Draw();
  FVTX_DCA_Pz_Arm_SUM_arm0_3->Draw("same");
  FVTX_DCA_Pz_Arm_SUM_arm0_2->Draw("same");
  FVTX_DCA_Pz_Arm_SUM_arm0_1->Draw("same");

  p = c1->cd(idx++);
  c1->Update();

  TF1 * fgaus = new TF1("fgaus0", "gaus", -.1, .1);

//  TF1 * f2 = new TF1(Form("dgaus0"),
//      "gaus + [3]*exp(-0.5*((x-[1])/[4])**2) + [5]", -.1, .1);
  TF1 * f2 = new TF1(Form("dgaus0"), "gaus + [3]", -.05, .05);

  FVTX_DCA_Pz_Arm_SUM_arm0->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm0_4_new", 4,
      -1);
  FVTX_DCA_Pz_Arm_SUM_arm0_4_new->GetXaxis()->SetRangeUser(-.2, .2);
  FVTX_DCA_Pz_Arm_SUM_arm0_4_new->Sumw2();
  FVTX_DCA_Pz_Arm_SUM_arm0_4_new->Fit(fgaus, "RMQ");

  f2->SetParameters(fgaus->GetParameter(0) / 2, fgaus->GetParameter(1),
      fgaus->GetParameter(2), fgaus->GetParameter(0) / 2,
      fgaus->GetParameter(2) / 4, 0);
//  f2->SetParNames("Amp1","Mean","#sigma_{1}","Amp2","#sigma_{2}","Const");
  f2->SetParNames("Amp1", "Mean", "#sigma_{1}", "Const");

  FVTX_DCA_Pz_Arm_SUM_arm0_4_new->Fit(f2, "RM");

  p = c1->cd(idx++);
  c1->Update();

  FVTX_DCA_Pz_Arm_SUM->GetZaxis()->SetRange(2, 2);
  TH2F * FVTX_DCA_Pz_Arm_SUM_arm1 = FVTX_DCA_Pz_Arm_SUM->Project3D("xy");
  FVTX_DCA_Pz_Arm_SUM_arm1->SetName("FVTX_DCA_Pz_Arm_SUM_arm1");

  FVTX_DCA_Pz_Arm_SUM_arm1->Draw("colz");

  p = c1->cd(idx++);
  c1->Update();
//  FVTX_DCA_Pz_Arm_SUM_arm0->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm0_5GeV",FVTX_DCA_Pz_Arm_SUM_arm0->GetXaxis()->FindBin(5),-1)->Draw("");
  FVTX_DCA_Pz_Arm_SUM_arm1->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm1_1", 1, 1);
  FVTX_DCA_Pz_Arm_SUM_arm1->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm1_2", 2, 2);
  FVTX_DCA_Pz_Arm_SUM_arm1->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm1_3", 3, 3);
  FVTX_DCA_Pz_Arm_SUM_arm1->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm1_4", 4, -1);
  FVTX_DCA_Pz_Arm_SUM_arm1_1->Scale(1. / FVTX_DCA_Pz_Arm_SUM_arm1_1->GetSum());
  FVTX_DCA_Pz_Arm_SUM_arm1_2->Scale(1. / FVTX_DCA_Pz_Arm_SUM_arm1_2->GetSum());
  FVTX_DCA_Pz_Arm_SUM_arm1_3->Scale(1. / FVTX_DCA_Pz_Arm_SUM_arm1_3->GetSum());
  FVTX_DCA_Pz_Arm_SUM_arm1_4->Scale(1. / FVTX_DCA_Pz_Arm_SUM_arm1_4->GetSum());

  FVTX_DCA_Pz_Arm_SUM_arm1_4->GetXaxis()->SetRangeUser(-.2, .2);

  FVTX_DCA_Pz_Arm_SUM_arm1_4->SetLineColor(kRed);
  FVTX_DCA_Pz_Arm_SUM_arm1_3->SetLineColor(kMagenta);
  FVTX_DCA_Pz_Arm_SUM_arm1_2->SetLineColor(kBlue);
  FVTX_DCA_Pz_Arm_SUM_arm1_1->SetLineColor(kGreen);

  FVTX_DCA_Pz_Arm_SUM_arm1_4->Draw();
  FVTX_DCA_Pz_Arm_SUM_arm1_3->Draw("same");
  FVTX_DCA_Pz_Arm_SUM_arm1_2->Draw("same");
  FVTX_DCA_Pz_Arm_SUM_arm1_1->Draw("same");

  p = c1->cd(idx++);
  c1->Update();

  TF1 * fgaus = new TF1("fgaus0", "gaus", -.1, .1);

//  TF1 * f2 = new TF1(Form("dgaus0"),
//      "gaus + [3]*exp(-0.5*((x-[1])/[4])**2) + [5]", -.1, .1);
  TF1 * f2 = new TF1(Form("dgaus1"), "gaus + [3]", -.05, .05);

  FVTX_DCA_Pz_Arm_SUM_arm1->ProjectionY("FVTX_DCA_Pz_Arm_SUM_arm1_4_new", 4,
      -1);
  FVTX_DCA_Pz_Arm_SUM_arm1_4_new->Sumw2();
  FVTX_DCA_Pz_Arm_SUM_arm1_4_new->GetXaxis()->SetRangeUser(-.2, .2);
  FVTX_DCA_Pz_Arm_SUM_arm1_4_new->Fit(fgaus, "RMQ");

  f2->SetParameters(fgaus->GetParameter(0) / 2, fgaus->GetParameter(1),
      fgaus->GetParameter(2), fgaus->GetParameter(0) / 2,
      fgaus->GetParameter(2) / 4, 0);
//  f2->SetParNames("Amp1","Mean","#sigma_{1}","Amp2","#sigma_{2}","Const");
  f2->SetParNames("Amp1", "Mean", "#sigma_{1}", "Const");

  FVTX_DCA_Pz_Arm_SUM_arm1_4_new->Fit(f2, "RM");

  c1->Paint();

  SaveCanvas(c1,
      TString(_file0->GetName()) + TString("_") + TString(c1->GetName()),
      kFALSE);

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

  for (int i = 1; i <= h2->GetNbinsX(); i++)
    {
      TH1D * h1 = h2->ProjectionY(Form("htmp_%d", rand()), i, i);

      if (h1->GetSum() < 30)
        continue;

      TF1 fgaus("fgaus", "gaus", -p2->GetBinError(i) * 4,
          p2->GetBinError(i) * 4);

      TF1 f2(Form("dgaus"), "gaus + [3]*exp(-0.5*((x-[1])/[4])**2) + [5]",
          -p2->GetBinError(i) * 4, p2->GetBinError(i) * 4);

      fgaus.SetParameter(1, p2->GetBinContent(i));
      fgaus.SetParameter(2, p2->GetBinError(i));

      h1->Fit(&fgaus, "MQ");

      f2.SetParameters(fgaus.GetParameter(0) / 2, fgaus.GetParameter(1),
          fgaus.GetParameter(2), fgaus.GetParameter(0) / 2,
          fgaus.GetParameter(2) / 4, 0);

      h1->Fit(&f2, "MQ");

//      new TCanvas;
//      h1->Draw();
//      fgaus.Draw("same");
//      break;

      x[n] = p2->GetBinCenter(i);
      ex[n] = (p2->GetBinCenter(2) - p2->GetBinCenter(1)) / 2;
//      y[n] = fgaus.GetParameter(1);
//      ey[n] = fgaus.GetParError(1);
      y[n] = f2.GetParameter(1);
      ey[n] = f2.GetParError(1);

//      p2->SetBinContent(i, fgaus.GetParameter(1));
//      p2->SetBinError(i, fgaus.GetParameter(2));

      n++;
      delete h1;
    }

  return new TGraphErrors(n, x, y, ex, ey);
}

void
MergeHist(
    TString infile =
        "/phenix/u/jinhuang/links/fvtx_data/taxi/Run12pp510MuonPro100/4117/se-368692.lst",
    const int n_hists, const char * hist_names[])
{

  //load source files
  TFile *_file1 = TFile::Open(infile + ".root", "recreate");

  fstream f_lst(infile, ios_base::in);

  string line;

  saHist * hs[1000] =
    { NULL };
  for (int i = 0; i < n_hists; i++)
    hs[i] = NULL;

  vector<string> missing_files;

  while (!f_lst.eof())
    {
      getline(f_lst, line);

      if (line.length() == 0)
        continue;

//      gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");

      cout << "MergeHist - process " << line << endl;

      TFile *_file0 = TFile::Open(line.c_str());
      assert(_file0);

      for (int i = 0; i < n_hists; i++)
        {

          _file0->cd();

          const TString hist_name = hist_names[i];

//          cout << "Loading " << hist_name << endl;

          saHist * h_src = (saHist *) _file0->GetObjectChecked(
              hist_name + "_saHist", "saHist");
          assert(h_src);
          h_src->Verbosity(0, true);
          if (h_src->AutoLoad(0))
            {
              h_src->Verbosity(0, true);

              h_src->RemoveAsymmetryHisto();

              if (!(hs[i]))
                {
                  _file1->cd();
                  cout
                      << "====================  h_src->MakeTemplate();  ====================="
                      << endl;
                  (hs[i]) = h_src->MakeTemplate();
                  assert((hs[i]));

//              cout << "Template Made " << hist_name << " : "
//                  << (hs[i])->GetName() << " : " << hs[i]->GetName() << endl;
                }

//              (hs[i])->AdoptRunSubHist(h_src);

              saHist::n_list run_list = h_src->get_run_list();
              saHist::n_list fill_list = h_src->get_fill_list();

              if (run_list.size() == 0 && fill_list.size() == 0)
                {
                  cout << "Warning: Empty file " << line << " for "
                      << h_src->GetName() << endl;
//                  h_src->Print();
                  missing_files.push_back(line);
                }

              if (run_list.size())
                (hs[i])->AdoptSubHist(h_src, true); // run-by-run histograms

              if (fill_list.size())
                (hs[i])->AdoptSubHist(h_src, false); // fill by fill histograms

//              hs[i]->Print();
            }
          else
            {
              cout << "MergeHist - Error - Fail to merge " << h_src->GetName()
                  << endl;
              h_src->Print();
              missing_files.push_back(line);
            }

          delete h_src;

//          cout << "Processed " << hist_name << " : " << (hs[i])->GetName()
//              << " : " << hs[0] << ":" << hs[1] << endl;
        }

      delete _file0;
      _file1->cd();
    }

  for (int i = 0; i < n_hists; i++)
    {

      const TString hist_name = hist_names[i];

      cout << "Final touch on " << hist_name << " : " << (hs[i])->GetName()
          << endl;

      (hs[i])->Write();
    }

  delete _file1;

  for (int i = 0; i < n_hists; i++)
    {
      delete (hs[i]);
    }

  cout << "missing_files = " << missing_files.size() << endl;
}

void
MergeHistSUM(
    TString infile =
        "/phenix/u/jinhuang/links/fvtx_data/taxi/Run12pp510MuonPro100/4117/se-368692.lst",
    const int n_hists, const char * hist_names[])
{

  //load source files
  TFile *_file1 = TFile::Open(infile + ".MergeHistSUM.root", "recreate");

  fstream f_lst(infile, ios_base::in);

  string line;

  TH1 * hs[1000] =
    { NULL };
  for (int i = 0; i < n_hists; i++)
    hs[i] = NULL;

  while (!f_lst.eof())
    {
      getline(f_lst, line);

      cout << "void - Process " << line << endl;
      if (line.length() == 0)
        continue;

      cout << "MergeHist - process " << line << endl;

      TFile *_file0 = TFile::Open(line.c_str());
      assert(_file0);

      for (int i = 0; i < n_hists; i++)
        {

          _file0->cd();

          const TString hist_name = hist_names[i];

//          cout << "Loading " << hist_name << endl;

          TH1 * h_src = (TH1 *) _file0->GetObjectChecked(hist_name + "_SUM",
              "TH1");
          assert(h_src);

          if (!(hs[i]))
            {
              _file1->cd();
              cout
                  << "====================  h_src->MakeTemplate();  ====================="
                  << endl;
              h_src->SetObjectStat(0);
              (hs[i]) = (TH1F *) (h_src->Clone());
              assert((hs[i]));

//              cout << "Template Made " << hist_name << " : "
//                  << (hs[i])->GetName() << " : " << hs[i]->GetName() << endl;
            }

//          (hs[i])->AdoptRunSubHist(h_src);
          cout << "Merge " << h_src->GetName() << endl;
          (hs[i])->Add(h_src);

          delete h_src;

//          cout << "Processed " << hist_name << " : " << (hs[i])->GetName()
//              << " : " << hs[0] << ":" << hs[1] << endl;
        }

      delete _file0;
      _file1->cd();
    }

  for (int i = 0; i < n_hists; i++)
    {

      const TString hist_name = hist_names[i];

      cout << "Final touch on " << hist_name << " : " << (hs[i])->GetName()
          << endl;

      (hs[i])->Write();
    }

  delete _file1;
}

