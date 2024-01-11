#include <cassert>
#include "SaveCanvas.C"
#include "SetOKStyle.C"
using namespace std;

TTree * alignment;

void
GetBeamPos(

//    TString infile =
//        "/phenix/u/jinhuang/links/fvtx_data/zero_field/ZEROFDATA_P00-0000366446-0000_eval.root",
//    int run = 366446

//    TString infile =
//        "/phenix/u/jinhuang/links/fvtx_data/zero_field/ZEROFDATA_P00-0000366446-0001_eval.root",
//    int run = 366446

//    TString infile =
//        "/phenix/u/jinhuang/links/fvtx_data/zero_field/ZEROFDATA_P00-0000368800-0000_eval.root",
//    int run = 368800

    //    TString infile =
    //        "/phenix/u/jinhuang/links/fvtx_data/zero_field/ZEROFDATA_P00-0000368800-0001_eval.root",
    //    int run = 368800

//        TString infile =
//            "/phenix/u/jinhuang/links/fvtx_data/zero_field/ZEROFDATA_P00-0000368800-0002_eval.root",
//        int run = 368800

//        TString infile =
//            "/phenix/u/jinhuang/links/fvtx_data/zero_field/ZEROFDATA_P00-0000368800-0003_eval.root",
//        int run = 368800

//    TString infile =
//        "/phenix/u/jinhuang/links/fvtx_data/zero_field/ZEROFDATA_P00-0000368197-0000_eval.root",
//    int run = 368197

//    TString infile =
//        "/phenix/u/jinhuang/links/fvtx_data/zero_field/ZEROFDATA_P00-0000367109-0000_eval.root",
//    int run = 367109

//    TString infile =
//        "/phenix/u/jinhuang/links/fvtx_data/zero_field/ZEROFDATA_P00-0000367109-0001_eval.root",
//    int run = 367109

    //    TString infile =
    //        "/direct/phenix+scratch/jinhuang/miliped_work/AlignDST_GD1/result/GOLDENEVENT_DIMUON_run12_online_muon-0000367545-0000.AlignDST.root",
    //    int run = 367545

//    TString infile =
//        "./FvtxGlobalAlign_AlignDST.root",
//    int run = 0
    TString infile =
        "./FvtxGlobalAlign_AlignDST.root",
    int run = 0
    )
{
  TVirtualFitter::SetDefaultFitter("Minuit2");
  TVirtualFitter::SetDefaultFitter("MINUIT2");

  cout << "run = " << run << endl;

  SetOKStyle();
  gStyle->SetOptStat(1111);
  gStyle->SetOptFit(1111);

  TFile *_file0 = TFile::Open(infile);

  assert(_file0);

  alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  TCanvas *c1 = new TCanvas("GetBeamPos", "GetBeamPos", 1800 * .5, 900 * .5);
  c1->Divide(2, 1);
  int idx = 1;

  c1->cd(idx++);
  c1->Update();
  pair<double, double> x = DrawAndFit("vtx_point.fX");

  c1->cd(idx++);
  c1->Update();
  pair<double, double> y = DrawAndFit("vtx_point.fY");

  fstream flog(infile + ".GetBeamPos", ios_base::out);
  flog << run << "\t" << x.first << "\t" << y.first << "\t" << x.second << "\t"
      << y.second << endl;
  flog.close();

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);
}

pair<double, double>
DrawAndFit(TString v)
{
  assert(alignment);

  TH1F * h = new TH1F(v, v, 100, -1, 1);

  alignment->Draw(v + ">>" + v);

  const double default_signma = .05;
  const double peak = h->GetXaxis()->GetBinCenter(h->GetMaximumBin());

  TF1 * f1 = new TF1("gaus", "gaus", peak - 2* default_signma,
      peak + 2* default_signma);
  TF1 * f2 = new TF1("dgaus" + v, "gaus + [3]*exp(-0.5*((x-[1])/[4])**2) + [5]",
      -.5, .5);
  TF1 * f3 =
      new TF1("tgaus" + v,
          "gaus + [3]*exp(-0.5*((x-[1])/[4])**2) + [5]+ [6]*exp(-0.5*((x-[1])/[7])**2)",
          -.5, .5);

  f1->SetLineColor(kGreen);
  f2->SetLineColor(kRed);
  f3->SetLineColor(kBlue);

  f1->SetParameters(12, peak, default_signma);
  h->Fit(f1, "MR");

//  f2->SetParameters(f1->GetParameter(0) / 2, f1->GetParameter(1),
//      f1->GetParameter(2), f1->GetParameter(0) / 2, f1->GetParameter(2) * 2, 0);
//
//  h->Fit(f2, "MR");

//  f3->SetParameters(f2->GetParameter(0), f2->GetParameter(1),
//      f2->GetParameter(2), f2->GetParameter(3), f2->GetParameter(4),
//      f2->GetParameter(5), f2->GetParameter(0), f2->GetParameter(2) / 2);
//
//  h->Fit(f3, "MR");

  return pair<double, double>(f1->GetParameter(1), f1->GetParError(1));
}

