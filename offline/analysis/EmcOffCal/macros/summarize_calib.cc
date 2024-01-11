#include "/direct/phenix+u/workarea/manion/source/EMC_Calib/EmcAnaCommon.h"

string PI0_HIST_DIR;
string WARNMAP_FILE_NAME;

string FNAME_ROOT_BEF;
string FNAME_TABLE_BEF;

string FNAME_ROOT_AFT;
string FNAME_TABLE_AFT;

string FNAME_COEF;
string FNAME_COEF_SUPERMOD;
string FNAME_COEF_RUN5;
string FNAME_UNCALIB_LIST;

class UncalibTowerList;
UncalibTowerList* uncalib_list;

void MakeMeanTowerByTower(
   TH1D* h1_mean[N_ARMSECT], TH1D* h1_sigma[N_ARMSECT], char* fname_table);

void MakeMeanVsPt(
   TGraphErrors* gr_mean[N_ARMSECT], TGraphErrors* gr_sigma[N_ARMSECT], 
   TGraphErrors* gr_rel_sigma[N_ARMSECT], 
   char* fname_root, double pT_min, double pT_max);

void summarize_calib()
{

   gROOT->SetStyle("Plain");
   gStyle->SetLabelFont(22, "XYZ");
   gStyle->SetTitleFont(22, "");
   gStyle->SetTitleFont(22, "XYZ");
   gStyle->SetLabelSize(0.06, "XYZ");
   gStyle->SetStatFont(22);
   gStyle->SetTextFont(22);
   gStyle->SetOptStat(0000);
   gStyle->SetPalette(1);

   gSystem->Load("/direct/phenix+u/workarea/manion/build/EMC_Calib/.libs/libEmcOffCal.so");
   using namespace EmcAnaCommon;

   PI0_HIST_DIR = gSystem->Getenv("PI0_HIST_DIR");
   WARNMAP_FILE_NAME      = gSystem->Getenv("WARNMAP_FILE_NAME");

   FNAME_ROOT_BEF  = gSystem->Getenv("FNAME_ROOT_BEF");
   FNAME_TABLE_BEF = gSystem->Getenv("FNAME_TABLE_BEF");

   FNAME_ROOT_AFT  = gSystem->Getenv("FNAME_ROOT_AFT");
   FNAME_TABLE_AFT = gSystem->Getenv("FNAME_TABLE_AFT");

   FNAME_COEF      = gSystem->Getenv("FNAME_COEF");
   FNAME_COEF_SUPERMOD      = gSystem->Getenv("FNAME_COEF_SUPERMOD");
   FNAME_COEF_RUN5 = gSystem->Getenv("FNAME_COEF_RUN5");
   FNAME_UNCALIB_LIST = gSystem->Getenv("FNAME_UNCALIB_LIST");

   ////
   //// uncalibrated towers
   ////
   string fname_uncalib_tower = PI0_HIST_DIR + "/uncalib_tower.eps";
   string fname_uncalib_number = PI0_HIST_DIR + "/uncalib_number.txt";
   uncalib_list = new UncalibTowerList();
   uncalib_list->ReadUncalibList(FNAME_UNCALIB_LIST.c_str());
   uncalib_list->Draw(fname_uncalib_tower.c_str());
   uncalib_list->Print(fname_uncalib_number.c_str(), WARNMAP_FILE_NAME.c_str());

   ////
   //// coefficient
   ////
   string fname_coef_1d   = PI0_HIST_DIR + "/coef_1d.eps";
   string fname_coef_2d   = PI0_HIST_DIR + "/coef_2d.eps";
   string fname_coef_diff = PI0_HIST_DIR + "/coef_diff.eps";
   string froot_name = PI0_HIST_DIR + "/coeff_hists.root";
   Coefficient* coef = new Coefficient();
   coef->ReadCoef(FNAME_COEF.c_str(),FNAME_COEF_SUPERMOD.c_str());
   coef->Draw(fname_coef_1d.c_str(), fname_coef_2d.c_str(),FNAME_UNCALIB_LIST.c_str(), froot_name.c_str());
//   coef->DrawDiff(FNAME_COEF_RUN5.c_str(), fname_coef_diff.c_str());
   delete coef;

   ////
   //// tower-by-tower mean and sigma
   ////
   string fname_mean_tower  = PI0_HIST_DIR + "/mean_twr_by_twr.eps";
   string fname_sigma_tower = PI0_HIST_DIR + "/sigma_twr_by_twr.eps";

   TH1D* h1_mean_bef[N_ARMSECT];
   TH1D* h1_sigma_bef[N_ARMSECT];

   TH1D* h1_mean_aft[N_ARMSECT];
   TH1D* h1_sigma_aft[N_ARMSECT];

   MakeMeanTowerByTower(h1_mean_bef, h1_sigma_bef, FNAME_TABLE_BEF.c_str());
   MakeMeanTowerByTower(h1_mean_aft, h1_sigma_aft, FNAME_TABLE_AFT.c_str());

   TCanvas* c1_mean_twr = new TCanvas("c1_mean_twr", "");
   c1_mean_twr->Divide(4, 2);
   TCanvas* c1_sigma_twr = new TCanvas("c1_sigma_twr", "");
   c1_sigma_twr->Divide(4, 2);

   for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
      c1_mean_twr->cd(ias + 1);
      c1_mean_twr->GetPad(ias + 1)->SetLogy(kTRUE);
      h1_mean_bef[ias]->Draw();
      //h1_mean_bef[ias]->SetMaximum(800);
      h1_mean_bef[ias]->SetMaximum(2000);
      
      h1_mean_aft[ias]->SetLineColor(2);
      h1_mean_aft[ias]->Draw("same");

      c1_sigma_twr->cd(ias + 1);
      c1_sigma_twr->GetPad(ias + 1)->SetLogy(kTRUE);
      h1_sigma_bef[ias]->Draw();
      //h1_sigma_bef[ias]->SetMaximum(400);

      h1_sigma_aft[ias]->SetLineColor(2);
      h1_sigma_aft[ias]->Draw("same");
   }

   c1_mean_twr ->SaveAs(fname_mean_tower.c_str());
   c1_sigma_twr->SaveAs(fname_sigma_tower.c_str());

   delete c1_mean_twr;
   delete c1_sigma_twr;

   ////
   //// mean/sigma vs pT
   ////
   string fname_mean_vs_pT      = PI0_HIST_DIR + "/mean_vs_pT.eps";
   string fname_sigma_vs_pT     = PI0_HIST_DIR + "/sigma_vs_pT.eps";
   string fname_rel_sigma_vs_pT = PI0_HIST_DIR + "/rel_sigma_vs_pT.eps";

   const double mean_min = 0.125;
   const double mean_max = 0.190;
   const double sigma_min = 0.008;
   const double sigma_max = 0.020;
   const double rel_sigma_min =  6;
   const double rel_sigma_max = 11;
   const double pT_min = 1.0;
   const double pT_max = 4.0;

   TGraphErrors* gr_mean_vs_pT_bef     [N_ARMSECT];
   TGraphErrors* gr_sigma_vs_pT_bef    [N_ARMSECT];
   TGraphErrors* gr_rel_sigma_vs_pT_bef[N_ARMSECT];

   TGraphErrors* gr_mean_vs_pT_aft     [N_ARMSECT];
   TGraphErrors* gr_sigma_vs_pT_aft    [N_ARMSECT];
   TGraphErrors* gr_rel_sigma_vs_pT_aft[N_ARMSECT];

   MakeMeanVsPt(gr_mean_vs_pT_bef, gr_sigma_vs_pT_bef, gr_rel_sigma_vs_pT_bef, FNAME_ROOT_BEF.c_str(), pT_min, pT_max);
   MakeMeanVsPt(gr_mean_vs_pT_aft, gr_sigma_vs_pT_aft, gr_rel_sigma_vs_pT_aft, FNAME_ROOT_AFT.c_str(), pT_min, pT_max);

   TCanvas* c1_mean_vs_pT      = new TCanvas("c1_mean_vs_pT", "");
   TCanvas* c1_sigma_vs_pT     = new TCanvas("c1_sigma_vs_pT", "");
   TCanvas* c1_rel_sigma_vs_pT = new TCanvas("c1_rel_sigma_vs_pT", "");
   c1_mean_vs_pT     ->Divide(4, 2);
   c1_sigma_vs_pT    ->Divide(4, 2);
   c1_rel_sigma_vs_pT->Divide(4, 2);

   TLatex latex1;
   latex1.SetNDC(kTRUE);
   latex1.SetTextSize(.1);

   for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
      c1_mean_vs_pT->cd(ias + 1);
      gr_mean_vs_pT_bef[ias]->Draw("AP");
      gr_mean_vs_pT_bef[ias]->GetYaxis()->SetRangeUser(mean_min, mean_max);

      gr_mean_vs_pT_aft[ias]->SetLineColor(2);
      gr_mean_vs_pT_aft[ias]->Draw("Psame");

      latex1.DrawLatex(0.2, 0.8, SECTOR_NAME[ias]);

      c1_sigma_vs_pT->cd(ias + 1);
      gr_sigma_vs_pT_bef[ias]->Draw("AP");
      gr_sigma_vs_pT_bef[ias]->GetYaxis()->SetRangeUser(sigma_min, sigma_max);

      gr_sigma_vs_pT_aft[ias]->SetLineColor(2);
      gr_sigma_vs_pT_aft[ias]->Draw("Psame");

      latex1.DrawLatex(0.2, 0.8, SECTOR_NAME[ias]);

      c1_rel_sigma_vs_pT->cd(ias + 1);
      gr_rel_sigma_vs_pT_bef[ias]->Draw("AP");
      gr_rel_sigma_vs_pT_bef[ias]->GetYaxis()->SetRangeUser(rel_sigma_min, rel_sigma_max);

      gr_rel_sigma_vs_pT_aft[ias]->SetLineColor(2);
      gr_rel_sigma_vs_pT_aft[ias]->Draw("Psame");

      latex1.DrawLatex(0.2, 0.8, SECTOR_NAME[ias]);
   }

   TLatex latex2;
   latex2.SetNDC(kTRUE);
   latex2.SetTextAlign(22);

   c1_mean_vs_pT->cd(0);       
   latex2.DrawLatex(0.5, 0.48, "Mass Peak Position vs #pi^{0} p_{T}");
   c1_sigma_vs_pT->cd(0); 
   latex2.DrawLatex(0.5, 0.48, "Mass Peak Width vs #pi^{0} p_{T}");
   c1_rel_sigma_vs_pT->cd(0); 
   latex2.DrawLatex(0.5, 0.48, "Mass Peak Width (%) vs #pi^{0} p_{T}");

   c1_mean_vs_pT     ->SaveAs(fname_mean_vs_pT.c_str());
   c1_sigma_vs_pT    ->SaveAs(fname_sigma_vs_pT.c_str());
   c1_rel_sigma_vs_pT->SaveAs(fname_rel_sigma_vs_pT.c_str());

   delete c1_mean_vs_pT;
   delete c1_sigma_vs_pT;
   delete c1_rel_sigma_vs_pT;

   delete uncalib_list;
}

void MakeMeanTowerByTower(
   TH1D* h1_mean[N_ARMSECT], TH1D* h1_sigma[N_ARMSECT], char* fname_table)
{
   for (int ias = 0; ias < N_ARMSECT; ias++) {
      ostringstream oss_name, oss_title;
      oss_name << "h1_mean_tower_as" << ias;
      while (gROOT->FindObject(oss_name.str().c_str())) oss_name << "z";
      oss_title << SECTOR_NAME[ias] << ";peak position;towers";
      h1_mean[ias] = new TH1D(oss_name.str().c_str(), oss_title.str().c_str(),
                              200, 0.08, 0.25);

      oss_name.str("");  oss_title.str("");
      oss_name << "h1_sigma_tower_as" << ias;
      while (gROOT->FindObject(oss_name.str().c_str())) oss_name << "z";
      oss_title << SECTOR_NAME[ias] << ";peak width;towers";
      h1_sigma[ias] = new TH1D(oss_name.str().c_str(), oss_title.str().c_str(),
                               200, 0.0, 0.05);
   }

   Int_t as, y, z;
   Double_t coef, new_coef, new_coef_err;
   Double_t height, height_err, mean, mean_err, sigma, sigma_err, chi2;
   Int_t NDF, poln;
   Double_t signal, bg;
   
   ifstream if_table(fname_table);
   while (if_table >> as >> y >> z >> coef >> new_coef >> new_coef_err
          >> height >> height_err >> mean >> mean_err 
          >> sigma >> sigma_err
          >> chi2 >> NDF >> poln >> signal >> bg) {
      if (! IsEdgePos(as, y, z) &&
          ! uncalib_list->IsInUncalibList(AsYZ2TowerID(as, y, z)) ) {
         h1_mean [as]->Fill(mean);
         h1_sigma[as]->Fill(fabs(sigma));
      }
   }
   if_table.close();
}

void MakeMeanVsPt(
   TGraphErrors* gr_mean[N_ARMSECT], TGraphErrors* gr_sigma[N_ARMSECT], 
   TGraphErrors* gr_rel_sigma[N_ARMSECT], 
   char* fname_root, double pT_min, double pT_max)
{
   for (int ias = 0; ias < N_ARMSECT; ias++) {
      gr_mean     [ias] = new TGraphErrors();
      gr_sigma    [ias] = new TGraphErrors();
      gr_rel_sigma[ias] = new TGraphErrors();

//      gr_mean[ias]     ->SetTitle(";#pi^{0} p_{T};");
//      gr_sigma[ias]    ->SetTitle(";#pi^{0} p_{T};");
//      gr_rel_sigma[ias]->SetTitle(";#pi^{0} p_{T};");
   }

   Pi0MassFitter* fitter = new Pi0MassFitter();

   TFile* ifile = new TFile(fname_root);
   for (int ias = 0; ias < N_ARMSECT; ias++) {
      ostringstream oss_hname;
      oss_hname.str("");
      oss_hname << "h2_pi0mass_as" << ias<<"_v2";
      TH2* h2_mass = (TH2*)ifile->Get(oss_hname.str().c_str());

      for (int pT_bin = 1; pT_bin <= h2_mass->GetNbinsY(); pT_bin++) {
         Double_t pT_cent = h2_mass->GetYaxis()->GetBinCenter(pT_bin);

         if (pT_cent < pT_min || pT_cent > pT_max) continue;

         fitter->SetHist(h2_mass, pT_bin, pT_bin);
         fitter->FitMass();
         Double_t height, height_err;
         Double_t mean, mean_err;
         Double_t sigma, sigma_err;
         Double_t chi2;
         Int_t NDF, poln;
         Double_t signal, bg;
         fitter->GetResults(height, height_err, mean, mean_err,
                            sigma, sigma_err, chi2, NDF, poln, signal, bg);

         Int_t n_point = gr_mean[ias]->GetN();
         gr_mean[ias]->SetPoint(n_point, pT_cent, mean);
         gr_mean[ias]->SetPointError(n_point, 0, mean_err);

         gr_sigma[ias]->SetPoint(n_point, pT_cent, sigma);
         gr_sigma[ias]->SetPointError(n_point, 0, sigma_err);

         gr_rel_sigma[ias]->SetPoint(n_point, pT_cent, 100*sigma/mean);
         gr_rel_sigma[ias]->SetPointError(n_point, 0, 100*sigma_err/mean);
      }
   }
//   delete ifile;
   delete fitter;
}
