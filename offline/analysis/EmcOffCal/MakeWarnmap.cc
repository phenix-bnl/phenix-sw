#include "MakeWarnmap.h"

#include <TCanvas.h>
#include <TF1.h>
#include <TFile.h>
#include <TGraphErrors.h>
#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include <TLine.h>
#include <TStyle.h>
#include <TSystem.h>
#include <TText.h>
#include <TVector3.h>


#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>

extern TSystem* gSystem;
extern TStyle* gStyle;

using namespace std;
using namespace EmcAnaCommon;

MakeWarnmap::MakeWarnmap(const Int_t n_ecore_range, const Double_t* ecore_range,
                         const Int_t n_run_range, const Int_t* run_range)
{
   m_fname_run_range = "";
   SetRunRange(n_run_range, run_range);
   SetEcoreRange(n_ecore_range, ecore_range);

   //// init hist
   gROOT->cd();
   double pos_array[100];
   for (int i = 0; i < 100; i++) pos_array[i] = i;
   for (Int_t irng = 0; irng < m_n_run_range; irng++) {
      for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
         char hname[256];
         sprintf(hname, "nhit_run%i_as%i", irng, ias);
      
         int ny_max, nz_max;
         if (IsPbGl(ias)) { ny_max = N_YPOS_PBGL;  nz_max = N_ZPOS_PBGL; }
         else             { ny_max = N_YPOS_PBSC;  nz_max = N_ZPOS_PBSC; }
      
         m_h3_nhit[irng][ias] = new TH3D(hname, "", nz_max, pos_array, ny_max, pos_array,
                                         n_ecore_range, ecore_range);
      }
   }

   memset(m_warnmap,     0, sizeof(m_warnmap));
   memset(m_current_map, 0, sizeof(m_current_map));
     
   gSystem->Exec("mkdir -p map/sub");
}

MakeWarnmap::~MakeWarnmap()
{
   ;
}

void MakeWarnmap::AddNhitInRun(Char_t* run_data_list)
{
     ifstream if_run_data_list(run_data_list);
     Int_t run;
     string data_file;
     while (if_run_data_list >> run >> data_file) {
        //// find run range
        Int_t range_run = -1;
        for (Int_t irng = 0; irng < m_n_run_range; irng++) {
           if (m_run_range[irng] <= run && run < m_run_range[irng + 1]) {
              range_run = irng;
	      cout<<"about to break"<<endl;
              break;
           }
        }
        if (range_run < 0) { cout<<"continued for range run"<<endl; continue; }
        cout << "Run " << run << " (range " << range_run << ")" << endl;
          
        TFile* file = new TFile(data_file.c_str());
        if (! file->IsOpen() ) {
           cerr << "cannot open file '" << data_file << "'\n";
           continue;
        }
        
        for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
           Char_t hname[1024];
           sprintf(hname, "nhit_run%i_as%i", run, ias);
           TH3D* h3_nhit = (TH3D*)file->Get(hname);

           if (! h3_nhit) { cout<<"did not get h3_nhit"<<endl; continue;}
           
           int Nbins_zpos  = h3_nhit->GetNbinsX();
           int Nbins_ypos  = h3_nhit->GetNbinsY();
           int Nbins_ecore = h3_nhit->GetNbinsZ();
           for (int ibin_zpos  = 1; ibin_zpos  <= Nbins_zpos;  ibin_zpos++) {
           for (int ibin_ypos  = 1; ibin_ypos  <= Nbins_ypos;  ibin_ypos++) {
           for (int ibin_ecore = 1; ibin_ecore <= Nbins_ecore; ibin_ecore++) {
              double ecore_cent = h3_nhit->GetZaxis()->GetBinCenter(ibin_ecore);
              double cont = h3_nhit->GetBinContent(ibin_zpos, ibin_ypos, ibin_ecore);

              int ibin_ecore_used = -1;
              const double* ecore_range = m_h3_nhit[0][0]->GetZaxis()->GetXbins()->GetArray();
              for (int i = 0; i < m_h3_nhit[0][0]->GetNbinsZ(); i++) {
		cout<<"ecore_range: "<<ecore_range[i]<<", ecore_cent: "<<ecore_cent<<", for i: "<<i<<endl;
                 if (ecore_range[i] <= ecore_cent && 
                     ecore_cent < ecore_range[i+1]) {
                    ibin_ecore_used = i + 1;
                    break;
                 }
              }
              if (ibin_ecore_used > 0) {
		cout<<"actually used a bin"<<endl;
                 m_h3_nhit[range_run][ias]->AddBinContent(
                    m_h3_nhit[range_run][ias]->GetBin(ibin_zpos, ibin_ypos, ibin_ecore_used), 
                    cont
                    );
              }
           }
           }
           }
        }
        file->Close();
     }
}

void MakeWarnmap::CalcDeadTower()
{
     const Int_t bin_ecore = 1; // only use the lowest ecore bin
     for (Int_t irng = 0; irng < m_n_run_range; irng++){
	  if (m_h3_nhit[irng][0] == 0 ||
              m_h3_nhit[irng][0]->Integral() == 0) continue;

	  memset(m_current_map, 0, sizeof(m_current_map));

	  for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
          for (Int_t iy = 0; iy < N_YPOS_PBGL; iy++ ) {
          for (Int_t iz = 0; iz < N_ZPOS_PBGL; iz++ ) {
          if (IsValidYZ(ias, iy, iz)) {
             Stat_t nhit = m_h3_nhit[irng][ias]->GetBinContent(
                m_h3_nhit[irng][ias]->GetXaxis()->FindBin(iz),
                m_h3_nhit[irng][ias]->GetYaxis()->FindBin(iy),
                bin_ecore
                );
             if (nhit == 0) {
                m_current_map[ias][iy][iz] = 1;
                m_warnmap    [ias][iy][iz] |= MASK_DEAD;
             }
          }
          }
          }
          }
//	  Char_t fname_map[256];
//	  sprintf(fname_map, "map/sub/dead_map_run_%i.eps", irng);
//	  DrawCurrentMap(fname_map);
     }
}

void MakeWarnmap::CalcHotTower(double sigma_cut, double sigma_cut_pbgl)
{
   gStyle->SetOptStat(0);
//     gStyle->SetOptStat(1111);
//     gStyle->SetOptFit(kTRUE);

   if (sigma_cut_pbgl < 0) sigma_cut_pbgl = sigma_cut;

   ofstream of_log("map/sub/calc_hot_tower.txt");

   TCanvas* c1_nhit = new TCanvas("c1_nhit");

   TF1* func = new TF1("func", "gaus");
   func->SetLineColor(4);

   TLine line;
   TText text;
   line.SetLineColor(2);
   text.SetTextColor(2);
   text.SetTextSize(0.1);
   
   for (Int_t irng = 0; irng < m_n_run_range; irng++) {
      if (m_h3_nhit[irng][0] == 0 ||
          m_h3_nhit[irng][0]->Integral() == 0) continue;
      
      of_log << "####" << endl
             << "#### Run Range " << irng << endl
             << "####" << endl;
      c1_nhit->Clear();
      c1_nhit->Divide(N_ARMSECT, m_n_ecore_range);
      
      TH1S* h1_nhit[N_ARMSECT][N_ECORE_RANGE_MAX];
      
      for (Int_t iec = 1; iec <= m_n_ecore_range; iec++) {
         of_log << "##" << endl
                << "## Ecore Range " << iec << endl
                << "##" << endl;

         memset(m_current_map, 0, sizeof(m_current_map));
         
         //// calculate the mean number of hits in one tower 
         //// to determine upper edge of histogram range
//         Double_t nhit_mean = 0.0;
//
//         for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
//         for (Int_t iy = 0; iy < N_YPOS_PBGL; iy++ ) {
//         for (Int_t iz = 0; iz < N_ZPOS_PBGL; iz++ ) {
//         if (IsValidYZ(ias, iy, iz)) {
//            nhit_mean += m_h3_nhit[irng][ias]->GetBinContent(
//               m_h3_nhit[irng][ias]->GetXaxis()->FindBin(iz),
//               m_h3_nhit[irng][ias]->GetYaxis()->FindBin(iy),
//               iec
//               );
//         }
//         }
//         }
//         }
//         nhit_mean /= 24768;
//         Int_t NHIT = 4*(Int_t)nhit_mean > 50  ?  4*(Int_t)nhit_mean  :  50;
//         Int_t NBIN = NHIT > 500  ?  500  :  NHIT;

         Int_t NHIT = 50000;
         Int_t NBIN = 50000;
         
         for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
            double scut = (IsPbGl(ias)  ?  sigma_cut_pbgl  :  sigma_cut);

            Int_t subpad_num = (iec - 1) * N_ARMSECT + ias + 1;
            c1_nhit->cd(subpad_num);
            c1_nhit->GetPad(subpad_num)->SetLogy(kTRUE);

            Char_t hname[256];
            sprintf(hname, "h1_nhit_as%i_ecore%i", ias, iec);
            h1_nhit[ias][iec] = new TH1S(hname, ";N of hits;N of towers", NBIN, 0, NHIT);
            
            //// fill 1d nhit hist
            for (Int_t iy = 0; iy < N_YPOS_PBGL; iy++ ) {
            for (Int_t iz = 0; iz < N_ZPOS_PBGL; iz++ ) {
            if (IsValidYZ(ias, iy, iz) && ! IsEdgePos(ias, iy, iz)) {
               Stat_t nhit = m_h3_nhit[irng][ias]->GetBinContent(
                  m_h3_nhit[irng][ias]->GetXaxis()->FindBin(iz),
                  m_h3_nhit[irng][ias]->GetYaxis()->FindBin(iy),
                  iec
                  );
               h1_nhit[ias][iec]->Fill(nhit);
            }
            }
            }

            for (Int_t itry = 0; itry < 10; itry++) {
               if (h1_nhit[ias][iec]->GetMaximum() > 50) break;
               h1_nhit[ias][iec]->Rebin();
            }

            //// determine nhit limit
//            h1_nhit[ias][iec]->Fit(func, "Q0");
            Double_t fit_upper = h1_nhit[ias][iec]->GetMean() + 3 * h1_nhit[ias][iec]->GetRMS();
//            func->SetParLimits(0, 0.0, 1e5);
            func->SetParLimits(1, 0.0, fit_upper);
//            func->SetParLimits(2, 0.0, NHIT);
            h1_nhit[ias][iec]->Fit(func, "EMQ", "", 0, fit_upper);
            Double_t mean  = func->GetParameter(1);
            Double_t sigma = func->GetParameter(2);
//            Double_t chi2  = func->GetChisquare();
            Int_t NDF      = func->GetNDF();

            Double_t nhit_limit;
            if (NDF > 0) {
               nhit_limit = mean + scut * sigma;
               h1_nhit[ias][iec]->GetXaxis()->SetRangeUser(0, 1.7 * nhit_limit);
            } else {
               nhit_limit = -1;
               h1_nhit[ias][iec]->GetXaxis()->SetRangeUser(0, 50);
            }

//            Double_t mean = h1_nhit[ias][iec]->GetMean();
//            Double_t sigma  = h1_nhit[ias][iec]->GetRMS();
//            Double_t nhit_limit = mean + scut * sigma;
            
            h1_nhit[ias][iec]->Draw();

            //// judge if each tower is hot
            Int_t n_bad;
            n_bad = 0;
            for (Int_t iy = 0; iy < N_YPOS_PBGL; iy++ ) {
            for (Int_t iz = 0; iz < N_ZPOS_PBGL; iz++ ) {
            if (IsValidYZ(ias, iy, iz) && ! IsEdgePos(ias, iy, iz)) {
               Stat_t nhit = m_h3_nhit[irng][ias]->GetBinContent(
                  m_h3_nhit[irng][ias]->GetXaxis()->FindBin(iz),
                  m_h3_nhit[irng][ias]->GetYaxis()->FindBin(iy),
                  iec
                  );
               if (nhit_limit > 0 && nhit > nhit_limit) {
                  m_current_map[ias][iy][iz] = 1;
                  m_warnmap[ias][iy][iz] |= MASK_HOT;
                  n_bad++;
               }
            }
            }
            }

            line.DrawLine(nhit_limit, 0.0, nhit_limit, 
                          h1_nhit[ias][iec]->GetMaximum() / 4.0);

            Char_t string_draw[256];
            sprintf(string_draw, "run range: %i",   irng);
            text.DrawTextNDC(0.5, 0.9, string_draw);
            sprintf(string_draw, "ecore range: %i", iec);
            text.DrawTextNDC(0.5, 0.8, string_draw);
            sprintf(string_draw, "sector: %i",      ias);
            text.DrawTextNDC(0.5, 0.7, string_draw);
            sprintf(string_draw, "mean: %.1f",      mean);
            text.DrawTextNDC(0.5, 0.6, string_draw);
            sprintf(string_draw, "sigma: %.1f",     sigma);
            text.DrawTextNDC(0.5, 0.5, string_draw);
//            sprintf(string_draw, "chi2: %.1f/%i", chi2, NDF);
//            text.DrawTextNDC(0.5, 0.4, string_draw);
            sprintf(string_draw, "limit: %.1f",     nhit_limit);
            text.DrawTextNDC(0.5, 0.3, string_draw);
            sprintf(string_draw, "N bad: %i",       n_bad);
            text.DrawTextNDC(0.5, 0.2, string_draw);

            of_log << ias << "   " << setw(8) << mean << "   " 
                   << setw(8) << sigma << "   "
                   << setw(5) << nhit_limit << "   " 
                   << setw(4) << n_bad << endl;
         }
//         Char_t fname_map[256];
//         sprintf(fname_map, "map/sub/hot_map_run_%i_ecore_%i.eps", irng, iec);
//         DrawCurrentMap(fname_map);
      }
      
      Char_t epsfname[256];
      sprintf(epsfname, "map/sub/nhit_dist_run_%i.eps", irng);
      c1_nhit->SaveAs(epsfname);
      for (Int_t iec = 1; iec <= m_n_ecore_range; iec++) {
         for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
            delete h1_nhit[ias][iec];
         }
      }
   }

   delete func;
}

void MakeWarnmap::DrawCurrentMap(Char_t* epsfname)
{
     TCanvas* c1_map = new TCanvas("c1_map");
     c1_map->Divide(2, 4);

     TH2D* h2_map[N_ARMSECT];
     for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
	  Char_t hname[256];
	  sprintf(hname, "h2_map_as%i", ias);
	  Int_t Ny, Nz;
	  if (IsPbGl(ias)) { Ny = N_YPOS_PBGL     ; Nz = N_ZPOS_PBGL; }
	  else                     { Ny = N_YPOS_PBSC; Nz = N_ZPOS_PBSC; }
	  h2_map[ias] = new TH2D(hname, "", Nz, 0, Nz, Ny, 0, Ny);
	  h2_map[ias]->SetStats(kFALSE);

	  Int_t ntot = 0;
	  for (Int_t iy  = 0; iy < N_YPOS_PBGL; iy++) {
	  for (Int_t iz  = 0; iz < N_ZPOS_PBGL; iz++) {
	  if (IsValidYZ(ias, iy, iz)) {
	       h2_map[ias]->Fill(iz, iy, m_current_map[ias][iy][iz]);
	       if (m_current_map[ias][iy][iz] != 0) ntot++;
	  }
	  }
	  }

	  Char_t htitle[256];
	  sprintf(htitle, "sector %i (N = %i);zpos;ypos", ias, ntot);
	  h2_map[ias]->SetTitle(htitle);

	  c1_map->cd( (ias<4) ? 2*(3-ias)+1 : 2*(8-ias) );
	  h2_map[ias]->Draw("col");
     }

     c1_map->SaveAs(epsfname);
     for (Int_t ias = 0; ias < N_ARMSECT; ias++) delete h2_map[ias];
     delete c1_map;
}

void MakeWarnmap::SetUncalibTower(Char_t* fname_uncalib_list)
{
   //// ignore 4th column (status etc.)
   ifstream if_uncalib_list(fname_uncalib_list);
   string one_line;
   while (getline(if_uncalib_list, one_line)) {
      istringstream iss_one_line(one_line.c_str());
      int as, y, z;
      if (iss_one_line >> as >> y >> z) {
         m_warnmap[as][y][z] |= MASK_UNCALIB;
      }
   }
   if_uncalib_list.close();
}

void MakeWarnmap::SetAroundAndEdge()
{
     for (int ias = 0; ias < N_ARMSECT; ias++) {
     for (int iypos = 0; iypos < N_YPOS_PBGL; iypos++) {
     for (int izpos = 0; izpos < N_ZPOS_PBGL; izpos++) {
     if (IsValidYZ(ias, iypos, izpos)) {

	  //// check around hot/dead/uncalib
          for (int dy = -1; dy <= 1; dy++) {
          for (int dz = -1; dz <= 1; dz++) {
          if ((! (dy == 0 && dz == 0)) && //// except itself
              IsValidYZ(ias, iypos + dy, izpos + dz)) {
	       if (m_warnmap[ias][iypos + dy][izpos + dz] & MASK_HOT) {
		    m_warnmap[ias][iypos][izpos] |= MASK_AROUND_HOT;
	       }
	       if (m_warnmap[ias][iypos + dy][izpos + dz] & MASK_DEAD) {
		    m_warnmap[ias][iypos][izpos] |= MASK_AROUND_DEAD;
	       }
	       if (m_warnmap[ias][iypos + dy][izpos + dz] & MASK_UNCALIB) {
		    m_warnmap[ias][iypos][izpos] |= MASK_AROUND_UNCALIB;
	       }
	  }
	  }
	  }
	  
	  //// check edge
	  if (IsPbGl(ias)) {
	       if (iypos == 0 || iypos == N_YPOS_PBGL-1 || 
		   izpos == 0 || izpos == N_ZPOS_PBGL-1) {
		    m_warnmap[ias][iypos][izpos] |= MASK_EDGE;
	       }
	  } else {
	       if (iypos == 0 || iypos == N_YPOS_PBSC-1 ||
		   izpos == 0 || izpos == N_ZPOS_PBSC-1) {
		    m_warnmap[ias][iypos][izpos] |= MASK_EDGE;
	       }
	  }
     }
     }
     }
     }
}

void MakeWarnmap::DumpResult(char* fname_map)
{
     FILE* of_status;
     if ( (of_status = fopen(fname_map, "w")) == 0 ) {
	  fprintf(stderr, "File open error: %s\n", fname_map);
	  return;
     }

     TH2D* h2_status[N_ARMSECT];
     for (int ias = 0; ias < N_ARMSECT; ias++) {
	  Int_t Ny, Nz;
	  if (IsPbGl(ias)) { Ny = N_YPOS_PBGL; Nz = N_ZPOS_PBGL; } 
	  else             { Ny = N_YPOS_PBSC; Nz = N_ZPOS_PBSC; }

	  char hname[128], htitle[128];
	  sprintf(hname, "h2_tower_status_%i", ias);
	  sprintf(htitle, "sector %i;zpos;ypos", ias);
	  h2_status[ias] = new TH2D(hname, htitle, Nz, 0, Nz, Ny, 0, Ny);

	  const Int_t Ncont = 5; // good, hot, dead, uncalib, around/edge
	  h2_status[ias]->GetZaxis()->SetRangeUser(0, Ncont);
	  gStyle->SetNumberContours(Ncont);

	  for (int iy = 0; iy < N_YPOS_PBGL; iy++) {
          for (int iz = 0; iz < N_ZPOS_PBGL; iz++) {
	  if (IsValidYZ(ias, iy, iz)) {
	       fprintf(of_status, "%i  %2i  %2i  %2i\n", 
		       ias, iy, iz, m_warnmap[ias][iy][iz]);

	       Int_t status;
	       if      (m_warnmap[ias][iy][iz] & MASK_HOT)     status = 4;
	       else if (m_warnmap[ias][iy][iz] & MASK_DEAD)    status = 3;
	       else if (m_warnmap[ias][iy][iz] & MASK_UNCALIB) status = 2;
	       else if (m_warnmap[ias][iy][iz] & MASK_AROUND_HOT     ||
			m_warnmap[ias][iy][iz] & MASK_AROUND_DEAD    ||
			m_warnmap[ias][iy][iz] & MASK_AROUND_UNCALIB ||
			m_warnmap[ias][iy][iz] & MASK_EDGE ) status = 1;
	       else                                          status = 0;
	       h2_status[ias]->Fill(iz, iy, status);
	  }
	  }
	  }
     }

     TCanvas* canvas = new TCanvas("canvas", "", 500, 700);
     canvas->Divide(1, N_SECTOR);
     for (Int_t iarm = 0; iarm < N_ARM; iarm++) {
          for (Int_t isector = 0; isector < N_SECTOR; isector++) {
	       Int_t ias = iarm * N_SECTOR + isector;
               canvas->cd(N_SECTOR - isector);
               h2_status[ias]->SetStats(kFALSE);
               h2_status[ias]->Draw("col");
          }
	  char epsfname[128];
	  sprintf(epsfname, "map/warnmap_arm_%i.eps", iarm);
          canvas->SaveAs(epsfname);
     }

     delete canvas;
     for (Int_t ias = 0; ias < N_ARMSECT; ias++) { delete h2_status[ias]; }
     fclose(of_status);
}

void MakeWarnmap::DrawEcoreDist()
{
   TH1D* h1_ecore_bef[N_ARMSECT];
   TH1D* h1_ecore_aft[N_ARMSECT];

   for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
      ostringstream oss_hname;
      oss_hname << "h1_ecore_bef_" << ias;
      h1_ecore_bef[ias] = new TH1D(oss_hname.str().c_str(), "", m_n_ecore_range, m_ecore_range);

      oss_hname.str("");
      oss_hname << "h1_ecore_aft_" << ias;
      h1_ecore_aft[ias] = new TH1D(oss_hname.str().c_str(), "", m_n_ecore_range, m_ecore_range);
   }

   for (Int_t irun = 0; irun < m_n_run_range; irun++) {
      for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
      for (int iy = 0; iy < N_YPOS_PBGL; iy++) {
      for (int iz = 0; iz < N_ZPOS_PBGL; iz++) {
      if (IsValidYZ(ias, iy, iz)) {
         for (Int_t ie = 0; ie < m_n_ecore_range; ie++) {
            Double_t cont = m_h3_nhit[irun][ias]->GetBinContent(iz+1, iy+1, ie+1);
            h1_ecore_bef[ias]->AddBinContent(ie+1, cont);
            if (! m_warnmap[ias][iy][iz]) h1_ecore_aft[ias]->AddBinContent(ie+1, cont);
         }
      }
      }
      }
      }
   }

   TCanvas* c1_ecore_bef = new TCanvas("c1_ecore_bef", "");
   TCanvas* c1_ecore_aft = new TCanvas("c1_ecore_aft", "");
   c1_ecore_bef->SetLogx(kTRUE);  c1_ecore_bef->SetLogy(kTRUE);
   c1_ecore_aft->SetLogx(kTRUE);  c1_ecore_aft->SetLogy(kTRUE);
   for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
      TText label;
      label.SetTextColor(ias + 1);

      c1_ecore_bef->cd();
      h1_ecore_bef[ias]->SetLineColor(ias + 1);
      if (ias == 0) {
         h1_ecore_bef[ias]->SetTitle("Before applying warnmap;ecore;yield");
         h1_ecore_bef[ias]->Draw("E1");
      } else {
         h1_ecore_bef[ias]->Draw("E1same");
      }
      label.DrawTextNDC(0.8, 0.8 - 0.05 * ias, SECTOR_NAME[ias]);

      Int_t ntwr_tot = 0;
      Int_t ntwr_good = 0;
      for (Int_t iy = 0; iy < N_YPOS_PBGL; iy++) {
      for (Int_t iz = 0; iz < N_ZPOS_PBGL; iz++) {
      if (IsValidYZ(ias, iy, iz) && ! IsEdgePos(ias, iy, iz)) {
         ntwr_tot++;
         if (m_warnmap[ias][iy][iz] == 0) ntwr_good++;
      }
      }
      }
      Double_t frac_good = (Double_t)ntwr_good/ntwr_tot;
      cout << ias << "  " << frac_good << " = " << ntwr_good << " / " << ntwr_tot << endl;
      h1_ecore_aft[ias]->Sumw2();
      h1_ecore_aft[ias]->Scale(1 / frac_good);
      
      c1_ecore_aft->cd();
      h1_ecore_aft[ias]->SetLineColor(ias + 1);
      if (ias == 0) {
         h1_ecore_aft[ias]->SetTitle("After applying warnmap;ecore;yield / live area fraction");
         h1_ecore_aft[ias]->Draw("E1");
      } else {
         h1_ecore_aft[ias]->Draw("E1same");
      }
      label.DrawTextNDC(0.8, 0.8 - 0.05 * ias, SECTOR_NAME[ias]);
   }
   c1_ecore_bef->SaveAs("map/ecore_dist_bef.eps");
   c1_ecore_aft->SaveAs("map/ecore_dist_aft.eps");

   delete c1_ecore_bef;
   delete c1_ecore_aft;

   for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
      delete h1_ecore_bef[ias];
      delete h1_ecore_aft[ias];
   }
}

void MakeWarnmap::SetEcoreRange(const Int_t n_ecore_range, const Double_t* ecore_range)
{
   int irng_used = 0;
   for (int irng_all = 0; irng_all <= N_ECORE_RANGE_FOR_NHIT_HIST; irng_all++) {
      if (ecore_range[irng_used] == ECORE_RANGE_FOR_NHIT_HIST[irng_all]) {
         irng_used++;
         if (irng_used == n_ecore_range + 1) { //// set and return
            m_n_ecore_range = n_ecore_range;
            memcpy(m_ecore_range, ecore_range, 
                   (n_ecore_range + 1) * sizeof(Double_t));
            cout << "Ecore range: n = " << m_n_ecore_range << endl << "    ";
            for (int i = 0; i < m_n_ecore_range + 1; i++) {
               cout << m_ecore_range[i] << "   ";
            }
            cout << endl << endl;
            return;
         }
      }
   }
   cout << "ecore_range doesn't match with ECORE_RANGE_FOR_NHIT_HIST." << endl;
   exit(1);
}

void MakeWarnmap::SetRunRange(const Int_t n_run_range, const Int_t* run_range)
{
   m_n_run_range = n_run_range;
   for (int i = 0; i <= m_n_run_range; i++) {
      m_run_range[i] = run_range[i];
   }
   cout << "Run range: n = " << m_n_run_range << endl << "    ";
   for (int i = 0; i < m_n_run_range + 1; i++) {
      cout << m_run_range[i] << "   ";
   }
   cout << endl << endl;
}

void MakeWarnmap::SetRunRange()
{
   ifstream if_run_range(m_fname_run_range.c_str());

   m_n_run_range = 0;
   int run;
   while (if_run_range >> run) {
      m_run_range[m_n_run_range] = run;
      m_n_run_range++;
   }
   m_n_run_range -= 1;
   if (m_n_run_range < 1) {
      m_n_run_range = 1;
      m_run_range[0] = 0;
      m_run_range[1] = 9999999;
   }

   cout << "Run range: n = " << m_n_run_range << endl << "    ";
   for (int i = 0; i < m_n_run_range + 1; i++) {
      cout << m_run_range[i] << "   ";
   }
   cout << endl << endl;

   if_run_range.close();
}
