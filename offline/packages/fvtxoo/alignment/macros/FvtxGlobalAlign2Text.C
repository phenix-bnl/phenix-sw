#include <cassert>
#include <fstream>
//#include <cmath>
//#include <TFvtxAlign.h>
#include <TFile.h>
#include <TROOT.h>
#include <TTree.h>
#include <TSeqCollection.h>

using namespace std;

void
FvtxGlobalAlign2Text(string input = "FvtxGlobalAlign.root", int sign = +1,
    bool fit_cor = false)
{
//////////////////////////////////////////////////////////
//   This file has been automatically generated 
//     (Fri May  4 15:19:52 2012 by ROOT version5.30/03)
//   from TTree misalignment/ misalignments parameters
//   found on file: FvtxGlobalAlign.root
//////////////////////////////////////////////////////////

  gSystem->Load("libfvtxoo_alignment");
  gSystem->Load("libfvtxgeom.so");

  TFvtxDatabaseCntrl::set_flag("deadmap_auto_load", false);
  TFvtxDatabaseCntrl::set_flag("geom_use_calibration_database", false);
  TFvtxDatabaseCntrl::set_filename("geom_root_file_path","./");
  FvtxGeom::create_arms();

//Reset ROOT and connect tree file
  gROOT->Reset();
  TFile *f = new TFile(input.c_str());
  TTree *misalignment = (TTree*) gDirectory->Get("misalignment");

//Declaration of leaves types
  Double_t delta_x;
  Double_t delta_y;
  Double_t delta_z;
  Double_t delta_w;
  Double_t delta_phi;
  Double_t delta_psix;
  Double_t delta_psiy;
  Double_t delta_w_millepede;
  Double_t delta_z_millepede;
  Double_t delta_phi_millepede;
  Double_t delta_psix_millepede;
  Double_t delta_psiy_millepede;
  Double_t delta_x_millepede;
  Double_t delta_y_millepede;
  Int_t arm;
  Int_t cage;
  Int_t station;
  Int_t sector;
  Int_t half;
  Double_t error_x;
  Double_t error_y;
  Double_t error_z;
  Double_t error_w;
  Double_t error_phi;
  Double_t error_psix;
  Double_t error_psiy;
  Int_t nb_tracks;
  Double_t angle;
  Int_t is_fvtx_wedge;

  // Set branch addresses.
  misalignment->SetBranchAddress("delta_x", &delta_x);
  misalignment->SetBranchAddress("delta_y", &delta_y);
  misalignment->SetBranchAddress("delta_z", &delta_z);
  misalignment->SetBranchAddress("delta_w", &delta_w);
  misalignment->SetBranchAddress("delta_phi", &delta_phi);
  misalignment->SetBranchAddress("delta_psix", &delta_psix);
  misalignment->SetBranchAddress("delta_psiy", &delta_psiy);
  misalignment->SetBranchAddress("delta_w_millepede", &delta_w_millepede);
  misalignment->SetBranchAddress("delta_z_millepede", &delta_z_millepede);
  misalignment->SetBranchAddress("delta_phi_millepede", &delta_phi_millepede);
  misalignment->SetBranchAddress("delta_psix_millepede", &delta_psix_millepede);
  misalignment->SetBranchAddress("delta_psiy_millepede", &delta_psiy_millepede);
  misalignment->SetBranchAddress("delta_x_millepede", &delta_x_millepede);
  misalignment->SetBranchAddress("delta_y_millepede", &delta_y_millepede);
  misalignment->SetBranchAddress("arm", &arm);
  misalignment->SetBranchAddress("cage", &cage);
  misalignment->SetBranchAddress("station", &station);
  misalignment->SetBranchAddress("sector", &sector);
  misalignment->SetBranchAddress("half", &half);
  misalignment->SetBranchAddress("error_x", &error_x);
  misalignment->SetBranchAddress("error_y", &error_y);
  misalignment->SetBranchAddress("error_z", &error_z);
  misalignment->SetBranchAddress("error_w", &error_w);
  misalignment->SetBranchAddress("error_phi", &error_phi);
  misalignment->SetBranchAddress("error_psix", &error_psix);
  misalignment->SetBranchAddress("error_psiy", &error_psiy);
  misalignment->SetBranchAddress("nb_tracks", &nb_tracks);
  misalignment->SetBranchAddress("angle", &angle);
  misalignment->SetBranchAddress("is_fvtx_wedge", &is_fvtx_wedge);

//     This is the loop skeleton
//       To read only selected branches, Insert statements like:
// misalignment->SetBranchStatus("*",0);  // disable all branches
// TTreePlayer->SetBranchStatus("branchname",1);  // activate branchname

  string sfit_cor = fit_cor ? ".fit_cor" : "";
  string outfile = input + Form(".Sign%d", sign) + sfit_cor
      + ".FvtxGlobalAlign2Text.txt";
  fstream out(outfile.c_str(), ios_base::out);

  Long64_t nentries = misalignment->GetEntries();

  Long64_t nbytes = 0;
  for (Long64_t i = 0; i < nentries; i++)
    {
      nbytes += misalignment->GetEntry(i);

      int gate = sign;

      if (sector < 0)
        { // station
          assert(half<0);
//          in  >> par._arm >> par._cage >> par._station
//              >> par._delta_x >> par._delta_y >>  par._delta_z
//              >> par._delta_phi >> par._delta_psix >> par._delta_psiy;

          out << "station" << "\t" //
              << arm << "\t" //
              << cage << "\t" //
              << station << "\t" //
              << delta_x + gate * delta_x_millepede << "\t" //
              << delta_y + gate * delta_y_millepede << "\t" //
              << delta_z + gate * delta_z_millepede << "\t" //
              << delta_phi + gate * delta_phi_millepede << "\t" //
              << delta_psix + gate * delta_psix_millepede << "\t" //
              << delta_psiy + gate * delta_psiy_millepede << "\t" //
              << endl;

        }
      else //  if (sector < 0)
        { // wedges

          if (half == 1)
            {

//              if (error_w<=0 || error_w>.01)
              if (fabs(delta_w_millepede) > 500e-4/*um e-4*/
              || fabs(error_w) > 100e-4/*um e-4*/
              )
                {
                  if (abs(error_w + 999.9)>1)
                    cout << "Ignore w = " << delta_w_millepede << " + "
                        << delta_w << " +- " << error_w << " cm " << "@ arm = "
                        << arm  << ", cage = " << cage << ", station = " << station
                        << ", sector = " << sector << endl;
                  gate = 0;
                }

              double fit_cor_delta_x = 0;
              double fit_cor_delta_y = 0;
              if (fit_cor
                  && (delta_x != 0 || delta_x_millepede != 0 || delta_y != 0
                      || delta_y_millepede != 0))
                {
//                    apply fit correction for w-z fit comparing to kalman fits

                  double angle_half[2] =
                    { 0 };
                  double delta_w_half[2] =
                    { 0 };

                  const double abs_w_shift_station[4] =
                    { (-2.46794 - 2.92604) / 2, (3.79959 + 3.66314) / 2,
                        (0.512049 + 1.13475) / 2, (-1.45964 - 2.16844) / 2 };

//                  const double w_shift_scale = 2.86567560392885623e-4;
                  const double w_shift_scale = 5.84122149135196e-4;

                  for (half = 0; half < 2; half++)
                    {
                      angle_half[half] = get_half_angle(arm, cage, station,
                          sector, half, 0);
                      delta_w_half[half] = (-1)
                          * get_w_sign(arm, cage, station, sector, half)
                          * abs_w_shift_station[station] * w_shift_scale;

                    }

                  // use average delta w between the two half sector instead !
                  fit_cor_delta_x = -(delta_w_half[0] * sin(angle_half[0])
                      + delta_w_half[1] * sin(angle_half[1])) / 2;
                  fit_cor_delta_y = (delta_w_half[0] * cos(angle_half[0])
                      + delta_w_half[1] * cos(angle_half[1])) / 2;

                }

//                gate = 0;

              out << "wedge" << "\t"
                  //
                  << arm << "\t"
                  //
                  << cage << "\t"
                  //
                  << station << "\t"
                  //
                  << sector << "\t"
                  //
                  << delta_x + gate * (delta_x_millepede + fit_cor_delta_x)
                  << "\t"
                  //
                  << delta_y + gate * (delta_y_millepede + fit_cor_delta_y)
                  << "\t" //
                  << delta_z + gate * delta_z_millepede << "\t" //
                  << delta_phi + gate * delta_phi_millepede << "\t" //
                  << delta_psix + gate * delta_psix_millepede << "\t" //
                  << delta_psiy + gate * delta_psiy_millepede << "\t" //
                  << endl;
            }
        }

    }

  cout << "Done (" << nbytes << " read)." << endl;
  cout << "Saved to " << outfile << endl;
}

double
get_half_angle(int arm, int cage, int station, int sector, int half,
    int strip) const
{
  double angle = 0;
  FvtxStrip *a_strip = 0;
  a_strip =
      FvtxGeom::get_arm(arm)->get_cage(cage)->get_station(station)->get_sector(
          sector)->get_column(half)->get_strip(strip);
  if (!a_strip)
    cout << "FvtxGlobalAlign::get_half_angle() " << " arm " << arm << " cage "
        << cage << " station " << station << " sector " << sector << " column "
        << half << " strip " << strip << " not found " << endl;
  else
    {

      const double M_PI = 3.14159265358979323846; /* pi */
      const double M_PI_2 = 1.57079632679489661923; /* pi/2 */

      angle = a_strip->get_angle();
      angle = (angle < -M_PI_2) ? angle + M_PI : angle;
      angle = (angle > M_PI_2) ? angle - M_PI : angle;
    }

  return angle;
}

int
get_w_sign(int arm, int cage, int station, int sector, int half) const
{
  int sign = 0;
  FvtxStrip *a_strip = 0;
  a_strip =
      FvtxGeom::get_arm(arm)->get_cage(cage)->get_station(station)->get_sector(
          sector)->get_column(half)->get_strip(0);
  if (!a_strip)
    cout << "FvtxGlobalAlign::get_w_sign() " << " arm " << arm << " cage "
        << cage << " station " << station << " sector " << sector << " column "
        << half << " strip " << 0 << " not found " << endl;
  else
    {
      sign = a_strip->get_w_sign();
    }
  return sign;
}
