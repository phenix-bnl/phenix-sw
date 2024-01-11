// $Id: GetBeamPos_Sum.C,v 1.1 2013/10/16 22:34:22 jinhuang Exp $                                                                                             

/*!
 * \file GetBeamPos_Sum.C
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.1 $
 * \date $Date: 2013/10/16 22:34:22 $
 */
#include <cassert>
#include <fstream>
#include "SaveCanvas.C"
#include "SetOKStyle.C"
using namespace std;

void
GetBeamPos_Sum(
    TString infile =
        "/direct/phenix+subsys+fvtx/jinhuang/GoldenDimuon/AlignDST_GD1/GoldenDimuon")
{
//  pair<double, double> x ;
//  pair<double, double> y ;
  SetOKStyle();

  double id[10000] =
    { 0 };
  double run[10000] =
    { 0 };

  double x[10000] =
    { 0 };
  double ex[10000] =
    { 0 };
  double y[10000] =
    { 0 };
  double ey[10000] =
    { 0 };

  int n = 0;

  int min_run = 1e7;
  int max_run = 0;

  // read in
  fstream flog(infile + ".GetBeamPos", ios_base::in);
  string line;
  while (getline(flog, line))
    {

      stringstream sline(line);

//      cout << line <<endl;

      sline >> run[n] >> x[n] >> y[n] >> ex[n] >> ey[n];

      min_run = min_run > run[n] ? run[n] : min_run;
      max_run = max_run > run[n] ? max_run : run[n];

//        cout << n << "  -  " << run[n] << "\t" << x[n] << "\t" << y[n] << "\t"
//            << ex[n] << "\t" << ey[n]<<" - "
//            <<(fabs(x[n]) > .1) << endl;

      id[n] = n;

      if (ex[n] < .5 && ey[n] < .5 && ex[n] > 0.00001 && ey[n] > .00001)
        n++;

    }
  flog.close();

  TGraphErrors * gex = new TGraphErrors(n, run, x, 0, ex);
  gex->SetName("gex");
  TGraphErrors * gey = new TGraphErrors(n, run, y, 0, ey);
  gey->SetName("gey");

  TGraph * grun = new TGraph(n, id, run);
  grun->SetName("grun");

  // interpolate

  const int min_run_src = min_run;

  n = 0;
  fstream flog(infile + ".ExportRunList", ios_base::in);
  while (getline(flog, line))
    {

      stringstream sline(line);
      sline >> run[n];

      if (n == 0)
        {
          cout << run[n] << " : " << line << endl;
        }

      min_run = min_run > run[n] ? run[n] : min_run;
      max_run = max_run > run[n] ? max_run : run[n];

      if (run[n] < min_run_src)
        {

          TF1 f2("fit2", "pol0", min_run_src, min_run_src + 100);

          gex->Fit(&f2, "MRQ");

          x[n] = f2.Eval(run[n]);
          ex[n] = 0;

          gey->Fit(&f2, "MRQ");
          y[n] = f2.Eval(run[n]);
          ey[n] = 0;

        }
      else
        {

          TF1 f("fit", "pol2", run[n] - 100, run[n] + 100);

          TFitResultPtr ptr = gex->Fit(&f, "MRQS");
          TFitResult* fit_res = ptr.Get();

          if (fit_res)
            {

              x[n] = f.Eval(run[n]);
              ex[n] = 0;

              gey->Fit(&f, "MRQ");
              y[n] = f.Eval(run[n]);
              ey[n] = 0;

            }
          else
            {

              cout << "Expand range for run " << run[n] << endl;

              TF1 f2("fit2", "pol0", run[n] - 1000, run[n] + 1000);

              gex->Fit(&f2, "MRQ");

              x[n] = f2.Eval(run[n]);
              ex[n] = 0;

              gey->Fit(&f2, "MRQ");
              y[n] = f2.Eval(run[n]);
              ey[n] = 0;

            }
        }
      n++;

    }
  flog.close();

  TGraphErrors * gx = new TGraphErrors(n, run, x);
  TGraphErrors * gy = new TGraphErrors(n, run, y);

  gex->SetLineColor(kBlue);
  gey->SetLineColor(kBlue);
  gx->SetLineColor(kRed);
  gy->SetLineColor(kRed);
  gx->SetLineWidth(2);
  gy->SetLineWidth(2);

  cout << " n = " << n << ", run[0] = " << run[0] << endl;
//  gx -> Print();

// save

  cout << "Save to " << infile + ".Smooth.GetBeamPos" << endl;
  fstream flog(infile + ".Smooth.GetBeamPos", ios_base::out);
  for (int i = 0; i < n; i++)
    {

      flog << (gx->GetX())[i] << "\t" << (gx->GetY())[i] << "\t"
          << (gy->GetY())[i] << "\t" << 0 << "\t" << 0 << endl;

    }
  flog.close();

  ///direct/phenix+subsys+fvtx/jinhuang/GoldenDimuon/AlignDST_GD_DST/result/ALL.AlignDST.root
//  Average VTX shift VS FVTX = (-0.222241 0.099704) cm
  const double VTX_Shift_X = -0.222241;
  const double VTX_Shift_Y = 0.099704;

  cout << "Save to " << infile + ".Smooth_Shifted.GetBeamPos" << endl;
  fstream flog(infile + ".Smooth_Shifted.GetBeamPos", ios_base::out);
  for (int i = 0; i < n; i++)
    {

      flog << (gx->GetX())[i] << "\t" << (gx->GetY())[i] - VTX_Shift_X << "\t"
          << (gy->GetY())[i] - VTX_Shift_Y << "\t" << 0 << "\t" << 0 << endl;

    }
  flog.close();

  TCanvas *c1 = new TCanvas("GetBeamPos_Sum", "GetBeamPos_Sum", 1800, 900 * .8);
  c1->Divide(3, 1);
  int idx = 1;

  c1->cd(idx++);
  c1->Update();
  gPad->DrawFrame(min_run - 1, -.3, max_run + 1, .3,
      "Beam X VS run;Run;VTX Beam X (cm)");
  gex->Draw("*");
  gx->Draw("l");

  c1->cd(idx++);
  c1->Update();
  gPad->DrawFrame(min_run - 1, -.3, max_run + 1, .3,
      "Beam Y VS run;Run;VTX Beam Y (cm)");
  gey->Draw("*");
  gy->Draw("l");

  c1->cd(idx++);
  c1->Update();
  grun->Draw("a*l");

  SaveCanvas(c1, TString(infile) + TString("_") + TString(c1->GetName()),
      kFALSE);
}

