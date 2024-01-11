#include "gsl/gsl_const.h"
#include "CalibratorFitPar.hh"
#include "TMath.h"

// light velocity [cm/ns]
static const float C = GSL_CONST_CGS_SPEED_OF_LIGHT / 1e9;

double fitfunc (double *x, double *par);

double
fitfunc (double *x, double *par)
{
  double fitval = par[0] + par[1] / TMath::Sqrt (x[0]);
  return fitval;
}

void
CalibratorFitPar::Initialize (const BbcTime_t & time, char *CalibType)
{
  char ObjectName0[10];
  char HistName0[72];
  char ObjectName1[10];
  char HistName1[72];

  HistInit =
    new TH2F ("HistInit", "Before Slewing Correction", 128, 0., 128., 1000,
	      -5., 5.);
  HistLast =
    new TH2F ("HistLast", "Before Slewing Correction", 128, 0., 128., 400,
	      -2., 2.);

  for (int ipmt = 0; ipmt < BBC_N_PMT; ipmt++)
    {
      sprintf (ObjectName0, "Pmt%3.3d", ipmt);
      sprintf (HistName0, "Calibration histgram for Pmt%3.3d", ipmt);
      sprintf (ObjectName1, "SlewPmt%3.3d", ipmt);
      sprintf (HistName1, "Calibration histgram for Pmt%3.3d", ipmt);
      Hist[ipmt] =
	new TProfile (ObjectName0, HistName0, 1024, 0.0, 4096.0, -5.0, 5.0);
      Hist2D[ipmt] =
	new TH2F (ObjectName1, HistName1, 1024, 0.0, 4096.0, 400, -5.0, 5.0);
    }
}

void
CalibratorFitPar::FillHist (BbcEvent * bbc, BbcEvent * ref, char *type)
{
  // square of distance each PMT from the beam axis [cm*cm]
  static const float R2[32] = {
    183.552402, 159.355605, 195.629199, 147.235605,
    114.973203, 183.514004, 127.054795, 86.726797,
    62.530000, 187.534805, 123.010000, 74.616401,
    207.691602, 135.101201, 78.641997, 163.328398,
    98.803594, 207.691602, 135.101201, 78.641997,
    187.534805, 123.010000, 74.616401, 183.514004,
    127.054795, 86.726797, 62.530000, 195.629199,
    147.235605, 114.973203, 183.552402, 159.355605
  };
  // distance of each arm from nominal vertex position [cm]
  static const float L = 144.350;

  float Z = bbc->getZVertex ();

  for (int ipmt = 0; ipmt < BBC_N_PMT; ipmt++)
    {

      if (ipmt >= 0 && ipmt < 64)
	{
	  float Distance = L - Z;
	  float TransitTime =
	    (sqrt (Distance * Distance + R2[ipmt % 32]) - Distance) / C;
	  float delta =
	    ref->getArmHitTime (Bbc::South) - (bbc->getHitTime0 (ipmt) -
					       TransitTime);
	  if (bbc->getTrueAdc (ipmt) < 3700)
	    {
	      Hist[ipmt]->Fill (bbc->getTrueAdc (ipmt), delta);
	      Hist2D[ipmt]->Fill (bbc->getTrueAdc (ipmt), delta);
	    }
	}
      else if (ipmt > 63 && ipmt < BBC_N_PMT)
	{
	  float Distance = L + Z;
	  float TransitTime =
	    (sqrt (Distance * Distance + R2[ipmt % 32]) - Distance) / C;
	  float delta =
	    ref->getArmHitTime (Bbc::North) - (bbc->getHitTime0 (ipmt) -
					       TransitTime);
	  if (bbc->getTrueAdc (ipmt) < 3700)
	    {
	      Hist[ipmt]->Fill (bbc->getTrueAdc (ipmt), delta);
	      Hist2D[ipmt]->Fill (bbc->getTrueAdc (ipmt), delta);
	    }
	}
    }
}

void
CalibratorFitPar::Calculate (BbcEvent * bbcevent, BbcEvent * bbcref)
{
  double Par[2];
  double chi2 = 0.0;
  int i, ndof = 0;

  for (i = 0; i < 128; i++)
    {

      Par[0] = 0.0;
      Par[1] = 0.0;

      TF1 *SlewFunc = new TF1 ("SlewFunc", fitfunc, 100., 2800., 2);
      SlewFunc->SetParameter (0, Par[0]);
      SlewFunc->SetParameter (1, Par[1]);

      Hist[i]->Fit ("SlewFunc", "RQ");

      TF1 *fit = Hist[i]->GetFunction ("SlewFunc");

      Par[0] = fit->GetParameter (0);
      Par[1] = fit->GetParameter (1);
      chi2 = fit->GetChisquare ();
      ndof = fit->GetNDF ();

      float TmpPar0 =
	bbcevent->getCalib ()->getSlewing0 ()->getCalibPar (i)->getPar0 ();
      float TmpPar1 =
	bbcevent->getCalib ()->getSlewing0 ()->getCalibPar (i)->getPar1 ();

      float Slowpar0 = TmpPar0 + Par[0];
      float Slowpar1 = TmpPar1 + Par[1];

      bbcevent->getCalib ()->getSlewing0 ()->getCalibPar (i)->
	setPar0 (Slowpar0);
      bbcevent->getCalib ()->getSlewing0 ()->getCalibPar (i)->
	setPar1 (Slowpar1);

      bbcref->getCalib ()->getSlewing0 ()->getCalibPar (i)->
	setPar0 (Slowpar0);
      bbcref->getCalib ()->getSlewing0 ()->getCalibPar (i)->
	setPar1 (Slowpar1);

      float chisquare = chi2 / (float) ndof;

      printf ("%9.4f %9.4f 0.0 0.0 0.0 %9.4f 0\n", Slowpar0, Slowpar1,
	      chisquare);

      (FitPar.getCalibPar (i))->setPar0 (Slowpar0);
      (FitPar.getCalibPar (i))->setPar1 (Slowpar1);
      (FitPar.getCalibPar (i))->setPar2 (0.0);
      (FitPar.getCalibPar (i))->setPar3 (0.0);
      (FitPar.getCalibPar (i))->setPar4 (0.0);
      (FitPar.getCalibPar (i))->setChi2 (chisquare);

      delete SlewFunc;
      delete fit;

    }
}

void
CalibratorFitPar::Evaluate ()
{
}

void
CalibratorFitPar::StoreToDB (const BbcTime_t & time, char *CalibType)
{
  FitPar.store (time, CalibType);
}

void
CalibratorFitPar::ResetHist ()
{
  for (int i = 0; i < 128; i++)
    {
      Hist[i]->Reset ();
      Hist2D[i]->Reset ();
    }
}

void
CalibratorFitPar::FillInit (BbcEvent * bbc)
{
  // square of distance each PMT from the beam axis [cm*cm]
  static const float R2[32] = {
    183.552402, 159.355605, 195.629199, 147.235605,
    114.973203, 183.514004, 127.054795, 86.726797,
    62.530000, 187.534805, 123.010000, 74.616401,
    207.691602, 135.101201, 78.641997, 163.328398,
    98.803594, 207.691602, 135.101201, 78.641997,
    187.534805, 123.010000, 74.616401, 183.514004,
    127.054795, 86.726797, 62.530000, 195.629199,
    147.235605, 114.973203, 183.552402, 159.355605
  };
  // distance of each arm from nominal vertex position [cm]
  static const float L = 144.350;

  float Z = bbc->getZVertex ();

  for (int ipmt = 0; ipmt < BBC_N_PMT; ipmt++)
    {

      if (ipmt >= 0 && ipmt < 64)
	{
	  float Distance = L - Z;
	  float TransitTime =
	    (sqrt (Distance * Distance + R2[ipmt % 32]) - Distance) / C;
	  float delta =
	    bbc->getArmHitTime (Bbc::South) - (bbc->getHitTime0 (ipmt) -
					       TransitTime);
	  if (bbc->getTrueAdc (ipmt) < 3700)
	    {
	      HistInit->Fill ((float) ipmt, delta);
	    }
	}
      else if (ipmt > 63 && ipmt < BBC_N_PMT)
	{
	  float Distance = L + Z;
	  float TransitTime =
	    (sqrt (Distance * Distance + R2[ipmt % 32]) - Distance) / C;
	  float delta =
	    bbc->getArmHitTime (Bbc::North) - (bbc->getHitTime0 (ipmt) -
					       TransitTime);
	  if (bbc->getTrueAdc (ipmt) < 3700)
	    {
	      HistInit->Fill ((float) ipmt, delta);
	    }
	}
    }
}

void
CalibratorFitPar::FillLast (BbcEvent * bbc)
{
  // square of distance each PMT from the beam axis [cm*cm]
  static const float R2[32] = {
    183.552402, 159.355605, 195.629199, 147.235605,
    114.973203, 183.514004, 127.054795, 86.726797,
    62.530000, 187.534805, 123.010000, 74.616401,
    207.691602, 135.101201, 78.641997, 163.328398,
    98.803594, 207.691602, 135.101201, 78.641997,
    187.534805, 123.010000, 74.616401, 183.514004,
    127.054795, 86.726797, 62.530000, 195.629199,
    147.235605, 114.973203, 183.552402, 159.355605
  };
  // distance of each arm from nominal vertex position [cm]
  static const float L = 144.350;

  float Z = bbc->getZVertex ();

  for (int ipmt = 0; ipmt < BBC_N_PMT; ipmt++)
    {
      if (ipmt >= 0 && ipmt < 64)
	{
	  float Distance = L - Z;
	  float TransitTime =
	    (sqrt (Distance * Distance + R2[ipmt % 32]) - Distance) / C;
	  float delta =
	    bbc->getArmHitTime (Bbc::South) - (bbc->getHitTime0 (ipmt) -
					       TransitTime);
	  HistLast->Fill ((float) ipmt, delta);
	}
      else if (ipmt > 63 && ipmt < BBC_N_PMT)
	{
	  float Distance = L + Z;
	  float TransitTime =
	    (sqrt (Distance * Distance + R2[ipmt % 32]) - Distance) / C;
	  float delta =
	    bbc->getArmHitTime (Bbc::North) - (bbc->getHitTime0 (ipmt) -
					       TransitTime);
	  HistLast->Fill ((float) ipmt, delta);
	}
    }
}
