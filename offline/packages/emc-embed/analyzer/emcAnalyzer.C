// $Header
//-----------------------------------------------------------------------------
//
//  (c) PHENIX Collaboration 2002-2003
//
//  Author: D. d'Enterria
//
//  Nevis Labs. Columbia University
//
//-----------------------------------------------------------------------------

#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdio>
#include <string>
#include <cstdlib>
#include <cmath>

#include "TROOT.h"
#include "TDirectory.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TCanvas.h"
#include "TGaxis.h"
#include "TString.h"
#include "TLegend.h"
#include "TLegendEntry.h"
#include "TStyle.h"
#include "TPaveText.h"
#include "TKey.h"
#include "TLine.h"
#include "TLegend.h"
#include "TLatex.h"
#include "TF1.h"
#include "TGraphErrors.h"
#include "TGraphAsymmErrors.h"
#include "TMath.h"
#include "TClonesArray.h"
#include "TPaveStats.h"
#include "TMarker.h"
#include "TArrow.h"
#include "TColor.h"

#include "emcAnalyzerUtils.h"
#include "emcAnalyzerCorrFactors.h"
#include "emcAnalyzer.h"
#include "emcReadOtherRAA.h"

using namespace std;
using namespace TMath;
using namespace emcAnalyzerUtils;
using namespace emcAnalyzerCorrFactors;

const int CentClasses = 26;

const int maxNpTbins = 50; // pT(max) = 25 GeV/c
const double twoPi = 2.*Pi();

const double sigma_NN = 42.2;   // mb

const double m_pi0 = 0.134977;
const double m_pi  = 0.13957018;
const double m_k0  = 0.49765;
const double m_eta = 0.54775;
const double m_p   = 0.938272;

// HOWTO:
// gSystem->Load("libemcAnalyzer"); emcAnalyzer e; TGraphErrors *eband;


//_____________________________________________________________________________
emcAnalyzer::emcAnalyzer()
{

  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);
  gStyle->SetOptStat(0);
  gStyle->SetOptFit(1111);
  
  fEfficParticle = new TString("pi0");
  
  fPlot = true;
  fLogy = false;
  fFit = true;
  fConstrainedHagFit = true;
  
  fFitFunc = "expo";
  fXaxis = "pT";
  
  fVerbose = 1;
  fSaveGifs = false;
  fSaveEps = false;
  fdumpLaTeX = false;
  fdumpAscii = false;
  
  fDataTablesPath = new TString("/home/enterria/offline/packages/emc-embed/analyzer/data");
  //fDataTablesPath = new TString("/home/enterria/wrk/CORRECTED_RUN2_FINAL/tables_new");
  
  fCanvasList = new TList;
  fLegend1 = 0;
  fLegend2 = 0;
  fPlotTitle = 0;
  
  print();
}

//_____________________________________________________________________________
emcAnalyzer::~emcAnalyzer()
{
  delete fCanvasList;
  delete fPlotTitle;
  delete fLegend1;
  delete fLegend2;
  delete fDataTablesPath;
  delete fEfficParticle;
}

//_____________________________________________________________________________
void
emcAnalyzer::print()
{

  cout << " <I> fEfficParticle: " << fEfficParticle->Data() << endl;

  cout << " <I> fPlot: " << fPlot << endl;
  cout << " <I> fFit: " << fFit << endl;
  cout << " <I> fConstrainedHagFit: " << fConstrainedHagFit << endl;
  cout << " <I> fLogy: " << fLogy << endl;
  cout << " <I> fXaxis: " << fXaxis << endl;
  cout << " <I> fVerbose: " << fVerbose << endl;
  cout << " <I> fSaveGifs: " << fSaveGifs << endl;
  cout << " <I> fSaveEps: " << fSaveEps << endl;
  cout << " <I> fdumpLaTeX: " << fdumpLaTeX << endl;
  cout << " <I> fdumpAscii: " << fdumpAscii << endl;

  cout << " <I> fDataTablesPath: ";
  if (fDataTablesPath) cout << fDataTablesPath->Data() << endl;
  else cout << "none" << endl;

  //   cout << " <I> fCanvasList: ";
  //   if (fCanvasList) fCanvasList->Print();
  //   else cout << "empty" << endl;
  //   cout << endl;

  //   cout << " <I> fPlotTitle: ";
  //   if (fPlotTitle) cout << fPlotTitle->Data() << endl;
  //   else cout << "none" << endl;
  //   cout << " <I> fLegend1: ";
  //   if (fLegend1) cout << fLegend1->Data() << endl;
  //   else cout << "none" << endl;
  //   cout << " <I> fLegend2: ";
  //   if (fLegend2) cout << fLegend2->Data() << endl;
  //   else cout << "none" << endl;

}

//_____________________________________________________________________________
void
emcAnalyzer::setTitle(const char* title)
{
  delete fPlotTitle;
  fPlotTitle = new TString(title);
}

//_____________________________________________________________________________
void
emcAnalyzer::setLegends(const char* legend1, const char* legend2)
{
  delete fLegend2;
  delete fLegend1;

  fLegend1 = new TString(legend1);
  fLegend2 = new TString(legend2);
}

//_____________________________________________________________________________
void
emcAnalyzer::setDataTablesPath(const char* datapath)
{
  delete fDataTablesPath;
  fDataTablesPath = new TString(datapath);
}

//_____________________________________________________________________________
void
emcAnalyzer::setEfficParticle(const char* partic)
{
  delete fEfficParticle;
  fEfficParticle = new TString(partic);
}

//_____________________________________________________________________________
// OPTIONS:
//          type = "raw"    = raw spectrum (normalized by Nevt)
//                 "rawraw" = raw spectrum (straight from raw file)
//                 "acc"    = acceptance-corrected spectrum
//                 "acceff" = acceptance & efficiency -corrected spectrum
//                 "full"   = fully corrected spectrum
//
// available cuts: add "trig" for high pT triggered data (Justin files already normalized by Nevts)
//
//          cut = "noPID"
//          cut = "chisq1"
//          cut = "chisq2"
//          cut = "tof1chisq1" == favorite cut (PPG014)
//          cut = "tof1chisq2"
//          cut = "tof1"
//          cut = "tof2chisq1"
//          cut = "tof2chisq2" == favorite cut (Dec.03 pi0 repass)
//          cut = "tof2"
//
//          cut = "r012" (reaction plane 0-2: in-plane)
//          cut = "r345" (reaction plane 3-5: in-between)
//          cut = "r678" (reaction plane 6-8: out-of-plane)
//
//          cut = "noPID_auau62" 
//          cut = "chisq1_auau62"
//
//          binshift == 0   : No correction for the true mean value of the $p_T$ bin applied
//          binshift == 0.5 : bin-shift correction in yield with forced binwidth = 0.5 GeV/c
//          binshift == 1   : default: bin-shift correction in yield
//          binshift == 2   : bin-shift correction in pT
//
// *********** Example from root:
// root> for (int i=0;i<100;i+=10){ int j=i+10; e.plot_spectrum(i,j); }; > pbsc_latex_tablesout.txt
// ***********
//

TGraphErrors*
emcAnalyzer::plot_spectrum( const int Cent1,
                            const int Cent2,
                            const char *cut,
                            const char *type,
                            const float binshift )
{

  TGraphErrors *Spectrum = 0;

  int CC = getCentralityClass(Cent1, Cent2);
  if ( CC == -1) return Spectrum;

  TString cut_str = cut;

  double eventsCC = 0.;
  double eventsCC_err = 0.;

  double pTbinwidth = 0.5; // GeV/c

  char name[300];
  char ffile[300];

  if (fEfficParticle->Contains("pi0"))
    {

      if ( cut_str.Contains("auau62", TString::kIgnoreCase) ) // Run-4 raw yields
        {
	  sprintf(name, "yields_pbsc_%d_%d_%s", Cent1, Cent2, cut);
          sprintf(ffile, "%s/pi0_pbsc_yields_auau62GeV/%s.txt", fDataTablesPath->Data(), name);

          eventsCC = getEvents62GeV(Cent1, Cent2, "value");
          eventsCC_err = getEvents62GeV(Cent1, Cent2, "error");
        }
      else
	{
	  sprintf(name, "yields_%d_%d_%s", Cent1, Cent2, cut);
	  sprintf(ffile, "%s/pi0_pbsc_yields/%s.txt", fDataTablesPath->Data(), name);

          eventsCC = getEvents(Cent1, Cent2, "value");
          eventsCC_err = getEvents(Cent1, Cent2, "error");

	  if (cut_str.Contains("r012") || cut_str.Contains("r345") || cut_str.Contains("r678"))
	    {
	      eventsCC = getEventsReacPlane(Cent1, Cent2, "value")/3.;
	      eventsCC_err = getEventsReacPlane(Cent1, Cent2, "error")/3.;
	    }

	}

      pTbinwidth = 0.5; // GeV/c
    }
  else
    {
      sprintf(name, "eta_yields_%d_%d_%s", Cent1, Cent2, cut);
      sprintf(ffile, "%s/eta_yields/%s.txt", fDataTablesPath->Data(), name);
      eventsCC = getEtaEvents(Cent1, Cent2, "value");
      eventsCC_err = getEtaEvents(Cent1, Cent2, "error");

      pTbinwidth = 1.0; // GeV/c // eta raw-yields already normalized by bin-width
    }

  cout << " <I> Constructing " << type << "-corrected "
       << cut << " spectrum for " << fEfficParticle->Data() << " and centrality class: "
       << CC << " (" << Cent1 << " - " << Cent2 << "%)"
       << " from raw yields file: " << name << ".txt" << endl;

  if (!binshift) // no bin-shift correction
    {
      cout << " <I> No correction for the true mean value of the $p_T$ bin applied to the steeply falling spectrum" << endl;
    }
  else if (binshift == 0.5) // bin-shift correction in yield with forced binwidth = 0.5 GeV/c
    {
      cout << " <I> Spectrum shifted along the y-axis (Yield) to correct for true mean value of the $p_T$ bin "
	   << "in the steeply falling spectrum. FORCED BINWIDTH = 0.5 GeV/c !" << endl;
    }
  else if (binshift == 1) // default: bin-shift correction in yield
    {
      cout << " <I> Spectrum shifted along the y-axis (Yield) to correct for true mean value of the $p_T$ bin "
	   << "in the steeply falling spectrum" << endl;
    }
  else if (binshift == 2) // bin-shift correction in pT
    {
      cout << " <I> Spectrum shifted (after doing and undoing the 1/pT normalization) along the x-axis (pT) "
	   << "to correct for the true mean value of the $p_T$ bin in the steeply falling spectrum" << endl;
    }
  else
    {
      cout << " <E> Don't know this bin-shift option: \'" << binshift << "\'" << endl;
      return Spectrum;
    }

  cout << " <I> Total # of events in centrality class: " << eventsCC
       << " +/- " << eventsCC_err << endl;

  char title[300];
  sprintf(title, "Centrality Class %d_%d", Cent1, Cent2);
  //if (Cent1==100) sprintf(title,"Centrality Class: min. bias");
  char ytitle[300];

  //_____________________________________________________________________________

  int cols = 3;
  int NpTbins = 0;
  int skiplines = 0; // 4;

  double **vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);

  if (!vals || !NpTbins)
    {
      if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
      return Spectrum;
    }

  double *pT = new double[NpTbins];
  double *epT = new double[NpTbins];
  double *counts = new double[NpTbins];
  double *e_stat = new double[NpTbins];
  double *e_extra1 = new double[NpTbins];
  double *e_extra2 = new double[NpTbins];

  pT = vals[0];
  counts = vals[1];
  e_stat = vals[2];

  // extraction errors for eta's read from file
  if ( fEfficParticle->Contains("eta") )
    {
      e_extra1 = vals[3]; // relative error point-to-point (type-A) systematics
      e_extra2 = vals[4]; // relative error pT-correlated (type-B) systematics
    }

  double yield[NpTbins];
  double e_tot[NpTbins];
  double erel_yield[NpTbins];

  double *e_syst = new double[NpTbins];  // total systematic
  double *e_CC = new double[NpTbins];    // Centrality-cancelling
  double *e_nonCC = new double[NpTbins]; // non Centrality-cancelling
  double *e_pTC = new double[NpTbins];   // pT-correlated error (aka. "type-B" error)

  double full_cross_sect_corr = 1.0; // min.bias. trigger efficiency = 0.92 (but "missed" is *just* in the 92-100% centrality)

  cout << "  pT   pT_err   Ntot   Ntot_err   (Rel_toterr %) ";
  cout << "  Stat_err   Syst_err   CCuncorr_toterr   pTcorr_toterr" << endl;

  int i;

  for (i = 0; i < NpTbins ; i++)
    {

      epT[i] = 0.05 ; // const pT bin error = 50 MeV/c

      // (fractional) extraction error added to (fractional) stat. error for eta's
      if ( fEfficParticle->Contains("eta") )
	{
	  e_stat[i] = counts[i]*quadRelatError( e_stat[i], 1. , e_extra1[i], 1.); // both are fractional errors
	}

      if (fVerbose || !binshift) cout << "  " << pT[i] << "  " << epT[i] ;
      if (fVerbose > 1) cout << " [" << counts[i] << " +/- " << e_stat[i] << "]" ;

      // Pure raw 

      if (strcasecmp(type, "rawraw") == 0)
	{
	  yield[i] = counts[i];
	  e_tot[i] = e_stat[i];

          sprintf(name, "yield_raw_cent%d_%d_%s", Cent1, Cent2, cut);
          sprintf(ytitle, "RAW dN/dp_{T}");
	  continue;
	}

      // Normalized raw yield

      yield[i] = counts[i]/(eventsCC * pTbinwidth);
      erel_yield[i] = quadRelatError( e_stat[i], counts[i],
                                      eventsCC_err, eventsCC); // FIXME: should we count the stat. error on the # evts. here

      //e_tot[i] = yield[i]*quadRelatError( e_stat[i], counts[i],
      //				  eventsCC_err, eventsCC );

      if (fVerbose > 1) cout << " (numEvts*binwidth) =" << (eventsCC*pTbinwidth) ;

      //double erel_stat = e_tot[i]/yield[i]; //e_stat[i]/counts[i];

      if (strcasecmp(type, "raw") == 0)  // Raw spectrum (normalized)
        {
          if (fVerbose > 1) cout << " --> " << yield[i] << " +/- " << yield[i]*erel_yield[i] << endl;
          sprintf(name, "yield_norm_raw_cent%d_%d_%s", Cent1, Cent2, cut);
          sprintf(ytitle, "NORMALIZED RAW dN/dp_{T}");
        }
      else  // Acceptance-corrected spectrum
        {
          double acc = acceptance(pT[i], "value", fEfficParticle->Data());
          double eacc = acceptance(pT[i], "error", fEfficParticle->Data());

          if (fVerbose > 1) cout << " Acc:" << acc << " +/- " << eacc;

          yield[i] /= (acc * twoPi);
          erel_yield[i] = quadRelatError(erel_yield[i], 1., eacc, acc);
          //e_tot[i] = yield[i]*quadRelatError(e_tot[i], yield[i], eacc, acc);

          if (strcasecmp(type, "acc") == 0) // acc
            {
              cout << " --> " << yield[i] << " +/- " << yield[i]*erel_yield[i] << endl;
              sprintf(name, "yield_acc_corr_cent%d_%d_%s", Cent1, Cent2, cut);
              sprintf(ytitle, "ACCEPTANCE CORRECTED dN/dp_{T}");
            }
          else // [eff.+acc]
            {
              double eff = efficiency(pT[i], CC, cut, "value", fEfficParticle->Data());
              double eeff = efficiency(pT[i], CC, cut, "error", fEfficParticle->Data());

              if (fVerbose > 1) cout << ", Eff:" << eff << ", Acc*Eff: x" << 1/(acc*eff) << "]";

              // Acceptance+Efficiency-corrected raw spectrum

              yield[i] /= eff;
	      erel_yield[i] = quadRelatError(erel_yield[i], 1., eeff, eff);
	      //e_tot[i] = yield[i]*quadRelatError( e_tot[i], yield[i], eeff, eff );

	      // (fractional) extraction error added 
	      if ( fEfficParticle->Contains("eta") )
		{
		  erel_yield[i] = quadRelatError(erel_yield[i], 1., e_extra2[i], 1.);
		  //e_tot[i] = quadRelatError( e_tot[i], yield[i] , e_extra2[i], 1.);
		}

              if (strcasecmp(type, "full") == 0) // full
                {

                  double offvtx = offVertex("value");
                  double eoffvtx = offVertex("error");

                  // Fully-corrected spectrum
                  // No pseudo-rapidity --> rapidity transformation: < 1.% (0.5%) at pT = 1.0 GeV/c for eta=0.5(0.0)
                  // dy/deta = p/E

                  yield[i] *= offvtx/(pT[i] * full_cross_sect_corr);

                  erel_yield[i] = quadRelatError( erel_yield[i], 1.,
                                                  eoffvtx, offvtx); // eoffvtx = 0 now
                  //epT[i],pT[i]); // this "error" included in the energy scale error

                  //e_tot[i] = quadRelatError( e_tot[i], yield[i],
		  //			     eoffvtx, offvtx); // eoffvtx = 0 now

                  sprintf(name, "fully_corr_cent%d_%d_%s", Cent1, Cent2, cut);
                  //sprintf(ytitle,"1/[2#pi p_{T}N_{evt} ] d^{2}N^{#pi^{0}}/dp_{T}dy (GeV/#font[72]{c})^{-2}");
                  sprintf(ytitle, "1/[2#pi p_{T}] d^{2}N/dp_{ T}dy (GeV/#font[72]{c})^{-2}");

                }
              else if (strcasecmp(type, "acceff") == 0) // acceff
                {

                  if (fVerbose > 1) cout << " --> " << yield[i] << " +/- " << yield[i]*erel_yield[i] << endl;
                  sprintf(name, "yield_raw_acceff_corr_cent%d_%d_%s", Cent1, Cent2, cut);
                  sprintf(ytitle, "ACC.+ EFF. CORRECTED dN/dp_{T}");
                }
              else
                {
                  cerr << "emcAnalyzer::plot_spectrum : unknown type " << type << endl;
                  return 0;
                }

            } // acc-eff-corr & full-corr

        } // acc-corr

      e_tot[i] = yield[i] * erel_yield[i];
      e_stat[i] = (e_stat[i]/counts[i]) * yield[i];
      e_syst[i] = Sqrt(e_tot[i]*e_tot[i] - e_stat[i]*e_stat[i]);

      e_CC[i] = getCentCorrelatRelError(pT[i])*yield[i];
      e_nonCC[i] = Sqrt(e_tot[i]*e_tot[i] - e_CC[i]*e_CC[i]);

      // extraction error added to pT-correlated error (aka. "type-B") for eta's
      if ( fEfficParticle->Contains("eta") )
	{
	  e_pTC[i] = yield[i]*quadRelatError( e_extra2[i], 1. , getpTCorrelatRelError(pT[i]), 1.);
	}
      else
	{
	  e_pTC[i] = yield[i]*getpTCorrelatRelError(pT[i]);
	}

      //double test = quadRelatError(erel_stat,1.,eacc,acc,eeff,eff)/(e_tot[i]/yield[i]);

      if (fVerbose || !binshift)
        {
          printf("    %.3e    %.3e    %.2f    %.3e    %.3e    %.3e    %.3e\n",
                 yield[i], e_tot[i], e_tot[i]/yield[i]*100., e_stat[i], e_syst[i], e_nonCC[i], e_pTC[i]);

          //printf("    %.3e    %.3e    %.2f    %.3e(%.2f)    %.3e    %.3e    %.3e\n",
          //yield[i],e_tot[i],e_tot[i]/yield[i]*100.,e_stat[i],(e_stat[i]*e_stat[i])/(e_tot[i]*e_tot[i])*100.,
          //e_syst[i],e_nonCC[i],e_pTC[i]);
        }

    }

  if (fVerbose) cout << endl << endl ;

  //_____________________________________________________________________________
  // pT binning or yield shift

  bool undoInvYieldNormFirst = false; // default

  if (binshift)
    {

      TGraphErrors* gtmp = new TGraphErrors(NpTbins, pT, yield, epT, e_tot);

      int N = gtmp->GetN();
      double ptmin = 1.0;
      double ptmax = gtmp->GetX()[N-1];

      TF1* hag = 0;
      if (fConstrainedHagFit)
        {
          hag = hagedornConstrainedFit(gtmp, "constrhagedorn", ptmin, ptmax, fVerbose);
        }
      else
        {
          hag = hagedornFit(gtmp, "hagedorn", ptmin, ptmax, fVerbose);
        }

      double ratio_shifted_over_unshifted_yield = 0.;

      if (binshift == 0.5) // shift along the y-axis (yield) with forced binshift = 0.5 GeV/c
        {
          if (fVerbose)
            cout << "Spectrum shifted along the y-axis (Yield) with FORCED binwidth = 0.5 GeV/c: " << endl;

	  pTbinwidth = 0.5;
	  
          shiftYield(gtmp, hag, pTbinwidth, undoInvYieldNormFirst);

        }
      else if (binshift == 1) // shift along the y-axis (yield)
        {
          if (fVerbose)
            cout << "Spectrum shifted along the y-axis (Yield): " << endl;
	  
          shiftYield(gtmp, hag, pTbinwidth, undoInvYieldNormFirst);

        }
      else if (binshift == 2) // shift along the x-axis (pT)
        {

          if (strcasecmp(type, "full") == 0)
            {
              undoInvYieldNormFirst = true;
              if (fVerbose) cout << "Spectrum shifted (after doing and undoing the 1/pT normalization)"
				 << "along the x-axis (pT): " << endl;
            }
          else
            {
              if (fVerbose) cout << " Spectrum shifted along the x-axis (pT): " << endl;
            }

          shiftPt(gtmp, hag, pTbinwidth, undoInvYieldNormFirst);

        }

      //       int N = gtmp->GetN();
      //       double ptmin = gtmp->GetX()[0]-0.5;
      //       double ptmax = gtmp->GetX()[N-1];

      //       cout << "<I> Bin shift correction: Fitting with a power-law in "
      //            << ptmin << " -- " << ptmax << " GeV/c" << endl;
      //       TF1 *pow = new TF1("pow","[0]/x^[1]",ptmin,ptmax);
      //       pow->SetParameters(1,10);
      //       gtmp->Fit("pow","R","",ptmin,ptmax);

      //       double ratio_shifted_over_unshifted_yield = 0.;

      //       if (fEfficParticle->Contains("pi0"))
      // 	{
      // 	  pTbinwidth = 0.5; // GeV/c
      // 	}
      //       else
      // 	{
      // 	  pTbinwidth = 1.0; // GeV/c
      // 	}

      //       if (binshift == 1) // shift along the y-axis (yield)
      // 	{
      // 	  if (fVerbose) cout << "Spectrum shifted along the y-axis (Yield): " << endl;
      // 	  shiftYieldPowLaw(gtmp,pow,pTbinwidth,undoInvYieldNormFirst);
      // 	}
      //       else if (binshift == 2) // shift along the x-axis (pT)
      // 	{
      // 	  if (strcasecmp(type,"full") == 0)
      // 	    {
      // 	      undoInvYieldNormFirst = true;
      // 	      if (fVerbose)cout << "Spectrum shifted (after doing and undoing the 1/pT normalization)"
      //                              << " along the x-axis (pT): " << endl;
      // 	    }
      // 	  else
      // 	    {
      // 	      if (fVerbose) cout << " Spectrum shifted along the x-axis (pT): " << endl;
      // 	    }
      // 	  shiftPtPowLaw(gtmp,pow,pTbinwidth,undoInvYieldNormFirst);
      // 	}

      if (fVerbose)
        {
          cout << "  pT   pT_err   Ntot   Ntot_err   (Rel_toterr %) ";
          cout << "  Stat_err   Syst_err   CCuncorr_toterr   pTcorr_toterr" << endl;
        }

      for ( int i = 0; i < N; i++ )
        {
          pT[i] = gtmp->GetX()[i];
          epT[i] = gtmp->GetErrorX(i);

          ratio_shifted_over_unshifted_yield = gtmp->GetY()[i]/yield[i];

          e_tot[i] *= ratio_shifted_over_unshifted_yield;
          //cout << " check: e = " << gtmp->GetErrorY(i) << " =? e2 =" << e_tot[i] << endl;

          yield[i] *= ratio_shifted_over_unshifted_yield;

          e_stat[i] *= ratio_shifted_over_unshifted_yield;
          e_syst[i] *= ratio_shifted_over_unshifted_yield;

          e_CC[i] *= ratio_shifted_over_unshifted_yield;
          e_nonCC[i] *= ratio_shifted_over_unshifted_yield;
          e_pTC[i] *= ratio_shifted_over_unshifted_yield;

          // ascii dump

          printf("  %.2f  %.2f    %.3e    %.3e    %.2f    %.3e    %.3e    %.3e    %.3e\n",
                 pT[i], epT[i], yield[i], e_tot[i], e_tot[i]/yield[i]*100., e_stat[i], e_syst[i], e_nonCC[i], e_pTC[i]);
        }

      cout << endl << endl ;

      delete gtmp;
    }

  //_____________________________________________________________________________
  // plot


  TCanvas *c1 = (TCanvas*)canvas(name, 550, 610);
  c1->cd();

  double max_yield = 10.*yield[0];
  double min_yield = 0.1*yield[NpTbins-1];

  Spectrum = new TGraphErrors( NpTbins, pT, yield, epT, e_tot);
  Spectrum->SetName(name);
  Spectrum->SetTitle(title);
  Spectrum->Draw("AP");
  Spectrum->GetYaxis()->SetTitle(ytitle);
  Spectrum->GetYaxis()->SetTitleOffset(1.4);
  Spectrum->GetXaxis()->SetTitle("p_{T} (GeV/#font[72]{c})");
  Spectrum->GetXaxis()->SetTitleOffset(0.95);
  Spectrum->SetMaximum(max_yield);
  Spectrum->SetMinimum(min_yield);
  setMarker(Spectrum, Cent1, Cent2);

  Spectrum->Draw("AP");
  c1->SetLogy();
  c1->Update();

  //_____________________________________________________________________________

  //double ptmin = 1.0;
  //if (strcasecmp(type,"acc") == 0) ptmin = 1.0 ; // acc.-corr. fit starts early (check of Hagedorn weights)
  double ptmin = 2.0;//pT[0];
  double ptmax = pT[NpTbins-1];

  TF1* hagedorn = 0;
  if (fConstrainedHagFit)
    {
      hagedorn = hagedornConstrainedFit(Spectrum, "constrhagedorn", ptmin, ptmax, fVerbose);
    }
  else
    {
      hagedorn = hagedornFit(Spectrum, "hagedorn", ptmin, ptmax, fVerbose);
    }
  hagedorn->Draw("same");
  c1->Update();

  if ( !fFit )
    {
      Spectrum->GetListOfFunctions()->Delete(); // this removes the fit panel too
      //Spectrum->GetListOfFunctions()->Remove(hagedorn);
      gStyle->SetOptFit(0);
      //hagedorn->Delete();
      c1->Update();
    }

  if (!fPlot)
    {
      c1->Close();
    }
  else
    {
      saveCanvas(c1);
    }

  if (fdumpLaTeX)
    {
      dumpLaTeX(Spectrum, e_stat, e_syst, e_nonCC, e_pTC);
    }

  if (fdumpAscii)
    {
      dumpAscii(Spectrum, e_stat, e_syst, e_nonCC, e_pTC);
    }

  return Spectrum;

}
//_____________________________________________________________________________
// OPTIONS:
//
// files:
//
// Au+Au:
//          "pi0_emcal_XX_YY.txt"             = AuAu->pi0 PHENIX EMCal 200 GeV (average PbSc+PbGl)
//          "pi0_pbsc_XX_YY.txt"              = AuAu->pi0 PHENIX PbSc 200 GeV (bin shift corrected in yield)
//          "pi0_pbsc_XX_YY_noshfit.txt"      = AuAu->pi0 PHENIX PbSc 200 GeV (bin shift UNcorrected)
//          "pi0_pbsc_XX_YY_ptshfit.txt"      = AuAu->pi0 PHENIX PbSc 200 GeV (bin shift corrected in pT)
//          "pbgl_year2_FitP3Cor4_YY-ZZ.dat"  = AuAu->pi0 PHENIX PbGl 200 GeV (bin shift corrected in yield)
//          "pbgl_year2_FitP3Cor2_YY-ZZ.dat"  = AuAu->pi0 PHENIX PbGl 200 GeV (bin shift UNcorrected)
//
//          "pi0_ms_pbgl_minb.dat"            = AuAu->pi0 PHENIX PbGl 200 GeV (QM'02)
//          "pi0_ms_pbgl_0-10.dat"            = AuAu->pi0 PHENIX PbGl 200 GeV (QM'02)
//          "pi0_ms_pbgl_70-80.dat"           = AuAu->pi0 PHENIX PbGl 200 GeV (QM'02)
//
//          "PizeroAuAu130_cent.dat"          = AuAu->pi0 PHENIX EMCal 130 GeV
//          "wa98_pi0_cen.dat"                = PbPb->pi0 WA98 central
//
//          "chargedrebin_0_100.txt"          = AuAu->(h-+h+)/2 PHENIX 200 GeV (JJia)
//          "pion_final_cent_10_20.dat"       = AuAu->(pi-+pi+)/2 PHENIX 200 GeV (Tatsuya)
//          "picharged.txt"                   = AuAu->pi+- PHENIX 200 GeV (QM'02)
//          "pbarp_final_feed_0_10.dat"       = AuAu->(p-+pbar)/2 PHENIX 200 GeV (Tatsuya,Julia)
//
//          "eta_yields_YY_ZZ_PID.txt         = AuAu->eta PHENIX PbSc 200 GeV
//
// p+p:
//          "pi0_pp_emcal_200GeV.txt"           = pp->pi0 PHENIX EMCal 200 GeV
//          "pi0_pp_pbsc_200GeV_cross.txt"      = pp->pi0 PHENIX PbSc 200 GeV
//          "pi0_pp_pbgl_200GeV_yields.txt"     = pp->pi0 PHENIX PbSc 200 GeV
//          "yields_pp_leadglass_klaus.txt"     = pp->pi0 PHENIX PbGl 200 GeV
//
//          "yields_pp_hisa_tbl_c22mbc_tot.txt" = pp->pi0 PHENIX PbSc 200 GeV (hi-pT + min.bias trigger QM'02)
//          "yields_pp_hisa_tbl_mbc_tot.txt"    = pp->pi0 PHENIX PbSc 200 GeV (hi-pT trigger QM'02)
//          "yields_pp_sasha.txt"               = pp->pi0 PHENIX PbSc 200 GeV (pre QM'02)
//
// Other data:
//          "pi0_aa_ISR_31.txt"
//          "pi0_dd_ISR_31.txt"
//          "R_AA_ivitev.txt"
//          "ppbar_over_chgdpi_gluonjets_DELPHI.txt"
//          "ppbar_over_chgdpi_quarkjets_DELPHI.txt"
//

TGraphErrors*
emcAnalyzer::plot_spectrum_from_ascii( char *file,
                                       TGraphErrors* &errBand )
{

  TString file_str = file;
  char ffile[400];

  TGraphErrors *SpectrumAscii = 0;

  char title[300];
  sprintf(title, "%s", file);

  //_____________________________________________________________________________

  if ( !file_str.Contains(fDataTablesPath->Data()) )
    {
      if ( file_str.Contains("v2", TString::kIgnoreCase) ) // v2 phenix & star tables
        {
          sprintf(ffile, "%s/v2_tables/%s", fDataTablesPath->Data(), file);
          fFit = false;
        }
      else if ( file_str.Contains("eta_pi0_ratio", TString::kIgnoreCase) ) // eta/pi0 tables
        {
          sprintf(ffile, "%s/eta_pi0_ratios/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("isr", TString::kIgnoreCase) )   // ISR pp data
        {
          sprintf(ffile, "%s/isr_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("plab", TString::kIgnoreCase) ) // other fixed target data
        {
          sprintf(ffile, "%s/other_fixed_target_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("ee_", TString::kIgnoreCase) ) // e+e- tables
        {
          sprintf(ffile, "%s/lep_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("_sis_", TString::kIgnoreCase) ) // GSI/SIS tables
        {
          sprintf(ffile, "%s/sis_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("eta_", TString::kIgnoreCase) ) // phenix eta tables
        {
          sprintf(ffile, "%s/eta_tables/%s", fDataTablesPath->Data(), file);
	  file_str = ffile;
        }
      //       else if ( file_str.Contains("Rcp_dAu",TString::kIgnoreCase)  ) // phenix Rcp d+Au data
      // 	{
      // 	  sprintf(ffile,"%s/RAA_tables/%s",fDataTablesPath->Data(),file);
      // 	}
      else if ( file_str.Contains("dAu", TString::kIgnoreCase) &&
                !file_str.Contains("brahms", TString::kIgnoreCase)&&
                !file_str.Contains("theory", TString::kIgnoreCase) ) // phenix d+Au data
        {
          sprintf(ffile, "%s/dAu_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("pi0_emcal", TString::kIgnoreCase) ||
                file_str.Contains("pi0_final", TString::kIgnoreCase) ) // EMCal tables
        {
          sprintf(ffile, "%s/pi0_emcal_tables/%s", fDataTablesPath->Data(), file);

	  if ( file_str.Contains("62", TString::kIgnoreCase) ) // 62.4 GeV
	    {
	      sprintf(ffile, "%s/pi0_auau62GeV_emcal_tables/%s", fDataTablesPath->Data(), file);
	    }
        }
      else if ( file_str.Contains("direct_gamma", TString::kIgnoreCase) ) // gamma tables
        {
          sprintf(ffile, "%s/gamma_emcal_auau200_tables/%s", fDataTablesPath->Data(), file);
	}
      else if ( file_str.Contains("pbsc", TString::kIgnoreCase) ) // PbSc tables
        {
          sprintf(ffile, "%s/pi0_pbsc_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("pbgl", TString::kIgnoreCase) ) // PbGl tables
        {
          sprintf(ffile, "%s/pi0_pbgl_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("theor", TString::kIgnoreCase) ||
                file_str.Contains("pQCD", TString::kIgnoreCase) ) // theoretical curves
        {
          sprintf(ffile, "%s/theory_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("brahms", TString::kIgnoreCase) ) // brahms data
        {
          sprintf(ffile, "%s/brahms_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("pp_", TString::kIgnoreCase) ) // pp data
        {
          sprintf(ffile, "%s/pp_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("star", TString::kIgnoreCase) ) // brahms data
        {
          sprintf(ffile, "%s/star_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("_sigma_", TString::kIgnoreCase) ) // pp,pn elastic total cross-sections
        {
          sprintf(ffile, "%s/sigma_total_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("130", TString::kIgnoreCase) ) // phenix tables @ 130
        {
          sprintf(ffile, "%s/phnx_130_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("wa98", TString::kIgnoreCase) ||  // WA98 data
                file_str.Contains("ceres", TString::kIgnoreCase) ) // CERES data
        {
          sprintf(ffile, "%s/wa98_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("wa80", TString::kIgnoreCase) )   // WA80 data
        {
          sprintf(ffile, "%s/wa80_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("RAA", TString::kIgnoreCase) ||  // R_AA
                file_str.Contains("Rcp", TString::kIgnoreCase) )  // R_cp
        {
          sprintf(ffile, "%s/RAA_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("pion_final", TString::kIgnoreCase) ) // final charged pions
        {
          sprintf(ffile, "%s/chargedpions_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("charged", TString::kIgnoreCase) ) // charged hadrons
        {
          sprintf(ffile, "%s/chargedhadrons_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("pbar", TString::kIgnoreCase) ||
		file_str.Contains("feed", TString::kIgnoreCase) ) // pr,pbar tables
        {
          sprintf(ffile, "%s/ppbar_tables/%s", fDataTablesPath->Data(), file);
        }
      else if ( file_str.Contains("EKS", TString::kIgnoreCase) ) // EKS DIS nuclear tables
        {
          sprintf(ffile, "%s/EKS_DIS_nuclear/%s", fDataTablesPath->Data(), file);
        }
      else
        {
          sprintf(ffile, "%s/%s", fDataTablesPath->Data(), file);
        }
    }
  else
    {
      sprintf(ffile, "%s", file);
    }

  if ( file_str.Contains("RAA", TString::kIgnoreCase) ||  // R_AA
       file_str.Contains("Rcp", TString::kIgnoreCase) ||  // R_cp
       file_str.Contains("_over_", TString::kIgnoreCase) ||  // ratios
       file_str.Contains("ratio", TString::kIgnoreCase) ) // ratios
    {
      fFit = false;
    }

  //_____________________________________________________________________________
  // read ascii files

  int cols = 0;
  int NpTbins = 0;

  double *pT = 0;
  double *epT = 0;

  double *epTl = 0; // low
  double *epTh = 0; // high

  double *counts = 0;
  double *ecounts = 0;

  double *esyst = 0; // systematics
  double *eNorm = 0; // normalization

  double *ecountsl = 0; // low
  double *ecountsh = 0; // high

  double **vals = 0;

  double sqrt_s = 200.; // default

  int skiplines = 0;


  if ( file_str.Contains("eta_pi0_ratio", TString::kExact) ) // read eta_pi0 ratios before anything else
    {
      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];
      ecounts = new double[NpTbins];
      ecounts = vals[2];
 
//      if ( file_str.Contains("phenix_", TString::kIgnoreCase) )
//        {
// 	 esyst = vals[4]; // TYPE-B syts. ??
// 	 eNorm = vals[6]; // norm. errors ??
// 	 for (int i = 0;i < NpTbins;i++)
// 	   {	      
// 	     if (i == 0) cout << " <I> Adding TYPE-B uncertanties to point-to-point errors for " << file << endl;
// 	     ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], esyst[i], counts[i]);
// 	   }
//        }
    }
  else if ( file_str.Contains("apana", TString::kIgnoreCase) ||  // FNAL E-706
	    file_str.Contains("ee_", TString::kIgnoreCase) )     // LEP data
    {
      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      epT = new double[NpTbins];
      epT = vals[1];
      counts = new double[NpTbins];
      counts = vals[2];
      ecounts = new double[NpTbins];
      ecounts = vals[3];
      esyst = new double[NpTbins];
      esyst = vals[4];

      for (int i = 0;i < NpTbins;i++)
        {

	  double ptmin = pT[i];
	  double ptmax = epT[i];
	  pT[i] = (ptmin + ptmax)/2.;
	  epT[i] = (pT[i] - ptmin);
	  
	  if (i == 0) cout << " <I> Computing <x> from x1 and x2 for file " 
			   << "(WARNING: just mean value of bin !) for" << file << endl;
	  
// 	  // mT scaling
// 	  double m0 = m_pi0;
// 	  if ( file_str.Contains("eta", TString::kIgnoreCase) )
// 	    {
// 	      m0 = m_eta;
// 	    }
// 	  pT[i] = sqrt(pT[i]*pT[i]*91.2*91.2/4.+m0*m0); 
// 	  if (i == 0) cout << " <W> m_T scaling the x-axis !" << file << endl;

	  // these files have an extra (3rd) column with <x> already
	  if ( file_str.Contains("ee_pi0_91.2GeV_aleph96", TString::kIgnoreCase) ||
	       (file_str.Contains("ee_pi0_91.2GeV_all", TString::kIgnoreCase) && i<27))
	    {
	      pT[i] = counts[i];
	      epT[i]= pT[i]-ptmin;
	      counts[i] = ecounts[i];
	      ecounts[i] = esyst[i];
	    }

	  if ( file_str.Contains("apana", TString::kIgnoreCase) )
	    {
	      double pb_to_mb = 1.0e-9;
	      
	      if (i == 0) cout << " <I> ALL CROSS-SECTIONS IN MB: pb ---> mb transformation for file " << file << endl;
	      counts[i] *= pb_to_mb;
	      ecounts[i] *= pb_to_mb;
	      esyst[i] *= pb_to_mb;
	    }

        }

      if ( file_str.Contains("ee_pi0_91.2GeV_aleph96", TString::kIgnoreCase))
	{
	  esyst = vals[5];
	}

      if ( file_str.Contains("ee_", TString::kIgnoreCase) )
	{
	  for (int i = 0;i < NpTbins;i++)
	    {	      
	      if (i == 0) cout << " <I> Adding syst. uncertanties to point-to-point errors for " << file << endl;
	      ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], esyst[i], counts[i]);
	    }
	}
    }
  
  else if ( file_str.Contains("v2_k0short_star", TString::kExact) ) // STAR v2 data (K0short)
    {
      const int bins = 16;

      // Ks minbias v2 (minbias means 0-80% for all species)
      double Ksv2pt[bins] = {
	0.3318, 0.5159, 0.7060, 0.8978, 1.0971, 1.2947, 1.4937, 1.6927,
	1.8916, 2.1675, 2.5500, 2.9394, 3.3532, 3.8767, 4.6943, 5.6322};

      double Ksv2[bins] = {
	0.008, 0.020, 0.044, 0.062, 0.082, 0.098, 0.109, 0.121,
	0.126, 0.132, 0.138, 0.129, 0.141, 0.142, 0.148, 0.161};

      double Ksv2e[bins] = {
	0.0076, 0.0032, 0.0024, 0.0022, 0.0023, 0.0026, 0.0031, 0.0039,
	0.0049, 0.0054, 0.0083, 0.0150, 0.0214, 0.0195, 0.0357, 0.0562};

      NpTbins = (int)bins;
      pT = &(*Ksv2pt);
      counts = &(*Ksv2);
      ecounts = &(*Ksv2e);

      if (file_str.Contains("scaled", TString::kIgnoreCase))
        {
          int constit_quarks = 2;
          for (int i = 0;i < NpTbins;i++)
            {
              pT[i] /= constit_quarks;
              counts[i] /= constit_quarks;
              ecounts[i] /= constit_quarks;
            }
        }
    }

  else if ( file_str.Contains("v2_lambda_star", TString::kExact) ) // STAR v2 data (Lambda)
    {
      const int bins = 16;

      // L+anti-L minbias v2

      double LLbarv2pt[bins] = {
	0.5317, 0.7151, 0.9069, 1.1027, 1.2999, 1.4979, 1.6962, 1.8954,
	2.0957, 2.2933, 2.5722, 2.9766, 3.3722, 3.8945, 4.6864, 5.6419};

      double LLbarv2[bins] = {
	0.000, 0.018, 0.029, 0.055, 0.073, 0.096, 0.111, 0.133,
	0.147, 0.156, 0.171, 0.185, 0.209, 0.198, 0.222, 0.184};

      double LLbarv2e[bins] = {
	0.0104, 0.0049, 0.0034, 0.0029, 0.0027, 0.0027, 0.0029, 0.0032,
	0.0040, 0.0045, 0.0045, 0.0065, 0.0105, 0.0140, 0.0336, 0.0811};

      NpTbins = (int)bins;
      pT = &(*LLbarv2pt);
      counts = &(*LLbarv2);
      ecounts = &(*LLbarv2e);

      if (file_str.Contains("scaled", TString::kIgnoreCase))
        {
          int constit_quarks = 3;
          for (int i = 0;i < NpTbins;i++)
            {
              pT[i] /= constit_quarks;
              counts[i] /= constit_quarks;
              ecounts[i] /= constit_quarks;
            }
        }
    }

  else if ( file_str.Contains("v2_phenix_pi0", TString::kExact) ) //|| // v2 pi0 data
    //file_str.Contains("v2_phenix_chg",TString::kExact) )  // v2 charged data
    {

      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];
      epT = new double[NpTbins];
      epTl = new double[NpTbins];
      epTl = vals[2];
      epTh = new double[NpTbins];
      epTh = vals[3];
      ecounts = new double[NpTbins];
      ecountsl = new double[NpTbins];
      ecountsl = vals[4];
      ecountsh = new double[NpTbins];
      ecountsh = vals[5];

      esyst = new double[NpTbins];
      esyst = vals[6];

      for (int i = 0;i < NpTbins;i++)
        {
          if (i == 0) cout << " <W> Transforming pT error: max,min ---> (max+min)/2 for file "
			   << file << endl;
          epT[i] = (epTh[i] + epTl[i])/2.; //

          if (i == 0) cout << " <W> Transforming v2 error: max,min ---> (max-min)/2 for file "
			   << file << endl;
          ecounts[i] = (ecountsh[i] - ecountsl[i])/2.;

          // fixme (asymmetric) ecountsl and ecountsh added in quad. with syst.
          ecounts[i] = quadRelatError( ecountsl[i], counts[i],
                                       ecountsh[i], counts[i],
                                       esyst[i], counts[i]);
          ecounts[i] *= counts[i];
        }

      if (file_str.Contains("scaled", TString::kIgnoreCase))
        {
          int constit_quarks = 2;
          for (int i = 0;i < NpTbins;i++)
            {
              pT[i] /= constit_quarks;
              counts[i] /= constit_quarks;
              ecounts[i] /= constit_quarks;
            }
        }
    }

  else if ( file_str.Contains("v2_hydro", TString::kExact) || // v2 hydro data
	    file_str.Contains("DELPHI", TString::kExact) ) // DELPHI jets
    {
      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];
    }

  else if ( file_str.Contains("v2", TString::kExact) ) // v2 data
    {
      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      epT = new double[NpTbins];
      epTl = new double[NpTbins];
      epTl = vals[1];
      epTh = new double[NpTbins];
      epTh = vals[2];
      counts = new double[NpTbins];
      counts = vals[3];
      ecounts = new double[NpTbins];
      ecounts = vals[4];

      ecountsl = new double[NpTbins];
      ecountsh = new double[NpTbins];

      //for (int i=0;i<NpTbins;i++){esyst[i]=0.;}

      if ( file_str.Contains("chg", TString::kExact) )
        {
          ecountsl = vals[5];
          ecountsh = vals[6];
        }

      for (int i = 0;i < NpTbins;i++)
        {
          if ( file_str.Contains("star", TString::kExact) )
            {
	      
              if (i == 0) cout << " <W> Transforming v2: % ---> relat for file "
			       << file << endl;
              counts[i] /= 100.; // v2 % ---> relat
              if (i == 0) cout << " <W> Transforming v2 error: max,min ---> (max+min)/2 for file "
			       << file << endl;
              epT[i] = (epTh[i] + epTl[i])/2.; //
              if (i == 0) cout << " <W> Transforming error v2: % ---> relat for file "
			       << file << endl;
              ecounts[i] *= counts[i];
            }
          else
            {
              if (i == 0) cout << " <W> Transforming v2 error: pt bin limits ---> ept for file "
			       << file << endl;
              epT[i] = (epTh[i] - epTl[i])/2.; // binwidth

              //if (esyst[i]==0) esyst[i] = 0.1*ecounts[i];
              //else esyst[i]-=ecounts[i];

              if (!file_str.Contains("chg", TString::kExact))
                {
                  ecounts[i] = quadRelatError( ecounts[i], counts[i],
                                               0.1, 1.);
                  if (i == 0) cout << " <W> Adding additional ~10% syst. to v2 errors for file "
				   << file << endl;
                }
              else
                {
                  // fixme: I take ecountsh (larger than ecountsl) as the error ... should use asymmetric errors
                  ecounts[i] = quadRelatError( ecounts[i], counts[i],
                                               ecountsh[i], 1.);
                  if (i == 0) cout << " <W> Adding additional syst. error to v2 errors for file "
				   << file << endl;
                }
              ecounts[i] *= counts[i];
            }
        }

      if (file_str.Contains("scaled", TString::kIgnoreCase))
        {
          int constit_quarks = 2; // for mesons
          if (file_str.Contains("pr", TString::kIgnoreCase))
            constit_quarks = 3; // for protons
          for (int i = 0;i < NpTbins;i++)
            {
              pT[i] /= constit_quarks;
              counts[i] /= constit_quarks;
              ecounts[i] /= constit_quarks;
            }
        }
    }

  else if (file_str.Contains("k0short_star", TString::kExact)) // STAR R_cp data (K0short)
    {
      const int bins = 20;

      double KsPt6080[bins] = { 0.332, 0.516, 0.715, 0.898, 1.097,
                                1.295, 1.494, 1.693, 1.892, 2.093,
                                2.293, 2.492, 2.689, 2.894, 3.173,
                                3.576, 3.976, 4.503, 5.375, 6.286 };

      double KsRcp6080[bins] = { 3.635e-01, 3.527e-01, 5.196e-01, 5.585e-01, 5.367e-01,
                                 6.143e-01, 6.789e-01, 6.009e-01, 6.847e-01, 5.041e-01,
                                 5.264e-01, 5.384e-01, 3.345e-01, 4.568e-01, 3.327e-01,
                                 2.931e-01, 2.261e-01, 2.522e-01, 2.398e-01, 2.539e-01 };

      double KsRcp6080e[bins] = { 5.794e-02, 4.460e-02, 6.280e-02, 6.490e-02, 6.529e-02,
                                  6.788e-02, 7.893e-02, 7.023e-02, 8.604e-02, 5.764e-02,
                                  6.568e-02, 7.670e-02, 4.868e-02, 8.386e-02, 4.521e-02,
                                  4.121e-02, 3.352e-02, 4.013e-02, 5.012e-02, 7.621e-02 };

      //       double KsPt4060[20] = { 0.332, 0.516, 0.715, 0.898, 1.097,1.295, 1.494, 1.693, 1.892, 2.093,
      // 			     2.293, 2.492, 2.689, 2.894, 3.173, 3.576, 3.976, 4.503, 5.375, 6.286 };

      //       double KsRcp4060[20] = { 5.132e-01, 5.230e-01, 6.297e-01, 6.420e-01, 6.687e-01,
      // 			      7.358e-01, 7.110e-01, 7.025e-01, 6.982e-01, 6.533e-01,
      // 			      6.441e-01, 6.084e-01, 5.004e-01, 5.877e-01, 4.622e-01,
      // 			      4.399e-01, 3.521e-01, 3.384e-01, 2.975e-01, 3.915e-01 };

      //       double KsRcp4060e[20] = {7.454e-02, 6.268e-02, 7.343e-02, 7.157e-02, 7.656e-02,
      // 			      7.858e-02, 7.841e-02, 7.726e-02, 8.068e-02, 7.041e-02,
      // 			      7.205e-02, 7.199e-02, 6.044e-02, 8.016e-02, 6.121e-02,
      // 			      5.930e-02, 4.895e-02, 4.807e-02, 4.512e-02, 7.090e-02 };

      NpTbins = (int)bins;
      pT = &(*KsPt6080);
      counts = &(*KsRcp6080);
      ecounts = &(*KsRcp6080e);
    }

  else if (file_str.Contains("lambda_star", TString::kExact)) // STAR R_cp data (lambda)
    {
      const int bins = 18;
      double Rcp_L_pt[bins] = { 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, 1.7, 1.9, 2.1, 2.3,
                                2.5, 2.7, 2.9, 3.175, 3.575, 4.1, 4.75, 5.55 };

      double Rcp_L_0005_6080[bins] = { 0.290254, 0.345064, 0.45833, 0.530175, 0.61141, 0.730314,
                                       0.785486, 0.791299, 0.7408, 0.833857, 0.741768, 0.790386,
                                       0.685856, 0.902518, 0.727431, 0.517601, 0.333009, 0.254783 };
      double Rcp_L_0005_6080err[bins] = { 0.0488342, 0.0371875, 0.0456934, 0.051473, 0.0569939, 0.0679414,
                                          0.0753439, 0.0803946, 0.0800381, 0.0984117, 0.0784009, 0.109184,
                                          0.102113, 0.156104, 0.18613, 0.0979706, 0.0691089, 0.202486 };

      // double Rcp_L_0005_4060[NpTbins] ={ 0.504669, 0.544987, 0.572532, 0.642528, 0.750399, 0.794633,
      // 	    0.849214, 0.89948, 0.8129, 0.854139, 0.870336, 0.875983,
      // 	    0.854607, 0.857082, 0.737616, 0.697988, 0.502563, 0.340057 };
      // double Rcp_L_0005_4060err[NpTbins] = { 0.0773125, 0.0525052, 0.0475579, 0.0524595, 0.0794571, 0.0684206,
      // 	    0.0741281, 0.0806588, 0.0689429, 0.076807, 0.0680179, 0.0954143,
      // 	    0.0971178, 0.144968, 0.124065, 0.0984918, 0.093963, 0.082472,  };

      NpTbins = (int)bins;
      pT = &(*Rcp_L_pt);
      counts = &(*Rcp_L_0005_6080);
      ecounts = &(*Rcp_L_0005_6080err);
    }
  
  else if (file_str.Contains("pi0_pp_cross_star", TString::kExact)) // STAR pi0 forward rapidity data
    {
      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[2];
      epT = new double[NpTbins];
      counts = new double[NpTbins];
      counts = vals[3];
      ecounts = new double[NpTbins];
      ecounts = vals[4];
      esyst = new double[NpTbins];
      esyst = vals[5];

      double mub_to_mb = 1.0e-3;

      for (int i = 0; i < NpTbins; i++) 
	{ 
	  if (i == 0)
	    {
	      cout << " <W> ALL CROSS-SECTIONS IN MB: mub ---> mb transformation for file "
		   << file << endl;
	      cout << " <I> Adding syst. uncertanties to point-to-point errors for " << file << endl;
	    }

	  counts[i] *= mub_to_mb ; // mub/GeV^2 --> mb/GeV^2
	  ecounts[i] *= mub_to_mb ; // mub/GeV^2 --> mb/GeV^2
	  esyst[i] *= mub_to_mb ; // mub/GeV^2 --> mb/GeV^2

	  ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], esyst[i], counts[i]);
	  epT[i] = 0.;//2.5; 
	}

    }

  else if (file_str.Contains("pQCD_vogelsang_pp_gamma_200GeV_cteq6_sc1_pdf_uncertainty", TString::kExact))
    {

      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];
      ecounts = new double[NpTbins];
      ecounts = vals[2];

      double pb_to_mb = 1.0e-9;

      for (int i = 0;i < NpTbins;i++)
	{
	  if (i == 0) cout << " <W> ALL CROSS-SECTIONS IN MB: pb ---> mb transformation for file "
			   << file << endl;

	  counts[i] *= pb_to_mb ; // pb/GeV^2 --> mb/GeV^2
	  ecounts[i] *= pb_to_mb ; // pb/GeV^2 --> mb/GeV^2

	  //XX --> XX/0.5/0.76/(2*pi*p_T)
	  //where "0.5" is the pt binwidth, 0.76 for eta, 2*pi*p_T
	  //is the conversion factor for dsig/dptdeta ->E* d3sig/dp**3.

	  if (i == 0) cout << " <W> Transforming: counts --> counts/[0.5*0.76*(2*Pi*pT[i])]"
			   << "(binwidth,1-unit y,1/pT) for file " << file << endl;
	  counts[i] /= 0.5*0.76*twoPi*pT[i];
	  ecounts[i] /= 0.5*0.76*twoPi*pT[i];

	  counts[i] = (pT[i]>10.)? counts[i]/2. : counts[i];
	  ecounts[i] = (pT[i]>10.)? ecounts[i]/2. : ecounts[i];

	}
    }

  else if ( file_str.Contains("pQCD_vogelsang_pp_gamma", TString::kExact) )
    {

      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];

      // Second, third, and fourth columns of:
      // pQCD photon files are: "direct","fragmentation", and total

      if (file_str.Contains("dir", TString::kIgnoreCase))
	{
	  cout << " <W> Reading just the ** DIRECT ** gamma yields from file: " << file << endl;
	  counts = vals[1];
	}
      else if (file_str.Contains("frag", TString::kIgnoreCase))	
	{
	  cout << " <W> Reading the ** FRAGMENTATION ** gamma yields from file: " << file << endl;
	  counts = vals[2];
	}
      else
	{ 
	  cout << " <W> Reading the ** TOTAL=DIR+FRAG ** gamma yields from file: " << file << endl;
	  counts = vals[3]; //  "total" gamma yields (4th column)
	}

      double pb_to_mb = 1.0e-9;

      for (int i = 0;i < NpTbins;i++)
	{
	  if (i == 0) cout << " <W> ALL CROSS-SECTIONS IN MB: pb ---> mb transformation for file "
			   << file << endl;
	  counts[i] *= pb_to_mb ; // pb/GeV^2 --> mb/GeV^2
	}
    }

  else if ( file_str.Contains("pQCD_vogelsang_pp_chhad_partonic_subproc", TString::kExact) )
    {

      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      ecounts = new double[NpTbins];

      // Second, third, and fourth columns of:
      // partonic subproc files are: "gg"  "qg" "qq"

      if (file_str.Contains("gg", TString::kIgnoreCase))
	{
	  cout << " <W> Reading just the ** g+g ** percentage of partonic subprocesses from file: " << file << endl;
	  counts = vals[1];
	}
      else if (file_str.Contains("qg", TString::kIgnoreCase))	
	{
	  cout << " <W> Reading the ** q+g ** percentage of partonic subprocesses from file: " << file << endl;
	  counts = vals[2];
	}
      else
	{ 
	  cout << " <W> Reading the ** g+g ** percentage of partonic subprocesses from file: " << file << endl;
	  counts = vals[3]; 
	}

      // arbitrary 10% error
      for (int i = 0;i < NpTbins;i++)
        {
	  ecounts[i] = counts[i]*0.;// *0.10;
        }

       //x-T scaling
//        for (int i = 0;i < NpTbins;i++)
//  	{
//  	  double sqrts = 17.3;
//  	  if (file_str.Contains("62", TString::kIgnoreCase))	
//  	    {
//  	      sqrts = 62.4;
//  	    }
//  	  else if (file_str.Contains("130", TString::kIgnoreCase))	
//  	    {
//  	      sqrts = 130.;
//  	    }
//  	  else if (file_str.Contains("200", TString::kIgnoreCase))	
//  	    {
//  	      sqrts = 200.;
//  	    }
//  	  else if (file_str.Contains("5500", TString::kIgnoreCase))	
//  	    {
//  	      sqrts = 5500.;
//  	    }
//  	  cout << "<I> Normalizing by: 2/" << sqrts << endl;
//  	  pT[i]/=(sqrts/2.);
//  	}
    }

  else if ( file_str.Contains("pQCD", TString::kExact) ||  // NLO pQCD pi0, gamma: Ina Sarcevic, Werner Vogelsang, M. Werlen
	    file_str.Contains("theory", TString::kExact) )
    {
      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];

      if (file_str.Contains("pQCD_vogelsang_pp_pi0_200", TString::kIgnoreCase))
	{
	  for (int i = 0;i < NpTbins;i++)
	    {
	      //XX --> XX/0.5/0.76/(2*Pi*p_T)
	      //where "0.5" is the pt binwidth, 0.76 for eta, 2*pi*p_T
	      //is the conversion factor for dsig/dptdeta ->E* d3sig/dp**3.
	      //if (i == 0) cout << " <W> Transforming: counts --> counts/[0.5*0.76*(2*Pi*pT[i])]"
	      //		       << "(binwidth,1-unit y,1/pT) for file " << file << endl;
	      //counts[i] /= 0.5*0.76*twoPi*pT[i];
	      //counts[i] = (pT[i]>10.)? counts[i]/2. : counts[i];
	    }
	}

      if (file_str.Contains("eta3.8", TString::kExact)) // STAR forward pi0's
	{
	  pT = vals[1];       // vals[0] is E_pion, vals[1] is pT
	  counts = vals[2]; // vals[2] is yield
	}

      if (!(file_str.Contains("AA", TString::kExact)) &&
	  !(file_str.Contains("over", TString::kIgnoreCase)) && // p+p data
	  !(file_str.Contains("werlen", TString::kIgnoreCase)) &&
	  !(file_str.Contains("upc", TString::kIgnoreCase)))
      {

	  double pb_to_mb = 1.0e-9;

          for (int i = 0;i < NpTbins;i++)
            {
              if (i == 0) cout << " <W> ALL CROSS-SECTIONS IN MB: pb ---> mb transformation for file "
			       << file << endl;
              counts[i] *= pb_to_mb ; // pb/GeV^2 --> mb/GeV^2
            }
        }

      if (file_str.Contains("ina"))
	{
	  // Ina's files: Columns 3 and 4 are K-factor and AA/pp
	}

    }

  else if (file_str.Contains("sigma_el", TString::kExact) || 
	   file_str.Contains("sigma_tot", TString::kExact)) // elastic or total p+p(bar) cross-section
    {
      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      fFit = false;

      pT = new double[NpTbins];
      pT = vals[1];
      counts = new double[NpTbins];
      counts = vals[4];
      ecounts = new double[NpTbins];
      ecounts = vals[5];
      esyst = new double[NpTbins];
      esyst = vals[7];

      if ( !file_str.Contains("epem", TString::kExact) && 
	   !file_str.Contains("gg", TString::kExact) )
	{
	  for (int i = 0;i < NpTbins;i++)
	    {
// 	      double plab2 = pT[i]*pT[i];
// 	      if (i == 0) cout << " <W> Transforming p_lab --> sqrt(s) for file "
// 			       << file << endl;
// 	      pT[i] = sqrt(2*m_p*m_p+2*sqrt(plab2+m_p)*m_p); // p_lab --> sqrt(s)
	      
	      //if (i == 0) cout << " <I> Adding syst. uncertanties to point-to-point errors for " << file << endl;
	      //ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], esyst[i], counts[i]);
	    }
	}
    }

  else if (file_str.Contains("dAu", TString::kExact)) // PHENIX d+Au
    {
      //label = new TString("PHENIX d+Au #rightarrow #pi^{0}X @ 200 GeV");
      sqrt_s = 200.;

      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];
      ecounts = new double[NpTbins];
      ecounts = vals[2];

      if (file_str.Contains("Rcp", TString::kIgnoreCase))
        {
          eNorm = new double[NpTbins];
          esyst = vals[3];

          for (int i = 0;i < NpTbins;i++)
            {
              if (i == 0) cout << " <W> Normalization errors transformed relat. to abs. for file "
			       << file << endl;
              eNorm[i] *= counts[i];
            }
        }
      if (file_str.Contains("charged", TString::kIgnoreCase))
        {
          esyst = new double[NpTbins];
          esyst = vals[3];
        }

      if (file_str.Contains("RdA", TString::kIgnoreCase))
        {
          esyst = new double[NpTbins];
          eNorm = new double[NpTbins];

          if (file_str.Contains("charged", TString::kIgnoreCase))
            {
              esyst = vals[5];
              eNorm = vals[6];
            }
          else // pi0
            {
              esyst = vals[3];
            }

          double abs_norm_err_RdA = 0.1183;

          for (int i = 0;i < NpTbins;i++)
            {
              if (i == 0) cout << " <W> Normalization errors for file " << file
			       << " are the sum of " << abs_norm_err_RdA*100.
			       << "% plus syst. errors" << endl;
              eNorm[i] = quadRelatError( esyst[i], counts[i],
                                         abs_norm_err_RdA, 1.);
              eNorm[i] *= counts[i];
            }
        }

    }

  else if ( file_str.Contains("pi0_emcal", TString::kExact) &&      // PHENIX EMCal AuAu 62.4 GeV
            file_str.Contains("62", TString::kExact) )              
    {
      //label = new TString("PHENIX AuAu #rightarrow #pi^{0}X @ 200 GeV");

        skiplines = 0;

      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];
      ecounts = new double[NpTbins];
      ecounts = vals[2];

      eNorm = new double[NpTbins];
      if (file_str.Contains("R_AA", TString::kExact)) // R_AA
	{
	  eNorm = vals[3]; // 3rd and 4th columns are the (same) upper & lower errors ...

	  if (!file_str.Contains("href", TString::kExact)) // R_AA pi0
	    {
	      for (int i = 0;i < NpTbins;i++)
		{
		  eNorm[i] = (counts[i] - eNorm[i]); 
		}
	    }
	}
    }

  else if (file_str.Contains("pi0_pbsc", TString::kExact) ||        // PHENIX PbSc AuAu 200 GeV
           file_str.Contains("pi0_emcal", TString::kExact) ||       // PHENIX EMCal AuAu 200 GeV
           file_str.Contains("eta_tables", TString::kExact) ||      // PHENIX eta AuAu 200 GeV
           file_str.Contains("direct_gamma", TString::kExact) ||    // PHENIX gamma AuAu 200 GeV
           file_str.Contains("pp_emcal_200GeV.", TString::kExact) || // PHENIX EMCal pp 200 GeV
           file_str.Contains("aguilar", TString::kExact) ) //  NA27 fixed target data plab=400 GeV
    {
      //label = new TString("PHENIX AuAu #rightarrow #pi^{0}X @ 200 GeV");

      if ( file_str.Contains("62", TString::kExact) || 
	   file_str.Contains("eta", TString::kExact) || 
	   file_str.Contains("aguilar", TString::kExact) )
        skiplines = 0;
      else if (file_str.Contains("QM02", TString::kExact))
        skiplines = 1;
      else if (file_str.Contains("pp_emcal", TString::kExact))
        skiplines = 1;
      else if (file_str.Contains("pi0_emcal", TString::kExact) || 
	       file_str.Contains("gamma", TString::kExact) )
        skiplines = 2;
      else
        skiplines = 4; // pbsc

      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      epT = new double[NpTbins];
      epT = vals[1];
      counts = new double[NpTbins];
      counts = vals[2];
      ecounts = new double[NpTbins];
      ecounts = vals[3];

      if ( file_str.Contains("aguilar", TString::kExact) )
	{
	  for (int i = 0;i < NpTbins;i++)
	    {
	      if (i == 0) cout << " <W> Transforming pT**2  ---> pT for file " << file << endl;
	      pT[i] = sqrt(pT[i]); 
	      epT[i]/=sqrt(pT[i]);

	      if (i == 0) cout << endl << " <W> Normalizing d^2sigma/dpT^2 ---> d^3sigma/d^3pT (i.e. 1/4pi) for file "<< endl ;
              counts[i] = counts[i]/(2.*twoPi);
              ecounts[i] = ecounts[i]/(2.*twoPi);
	    }
	}
    }

  else if (file_str.Contains("brahms", TString::kExact))  // BRAHMS pp, RdAu, Rcp, and R_AA
    {
      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      epT = new double[NpTbins];
      epT = vals[1];
      counts = new double[NpTbins];
      counts = vals[2];
      ecounts = new double[NpTbins];
      ecounts = vals[3];

      esyst = new double[NpTbins];
      epTl = new double[NpTbins];
      epTh = new double[NpTbins];
      
      if ( file_str.Contains("pp_eta2", TString::kExact) ||
	   file_str.Contains("pp_eta3", TString::kExact) ||
	   file_str.Contains("dau_eta2", TString::kExact) ||
	   file_str.Contains("dau_eta3", TString::kExact) ) // those files have 1 more column for epTh,epTl
	{
	  epTl = vals[1];
	  epTh = vals[2];
	  counts = vals[3];
	  ecounts = vals[4];
	  esyst = vals[5];
	  
	  for (int i = 0;i < NpTbins;i++)
	    {
	      if (i == 0) cout << " <W> Transforming pT error: max,min ---> (max-min)/2 for file "
			       << file << endl;
	      epT[i] = (epTh[i] - epTl[i])/2.; 
	    }
	}
      else
	{
	  for (int i = 0;i < NpTbins;i++)
	    {
	      if (i == 0) cout << " <W> Dividing epT by 2 (full --> half bin width) for file "
			       << file << endl;
	      epT[i] /= 2.; // The epT column is actually the full bin (pT) width
	    }
	}

      if (!file_str.Contains("RAA", TString::kExact) && !file_str.Contains("rda", TString::kExact))
        {
          esyst = vals[4];
          for (int i = 0;i < NpTbins;i++)
	    {
	      if (i == 0) cout << " <I> Adding syst. uncertanties to point-to-point errors for " << file << endl;
	      ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], esyst[i], counts[i]);
	    }
        }
      else
       {
	 eNorm = vals[5];
       }

    }

  else if (file_str.Contains("wa98", TString::kExact)) // WA98 PbPb
    {
      //label = new TString("WA98 PbPb #rightarrow #pi^{0}X @ 17 GeV");
      sqrt_s = 17.3;

      //skiplines = 8;
      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];
      epT = new double[NpTbins];
      epT = vals[2];
      ecounts = new double[NpTbins];
      ecounts = vals[3];

      eNorm = new double[NpTbins];

      if (file_str.Contains("cen_RAA", TString::kIgnoreCase))
        {
          // The 5th column is the stat R_AA error plus (linear) WA98 normalization error (0.32*y)
          ecounts = vals[4];
        }
      else if (file_str.Contains("RAA_blatt", TString::kIgnoreCase)) // RAA from blattnig parametrization
        {
          for ( int i = 0 ; i < NpTbins ; i++)
            {
              double syst_err = 0.25;
              eNorm[i] = syst_err*counts[i];
            }
        }
    }

  else if (file_str.Contains("wa80_sigpi0_O", TString::kExact)) // O+Au --> pi0 (Edsigma/d3p)
    {
      //label = new TString("WA80 S+Au #rightarrow #pi^{0}X @ 19.4 GeV");
      sqrt_s = 19.4;

      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      epT = new double[NpTbins];
      epT = vals[1];
      counts = new double[NpTbins];
      counts = vals[2];
      ecounts = new double[NpTbins];
      ecounts = vals[3];

      for (int i = 0;i < NpTbins;i++)
        {
          pT[i] += epT[i] ; // (e)pT is actually pTmin(max)
          pT[i] /= 2;
          epT[i] = 0.;

          double sigma_OAu = 3490.; // = 2450 mb in PRC44, 2736 (1991) // = 2970 mb (Glauber calc. in paper) // sigma = 3488.91

          if (i == 0) cout << " <W> Transforming counts: sigma(O+Au)/" << sigma_OAu
			   << ". --> Ninv(O+Au) for file " << file << endl;

          double frac_sigma = 0.; // 37% most central colls.

          if (file_str.Contains("cent0_37", TString::kExact))
            frac_sigma = 0.37; // 37% of cross-section
          else if (file_str.Contains("periph40_88", TString::kExact))
            frac_sigma = 0.54; // 54% of cross-section

          if (i == 0) cout << " <W> Normalizing to fraction of centrality: 1/"
			   << frac_sigma*100. << "% for file " << file << endl;

          counts[i] /= sigma_OAu*frac_sigma ;
          ecounts[i] /= sigma_OAu*frac_sigma;
        }
    }

  else if (file_str.Contains("wa80_sigpi0_S", TString::kExact)) // WA80 S+S,Au --> pi0 (Edsigma/d3p)
    {
      //label = new TString("WA80 S+Au #rightarrow #pi^{0}X @ 19.4 GeV");
      sqrt_s = 19.4;

      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      epT = new double[NpTbins];
      epT = vals[1];
      counts = new double[NpTbins];
      counts = vals[2];
      ecounts = new double[NpTbins];
      ecounts = vals[3];

      for (int i = 0;i < NpTbins;i++)
        {
          if (i == 0) cout << " <W> Transforming pT: MeV/c --> GeV/c for file " << file << endl;

          pT[i] /= 1000. ; // pT in MeV/c
          epT[i] /= 1000.; // pT in MeV/c

          if (file_str.Contains("SS", TString::kExact))
            {
              double sigma_SS = 1450.; // EPJ C5, 255 // = 1870 mb (Klaus Glauber) // = 1220. mb (overlap-gsi)
              double frac_sigma = 0.25; // 25% most central colls.

              if (i == 0) cout << " <W> Transforming counts: sigma(S+S)/" << sigma_SS
			       << ". --> Ninv(S+S) for file " << file << endl;
              if (i == 0) cout << " <W> Normalizing to fraction of centrality: x"
			       << frac_sigma*100. << "% for file " << file << endl;

              counts[i] /= sigma_SS*frac_sigma ;
              ecounts[i] /= sigma_SS*frac_sigma;
            }
          else
            {
              double sigma_SAu = 3600.; // EPJ C5, 255 // = 3640 mb (Klaus Glauber) // =  3500. mb (overlap-gsi)
              double frac_sigma = 1.;

              if (i == 0) cout << " <W> Transforming counts: sigma(S+Au)/" << sigma_SAu
			       << ". --> Ninv(S+Au) for file " << file << endl;

              if (file_str.Contains("t01", TString::kExact))
                frac_sigma = 0.300; // 30% of cross-section
              else if (file_str.Contains("t02", TString::kExact))
                frac_sigma = 0.201; // 20% of cross-section
              else if (file_str.Contains("t03", TString::kExact))
                frac_sigma = 0.152; // 15% of cross-section
              else if (file_str.Contains("t04", TString::kExact))
                frac_sigma = 0.095; // 9.5% of cross-section
              else if (file_str.Contains("t05", TString::kExact))
                frac_sigma = 0.105; // 10% of cross-section
              else if (file_str.Contains("t06", TString::kExact))
                frac_sigma = 0.070; // 7% of cross-section
              else if (file_str.Contains("t07", TString::kExact))
                frac_sigma = 0.049; // 5% of cross-section
              else if (file_str.Contains("t08", TString::kExact))
                frac_sigma = 0.028; // 3% of cross-section
              else if (file_str.Contains("t09", TString::kExact))
                frac_sigma = 0.077; // 8% of cross-section

              if (i == 0) cout << " <W> Normalizing to fraction of centrality: 1/"
			       << frac_sigma*100. << "% for file " << file << endl;

              counts[i] /= sigma_SAu*frac_sigma ;
              ecounts[i] /= sigma_SAu*frac_sigma;
            }
        }
    }

  else if ( (file_str.Contains("sasha_qm", TString::kExact)) )  // PHENIX pp 200 GeV [Sasha QM'02]
    {
      //label = new TString("PHENIX pp #rightarrow #pi^{#pm}X @ 200 GeV");

      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];
      ecounts = new double[NpTbins];
      ecounts = vals[3];
    }

  else if (file_str.Contains("130", TString::kExact)) // PHENIX EMCal AuAu 130 GeV
    {
      //label = new TString("PHENIX AuAu #rightarrow #pi^{0}X @ 130 GeV");
      sqrt_s = 130.;

      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      if ( !file_str.Contains("raa", TString::kIgnoreCase) )
        {
          pT = new double[NpTbins];
          pT = vals[0];
          epT = new double[NpTbins];
          epT = vals[1];
          counts = new double[NpTbins];
          counts = vals[2];
          ecounts = new double[NpTbins];
          ecounts = vals[3];
        }
      else
        {
          pT = new double[NpTbins];
          pT = vals[0];
          counts = new double[NpTbins];
          counts = vals[1];

          // the treatment of errors for 130 GeV data is rather peculiar
          ecounts = new double[NpTbins];
          ecounts = vals[6]; // actually Ncoll sigma_{-}
          esyst = new double[NpTbins];
          esyst = vals[7]; // actually Ncoll sigma_{+}

          for (int i = 0;i < NpTbins;i++)
            {
              if (i == 0) cout << " <W> Taking average of syst. (+) and (-) errors for file "
			       << file << endl;
              ecounts[i] = (ecounts[i] + esyst[i])/2.; // we take the average of sigma_{-},{+}
            }
        }
    }

  // this is the STANDARD structure "x-y-ey-ey_syst" or "x-y-ey" (plus added syst. errors)

  else if ( file_str.Contains("sasha", TString::kExact) ||    // PHENIX pp 200 GeV PbSc [Run-2 Sasha Emcal & PbSc Final]
            file_str.Contains("hisa", TString::kExact) ||     // PHENIX pp 200 GeV PbSc [Hisa QM'02]
            file_str.Contains("pp_pbgl", TString::kExact) ||  // PHENIX pp 200 GeV [PbGl Final]
            file_str.Contains("pp_pbsc", TString::kExact) ||  // PHENIX pp 200 GeV PbSc [Hisa QM'02]
            file_str.Contains("pp_200GeV_xsect", TString::kExact) ||  // PHENIX pp 200 GeV pi/K/p
            file_str.Contains("adams", TString::kExact) ||    // fixed-target data
            file_str.Contains("donalds", TString::kExact) ||  // fixed-target data
            file_str.Contains("RAA_chhad_phenix", TString::kExact) )  // PHENIX RAA AuAu charged
    {

      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];
      ecounts = new double[NpTbins];
      ecounts = vals[2];
      esyst = new double[NpTbins];
      esyst = vals[3];

      double pp_luminosity_relat_error = 0.;

      for (int i = 0;i < NpTbins;i++)
        {

          if (file_str.Contains("adams", TString::kIgnoreCase) ||
	      file_str.Contains("pp_200GeV_xsect", TString::kExact))
            {
              //double syst_err = 0.15;
              //if (i==0) cout << " <I> Adding 15% uncertanties to point-to-point error " << file << endl;
              //ecounts[i] = counts[i]*quadRelatError(ecounts[i],counts[i],syst_err,1.);
              //esyst[i]=syst_err*counts[i];

              if (i == 0) cout << " <I> Adding syst. uncertanties to point-to-point errors for " << file << endl;
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], esyst[i], counts[i]);

            }
          else if (file_str.Contains("donalds", TString::kIgnoreCase))
            {
              double syst_err = 0.05;
              // 3% uncertainty in pT scale added as 25% uncertainty
              syst_err = 0.25;

              if (i == 0) cout << " <I> Adding 25% uncertanties to point-to-point error for " << file << endl;
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], syst_err, 1.);
              esyst[i] = syst_err * counts[i];
            }
          else if ( file_str.Contains("pp_pb", TString::kExact) )
            {
              // 1/pT invariant yield normalization lacking in final pbsc pbgl pp ascii files
              if (i == 0)
                {
                  cout << " <W> Normalizing ascii pp spectrum by 1/pT  for file "
		       << file << endl;
                }
              counts[i] /= pT[i];
              ecounts[i] /= pT[i];
              esyst[i] /= pT[i];
            }
          else if ( file_str.Contains("RAA", TString::kExact) )
            {
              if (i == 0)
                {
                  cout << " <W> Transforming % errors --> relat. errors for file "
		       << file << endl;
                }
              // Errors are in %
              ecounts[i] /= 100.;
              esyst[i] /= 100.;

              //if (i == 0) cout << " <W> Setting syst. error to zero for file "
	      //		       << file << endl;
              //esyst[i] = 0.;
            }
          else if (file_str.Contains("hisa", TString::kExact))
            {
              pp_luminosity_relat_error = 0.20; // Additional 20% error on pp yield (luminosity) at QM time
            }

          if ( i==0 )
            {
              cout << " <W> Adding syst. to point-to-point errors in quad. for file " << file << endl;

              if (pp_luminosity_relat_error)
                cout << " <W> Adding an additional " << pp_luminosity_relat_error*100.
		     << "% (luminosity) error to the pp spectrum ... for file "
		     << file << endl;
            }

          ecounts[i] = quadRelatError( ecounts[i], counts[i],
                                       esyst[i] , counts[i],
                                       pp_luminosity_relat_error, 1.);
          ecounts[i] *= counts[i];
          //cout << pT[i] << "  " << counts[i] << " +/- " << ecounts[i] << " +/- " << esyst[i] << endl ;
        }
    }

  else if ( file_str.Contains("ceres.txt", TString::kExact) ||   // CERES Pb+Au --> pi+/- (1/N*dN/dpT)
	    file_str.Contains("star_AuAu_62", TString::kExact) ) // STAR R_AA AuAu 62.4 GeV
    {
      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];
      ecountsh = new double[NpTbins];
      ecountsh = vals[2];
      ecountsl = new double[NpTbins];
      ecountsl = vals[3];

      ecounts = new double[NpTbins];
      eNorm = new double[NpTbins];

      double eSyst = 0.; //0.2

      for (int i = 0;i < NpTbins;i++)
	{
	  if (file_str.Contains("star_AuAu_62", TString::kExact)) // STAR R_AA AuAu 62.4 GeV
	    {
	      if (i == 0) cout << " <W> Transforming ecounts error: max,min ---> (max + min)/2 for file "
			       << file << endl;
	      ecounts[i] = (ecountsl[i] + ecountsh[i])/2.; 
	      eNorm[i] = 0.25;
	    }
	  else if (file_str.Contains("ceres.txt", TString::kExact)) // CERES Pb+Au --> pi+/- (1/N*dN/dpT)
	    {
	      eNorm[i] = 0.2;
	      
	      // FIXME: (asymmetric) ecountsl and ecountsh ... using only "high"
	      ecounts[i] = ecountsh[i] - counts[i];
	      //ecounts[i]=(ecountsh[i]-ecountsl[i])/2.;
	      if (i == 0) cout << " <W> Using only 'high' errors for file " << file << endl;
	      
	      ecounts[i] = quadRelatError( ecounts[i], counts[i], eSyst, 1.);
	      
	      if (i == 0) cout << " <W> Normalizing by 1/(2*pi) counts of file " << file << endl;
	      counts[i] /= 2.*Pi();
	      
	      ecounts[i] *= counts[i];
	    }
	}
      
    }

  // this is the STANDARD structure "x-y-ey" (NO syst. errors)

  else if ( file_str.Contains("ms_pbgl", TString::kExact) ||     // PHENIX AuAu 200 GeV PbGl [Christian QM02]
            file_str.Contains("charged", TString::kExact) ||     // PHENIX AuAu charged hadrons 200 GeV
            file_str.Contains("pion_final", TString::kExact) ||  // PHENIX AuAu charged pions 200 GeV
            file_str.Contains("david", TString::kExact) ||       // PHENIX pp 200 GeV [D.d'E. average]
            file_str.Contains("phojet", TString::kExact) ||      // PHOJET diffractive
            file_str.Contains("star", TString::kExact) ||        // STAR pp
            file_str.Contains("phobos", TString::kExact) ||      // PHOBOS R_AA
            file_str.Contains("Rcp", TString::kExact) ||         // PHENIX Rcp (pi0, ppbar)
            file_str.Contains("ceres", TString::kExact) ||       // latest CERES pi+/- data
	    file_str.Contains("pp_emcal", TString::kExact) ||    // PHENIX pp 200 GeV [Emcal Run-3]
	    file_str.Contains("bass", TString::kExact) ||     // S.Bass PCM gamma 
	    file_str.Contains("taps", TString::kExact) )     // TAPS pi0,eta data
	    
    {

      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];
      ecounts = new double[NpTbins];
      ecounts = vals[2];
 
      if (file_str.Contains("run3", TString::kExact)) // we want to return eSyst as eNorm to plot it w/ boxes ...
       {
          eNorm = new double[NpTbins]; eNorm = vals[3];
       }

      if (file_str.Contains("Rcp", TString::kExact))
        {
          //eNorm = new double[NpTbins]; eNorm = vals[3];

          //cout << " <W> Assigning 20% of syst. error to file: "
          //     << file << endl;
          //for(int i=0;i<NpTbins;i++)
          //  {
          //    ecounts[i] += counts[i]*0.2;
          //  }
        }

      if (file_str.Contains("charged", TString::kExact)) // charged hadron yield is for the sum of + and -
        {
          cout << " <W> Dividing (h- + h+) yield by 2. for file "
	       << file << endl;
          for (int i = 0;i < NpTbins;i++)
            {
              counts[i] = counts[i]/2.;
              ecounts[i] = ecounts[i]/2.;
            }
        }

      if (file_str.Contains("ceres", TString::kExact)) 
	{
	  
	  cout << " <W> Adding 15% syst. errors to point-to-point errors in quad. for file ";
	  
	  double syst_err1 = 0.0;//0.30;
	  double syst_err2 = 0.0;//0.15;
	  
          for (int i = 0;i < NpTbins;i++)
            {
	      if (pT[i] < 1.2)
		{
		  ecounts[i] = counts[i]*quadRelatError(ecounts[i], counts[i], syst_err1, 1.);
		}
	      else
		{
		  ecounts[i] = counts[i]*quadRelatError(ecounts[i], counts[i], syst_err2, 1.);
		}
	    }
	}

      if (file_str.Contains("star_Kshort", TString::kExact)) 
	{
	  
	  cout << " <W> Transforming mT-m0 ---> pT for file ";
	  cout << " <W> Adding 10% syst. errors to point-to-point errors in quad. for file ";
	  
	  double syst_err = 0.10;
	  double m0 = m_k0;
	  
          for (int i = 0;i < NpTbins;i++)
            {
	      ecounts[i] = counts[i]*quadRelatError(ecounts[i], counts[i], syst_err, 1.);
	      pT[i] = sqrt(pT[i]*pT[i]+2*pT[i]*m0); 
	    }
	}

      if (file_str.Contains("sis", TString::kExact))     // TAPS pi0,eta data
	{
	  cout << " <W> Transforming mT ---> pT for file " << file << endl ;

	  double m0 = 0.135;
	  if (file_str.Contains("eta", TString::kExact)) m0 = 0.5475;

          for (int i = 0;i < NpTbins;i++)
            {
	      pT[i]/= 1000.; // MeV --> GeV
	      pT[i] = sqrt(pT[i]*pT[i]-m0*m0); 
	    }
	}
    }

  else if (file_str.Contains("pbgl", TString::kExact)) // PHENIX AuAu 200 GeV PbGl Final
    {

      //label = new TString("PHENIX AuAu #rightarrow #pi^{0}X @ 200 GeV (PbGl)");

      skiplines = 1;
      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      epT = new double[NpTbins];
      epT = vals[1];
      counts = new double[NpTbins];
      counts = vals[2];
      ecounts = new double[NpTbins];
      ecounts = vals[3];
    }

  else if ( file_str.Contains("RAA", TString::kExact) ||  // PHENIX RAA Run-2
            file_str.Contains("ratio", TString::kExact) ) // PHENIX Rcp Run-2, RAA ISR (alpha), ISR g_pi0_ratio
    {

      // label = new TString("PHENIX R_{AuAu}^{#pi^{0}} @ 200 GeV");

      skiplines = 1;
      if ( file_str.Contains("QM02", TString::kExact) ||
	   file_str.Contains("ISR", TString::kIgnoreCase) )
        skiplines = 0;

      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];

      if (file_str.Contains("ISR", TString::kIgnoreCase))
        {
          counts = new double[NpTbins];
          counts = vals[1];
          ecounts = new double[NpTbins];
          ecounts = vals[2];
        }
      else
        {
          epT = new double[NpTbins];
          epT = vals[1];
          counts = new double[NpTbins];
          counts = vals[2];
          ecounts = new double[NpTbins];
          ecounts = vals[3];
          if (file_str.Contains("qm", TString::kIgnoreCase))
            {
              eNorm = new double[NpTbins];
              eNorm = vals[4];
            }
          else
            {
              eNorm = new double[NpTbins];
              eNorm = vals[5];
            }
        }
    }

  else if ( (file_str.Contains("ISR", TString::kIgnoreCase)) ||  // CERN ISR pp, dd, aa --> pi0 X
            (file_str.Contains("plab", TString::kIgnoreCase)) )  // other fixed-target data
    {

      bool apply_corrs = false;

      if (file_str.Contains("_corr", TString::kIgnoreCase))
        {
          apply_corrs = true;
          cout << endl << " <I> Will apply corrections (if there) for file: " << file << endl;

          TString ffile_corr = ffile;
          ffile_corr.Remove(ffile_corr.Capacity()-9); // "chop" the "_corr.txt" suffix
          ffile_corr.Append(".txt"); // add back the ".txt" suffix

          vals = readAscii(ffile_corr.Data(), cols, NpTbins, skiplines, fVerbose);
        }
      else
        {
          vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
        }

      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      sqrt_s = 62.4; // (default) note it could be: 31.; 53.; 63.;

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];
      ecounts = new double[NpTbins];
      ecounts = vals[2];

      esyst = new double[NpTbins];
      if (cols == 4)
        esyst = vals[3];

      ecountsh = new double[NpTbins];
      ecountsl = new double[NpTbins];
      epTl = new double[NpTbins];
      epTh = new double[NpTbins];

      //_____________________________________________________________________________
      // Direct photon contamination at ISR ~63 GeV
      // run: plot_gamma_over_pi0()

      // pol2 fit to Akessson 89 gamma/pi0 ratio [forced to go through (0.,0.)]
      //double p0 = 6.18783e-03;
      //double p1 = -2.44061e-03;
      //double p2 =  4.70673e-03;
      //TF1 *gamma_over_pi0 = new TF1("gamma_contamination","pol2",0.,10.);
      //gamma_over_pi0->SetParameters(p0,p1,p2);

      // pol1 fit: e.plot_spectrum_from_ascii("isr_gamma_over_pi0_62GeV.txt",eband)
      //double p0 = -1.94284e-01;
      //double p1 = 6.41268e-02;
      //TF1 *gamma_over_pi0 = new TF1("gamma_contamination", "pol1", 0., 16.);
      //gamma_over_pi0->SetParameters(p0, p1);

      // pol4 fit to e.plot_spectrum_from_ascii("isr_ratio_gamma_over_pi0_62GeV_exp_nlo.txt",eband)
      //       double p0 =  7.30757e-03;
      //       double p1 = -1.36874e-02;
      //       double p2 =  9.03436e-03;
      //       double p3 = -8.07873e-04;
      //       double p4 =  4.97502e-05;      
      //TF1 *gamma_over_pi0 = new TF1("gamma_contamination", "pol4", 0., 16.);
      //gamma_over_pi0->SetParameters(p0,p1,p2,p3,p4);

      // pol2 fit to exp. data + pQCD (scale = 1 +1 -05)
      //double p0 =  5.54489e-03;
      //double p1 = -9.58138e-03;
      //double p2 =  4.96962e-03;
      //TF1 *gamma_over_pi0 = new TF1("gamma_contamination","pol2",0.,15.);
      //gamma_over_pi0->SetParameters(p0,p1,p2);

      // pol3 fit to exp. data + pQCD (scale = 1 +1 -05)
      //double p0 = 7.45883e-03;
      //double p1 = -1.18557e-02;
      //double p2 = 7.15807e-03;
      //double p3 = -1.07253e-04;
      //TF1 *gamma_over_pi0 = new TF1("gamma_contamination","pol3",0.,15.);
      //gamma_over_pi0->SetParameters(p0,p1,p2,p3);

      double p0 = 4.55122e-02;
      double p1 =-6.03625e-02;
      double p2 = 2.51208e-02;
      double p3 =-2.53308e-03;
      double p4 = 1.02714e-04;
      
      TF1 *gamma_over_pi0 = new TF1("gamma_contamination","pol4",0.,15.);
      gamma_over_pi0->SetParameters(p0,p1,p2,p3,p4);

      //_____________________________________________________________________________
      // Correction for yields differences: 62 -- 62.4 -- 63 GeV
      // run: plot_ratio_62GeV_63GeV.C

      TF1 *ratio_sqrts63_to_624 = new TF1("ratio_sqrts63_to_624","pol2",0.,15.);
      p0 = 1.01461; p1 = 1.68315e-03; p2 = 3.11128e-04;
      ratio_sqrts63_to_624->SetParameters(p0,p1,p2);

      //TF1 *ratio_sqrts62_to_624 = new TF1("ratio_sqrts62_to_624","pol2",0.,15.);
      //p0 = 0.990389; p1 = -1.14242e-03; p2 = -1.92400e-04;
      //ratio_sqrts62_to_624->SetParameters(p0,p1,p2);


      //_____________________________________________________________________________
      // Adding extra systematic errors & other corrections

      for ( int i = 0 ; i < NpTbins ; i++)
        {

          if (file_str.Contains("pp_pi0_isr_63GeV_akesson", TString::kIgnoreCase))
            {
              double pb_to_mb = 1.0e-9;

              if (i == 0) cout << " <I> ALL CROSS-SECTIONS IN MB: pb ---> mb transformation for file " << file << endl;
              counts[i] *= pb_to_mb;
              ecounts[i] *= pb_to_mb;
              if (esyst) esyst[i] *= pb_to_mb;

	      //  corrections for different sqrts = 63 GeV instead of 62.4 GeV
              if (apply_corrs)
                {
		  if (i == 0) cout << " <W> CORRECTION: Scale yields for SQRT(S) = 63 ---> 62.4 GeV for file " << file << endl;
		  double corr_sqrts = ratio_sqrts63_to_624->Eval(pT[i]);
		  counts[i] /= corr_sqrts;
		  ecounts[i] /= corr_sqrts;
		  if (esyst) esyst[i] /= corr_sqrts;
		}

	      // no errors added here: included already in point-to-point

            }

          else if ( ( file_str.Contains("drijard", TString::kIgnoreCase) ||
                      file_str.Contains("alper", TString::kIgnoreCase) ||
                      file_str.Contains("breakstone", TString::kIgnoreCase) ) &&
		     !file_str.Contains("chg", TString::kIgnoreCase) ) // averaged data contains 10% error already
            {

	      //  corrections for different sqrts = 63 GeV instead of 62.4 GeV
	      if ( (apply_corrs) && ( file_str.Contains("alper", TString::kIgnoreCase) || 
				      file_str.Contains("drijard", TString::kIgnoreCase) ) )
		{
		  if (i == 0) cout << " <W> CORRECTION: Scale yields for SQRT(S) = 63 ---> 62.4 GeV for file " << file << endl;
		  double corr_sqrts = ratio_sqrts63_to_624->Eval(pT[i]);
		  counts[i] /= corr_sqrts;
		  ecounts[i] /= corr_sqrts;
		  if (esyst) esyst[i] /= corr_sqrts;
		}

	      // uncertainties
              double syst_err = 0.10;
	      if (file_str.Contains("23GeV_alper", TString::kIgnoreCase)) syst_err = 0.15;

              if (i == 0) cout << " <W> Adding " << syst_err*100. << " syst. to point-to-point errors in quad. for file ";
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], syst_err, 1.);
              if (esyst) esyst[i] = syst_err*counts[i];

            }

          else if (file_str.Contains("pi0_isr_62.4GeV_busser76", TString::kIgnoreCase)) // pi-,pi+,pichg uncorrected ...
            {

              if (apply_corrs)
                {
                  if (i == 0)
		    {
		      cout << " <W> CORRECTION: Subtracting direct-photon contamination for file " << file << endl;
		    }

		  if (pT[i] > 1.5)
		    {		      
		      double R = 1/gamma_over_pi0->Eval(pT[i]);
		      counts[i] *= R/(1+R);
		      ecounts[i] *= R/(1+R);
		    }
		}

	      // uncertainties
              double syst_err = 0.26;

              if (i == 0) cout << " <W> Adding 26% syst. (3% en. scale)"
			       << " to point-to-point errors in quad. for file ";
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], syst_err, 1.);
              esyst[i] = syst_err*counts[i];

	    }

          else if (file_str.Contains("pi0_isr_62.4GeV_busser73", TString::kIgnoreCase))
            {

              if (apply_corrs)
                {
		  if (i == 0) cout << " <W> CORRECTION: Multipliying by 0.72 (eta contamination correction: Busser PLB75)"
				   << " from pi0 yields for file " << file << endl;
		  if (pT[i] > 1.5)
		    {
		      counts[i] *= 0.72;
		      ecounts[i] *= 0.72;
		    }
		}

	      // uncertainties
              double syst_err = 0.55;

              if (i == 0) cout << " <W> Adding 55% syst. (6% en. scale)"
			       << " to point-to-point errors in quad. for file ";
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], syst_err, 1.);
              esyst[i] = syst_err*counts[i];

	      // photon not needed to be corrected: low pT

            }

	  else if (file_str.Contains("pi0_isr_23.5GeV_busser76", TString::kIgnoreCase))
            {
	      
	      // uncertainties
              double syst_err = 0.26;
	      
              if (i == 0) cout << " <W> Adding 26% syst. (3% en. scale) to point-to-point errors in quad. for file ";
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], syst_err, 1.);
              esyst[i] = syst_err*counts[i];
	      
	      // photon not needed to be corrected: low pT
	      
            }
	  
	  else if (file_str.Contains("clark", TString::kIgnoreCase))
            {
              ecountsh[i] = ecounts[i];
              ecountsl[i] = esyst[i];

              if (apply_corrs)
                {
                  if (i == 0)
		    {
		      cout << " <W> CORRECTION: Substracting 18% eta contamination above pT = 1.5 GeV/c"
			   << " from pi0 yields for file " << file << endl;
		      cout << " <W> CORRECTION: Subtracting direct-photon contamination " 
			   << "(authors claim -15% subtracted already...) for file " << file << endl ;
		      cout << " <W> CORRECTION: Scale yields for SQRT(S) = 63 ---> 62.4 GeV for file " << file << endl;
		    }

		  //  corrections for different sqrts = 63 GeV instead of 62.4 GeV
		  double corr_sqrts = ratio_sqrts63_to_624->Eval(pT[i]);
		  counts[i] /= corr_sqrts;
		  ecounts[i] /= corr_sqrts;

                  if (pT[i] > 1.5)
                    {
                      counts[i] *= 0.82;
                      ecounts[i] *= 0.82;

		      double R = 1/gamma_over_pi0->Eval(pT[i]);
                      counts[i] *= R/(1+R);
                      ecounts[i] *= R/(1+R);
                    }
                }

	      // uncertainties
              double syst_err = 0.17;
              double syst_err2 = 0.25;

              if (i == 0) cout << " <W> Adding 17% (syst.) + 25% (3%+2% en. scale)"
			       << " to point-to-point errors in quad. for file ";
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], syst_err, 1., syst_err2, 1.);
              esyst[i] = syst_err*counts[i];

            }

          else if ( file_str.Contains("angelis78", TString::kIgnoreCase) )
            {

              if (apply_corrs)
                {
                  if (i == 0)
		    {
		      cout << " <W> CORRECTION: Substracting 18% eta contamination above pT = 1.5 GeV/c"
			   << " from pi0 yields for file " << file << endl;
		      cout << " <W> CORRECTION: Subtracting direct-photon contamination for file " << file << endl;
		    }

                  if (pT[i] > 1.5)
                    {
                      counts[i] *= 0.82;
                      ecounts[i] *= 0.82;

		      double R = 1/gamma_over_pi0->Eval(pT[i]);
                      counts[i] *= R/(1+R);
                      ecounts[i] *= R/(1+R);
                    }
                }

	      // uncertainties
              double syst_err = 0.25;
              double abs_err = 0.05;
              if (i == 0) cout << " <W> Adding 25% (5% en. scale) "
			       << " + 5% (abs. normalization: luminosity)"
			       << " to point-to-point errors in quad. for file ";
	      
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], syst_err, 1., abs_err, 1.);
              esyst[i] = syst_err*counts[i];

            }
          else if ( file_str.Contains("eggert", TString::kIgnoreCase) )
            {

              if (apply_corrs)
                {
                  if (i == 0)
		    {
		      cout << " <W> CORRECTION: Substracting 18% eta contamination above pT = 1.5 GeV/c"
			   << " from pi0 yields for file " << file << endl;
		      cout << " <W> CORRECTION: Subtracting direct-photon contamination for file " << file << endl;
		      cout << " <W> CORRECTION: Scale yields for SQRT(S) = 63 ---> 62.4 GeV for file " << file << endl;
		    }

		  //  corrections for different sqrts = 63 GeV instead of 62.4 GeV
		  double corr_sqrts = ratio_sqrts63_to_624->Eval(pT[i]);
		  counts[i] /= corr_sqrts;
		  ecounts[i] /= corr_sqrts;


                  if (pT[i] > 1.5)
                    {
                      counts[i] *= 0.82;
                      ecounts[i] *= 0.82;

		      double R = 1/gamma_over_pi0->Eval(pT[i]);
                      counts[i] *= R/(1+R);
                      ecounts[i] *= R/(1+R);
                    }
                }

	      // uncertainties
              double syst_err = 0.35; // uncertainty is ~25% but resolved pi0 are -30% below unresolved ones ...
              double abs_err = 0.05;

              if (i == 0) cout << " <W> Adding 35% (5% + 50-MeV en. scale) "
			       << " + 5% (abs. normalization: luminosity)"
			       << " to point-to-point errors in quad. for file ";

              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], syst_err, 1., abs_err, 1.);
              esyst[i] = syst_err*counts[i];

            }

          else if (file_str.Contains("angelis89", TString::kIgnoreCase))
            {
              pT[i] = (pT[i] + counts[i])/2.;
              counts[i] = ecounts[i];
              ecounts[i] = esyst[i];

              if (apply_corrs)
                {
                  if (i == 0) cout << " <W> CORRECTION: Substracting 18% eta contamination above pT = 1.5 GeV/c"
				   << " from pi0 yields for file " << file << endl;
                  if (pT[i] > 1.5)
                    {
                      counts[i] *= 0.82;
                      ecounts[i] *= 0.82;
                    }
                }

 	      // uncertainties
	      double syst_err = 0.17; // 15% at low pT -- 19% at high pT
              double syst_err2 = 0.25; // 5% energy scale uncertainty
	      
              if (i == 0) cout << " <W> Adding 17% (syst.) + 25% (5% en.scale) to point-to-point errors in quad. for file ";
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], syst_err, 1., syst_err2, 1.);
              esyst[i] = syst_err*counts[i];

            }

          else if (file_str.Contains("kourkou", TString::kIgnoreCase))
            {

              pT[i] = (pT[i] + counts[i])/2.;
              counts[i] = ecounts[i];
              ecounts[i] = esyst[i];

              if (apply_corrs)
                {

		  //  corrections for different sqrts = 63 GeV instead of 62.4 GeV
		  if (file_str.Contains("62.8GeV_ kourkou", TString::kIgnoreCase))
		    {
		      if (i == 0) cout << " <W> CORRECTION: Scale yields for SQRT(S) = 63 ---> 62.4 GeV for file " 
				       << file << endl;
		      double corr_sqrts = ratio_sqrts63_to_624->Eval(pT[i]);
		      counts[i] /= corr_sqrts;
		      ecounts[i] /= corr_sqrts;
		      if (esyst) esyst[i] /= corr_sqrts;
		    }

                  if (i == 0) cout << " <W> CORRECTION: Subtracting direct-photon contamination for file " << file << endl;

                  if (pT[i] > 1.5)
                    {
		      double R = 1/gamma_over_pi0->Eval(pT[i]);
                      counts[i] *= R/(1+R);
                      ecounts[i] *= R/(1+R);
                    }
                }

	      // uncertainties
              double syst_err = 0.;
	      TString mess;
	      if (file_str.Contains("62.8GeV", TString::kIgnoreCase))
		{
		  syst_err = 0.35;
		  mess = " <W> Some of the pT points should be corrected for finite bin width ...\n <W> Adding";
		  mess += syst_err*100.;
		  mess += "% (en.scale+norm.: 27%-42%) to point-to-point errors in quad. for file ";
		  mess += file;
		}
	      else if (file_str.Contains("62.4GeV", TString::kIgnoreCase))
		{
		  syst_err = 0.22; 
		  mess = " <W> Adding ";
		  mess += syst_err*100.;
		  mess += "% to point-to-point errors in quad. for file ";
		  mess += file;
		}

              if (i == 0)
		{
		  cout << mess << endl;
		}
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], syst_err, 1.);
              esyst[i] = syst_err*counts[i];

            }

          else if (file_str.Contains("carey", TString::kIgnoreCase))
            {

	      if (i == 0) cout << " <W> Substracting 18% eta contamination above pT = 1.5 GeV/c"
			       << " from pi0 yields for file " << file << endl;
	      if (pT[i] > 1.5)
		{
		  counts[i] *= 0.82;
		  ecounts[i] *= 0.82;
		}

	      // uncertainties
              double syst_err = 0.3;  // FIXME: X% uncertainty in pT scale not added

              if (i == 0) cout << " <W> Adding 30% syst. to point-to-point errors in quad. for file ";
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], syst_err, 1.);
              esyst[i] = syst_err*counts[i];

            }

          else if (file_str.Contains("antrea", TString::kIgnoreCase))
            {
	      // uncertainties
              double syst_err = 0.2; // fixme: 1% uncertainty in pT scale not added

              if (i == 0) cout << " <W> Adding 20% syst. to point-to-point errors in quad. for file ";
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], syst_err, 1.);
              esyst[i] = syst_err*counts[i];
            }
          else if (file_str.Contains("amaldi", TString::kIgnoreCase))
            {
	      // uncertainties
              double syst_err = 0.1; // fixme: possible 3% uncertainty in pT scale not added

              if (i == 0) cout << " <W> Adding 10% syst. to point-to-point errors in quad. for file ";
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], syst_err, 1.);
              esyst[i] = syst_err*counts[i];
            }
          else if (file_str.Contains("bonesi", TString::kIgnoreCase))
            {
	      // uncertainties

              if (i == 0) cout << " <W> Adding syst. to point-to-point errors in quad. for file ";
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], esyst[i], counts[i]);
            }
          else if (file_str.Contains("adamus", TString::kIgnoreCase))
            {

	      if (i == 0) cout << endl << " <W> Transforming X-axis: pT**2 ---> pT for file "<< endl ;
	      
	      pT[i] = sqrt(pT[i]);
	      //epT[0epT[i] /= sqrt(epT[i]);

	      if (i == 0) cout << endl << " <W> Normalizing d^2sigma/dpT^2 ---> d^3sigma/d^3pT " 
			       << "(i.e. 1/4pi) for file "<< endl ;
              counts[i] = counts[i]/(2.*twoPi);
              ecounts[i] = ecounts[i]/(2.*twoPi);

	      // uncertainties
	      // 10% ?? unknown (Russian original)

              double syst_err = 0.10;

              if (i == 0) cout << " <W> Adding 35% syst. to point-to-point errors in quad. for file ";
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], syst_err, 1.);
              esyst[i] = syst_err*counts[i];

            }
          else if (file_str.Contains("lloyd", TString::kIgnoreCase))
            {

	      // uncertainties
	      // 5% uncertainty in pT scale

              double syst_err = 0.35;

              if (i == 0) cout << " <W> Adding 35% syst. to point-to-point errors in quad. for file ";
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], syst_err, 1.);
              esyst[i] = syst_err*counts[i];

            }
          else if (file_str.Contains("marzo", TString::kIgnoreCase))
            {
	      // uncertainties
	      // THERE IS ALSO A 1 PCT UNCERTAINTY IN THE PT SCALE AND A 7 PCT UNCERTAINTY IN THE NORMALISATION

              double syst_err = 0.15; // 1% uncertainty in pT scale == pow(1.01,-13.)~12% yield uncertainty

              if (i == 0) cout << " <W> Adding 15% syst. to point-to-point errors in quad. for file ";
              ecounts[i] = counts[i] * quadRelatError(ecounts[i], counts[i], syst_err, 1.);
              esyst[i] = syst_err*counts[i];
            }
          else
            {
              if (i == 0) cout << " <W> NO syst. errors added to point-to-point for file " << file << endl;
            }
        }
      cout << " " << file << endl;
    }

  else if ( file_str.Contains("pbar", TString::kExact) ||
	    file_str.Contains("feed", TString::kIgnoreCase) ) // PHENIX pr, pbar, and (p+pbar)/2  Run-2
    {
      skiplines = 0;
      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];
      ecounts = new double[NpTbins];
      ecounts = vals[2];
    }

  else if (file_str.Contains("EKS", TString::kExact) ) // DIS nuclear (EKS)
    {
      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];

      if (file_str.Contains("E665.", TString::kIgnoreCase))
        {
          for (int i = 0;i < NpTbins;i++)
            {
              pT[i] = pT[i] + counts[i]; // counts[]=vals[1] is actually pTmax
              pT[i] /= 2.;
            }
          counts = vals[2]; // vals[2] is actual counts
        }
    }

  else if (file_str.Contains("vitev", TString::kExact) ) // R_AA from Ivan Vitev
    {
      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];
    }

  else if (file_str.Contains("pi0_final", TString::kExact) ) // Run-2 pi0 final
    {
      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ... " << endl;
          return SpectrumAscii;
        }

      pT = new double[NpTbins];
      pT = vals[0];
      counts = new double[NpTbins];
      counts = vals[1];
      ecounts = new double[NpTbins];
      ecounts = vals[2];
      esyst = new double[NpTbins];
      esyst = vals[6];

    }

  else
    {
      cout << " <E> emcAnalyzer::plot_spectrum_from_ascii. "
	   << " I don't know how to read this file \"" << file << "\"..." << endl;
      //cout << " <W> Will try to read just 2 columns ..." << endl;
      return SpectrumAscii;
    }

  //_____________________________________________________________________________

  double corr_yield[NpTbins];
  double ecorr_yield[NpTbins];
  bool const_ept = false;
  bool no_ept = false;
  bool no_ecounts = false;

  if (!epT)
    {
      if (fVerbose > 1) cout << endl << " <I> No epT ascii points found in file ..." << endl ;
      epT = new double[NpTbins];

      //const_ept = true;
      no_ept = true;
    }

  if (!ecounts)
    {

      if (fVerbose > 1) cout << endl << " <I> No ecounts ascii points found in file ..." << endl ;

      ecounts = new double[NpTbins];
      no_ecounts = true;
    }

  for (int pTbin = 0; pTbin < NpTbins ; pTbin++)
    {

      //       corr_yield[pTbin]  = scale*Abs(counts[pTbin]);
      //       ecorr_yield[pTbin] = scale*Abs(ecounts[pTbin]);

      //       if (pT[pTbin]==0) pT[pTbin] = 0.1;
      //       if (corr_yield[pTbin]==0) corr_yield[pTbin] = 1.;

      if (no_ecounts)
        ecounts[pTbin] = 0.; // null counts errors

      // cm2 --> mb
      double cm2_to_mb = 1.0e27;

      if (counts[0] && abs(counts[0]) < 10e-20) // counts could be exactly 0 for theoretical curves
        {
          if (pTbin == 0)
            cout << " <I> ALL CROSS-SECTIONS IN MB: cm^2 ---> mb transformation for file " << file << endl;
          for ( int i = 0 ; i < NpTbins ; i++)
            {
              counts[i] *= cm2_to_mb;
              ecounts[i] *= cm2_to_mb;
              if (esyst)
                esyst[i] *= cm2_to_mb;
            }
        }

      corr_yield[pTbin] = counts[pTbin];
      ecorr_yield[pTbin] = ecounts[pTbin];

      if (const_ept)
        epT[pTbin] = 0.05 ; // const pT bin error
      if (no_ept)
        epT[pTbin] = 0. ; // null pT bin error

      if (fVerbose > 1)
        {
          cout << "  " << pT[pTbin] << "+/-" << epT[pTbin] ;
          //cout << "  " << counts[pTbin] << " +/- " << ecounts[pTbin] << endl ;
          cout << " --> " << corr_yield[pTbin] << " +/- " << ecorr_yield[pTbin] << endl;
        }

    }

  //_____________________________________________________________________________

  // pT axis instead of pT**2

  if (fXaxis.Contains("pT**2", TString::kIgnoreCase))
    {

      sprintf(title, "pt2_%s", file);
      cout << endl << " <W> Transforming X-axis: pT**2 ---> pT "<< endl ;

      for (int i = 0; i < NpTbins ; i++)
        {
          pT[i] = sqrt(pT[i]);
          epT[i] /= sqrt(epT[i]);
        }
    }

  // mT axis:

  if (fXaxis.Contains("mT", TString::kIgnoreCase))
    {

      sprintf(title, "mt_%s", file);
      cout << endl << " <W> Transforming X-axis: pT ---> mT " ;

      double m0 = 0.;

      if (fXaxis.Contains("eta", TString::kExact))
	{
	  cout << " assuming eta mass " << endl;
	  m0 = m_eta;
	}
      else // if (fXaxis.Contains("pi", TString::kExact))
	{
	  cout << " assuming pi0 mass " << endl;
	  m0 = m_pi0;
	}

      for (int i = 0;i < NpTbins;i++)
	{
	  pT[i] = sqrt(pT[i]*pT[i]+m0*m0);
	  //epT[i] = ;
	}
      }


  // xT axis instead of pT
  if (fXaxis.Contains("xT", TString::kIgnoreCase))
    {
      sprintf(title, "xt_%s", file);
      cout << endl << " <W> Transforming X-axis: pT ---> xT (Note: sqrt(s)= " << sqrt_s << " GeV)" << endl ;

      for (int i = 0; i < NpTbins ; i++)
        {
          pT[i] = 2.*pT[i]/sqrt_s;
          epT[i] = 2.*epT[i]/sqrt_s;
        }
    }

  //_____________________________________________________________________________
  // plot

  //   double max_yield = 10.*Max(corr_yield[0],corr_yield[1]);
  //   double min_yield = 0.1*corr_yield[NpTbins-1];

  TCanvas *c1 = new TCanvas(title, title, 600, 600);
  //fCanvasList->Add(c1);
  c1->cd();

  SpectrumAscii = new TGraphErrors( NpTbins, pT, corr_yield, epT, ecorr_yield);
  SpectrumAscii->SetTitle(title);
  SpectrumAscii->SetName(title);
  SpectrumAscii->Draw("AP");
  //SpectrumAscii->GetYaxis()->SetTitleOffset(1.25);

  if (file_str.Contains("raa", TString::kIgnoreCase))
    {
      SpectrumAscii->GetYaxis()->SetTitle("R_{AA}");
    }
  else if (file_str.Contains("rcp", TString::kIgnoreCase))
    {
      SpectrumAscii->GetYaxis()->SetTitle("R_{cp}");
    }
  else if (file_str.Contains("v2", TString::kIgnoreCase))
    {
      SpectrumAscii->GetYaxis()->SetTitle("v_{2}");
    }
  else
    {
      SpectrumAscii->GetYaxis()->SetTitle("1/[2#pi p_{T}] d^{2}N/dp_{ T}dy (GeV/#font[72]{c})^{-2}");
    }

  SpectrumAscii->GetXaxis()->SetTitle("p_{T} (GeV/#font[72]{c})");

  double min_yield, max_yield ;
  getYminmax( SpectrumAscii, min_yield, max_yield);
  max_yield *= 10.;
  min_yield *= 0.1;

  SpectrumAscii->SetMaximum(max_yield);
  SpectrumAscii->SetMinimum(min_yield);
  setMarkerLineType(SpectrumAscii, 20, 2, 1.2);

  SpectrumAscii->Draw("AP");
  if ( (!file_str.Contains("raa", TString::kIgnoreCase)) &&
       (!file_str.Contains("rcp", TString::kIgnoreCase)) &&
       (!file_str.Contains("ratio", TString::kIgnoreCase)) &&
       (!file_str.Contains("_over_", TString::kIgnoreCase)) &&
       (!file_str.Contains("v2", TString::kIgnoreCase)) )
    {
      c1->SetLogy();
    }

  if (fXaxis.Contains("xT", TString::kExact))
    {
      c1->SetLogx();
      SpectrumAscii->GetYaxis()->SetTitle("Invariant #pi^{0} yield"); // (GeV/c)^{-2}");
      SpectrumAscii->GetXaxis()->SetTitle("x_{T}=2p_{T}/#sqrt{s_{NN}}");
      SpectrumAscii->GetXaxis()->SetRangeUser(0.01, 1.);
    }

  // Plot (and return) normalization error band (if errors are quoted in file ...)
  if (eNorm)
    {

      cout << " <W> Normalization errors present for file " << file << endl
	   << "     YOU SHOULD PLOT eband !! " << endl;

      errBand = new TGraphErrors( NpTbins, pT, corr_yield, epT, eNorm);
      TH1F* band = (TH1F*)graph2histo(errBand);
      band->SetFillColor(kYellow);
      c1->cd();
      band->Draw("e3same");
      SpectrumAscii->Draw("P"); // redraw it
    }

  c1->Update();

  //_____________________________________________________________________________
  // fitting

  //double ptmin = 1.0;
  double ptmin = 0.4;
  double ptmax = 10.;
  double A = 1.e5; // mb/(GeV/c)^2
  //double p0 = 1.22;
  double p0 = 1.72;
  double n = 12.4;

  if (file_str.Contains("wa98", TString::kExact))
    {
      ptmax = 5.0; //pT[NpTbins-3];
      p0 = 0.3;
    }
  else
    {
      ptmax = pT[NpTbins-1]; // play with this value in case of no fit ...
    }

  TF1 *hagedornascii = 0;

  //   if ( !file_str.Contains("RAA",TString::kIgnoreCase) &&
  //        !file_str.Contains("RdA",TString::kIgnoreCase) &&
  //        !file_str.Contains("Rcp",TString::kIgnoreCase) &&
  //        !file_str.Contains("ratio",TString::kIgnoreCase) &&
  //        !file_str.Contains("v2",TString::kIgnoreCase) )

  if ( fFit )
    {
      if (fConstrainedHagFit)
        {
          hagedornascii = hagedornConstrainedFit(SpectrumAscii, "constrhagedornascii", ptmin, ptmax, fVerbose);
        }
      else
        {
          hagedornascii = hagedornFit(SpectrumAscii, "hagedornascii", ptmin, ptmax, fVerbose, A, p0, n);
        }

      hagedornascii->Draw("same");
      c1->Update();

      //       if ( !fFit )
      // 	{
      // 	  SpectrumAscii->GetListOfFunctions()->Delete();
      // 	  //SpectrumAscii->GetListOfFunctions()->Remove(hagedornascii);
      // 	  gStyle->SetOptFit(0);
      // 	  //if (hagedornascii) hagedornascii->Delete();
      // 	  c1->Update();
      // 	}
    }

  if ( !fPlot )
    {
      c1->Close();
    }
  else
    {
      saveCanvas(c1);
    }

  if (fdumpAscii && fVerbose) // dump only when verbose on (so that, e.g., it does not get dumped when plotting R_AA)
    {
      dumpAscii(SpectrumAscii);
      if (eNorm) dumpAscii(SpectrumAscii,eNorm);
    }

  if (fdumpLaTeX)
    {
      dumpLaTeX(SpectrumAscii);
      if (eNorm) dumpLaTeX(SpectrumAscii,eNorm);
    }

  return SpectrumAscii;

}

//_____________________________________________________________________________

TGraphErrors*
emcAnalyzer::plot_spectrum_from_ascii( const int Cent1,
                                       const int Cent2,
                                       const char *cut ) // "tof1chisq1", "tof1chisq1_noshift", "pbgl", "emcal", "pp"

  //  If cut = "pbgl" then the function plots PbGl files
  //  If cut = "pp" then the function plots pp pi0 cross-section

{

  TString cut_str = cut ;

  char file[300];
  char title[300];
  //TString *legend = 0;

  if ( cut_str.Contains("pbgl") ) // PbGl
    {
      //sprintf(title,"pbgl_year2_FitP3Cor2_%d-%d",Cent1,Cent2); // bin-UNshifted PbGl files
      sprintf(title, "pbgl_year2_FitP3Cor4_%d-%d", Cent1, Cent2); // default bin-shifted PbGl files
      sprintf(file, "%s/pbgl_tables/%s.dat", fDataTablesPath->Data(), title);
    }
  else if ( cut_str.Contains("QM02") ) // QM PbSc
    {
      sprintf(title, "pi0_pbsc_%d_%d_QM02.txt", Cent1, Cent2);
      sprintf(file, "%s/pbsc_tables_QM02/%s.dat", fDataTablesPath->Data(), title);
    }
  else if ( cut_str.Contains("emcal") ) // PbGl+PbSc
    {
      sprintf(title, "pi0_emcal_%d_%d_%s", Cent1, Cent2, favoritecut);
      sprintf(file, "%s/pi0_emcal_tables/%s.txt", fDataTablesPath->Data(), title);
    }
  else if ( cut_str.Contains("pp") ) // pp pi0
    {
      sprintf(title, "pp_pi0");
      sprintf(file, "%s/pp_tables/pi0_pp_emcal_200GeV_final_cross_sasha.txt", fDataTablesPath->Data());
    }
  else if ( cut_str.Contains("eta") ) // PbGl+PbSc
    {
      sprintf(title, "eta_pbsc_%d_%d_tof2chisq2", Cent1, Cent2);
      sprintf(file, "%s/eta_tables/%s.txt", fDataTablesPath->Data(), title);
    }
  else // true PbSc cut
    {
      int CC = getCentralityClass(Cent1, Cent2);
      if ( CC == -1) return 0;

      sprintf(title, "pi0_pbsc_%d_%d_%s", Cent1, Cent2, cut);     // default y-shifted file (unless "cut" = "cut_noshift")
      sprintf(file, "%s/pbsc_tables/%s.txt", fDataTablesPath->Data(), title);
    }

  TGraphErrors *errBand;
  return plot_spectrum_from_ascii( file, (TGraphErrors*&)errBand);

}

//_____________________________________________________________________________
//  cut = "tof1chisq1" ("tof1chisq_noshift"), ..., "pbgl", "emcal", "eta"
//  spectrum = "full", "acc" ...
//  type_of_plot = "global" (1 canvas) or "singles" (n canvases)

void
emcAnalyzer::plot_all_spectra_run2( const char *cut,
                                    const char *spectrum,
                                    const char *type_of_plot,
                                    double relativeScaling,
                                    bool readAscii )
{

  TString cut_str = cut ;

  int Cent1 = 0;
  int Cent2 = 0;
  double Scale = relativeScaling;

  double ptmax = 12.5;
  int ptbins = (int)ptmax*2;
  double ymax = 1500.;
  double ymin = 5e-17;  // /10.*relativeScaling;
  if (ymin == 1.) ymin = 1.e-8;

  char title[50];

  TString label;

  if ( !cut_str.Contains("pbgl") ) // true PbSc cut
    {
      sprintf(title, "pi0_pbsc_spectra_all_cent_%s", cut);
      label = "PbSc #pi^{0} spectra: ";
    }
  else // PbGl file
    {
      sprintf(title, "pi0_pbgl_spectra_all");
      label = "PbGl #pi^{0} spectra: ";
      readAscii = true; // forced reading from corrected data files on disk
    }

  if ( cut_str.Contains("QM02") )
    {
      sprintf(title, "pi0_pbsc_spectra_all_QM02.txt"); // QM02 spectra
      label = "#pi^{0} spectra: ";
      readAscii = true; // forced reading from corrected data files on disk
    }

  if ( cut_str.Contains("emcal") ) // PbGl+PbSc final yields
    {
      sprintf(title, "pi0_emcal_spectra_all_cent_%s", cut);
      label = "#pi^{0} spectra: ";
      readAscii = true; // forced reading from corrected data files on disk
    }

  if ( cut_str.Contains("eta") ) // eta yields
    {
      sprintf(title, "eta_pbsc_spectra_all_cent_%s", cut);
      label = "#eta spectra: ";
      readAscii = true; // forced reading from corrected data files on disk
    }

  TCanvas *c10 = 0;
  TLegend *legend = 0;

  if (strcasecmp(type_of_plot, "global") == 0)
    {
      c10 = (TCanvas*)canvas(title, 650, 650);
      c10->Range( -1.7, -19.4, 12.6, 1.40);
      c10->SetLogy();

      TH2F *myframe = (TH2F*)frame(title, ptbins, 0., ptmax, 100, ymin, ymax);
      myframe->Draw();
      c10->Update();

      legend = new TLegend(0.645, 0.61, 0.96, 0.96, label, "brNDC");
      legend->SetMargin(0.1);
      legend->SetTextSize(0.042);
      legend->SetFillStyle(0);
      legend->SetBorderSize(0);
      legend->Draw();
      c10->Update();
    }

  char sublabel[100];

  //_____________________________________________________________________________
  // loop on all centralities

  TGraphErrors *temp_spec_[CentClasses+1] ;

  int binshift = 1;
  fPlot = false;
  fFit = false;
  gStyle->SetOptFit(0);

  // Let's start with min.bias
  int i = CentClasses; // 20;
  getCentralityClassLimits2( i, Cent1, Cent2);

  if (strcasecmp(type_of_plot, "global") == 0)
    {
      if (readAscii)
        {
          temp_spec_[i] = (TGraphErrors*)plot_spectrum_from_ascii(Cent1, Cent2, cut);
        }
      else // reconstruct it from raw yields (only PbSc)
        {
          temp_spec_[i] = (TGraphErrors*)plot_spectrum(Cent1, Cent2, cut, spectrum, binshift);
        }

      if (temp_spec_[i])
        {
          scale(*temp_spec_[i], 2, 0, fVerbose);
          c10->cd();
          temp_spec_[i]->Draw("P");
          sprintf(sublabel, "min.bias #times 2");
          legend->AddEntry(temp_spec_[i], sublabel, "P");
          setMarker(temp_spec_[i], Cent1, Cent2);
        }
    }
  else // singles
    {
      fFit = true;
      fPlot = true;
      gStyle->SetOptFit(1);
      if (readAscii)
        {
          temp_spec_[i] = (TGraphErrors*)plot_spectrum_from_ascii(Cent1, Cent2, cut);
        }
      else
        {
          temp_spec_[i] = (TGraphErrors*)plot_spectrum(Cent1, Cent2, cut, spectrum, binshift);
        }
    }

  // Rest of cent. classes
  for (int i = 0; i < CentClasses; i+=2)
    {
      getCentralityClassLimits2( i, Cent1, Cent2);

      if (i == 18) continue; // skip 60-80% here

      if (strcasecmp(type_of_plot, "global") == 0)
        {

          if (readAscii)
            {
              temp_spec_[i] = (TGraphErrors*)plot_spectrum_from_ascii(Cent1, Cent2, cut);
            }
          else
            {
              temp_spec_[i] = (TGraphErrors*)plot_spectrum(Cent1, Cent2, cut, spectrum, binshift);
            }

          if (temp_spec_[i])
            {
              if (i == 18) Scale = Scale/relativeScaling; // 60-80%
              scale(*temp_spec_[i], Scale, 0, fVerbose);
              c10->cd();
              temp_spec_[i]->Draw("P");
              setMarker(temp_spec_[i], Cent1, Cent2);
              sprintf(sublabel, "%i-%i%%", Cent1, Cent2);
              if (Scale != 1)
                {
                  if (fVerbose && Scale < 1. )
                    {
                      cout << " <I> Spectrum scaled by a factor: 1/" << 1/Scale << endl ;
                    }
                  else if (fVerbose && Scale > 1. )
                    {
                      cout << " <I> Spectrum scaled by a factor: " << Scale << endl ;
                    }
                  //if (relativeScaling != 1) sprintf(sublabel,"%s (#times%2f.1)",sublabel,relativeScaling);
                  int expon = (int) Log10(Scale);
                  sprintf(sublabel, "%s #times 10^{%d}", sublabel, expon);
                }
              legend->AddEntry(temp_spec_[i], sublabel, "P");
              //legend->Draw();
              c10->Update();
              Scale *= relativeScaling;
            }
        }
      else // "singles"
        {
          fFit = true;
          fPlot = true;
          gStyle->SetOptFit(1);
          if (readAscii)
            {
              temp_spec_[i] = (TGraphErrors*)plot_spectrum_from_ascii(Cent1, Cent2, cut);
            }
          else
            {
              temp_spec_[i] = (TGraphErrors*)plot_spectrum(Cent1, Cent2, cut, spectrum, binshift);
            }
        }

    } // loop on centralities

  //   // let's replot the most central
  //   if (strcasecmp(type_of_plot,"global") == 0)
  //     {
  //       c10->cd();
  //       if (temp_spec_[0]) temp_spec_[0]->Draw("P");
  //       c10->Update();
  //     }


}

//_____________________________________________________________________________
// COMPETE collab. parametrization (hep-ph/0212101)
//

double emcAnalyzer::sigma_pp_tot( const double sqrts )
{
  double sigma_tot = 42.6*pow(sqrts*sqrts,-0.46) + 35.5 + 0.307*pow(log(sqrts*sqrts/29.1), 2.);
  double sigma_tot_err = 33.4*pow(sqrts*sqrts,-0.545);

  cout << "<I> sigma_tot = " << sigma_tot << " +/- " << sigma_tot_err 
       << " mb at sqrt(s) = " << sqrts << " GeV " << endl;

  return sigma_tot;
}

//_____________________________________________________________________________
// "poor man fit": TF1 ln2s("ln2s","[0]+[1]*(x*x)^[2]+[3]*pow(log(x*x),2)",5,5000);
// to pp(bar) elastic tables from http://pdg.lbl.gov/~sbl/pp_elastic.dat,ppbar_elastic.dat
//

double emcAnalyzer::sigma_pp_el( const double sqrts )
{
  double sigma_el =0.;

  double p0 =  3.42,   ep0 = 0.;
  double p1 = 34.51,   ep1 = 1.896;
  double p2 = -0.5405, ep2 = 0.012;
  double p3 = 0.06,    ep3 = 0.;

  double epar[]={ep0,ep1,ep2,ep3};

  TF1 ln2s("ln2s","[0]+[1]*(x*x)^[2]+[3]*pow(log(x*x),2.)",5,5000);
  ln2s.SetParameters(p0,p1,p2,p3);
  ln2s.SetParErrors(epar);
  sigma_el = ln2s.Eval(sqrts);

  cout << "<I> sigma_el = " << sigma_el << " +/- " << sigma_el*0.08
       << " mb at sqrt(s) = " << sqrts << " GeV " << endl;

  return sigma_el;
}

//_____________________________________________________________________________
// sigma_inel ~ (2/3)*sigma_tot

double emcAnalyzer::sigma_pp_inel( const double sqrts )
{
  double sigma_inel = sigma_pp_tot(sqrts) - sigma_pp_el(sqrts);
  double sigma_inel_err = quadRelatError(33.4*pow(sqrts*sqrts,-0.545),sigma_pp_tot(sqrts),
					 0.08,1.);

  cout << "<I> sigma_inel = " << sigma_inel << " +/- " << sigma_inel_err*sigma_inel
       << " mb at sqrt(s) = " << sqrts << " GeV " << endl;

  return sigma_inel;
}

//_____________________________________________________________________________
// Returns parametrized invariant dsigma/dpT p+p spectra (mb/GeV^2)
//
// type   = "blattnig"
//        = "xnwang"  ---> partic = "had-", "had0-", "pi0"
//        = "axel"    (had 130GeV)
//        = "wa98"    (pi0)
//        = "carey"   (pi0)
//        = "breakstone"  (had+,-)
//        = "dente"   (pi0)
//        = "compete"  (p+p total cross-section)
//        = "soft_pp" (fit to exponential pi+/- p+p)
//        = "run3_pi0" (pi0 Run-3)

TF1*
emcAnalyzer::plot_pp_sigma_parametrization( const char* partic,
					    const char* type,
					    double sqrts,
					    const double scale,
					    double eta )
{

  TF1 *f_pp_param = 0;

  TString partic_str = partic;
  TString type_str = type;

  char name[300];
  if (type_str.Contains("compete", TString::kIgnoreCase) )
    sprintf(name, "sigma_tot_pp_param_%s",type);
  else
    sprintf(name, "pp_param_%s_%.2f_%s", partic, sqrts, type);

  TCanvas *c4 = new TCanvas(name, name, 600, 600);
  //fCanvasList->Add(c4);
  c4->SetLogy();
  c4->cd();

  //   TPaveText *pave = new TPaveText(0.59,0.56,0.90,0.88,"brNDC");
  //   pave->SetFillColor(0);
  //   pave->SetTextColor(1);
  //   TText *text = 0;
  TLegend *leg = new TLegend(0.305369, 0.809783, 0.880872, 0.95471, NULL, "brNDC");
  leg->SetMargin(0.1);
  leg->SetLineColor(1);
  leg->SetLineStyle(1);
  leg->SetLineWidth(1);
  leg->SetFillStyle(0);
  leg->SetBorderSize(0);
  leg->SetTextSize(0.034);
  TLegendEntry *entry = 0;

  double mpartic = 0;

  double ptmin = 1.;
  double ptmax = 5.;

  if (type_str.Contains("blat", TString::kIgnoreCase))
    {

      double sqrts_min = 2.*m_p + m_pi0;
      double sqrts2 = sqrt(sqrts);

      double k1 = 3.24;
      double k2 = -6.046;
      double k3 = 4.35;
      double k4 = 0.883;
      double k5 = -4.08;
      double k6 = -3.05;
      double k7 = -0.0347;
      double k8 = 3.046;
      double k9 = 4.098;
      double k10 = -1.152;
      double k11 = -0.0005;

      double params[15] = {scale, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, sqrts, sqrts_min, sqrts2};

      TString func = "[0]*log([12]/[13])";
      func += "*exp([1]+[2]*x+[3]/[14]+[4]*x^2.+[5]/([14]^2.)+[6]*x/[14]+[7]*x^3.+[8]/([14]^3.)";
      func += "+[9]*x/([14]^2.)+[10]*x^2./[14]+[11]/(x^3.))";

      ptmax = sqrts/2.;
      //TF1 *f_blatt_default = new TF1("f_blatt_default",func.Data(),0.,ptmax);
      //f_blatt_default->SetParameters(params);
      //f_pp_param = new TF1("f_pp_blattnig","f_blatt_default*(x>1.5)+1.5*f_blatt_default*(x<1.5)",0.,ptmax);

      f_pp_param = new TF1("f_pp_blattnig", func.Data(), 0., ptmax);
      f_pp_param->SetParameters(params);


      entry = leg->AddEntry(f_pp_param, "Blattnig p+p  #rightarrow #pi^{0}", "L");

    }

  else if (type_str.Contains("wang", TString::kIgnoreCase))
    {

      // Wang: pi0's in p+p at sqrt(s) = 17.3 GeV
      ptmax = sqrts/2.;
      f_pp_param = new TF1("f_pp_wang",
                           "[0]*exp(-sqrt([1]**2+x**2)/[2])+[3]*(1.-2.*x/[4])**[5]/(x**2.+[6]**2.)**[7]",
                           0.5, ptmax);
      double C0 = 0.;
      double T0 = 0.154;
      double C = 0.;
      double a = 0.;
      double p0 = 0.;
      double n = 0.;

      if (sqrts >= 17.3 && sqrts <= 17.4) // (h- = pi- + K- + pbar)
        {
          C0 = 169.6;
          mpartic = m_pi0;
          C = 24565;
          a = 9.3;
          p0 = 2.43;
          n = 6.29;
        }
      else if (sqrts == 19.4)
        {
          if ( partic_str.Contains("had0-", TString::kIgnoreCase) ) // (h0- = (pi-+pi+)/2 + K- + pbar)
            {
              C0 = 174.7;
              mpartic = m_pi0; // fixme: is that true ? (not a big effect at high pT in any case)
              C = 13653;
              a = 9.1;
              p0 = 2.27;
              n = 6.22;
            }
          if ( partic_str.Contains("had-", TString::kIgnoreCase) ) // (h- = pi- + K- + pbar)
            {
              C0 = 174.7;
              mpartic = m_pi0; // fixme: is that true ? (not a big effect at high pT in any case)
              C = 17653;
              a = 9.8;
              p0 = 2.27;
              n = 6.29;
            }
          else if ( partic_str.Contains("pi", TString::kIgnoreCase)) // (h- = pi- + K- + pbar)
            {
              C0 = 150.3;
              mpartic = m_pi0;
              C = 13653;
              a = 9.1;
              p0 = 2.37;
              n = 6.22;
            }
        }
      else if (sqrts == 200.) // RHIC
        {
          C0 = 440.;
          mpartic = m_pi0;
          C = 653;
          a = 0.;
          p0 = 1.75;
          n = 4.98;
        }
      else
        {
          cout << " <E> Wang's parametrization is only for 17.3, 19.4 and 200 GeV" << endl;
          return 0;
        }

      f_pp_param->SetParameters(scale*C0, mpartic, T0, scale*C, sqrts, a, p0, n);

      entry = leg->AddEntry(f_pp_param, "X.N.Wang p+p  #rightarrow #pi^{0}", "L");

    }

  else if (type_str.Contains("wa98", TString::kIgnoreCase))
    {
      ptmax = sqrts/2.;
      double sqrts_sps = 17.3;
      double F = 9.;

      // xT scaled and normalized to sqrt(s) = 17.3 GeV (Christoph Blume's thesis)
      if (sqrts != sqrts_sps)
        {
          cout << " <I> plot_pp_sigma_parametrization() scaling WA98 parametrizaton for sqrt(s)="
	       << sqrts << " GeV !" << endl;
        }

      f_pp_param = new TF1("f_pp_wa98", "(((1-2*x/[0])/(1-2*x/[1]))^[2])*[3]*pow([4]/(x+[4]),[5])", 0., ptmax);
      f_pp_param->SetParameters( sqrts, sqrts_sps, F,
                                 scale*132., 9.02, 55.77 );
      f_pp_param->SetLineColor(1);
      f_pp_param->Draw("same");
      entry = leg->AddEntry(f_pp_param, "WA98 p+p #rightarrow #pi^{0}", "L");
    }

  else if (type_str.Contains("carey", TString::kIgnoreCase))
    {
      ptmax = sqrts/2.;
      f_pp_param = new TF1("f_pp_carey", "[0]*5.*(x^2+0.86)^(-4.5)*(1-2.*x/[1])^4", 0, ptmax);
      f_pp_param->SetParameters(scale, sqrts);
      f_pp_param->SetLineColor(8);
      f_pp_param->Draw("same");
      entry = leg->AddEntry(f_pp_param, "Carey p+p  #rightarrow #pi^{0}", "L");

    }

  else if (type_str.Contains("axel", TString::kIgnoreCase) && sqrts==130. )
    {

      double A = scale*330.; // mb/(GeV/c)^2
      double p0 = 1.72;
      double n = 12.4;
      sqrts = 130.;
      int verbose = 0;

      f_pp_param = (TF1*) hagedornInit("haged", ptmin, ptmax, verbose, A, p0, n);
      f_pp_param->SetName("f_axel_pp_130");

      entry = leg->AddEntry(f_pp_param, "130 GeV Power-law fit:");
    }

  else if (type_str.Contains("axel", TString::kIgnoreCase) && (sqrts>=62.&&sqrts<=64) )
    {

      //_____________________________________________________________________________
      // Charged hadron parametrization @ 62 GeV by Axel Drees 
      // (divide by h/pi~1.6 ratio to get pi0 ref.)
      // 1st one: f(p_T) = 223.3/(exp(-0.00405*x)+x/2.0657))**14.556

      double A = scale*196.4; // mb/(GeV/c)^2
      double a = 0.0226;
      double p0 = 2.301;
      double n = 14.86;

      ptmin=1.5, ptmax=10.;

      f_pp_param = new TF1("f_axel_pp_62", "[0]/(exp([1]*x)+x/[2])^[3]", ptmin, ptmax);
      f_pp_param->SetParameters(A,a,p0,n);
      f_pp_param->SetName("f_axel_pp_62");

      entry = leg->AddEntry(f_pp_param, "62 GeV Axel parametrization");
    }

  else if (type_str.Contains("break", TString::kIgnoreCase)) // breakstone +,-
    {

      char title[300];

      //       // positives
      //       double A00plus = 4.8164,
      // 	A10plus = -6.2953,
      // 	A20plus = 0.3332,
      // 	A30plus = -0.0155,
      // 	A01plus = -5.1510,
      // 	A11plus = 11.0073,
      // 	A21plus = -5.3261,
      // 	A31plus = 0.5068,
      //  	A02plus = 0.,
      //  	A12plus = 0.,
      //  	A22plus = 0.,
      //  	A32plus = 0.;
      
      //       // negatives
      //       double A00minus = 5.4505,
      // 	A10minus = -9.5992,
      // 	A20minus = 2.9395,
      // 	A30minus = -1.3642,
      // 	A01minus = -14.2117,
      // 	A11minus = 36.6920,
      // 	A21minus = -80.1132,
      // 	A31minus = 64.8912,
      // 	A02minus = 0.,
      // 	A12minus = -38.2022,
      // 	A22minus = 5.8091,
      //      A32minus = 0.;
      
      //       // positives
      //       double P00plus = 0.0321,
      // 	P10plus = 0.0129,
      // 	P20plus = 0.3755,
      // 	P30plus = 0.6649,
      // 	P01plus = 0.0079,
      // 	P11plus = -0.0046,
      // 	P21plus = 0.1999,
      // 	P31plus = 0.4258,
      //  	P02plus = 0.,
      //  	P12plus = 0.,
      //  	P22plus = 0.,
      //  	P32plus = 0.;
      
      //       // negatives
      //       double P00minus = 0.0010,
      // 	P10minus = -0.0872,
      // 	P20minus = -0.2122,
      // 	P30minus = -0.6201,
      // 	P01minus = -0.2962,
      // 	P11minus = -0.2304,
      // 	P21minus = -0.4826,
      // 	P31minus = -0.9479,
      // 	P02minus = 0.,
      // 	P12minus = 0.0793,
      // 	P22minus = 0.4283,
      // 	P32minus = 0.;
      
      // A{k,l}+ : positives
      double A_plus[4][3] = { { 4.8164, -5.1510, 0.},   // [0][0],[0][1],[0][2]
			      {-6.2953, 11.0073, 0.},   // [1][0],[1][1],[1][2]
			      { 0.3332, -5.3261, 0.},   // [2][0],[2][1],[2][2]
			      {-0.0155,  0.5068, 0.} }; // [3][0],[3][1],[3][2]      
      // A{k,l}- : negatives
      double A_minus[4][3] = { { 5.4505,-14.2117, 0.},       // [0][0],[0][1],[0][2]
			       {-9.5992, 36.6920, -38.2022}, // [1][0],[1][1],[1][2]
			       { 2.9395,-80.1132, 5.8091},   // [2][0],[2][1],[2][2]
			       {-1.3642, 64.8912, 0.} };     // [3][0],[3][1],[3][2]      
      
      // P{k,l}+: positives:
      double P_plus[4][3] = { {0.0321, 0.0079, 0.},   // [0][0],[0][1],[0][2]
			      {0.0129,-0.0046, 0.},   // [1][0],[1][1],[1][2]
			      {0.3755, 0.1999, 0.},   // [2][0],[2][1],[2][2]
			      {0.6649, 0.4258, 0.} }; // [3][0],[3][1],[3][2]      
      // P{k,l}-: negatives:
      double P_minus[4][3] = { { 0.0010, -0.2962, 0.},     // [0][0],[0][1],[0][2]
			       {-0.0872, -0.2304, 0.0793}, // [1][0],[1][1],[1][2]
			       {-0.2122, -0.4826, 0.4283}, // [2][0],[2][1],[2][2]
			       {-0.6201, -0.9479, 0.} };   // [3][0],[3][1],[3][2]   

      double akl_plus[4][3];
      double akl_minus[4][3];

      //ptmax = sqrts/2.;

      double z = pow(eta/log(sqrts/m_pi),2.);

      double factor = sqrts/(2*m_p);

      for (int k=0; k<4; k++)
	{
	  for (int l=0; l<3; l++)
	    {
	      akl_plus[k][l] = A_plus[k][l]*pow(factor,P_plus[k][l]);
	      akl_minus[k][l] = A_minus[k][l]*pow(factor,P_minus[k][l]);
	    }
	}

      TString func = "[0]*exp(";
      func += "[1]      + [2]*[13]      + [3]*[13]^2      +";
      func += "[4]*x    + [5]*[13]*x    + [6]*[13]^2*x    +";
      func += "[7]*x^2  + [8]*[13]*x^2  + [9]*[13]^2*x^2  +";
      func += "[10]*x^3 + [11]*[13]*x^3 + [12]*[13]^2*x^3 ";
      func += ")";

      sprintf(title,"f_pp_breakstone_%s_eta%3.2f",partic,eta);
      f_pp_param = new TF1(title, func.Data(), 0., ptmax);
      f_pp_param->SetParameter(0,scale);
      f_pp_param->SetParameter(13,z);

      double akl = 0.;
      int i = 1;
      for (int k=0; k<4; k++)
	{
	  for (int l=0; l<3; l++)
	    {
	      if (partic_str.Contains("+", TString::kIgnoreCase)) akl = akl_plus[k][l];
	      if (partic_str.Contains("-", TString::kIgnoreCase)) akl = akl_minus[k][l];
	      cout << "<I> Setting index " << i << "(k,l)=(" << k << "," << l  
		   << ") to value: " << akl << endl;
	      f_pp_param->SetParameter(i,akl);
	      i++;
	    }
	}

      char label[200];
      sprintf(label,"Breakstone95 p+p #rightarrow h^{%s} #eta=%3.2f",partic, eta);
      entry = leg->AddEntry(f_pp_param, label, "L");

    }

  else if (type_str.Contains("dente", TString::kIgnoreCase) && (sqrts>=62.&&sqrts<=64) )
    {

      ptmin=1.5, ptmax=16.;

      f_pp_param = new TF1("f_denterria_pp_pi0_62", "[0]/(exp([2]*x*x+[3]*x)+x/[1])^[4]", ptmin, ptmax);
      //f_pp_param->SetParameters(scale*2.93419e+02,1.07127,1.74233e-02,-2.23236e-01,1.13465e+01); // idem force through highest alper75
      //f_pp_param->SetParameters(scale*288.53,2.39469,-1.11643e-02,5.33446e-02,1.67403e+01); // no force through alper75 + busser76 pichg
      //f_pp_param->SetParameters(scale*288.062,2.41324,-1.12472e-02,5.41171e-02,1.68118e+01); // idem + no busser73
      f_pp_param->SetParameters(scale*273.251,2.34601,-9.54848e-03,4.67200e-02,1.64203e+01); // no force through highest alper75 + busser76 pichg
      f_pp_param->SetName("f_denterria_pp_pi0_62");

      entry = leg->AddEntry(f_pp_param, "62 GeV D.d'E. #pi^{0} parametrization");
    }

  else if (type_str.Contains("phobos", TString::kIgnoreCase) && (sqrts>=62.&&sqrts<=64) )
    {
      ptmin=0., ptmax=5.;
      f_pp_param = new TF1("f_pp_param_phobos_62","[0]*(1.+x/[1])^[2]*0.99*x/sqrt(x*x+0.0085)",ptmin,ptmax);
      f_pp_param->SetParameters(scale*sigma_pp_inel(62.4)*43.231/twoPi,2.188,-15.37);
      f_pp_param->SetName("f_pp_param_phobos_62");

      entry = leg->AddEntry(f_pp_param, "62 GeV PHOBOS h^{#pm} parametrization (#eta = 0.75)");
    }

  else if (type_str.Contains("star", TString::kIgnoreCase) && (sqrts>=62.&&sqrts<=64) )
    {
      ptmin=0., ptmax=15.;
      f_pp_param = new TF1("f_pp_param_star_62","[0]*(1.+x/[1])^[2]",ptmin,ptmax);
      f_pp_param->SetParameters(scale*twoPi*46.55,1.75,-13.23);

      f_pp_param->SetName("f_pp_param_star_62");

      entry = leg->AddEntry(f_pp_param, "62 GeV STAR h^{#pm} parametrization (#eta = 0.)");
    }

  else if (type_str.Contains("compete", TString::kIgnoreCase) )
    {
      double sqrtsmin=5., sqrtsmax=5500.; 
      f_pp_param = new TF1("f_pp_sigmatot_param_compete","[0]*pow(x*x,[1])+[2]+[3]*pow(log(x*x/[4]),2.)",sqrtsmin,sqrtsmax);
      f_pp_param->SetParameters(scale*42.6,-0.46,35.5,0.307,29.1);
      f_pp_param->SetName("f_pp_sigma_tot_param_compete");

      entry = leg->AddEntry(f_pp_param, "COMPETE p+p sigma total parametrization");
    }

  else if (type_str.Contains("soft_pp", TString::kIgnoreCase) )
    {
      sqrts = 200.;
      ptmin=0., ptmax=15.;
      double Asoftpp = 2.73030e+02, eAsoftpp = 5.54704e+01;
      double Tsoftpp = -1.78246e-01, eTsoftpp = -1.16992e-02;
      f_pp_param = new TF1("f_pp_soft_200", "[0]*exp(x/[1])", ptmin, ptmax);
      f_pp_param->SetParameters(scale*Asoftpp,Tsoftpp);
      f_pp_param->SetName("f_pp_soft_200");

      entry = leg->AddEntry(f_pp_param, "Soft p+p dsigma/dpT exponential parametrization");
    }

  else if (type_str.Contains("soft_AuAu", TString::kIgnoreCase) )
    {
      sqrts = 200.;
      ptmin=0., ptmax=0.7;
      double Asoft = 200;
      double Tsoft = -2.;
      f_pp_param = new TF1("f_AuAu_soft_200", "[0]*exp(x/[1])", ptmin, ptmax);
      f_pp_param->SetParameters(scale*Asoft,Tsoft);
      f_pp_param->SetName("f_AuAu_soft_200");

      entry = leg->AddEntry(f_pp_param, "Soft Au+Au dsigma/dpT exponential parametrization");

      char temp[100];
      sprintf(temp,"pion_final_cent_%s.txt",partic);
      //keepPlot(false);
      TGraphErrors *eband;
      TGraphErrors *tmp =(TGraphErrors*)plot_spectrum_from_ascii(temp,eband);
      tmp->Fit(f_pp_param,"Q+","",ptmin, ptmax);
    }

  else if (type_str.Contains("run3_pi0", TString::kIgnoreCase) && (sqrts>=200.&&sqrts<=200.))
    {

      // A [mbarn GeV 2c3] = 229.6 ± 43.5 
      // p0 [GeV/c] = 1.466 ± 0.118 
      // m = 10.654 ± 0.350 
      // B [mbarn GeV 2c3] = 14.43 ± 1.32 
      // n = 8.1028 ± 0.0519 
      // t [GeV/c] = 4.5 
      // w [GeV/c] = 0.114

      double A  = 229.6;
      double p0 = 1.466;
      double m = 10.654;
      double B = 14.43;
      double n = 8.1028;
      double t = 4.5 ;
      double w = 0.114;

      TString func = "[0]/(1+exp((x-[1])/[2]))*[3]/pow( 1+x/[4], [5])"; // Woods-Saxon * mod.-powlaw
      func += "+ [0]*(1 - 1/(1+exp((x-[1])/[2])))*[6]/pow(x,[7])"; // (1 - Woods-Saxon) * powlaw

      ptmax = sqrts/2.;

      f_pp_param = new TF1("f_pp_run3_pi0", func.Data(), 0., ptmax);
      f_pp_param->SetParameters(scale,t,w,A,p0,m,B,n);

      entry = leg->AddEntry(f_pp_param, "Run-3 p+p #rightarrow #pi^{0}", "L");

    }

  f_pp_param->SetLineColor(4);
  f_pp_param->Draw("");

  if (fXaxis.Contains("xT", TString::kExact)) // xT axis instead of pT
    {
      double pt2xt = 2./sqrts;
      f_pp_param->GetHistogram()->GetXaxis()->SetLimits(pt2xt*ptmin, pt2xt*ptmax);
      f_pp_param->GetHistogram()->Draw("");
      c4->SetLogx();
    }

  leg->Draw();

  c4->Update();

  if ( !fPlot )
    {
      c4->Close();
    }

  return f_pp_param;
}

//_____________________________________________________________________________
// Returns Axel Drees interpolated pp spectrum at 130 GeV
//
TH1*
emcAnalyzer::plot_interpolated_pp_spectrum_130( const double scale )
{

  TH1 *h_pp_130 = 0;

  char name[300];
  sprintf(name, "interpolated_pp_spec_130");

  TCanvas *c4 = new TCanvas(name, name, 600, 600);
  //fCanvasList->Add(c4);
  c4->SetLogy();
  c4->cd();

  double ptmin = 1.;
  double ptmax = 5.;
  double A = scale*330.; // mb/(GeV/c)^2
  double p0 = 1.72;
  double n = 12.4;
  double sqrt_s = 130.;
  double pt2xt = 2./sqrt_s;

  char textfit[300];
  int verbose = 0;

  TF1 *haged = (TF1*) hagedornInit("haged", ptmin, ptmax, verbose, A, p0, n);
  haged->Draw("");

  if (fXaxis.Contains("xT", TString::kExact)) // xT axis instead of pT
    {
      haged->GetHistogram()->GetXaxis()->SetLimits(pt2xt*ptmin, pt2xt*ptmax);
      haged->GetHistogram()->Draw("");
      c4->SetLogx();
    }

  TPaveText *pave = new TPaveText(0.59, 0.56, 0.90, 0.88, "brNDC");
  pave->SetFillColor(0);
  pave->SetTextColor(1);
  TText *text = pave->AddText("130 GeV Power-law fit:");
  sprintf(textfit, "A_{Hag} = %.2f", A);
  text = pave->AddText(textfit);
  sprintf(textfit, "p_{0} = %.2f", p0);
  text = pave->AddText(textfit);
  sprintf(textfit, "n = %.2f", n);
  text = pave->AddText(textfit);
  pave->Draw();

  h_pp_130 = (TH1*)haged->GetHistogram();
  h_pp_130->GetListOfFunctions()->Add(pave);

  c4->Update();

  if ( !fPlot )
    {
      c4->Close();
    }

  return h_pp_130;
}

//_____________________________________________________________________________
//
void
emcAnalyzer::plot_xT_scaling_130_200()
{

  TCanvas *c21 = 0;

  //   TH1F *pp_130 = 0;
  //   TGraphErrors *pp_200 = 0;

  TGraphErrors *emcal_200_cent = 0;
  TGraphErrors *emcal_130_cent = 0;
  TGraphErrors *emcal_200_periph = 0;
  TGraphErrors *emcal_130_periph = 0;

  char title[50];
  sprintf(title, "pi0_xT_scaling_200_130");
  c21 = (TCanvas*)canvas(title, 700, 700);
  c21->Range( -2.21568, -8.29572, -0.807725, 1.68179);

  TH2F *myframe = (TH2F*)frame(title, 100, 0.009, 0.15, 100, 5E-08, 35,
                               "x_{T}=2p_{T}/#sqrt{s_{NN}}",
                               "(#sqrt{s_{NN}}/200)^{n=6.3} d^{2}N^{#pi^{0}}/2#pi dp_{T}dy");
  myframe->Draw();
  c21->SetLogy();

  fXaxis = "xT";
  fPlot = false;
  fFit = false;
  c21->SetLogx();

  c21->Update();

  TString label = "  Central #pi^{0} spectra";
  //TLegend *legend = new TLegend(0.409548,0.708589,0.90,0.90,label,"brNDC");
  TLegend *legend = new TLegend(0.366379, 0.797546, 0.979885, 0.976994, NULL, "brNDC");
  //TLegend *legend = new TLegend(0.59,0.51,0.90,0.88,label,"brNDC");
  legend->SetMargin(0.1);
  legend->SetFillStyle(0);
  legend->SetBorderSize(3);
  legend->SetTextSize(0.034);
  legend->Draw();
  char sublabel_130c[50];
  char sublabel_130p[50];
  char sublabel_200c[50];
  char sublabel_200p[50];

  //   char sublabel_130[50]; char sublabel_200[50];

  double scaling = pow((130./200.), 6.3);

  // p+p ->pi0
  //   pp_200 = (TGraphErrors*)plot_spectrum_from_ascii("pi0_pp_emcal_200GeV.txt",0); // pp 200 GeV
  //   pp_200->SetMarkerStyle(29);
  //   pp_200->SetMarkerSize(1.);
  //   pp_200->SetMarkerColor(1);
  //   c21->cd();
  //   pp_200->Draw("P");
  //   sprintf(sublabel_200,"pp #pi^{0} @ 200 GeV");
  //   c21->Update();

  //   pp_130 = (TH1F*)plot_interpolated_pp_spectrum_130(1.,false); // pp 130 GeV
  //   pp_130->Scale(scaling);
  //   pp_130->SetMarkerStyle(29);
  //   pp_130->SetMarkerSize(1.);
  //   pp_130->SetMarkerColor(1);
  //   c21->cd();
  //   pp_130->Draw("Psame");
  //   sprintf(sublabel_130,"pp #pi^{0} @ 130 GeV");
  //   c21->Update();

  // CENTRAL

  emcal_200_cent = (TGraphErrors*)plot_spectrum_from_ascii(0, 10, "emcal"); // 0-10% 200 GeV
  emcal_200_cent->SetMarkerStyle(21);
  emcal_200_cent->SetMarkerSize(1.3);
  emcal_200_cent->SetMarkerColor(4);
  c21->cd();
  emcal_200_cent->Draw("P");
  sprintf(sublabel_200c, "AuAu #pi^{0} @ 200 GeV [0-10%%]");
  c21->Update();

  TGraphErrors *errBand;
  emcal_130_cent = (TGraphErrors*)plot_spectrum_from_ascii("PizeroAuAu130_cent.dat", (TGraphErrors*&)errBand); // 0-10% 130 GeV
  scale(*emcal_130_cent, scaling, 0, fVerbose);
  emcal_130_cent->SetMarkerStyle(21);
  emcal_130_cent->SetMarkerSize(1.3);
  emcal_130_cent->SetMarkerColor(2);
  c21->cd();
  emcal_130_cent->Draw("P");
  sprintf(sublabel_130c, "AuAu #pi^{0} @ 130 GeV [0-10%%]");
  c21->Update();

  // PERIPH
  c21->cd();
  emcal_200_periph = (TGraphErrors*)plot_spectrum_from_ascii(60, 80, "emcal"); // 60-80%
  emcal_200_periph->SetMarkerStyle(20);
  emcal_200_periph->SetMarkerSize(1.3);
  emcal_200_periph->SetMarkerColor(4);
  c21->cd();
  emcal_200_periph->Draw("P");
  sprintf(sublabel_200p, "AuAu #pi^{0} @ 200 GeV [60-80%%]");
  c21->Update();

  emcal_130_periph = (TGraphErrors*)plot_spectrum_from_ascii("PizeroAuAu130_periph.dat", (TGraphErrors*&)errBand); //60-80% 130GeV
  scale(*emcal_130_periph, scaling, 0, fVerbose);
  emcal_130_periph->SetMarkerStyle(20);
  emcal_130_periph->SetMarkerSize(1.3);
  emcal_130_periph->SetMarkerColor(2);
  c21->cd();
  emcal_130_periph->Draw("P");
  sprintf(sublabel_130p, "AuAu #pi^{0} @ 130 GeV [60-80%%]");

  legend->AddEntry(emcal_130_cent, sublabel_130c, "P");
  legend->AddEntry(emcal_130_periph, sublabel_130p, "P");
  //legend->AddEntry(pp_130, sublabel_130, "P");

  legend->AddEntry(emcal_200_cent, sublabel_200c, "P");
  legend->AddEntry(emcal_200_periph, sublabel_200p, "P");
  //legend->AddEntry(pp_200, sublabel_200, "P");
  c21->Update();

  //   // Ratios now ...
  //   TH1F *h_xT_130_cent = new TH1F("h_xT_130_cent","h_xT_130_cent",100,0,1);
  //   TH1F *h_xT_200_cent = new TH1F("h_xT_200_cent","h_xT_200_cent",100,0,1);
  //   int points_130_cent = emcal_130->GetN();
  //   int points_200_cent = emcal_200->GetN();
  //   double xT_130_cent_xT[points_130_cent], xT_130_centYield[points_130_cent], exT_130_centYield[points_130_cent];
  //   double xT_200_cent_xT[points_200_cent], xT_200_centYield[points_200_cent], exT_200_centYield[points_200_cent];
  //   int binxT = 0;

  //   for (int i=0; i<points_130_cent;i++)
  //     {
  //       emcal_130->GetPoint(i,xT_130_cent_xT[i],xT_130_centYield[i]);
  //       exT_130_centYield[i] = emcal_130->GetErrorY(i);
  //       binxT = emcal_130->GetXaxis()->FindBin( xT_130_cent_xT[i] );
  //       h_xT_130_cent->SetBinContent(binxT,xT_130_centYield[i]);
  //       h_xT_130_cent->SetBinError(binxT,exT_130_centYield[i]);
  //     }
  //   for (int i=0; i<points_200_cent;i++)
  //     {
  //       emcal_200->GetPoint(i,xT_200_cent_xT[i],xT_200_centYield[i]);
  //       exT_200_centYield[i] = emcal_200->GetErrorY(i);
  //       binxT = emcal_200->GetXaxis()->FindBin( xT_200_cent_xT[i] );
  //       h_xT_200_cent->SetBinContent(binxT,xT_200_centYield[i]);
  //       h_xT_200_cent->SetBinError(binxT,exT_200_centYield[i]);
  //     }

  //   // Ratios now ...

  //   TH1F *h_xT_130_periph = new TH1F("h_xT_130_periph","h_xT_130_periph",100,0.009,0.15);
  //   TH1F *h_xT_200_periph = new TH1F("h_xT_200_periph","h_xT_200_periph",100,0.009,0.15);
  //   int points_130_periph = emcal_130->GetN();
  //   int points_200_periph = emcal_200->GetN();
  //   double xT_130_periph_xT[points_130_periph], xT_130_periphYield[points_130_periph], exT_130_periphYield[points_130_periph];
  //   double xT_200_periph_xT[points_200_periph], xT_200_periphYield[points_200_periph], exT_200_periphYield[points_200_periph];

  //   for (int i=0; i<points_130_periph;i++)
  //     {
  //       emcal_130->GetPoint(i,xT_130_periph_xT[i],xT_130_periphYield[i]);
  //       exT_130_periphYield[i] = emcal_130->GetErrorY(i);
  //       binxT = emcal_130->GetXaxis()->FindBin( xT_130_periph_xT[i] );
  //       h_xT_130_periph->SetBinContent(binxT,xT_130_periphYield[i]);
  //       h_xT_130_periph->SetBinError(binxT,exT_130_periphYield[i]);
  //       cout << "130: " << binxT << " " << xT_130_periphYield[i] << endl;
  //     }
  //   for (int i=0; i<points_200_periph;i++)
  //     {
  //       emcal_200->GetPoint(i,xT_200_periph_xT[i],xT_200_periphYield[i]);
  //       exT_200_periphYield[i] = emcal_200->GetErrorY(i);
  //       binxT = emcal_200->GetXaxis()->FindBin( xT_200_periph_xT[i] );
  //       h_xT_200_periph->SetBinContent(binxT,xT_200_periphYield[i]);
  //       h_xT_200_periph->SetBinError(binxT,exT_200_periphYield[i]);
  //       cout << "200: " << binxT << " " << xT_200_periphYield[i] << endl;
  //     }

  //   char title2[50];
  //   sprintf(title2,"ratio_xT");
  //   TCanvas *c22 = new TCanvas(title2,title2,700,700);
  //   c22->cd();
  //   TH2F *myframe2 = new TH2F(title2,title2,100,0.009,0.15,100,0,10);
  //   myframe2->SetYTitle("Ratio 200/130");
  //   myframe2->SetXTitle("x_{T}=2p_{T}/#sqrt{s_{NN}}");
  //   myframe2->Draw();
  //   c22->SetLogx();
  //   c22->Update();

  //   TH1F *ratio = 0;
  //   ratio = (TH1F*)h_xT_200_periph->Clone();
  //   ratio->SetTitle("ratio_xT_200_130");
  //   ratio->Divide(h_xT_130_periph);
  //   ratio->Draw("e");
  //   c22->Update();

  //   h_xT_200_periph->Draw("same");
  //   h_xT_130_periph->Draw("same");
  //   c22->Update();

}

//_____________________________________________________________________________
//
void
emcAnalyzer::plot_comparison_spectra( const char* file1,
                                      const char* file2,
                                      const char* file3,
                                      const double scale1,
                                      const double scale2,
                                      const double scale3,
                                      const char* Xtitle,
                                      const char* Ytitle,
                                      const char* title )
{

  // fixme: should recover the normalization errors too ...
  TGraphErrors *errBand;

  TGraphErrors *one = (TGraphErrors*)plot_spectrum_from_ascii((char*)file1, (TGraphErrors*&)errBand);
  TGraphErrors *two = (TGraphErrors*)plot_spectrum_from_ascii((char*)file2, (TGraphErrors*&)errBand);
  if ( one == 0 || two == 0) return ;

  TGraphErrors *three = 0;
  if (file3) three = (TGraphErrors*)plot_spectrum_from_ascii((char*)file3, (TGraphErrors*&)errBand);

  if (scale1 != 1) scale(*one, scale1, 0, fVerbose);
  if (scale2 != 1) scale(*two, scale2, 0, fVerbose);
  if (scale3 != 1 && three ) scale(*three, scale3, 0, fVerbose);

  double ptmin = 0.;
  double ptmax = 10.5;
  double Ymin = 0.;
  double Ymax = 30.;
  getXYminmax( one, two, ptmin, ptmax, Ymin, Ymax);
  if (three) getXYminmax( one, three, ptmin, ptmax, Ymin, Ymax);
  ptmax += 1.; // additional 1 GeV/c at high pT
  ptmin = (ptmin > 0.) ? 0. : ptmin; // start at 0. GeV/c always
  int ptbins = (int)(ptmax - ptmin)*2;

  if (!Ymin)
    Ymin = 1.e-10; // if Ymin == 0.

  TCanvas *c20 = (TCanvas*)canvas(title, 650, 650);
  c20->Range( -1.48766, -8.33765, 10.9932, 1.69253);
  c20->SetLogy();

  TH2F *myframe = (TH2F*)frame(title, ptbins, 0., ptmax, 20, Ymin/10., Ymax*10., Xtitle, Ytitle);
  //  TH2F *myframe = (TH2F*)frame(title,21,0.,10.5,100,5E-08,35);
  myframe->Draw();

  c20->Update();
  c20->cd();

  TLegend *legend = new TLegend(0.59, 0.56, 0.90, 0.88, NULL, "brNDC");
  legend->SetMargin(0.1);
  legend->SetFillStyle(0);
  legend->SetBorderSize(1);
  legend->SetTextSize(0.042);

  legend->AddEntry(one, one->GetTitle(), "P");
  legend->AddEntry(two, two->GetTitle(), "P");

  setMarkerLineType(one, 20, 4, 1.2);
  one->Draw("P");
  setMarkerLineType(two, 20, 2, 1.2);
  two->Draw("P");
  if (three)
    {
      setMarkerLineType(three, 20, 8, 1.2);
      legend->AddEntry(three, three->GetTitle(), "P");
      three->Draw("P");
    }
  legend->Draw();

  TPaveStats *st = (TPaveStats*)one->GetListOfFunctions()->FindObject("stats");
  if (st)
    {
      double y1 = st->GetY1NDC();
      double y2 = st->GetY2NDC();
      TPaveStats *st2 = (TPaveStats*)two->GetListOfFunctions()->FindObject("stats");
      if (st2)
        {
          double newy1 = y1 - Abs(y1 - y2); // right below
          st2->SetY1NDC(newy1); //'two' y start position
          st2->SetY2NDC(y1);    //'two' y end position = y start position of 'one'
        }
    }

  c20->Update();

  saveCanvas(c20);

}

//_____________________________________________________________________________
//

void
emcAnalyzer::plot_comparison_AuAu_pp( const int Cent1,
                                      const int Cent2,
                                      const char *cut)
{

  fPlot = true;
  fFit = false;

  TString cut_str = cut ;

  TGraphErrors *errBand;
  TGraphErrors *AuAu = 0;

  if ( cut_str.Contains("emcal") ) // PbGl+PbSc
    {
      char file[300];
      sprintf(file, "pi0_emcal_%d_%d_%s.txt", Cent1, Cent2, favoritecut);
      AuAu = (TGraphErrors*)plot_spectrum_from_ascii((char*)file, (TGraphErrors*&)errBand);
    }
  else
    {
      AuAu = (TGraphErrors*)plot_spectrum(Cent1, Cent2, cut, "full", 1);
    }

  //TGraphErrors *pp = (TGraphErrors*)plot_spectrum_from_ascii("pi0_pp_emcal_200GeV_final_cross_sasha.txt", (TGraphErrors*&)errBand);
  TGraphErrors *pp =  (TGraphErrors*)plot_spectrum_from_ascii("pi0_pp_emcal_200GeV_run3.txt",errBand);
  //double abs_sigma = 20.5/0.785;
  //scale(*pp,abs_sigma);

  //TGraphErrors *dAu = (TGraphErrors*)plot_spectrum_from_ascii("pi0_emcal_dAu_minbias.txt",(TGraphErrors*&)errBand);

  TGraphErrors *pQCD1 = (TGraphErrors*)
    plot_spectrum_from_ascii("PionRHIC200_pQCD_EKSNoLossQF1.0.out.data", (TGraphErrors*&)errBand); //,"pQCD1",(TString*)"pQCD1");
  TGraphErrors *pQCD2 = (TGraphErrors*)
    plot_spectrum_from_ascii("PionRHIC200_pQCD_EKSNoLossQF2.0.out.data", (TGraphErrors*&)errBand); //,"pQCD2",(TString*)"pQCD2");

  double pp_scale = getTAB(Cent1, Cent2);

  // Global normalization errors added to error band:
  // pp abs.norm. error (9.6%), pp accept. error (3.8%), AuAu accept. error (5.0%)

  double pp_scale_relat_err = getTAB(Cent1, Cent2, "error")/pp_scale;
  //double pp_scale_relat_err = getNcoll(Cent1,Cent2,"error")/getNcoll(Cent1,Cent2);
  double pp_abs_yield_erelat = 0.096; // Additional 9.6% error on pp yield (BBC efficiency + luminosity): AN184
  double FiduDeadWarnPlusAccRelatError = 0.05; // /Sqrt(2.) ;
  double ppAcceptanceError = 0.038;

  pp_scale_relat_err = quadRelatError( pp_scale_relat_err, 1.,   // T_AB error
                                       pp_abs_yield_erelat, 1.,  // pp BBC,sigma normalization
                                       ppAcceptanceError, 1.,    // pp acceptance
                                       FiduDeadWarnPlusAccRelatError, 1.); // AuAu acceptance

  cout << " <I> The error in the band is: " << pp_scale_relat_err*100 << "%" << endl;

  scale(*pp, pp_scale, 0, fVerbose);
  scale(*pQCD1, pp_scale, 0, fVerbose);
  scale(*pQCD2, pp_scale, 0, fVerbose);

  TClonesArray* errBandpp = (TClonesArray*)errorBand( pp, pp_scale_relat_err );

  TGraph *ppshade = (TGraph*)(*errBandpp)[0];
  TGraph *ppmin = (TGraph*)(*errBandpp)[1];
  TGraph *ppmax = (TGraph*)(*errBandpp)[2];

  char title[200] ;
  sprintf(title, "pi0_comparison_AuAu_pp_%d_%d_%s", Cent1, Cent2, cut);
  char ytitle[] = "1/[2#pi p_{T}] d^{2}N/dp_{ T}dy (GeV/#font[72]{c})^{-2}";
  char xtitle[] = "p_{T} (GeV/#font[72]{c})";

  double ptmin = 0.;
  double ptmax = 10.5;
  double Ymin = 0.;
  double Ymax = 30.;
  getXYminmax( AuAu, pp, ptmin, ptmax, Ymin, Ymax);
  ptmax += 1.; // additional 1 GeV/c at high pT
  ptmin = (ptmin > 0.) ? 0. : ptmin; // start at 0. GeV/c always
  int ptbins = (int)(ptmax - ptmin)*2;

  if (!Ymin)
    Ymin = 1.e-10; // if Ymin == 0.

  TCanvas *c20 = (TCanvas*)canvas(title, 650, 650);
  c20->Range( -1.48766, -8.33765, 10.9932, 1.69253);
  c20->SetLogy();

  c20->SetLogx();
  ptmin = 0.2;
  ptmax = 20.5;

  TH2F *myframe = (TH2F*)frame(title, ptbins, 0., ptmax, 20, Ymin/10., Ymax*10., xtitle, ytitle);
  //  TH2F *myframe = (TH2F*)frame(title,21,0.,10.5,100,5E-08,35);
  myframe->Draw();

  c20->cd();
  setMarkerLineType(pQCD1, 21, 12, 1.3, 1, 12, 4);
  setMarkerLineType(pQCD2, 21, 12, 1.3, 1, 12, 4);
  pQCD1->Draw("l");
  //pQCD2->Draw("l");

  ppshade->Draw("f");
  ppmin->Draw("l");
  ppmax->Draw("l");

  c20->Update();
  c20->cd();

  TLegend *legend = new TLegend(0.25, 0.834, 0.90, 0.96, NULL, "brNDC");
  legend->SetMargin(0.1);
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);
  legend->SetTextSize(0.034);

  sprintf(title, "pp #rightarrow #pi^{0}X @ 200 GeV (N_{coll}[%d-%d%%] scaled)", Cent1, Cent2);
  TLegendEntry *entry = legend->AddEntry(pp, title, "P");
  entry->SetTextAlign(22);
  entry->SetTextColor(4);
  sprintf(title, "AuAu #rightarrow #pi^{0}X @ 200 GeV [%d-%d%%]               ", Cent1, Cent2);
  entry = legend->AddEntry(AuAu, title, "P");
  entry->SetTextAlign(22);
  entry->SetTextColor(2);
  sprintf(title, "NLO pQCD, EKS nPDF, Q_{F} = p_{T} [I.Sarcevic et al.] ");
  entry = legend->AddEntry(pQCD1, title, "P");
  //entry->SetTextSize(0.034);
  entry->SetTextAlign(22);
  //entry->SetLineWidth(0.5);
  entry->SetTextColor(1);
  //sprintf(title,"NLO pQCD, EKS nPDF, Q_{F} = 2p_{T} (I.Sarcevic et al.) ");
  //entry = legend->AddEntry(pQCD2, title, "P");
  //entry->SetTextAlign(22);
  //entry->SetTextColor(4);

  setMarkerLineType(pp, 20, 4, 1.5, 1, 4);
  pp->Draw("P");
  setMarkerLineType(AuAu, 29, 2, 2.3, 1, 2);
  AuAu->Draw("P");

  legend->Draw();

  c20->Update();

  saveCanvas(c20);

}

//_____________________________________________________________________________
//
void
emcAnalyzer::plot_comparison_pbsc_pbgl( const int Cent1,
                                        const int Cent2,
                                        const char*cut,
                                        const bool correctedyields)
{

  TString cut_str = cut;

  char title[200] ;
  char ytitle[] = "1/[2#pi p_{T}] d^{2}N/dp_{ T}dy (GeV/#font[72]{c})^{-2}";
  char xtitle[] = "p_{T} (GeV/#font[72]{c})";
  char pbsc_file[300];
  char pbgl_file[300];

  if ( !cut_str.Contains("pp") ) // AuAu
    {
      sprintf(title, "pi0_spectra_pbsc_pbgl_comparison_%d_%d_%s", Cent1, Cent2, cut);

      if (!correctedyields) // bin shift uncorrected
        {
          sprintf(pbsc_file, "pi0_pbsc_%d_%d_%s_noshift.txt", Cent1, Cent2, cut);
          sprintf(pbgl_file, "pbgl_year2_FitP3Cor2_%d-%d.dat", Cent1, Cent2);
        }
      else // bin shift corrected
        {
          sprintf(pbsc_file, "pi0_pbsc_%d_%d_%s.txt", Cent1, Cent2, cut);
          sprintf(pbgl_file, "pbgl_year2_FitP3Cor4_%d-%d.dat", Cent1, Cent2);
        }

      //sprintf(pbgl_file,"pi0_ms_pbgl_%d-%d.dat",Cent1,Cent2); // QM02
    }
  else // pp
    {
      sprintf(title, "pi0_comparison_pbsc_pbgl_pp");

      sprintf(pbsc_file, "%s/pp_tables/pi0_pp_pbsc_200GeV_cross.txt", fDataTablesPath->Data());
      //sprintf(pbgl_file,"%s/pp_tables/pi0_pp_pbgl_shifty_fixbin_jan_14_2003.txt",fDataTablesPath->Data());
      sprintf(pbgl_file, "%s/pp_tables/pi0_pp_pbgl_200GeV_yields.txt", fDataTablesPath->Data());
    }

  plot_comparison_spectra(pbsc_file, pbgl_file, 0, 0, 0, 0, xtitle, ytitle, title);

  TString label("EMCal #pi^{0} spectra: ");
  TLegend *legend = new TLegend(0.59, 0.56, 0.90, 0.88, label, "brNDC");
  legend->SetMargin(0.1);
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);
  legend->SetTextSize(0.042);
  //legend->Draw();
  char sublabel[50];

  sprintf(sublabel, "PbSc 0-10%%");
  //  legend->AddEntry(pbsc, sublabel, "P");

  sprintf(sublabel, "PbGl 0-10%%");
  //  legend->AddEntry(pbgl, sublabel, "P");

}

//_____________________________________________________________________________
//
void
emcAnalyzer::plot_comparison_qm_new( const int Cent1,
                                     const int Cent2,
                                     const char* cut )
{

  char title[200];
  sprintf(title, "comparison_qm_new_%d_%d_%s", Cent1, Cent2, cut);
  char ytitle[200];
  sprintf(ytitle, "1/[2#pi p_{T}] d^{2}N/dp_{ T}dy (GeV/#font[72]{c})^{-2}");
  char xtitle[] = "p_{T} (GeV/#font[72]{c})";

  char filenew[200];
  char fileqm[200];

  TString cut_str = cut ;
  if ( cut_str.Contains("pbgl") ) // pbgl
    {
      //sprintf(filenew,"pbgl_year2_FitP3Cor2_%d-%d.dat",Cent1,Cent2); // bin shift UNcorrected
      sprintf(filenew, "pbgl_year2_FitP3Cor4_%d-%d.dat", Cent1, Cent2);
      sprintf(fileqm, "pi0_ms_pbgl_%d-%d.dat", Cent1, Cent2);
    }
  else // pbsc
    {
      sprintf(filenew, "pi0_pbsc_%d_%d_%s_ptshift.txt", Cent1, Cent2, cut);
      sprintf(fileqm, "pi0_pbsc_%d_%d_QM02.txt", Cent1, Cent2);
    }

  plot_comparison_spectra(filenew, fileqm, 0, 0, 0, 0, xtitle, ytitle, title);
  //setMarker(ratio1_2,Cent1,Cent2);

}

//_____________________________________________________________________________
//
TGraphErrors*
emcAnalyzer::plot_ratio_spectra( const char* file1,
                                 const char* file2,
                                 const char* Xtitle,
                                 const char* Ytitle,
                                 const char* title,
                                 const bool ratiofits )
{

  TGraphErrors *ratio1_2 = 0;
  TGraphErrors *errBand;

  TGraphErrors *one = (TGraphErrors*)plot_spectrum_from_ascii((char*)file1, (TGraphErrors*&)errBand);
  TGraphErrors *two = (TGraphErrors*)plot_spectrum_from_ascii((char*)file2, (TGraphErrors*&)errBand);
  if ( one == 0 || two == 0) return ratio1_2;

  double ptmin = 0.;
  double ptmax = 10.5;
  double Ymin = 0.;
  double Ymax = 30.;
  getXYminmax( one, two, ptmin, ptmax, Ymin, Ymax);
  ptmax += 1.; // additional 1 GeV/c at high pT
  ptmin = (ptmin > 0.) ? 0. : ptmin; // start at 0. GeV/c always
  int ptbins = (int)(ptmax - ptmin)*2;

  if (!ratiofits)
    {
      ratio1_2 = (TGraphErrors*)ratio(one, two, fVerbose, 0.);
    }
  else
    {
      ratio1_2 = (TGraphErrors*)ratioFits(one, two, fVerbose);
    }
  if (!ratio1_2) return ratio1_2;

  double ratiomax; // = 3.;
  double ratiomin; // = 0.;
  getYminmax( ratio1_2, ratiomin, ratiomax); //, ptmin, ptmax);
  //ratiomin = (ratiomin > 0) ? 0. : ratiomin; //
  ratiomin = 0.; //
  ratiomax = (ratiomax < 2) ? 2. : ratiomax; //
  //ratiomax = (ratiomax > 3) ? 3. : ratiomax; //

  if (strcasecmp(title, "ratio_1_2") == 0) // default
    {
      TString title2 = file1;
      title2 += file2;
      title = title2.Data();
    }

  TCanvas *c10 = (TCanvas*)canvas(title, 600, 600);
  c10->Range( -0.99084, -0.234801, 9.01832, 2.07966);

  TH2F *myframe = (TH2F*)frame(title, ptbins, ptmin, ptmax, 20, ratiomin, ratiomax, Xtitle, Ytitle);
  myframe->Draw();

  TLine *line = new TLine(ptmin, 1., ptmax, 1.);
  line->SetLineColor(2);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw();

  c10->cd();
  setMarker(ratio1_2, 0, 100); //default marker ...
  ratio1_2->Draw("P");

  c10->Update();

  saveCanvas(c10);

  return ratio1_2;
}

//_____________________________________________________________________________
//
void
emcAnalyzer::plot_ratio_qm_new( const int Cent1,
                                const int Cent2,
                                const char* cut )
{

  char title[200];
  sprintf(title, "ratio_new_qm_%d_%d_%s", Cent1, Cent2, cut);
  char ytitle[200];
  sprintf(ytitle, "ratio new/QM02 (%d-%d%%)", Cent1, Cent2);
  char xtitle[] = "p_{T} (GeV/#font[72]{c})";

  char filenew[200];
  char fileqm[200];

  TString cut_str;
  if ( cut_str.Contains("pbgl") ) // pbgl
    {
      //sprintf(filenew,"pbgl_year2_FitP3Cor2_%d-%d.dat",Cent1,Cent2); // bin shift UNcorrected
      sprintf(filenew, "pbgl_year2_FitP3Cor4_%d-%d.dat", Cent1, Cent2);
      sprintf(fileqm, "pi0_ms_pbgl_%d-%d.dat", Cent1, Cent2);
    }
  else // pbsc
    {
      sprintf(filenew, "pi0_pbsc_%d_%d_%s_ptshift.txt", Cent1, Cent2, cut);
      sprintf(fileqm, "pi0_pbsc_%d_%d_QM02.txt", Cent1, Cent2);
    }

  plot_ratio_spectra(filenew, fileqm, xtitle, ytitle, title);

}
//_____________________________________________________________________________
//
// others = "pi" (charged pions/2), "pbar" (p+pbar)/2, "had" (unidentified hadrons)
// if "others" contains "3" (e.g. "pbar3") plot only: 0-10%, 20-30% and 80-92%.

void
emcAnalyzer::plot_ratio_pi0_others_all_cent( const char* others,
					     const char* cut )
{

  TString others_str = others;

  char title[200];
  sprintf(title, "ratio_pi0_%s_%s", others, cut);

  //_____________________________________________________________________________

  TCanvas *c10 = (TCanvas*)canvas(title, 600, 600);
  c10->Range( -53.3708, -0.370851, 379.213, 2.77853);

  double ptmin = 0.;
  double ptmax = 5.;

  TH2F *myframe = (TH2F*)frame(title, 20, ptmin, ptmax, 10, 0., 1.5, "p_{T} (GeV/#font[72]{c})", "ratio [(p+#bar{p})/2]/#pi^{0}");
  myframe->Draw();
  c10->Update();

  TLegend *leg2 = new TLegend(0.105369, 0.809783, 0.480872, 0.95471, NULL, "brNDC");
  leg2->SetMargin(0.1);
  leg2->SetLineColor(1);
  leg2->SetLineStyle(1);
  leg2->SetLineWidth(1);
  leg2->SetFillStyle(0);
  leg2->SetBorderSize(0);
  leg2->SetTextSize(0.034);

  //_____________________________________________________________________________
  // loop on centralities
  int Cent1 = 0;
  int Cent2 = 10;

  TGraphErrors *ratio[10];
  TLegendEntry *entry = 0;
  char text[100];

  if ( others_str.Contains("3", TString::kIgnoreCase) )
    {
      getCentralityClassLimits2( 0, Cent1, Cent2);  // 0-10%
      ratio[0] = (TGraphErrors*)plot_ratio_pi0_others(Cent1, Cent2, others, cut);
      c10->cd();
      setMarker(ratio[0], Cent1, Cent2);
      //sprintf(text,"Au+Au (%d-%d%) PHENIX 200 GeV",Cent1,Cent2)
      entry = leg2->AddEntry(ratio[0], "Au+Au (0-10%) PHENIX 200 GeV", "P");
      ratio[0]->Draw("P");

      getCentralityClassLimits2( 4, Cent1, Cent2);  // 20-30%
      ratio[1] = (TGraphErrors*)plot_ratio_pi0_others(Cent1, Cent2, others, cut);
      c10->cd();
      setMarker(ratio[1], Cent1, Cent2);
      //sprintf(text,"Au+Au (%d-%d%) PHENIX 200 GeV",Cent1,Cent2)
      entry = leg2->AddEntry(ratio[1], "Au+Au (20-30%) PHENIX 200 GeV", "P");
      ratio[1]->Draw("P");

      //getCentralityClassLimits2( 16, Cent1, Cent2); // 80-92%
      getCentralityClassLimits2( 18, Cent1, Cent2); // 60-92% (or 70-92%)
      ratio[2] = (TGraphErrors*)plot_ratio_pi0_others(Cent1, Cent2, others, cut);
      c10->cd();
      setMarker(ratio[2], Cent1, Cent2);
      sprintf(text, "Au+Au (%d-%d%%) PHENIX 200 GeV", Cent1, Cent2);
      entry = leg2->AddEntry(ratio[2], text, "P");
      ratio[2]->Draw("P");
    }
  else
    {
      for (int i = 0; i <= CentClasses; i+=2)
        {
          getCentralityClassLimits2( i, Cent1, Cent2);
          ratio[i] = (TGraphErrors*)plot_ratio_pi0_others(Cent1, Cent2, others, cut);
        }
    }

  // p/pi ratios: DELPHI e+e- and ISR p+p

  TGraphErrors *isr_ppi = 0;
  TGraphErrors *delphi_ppi_q = 0;
  TGraphErrors *delphi_ppi_g = 0;

  fPlot = true;
  fFit = false;

  TGraphErrors *errBand;

  if ( others_str.Contains("pbar", TString::kIgnoreCase) )
    {
      isr_ppi = (TGraphErrors*)plot_spectrum_from_ascii("ppbar_over_pi0_pp_isr_53GeV.txt", (TGraphErrors*&)errBand);
      delphi_ppi_g = (TGraphErrors*)plot_spectrum_from_ascii("ppbar_over_chgdpi_gluonjets_DELPHI.txt", (TGraphErrors*&)errBand);
      delphi_ppi_q = (TGraphErrors*)plot_spectrum_from_ascii("ppbar_over_chgdpi_quarkjets_DELPHI.txt", (TGraphErrors*&)errBand);

      c10->cd();
      isr_ppi->Draw("PL");
      delphi_ppi_g->Draw("L");
      delphi_ppi_g->SetLineStyle(3);
      delphi_ppi_q->Draw("L");
      delphi_ppi_q->SetLineStyle(2);

      entry = leg2->AddEntry(isr_ppi, "p+p ISR 53 GeV", "P");
      entry = leg2->AddEntry(delphi_ppi_g, "e^{+}e^{-} gluon jets DELPHI Z decays", "L");
      entry = leg2->AddEntry(delphi_ppi_q, "e^{+}e^{-} quark jets DELPHI Z decays", "L");
    }

  c10->cd();
  leg2->Draw("same");

  TLine *line = new TLine(ptmin, 1., ptmax, 1.);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw("same");

  c10->Update();

}

//_____________________________________________________________________________
//
TGraphErrors*
emcAnalyzer::plot_ratio_pi0_others( const int Cent1, const int Cent2,
                                    const char* others,
                                    const char* cut )
{

  TGraphErrors *ratio = 0;

  char fileothers[200];
  char title[200];
  char ytitle[200];
  char xtitle[] = "p_{T} (GeV/#font[72]{c})";

  TString others_str = others;
  TString cut_str = cut;

  char filepi0[200];

  //_____________________________________________________________________________
  // charged pions or hadrons

  if ( others_str.Contains("pi") )  // others = "pi" (charged pions)
    {
      sprintf(fileothers, "pion_final_cent_%d_%d.txt", Cent1, Cent2);
      sprintf(ytitle, "ratio #pi^{0}/[(#pi^{+}+#pi^{-})/2]");
      sprintf(title, "ratio_pi0_%s_chargedpion", cut);
    }
  else if ( others_str.Contains("pbar") )  // (p+pbar)/2
    {
      sprintf(fileothers, "pbarp_final_feed_%d_%d.dat", Cent1, Cent2);
      sprintf(ytitle, "ratio [(p+#bar{p})/2]/#pi^{0}");
      sprintf(title, "ratio_ppbar_pi0_%s", cut);
    }
  else // if ( others_str.Contains("hadrons") )  // others = "had" (charged hadrons)
    {
      sprintf(fileothers, "chargedrebin_%d_%d.txt", Cent1, Cent2);
      sprintf(ytitle, "ratio [(h^{+}+h^{-})/2]/#pi^{0}");
      sprintf(title, "ratio_chargedhadrons_pi0_%s", cut);
    }

  //_____________________________________________________________________________
  // pi0

  if ( cut_str.Contains("emcal") )
    {
      sprintf(filepi0, "pi0_emcal_%d_%d_tof1chisq1.txt", Cent1, Cent2);
    }
  else if ( cut_str.Contains("pbgl") ) // pbgl
    {
      //sprintf(filepi0,"pbgl_year2_FitP3Cor2_%d-%d.dat",Cent1,Cent2); // bin shift UNcorrected
      sprintf(filepi0, "pbgl_year2_FitP3Cor4_%d-%d.dat", Cent1, Cent2);
    }
  else // pbsc cut
    {
      sprintf(filepi0, "pi0_pbsc_%d_%d_%s.txt", Cent1, Cent2, cut);
    }

  sprintf(title, "%s_%d_%d", title, Cent1, Cent2);
  sprintf(ytitle, "%s (%d-%d%%)", ytitle, Cent1, Cent2);

  //_____________________________________________________________________________
  // ratio

  if ( others_str.Contains("pi") )
    {
      ratio = (TGraphErrors*)plot_ratio_spectra(filepi0, fileothers, xtitle, ytitle, title); // pi0 in numerator
    }
  else // "ppbar" or "had" // if ( others_str.Contains("hadrons") )
    {
      ratio = (TGraphErrors*)plot_ratio_spectra(fileothers, filepi0, xtitle, ytitle, title); // pi0 in denominator
    }

  return ratio;
}

//_____________________________________________________________________________
//
void
emcAnalyzer::plot_ratio_pbsc_pbgl( const int Cent1,
                                   const int Cent2,
                                   const char*cut,
                                   const bool correctedyields )
{

  TString cut_str = cut;

  char title[200];
  char ytitle[200];
  char xtitle[] = "p_{T} (GeV/#font[72]{c})";
  char pbsc_file[300];
  char pbgl_file[300];

  if ( !cut_str.Contains("pp") ) // AuAu
    {
      sprintf(title, "ratio_pbsc_pbgl_%d_%d_%s", Cent1, Cent2, cut);
      if (!correctedyields)
        {
          sprintf(pbsc_file, "pi0_pbsc_%d_%d_%s_noshift.txt", Cent1, Cent2, cut);
          sprintf(pbgl_file, "pbgl_year2_FitP3Cor2_%d-%d.dat", Cent1, Cent2);    // bin shift UNcorrected
          sprintf(ytitle, "ratio (bin shift uncorrected) PbSc/PbGl (%d-%d%%)", Cent1, Cent2);
        }
      else
        {
          sprintf(pbsc_file, "pi0_pbsc_%d_%d_%s.txt", Cent1, Cent2, cut);
          sprintf(pbgl_file, "pbgl_year2_FitP3Cor4_%d-%d.dat", Cent1, Cent2);
          sprintf(ytitle, "ratio (bin shift corrected) PbSc/PbGl (%d-%d%%)", Cent1, Cent2);
        }

      //sprintf(pbgl_file,"pi0_ms_pbgl_%d-%d.dat",Cent1,Cent2); // QM02
    }
  else // pp
    {
      sprintf(title, "ratio_pi0_pbsc_pbgl_pp");

      sprintf(pbsc_file, "%s/pp_tables/pi0_pp_pbsc_200GeV_cross.txt", fDataTablesPath->Data());
      //sprintf(pbgl_file,"%/pp_tables/spi0_pp_pbgl_shifty_fixbin_jan_14_2003.txt",fDataTablesPath->Data());
      sprintf(pbgl_file, "%s/pp_tables/pi0_pp_pbgl_200GeV_yields.txt", fDataTablesPath->Data());
      sprintf(ytitle, "ratio pp #pi^{0} PbSc/PbGl");
    }


  plot_ratio_spectra(pbsc_file, pbgl_file, xtitle, ytitle, title);

}

//_____________________________________________________________________________
//
TGraphErrors*
emcAnalyzer::plot_ratio_centralities( const int Cent1,
                                      const int Cent2,
                                      const int Cent3,
                                      const int Cent4,
                                      const char* cut,
                                      bool readAscii,
                                      const bool bin_scaling)
{

  TGraphErrors *Ratio = 0;
  //int CC = getCentralityClass(Cent1,Cent2)*getCentralityClass(Cent3,Cent4);
  //if ( CC < 0 ) return Ratio;

  TString cut_str = cut ;

  char file1[300];
  char file2[300];

  if ( !cut_str.Contains("pbgl") ) // true PbSc cut
    {
      sprintf(file1, "pi0_pbsc_%d_%d_%s.txt", Cent1, Cent2, cut); // default y-shifted file (unless "cut" = "cut_noshift")
      sprintf(file2, "pi0_pbsc_%d_%d_%s.txt", Cent3, Cent4, cut);
    }
  else // PbGl file
    {
      readAscii = true;
      //sprintf(file1,"pbgl_year2_FitP3Cor2_%d-%d.dat",Cent1,Cent2); // bin shift UNcorrected
      //sprintf(file2,"pbgl_year2_FitP3Cor2_%d-%d.dat",Cent3,Cent4); // bin shift UNcorrected
      sprintf(file1, "pbgl_year2_FitP3Cor4_%d-%d.dat", Cent1, Cent2);
      sprintf(file2, "pbgl_year2_FitP3Cor4_%d-%d.dat", Cent3, Cent4);
    }

  if ( cut_str.Contains("QM02") )
    {
      readAscii = true;
      sprintf(file1, "pi0_pbsc_%d_%d_QM02.txt", Cent1, Cent2); // QM02 spectra
      sprintf(file2, "pi0_pbsc_%d_%d_QM02.txt", Cent3, Cent4); // QM02 spectra
    }

  if ( cut_str.Contains("emcal") ) // PbGl+PbSc
    {
      readAscii = true;
      sprintf(file1, "pi0_emcal_%d_%d_%s.txt", Cent1, Cent2, favoritecut);
      sprintf(file2, "pi0_emcal_%d_%d_%s.txt", Cent3, Cent4, favoritecut);
    }

  TGraphErrors *Spectrum1 = 0;
  TGraphErrors *Spectrum2 = 0;

  fPlot = true;
  TGraphErrors *errBand;

  if ( readAscii ) // read from corrected data files on disk
    {
      Spectrum1 = (TGraphErrors*)plot_spectrum_from_ascii((char*)file1, (TGraphErrors*&)errBand);
      Spectrum2 = (TGraphErrors*)plot_spectrum_from_ascii((char*)file2, (TGraphErrors*&)errBand);
    }
  else // reconstruct the spectrum
    {
      fFit = false;
      int binshift = 1;
      Spectrum1 = (TGraphErrors*)plot_spectrum(Cent1, Cent2, cut, "full", binshift);
      Spectrum2 = (TGraphErrors*)plot_spectrum(Cent3, Cent4, cut, "full", binshift);
    }

  if ( !Spectrum1 || !Spectrum2) return Ratio;

  //_____________________________________________________________________________
  // Let's scale both spectra

  // default is Ncoll scaling ...

  double NormalizedAuAu1 = getNcoll(Cent1, Cent2, "value");
  double NormalizedAuAu2 = getNcoll(Cent3, Cent4, "value");

  if ( cut_str.Contains("QM02") )
    {
      NormalizedAuAu1 = getNcoll(Cent1, Cent2, "value_old");
      NormalizedAuAu2 = getNcoll(Cent3, Cent4, "value_old");
    }

  if ( bin_scaling == false )
    {
      NormalizedAuAu1 = getNpart(Cent1, Cent2, "value");
      NormalizedAuAu2 = getNpart(Cent3, Cent4, "value");
    }

  scale(*Spectrum1, 1/NormalizedAuAu1, 0, fVerbose);
  scale(*Spectrum2, 1/NormalizedAuAu2, 0, fVerbose);

  //_____________________________________________________________________________
  // Let's make the ratio

  // Substract first the cancelling errors

  AddOrSubstractRelatErrors(Spectrum1, "CC", "-", fVerbose); // "CC" cancels energy scale and FiduDead
  AddOrSubstractRelatErrors(Spectrum2, "CC", "-", fVerbose); // "CC" cancels energy scale and FiduDead

  Ratio = (TGraphErrors*)ratio(Spectrum1, Spectrum2, fVerbose);
  if (!Ratio) return Ratio;

  for (int i = (Ratio->GetN() - 2); i < Ratio->GetN(); i++)
    {
      //Ratio->RemovePoint(i);
      //cout << "<W> REMOVING point " << i <<  " OF THE RATIO !!!" << endl;
    }
  Ratio->Print();

  //_____________________________________________________________________________
  // Let's plot it

  double ptmin = 0.;
  double ptmax = 10.5;
  double ratiomax = 2.;
  double ratiomin = 0.;
  getXYminmax( Ratio, ptmin, ptmax, ratiomin, ratiomax);
  ptmax += 1.; // additional 1 GeV/c at high pT
  ptmin = (ptmin > 0.) ? 0. : ptmin; // start at 0. GeV/c always
  int ptbins = (int)(ptmax - ptmin)*2;
  ratiomin = (ratiomin > 0) ? 0. : ratiomin; //
  ratiomax = 2.; // (ratiomax < 2) ? 2. : ratiomax; //

  char title[100];
  char name[100];
  if (bin_scaling)
    {
      //sprintf(title,"[dN^{%d-%d%%}/dp_{T} #times #LT N_{coll}#GT^{%d-%d%%}]/",Cent1,Cent2,Cent3,Cent4);
      //sprintf(title,"%s[dN^{%d-%d%%}/dp_{T} #times #LT N_{coll}#GT^{%d-%d%%}]",title,Cent3,Cent4,Cent1,Cent2);
      sprintf(title, "[Yield^{%d-%d%%}/#LT N_{coll}#GT^{%d-%d%%}]/", Cent1, Cent2, Cent1, Cent2);
      sprintf(title, "%s[Yield^{%d-%d%%}/#LT N_{coll}#GT^{%d-%d%%}]", title, Cent3, Cent4, Cent3, Cent4);
    }
  else
    {
      sprintf(title, "[dN^{%d-%d%%}/dp_{T} #times #LT N_{part}#GT^{%d-%d%%}]/", Cent1, Cent2, Cent3, Cent4);
      sprintf(title, "%s[dN^{%d-%d%%}/dp_{T} #times #LT N_{part}#GT^{%d-%d%%}]", title, Cent3, Cent4, Cent1, Cent2);
    }
  sprintf(name, "ratio_%d_%d_to_%d_%d_%s", Cent1, Cent2, Cent3, Cent4, cut);

  TCanvas *c5 = (TCanvas*)canvas(name, 600, 600);
  //c5->Range(-0.12,-0.43,7.25,3.86);
  c5->Range( -0.5325, -0.343035, 8.47523, 3.09979);
  c5->SetLeftMargin(0.110738);
  c5->SetRightMargin(0.0033557);
  c5->SetTopMargin(0.0289855);

  TH2F *myframe = (TH2F*)frame(name, ptbins, ptmin, ptmax, 20, ratiomin, ratiomax, "p_{T} (GeV/#font[72]{c})", title);
  myframe->Draw();

  setMarkerLineType(Ratio, 20, 2, 1.4, 1, 1, 1);
  Ratio->Draw("P");

  // for returning Ratio with the whole attributes
  Ratio->SetTitle(title);
  Ratio->GetYaxis()->SetTitle(title);
  Ratio->GetYaxis()->SetTitleOffset(1.2);
  Ratio->GetYaxis()->SetTitleSize(0.04);
  Ratio->GetXaxis()->SetTitle("p_{T} (GeV/#font[72]{c})");
  Ratio->GetXaxis()->SetTitleOffset(0.9);
  Ratio->GetXaxis()->SetTitleSize(0.05);
  Ratio->SetMaximum(ratiomax);
  Ratio->SetMinimum(ratiomin);

  //_____________________________________________________________________________
  // Ncoll errors drawn separately

  // There are no pT-correlated errors added to Ncoll band: all cancel in ratio

  const int N = Ratio->GetN();

  double *epTcorr_relat = 0;

  bool useTAB = false; // for ratios of centralities we use Ncoll directly
  TH1F* h_eNcollRatio = (TH1F*)getPropagErrorNcollBand(Ratio, Cent1, Cent2, Cent3, Cent4,
						       epTcorr_relat, bin_scaling, useTAB, fVerbose);
  h_eNcollRatio->SetFillColor(kYellow);
  h_eNcollRatio->Draw("e3same");
  //h_eNcollRatio->Draw("e1same"); // just to test the position of the Ncoll errors
  //h_eNcollRatio->Draw("e4same");

  myframe->Draw("same"); // replot the axes

  TLine *line = new TLine(ptmin, 1., ptmax, 1.);
  line->SetLineColor(2);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw("same");

  //   double alternative_scaling = 0.;
  //   if (bin_scaling) alternative_scaling = NpartAuAu*Ncoll_pp/(NcollAuAu*Npart_pp);
  //   else alternative_scaling = (NcollAuAu*Npart_pp)/(NpartAuAu*Ncoll_pp);

  //   TLine *line2 = new TLine(ptmin,alternative_scaling,ptmax,alternative_scaling);
  //   line2->SetLineColor(1);
  //   line2->SetLineStyle(3);
  //   line2->SetLineWidth(3);
  //   line2->Draw("same");

  Ratio->Draw("P"); // replot the points

  c5->Update();

  //_____________________________________________________________________________
  // output

  if (!fPlot)
    {
      c5->Close();
    }
  else
    {
      saveCanvas(c5);
    }

  //_____________________________________________________________________________
  // dump ascii output

  if (fVerbose)
    {
      cout << "  pT   pT_err    ratio    ratio_err    Ncoll_err " << endl;
    }

  double eNcoll[N];

  for (int i = 0; i < Ratio->GetN(); i++)
    {
      double x = 0.;
      double y = 0.;
      Ratio->GetPoint(i, x, y);
      double ex = Ratio->GetErrorX(i);
      double ey = Ratio->GetErrorY(i);
      eNcoll[i] = h_eNcollRatio->GetBinError(i+1); // histo binning starts at 1 (not 0).
      if (fVerbose) printf("  %.2f    %.2f    %.3f    %.3f    %.3f\n", x, ex, y, ey, eNcoll[i]);
    }

  if (fdumpLaTeX)
    {
      dumpLaTeX( Ratio , eNcoll );
    }

  if (fdumpAscii)
    {
      dumpAscii( Ratio , eNcoll );
    }

  return Ratio;

}

//_____________________________________________________________________________
void
emcAnalyzer::plot_all_ratios_centralities( const char* cut,
					   const bool readAscii,
					   const bool bin_scaling )
{

  char title1[100];
  char title2[100];
  sprintf(title1, "pi0_centrality_ratios_set1");
  sprintf(title2, "pi0_centrality_ratios_set2");

  TCanvas *c10 = new TCanvas(title1, title1, 900, 700);
  //gStyle->SetOptStat(11111111);
  c10->Divide(2, 2, 0, 0);

  TCanvas *c10b = new TCanvas(title2, title2, 900, 700);
  //gStyle->SetOptStat(11111111);
  c10b->Divide(2, 2, 0, 0);

  int Cent1 = 0;
  int Cent2 = 10;

  int PeriphRef1 = 80;
  int PeriphRef2 = 92;

  TGraphErrors *ratio_cent = 0;
  TH1F* h_eNcollRatio = 0;

  fPlot = true;

  for (int i = 0; i <= CentClasses; i+=2)
    {
      int subpad = i/2+1;
      if (subpad > 4)
        subpad -= 4;

      getCentralityClassLimits2( i, Cent1, Cent2);
      ratio_cent = (TGraphErrors*)plot_ratio_centralities(Cent1, Cent2, PeriphRef1, PeriphRef2, cut,
							  readAscii, bin_scaling);
      if (Cent1 < 40)
        c10->cd(subpad); // half in 1st Canvas
      else
        c10b->cd(subpad); // the other half in the 2nd Canvas
      if (ratio_cent)
        {
          ratio_cent->GetYaxis()->SetTitleSize(0.055);
          ratio_cent->GetYaxis()->SetTitleOffset(0.9);
          ratio_cent->Draw("AP");

          // Ncoll band
          const int N = ratio_cent->GetN();
          double *epTcorr_relat = new double[N];
          for (int i = 0; i < N; i++)
            {
              double pt = ratio_cent->GetX()[i];
              epTcorr_relat[i] = getpTCorrelatRelError(pt);
            }

          bool useTAB = false; // for ratios of centralities we use Ncoll directly
          h_eNcollRatio = (TH1F*)getPropagErrorNcollBand(ratio_cent, Cent1, Cent2, PeriphRef1, PeriphRef2,
							 epTcorr_relat, bin_scaling, useTAB, fVerbose);
          h_eNcollRatio->Smooth(4);
          h_eNcollRatio->SetFillColor(kYellow);
          h_eNcollRatio->Draw("e3same");

          TLine *line = new TLine(0., 1.0, 8., 1.0);
          line->SetLineColor(1);
          line->SetLineStyle(2);
          line->SetLineWidth(3);
          line->Draw("same");
          ratio_cent->Draw("P"); // replot
        }
      c10->Update();
      c10b->Update();
    }

  c10->Update();
  c10b->Update();
}

//_____________________________________________________________________________
//
// cut = "emcal","pbgl", "QM02", "emcalrun3"
// return_RAA_w_fitted_pp_denom = false (default)
// additionalRAA = "dAu","130"
//
TGraphErrors*
emcAnalyzer::plot_RAA( const int Cent1,
                       const int Cent2,
                       const char* cut,
                       bool readAscii,
                       const bool bin_scaling,
		       const bool return_RAA_w_fitted_pp_denom,
                       TString additionalRAA,
                       const bool useTABinsteadOfNcoll )
{

  TGraphErrors *RAA = 0;

  TString cut_str = cut ;
  if ( cut_str.Contains("emcal") ||
       cut_str.Contains("pbgl") ||
       cut_str.Contains("QM02") )
    {
      readAscii = true; // combined PbSc+PbGl, PbGl and QM02 exist as ascii
    }

  int CC = getCentralityClass(Cent1, Cent2);
  if ( CC == -1) return RAA;

  double ptmin = 0.0;
  double ptmax = 14.5;

  //_____________________________________________________________________________
  // AuAu spectrum

  TGraphErrors *SpectrumAuAu = 0;
  TGraphErrors *errBand;

  if ( readAscii ) // read from data files on disk
    {
      char file[200];
      if ( cut_str.Contains("emcal") )  // emcal
        {
          sprintf(file, "phenix_pi0_final_emcal_Run2_AuAu_%d_%d.txt", Cent1, Cent2);
          //sprintf(file, "pi0_emcal_%d_%d_%s.txt", Cent1, Cent2, favoritecut); // Combined PbSc+PbGl is for tof1chisq1 (PbSc)
        }
      else if ( cut_str.Contains("pbgl") ) // pbgl
        {
          //sprintf(file,"pbgl_year2_FitP3Cor2_%d-%d.dat",Cent1,Cent2); // bin shift UNcorrected
          sprintf(file, "pbgl_year2_FitP3Cor4_%d-%d.dat", Cent1, Cent2); // y-shifted file
        }
      else if ( cut_str.Contains("QM02") )
        {
          sprintf(file, "pi0_pbsc_%d_%d_QM02.txt", Cent1, Cent2); // QM02 spectra
        }
      else // pbsc
        {
          sprintf(file, "pi0_pbsc_%d_%d_%s.txt", Cent1, Cent2, cut); // default y-shifted file (unless "cut" = "cut_noshift")
        }

      fFit = false;
      SpectrumAuAu = (TGraphErrors*)plot_spectrum_from_ascii((char*)file, (TGraphErrors*&)errBand);
    }
  else // PbSc - reconstruct the spectrum
    {
      int binshift = 1;
      fFit = false;
      SpectrumAuAu = (TGraphErrors*)plot_spectrum(Cent1, Cent2, cut, "full", binshift);
    }

//   // Substract "soft" component of Au+Au spectrum:

//   char tempo[100];
//   sprintf(tempo,"%d_%d",Cent1,Cent2);
//   TF1 *soft_pi_AuAu200 = (TF1*)plot_pp_sigma_parametrization(tempo,"soft_AuAu",200);
//   SpectrumAuAu=(TGraphErrors*)add(SpectrumAuAu,soft_pi_AuAu200,-1.,1);
//   SpectrumAuAu->Draw("A");

  //_____________________________________________________________________________
  // pp spectrum

  TGraphErrors *Spectrum_pp = 0;
  fFit = false;

  if ( cut_str.Contains("QM02") )
    {
      Spectrum_pp = (TGraphErrors*)plot_spectrum_from_ascii("yields_pp_hisa_tbl_c22mbc_tot.txt", (TGraphErrors*&)errBand);
    }
  else if ( cut_str.Contains("run3") )
    {
      Spectrum_pp = (TGraphErrors*)plot_spectrum_from_ascii("pi0_pp_emcal_200GeV_run3.txt",errBand);
      //double abs_sigma = 20.5/0.785;
      //scale(*Spectrum_pp,abs_sigma);
      ptmin = 0.;
      ptmax = 16.;
    }
  else
    {
      Spectrum_pp = (TGraphErrors*)plot_spectrum_from_ascii("pi0_pp_emcal_200GeV_final_cross_sasha.txt", (TGraphErrors*&)errBand);
    }

  if (!SpectrumAuAu || !Spectrum_pp) return RAA;


//   // Substract "soft" component of p+p spectrum:

//   TF1 *soft_pp_pi_200 = (TF1*)plot_pp_sigma_parametrization("pi","soft_pp",200);
//   Spectrum_pp=(TGraphErrors*)add(Spectrum_pp,soft_pp_pi_200,-1.,1);
//   Spectrum_pp->Draw("A");

  //_____________________________________________________________________________
  // Let's scale both spectra

  double NormalizedAuAu = 1.;
  double Normalized_pp = 1.;

  // errors on T_AB, Ncoll, Npart considered below (band)

  double T_AuAu = getTAB(Cent1, Cent2, "value");

  double NcollAuAu = getNcoll(Cent1, Cent2, "value");
  double Ncoll_pp = getNcoll("pp"); // equal to 1

  double NpartAuAu = getNpart(Cent1, Cent2, "value");
  double Npart_pp = getNpart("pp"); // equal to 2

  if ( cut_str.Contains("QM02") )
    {
      NcollAuAu = getNcoll(Cent1, Cent2, "value_old");
      NpartAuAu = getNpart(Cent1, Cent2, "value_old");
    }

  // default is T_AB scaling

  if (useTABinsteadOfNcoll)
    {
      NormalizedAuAu = T_AuAu;
      Normalized_pp = 1.;
    }
  else
    {
      NormalizedAuAu = NcollAuAu;
      Normalized_pp = 1.;

      // pp ascii data tables are in mb/(GeV/c2)^{-2}
      scale(*Spectrum_pp, 1./sigma_NN, 0, fVerbose);  //,sigma_NN*pp_luminosity_erelat);
    }

  if ( bin_scaling == false )
    {
      NormalizedAuAu = NpartAuAu;
      Normalized_pp = Npart_pp; // equal to 2

      // pp ascii data tables are in mb/(GeV/c2)^{-2}
      scale(*Spectrum_pp, 1./sigma_NN, 0, fVerbose);
    }

  scale(*SpectrumAuAu, 1./NormalizedAuAu, 0, fVerbose); //,eNormalizedAuAu);// do not propagate these errors to spectrum but to band (below)
  scale(*Spectrum_pp, 1./Normalized_pp, 0, fVerbose);   //,eNcoll_pp);

  // Multi-parameter fit of pp spectrum

  TF1* fit_pp_pi0 = 0;

  if ( !cut_str.Contains("run3") )
    {
      fit_pp_pi0 = new TF1("fit_pp_pi0", "[0]/(exp([2]*x*x+[3]*x)+x/[1])^[4]", ptmin, ptmax); 
      fit_pp_pi0->SetParameters(300.,1.,0.,0.,10.);
      Spectrum_pp->Fit("fit_pp_pi0","Q","",ptmin,ptmax);
      fit_pp_pi0->SetLineColor(2);
      fit_pp_pi0->SetLineWidth(4);
    }
  else
    {
      fit_pp_pi0 = (TF1*)plot_pp_sigma_parametrization("pi0","run3_pi0",200.,1.,0.);
      fit_pp_pi0->SetName("fit_pp_pi0");
    }

  //_____________________________________________________________________________
  // Let's make the ratio

  // Subtract first the cancelling errors in the ratio or those which go to the band

  //AddOrSubstractRelatErrors(SpectrumAuAu, "RAA", "-", fVerbose); // The AuAu acceptance error (5.%) goes into the band
  //AddOrSubstractRelatErrors(Spectrum_pp, "pp", "-", fVerbose);   // The pp acceptance error (3.8%) goes into the band

  RAA = (TGraphErrors*)ratio(SpectrumAuAu, Spectrum_pp, fVerbose);

  if ( cut_str.Contains("QM02") ) // we read directly QM02 R_AA ascii file
    {
      fPlot = false;
      fFit = false;
      RAA = (TGraphErrors*)plot_spectrum_from_ascii("RAA_0_10_QM02.txt", (TGraphErrors*&)errBand);
      if ( !bin_scaling )
        {
          double Ncoll2Npart = getNcoll(Cent1, Cent2, "value_old")*Npart_pp;
          Ncoll2Npart /= getNpart(Cent1, Cent2, "value_old")*Ncoll_pp;
          scale(*RAA, Ncoll2Npart, 0, fVerbose);
          cout << "Scaling R_AA by: " << Ncoll2Npart << endl;
        }
    }
  if (!RAA) return RAA;

  //_____________________________________________________________________________
  // Ratio to fit

  TGraphErrors *RAAfit = (TGraphErrors*)ratio(SpectrumAuAu,fit_pp_pi0,0,fVerbose);

  if ( !cut_str.Contains("run3") )
    {
      cout << " <W> You should propagate the reference p+p fit uncertainties to the R_AA !" << endl;
    }
  else
    {

      cout << " <I> Propagating the reference p+p fit uncertainties to the R_AA ..." << endl;

      // Let's propagate the error uncertainty quoted in AN472:
      // https://www.phenix.bnl.gov/phenix/WWW/p/info/an/472/table/relative_uncertainty.txt
      double pt[21] = {1.25,1.75,2.25,2.75,3.25,3.75,4.25,4.75,5.25,5.75,
		       6.25,6.75,7.25,7.75,8.25,8.75,9.25,9.75,11.0,13.0,15.};
      double ept[21] = {0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.};
      double fit[21] = {1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.};
      double efit[21] = {0.00650764,0.00734413,0.0109583,0.00744486,0.003968,0.0143293,0.0260206,0.0101178, 
			 0.00512087,0.000600457,0.00446808,0.00848758,0.0122342,0.0157435,0.0190443,
			 0.0221605,0.0251124,0.0279495,0.0346792,0.0440718,0.0521854};

      TGraphErrors *errorfit = new TGraphErrors( 21, pt, fit, ept, efit);
      errorfit->SetName("errorfit");
      TGraphErrors *RAAfit2 =(TGraphErrors*)RAAfit->Clone();
      RAAfit = (TGraphErrors*)ratio(RAAfit2,errorfit,fVerbose,0.25);
    }

  //RAAfit->Print();

  //_____________________________________________________________________________
  // Let's plot it

  double ratiomin = 0.;
  double ratiomax = 2.;
  //getXYminmax( RAA, ptmin, ptmax, ratiomin, ratiomax);
  ptmax += 1.; // additional 1 GeV/c at high pT
  ptmin = (ptmin > 0.) ? 0. : ptmin; // start at 0. GeV/c always
  int ptbins = (int)(ptmax - ptmin)*2;
  ratiomin = (ratiomin > 0.) ? 0. : ratiomin;
  ratiomax = (bin_scaling) ? 1.6 : ratiomax; 

  char title[100];
  char name[100];
  char ytitle[100];

  sprintf(title, "Au+Au #rightarrow #pi^{0}X 200 GeV (%d-%d)", Cent1, Cent2);
  char *scaling = "Ncoll";
  if (!bin_scaling)
    scaling = "Npart";
  sprintf(name, "RAA_%d_%d_%s_%s", Cent1, Cent2, cut, scaling);

  //if (bin_scaling) sprintf(ytitle,"R_{AA} = d#sigma^{AuAu}/dp_{T}/( #LT N_{coll}#GT d#sigma^{pp}/dp_{T} )");
  if (bin_scaling)
    sprintf(ytitle, "R_{AA}");
  //else sprintf(ytitle,"R_{AA} = d#sigma^{AuAu}/dp_{T} /( #LT N_{part}#GT Yield^{pp} )");
  else
    sprintf(ytitle, "R_{AA}^{part}");

  TCanvas *c5 = (TCanvas*)canvas(name, 650, 500);
  c5->Range( -0.5325, -0.343035, 8.47523, 3.09979);
  c5->SetLeftMargin(0.110738);
  c5->SetRightMargin(0.0033557);
  c5->SetTopMargin(0.0289855);

  TH2F *myframe = (TH2F*)frame(name, ptbins, ptmin, ptmax, 20, ratiomin, ratiomax, "p_{T} (GeV/#font[72]{c})", ytitle);
  myframe->SetTitle(title);
  myframe->SetTitleOffset(1.,"Y");
  myframe->Draw();

  setMarkerLineType(RAA, 20, 2, 1.4, 1, 1, 1);
  RAA->Draw("P");

  //   TLegend *leg = new TLegend(0.484899,0.836957,0.983221,0.952899,"Au+Au #rightarrow #pi^{0}X @ 200 GeV (0-10%)","brNDC");
  //   leg->SetMargin(0.1);
  //   leg->SetLineColor(1);
  //   leg->SetLineStyle(1);
  //   leg->SetLineWidth(1);
  //   leg->SetFillStyle(1001);
  //   TLegendEntry *entry=leg->AddEntry("NULL","PHENIX preliminary","h");
  //   leg->Draw("same");

  //_____________________________________________________________________________
  // Additional RAA

  TGraphErrors *otherRAA = 0;
  //TString additionalRAA_str = additionalRAA ;

  fFit = false;

  if (additionalRAA.Contains("dAu", TString::kIgnoreCase)) // comparison to d+Au
    {
      otherRAA = (TGraphErrors*)plot_spectrum_from_ascii("RdAu_pi0_emcal_minbias.txt", (TGraphErrors*&)errBand);
    }
  if (additionalRAA.Contains("QM02")) // comparison to QM'02
    {
      otherRAA = (TGraphErrors*)plot_spectrum_from_ascii("RAA_0_10_QM02.txt", (TGraphErrors*&)errBand); // QM02 R_AA
      //otherRAA = (TGraphErrors*)plot_spectrum_from_ascii("RAA_60_80_QM02.txt",(TGraphErrors*&)errBand);
    }
  else if (additionalRAA.Contains("130")) // comparison to RAA 130 GeV
    {
      otherRAA = (TGraphErrors*)get_pi0_130_RAA();
      // Draw PHENIX 130 errors
      TBox *box = 0;
      TClonesArray *array_of_boxes = 0;
      array_of_boxes = (TClonesArray*)get_pi0_130_errors();
      int boxes = array_of_boxes->GetLast()+1;
      for (int j = 0; j < boxes;j++)
        {
          box = ((TBox *) (*array_of_boxes)[j]);
          c5->cd();
          box->Draw();
        }
    }
  if (otherRAA)
    {
      setMarkerLineType(otherRAA, 20, 4, 1.4, 1, 4, 1);
      c5->cd();
      otherRAA->Draw("P");
    }

  //_____________________________________________________________________________
  // Ncoll errors drawn separately

  // Global acceptance normalization errors added to Ncoll band: pp abs.norm. error, pp accept. error, AuAu accept. error

  double pp_abs_yield_erelat = 0.096; // Additional 9.6% error on pp yield (BBC efficiency + luminosity): AN184
  double FiduDeadWarnPlusAccRelatError = 0.05; // /Sqrt(2.) ;
  double ppAcceptanceError = 0.038;

  double extra_erelat = quadRelatError( pp_abs_yield_erelat, 1.,  // pp BBC,sigma normalization
                                        ppAcceptanceError, 1.,    // pp acceptance
                                        FiduDeadWarnPlusAccRelatError, 1.); // AuAu acceptance
  if ( cut_str.Contains("QM02") )
    {
      extra_erelat = 0.20; // luminosity error at QM02 time
    }

  if ( cut_str.Contains("run3") ) // Acceptance errors cancel and BBC 9.7% uncertainty is not considered pT-dependent ...
    {
      extra_erelat = 0.; 
    }

  //   double *extra_erelat = new double[NpTbins];
  //   for (int i=0; i<NpTbins; i++)
  //     {
  //       //double pt = Ratio->GetX()[i];
  //       extra_erelat[i] = quadRelatError( pp_abs_yield_erelat,1., // pp BBC,sigma normalization
  // 				        ppAcceptanceError,1.,   // pp acceptance
  // 				        FiduDeadWarnPlusAccRelatError,1.); // AuAu acceptance
  //       if ( cut_str.Contains("QM02") )
  // 	{
  // 	  extra_erelat[i] = 0.20; // luminosity error at QM02 time
  // 	}
  //     }

  //   // fixme: this guy does not know anything about glauber type ("new" or QM02 "old" ...)
  //   TH1F *h_eNcoll_RAA = (TH1F*)getPropagErrorNcollBand( RAA,Cent1,Cent2,0,0,extra_erelat,
  // 						       bin_scaling,useTABinsteadOfNcoll,fVerbose);
  //   c5->cd();
  //   h_eNcoll_RAA->Smooth(1);
  //   h_eNcoll_RAA->SetFillColor(kYellow);
  //   h_eNcoll_RAA->Draw("e3same");
  //   //h_eNcoll_RAA->Draw("e4same");

  //   TH1F *h_eNcoll_RAAfit = (TH1F*)getPropagErrorNcollBand( RAAfit,Cent1,Cent2,0,0,extra_erelat,
  // 							  bin_scaling,useTABinsteadOfNcoll,fVerbose);
  //   c5->cd();
  //   h_eNcoll_RAAfit->Smooth(3);
  //   h_eNcoll_RAAfit->SetFillStyle(4);
  //   h_eNcoll_RAAfit->SetFillColor(7);
  //   //h_eNcoll_RAAfit->Draw("e3same");

  double eNorm = getPropagNormRelatError(Cent1, Cent2, 0, 0, extra_erelat, bin_scaling, useTABinsteadOfNcoll, fVerbose);

  TClonesArray* errBandAA = (TClonesArray*)errorBand( RAAfit, eNorm );

  TGraph *RAAshade = (TGraph*)(*errBandAA)[0];
  TGraph *RAAmin = (TGraph*)(*errBandAA)[1];
  TGraph *RAAmax = (TGraph*)(*errBandAA)[2];

//   RAAshade->Draw("f");
//   RAAmin->Draw("l");
//   RAAmax->Draw("l");


  TH1F *eNormBox= new TH1F("eNormBox","",2,0.2,0.8);
  for (int i=0;i<=2;i++)
    {
      eNormBox->SetBinContent(i,1.);
      eNormBox->SetBinError(i,eNorm);
    }
  eNormBox->SetFillColor(1);
  eNormBox->SetFillStyle(3001);
  eNormBox->Draw("e3same");

  myframe->Draw("same"); // replot the axes

  TLine *line = new TLine(ptmin, 1.0, ptmax, 1.0);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw("same");

  double alternative_scaling = 0.;
  if (bin_scaling)
    {
      alternative_scaling = NpartAuAu*Ncoll_pp/(NcollAuAu*Npart_pp);
    }
  else
    {
      alternative_scaling = NcollAuAu*Npart_pp/(NpartAuAu*Ncoll_pp);
    }

  TLine *line2 = new TLine(ptmin, alternative_scaling, ptmax, alternative_scaling);
  line2->SetLineColor(1);
  line2->SetLineStyle(3);
  line2->SetLineWidth(3);
  line2->Draw("same");

  setMarkerLineType(RAAfit, 24, 2, 1.4, 1, 1, 1);
  RAAfit->Draw("P");

  if (otherRAA)
    otherRAA->Draw("P"); // replot it
  RAA->Draw("P"); // replot the points

  c5->Update();

  //_____________________________________________________________________________
  // output

  if (!fPlot)
    {
      c5->Close();
    }
  else
    {
      saveCanvas(c5);
    }

  //_____________________________________________________________________________
  // dump ascii output

  if (fVerbose)
    {
      cout << "  pT     pT_err   RAA    RAA_err    RAA_err(%)    Ncoll_err " << endl;
    }

  int N = RAA->GetN();
  double errNorm[N];

  for (int i = 0; i < N; i++)
    {
      double x, y;
      RAA->GetPoint(i, x, y);
      double ex = RAA->GetErrorX(i);
      double ey = RAA->GetErrorY(i);
      double erelat = ey/y*100.;
      errNorm[i] = eNorm*y;
      if (fVerbose) printf("  %.2f    %.2f    %.3f    %.3f    %.1f    %.3f\n", x, ex, y, ey, erelat, errNorm[i]);
    }

  N = RAAfit->GetN();
  double errNormfit[N];

  for (int i = 0; i < N; i++)
    {
      double x, y;
      RAAfit->GetPoint(i, x, y);
      double ex = RAAfit->GetErrorX(i);
      double ey = RAAfit->GetErrorY(i);
      double erelat = ey/y*100.;
      errNormfit[i] = eNorm*y;
      if (fVerbose) printf("  %.2f    %.2f    %.3f    %.3f    %.1f    %.3f\n", x, ex, y, ey, erelat, errNormfit[i]);
    }

  // fixme: can we separate stat & syst output for RAA ?

  if (fdumpLaTeX)
    {
      dumpLaTeX( RAA , errNorm );
      if (return_RAA_w_fitted_pp_denom)
	{
	  cout << " <I> This is RAA_fit !" << endl;
	  dumpLaTeX( RAAfit , errNormfit );
	}
    }

  if (fdumpAscii)
    {
      dumpAscii( RAA , errNorm );
      if (return_RAA_w_fitted_pp_denom)
	{
	  cout << " <I> This is RAA_fit !" << endl; 
	  dumpAscii( RAAfit , errNormfit );
	}
    }


  if (return_RAA_w_fitted_pp_denom)
    {
      return RAAfit;
    }
  else
    {
      return RAA;
    }

}

//_____________________________________________________________________________
//
// cut: "emcal", "emcal_ntagged", "pbsc", "pbgl", "charged"
// additionalRdA: "charged", "ntagged", "AuAu010"

TGraphErrors*
emcAnalyzer::plot_RdA( const int Cent1,
                       const int Cent2,
                       const char* cut,
                       const bool bin_scaling,
                       TString additionalRdA,
                       const bool useTABinsteadOfNcoll,
                       const bool readAscii )
{

  TGraphErrors *RdA = 0;
  TGraph *RdAeBand = 0;

  TGraphErrors *otherRdA = 0;
  TGraph *otherRdAeBand = 0;

  TGraphErrors *errBand = 0; // default band

  double eNorm = 0.;

  //int CC = getCentralityClass(Cent1,Cent2);
  //if ( CC == -1) return RdA;

  TString cut_str = cut ;

  double NcolldAu = 8.4;
  double eNcolldAu = 0.7;
  double Ncoll_pp = getNcoll("pp"); // equal to 1

  double NpartdAu = 8.7;
  double eNpartdAu = 0.7;
  double Npart_pp = getNpart("pp"); // equal to 2

  // do not fit intermediate spectra
  fPlot = true;
  fFit = false;

  //_____________________________________________________________________________
  // let's try to compute RdA ourselves ...

  if (!readAscii)
    {
      //_____________________________________________________________________________
      // dAu spectrum

      TGraphErrors *SpectrumdAu = 0;

      char file[200];
      if ( cut_str.Contains("ntagged") )  // n-tagged
        {
          sprintf(file, "pi0_%s_dAu_ntagged.txt", cut);
        }
      else if ( cut_str.Contains("emcal") )  // emcal
        {
          sprintf(file, "pi0_emcal_dAu_%d_%d.txt", Cent1, Cent2);
        }
      //   else if ( cut_str.Contains("pbgl") ) // pbgl
      //     {
      //       //sprintf(file,"",Cent1,Cent2); // bin shift UNcorrected
      //       sprintf(file,"pi0_pbgl_dAu_%d_%d.txt",Cent1,Cent2); // y-shifted file
      //     }
      //   else // pbsc
      //     {
      //       sprintf(file,"pi0_pbsc_dAu_%d_%d.txt",Cent1,Cent2); // default y-shifted file (unless "cut" = "cut_noshift")
      //     }

      SpectrumdAu = (TGraphErrors*)plot_spectrum_from_ascii((char*)file,
							    (TGraphErrors*&)errBand);
      if (!SpectrumdAu) return RdA;

      //_____________________________________________________________________________
      // pp spectrum

      TGraphErrors *Spectrum_pp = 0;

      Spectrum_pp = (TGraphErrors*)plot_spectrum_from_ascii("pi0_pp_emcal_200GeV_final_cross_sasha.txt",
							    (TGraphErrors*&)errBand);

      if (!Spectrum_pp) return RdA;

      // pp ascii data tables are in mb/(GeV/c)^{-2}
      scale(*Spectrum_pp, 1./sigma_NN, 0, fVerbose);  //,sigma_NN*pp_luminosity_erelat);

      //_____________________________________________________________________________
      // Let's scale both spectra

      double NormalizeddAu = 1.;
      double Normalized_pp = 1.;
      double eNormalizeddAu = 1.;

      // errors on T_AB, Ncoll, Npart considered below (band)

      //double T_dAu = 8.4/sigma_NN;

      // default is Ncoll scaling

      NormalizeddAu = NcolldAu;
      Normalized_pp = 1.;
      eNormalizeddAu = eNcolldAu;

      if ( bin_scaling == false )
        {
          NormalizeddAu = NpartdAu;
          Normalized_pp = Npart_pp; // equal to 2
          eNormalizeddAu = eNpartdAu;
        }

      scale(*SpectrumdAu, 1./NormalizeddAu, 0, fVerbose); //,eNormalizeddAu); // do not propagate these errors to spect. but to band (below)
      scale(*Spectrum_pp, 1./Normalized_pp, 0, fVerbose); //,eNcoll_pp);

      // Hagedorn fit of pp spectrum

      double ptmin = 1.0, ptmax = 12.0;
      TF1* hagedornpp = 0;
      if (fConstrainedHagFit)
        {
          hagedornpp = hagedornConstrainedFit(Spectrum_pp, "constrhagedorn", ptmin, ptmax, fVerbose);
        }
      else
        {
          hagedornpp = hagedornFit(Spectrum_pp, "hagedorn", ptmin, ptmax, fVerbose);
        }
      hagedornpp->SetLineColor(2);
      hagedornpp->SetLineWidth(4);

      //_____________________________________________________________________________
      // Let's make the ratio

      // Subtract first the cancelling errors in the ratio or those which go to the band

      AddOrSubstractRelatErrors(SpectrumdAu, "RdA", "-", fVerbose); // The dAu acceptance error (5.%) goes into the band
      AddOrSubstractRelatErrors(Spectrum_pp, "pp", "-", fVerbose);  // The pp acceptance error (3.8%) goes into the band

      RdA = (TGraphErrors*)ratio(SpectrumdAu, Spectrum_pp, fVerbose);

      //_____________________________________________________________________________
      // Normalization errors drawn separately

      //   // Global acceptance normalization errors added to Ncoll band:
      //   // pp abs.norm. error, pp accept. error, dAu accept. error

      double pp_abs_yield_erelat = 0.096; // Additional 9.6% error on pp yield (BBC efficiency + luminosity): AN184
      double FiduDeadWarnPlusAccRelatError = 0.05; // /Sqrt(2.) ;
      double ppAcceptanceError = 0.038;

      double extra_erelat = quadRelatError( pp_abs_yield_erelat, 1.,  // pp BBC,sigma normalization
                                            ppAcceptanceError, 1.,    // pp acceptance
                                            FiduDeadWarnPlusAccRelatError, 1.); // dAu acceptance

      // error band constructed from eNorm

      eNorm = quadRelatError(extra_erelat, 1, eNormalizeddAu, NormalizeddAu);
      TClonesArray* eb = (TClonesArray*)errorBand( RdA, eNorm );
      RdAeBand = (TGraph*)(*eb)[0];
      //TGraph *RdAmin = (TGraph*)(*eb)[1];
      //TGraph *RdAmax = (TGraph*)(*eb)[2];
    }
  //_____________________________________________________________________________
  // MUCH SIMPLER: read from file
  else
    {
      RdA = (TGraphErrors*)plot_spectrum_from_ascii("RdAu_pi0_emcal_0_88.txt", errBand);

      // error band read from file
      TClonesArray* eb = (TClonesArray*)errorBand( errBand );
      RdAeBand = (TGraph*)(*eb)[0];
      //TGraph *RdAmin = (TGraph*)(*eb)[1];
      //TGraph *RdAmax = (TGraph*)(*eb)[2];

      // fixme: we need a way to propagate errBand to eNorm for ascii-dump purposes below ...
      // eNorm = ...;
    }

  //_____________________________________________________________________________

  if (!RdA) return RdA;

  setMarkerLineType(RdA, 21, 8, 1.4, 1, 1, 1);

  //setMarkerLineType(RdAfit,20,4,1.4,1,4,1);

  TString label = "PHENIX high p_{T} #pi^{0}";
  TLegend *leg = new TLegend(0.3, 0.75, 0.95, 0.95, label, "brNDC");
  leg->SetMargin(0.1);
  leg->SetTextSize(0.042);
  leg->SetFillColor(kWhite);

  char sublabel[50];
  sprintf(sublabel, "d+Au @ 200 GeV [min. bias]");
  leg->AddEntry(RdA, sublabel, "P");

  //_____________________________________________________________________________
  // Additional RdA

  errBand = 0;

  if (additionalRdA.Contains("charged", TString::kIgnoreCase)) // comparison to d+Au charged
    {
      otherRdA = (TGraphErrors*)plot_spectrum_from_ascii("RdAu_chargedhadrons_minbias.txt", errBand);
      sprintf(sublabel, "charged hadrons d+Au @ 200 GeV [min. bias]");
    }
  else if (additionalRdA.Contains("ntag", TString::kIgnoreCase)) // comparison to n-tag d+Au charged
    {
      otherRdA = (TGraphErrors*)plot_spectrum_from_ascii("RdAu_chargedhadrons_ntagged.txt", errBand);
      sprintf(sublabel, "charged hadrons n+Au @ 200 GeV [min. bias]");
    }
  else if (additionalRdA.Contains("AuAu010", TString::kIgnoreCase)) // comparison to Au+Au
    {
      otherRdA = (TGraphErrors*)plot_spectrum_from_ascii("RAA_emcal_0_10_tof1chisq1.txt", errBand);
      sprintf(sublabel, "Au+Au @ 200 GeV [0-10%%]");
    }
  else if (additionalRdA.Contains("AuAuMinB", TString::kIgnoreCase)) // comparison to Au+Au
    {
      otherRdA = (TGraphErrors*)plot_spectrum_from_ascii("RAA_emcal_0_100_tof1chisq1.txt", errBand);
      sprintf(sublabel, "Au+Au @ 200 GeV [min. bias]");
    }

  if (otherRdA)
    {
      setMarkerLineType(otherRdA, 20, 2, 1.4); //,1,2,1);
      leg->AddEntry(otherRdA, sublabel, "P");
    }
  if (errBand)
    {
      TClonesArray* eb = (TClonesArray*)errorBand( errBand );
      otherRdAeBand = (TGraph*)(*eb)[0];
    }

  //_____________________________________________________________________________
  // Let's plot them

  double ptmin = 0., ptmax = 10.5;
  double ratiomin = 0., ratiomax = 3.;
  getXYminmax( RdA, ptmin, ptmax, ratiomin, ratiomax);
  ptmax += 1.; // additional 1 GeV/c at high pT
  ptmin = (ptmin > 0.) ? 0. : ptmin; // start at 0. GeV/c always
  int ptbins = (int)(ptmax - ptmin)*2;

  if (fLogy)
    {
      ratiomin = 0.08;
      ratiomax = 8.0;
    }
  else
    {
      ratiomin = (ratiomin > 0.) ? 0. : ratiomin;
      //ratiomax = (bin_scaling) ? 1.6 : ratiomax;
      ratiomax = 2.5; // liny
    }

  char title[100];
  char name[100];
  char ytitle[100];

  sprintf(title, "d+Au #rightarrow #pi^{0}X 200 GeV (%d-%d)", Cent1, Cent2);
  char *scaling = "Ncoll";
  if (!bin_scaling)
    scaling = "Npart";
  sprintf(name, "RdA_%d_%d_%s_%s", Cent1, Cent2, cut, scaling);
  if (additionalRdA)
    {
      sprintf(name, "%s_%s", name, additionalRdA.Data());
    }

  if (bin_scaling)
    sprintf(ytitle, "Nuclear modification factor");
  //if (bin_scaling) sprintf(ytitle,"R_{dA}");
  else
    sprintf(ytitle, "R_{dA}^{part}");

  //_____________________________________________________________________________
  // Canvas

  TCanvas *c5 = (TCanvas*)canvas(name, 600, 600);
  c5->Range( -0.5325, -0.343035, 8.47523, 3.09979);
  if (fLogy)
    {
      c5->SetLogy();
    }

  TH2F *myframe = (TH2F*)frame(name, ptbins, ptmin, ptmax, 20, ratiomin, ratiomax, "p_{T} (GeV/#font[72]{c})", ytitle);
  myframe->Draw();

  //_____________________________________________________________________________
  // Lines

  TLine *line = new TLine(ptmin, 1.0, ptmax, 1.0);
  if (fLogy)
    line = new TLine(ptmin, Log(1.0), ptmax, Log(1.0));
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw("same");

  double alternative_scaling = 0.;
  if (bin_scaling)
    {
      alternative_scaling = NpartdAu*Ncoll_pp/(NcolldAu*Npart_pp);
    }
  else
    {
      alternative_scaling = NcolldAu*Npart_pp/(NpartdAu*Ncoll_pp);
    }

  TLine *line2 = new TLine(ptmin, alternative_scaling, ptmax, alternative_scaling);
  if (fLogy)
    line2 = new TLine(ptmin, Log(alternative_scaling),
                      ptmax, Log(alternative_scaling));
  line2->SetLineColor(1);
  line2->SetLineStyle(3);
  line2->SetLineWidth(3);
  line2->Draw("same");

  leg->Draw();
  c5->Update();

  //_____________________________________________________________________________
  // real stuff ...

  RdAeBand->Draw("f");
  RdA->Draw("P");
  //RdAfit->Draw("P");

  //RdAmin->Draw("l");
  //RdAmax->Draw("l");

  if (otherRdAeBand)
    otherRdAeBand->Draw("f");
  if (otherRdA)
    otherRdA->Draw("P");

  line->Draw("same");
  line2->Draw("same");
  c5->Update();

  //myframe->Draw("same"); // replot the axes (nope: we cannot access the objects with the cursor ...)
  //c5->Update();

  //_____________________________________________________________________________
  // output

  if (!fPlot)
    {
      c5->Close();
    }
  else
    {
      saveCanvas(c5);
    }

  //_____________________________________________________________________________
  // dump ascii output

  if (fVerbose)
    {
      cout << "  pT     pT_err   RdA    RdA_err    RdA_err(%)    Norm_err " << endl;
    }

  const int N = RdA->GetN();
  double errNorm[N];

  for (int i = 0; i < N; i++)
    {
      double x, y;
      RdA->GetPoint(i, x, y);
      double ex = RdA->GetErrorX(i);
      double ey = RdA->GetErrorY(i);
      double erelat = ey/y*100.;
      errNorm[i] = eNorm*x;
      if (fVerbose) printf("  %.2f    %.2f    %.3f    %.3f    %.1f    %.3f\n", x, ex, y, ey, erelat, errNorm[i]);
    }

  // fixme: can we separate stat & syst output for RdA ?

  if (fdumpLaTeX)
    {
      dumpLaTeX( RdA , errNorm );
    }

  if (fdumpAscii)
    {
      dumpAscii( RdA , errNorm );
    }

  return RdA;

}

//_____________________________________________________________________________
void
emcAnalyzer::plot_spectra_RHIC_SPS( const int Cent1,
                                    const int Cent2,
                                    const char* cut,
                                    const bool readAscii )
{

  char title[100];
  sprintf(title, "spectra_pi0_RHIC_SPS");

  TCanvas *c22 = (TCanvas*)canvas(title, 600, 700);
  c22->Range( -1.23529, -9.51842, 9.28235, 2.69298);
  c22->Range(-3.18658,-10.9312,16.5367,2.77103);
  c22->SetLeftMargin(0.161565);
  c22->SetRightMargin(0.0272109);
  c22->SetTopMargin(0.0562701);
  c22->SetBottomMargin(0.118971);

  double ptmin = 0.;
  double ptmax = 16.;
  double yieldmin = 5E-10;
  double yieldmax = 100;

  TH2F *myframe = (TH2F*)frame(title, 21, ptmin, ptmax, 100, yieldmin, yieldmax);
  myframe->GetYaxis()->SetTitleOffset(1.4);
  myframe->Draw();
  c22->SetLogy();
  c22->Update();

  TGraphErrors *eband;
  fPlot = false;
  fFit = false;

  TGraphErrors *pbsc_200 = (TGraphErrors*)plot_spectrum_from_ascii("pi0_emcal_0_10_tof1chisq1_extlvl2.txt",eband);
  TGraphErrors *emcal_130 = (TGraphErrors*)plot_spectrum_from_ascii("PizeroAuAu130_cent.dat",eband);
  TGraphErrors *phnx_62 = (TGraphErrors*)plot_spectrum_from_ascii("pi0_emcal_0_10_noPID_auau62GeV.txt",eband);
  TGraphErrors *wa98_17 = (TGraphErrors*)plot_spectrum_from_ascii("wa98_pi0_et010.dat",eband);

  c22->cd();
  double scale = 1.2;
  setMarkerLineType(wa98_17, 22, 51, 1.9*scale, 1, 1);
  wa98_17->Draw("P");
  setMarkerLineType(phnx_62, 24, 6, 2., 1, 1);
  phnx_62->Draw("P");
  setMarkerLineType(emcal_130, 21, 5, 1.5*scale, 1, 1);
  emcal_130->Draw("P");
  setMarkerLineType(pbsc_200, 20, 2, 1.6*scale, 1, 1);
  pbsc_200->Draw("P");

  TGraphErrors *emcal_130b=(TGraphErrors*)emcal_130->Clone();
  emcal_130b->SetMarkerStyle(25);
  emcal_130b->SetMarkerColor(2);
  emcal_130b->Draw("P");

  TString label = "#pi^{0} spectra 0-10% central A+A: ";
  TLegend *legend = new TLegend(0.321818,0.749164,0.896364,0.989967, label, "brNDC");
  legend->SetMargin(0.1);
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);
  legend->SetTextSize(0.034);
  legend->Draw();
  char sublabel[50];
  sprintf(sublabel, "PHENIX Preliminary AuAu, #sqrt{s} = 200 GeV");
  legend->AddEntry(pbsc_200, sublabel, "P");

  sprintf(sublabel, "PHENIX AuAu, #sqrt{s} = 130 GeV");
  legend->AddEntry(emcal_130, sublabel, "P");

  sprintf(sublabel, "PHENIX Preliminary AuAu, #sqrt{s} = 62.4 GeV");
  legend->AddEntry(phnx_62, sublabel, "P");

  sprintf(sublabel, "WA98 PbPb, #sqrt{s} = 17.3 GeV");
  legend->AddEntry(wa98_17, sublabel, "P");

  c22->Update();

}



//_____________________________________________________________________________
// type = "corr", "uncorr"

void
emcAnalyzer::plot_pp_picharged_spectra_ISR( const TString type )
{

  gStyle->SetOptFit(111);
  //fPlot = true;
  fPlot = false;
  fFit = false;

  //_____________________________________________________________________________
  // General spectra plot (pi+/- data)

  char title1[100];
  sprintf(title1, "charged_pion_spectra_ISR");

  TCanvas *c_pichg = (TCanvas*)canvas(title1, 600, 700);
  c_pichg->Range( -1.23529, -9.51842, 9.28235, 2.69298);
  c_pichg->SetLeftMargin(0.11745);
  c_pichg->SetRightMargin(0.0268456);
  c_pichg->SetTopMargin(0.0567485);
  c_pichg->SetLogy();

  TH2F *myframe1 = (TH2F*)frame(title1, 27, 0., 13.5, 100, 5E-14, 300,
				"p_{T} (GeV/#font[72]{c})", "Ed^{3}#sigma/d^{3}p (mb GeV^{-2}c^{3})");
  myframe1->GetYaxis()->SetTitleOffset(1.4);
  myframe1->Draw();

  TString label = "ISR charged pion spectra @ #sqrt{s} #approx 62.4 GeV";
  TLegend *legend1 = new TLegend(0.409548, 0.708589, 0.90, 0.90, label, "brNDC");
  legend1->SetMargin(0.1);
  legend1->SetFillStyle(0);
  legend1->SetBorderSize(0);
  legend1->SetTextSize(0.034);
  legend1->Draw();

  c_pichg->Update();

  //_____________________________________________________________________________
  // Average of pi- + pi+ data sets ...

  const int N = 8 ;
  TGraphErrors *isr_picharged[N];
  TGraphErrors *isr_picharged_average[N/2];

  TGraphErrors *isr_picharged_average2[N/2];
  TLegend *leg_picharged_average[N/2];

  TGraphErrors *errBand;

  TString file_picharged[N] = {
    "pp_pi-_isr_62.4GeV_busser76.txt",
    "pp_pi+_isr_62.4GeV_busser76.txt",
    "pp_pi-_isr_63GeV_alper75.txt",
    "pp_pi+_isr_63GeV_alper75.txt",
    "pp_pi-_isr_62GeV_banner77.txt",
    "pp_pi+_isr_62GeV_banner77.txt",
    "pp_pi-_isr_63GeV_50deg_drijard82.txt",
    "pp_pi+_isr_63GeV_50deg_drijard82.txt"
  };

  TString leg_picharged[N/2] =
    {
      "p+p #rightarrow #pi^{#pm}+X #sqrt{s} = 62 GeV -- CCRS [busser76]",
      "p+p #rightarrow #pi^{#pm}+X #sqrt{s} = 63 GeV -- Brit.-Scand. [alper75]",
      "p+p #rightarrow #pi^{#pm}+X #sqrt{s} = 62 GeV -- Saclay [banner77]",
      "p+p #rightarrow #pi^{#pm}+X #sqrt{s} = 63 GeV -- SFM [drijard82]"
    };

  for (int i = 0; i < N; i++)
    {
      isr_picharged[i] = (TGraphErrors*)plot_spectrum_from_ascii((char*)file_picharged[i].Data(), errBand);
    }

  TCanvas *c[N/2];
  char ctitle[300];

  int j = 0;
  for (int i = 0; i < N/2; i++)
    {
      j = i*2;
      isr_picharged_average[i] = add(isr_picharged[j], isr_picharged[j+1]);
      scale(*isr_picharged_average[i], 0.5);

      setMarkerLineType(isr_picharged_average[i], 29, 2, 2.);
      setMarkerLineType(isr_picharged[j], 22, 3, 1.8);
      setMarkerLineType(isr_picharged[j+1], 23, 4, 1.8);

      sprintf(ctitle,"average_%s_%s",(char*)file_picharged[j].Data(),(char*)file_picharged[j+1].Data());
      c[i] = (TCanvas*)canvas(ctitle,600,700);
      c[i]->SetLogy();
      c[i]->cd();
      isr_picharged_average[i]->Draw("AP");
      isr_picharged[j]->Draw("P");
      isr_picharged[j+1]->Draw("P");
      isr_picharged_average[i]->Draw("P");
      isr_picharged_average[i]->GetYaxis()->SetTitle("Ed^{3}#sigma/d^{3}p (mb GeV^{-2}c^{3})");
      isr_picharged_average[i]->GetXaxis()->SetTitle("p_{T} (GeV/#font[72]{c})");
      leg_picharged_average[i] = (TLegend*)legend1->Clone();
      leg_picharged_average[i]->AddEntry(isr_picharged_average[i], leg_picharged[i], "P");
      leg_picharged_average[i]->Draw();
      c[i]->Update();

      //dumpGraph(isr_picharged_average[i]);

    }

  int k=12;

  for (int i = 0; i < N/2; i++)
    {
      c_pichg->cd();
      isr_picharged_average2[i] =(TGraphErrors*)isr_picharged_average[i]->Clone();
      isr_picharged_average2[i]->Draw("P");
      setMarkerLineType(isr_picharged_average2[i], 20, k++);
      legend1->AddEntry(isr_picharged_average2[i], leg_picharged[i], "P");
      c_pichg->Update();
    }

}

//_____________________________________________________________________________
// type = "corr", "uncorr", "nobadset"
// returns "average" spectrum

TGraphErrors*
emcAnalyzer::plot_pp_pi0_spectra_ISR( const TString type )
{

  gStyle->SetOptFit(111);

  fPlot = false;
  fFit = false;

  //_____________________________________________________________________________
  // General spectra plot (all data)

  char title[100];
  sprintf(title, "spectra_ISR");

  TCanvas *c_pi0corr = (TCanvas*)canvas(title, 600, 700);
  c_pi0corr->Range( -1.23529, -9.51842, 9.28235, 2.69298);
  c_pi0corr->SetLeftMargin(0.11745);
  c_pi0corr->SetRightMargin(0.0268456);
  c_pi0corr->SetTopMargin(0.0567485);
  c_pi0corr->SetLogy();

  double xmax = 17.5;
  double xmin = 5E-14;
  char xtitle[100];
  sprintf(xtitle, "p_{T} (GeV/#font[72]{c})");

  if (fXaxis.Contains("xT", TString::kIgnoreCase))
    {
      xmax = 1.;
      xmin = 5E-16;
      sprintf(xtitle, "x_{T}=2p_{T}/#sqrt{s}");
      c_pi0corr->SetLogx();
    }

  TH2F *myframe = (TH2F*)frame(title, 33, 0., xmax, 100, xmin, 500, xtitle, "Ed^{3}#sigma/d^{3}p (mb GeV^{-2}c^{3})");
  myframe->GetYaxis()->SetTitleOffset(1.6);
  myframe->Draw();

  TString label = "ISR pion spectra @ #sqrt{s} #approx 62.4 GeV";
  TLegend *legend = new TLegend(0.409548, 0.708589, 0.90, 0.90, label, "brNDC");
  legend->SetMargin(0.1);
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);
  legend->SetTextSize(0.034);
  legend->Draw();

  c_pi0corr->Update();

  //_____________________________________________________________________________
  // General spectra plot (NLO)

  char title2[100];
  sprintf(title2, "spectra_ISR_NLO");

  TCanvas *c_pi0nlo = (TCanvas*)canvas(title2, 600, 700);
  c_pi0nlo->Range( -1.23529, -9.51842, 9.28235, 2.69298);
  c_pi0nlo->SetLeftMargin(0.11745);
  c_pi0nlo->SetRightMargin(0.0268456);
  c_pi0nlo->SetTopMargin(0.0567485);
  c_pi0nlo->SetLogy();

  myframe->Draw();

  label = "pQCD NLO #pi^{0} spectra @ #sqrt{s} = 62.4 GeV [W.Vogelsang]";
  TLegend *legend2 = new TLegend(0.409548, 0.708589, 0.90, 0.90, label, "brNDC");
  legend2->SetMargin(0.1);
  legend2->SetFillStyle(0);
  legend2->SetBorderSize(0);
  legend2->SetTextSize(0.034);
  legend2->Draw();

  c_pi0nlo->Update();

  //_____________________________________________________________________________
  // REAL DATA SETS

  const int Nsets = 13 + 3;

  TGraphErrors *isr_spectrum[Nsets];
  TGraphErrors *isr_spectrum_corr[Nsets];
  TGraphErrors *isr_spectrum_nlo[Nsets];

  TString file[Nsets] =
    {
      "pp_meson0_isr_62.4GeV_angelis89.txt",
      "pp_pi0_isr_63GeV_akesson89.txt",
      "pp_pi0_isr_62.8GeV_kourkoumelis80.txt",
      "pp_pi0_isr_62.4GeV_angelis78.txt",
      "pp_pi0_isr_63GeV_clark78.txt",
      "pp_pi0_isr_62.4GeV_busser76.txt",
      "pp_pi0_isr_62.9GeV_eggert75.txt",
      "pp_pi0_isr_62.4GeV_busser73.txt",

      "pp_pi0_isr_62.4GeV_kourkoumelis79.txt",

      "pp_pichg_isr_63GeV_50deg_drijard82.txt",
      "pp_pichg_isr_62GeV_banner77.txt",
      "pp_pichg_isr_62.4GeV_busser76.txt",
      "pp_pichg_isr_63GeV_alper75.txt",

      "pp_chghad_isr_62GeV_breakstone95_eta0.25.txt",
      "pp_chghad_isr_62GeV_breakstone95_eta0.75.txt",
      "pp_chhad_isr_63GeV_akesson82.txt"
    };

  TString file_corr[Nsets] =
    {
      "pp_meson0_isr_62.4GeV_angelis89_corr.txt",
      "pp_pi0_isr_63GeV_akesson89_corr.txt",
      "pp_pi0_isr_62.8GeV_kourkoumelis80_corr.txt",
      "pp_pi0_isr_62.4GeV_angelis78_corr.txt",
      "pp_pi0_isr_63GeV_clark78_corr.txt",
      "pp_pi0_isr_62.4GeV_busser76_corr.txt",
      "pp_pi0_isr_62.9GeV_eggert75_corr.txt",
      "pp_pi0_isr_62.4GeV_busser73_corr.txt",

      "pp_pi0_isr_62.4GeV_kourkoumelis79_corr.txt",

      "pp_pichg_isr_63GeV_50deg_drijard82_corr.txt",
      "pp_pichg_isr_62GeV_banner77_corr.txt",     // no need to correct for != sqrts (62 <--> 62.4 GeV): pTmax=1.5 GeV/c only
      "pp_pichg_isr_62.4GeV_busser76_corr.txt",
      "pp_pichg_isr_63GeV_alper75_corr.txt",

      "pp_chghad_isr_62GeV_breakstone95_eta0.25_corr.txt", // no need to correct for != sqrts ... low pTmax
      "pp_chghad_isr_62GeV_breakstone95_eta0.75_corr.txt", // no need to correct for != sqrts ... low pTmax
      "pp_chhad_isr_63GeV_akesson82_corr.txt" // no need to correct for != sqrts ... low pTmax
    };

  TString leg[Nsets] =
    {
      "p+p #rightarrow #pi^{0}+X #sqrt{s} = 62.4 GeV -- CMOR [angelis89]",
      "p+p #rightarrow #pi^{0}+X #sqrt{s} = 63 GeV -- AFS [akesson89]",
      "p+p #rightarrow #pi^{0}+X #sqrt{s} = 62.8 GeV -- R-806 [kourkou80]",
      "p+p #rightarrow #pi^{0}+X #sqrt{s} = 62.4 GeV -- CCOR [angelis78]",
      "p+p #rightarrow #pi^{0}+X #sqrt{s} = 63 GeV -- CSZ [clark78]",
      "p+p #rightarrow #pi^{0}+X #sqrt{s} = 62.4 GeV -- CCRS [busser76]",
      "p+p #rightarrow #pi^{0}+X #sqrt{s} = 62.9 GeV -- ACHM [eggert75]",
      "p+p #rightarrow #pi^{0}+X #sqrt{s} = 62.4 GeV -- CCR [busser73]",

      "p+p #rightarrow #pi^{0}+X #sqrt{s} = 62.4 GeV -- R-806 [kourkou79]",

      "p+p #rightarrow #pi^{#pm}+X #sqrt{s} = 63 GeV -- SFM [drijard82]",
      "p+p #rightarrow #pi^{#pm}+X #sqrt{s} = 62 GeV -- Saclay [banner77]",
      "p+p #rightarrow #pi^{#pm}+X #sqrt{s} = 62 GeV -- CCRS [busser76]",
      "p+p #rightarrow #pi^{#pm}+X #sqrt{s} = 63 GeV -- Brit.-Scand. [alper75]",

      "p+p #rightarrow h^{#pm}/1.6+X #sqrt{s} = 62 GeV (#eta = 0.25) -- CDHW [breakstone95]",
      "p+p #rightarrow h^{#pm}/1.6+X #sqrt{s} = 62 GeV (#eta = 0.75) -- CDHW [breakstone95]",
      "p+p #rightarrow h^{#pm}/1.6+X #sqrt{s} = 63 GeV (#eta = 0.) -- SFM [akesson82]"
    };

  //_____________________________________________________________________________
  // NLO DATA SETS

  const int NsetsNLO = 6;

  TString file_nlo[NsetsNLO] =
    {
      "pQCD_vogelsang_pp_pi0_62.4GeV_kkp_sc05.dat",
      "pQCD_vogelsang_pp_pi0_62.4GeV_kkp_sc1.dat",
      "pQCD_vogelsang_pp_pi0_62.4GeV_kkp_sc2.dat",
      "pQCD_vogelsang_pp_pi0_62.4GeV_k_sc05.dat",
      "pQCD_vogelsang_pp_pi0_62.4GeV_k_sc1.dat",
      "pQCD_vogelsang_pp_pi0_62.4GeV_k_sc2.dat"
    };

  TString leg_nlo[NsetsNLO] =
    {
      "p+p#rightarrow#pi^{0}+X CTEQ6, KKP, #mu=p_{T}/2",
      "p+p#rightarrow#pi^{0}+X CTEQ6, KKP, #mu=p_{T}",
      "p+p#rightarrow#pi^{0}+X CTEQ6, KKP, #mu=2p_{T}",
      "p+p#rightarrow#pi^{0}+X CTEQ6, Kretzer, #mu=p_{T}/2",
      "p+p#rightarrow#pi^{0}+X CTEQ6, Kretzer, #mu=p_{T}",
      "p+p#rightarrow#pi^{0}+X CTEQ6, Kretzer, #mu=2p_{T}"
    };

  //_____________________________________________________________________________

  TGraphErrors *ratio_tot[Nsets];
  TGraphErrors *ratio_tot_corr[Nsets];
  TGraphErrors *ratio_tot_nlo[Nsets];

  TCanvas *ratio_data_fit_total = (TCanvas*)canvas("ratio_data_fit_total", 700, 600);
  ratio_data_fit_total->cd();
  TH2F *myframe_ratio = (TH2F*)frame("myframe_ratio", 33, 0., xmax, 10, 0., 4., xtitle, "ratio data/fit");
  myframe_ratio->Draw();

  TCanvas *ratio_data_corr_fit_total = (TCanvas*)canvas("ratio_data_corr_fit_total", 700, 600);
  ratio_data_corr_fit_total->cd();
  myframe_ratio->Draw();

  TCanvas *ratio_nlo_fit_total = (TCanvas*)canvas("ratio_nlo_fit_total", 700, 600);
  ratio_nlo_fit_total->cd();
  TH2F *myframe_nlo = (TH2F*)frame("myframe_ratio", 33, 0., xmax, 10, 0., 4., xtitle, "ratio NLO/fit");
  myframe_nlo->Draw();

  TLine *line = new TLine(0., 1., 17.5, 1.);
  line->SetLineColor(1);
  line->SetLineStyle(1);
  line->SetLineWidth(3);
  TLine *line0 = new TLine(0., 0.75, 17.5, 0.75);
  line0->SetLineColor(1);
  line0->SetLineStyle(2);
  line0->SetLineWidth(2);
  TLine *line1 = new TLine(0., 1.25, 17.5, 1.25);
  line1->SetLineColor(1);
  line1->SetLineStyle(2);
  line1->SetLineWidth(2);

  //_____________________________________________________________________________
  // FINAL PARAMETRIZATION

  double ptmin = 0., ptmax = 16.; 

  //TF1 *fit_total = new TF1("fit_total", "[0]/(1+x/[1])^[2]", ptmin, ptmax);

  //TF1 *fit_total = new TF1("fit_total", "[0]/(exp([2]*x)+x/[1])^[3]", ptmin, ptmax);
  //fit_total->SetParameters(3.56223e+02,1.52689,4.51130e-02,1.26041e+01); // final corrected spectra (all but drijard82)

  TF1 *fit_total = new TF1("fit_total", "[0]/(exp([2]*x*x+[3]*x)+x/[1])^[4]", ptmin, ptmax);
  //fit_total->SetParameters(2.97778e+02,2.38846,-1.27724e-02,6.13624e-02,1.68036e+01); // final gamma-corr. spectra (all but drijard82)
  //fit_total->SetParameters(2.42166e+02,3.12165,-1.07946e-02,6.17795e-02,1.92115e+01); // idem minus Eggert75
  //fit_total->SetParameters(2.40426e+02,3.00176,-1.07274e-02,5.70144e-02,1.88046e+01); // idem minus Busser76-picharged
  //fit_total->SetParameters(2.37135e+02,3.15524,-1.11667e-02,5.68221e-02,1.94920e+01); // idem with fit from pT = 0 to 16 GeV/c
  //fit_total->SetParameters(2.76923e+02,9.44154e-01,2.67160e-02,-3.31081e-01,1.07645e+01); // idem adding a few points missed
  //fit_total->SetParameters(2.76543e+02,2.27665,-1.03118e-02,4.58470e-02,1.62028e+01); // idem fit from pT = 0 to 14 GeV/c
  //fit_total->SetParameters(2.76786e+02,2.27293,-1.03295e-02,4.59882e-02,1.61817e+01); // idem (final eta correction)
  //fit_total->SetParameters(2.76944e+02,9.41907e-01,2.68670e-02,-3.32583e-01,1.07492e+01); // idem fit from pT = 0 to 15 GeV/c
  //fit_total->SetParameters(3.02408e+02,1.05222,1.89546e-02,-2.1813e-01,1.12354e+01); // fit pT = 0 to 16 GeV/c + pQCD (4 points)
  //fit_total->SetParameters(3.05916e+02,2.02247,-9.51683e-03,3.58007e-02,1.52820e+01); // idem fit pT = 0.2 to 14.8 GeV/c 
  //fit_total->SetParameters(3.12217e+02,9.52688e-01,2.42348e-02,-2.95935e-01,1.08287e+01); // idem fit pT = 0.2 to 15.2 GeV/c 
  //fit_total->SetParameters(2.93419e+02,1.07127,1.74233e-02,-2.23236e-01,1.13465e+01); // idem force through highest alper75
  //fit_total->SetParameters(288.53,2.39469,-1.11643e-02,5.33446e-02,1.67403e+01); // no force through highest alper75 + busser76 pichg
  //fit_total->SetParameters(288.062,2.41324,-1.12472e-02,5.41171e-02,1.68118e+01); // idem + no busser73
  //fit_total->SetParameters(256.838,2.30679,-7.68381e-03,4.06836e-02,1.60965e+01); // no force through highest alper75 + busser76 pichg
  fit_total->SetParameters(273.251,2.34601,-9.54848e-03,4.67200e-02,1.64203e+01); // no force through highest alper75 + busser76 pichg

  //fit_total->SetParameters(268.160,2.02343,-3.41989e-03,1.52620e-01,1.24347e+01); // Scaled h+/- (AFS, CDHW1, CDHW2) added

  //TF1 *fit_total = new TF1("fit_total","[0]*exp([1]*x)/([2]*x^[3]+x^[4])", ptmin, ptmax);
  //fit_total->SetParameters(1.14014,-0.5728,0.2906,7.0775,3.0634); // with h+/- AND drijard 50deg
  //fit_total->SetParameters(1.1202,-0.46389,0.346616,7.38933,3.08223); // withOUT: h+/- and drijard 50deg

  //TF1 *fit_total = new TF1("fit_total", "[0]/(exp([1]*x)+[2]*x)^[3]*x^[4]", ptmin, ptmax);
  //fit_total->SetParameters(115.23,3.13510e-06,4.89630e-01,1.36691e+01,-4.72572e-01); // withOUT: h+/- and drijard 50deg

  //TF1 *fit_total = new TF1("fit_total", "[0]/(1+x/[1])^[2]*x^[3]", ptmin, ptmax);
  //fit_total->SetParameters(115.23,2.04,1.36691e+01,-4.72572e-01);

//   // Bill's fit
//   // dN/dp_T^2 = a exp{ - (2 \Lambda / b ) \sqrt[ log(1 + (p_T/\Lambda)^2 )) ] }
//   TF1 *fit_total = new TF1("fit_total", "[0]*exp( -(2*[1]/[2])*sqrt(log(1+(x/[1])^2 )) )", ptmin, ptmax);
//   fit_total->SetParameters(1,30,3);

//   TF1 *fit_total = new TF1("fit_total","[0]*exp([1]*x+[2]*x*x)/(1+x/[3])^[4]", ptmin, ptmax);
//   fit_total->SetParameters(302.178,-0.6695,0.01905,1.25863,9.84134);

  //fit_total->SetParameters(254.2,2.18,-4.41e-03,1.78e-01,12.44); // ALL DATA SETS included
  //fit_total->SetParameters(239.1,2.367,-5.247e-03,0.1823,12.77); // Above minus 2 "bad", minus kourkou79
  //fit_total->SetParameters(253.7,1.295,0.00778,-0.1333,12.31); // Above minus 63GeV_50deg_drijard82.
  //fit_total->SetParameters(258.9,1.384,0.006616,-0.08525,12.47); // Above minus busser76_pichg
  //fit_total->SetParameters(288.5,2.11,-0.01012,0.03472,15.69); // Above with chhad with +/-25%
  fit_total->SetParameters(265.1,2.639,-0.0129,0.04975,17.95); // Above with 4 last NLO points (pT>13.5 GeV/c)
  //fit_total->SetParameters(278.6,2.273,-0.01187,0.03389,16.6); // Above plus kourkou79
  //fit_total->SetParameters(274.8,2.384,-0.01135,0.03988,16.99); // Above recomputed errors
  //fit_total->SetParameters(273.4,2.438,-0.01127,0.04264,17.18); // Above minus kourkou79
  //fit_total->SetParameters(256.3,2.579,-0.01114,0.04526,17.65); // Above plus ptmin(fit)=0
  //fit_total->SetParameters(64.03,15.36,-0.003582,-0.01956,120.9); // Above plus ptmin(fit)=1 -- ptmax=10 (bullshit !)

  fit_total->SetLineColor(4);

  //_____________________________________________________________________________
  // Charged hadron parametrization @ 62 GeV by Axel Drees (divided by h/pi~1.6 ratio)

  double ptminch=1.5, ptmaxch=10.;

  TF1 *chhag = (TF1*)plot_pp_sigma_parametrization("had","axel",62.4,1./1.6,0.);
  chhag->SetName("f_axel_pp_62");
  //chhag->Draw("same");

  // ratio charged to neutral pion fit
  TF1 *ch_over_pi0 = new TF1("ch_over_pi0","f_axel_pp_62/fit_total", ptminch, ptmaxch);
  ch_over_pi0->SetLineColor(16);
  ch_over_pi0->SetLineWidth(6);

  TF1 *ch_over_pi0up = new TF1("ch_over_pi0up","[0]*f_axel_pp_62/fit_total", ptminch, ptmaxch);
  ch_over_pi0up->SetParameter(0,1.16);
  ch_over_pi0up->SetLineColor(16);
  ch_over_pi0up->SetLineWidth(4);
  ch_over_pi0up->SetLineStyle(2);

  TF1 *ch_over_pi0dwn = new TF1("ch_over_pi0dwn","[0]*f_axel_pp_62/fit_total", ptminch, ptmaxch);
  ch_over_pi0dwn->SetParameter(0,0.84);
  ch_over_pi0dwn->SetLineColor(16);
  ch_over_pi0dwn->SetLineWidth(4);
  ch_over_pi0dwn->SetLineStyle(2);

  ratio_data_corr_fit_total->cd();
  ch_over_pi0->Draw("same");
  ch_over_pi0up->Draw("same");
  ch_over_pi0dwn->Draw("same");

  //_____________________________________________________________________________
  // test Blattnig ...

  double sqrts = 62.4;
  TF1* f_blatt = (TF1*)plot_pp_sigma_parametrization("pi0", "blatt", sqrts);
  f_blatt->SetLineColor(9);

  //_____________________________________________________________________________
  // ratio data/fit

  TGraphErrors *eband;

  int col = 0;
  int mark = 19;

  for (int i = 0; i < Nsets; i++)
    {
      isr_spectrum[i] = (TGraphErrors*)plot_spectrum_from_ascii((char*)file[i].Data(), eband);
      isr_spectrum_corr[i] = (TGraphErrors*)plot_spectrum_from_ascii((char*)file_corr[i].Data(), eband);

      // scale h+/- data sets by h/pi ratio
      if (i >= (Nsets-3))
	{
	  double norm = 1./1.6;
	  if (i==(Nsets-1)) norm/=2.; // (h+ + h-)/2 for Akesson82
	  scale(*isr_spectrum[i], norm);
	  scale(*isr_spectrum_corr[i], norm);
	  double hpi_ratio_uncertainty = 0.25;
	  AddOrSubstractRelatError(*isr_spectrum_corr[i],hpi_ratio_uncertainty,1);
	}

      col = i;

      if (isr_spectrum[i] && isr_spectrum_corr[i])
        {
          col++;
          if (col == 9)
            col = 12;
          if (col == 10)
            col = 50;
          mark++;
          mark = 20;

          setMarkerLineType(isr_spectrum[i], mark, col);
          setMarkerLineType(isr_spectrum_corr[i], mark, col);

          ratio_tot[i] = (TGraphErrors*)ratio(isr_spectrum[i], fit_total);
          //ratio_tot[i] = (TGraphErrors*)ratio(isr_spectrum[i], fit_total2);
          ratio_tot[i]->SetMarkerColor(col);

          ratio_tot_corr[i] = (TGraphErrors*)ratio(isr_spectrum_corr[i], fit_total);
          //ratio_tot_corr[i] = (TGraphErrors*)ratio(isr_spectrum_corr[i], fit_total2);
          ratio_tot_corr[i]->SetMarkerColor(col);

	  if (type.Contains("uncorr"))
	    {
	      c_pi0corr->cd();
	      isr_spectrum[i]->Draw("P");
	      legend->AddEntry(isr_spectrum[i], leg[i], "P");

	      ratio_data_fit_total->cd();
	      ratio_tot[i]->Draw("P");

	      ratio_data_corr_fit_total->cd();
	      ratio_tot_corr[i]->Draw("P");
	    }
	  else if (type.Contains("corr"))
	    {
	      c_pi0corr->cd();
	      isr_spectrum_corr[i]->Draw("P");
	      legend->AddEntry(isr_spectrum[i], leg[i], "P");

	      ratio_data_fit_total->cd();
	      ratio_tot[i]->Draw("P");

	      ratio_data_corr_fit_total->cd();
	      ratio_tot_corr[i]->Draw("P");
	    }
	  else if ( (type.Contains("nobad")) && 
		    ( !(file_corr[i].Contains("eggert75", TString::kIgnoreCase)) &&
		      !(file_corr[i].Contains("busser73", TString::kIgnoreCase)) &&
		      //!(file_corr[i].Contains("kourkoumelis79", TString::kIgnoreCase)) &&
		      !(file_corr[i].Contains("63GeV_50deg_drijard82", TString::kIgnoreCase))
		      //!(file_corr[i].Contains("pichg_isr_62.4GeV_busser76", TString::kIgnoreCase))
		     )
		   )
	    
	    {
	      c_pi0corr->cd();
	      isr_spectrum_corr[i]->Draw("P"); // exclude "bad" data sets
	      legend->AddEntry(isr_spectrum[i], leg[i], "P"); // exclude "bad" data sets

	      ratio_data_fit_total->cd();
	      ratio_tot[i]->Draw("P");

	      ratio_data_corr_fit_total->cd();
	      ratio_tot_corr[i]->Draw("P");
	    }

          //f_blatt->Draw("same");
        }
      else
        {
          cout << "<W> PROBLEMS WITH GRAPH " << file[i] << endl;
        }
    }

  c_pi0corr->cd();
  legend->Draw();
  fit_total->Draw("same");
  c_pi0corr->Update();

  ratio_data_fit_total->cd();
  TLegend *legend3 = (TLegend*)legend->Clone();
  legend3->Draw();
  line->Draw();
  line0->Draw();
  line1->Draw();
  ratio_data_fit_total->Update();

  ratio_data_corr_fit_total->cd();

  TLatex *tex = new TLatex(0.499406,0.174573,"(h^{#pm} parametrization)/1.6");
  tex->SetTextSize(0.032);
  tex->SetLineWidth(2);
  //tex->Draw();
  TArrow *arrow = new TArrow(2.39368,0.35443,1.8908,0.904159,0.03,"|>");
  arrow->SetFillColor(17);
  arrow->SetFillStyle(1001);
  arrow->SetLineColor(17);
  arrow->SetLineWidth(3);
  //arrow->Draw();

  legend3->Draw();
  line->Draw();
  line0->Draw();
  line1->Draw();

  ratio_data_corr_fit_total->Update();

  //_____________________________________________________________________________
  // ratio NLO/fit

  //fPlot = true; // test NLO plots ...

  for (int i = 0; i < NsetsNLO; i++)
    {
      isr_spectrum_nlo[i] = (TGraphErrors*)plot_spectrum_from_ascii((char*)file_nlo[i].Data(), eband);

      col = i;

      if (isr_spectrum_nlo[i])
        {
          col++;
          if (col == 9)
            col = 12;
          if (col == 10)
            col = 50;

          //mark++;
          mark = 30;

          if (i<3) setMarkerLineType(isr_spectrum_nlo[i], 20, col, 0, 2, col, 3); // kkp
          else setMarkerLineType(isr_spectrum_nlo[i], mark, col, 0, 2, col, 3); // kretzer

          c_pi0nlo->cd();
          isr_spectrum_nlo[i]->Draw("L");
          legend2->AddEntry(isr_spectrum_nlo[i], leg_nlo[i], "L");

          ratio_nlo_fit_total->cd();
          ratio_tot_nlo[i] = (TGraphErrors*)ratio(isr_spectrum_nlo[i], fit_total);
          //ratio_tot_nlo[i] = (TGraphErrors*)ratio(isr_spectrum_nlo[i], fit_total2);
	  if (i<3) setMarkerLineType(ratio_tot_nlo[i],20,col, 0, 1, col, 3); // kkp
	  else setMarkerLineType(ratio_tot_nlo[i],mark,col, 0, 1, col, 3); // kretzer
          ratio_tot_nlo[i]->Draw("L");
        }
      else
        {
          cout << "<W> PROBLEMS WITH GRAPH " << file_nlo[i] << endl;
        }
    }

  c_pi0nlo->cd();
  legend2->Draw();
  fit_total->Draw("same");
  c_pi0nlo->Update();

  ratio_nlo_fit_total->cd();
  legend2->Draw();
  line->Draw();
  line0->Draw();
  line1->Draw();
  ratio_nlo_fit_total->Update();

  //_____________________________________________________________________________
  // Final fit to total data ...

  TCanvas *ctot = (TCanvas*)canvas("pions_isr_total", 600, 700);
  ctot->cd();
  myframe->Draw();
  ctot->SetLogy();
  ctot->Update();
  if (fXaxis.Contains("xT", TString::kIgnoreCase))  ctot->SetLogx();

  for (int i = 0; i < Nsets; i++)
    {
      if (isr_spectrum_corr[i])
	{
	  cout << "dumping CORRECTED graph " << file[i] << endl;
	  dumpGraph(isr_spectrum_corr[i], "x-y-ey");
	}
      else
	{
	  cout << "<W> GRAPH " << file[i] << " MISSING " << endl;
	}
    }

  fFit = false;

  TGraphErrors *isr_pi0_total = (TGraphErrors*)plot_spectrum_from_ascii("pp_pi0_isr_62.4GeV_total.txt", eband);
  ctot->cd();
  isr_pi0_total->Draw("P");

  ptmin = 1.;
  ptmax = 10.; // 15.2;

  double A = 300;
  double p0 = 2.5;
  double n2 = 0.;
  double n_exp = 15;

  // fits that do NOT work:
  //TF1 *exp_pow = new TF1("hagexp", "[0]*exp([1]*x)/(x/[2])^[3]", ptmin, ptmax);
  //TF1 *b_cole = new TF1("hagexp", "[0]/(x*x+[1]/pow(log(x*x/[2])),2.))^[3]", ptmin, ptmax);
  // try of Justin's fit ...
  //   double ptmin0 = 0.5;
  //   double ptmax0 = 6.;
  //   TF1 *hag = new TF1("hag", "[0]/(1+x/[1])^[2]", ptmin0, ptmax0);
  //   hag->SetParameters(10, p0, n_exp);
  //   //isr_pi0_total->Fit("hag", "Q+", "", ptmin0, ptmax0);
  //   hag->SetLineColor(3);
  
  //   double ptmin1 = 6.;
  //   double ptmax1 = 14.;
  //   TF1 *pow = new TF1("pow", "[0]/(x^[1])", ptmin1, ptmax1);
  //   pow->SetParameters(p0, n_exp);
  //   //isr_pi0_total->Fit("pow", "Q+", "", ptmin1, ptmax1);
  //   pow->SetLineColor(8);

  // Bill's fit:
  // dN/dp_T^2 = a exp{ - (2 \Lambda / b ) \sqrt[ log(1 + (p_T/\Lambda)^2 )) ] }
  //TF1 *hagexp = new TF1("hagexp", "[0]*exp( -(2*[1]/[2])*sqrt(log(1+(x/[1])^2 )) )", ptmin, ptmax);
  //hagexp->SetParameters(65,2.5,0.5);
  //TF1 *hagexp = new TF1("hagexp", "[0]*exp( [1]*sqrt(log([2]+[3]*x*x)) )", ptmin, ptmax);
  //hagexp->SetParameters(45.,-17.,1.,0.2);
  //hagexp->SetParameters(0.6,-15,1,0.03);
  //TF1 *hagexp = new TF1("hagexp","[0]*exp([1]*x)/(1+x/[2])^[3]", ptmin, ptmax);
  //hagexp->SetParameters(225.,-0.02,1.7,13);

  //TF1 *fit_total = new TF1("fit_total", "[0]/(x^[1]*(1+exp([2]-x)/[3])", ptmin, ptmax);
  //fit_total->SetParameters(A,n_exp,n2,n2);

  TF1 *hagexp = new TF1("hagexp", "[0]/(exp([2]*x*x+[3]*x)+x/[1])^[4]", ptmin, ptmax);
  hagexp->SetParameters(A, p0, n2, n2, n_exp);

  //TF1 *hagexp = new TF1("hagexp","[0]*exp([1]*x)/([2]*x^[3]+x^[4])", ptmin, ptmax);
  //hagexp->SetParameters(1,-0.6,0.2,7,3);

  //TF1 *hagexp = new TF1("hagexp", "[0]/((1+[1]*x)^[2]*(1-x^[3])^[4])", ptmin, ptmax);
  //hagexp->SetParameters(100.,0.5,15,-0.5,1.);

  //TF1 *hagexp = new TF1("hagexp", "[0]/x^[1]*(1-x)^[2]*exp([3]*x)", ptmin, ptmax);
  //hagexp->SetParameters(300.,10.,0.,-0.2);
  //hagexp->SetParameters(115.23,3.13510e-06,4.89630e-01,1.36691e+01,0.);

  hagexp->SetLineWidth(3);

  //hagexp->SetParLimits(0, 0.0001, 200000000);
  //hagexp->SetParLimits(1, p0-2, p0+2);
  //hagexp->SetParLimits(2,-10.,-0.1);
  //hagexp->SetParLimits(3, n_exp - 6., n_exp + 6.);

  if (fXaxis.Contains("pT", TString::kIgnoreCase))
    {
      isr_pi0_total->Fit("hagexp", "Q+", "", ptmin, ptmax);
    }

  char ffunc[300];
  sprintf(ffunc,"Fit: f(p_{T})=%s",fit_total->GetTitle());
  //TLatex *t = new TLatex(4, 1e-6, "Fit function: f(p_{T})=p0/(exp(p2*p_{T})+p_{T}/p1)^{p3}");
  TLatex *t = new TLatex(4, 1e-6, ffunc);
  t->SetTextSize(0.03);
  ctot->cd();
  t->Draw();

  fit_total->Draw("same");

  ctot->Update();

  return isr_pi0_total;

}

//_____________________________________________________________________________
// type = "tot", "dir", "frag"

TGraphErrors*
emcAnalyzer::plot_gamma_over_pi0( const double sqrts, TString type )
{

  gStyle->SetOptFit(111);
  fPlot = false;

  TGraphErrors *eband;

  char title[100];
  sprintf(title, "%s_g_over_pi0_%3.1f",type.Data(),sqrts);

  TCanvas *c = (TCanvas*)canvas(title, 850, 580);
  TH2F *myframe = (TH2F*)frame("myframe", 15, 0., 15., 10, 0., 1., "p_{T} (GeV/#font[72]{c})", "ratio #gamma/#pi^{0}");
  c->cd();
  myframe->Draw();

  TString label = "#gamma/#pi^{0} in p+p collisions @ #sqrt{s} #approx 62.4 GeV";
  TLegend *legend = new TLegend(0.159664,0.623468,0.89916,0.961471,label,"brNDC");
  legend->SetMargin(0.1);
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);
  legend->SetTextSize(0.034);

  //_____________________________________________________________________________
  // exp. data

  const int Nexp = 3;

  TGraphErrors *ratio_exp_g_pi0[Nexp];

  TString file_exp[Nexp] =
    {
      "isr_ratio_gamma_over_pi0_cmor89_62GeV.txt",
      "isr_ratio_gamma_over_pi0_r806_79_62GeV.txt",
      "isr_ratio_gamma_over_pi0_r806_80_62GeV.txt"
    };

  TString leg_exp[Nexp] =
    {
      "CMOR Collab. [Angelis89]",
      "R806 Exp. [Diakonou79]",
      "R806 Exp. [Diakonou80]"
    };

  for (int i = 0; i < Nexp; i++)
    {
      ratio_exp_g_pi0[i] =(TGraphErrors*)plot_spectrum_from_ascii((char*)file_exp[i].Data(), eband);
      c->cd();
      ratio_exp_g_pi0[i]->Draw("P");
      setMarkerLineType(ratio_exp_g_pi0[i],21,i+4,2.);
      legend->AddEntry(ratio_exp_g_pi0[i], leg_exp[i], "P");

      //cout << "dumping graph " << ratio_exp_g_pi0[i]->GetName() << endl;
      //dumpGraph(ratio_exp_g_pi0[i], "x-y-ey");
    }

  //_____________________________________________________________________________
  // NLO

  const int Nnlo = 6;

  TGraphErrors *nlo_pi0[Nnlo/2];
  TGraphErrors *nlo_gamma[Nnlo/2];
  TGraphErrors *ratio_nlo_g_pi0[Nnlo/2];

  TString file_nlo[Nnlo] =
    {
      "pQCD_vogelsang_pp_gamma_63GeV_cteq6_sc2.dat",
      "pQCD_vogelsang_pp_gamma_63GeV_cteq6_sc1.dat",
      "pQCD_vogelsang_pp_gamma_63GeV_cteq6_sc05.dat",
      "pQCD_vogelsang_pp_pi0_62.4GeV_kkp_sc2.dat",
      "pQCD_vogelsang_pp_pi0_62.4GeV_kkp_sc1.dat",
      "pQCD_vogelsang_pp_pi0_62.4GeV_kkp_sc05.dat"
    };

  TString leg_nlo[Nnlo/2] =
    {
      "pQCD NLO (W.Vogelsang) -- CTEQ6, KKP, #mu = 2p_{T}",
      "pQCD NLO (W.Vogelsang) -- CTEQ6, KKP, #mu = 1^{ -0.5}_{ +1}p_{T}",
      "pQCD NLO (W.Vogelsang) -- CTEQ6, KKP, #mu = p_{T}/2"
    };

  for (int i = 0; i < Nnlo; i++)
    {
      if (i<3)
	{
	  file_nlo[i].Remove(file_nlo[i].Capacity() - 4); // "chop" the ".txt" suffix
	  if (type.Contains("dir", TString::kIgnoreCase))
	    {
	      file_nlo[i].Append("_dir");
	    }
	  else if (type.Contains("frag", TString::kIgnoreCase))
	    {
	      file_nlo[i].Append("_frag"); // add
	    }
	  file_nlo[i].Append(".dat"); // add back the ".dat" suffix

	  nlo_gamma[i] = (TGraphErrors*)plot_spectrum_from_ascii((char*)file_nlo[i].Data(), eband);
	}
      else
	{ 
	  nlo_pi0[i-3] = (TGraphErrors*)plot_spectrum_from_ascii((char*)file_nlo[i].Data(), eband);
	}
    }

  for (int i = 0; i < Nnlo/2; i++)
    {
      ratio_nlo_g_pi0[i] = (TGraphErrors*)ratio(nlo_gamma[i],nlo_pi0[i]);
      c->cd();
      if (i==1) 
	{
	  c->cd();
	  ratio_nlo_g_pi0[i]->Draw("P");
	  setMarkerLineType(ratio_nlo_g_pi0[i],20,2,2.);
	  legend->AddEntry(ratio_nlo_g_pi0[i], leg_nlo[i], "P");
	}
      cout << "dumping graph " << ratio_nlo_g_pi0[i]->GetName() << endl;
      dumpGraph(ratio_nlo_g_pi0[i], "x-y-ey");
    }

  //_____________________________________________________________________________
  // total

  char exp_nlo[300];
  sprintf(exp_nlo,"isr_ratio_%s_gamma_over_pi0_62GeV_exp_nlo.txt",type.Data());

  TGraphErrors *g_pi0 =(TGraphErrors*)plot_spectrum_from_ascii(exp_nlo, eband);
  setMarkerLineType(g_pi0, 1, 1, 0);
  c->cd();
  g_pi0->Draw("P");  

  double p0 = 4.55122e-02;
  double p1 =-6.03625e-02;
  double p2 = 2.51208e-02;
  double p3 =-2.53308e-03;
  double p4 = 1.02714e-04;

  double ptmin = 1., ptmax = 15.;  

  TF1 *gamma_over_pi0_fit = new TF1("gamma_over_pi0_fit","pol4",ptmin,ptmax);
  gamma_over_pi0_fit->SetParameters(p0,p1,p2,p3,p4);
  g_pi0->Fit("gamma_over_pi0_fit","Q+","", ptmin, ptmax);

  c->cd();
  gamma_over_pi0_fit->SetRange(ptmin,ptmax);
  gamma_over_pi0_fit->Draw("same");

  legend->Draw();

  //TGraphErrors *sc1 = plot_spectrum_from_ascii("pQCD_vogelsang_pp_totgamma_over_pi0_63GeV_sc1.dat",eband);
  //TGraphErrors *sc05 = plot_spectrum_from_ascii("pQCD_vogelsang_pp_totgamma_over_pi0_63GeV_sc05.dat",eband);
  //TGraphErrors *sc2 = plot_spectrum_from_ascii("pQCD_vogelsang_pp_totgamma_over_pi0_63GeV_sc2.dat",eband);

  c->Update();

  return g_pi0;

}


//_____________________________________________________________________________
// numerator = "tot", "dir", "frag", "suppr" ( suppr = tot - 0.5*frag)
// denominator = "tot", "dir", "frag"
// if currCanvas is provided, figs are also plotted there ...

TGraphErrors*
emcAnalyzer::plot_ratio_gamma( const double sqrts, 
			       const TString numerator, const TString denominator,
			       TCanvas *currCanvas )
{

  gStyle->SetOptFit(111);
  fPlot = false;

  char title[100];
  sprintf(title, "gamma_%s_over_%s_sqrts%d",numerator.Data(), denominator.Data(),(int)sqrts);

  TCanvas *c = (TCanvas*)canvas(title, 850, 580);
  char ytitle[100];
  sprintf(ytitle, "ratio #gamma^{%s}/#gamma^{%s}",numerator.Data(), denominator.Data());

  char leg_nlo[200];
  sprintf(leg_nlo, "#gamma^{%s}",numerator.Data());

  //_____________________________________________________________________________

  double xmin = 0, xmax = 15;
  double ratiomin = 0., ratiomax = 3.;

  double suppr_factor = -0.75;

  if (numerator.Contains("suppr"))
    {
      ratiomax = 1.2;
      sprintf(ytitle, "ratio #gamma^{(tot - %2.2f frag)}/#gamma^{%s}",suppr_factor,denominator.Data());
    }
  if (denominator.Contains("tot"))
    {
      ratiomax = 1.2;
      sprintf(ytitle, "ratio #gamma^{subproc}/#gamma^{%s}",denominator.Data());
      if (numerator.Contains("dir")) sprintf(leg_nlo, "Compton+Annhilation");
      if (numerator.Contains("frag")) sprintf(leg_nlo,"Parton fragmentation");
    }

  TH2F *myframe = (TH2F*)frame("myframe", 15, xmin, xmax, 10, ratiomin, ratiomax, "p_{T} (GeV/#font[72]{c})", ytitle);
  c->cd();
  myframe->Draw();
  TLine *line = new TLine(xmin, 1., xmax, 1.);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(2);
  line->Draw();

  //_____________________________________________________________________________
  // NLO data

  char file1[200];
  char file2[200];
  char sc[5];

  TGraphErrors *nlo1[3]; // 3 scales
  TGraphErrors *nlo2[3]; // 3 scales
  TGraphErrors *ratio_nlo_g1_g2[3]; // 3 scales

  TGraphErrors *eband;
  TGraphErrors *temp = 0;

  for (int i=0;i<=2;i++)
    {
      if (i==0) sprintf(sc,"05");
      if (i==1) sprintf(sc,"1");
      if (i==2) sprintf(sc,"2");

      if (numerator.Contains("suppr"))
	{
	  sprintf(file1,"pQCD_vogelsang_pp_gamma_%dGeV_cteq6_sc%s_frag.dat",(int)sqrts,sc);
	  sprintf(file2,"pQCD_vogelsang_pp_gamma_%dGeV_cteq6_sc%s.dat",(int)sqrts,sc);

	  temp = plot_spectrum_from_ascii(file1,eband);
	  nlo2[i] = plot_spectrum_from_ascii(file2,eband);
	  nlo1[i]=(TGraphErrors*)add(nlo2[i],temp,suppr_factor);
	}
      else 
	{
	  sprintf(file1,"pQCD_vogelsang_pp_gamma_%dGeV_cteq6_sc%s_%s.dat",(int)sqrts,sc,numerator.Data());

	  if (denominator.Contains("tot"))
	    {
	      sprintf(file2,"pQCD_vogelsang_pp_gamma_%dGeV_cteq6_sc%s.dat",(int)sqrts,sc);
	    }
	  else
	    {
	      sprintf(file2,"pQCD_vogelsang_pp_gamma_%dGeV_cteq6_sc%s_%s.dat",(int)sqrts,sc,denominator.Data());
	    }

	  nlo1[i] = plot_spectrum_from_ascii(file1,eband);
	  nlo2[i] = plot_spectrum_from_ascii(file2,eband);
	}

      if (nlo1[i] && nlo2[i])
	{
	  ratio_nlo_g1_g2[i] = (TGraphErrors*)ratio(nlo1[i],nlo2[i]);
	  
	  c->cd();
	  ratio_nlo_g1_g2[i]->Draw("L");
	  setMarkerLineType(ratio_nlo_g1_g2[i],20,2,2.,1,2,2);
	
	  if (currCanvas)
	    {
	      currCanvas->cd();
	      ratio_nlo_g1_g2[i]->Draw("L");
	      setMarkerLineType(ratio_nlo_g1_g2[i],20,4,2.,4,4,2);
	    }

//       int N = ratio_nlo_g1_g2[i]->GetN();
//       double* ratio = ratio_nlo_g1_g2[i]->GetY();
//       int locmax = LocMax(N,ratio);
//       double ratiomax = ratio[locmax];
//       ratiomax = max(1.1,ratiomax*1.1); // add 10% more
	}
    }

  // band
  TClonesArray* eb=(TClonesArray*)emcAnalyzerUtils::errorBand(ratio_nlo_g1_g2[2],ratio_nlo_g1_g2[0]);
  TGraph *shadeband = (TGraph*)(*eb)[0]; 
  c->cd();
  shadeband->Draw("f");
  ratio_nlo_g1_g2[2]->Draw("L");
  ratio_nlo_g1_g2[0]->Draw("L");
  myframe->Draw("same");

  char label[300];
  sprintf(label,"p+p #rightarrow #gamma+X @ #sqrt{s} = %i GeV",(int)sqrts);
  TLegend *legend = new TLegend(0.2,0.9,0.5,0.9,label,"brNDC");
  legend->SetName("leg");
  legend->SetMargin(0.1);
  legend->SetFillStyle(0);
  legend->SetBorderSize(4);
  legend->SetTextSize(0.04);
  legend->AddEntry(ratio_nlo_g1_g2[0], leg_nlo, "L");
  c->cd();
  legend->Draw();

  if (currCanvas)
    {
      currCanvas->cd();
      shadeband->Draw("f");
      ratio_nlo_g1_g2[2]->Draw("L");
      ratio_nlo_g1_g2[0]->Draw("L");
      myframe->Draw("same");
      TLegend *leg = (TLegend *)currCanvas->GetListOfPrimitives()->FindObject("leg");
      leg->AddEntry(ratio_nlo_g1_g2[0], leg_nlo, "L");
      //legend->Draw();
      currCanvas->Update();
    }

  TLatex *tex = new TLatex(0.2, 0.9, "NLO pQCD: CTEQ6, GRV, #mu = p_{T}/2 - 2p_{T} [W.Vogelsang]");
  tex->SetTextSize(0.03);
  tex->SetLineWidth(2);
  c->cd();
  tex->Draw();

  //cout << "dumping graph " << ratio_nlo_g1_g2->GetName() << endl;
  //dumpGraph(ratio_nlo_g1_g2, "x-y-ey");

  c->Update();

  return ratio_nlo_g1_g2[1];

}

//_____________________________________________________________________________
//
void
emcAnalyzer::plot_pp_charged_spectra_ISR()
{

  gStyle->SetOptFit(111);

  //_____________________________________________________________________________
  // General spectra plot

  char title1[100];
  sprintf(title1, "charged_hadron_spectra_ISR");

  TCanvas *c_hadchg = (TCanvas*)canvas(title1, 600, 700);
  c_hadchg->Range( -1.23529, -9.51842, 9.28235, 2.69298);
  c_hadchg->SetLeftMargin(0.11745);
  c_hadchg->SetRightMargin(0.0268456);
  c_hadchg->SetTopMargin(0.0567485);
  c_hadchg->SetLogy();

  double ptmin = 0., ptmax = 6.0;

  TH2F *myframe1 = (TH2F*)frame(title1, 6, ptmin, ptmax, 500, 1E-7, 1000,
				"p_{T} (GeV/#font[72]{c})", "Ed^{3}#sigma/d^{3}p (mb GeV^{-2}c^{3})");
  myframe1->GetYaxis()->SetTitleOffset(1.4);
  myframe1->Draw();

  char label[300];
  sprintf(label,"ISR p+p #rightarrow h^{#pm}+X, @ #sqrt{s} = 62. GeV");
  TLegend *legend1 = new TLegend(0.409548, 0.708589, 0.90, 0.90, label, "brNDC");
  legend1->SetMargin(0.1);
  legend1->SetFillStyle(0);
  legend1->SetBorderSize(0);
  legend1->SetTextSize(0.034);
  legend1->Draw();

  c_hadchg->Update();

  //fPlot = true;
  fPlot = false;
  fFit = false;

  TGraphErrors *eband;
  
  //__________________________________________________________________________________
  // CDHW-Collaboration chhad-, chhad+ average  (Breakstone et al.)

  //   char file_chargedplus[300];
  //   char leg_chargedplus[300];
  //   sprintf(file_chargedplus,"pp_chhad+_isr_62GeV_breakstone95_eta0.25.txt");  
  //   sprintf(leg_chargedplus,"p+p #rightarrow h^{+}+X, #sqrt{s} = 62 GeV), #eta = %3.2f",eta);
  
  //   char file_chargedminus[300];
  //   char leg_chargedminus[300];
  //   sprintf(file_chargedminus,"pp_chhad-_isr_62GeV_breakstone95_eta0.25.txt");
  //   sprintf(leg_chargedminus,"p+p #rightarrow h^{-}+X, #sqrt{s} = 62 GeV, #eta = %3.2f",eta);

  //   TGraphErrors *isr_chargedplus  = (TGraphErrors*)plot_spectrum_from_ascii(file_chargedplus, eband);
  //   TGraphErrors *isr_chargedminus = (TGraphErrors*)plot_spectrum_from_ascii(file_chargedminus, eband);
  //   setMarkerLineType(isr_chargedplus, 20, 3, 1.6);
  //   setMarkerLineType(isr_chargedminus, 20, 4, 1.6);
  
  //   sprintf(ctitle,"average_%s_%s",(char*)file_chargedplus.Data(),(char*)file_chargedminus.Data());
  //   TCanvas *c = (TCanvas*)canvas(ctitle,600,700);
  //   c->SetLogy();
  //   char ctitle[300];
  //   isr_chargedaverage = add(isr_chargedplus, isr_chargedminus);
  //   scale(*isr_chargedaverage, 0.5);

  //   c_hadchg->cd();
  //   isr_chargedplus->Draw("P");
  //   isr_chargedminus->Draw("P");

  //   legend1->AddEntry(isr_chargedplus, leg_chargedplus, "P");
  //   legend1->AddEntry(isr_chargedminus, leg_chargedminus, "P");

  //   char label_breaks_plus[300];
  //   sprintf(label_breaks_plus,"Breakstone param. h^{+}, #eta = %3.2f",eta);
  //   char label_breaks_minus[300];
  //   sprintf(label_breaks_minus,"Breakstone param. h^{-}, #eta = %3.2f",eta);

  //TF1 *breakstone_pp_param_plus = (TF1*)plot_pp_sigma_parametrization("+","break",62.4,1.,eta);
  //TF1 *breakstone_pp_param_minus = (TF1*)plot_pp_sigma_parametrization("-","break",62.4,1.,eta);

  //c_hadchg->cd();
  //breakstone_pp_param_plus->SetLineStyle(1);
  //breakstone_pp_param_plus->Draw("same");
  //breakstone_pp_param_minus->SetLineStyle(1);
  //breakstone_pp_param_minus->Draw("same");

  //legend1->AddEntry(breakstone_pp_param_plus, label_breaks_plus, "L");
  //legend1->AddEntry(breakstone_pp_param_minus, label_breaks_minus, "L");

  //__________________________________________________________________________________
  // CDHW-Collaboration chhadaverage data sets (Breakstone et al.)

  char file_cdhw_eta025[300];
  char leg_cdhw_eta025[300];
  sprintf(file_cdhw_eta025,"pp_chghad_isr_62GeV_breakstone95_eta0.25.txt");
  sprintf(leg_cdhw_eta025,"CDHW Collab., #eta = 0.25");
  //sprintf(leg_cdhw_eta025,"p+p #rightarrow h^{#pm}+X, #sqrt{s} = 62 GeV, #eta = 0.25");

  char file_cdhw_eta075[300];
  char leg_cdhw_eta075[300];
  sprintf(file_cdhw_eta075,"pp_chghad_isr_62GeV_breakstone95_eta0.75.txt");
  sprintf(leg_cdhw_eta075,"CDHW Collab., #eta = 0.75");
  //sprintf(leg_cdhw_eta075,"p+p #rightarrow h^{#pm}+X, #sqrt{s} = 62 GeV, #eta = 0.75");

  TGraphErrors *isr_cdhw_eta025 = (TGraphErrors*)plot_spectrum_from_ascii(file_cdhw_eta025, eband); 
  TGraphErrors *isr_cdhw_eta075 = (TGraphErrors*)plot_spectrum_from_ascii(file_cdhw_eta075, eband); 

  setMarkerLineType(isr_cdhw_eta025, 29, 2, 2.0);
  setMarkerLineType(isr_cdhw_eta075, 29, 4, 2.0);

  c_hadchg->cd();

  isr_cdhw_eta025->Draw("P");
  isr_cdhw_eta075->Draw("P");

  legend1->AddEntry(isr_cdhw_eta025, leg_cdhw_eta025, "P");
  legend1->AddEntry(isr_cdhw_eta075, leg_cdhw_eta075, "P");

  // for data/fit ratios ...
  TGraphErrors *isr_h_data[3];
  isr_h_data[0] = (TGraphErrors*)isr_cdhw_eta025->Clone();
  isr_h_data[1] = (TGraphErrors*)isr_cdhw_eta075->Clone();

  //__________________________________________________________________________________
  // AFS-Collaboration chhad spectra (Akesson et al.)

  char file_afs[300];
  char leg_afs[300];
  sprintf(file_afs,"pp_chhad_isr_63GeV_akesson82.txt");  
  sprintf(leg_afs,"AFS Collab., #eta = 0");
  //sprintf(leg_afs,"p+p #rightarrow h^{+}+X, #sqrt{s} = 63 GeV), #eta = 0");

  TGraphErrors *isr_chhad_afs = (TGraphErrors*)plot_spectrum_from_ascii(file_afs, eband); 
  scale(*isr_chhad_afs, 1./2.); // (h+ + h-)/2  
  setMarkerLineType(isr_chhad_afs, 21, 2, 1.4);

  c_hadchg->cd();
  isr_chhad_afs->Draw("P");
  legend1->AddEntry(isr_chhad_afs, leg_afs, "P");

  // for data/fit ratios ...
  isr_h_data[2] = (TGraphErrors*)isr_chhad_afs->Clone();

  //__________________________________________________________________________________
  // p+p parametrizations

  TF1 *axel_pp_param = (TF1*)plot_pp_sigma_parametrization("had","axel",62.4,1.,0.);
  axel_pp_param->SetLineColor(7);
  TF1 *dente_pp_param = (TF1*)plot_pp_sigma_parametrization("pi0","denterria",62.4,1.6,0.);
  dente_pp_param->SetLineColor(6);
  TF1 *star_pp_param = (TF1*)plot_pp_sigma_parametrization("had","star",62.4,1.,0.);
  star_pp_param->SetLineColor(2);
  TF1 *phobos_pp_param = (TF1*)plot_pp_sigma_parametrization("had","phobos",62.4,1.,0.75);
  phobos_pp_param->SetLineColor(1);

  c_hadchg->cd();
  axel_pp_param->Draw("same");
  dente_pp_param->Draw("same");
  star_pp_param->Draw("same");
  phobos_pp_param->Draw("same");

  char label_axel[300];
  sprintf(label_axel,"PHENIX param. h^{+/-}, #eta = 0.");
  char label_dente[300];
  sprintf(label_dente,"PHENIX param. #pi^{0}x1.6, #eta = 0.");
  char label_star[300];
  sprintf(label_star,"STAR param. h^{+/-}, #eta = 0.");
  char label_phobos[300];
  sprintf(label_phobos,"PHOBOS param. h^{+/-}, #eta = 0.75");

  legend1->AddEntry(axel_pp_param, label_axel, "L");
  legend1->AddEntry(dente_pp_param, label_dente, "L");
  legend1->AddEntry(star_pp_param, label_star, "L");
  legend1->AddEntry(phobos_pp_param, label_phobos, "L");

  legend1->Draw();

//   // for data/fit ratios ...
//   TGraphErrors *isr_h_param[0] = (TGraphErrors*)axel_pp_param->Clone();
//   TGraphErrors *isr_h_param[1] = (TGraphErrors*)dente_pp_param->Clone();
//   TGraphErrors *isr_h_param[2] = (TGraphErrors*)star_pp_param->Clone();
//   TGraphErrors *isr_h_param[3] = (TGraphErrors*)phobos_pp_param->Clone();

  //_____________________________________________________________________________
  // ratio data/fit

  TCanvas *ratio_h_data_fit = (TCanvas*)canvas("ratio_h_data_fit_63GeV", 700, 600);
  ratio_h_data_fit->cd();
  TH2F *myframe_ratio = (TH2F*)frame("myframe_ratio", 24, 0., 6., 10, 0., 2., "p_{T} (GeV/#font[72]{c})", "ratio data/fit");
  myframe_ratio->Draw();

  sprintf(label,"p+p #rightarrow h^{#pm}+X parametrizations @ #sqrt{s} = 62.4 GeV");
  TLegend *legend = new TLegend(0.409548, 0.708589, 0.90, 0.90, label, "brNDC");
  legend->SetMargin(0.1);
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);
  legend->SetTextSize(0.034);

  TLine *line = new TLine(0., 1., 6, 1.);
  line->SetLineColor(1);
  line->SetLineStyle(1);
  line->SetLineWidth(3);
  ratio_h_data_fit->cd();
  line->Draw();

  const int N =3;
  TGraphErrors *ratio_h_isr_data_axelfit[N];
  //TGraphErrors *ratio_h_isr_data_dentefit[N];
  TGraphErrors *ratio_h_isr_data_starfit[N];
  TGraphErrors *ratio_h_isr_data_phobosfit[N];

  for (int i=0; i<N; i++)
    {
      if (i==0 || i==2) // only at eta ~ 0
	{
	  ratio_h_isr_data_axelfit[i]  = (TGraphErrors*)ratio(isr_h_data[i],axel_pp_param);
	  //ratio_h_isr_data_dentefit[i] = (TGraphErrors*)ratio(isr_h_data[i],dente_pp_param);
	  ratio_h_isr_data_starfit[i]  = (TGraphErrors*)ratio(isr_h_data[i],star_pp_param);

	  ratio_h_data_fit->cd();
	  setMarkerLineType(ratio_h_isr_data_axelfit[i], 20, 7, 2.0);
	  //setMarkerLineType(ratio_h_isr_data_dentefit[i], 20, 2, 2.0);
	  setMarkerLineType(ratio_h_isr_data_starfit[i], 20, 3, 2.0);

	  ratio_h_isr_data_axelfit[i]->Draw("P");
	  //ratio_h_isr_data_dentefit[i]->Draw("P");
	  ratio_h_isr_data_starfit[i]->Draw("P");
	}
      else // only at eta ~ 0.75
	{
	  ratio_h_isr_data_phobosfit[i]= (TGraphErrors*)ratio(isr_h_data[i],phobos_pp_param);

	  ratio_h_data_fit->cd();
	  setMarkerLineType(ratio_h_isr_data_phobosfit[i], 20, 1, 2.0);
	  ratio_h_isr_data_phobosfit[i]->Draw("P");

	  legend->AddEntry(ratio_h_isr_data_phobosfit[i], label_phobos, "P");
	}
    }
  
  ratio_h_data_fit->cd();
  legend->AddEntry(ratio_h_isr_data_axelfit[0], label_axel, "P");
  //legend->AddEntry(ratio_h_isr_data_dentefit[0], label_dente, "P");
  legend->AddEntry(ratio_h_isr_data_starfit[0], label_star, "P");
  legend->Draw();
  ratio_h_data_fit->Update();

}


//_____________________________________________________________________________
// data = "star", "brahms"

void
emcAnalyzer::plot_pp_charged_vs_nlo_RHIC( const TString dataset, const int eta )
{

  char title[100];
  sprintf(title, "%s_hipt_charged_eta%d_vs_NLOpQCD_vogelsang",dataset.Data(),eta);

  TCanvas *c22 = (TCanvas*)canvas(title, 725, 690);
  c22->Range( -1.55644, -9.47202, 11.0525, 2.21956);
  c22->SetLeftMargin(0.12344);
  c22->SetRightMargin(0.0042);
  c22->SetTopMargin(0.019);

  // NLO data

  const int Npqcd = 14;

  TGraphErrors *nlo_spectrum[Npqcd];
  TGraphErrors *errBand;

  TString file_pqcd[Npqcd] = 
    {
      // STAR
      "pQCD_vogelsang_pp_chhad_200GeV_kkp_sc1.txt",
      "pQCD_vogelsang_pp_chhad_200GeV_kretzer_sc1.txt",
      "pQCD_vogelsang_pp_chhad_200GeV_kkp_sc2.txt",
      "pQCD_vogelsang_pp_chhad_200GeV_kkp_sc05.txt",
      "pQCD_vogelsang_pp_pi0_200GeV_cteq6_sc1_kkp_eta3.8.dat",

      // BRAHMS
      "pQCD_vogelsang_pp_chhad_200GeV_brahms_y0_bfgw_sc1.dat",
      "pQCD_vogelsang_pp_chhad_200GeV_brahms_y0_kkp_sc1.dat",
      "pQCD_vogelsang_pp_chhad_200GeV_brahms_y0_kretzer_sc1.dat",
      "pQCD_vogelsang_pp_chhad_200GeV_brahms_y1_bfgw_sc1.dat",
      "pQCD_vogelsang_pp_chhad_200GeV_brahms_y1_kkp_sc1.dat",
      "pQCD_vogelsang_pp_chhad_200GeV_brahms_y1_kretzer_sc1.dat",
      "pQCD_vogelsang_pp_had-_200GeV_brahms_y2.2_kretzer_sc1.dat",
      "pQCD_vogelsang_pp_had-_200GeV_brahms_y3.2_kretzer_sc1.dat",
      "pQCD_vogelsang_pp_had-_200GeV_brahms_y3.2_kretzer_modified_sc1.dat"
    };
  
  TString leg[Npqcd] = 
    {
      
      // STAR
      "PDF: CTEQ6M, FF: KKP, scales: #mu=p_{T}",
      "PDF: CTEQ6M, FF: Kretzer, scales: #mu=p_{T}",
      "PDF: CTEQ6M, FF: KKP, scales: #mu=2p_{T}",
      "PDF: CTEQ6M, FF: KKP, scales: #mu=p_{T}/2",
      "PDF: CTEQ6M, FF: KKP, scales: #mu=p_{T}",

      // BRAHMS
      "PDF: CTEQ6, FF: Bourhis et al., scales: #mu=p_{T}",
      "PDF: CTEQ6, FF: KKP, scales: #mu=p_{T}",
      "PDF: CTEQ6, FF: Kretzer, scales: #mu=p_{T}",
      "PDF: CTEQ6, FF: Bourhis et al., scales: #mu=p_{T}",
      "PDF: CTEQ6, FF: KKP, scales: #mu=p_{T}",
      "PDF: CTEQ6, FF: Kretzer, scales: #mu=p_{T}",
      "PDF: CTEQ6, FF: Kretzer, scales: #mu=p_{T}",
      "PDF: CTEQ6, FF: Kretzer, scales: #mu=p_{T}",
      "PDF: CTEQ6, FF: mod. KKP, scales: #mu=p_{T}"
    };

  setFit(false);

  // Experimental data

  TGraphErrors *pp_exp = 0;
  char ppfile[100];
  char pplabel[100];

  double ppNSDcross = 0.;
  double ptmax = 10.5;
  double ptmin = 0.;
  double yieldmax = 100.;
  double yieldmin = 1e-8;

  char xtitle[100],ytitle[100];
  sprintf(xtitle,"p_{T} (GeV/#font[72]{c})");
  sprintf(ytitle,"Ed^{3}#sigma/d^{3}p (mb GeV^{-2}c^{3})");

  if (dataset.Contains("star"))
    {
      sprintf(ppfile,"h_pp_yields_NSD_star_200.txt");
      sprintf(pplabel,"p+p #rightarrow h^{#pm}+X @ #sqrt{s} = 200 GeV [STAR]");
      ppNSDcross = 30.;

      if ( eta==4 )
	{
	  sprintf(ppfile,"pi0_pp_cross_star_200_eta3.8.txt");
	  sprintf(pplabel,"p+p #rightarrow #pi^{0}+X @ #sqrt{s} = 200 GeV, #eta = 3.8 [STAR]");
	  sprintf(xtitle,"E_{#pi} (GeV)");
	  ptmax = 60;
	  ptmin = 25;
	  yieldmax = 0.5;
	  yieldmin = 1e-5;
	  ppNSDcross = 1.; // file already in mb
	}
    }
  else if (dataset.Contains("brahms"))
    {
      sprintf(ppfile,"brahms_pp_eta%d.dat",eta);
      sprintf(pplabel,"p+p #rightarrow h^{#pm}+X @ #sqrt{s} = 200 GeV , #eta = %d [BRAHMS]",eta);
      ptmax = 6.5;

      if ( eta==2 || eta==3 )
	{
	  sprintf(ppfile,"brahms_pp_eta%d.dat",eta);
	  sprintf(pplabel,"p+p #rightarrow h^{-}+X @ #sqrt{s} = 200 GeV , #eta = %d.2 [BRAHMS]",eta);
	  ptmax = 5.;
	}

      ppNSDcross = 41.; // BRAHMS cross-section (Ramiro Debbe): we normalize our results to total inelastic (41 mb). 
                        // Our triggers do pick up NSD events but we corrected the spectra. 
      yieldmin = 1e-6;
    }
  else if (dataset.Contains("phenix"))
    {
      // to be implemented
    }
  else
    {
      cout << " <I> " << dataset.Data() << " not implemented ..." << endl;
      return;
    }

  // Plot results

  TH2F *myframe = (TH2F*)frame(title, 10, ptmin, ptmax, 100, yieldmin, yieldmax, xtitle, ytitle);
  myframe->GetYaxis()->SetTitleOffset(1.35);
  //myframe->GetXaxis()->SetLabelOffset(0.0);
  c22->cd();
  myframe->Draw();
  c22->SetLogy();

  pp_exp = (TGraphErrors*)plot_spectrum_from_ascii(ppfile, errBand);
  if (!pp_exp) return;

  scale(*pp_exp, ppNSDcross);  
  setMarkerLineType(pp_exp, 29, 2, 1.9);

  // a trick to plot BRAHMS "asymmetric" errors
  TGraphErrors *asym_pp_exp = 0;
  if ( eta==2 || eta==3 )
    {
      for (int i=0;i<pp_exp->GetN();i++) 
	pp_exp->SetPointError(i, 0., pp_exp->GetErrorY(i));

      sprintf(ppfile,"brahms_pp_eta%d_bin_unshift.dat",eta);
      asym_pp_exp=(TGraphErrors*)(TGraphErrors*)plot_spectrum_from_ascii(ppfile, errBand);
      scale(*asym_pp_exp, ppNSDcross);  
      setMarkerLineType(asym_pp_exp,1,1,1);

      for (int i=0;i<asym_pp_exp->GetN();i++) 
	asym_pp_exp->SetPointError(i, asym_pp_exp->GetErrorX(i) , 0.);

      c22->cd();
      asym_pp_exp->Draw("P");
    }

  c22->cd();
  pp_exp->Draw("P");

  TLatex *tex = new TLatex(ptmin+2.5, yieldmax/2., pplabel);
  tex->SetTextSize(0.031);
  tex->Draw();
  TMarker *marker = new TMarker(ptmin+2., yieldmax/2, 30);
  marker->SetMarkerColor(2);
  marker->SetMarkerStyle(29);
  marker->SetMarkerSize(1.9);
  marker->Draw();

  TString label = "vs. NLO pQCD [W.Vogelsang]:";
  TLegend *legend = new TLegend(0.409548, 0.708589, 0.90, 0.90, label, "brNDC");
  legend->SetMargin(0.1);
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);
  legend->SetTextSize(0.031);
  legend->Draw();

  //fPlot = false;
  
  int Npqcd1=0, Npqcd2=0;

  // Data is (h+ + h-)/2 but NLO is (h+ + h-), except for forward rapidities (h- alone both).
  double scalefactor = 1.;

  if ( dataset.Contains("star") && eta!=4 ) { Npqcd1 = 0; Npqcd2 = 3; scalefactor = 0.5; }
  if ( dataset.Contains("star") && eta==4 ) { Npqcd1 = 4; Npqcd2 = 4; scalefactor = 1; }
  if ( dataset.Contains("brahms") && eta==0 ) { Npqcd1 = 5; Npqcd2 = 7; scalefactor = 0.5; }
  if ( dataset.Contains("brahms") && eta==1 ) { Npqcd1 = 8; Npqcd2 = 10; scalefactor = 0.5; }
  if ( dataset.Contains("brahms") && eta==2 ) { Npqcd1 = 11; Npqcd2 = 11; scalefactor = 1.; }
  if ( dataset.Contains("brahms") && eta==3 ) { Npqcd1 = 12; Npqcd2 = 13; scalefactor = 1.; }

  int linestyle = 1;

  for (int i = Npqcd1; i <= Npqcd2; i++)
    {
      nlo_spectrum[i] = (TGraphErrors*)plot_spectrum_from_ascii((char*)file_pqcd[i].Data(), errBand);

      if (nlo_spectrum[i])
        {
          scale(*nlo_spectrum[i], scalefactor);

          c22->cd();
          nlo_spectrum[i]->SetLineColor(1);
          nlo_spectrum[i]->SetLineStyle(linestyle);
          nlo_spectrum[i]->SetLineWidth(2);
	  nlo_spectrum[i]->Draw("L");
	  legend->AddEntry(nlo_spectrum[i], leg[i], "L");
	  linestyle++;
	}
    }

  c22->Update();


}

//_____________________________________________________________________________
void
emcAnalyzer::plot_DIS_nuclear()
{

  char title[100];

  sprintf(title, "DIS_nuclear_x_Q2_map");

  TCanvas *c22 = (TCanvas*)canvas(title, 725, 690);
  c22->Range( -5.8, -2.9, 0.0, 4.1);
  c22->SetLeftMargin(0.14);
  c22->SetRightMargin(0.0042);
  c22->SetTopMargin(0.019);

  Axis_t xmin = 4.e-07;//1e-5;
  Axis_t xmax = 1. ;
  int ncx = 100;
  Axis_t ymin = 1e-02;
  Axis_t ymax = 400000. ;
  int ncy = 100;

  TH2F *myframe = (TH2F*)frame(title, ncx, xmin, xmax, ncy, ymin, ymax,
                               "x  ", "Q^{2} (GeV^{2}/c^{2})");
  myframe->GetYaxis()->SetTitleOffset(1.1);
  myframe->GetXaxis()->SetLabelOffset(0.0);
  myframe->Draw();
  c22->SetLogx();
  c22->SetLogy();
  c22->Update();

  TString label = "Nuclear DIS & DY data:";
  TLegend *legend = new TLegend(0.54, 0.72, 0.90, 0.97, label, "brNDC");
  legend->SetMargin(0.1);
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);
  legend->SetTextSize(0.035);
  //  legend->Draw();

  // Experimental data

  //_____________________________________________________________________________
  // NMC
  //

  int NMC_marker = 20;
  double NMC_size = 2.;
  int NMC_col = 6;
  char NMC_leg[30];
  sprintf(NMC_leg, "NMC (DIS)");

  // He_NMC (18),  C_NMC (18), Ca_NMC (18)

  int N = 18;

  double x_NMC[] = { 0.0035, 0.0055, 0.0085, 0.0125, 0.0175, 0.025,
                     0.035, 0.045, 0.055, 0.07, 0.09, 0.125, 0.175,
                     0.25, 0.35, 0.45, 0.55, 0.65};

  double Q2_He_NMC[] = { 0.77, 1.3, 1.8, 2.4, 3.0, 3.8, 4.7, 5.6, 6.3, 7.3, 8.7, 11, 14, 19, 24, 31, 38, 44};

  TGraph *x_Q2_He_NMC = new TGraph( N, x_NMC, Q2_He_NMC);
  x_Q2_He_NMC->SetTitle("x_Q2_He_NMC");
  x_Q2_He_NMC->SetName("");
  setMarkerLineType(x_Q2_He_NMC, NMC_marker, NMC_col, NMC_size);
  x_Q2_He_NMC->SetHistogram(myframe);
  x_Q2_He_NMC->Draw("AP");
  legend->AddEntry(x_Q2_He_NMC, NMC_leg, "P");

  double Q2_C_NMC[] = { 0.74, 1.2, 1.7, 2.3, 3.0, 3.8, 4.9, 6.0, 7.2, 8.8, 11, 14, 17, 21, 26, 31, 37, 44};

  TGraph *x_Q2_C_NMC = new TGraph( N, x_NMC, Q2_C_NMC);
  x_Q2_C_NMC->SetTitle("x_Q2_C_NMC");
  x_Q2_C_NMC->SetName("");
  setMarkerLineType(x_Q2_C_NMC, NMC_marker, NMC_col, NMC_size);
  x_Q2_C_NMC->SetHistogram(myframe);
  x_Q2_C_NMC->Draw("P");

  double Q2_Ca_NMC[] = { 0.60, 0.94, 1.4, 1.9, 2.5, 3.4, 4.7, 5.7, 6.8, 8.1, 9.7, 12, 14, 19, 24, 30, 35, 41};

  TGraph *x_Q2_Ca_NMC = new TGraph( N, x_NMC, Q2_Ca_NMC);
  x_Q2_Ca_NMC->SetTitle("x_Q2_Ca_NMC");
  x_Q2_Ca_NMC->SetName("");
  setMarkerLineType(x_Q2_Ca_NMC, NMC_marker, NMC_col, NMC_size);
  x_Q2_Ca_NMC->SetHistogram(myframe);
  x_Q2_Ca_NMC->Draw("P");

  // BeC_NMC (15)

  N = 15;

  double x_BeC_NMC[] = { 0.0125, 0.0175, 0.025, 0.035, 0.045, 0.055, 0.07,
                         0.09, 0.125, 0.175, 0.25, 0.35, 0.45, 0.55, 0.70};

  double Q2_BeC_NMC[] = { 3.4, 4.5, 6.0, 8.0, 9.8, 11.4, 13.8, 16.8, 21.4, 27.6, 35.5, 45.4, 53.9, 60.5, 66.7};

  TGraph *x_Q2_BeC_NMC = new TGraph( N, x_BeC_NMC, Q2_BeC_NMC);
  x_Q2_BeC_NMC->SetTitle("x_Q2_BeC_NMC");
  x_Q2_BeC_NMC->SetName("");
  setMarkerLineType(x_Q2_BeC_NMC, NMC_marker, NMC_col, NMC_size);
  x_Q2_BeC_NMC->SetHistogram(myframe);
  x_Q2_BeC_NMC->Draw("P");

  // AlC_NMC (15)

  double x_AlC_NMC[] = { 0.0125, 0.0175, 0.025, 0.035, 0.045, 0.055, 0.07,
                         0.09, 0.125, 0.175, 0.25, 0.35, 0.45, 0.55, 0.70};

  double Q2_AlC_NMC[] = { 3.4, 4.5, 6.1, 8.0, 9.8, 11.6, 13.9, 16.9, 21.3,
                          27.4, 35.5, 45.3, 52.9, 58.2, 63.9};

  TGraph *x_Q2_AlC_NMC = new TGraph( N, x_AlC_NMC, Q2_AlC_NMC);
  x_Q2_AlC_NMC->SetTitle("x_Q2_AlC_NMC");
  x_Q2_AlC_NMC->SetName("");
  setMarkerLineType(x_Q2_AlC_NMC, NMC_marker, NMC_col, NMC_size);
  x_Q2_AlC_NMC->SetHistogram(myframe);
  x_Q2_AlC_NMC->Draw("P");

  // CaC_NMC (15)

  double x_CaC_NMC[] = { 0.0125, 0.0175, 0.025, 0.035, 0.045, 0.055, 0.07,
                         0.09, 0.125, 0.175, 0.25, 0.35, 0.45, 0.55, 0.70};

  double Q2_CaC_NMC[] = { 3.4, 4.5, 6.0, 7.9, 9.7, 11.4, 13.8, 16.7, 21.2,
                          27.4, 35.3, 45.3, 53.6, 60.3, 66.4};

  TGraph *x_Q2_CaC_NMC = new TGraph( N, x_CaC_NMC, Q2_CaC_NMC);
  x_Q2_CaC_NMC->SetTitle("x_Q2_CaC_NMC");
  x_Q2_CaC_NMC->SetName("");
  setMarkerLineType(x_Q2_CaC_NMC, NMC_marker, NMC_col, NMC_size);
  x_Q2_CaC_NMC->SetHistogram(myframe);
  x_Q2_CaC_NMC->Draw("P");

  // FeC_NMC (15)

  double x_FeC_NMC[] = { 0.0125, 0.0175, 0.025, 0.035, 0.045, 0.055, 0.07,
                         0.09, 0.125, 0.175, 0.25, 0.35, 0.45, 0.55, 0.70};

  double Q2_FeC_NMC[] = { 3.4, 4.5, 6.1, 8.1, 10.0, 11.8, 14.1, 17.1, 21.7,
                          27.8, 35.7, 45.4, 53.6, 60.3, 66.6};

  TGraph *x_Q2_FeC_NMC = new TGraph( N, x_FeC_NMC, Q2_FeC_NMC);
  x_Q2_FeC_NMC->SetTitle("x_Q2_FeC_NMC");
  x_Q2_FeC_NMC->SetName("");
  setMarkerLineType(x_Q2_FeC_NMC, NMC_marker, NMC_col, NMC_size);
  x_Q2_FeC_NMC->SetHistogram(myframe);
  x_Q2_FeC_NMC->Draw("P");

  // PbC_NMC (15)

  double x_PbC_NMC[] = { 0.0125, 0.0175, 0.025, 0.035, 0.045, 0.055, 0.07,
                         0.09, 0.125, 0.175, 0.25, 0.35, 0.45, 0.55, 0.70};

  double Q2_PbC_NMC[] = { 3.4, 4.5, 6.1, 8.0, 9.8, 11.6, 13.9, 16.9, 21.4,
                          27.5, 35.5, 45.3, 53.6, 60.0, 66.1};

  TGraph *x_Q2_PbC_NMC = new TGraph( N, x_PbC_NMC, Q2_PbC_NMC);
  x_Q2_PbC_NMC->SetTitle("x_Q2_PbC_NMC");
  x_Q2_PbC_NMC->SetName("");
  setMarkerLineType(x_Q2_PbC_NMC, NMC_marker, NMC_col, NMC_size);
  x_Q2_PbC_NMC->SetHistogram(myframe);
  x_Q2_PbC_NMC->Draw("P");

  // SnC_NMC (15) (average)

  double x_SnCC_NMC[] = { 0.0125, 0.0175, 0.025, 0.035, 0.045, 0.055, 0.07,
                          0.09, 0.125, 0.175, 0.25, 0.35, 0.45, 0.55, 0.70};

  double Q2_SnCC_NMC[] = { 3.2, 4.3, 5.6, 7.3, 8.6, 9.8, 11.2, 12.7, 14.8,
                           17.3, 20.5, 25.1, 32.0, 43.9, 56.8};

  TGraph *x_Q2_SnCC_NMC = new TGraph( N, x_SnCC_NMC, Q2_SnCC_NMC);
  x_Q2_SnCC_NMC->SetTitle("x_Q2_SnCC_NMC");
  x_Q2_SnCC_NMC->SetName("");
  setMarkerLineType(x_Q2_SnCC_NMC, NMC_marker, NMC_col, NMC_size);
  x_Q2_SnCC_NMC->SetHistogram(myframe);
  x_Q2_SnCC_NMC->Draw("P");

  // SnC_NMC (145) (x=0.125 Q2=2.3 excluded)

  N = 145;
  double x_SnC_NMC[] = { 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125,
                         0.0125, 0.0125, 0.0175, 0.0175, 0.0175, 0.0175, 0.0175,
                         0.0175, 0.0175, 0.0175, 0.0175, 0.025, 0.025, 0.025,
                         0.025, 0.025, 0.025, 0.025, 0.025, 0.025, 0.035, 0.035,
                         0.035, 0.035, 0.035, 0.035, 0.035, 0.035, 0.035, 0.035, 0.035,
                         0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045,
                         0.045, 0.045, 0.055, 0.055, 0.055, 0.055, 0.055, 0.055, 0.055,
                         0.055, 0.055, 0.055, 0.055, 0.070, 0.070, 0.070, 0.070, 0.070,
                         0.070, 0.070, 0.070, 0.070, 0.070, 0.070, 0.070, 0.090, 0.090,
                         0.090, 0.090, 0.090, 0.090, 0.090, 0.090, 0.090, 0.090, 0.090,
                         0.090, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125,
                         0.125, 0.125, 0.125, 0.125, 0.125, 0.175, 0.175, 0.175, 0.175,
                         0.175, 0.175, 0.175, 0.175, 0.175, 0.175, 0.175, 0.175, 0.250,
                         0.250, 0.250, 0.250, 0.250, 0.250, 0.250, 0.250, 0.250, 0.250,
                         0.250, 0.350, 0.350, 0.350, 0.350, 0.350, 0.350, 0.350, 0.350,
                         0.350, 0.450, 0.450, 0.450, 0.450, 0.450, 0.450, 0.450, 0.550,
                         0.550, 0.550, 0.550, 0.550, 0.550, 0.700, 0.700, 0.700, 0.700};

  double Q2_SnC_NMC[] = { 1.3, 1.8, 2.3, 2.8, 3.5, 4.5, 5.5, 7.0,
                          1.3, 1.8, 2.3, 2.8, 3.5, 4.5, 5.5, 7.0, 9.0,
                          1.8, 2.3, 2.8, 3.5, 4.5, 5.5, 7.0, 9.0, 11.5,
                          1.8, 2.3, 2.8, 3.5, 4.5, 5.5, 7.0, 9.0, 11.5, 15.0, 20.0,
                          1.8, 2.3, 2.8, 3.5, 4.5, 5.5, 7.0, 9.0, 11.5, 15.0, 20.0,
                          2.3, 2.8, 3.5, 4.5, 5.5, 7.0, 9.0, 11.5, 15.0, 20.0, 27.0,
                          2.3, 2.8, 3.5, 4.5, 5.5, 7.0, 9.0, 11.5, 15.0, 20.0, 27.0, 36.0,
                          2.3, 2.8, 3.5, 4.5, 5.5, 7.0, 9.0, 11.5, 15.0, 20.0, 27.0, 36.0,
                          2.8, 3.5, 4.5, 5.5, 7.0, 9.0, 11.5, 15.0, 20.0, 27.0, 36.0, 48.0, 65.0,
                          3.5, 4.5, 5.5, 7.0, 9.0, 11.5, 15.0, 20.0, 27.0, 36.0, 48.0, 65.0,
                          5.5, 7.0, 9.0, 11.5, 15.0, 20.0, 27.0, 36.0, 48.0, 65.0, 110.0,
                          9.0, 11.5, 15.0, 20.0, 27.0, 36.0, 48.0, 65.0, 110.0,
                          15.0, 20.0, 27.0, 36.0, 48.0, 65.0, 110.0,
                          20.0, 27.0, 36.0, 48.0, 65.0, 110.0,
                          36.0, 48.0, 65.0, 110.0};

  TGraph *x_Q2_SnC_NMC = new TGraph( N, x_SnC_NMC, Q2_SnC_NMC);
  x_Q2_SnC_NMC->SetTitle("x_Q2_SnC_NMC");
  x_Q2_SnC_NMC->SetName("");
  setMarkerLineType(x_Q2_SnC_NMC, NMC_marker, NMC_col, NMC_size);
  x_Q2_SnC_NMC->SetHistogram(myframe);
  x_Q2_SnC_NMC->Draw("P");

  //_____________________________________________________________________________
  // E139
  //

  int E139_marker = 21;
  double E139_size = 1.8;
  int E139_col = 51;
  char E139_leg[30];
  sprintf(E139_leg, "SLAC-E139 (DIS)");

  // He_E139 (18)

  N = 18;

  double x_He_E139[] = { 0.13, 0.22, 0.22, 0.30, 0.30, 0.40, 0.40, 0.40,
                         0.50, 0.50, 0.50, 0.50, 0.60, 0.60, 0.60, 0.70, 0.70, 0.80};

  double Q2_He_E139[] = { 2.0 , 2.0, 5.0, 2.0, 5.0, 2.0, 5.0, 10.0,
                          2.0, 5.0, 5.0, 10.0, 5.0, 10.0, 15.0, 5.0, 10.0, 10.0};

  TGraph *x_Q2_He_E139 = new TGraph( N, x_He_E139, Q2_He_E139);
  x_Q2_He_E139->SetTitle("x_Q2_He_E139");
  x_Q2_He_E139->SetName("");
  setMarkerLineType(x_Q2_He_E139, E139_marker, E139_col, E139_size);
  x_Q2_He_E139->SetHistogram(myframe);
  x_Q2_He_E139->Draw("P");
  legend->AddEntry(x_Q2_He_E139, E139_leg, "P");


  // Be_E139 (17)

  N = 17;

  double x_Be_E139[] = { 0.13, 0.22, 0.22, 0.30, 0.30, 0.40, 0.40, 0.40,
                         0.50, 0.50, 0.50, 0.60, 0.60, 0.60, 0.70, 0.70, 0.80};

  double Q2_Be_E139[] = { 2.0 , 2.0, 5.0, 2.0, 5.0, 2.0, 5.0, 10.0,
                          2.0, 5.0, 10.0, 5.0, 10.0, 15.0, 5.0, 10.0, 10.0};

  TGraph *x_Q2_Be_E139 = new TGraph( N, x_Be_E139, Q2_Be_E139);
  x_Q2_Be_E139->SetTitle("x_Q2_Be_E139");
  x_Q2_Be_E139->SetName("");
  setMarkerLineType(x_Q2_Be_E139, E139_marker, E139_col, E139_size);
  x_Q2_Be_E139->SetHistogram(myframe);
  x_Q2_Be_E139->Draw("P");

  // C_E139 (7)

  N = 7;

  double x_C_E139[] = { 0.22 , 0.30 , 0.40 , 0.50 , 0.60 , 0.60 , 0.70};

  double Q2_C_E139[] = { 5.0, 5.0, 5.0 , 5.0 , 5.0 , 10.0 , 5.0};

  TGraph *x_Q2_C_E139 = new TGraph( N, x_C_E139, Q2_C_E139);
  x_Q2_C_E139->SetTitle("x_Q2_C_E139");
  x_Q2_C_E139->SetName("");
  setMarkerLineType(x_Q2_C_E139, E139_marker, E139_col, E139_size);
  x_Q2_C_E139->SetHistogram(myframe);
  x_Q2_C_E139->Draw("P");

  // Al_E139 (17)

  N = 17;

  double x_Al_E139[] = { 0.13, 0.22, 0.22, 0.30, 0.30, 0.40, 0.40, 0.40,
                         0.50, 0.50, 0.50, 0.60, 0.60, 0.60, 0.70, 0.70, 0.80};

  double Q2_Al_E139[] = { 2.0 , 2.0, 5.0, 2.0, 5.0, 2.0, 5.0, 10.0,
                          2.0, 5.0, 10.0, 5.0, 10.0, 15.0, 5.0, 10.0, 10.0};

  TGraph *x_Q2_Al_E139 = new TGraph( N, x_Al_E139, Q2_Al_E139);
  x_Q2_Al_E139->SetTitle("x_Q2_Al_E139");
  x_Q2_Al_E139->SetName("");
  setMarkerLineType(x_Q2_Al_E139, E139_marker, E139_col, E139_size);
  x_Q2_Al_E139->SetHistogram(myframe);
  x_Q2_Al_E139->Draw("P");

  // Ca_E139 (7)

  N = 7;

  double x_Ca_E139[] = { 0.22 , 0.30 , 0.40 , 0.50 , 0.60 , 0.60 , 0.70};

  double Q2_Ca_E139[] = { 5.0, 5.0, 5.0 , 5.0 , 5.0 , 10.0 , 5.0};

  TGraph *x_Q2_Ca_E139 = new TGraph( N, x_Ca_E139, Q2_Ca_E139);
  x_Q2_Ca_E139->SetTitle("x_Q2_Ca_E139");
  x_Q2_Ca_E139->SetName("");
  setMarkerLineType(x_Q2_Ca_E139, E139_marker, E139_col, E139_size);
  x_Q2_Ca_E139->SetHistogram(myframe);
  x_Q2_Ca_E139->Draw("P");

  // Fe_E139 (23)

  N = 23;

  double x_Fe_E139[] = { 0.089, 0.13, 0.14, 0.22, 0.22, 0.30, 0.30, 0.30,
                         0.30 , 0.40, 0.40, 0.40, 0.50, 0.50, 0.50, 0.50,
                         0.60 , 0.60, 0.60, 0.70, 0.70, 0.70, 0.80};

  double Q2_Fe_E139[] = { 2.0, 2.0, 5.0, 2.0, 5.0, 2.0, 5.0 , 5.0 ,
                          10.0, 2.0, 5.0, 10.0, 2.0, 5.0, 5.0 , 10.0,
                          5.0, 10.0, 15.0, 5.0, 5.0, 10.0, 10.0};

  TGraph *x_Q2_Fe_E139 = new TGraph( N, x_Fe_E139, Q2_Fe_E139);
  x_Q2_Fe_E139->SetTitle("x_Q2_Fe_E139");
  x_Q2_Fe_E139->SetName("");
  setMarkerLineType(x_Q2_Fe_E139, E139_marker, E139_col, E139_size);
  x_Q2_Fe_E139->SetHistogram(myframe);
  x_Q2_Fe_E139->Draw("P");

  // Ag_E139 (7)

  N = 7;

  double x_Ag_E139[] = { 0.22 , 0.30 , 0.40 , 0.50 , 0.60 , 0.60 , 0.70};

  double Q2_Ag_E139[] = { 5.0, 5.0, 5.0 , 5.0 , 5.0 , 10.0 , 5.0};

  TGraph *x_Q2_Ag_E139 = new TGraph( N, x_Ag_E139, Q2_Ag_E139);
  x_Q2_Ag_E139->SetTitle("x_Q2_Ag_E139");
  x_Q2_Ag_E139->SetName("");
  setMarkerLineType(x_Q2_Ag_E139, E139_marker, E139_col, E139_size);
  x_Q2_Ag_E139->SetHistogram(myframe);
  x_Q2_Ag_E139->Draw("P");

  // Au_E139 (18)

  double x_Au_E139[] = { 0.13, 0.22, 0.22, 0.30, 0.30, 0.40, 0.40, 0.40,
                         0.50, 0.50, 0.50, 0.50, 0.60, 0.60, 0.60, 0.70, 0.70, 0.80};

  double Q2_Au_E139[] = { 2.0 , 2.0, 5.0, 2.0, 5.0, 2.0, 5.0, 10.0,
                          2.0, 5.0, 5.0, 10.0, 5.0, 10.0, 15.0, 5.0, 10.0, 10.0};

  TGraph *x_Q2_Au_E139 = new TGraph( N, x_Au_E139, Q2_Au_E139);
  x_Q2_Au_E139->SetTitle("x_Q2_Au_E139");
  x_Q2_Au_E139->SetName("");
  setMarkerLineType(x_Q2_Au_E139, E139_marker, E139_col, E139_size);
  x_Q2_Au_E139->SetHistogram(myframe);
  x_Q2_Au_E139->Draw("P");

  //_____________________________________________________________________________
  // other DIS data

  fPlot = false;
  fFit = false;

  const int Neks = 3;
  TString file_EKS[Neks] = {
    "EKS_DIS_E665.txt",
    "EKS_DIS_E665_2.txt",
    //"EKS_DY_E772.txt",
    "EKS_EMC_95.txt"
    //"EKS_NMC_95.txt"
    //"EKS_NMC_96.txt"
  };

  TString leg[Neks] = {
    "FNAL-E665 (DIS)",
    "FNAL-E665-2 (DIS)",
    //"DY_E772",
    "EMC (DIS)"
    //"DIS NMC 95"
    //"NMC NMC 96"
  };

  int e665_col = 71;
  int emc_col = 61;

  TGraphErrors *DIS_EKS[Neks];
  TGraphErrors *errBand;

  for (int i = 0; i < Neks; i++)
    {
      DIS_EKS[i] = (TGraphErrors*)plot_spectrum_from_ascii((char*)file_EKS[i].Data(), errBand);
      DIS_EKS[i]->Print();

      if (DIS_EKS[i])
        {
          c22->cd();
          if (i == 2)
            setMarkerLineType(DIS_EKS[i], 22, emc_col, 2);
	  else
	    setMarkerLineType(DIS_EKS[i], 23, e665_col, 2);
          DIS_EKS[i]->SetHistogram(myframe);
          DIS_EKS[i]->Draw("P");
          if (i != 1) // skip legend for 2nd E665 data set
            legend->AddEntry(DIS_EKS[i], leg[i], "P");
        }
    }

  //_____________________________________________________________________________
  // DRELL YAN

  int DY_marker = 29;
  double DY_size = 2.4;
  int DY_col = 61;
  char DY_leg[30];
  sprintf(DY_leg, "FNAL-E772 (DY)");

  //double Q_EXP[] = { 4.955,5.703,6.457,7.007,7.480,8.104,8.925,
  //		     9.785,11.34};

  double Q2_DY[] = { 4.955*4.955, 5.703*5.703, 6.457*6.457, 7.007*7.007, 7.480*7.480, 8.104*8.104, 8.925*8.925,
                     9.785*9.785, 11.34*11.34};

  N = 9;

  // DY_C (9)

  double xT_C_DY[] = { 0.041, 0.062, 0.087, 0.111, 0.136, 0.161, 0.186,
                       0.216, 0.269};

  TGraph *xT_Q2_C_DY = new TGraph( N, xT_C_DY, Q2_DY);
  xT_Q2_C_DY->SetTitle("xT_Q2_C_DY");
  xT_Q2_C_DY->SetName("");
  setMarkerLineType(xT_Q2_C_DY, DY_marker, DY_col, DY_size);
  xT_Q2_C_DY->SetHistogram(myframe);
  xT_Q2_C_DY->Draw("P");
  legend->AddEntry(xT_Q2_C_DY, DY_leg, "P");

  // DY_Ca (9)

  double xT_Ca_DY[] = { 0.041, 0.062, 0.087, 0.111, 0.136, 0.161, 0.186,
                        0.216, 0.269};

  TGraph *xT_Q2_Ca_DY = new TGraph( N, xT_Ca_DY, Q2_DY);
  xT_Q2_Ca_DY->SetTitle("xT_Q2_Ca_DY");
  xT_Q2_Ca_DY->SetName("");
  setMarkerLineType(xT_Q2_Ca_DY, DY_marker, DY_col, DY_size);
  xT_Q2_Ca_DY->SetHistogram(myframe);
  xT_Q2_Ca_DY->Draw("P");

  // DY_FE (9)

  double xT_Fe_DY[] = { 0.041, 0.062, 0.087, 0.111, 0.136, 0.161, 0.186,
                        0.219, 0.271};

  TGraph *xT_Q2_Fe_DY = new TGraph( N, xT_Fe_DY, Q2_DY);
  xT_Q2_Fe_DY->SetTitle("xT_Q2_Fe_DY");
  xT_Q2_Fe_DY->SetName("");
  setMarkerLineType(xT_Q2_Fe_DY, DY_marker, DY_col, DY_size);
  xT_Q2_Fe_DY->SetHistogram(myframe);
  xT_Q2_Fe_DY->Draw("P");

  // DY_W (9)

  double xT_W_DY[] = { 0.041, 0.062, 0.087, 0.111, 0.136, 0.161, 0.186,
                       0.216, 0.269};

  TGraph *xT_Q2_W_DY = new TGraph( N, xT_W_DY, Q2_DY);
  xT_Q2_W_DY->SetTitle("xT_Q2_W_DY");
  xT_Q2_W_DY->SetName("");
  setMarkerLineType(xT_Q2_W_DY, DY_marker, DY_col, DY_size);
  xT_Q2_W_DY->SetHistogram(myframe);
  xT_Q2_W_DY->Draw("P");

  //_____________________________________________________________________________
  // RHIC data

  label = "RHIC data (forw. #eta)";
  TLegend *legend2 = new TLegend(0.192788,0.757009,0.538141,0.933022,label,"brNDC");
  legend2->SetMargin(0.15);
  legend2->SetFillStyle(0);
  legend2->SetBorderSize(0);
  legend2->SetTextSize(0.035);

  int brahms_marker = 21; //25;
  double brahms_size = 2.0;
  int brahms_col = 90; //14;
  char brahms_leg[30];
  sprintf(brahms_leg, "BRAHMS h^{#pm} (#eta = 3.2)");

  double Q2_BRAHMS[] = { 0.25*0.25, 0.75*0.75, 1.25*1.25, 1.75*1.75, 2.25*2.25, 2.75*2.75, 3.25*3.25, 3.75*3.75 };

//   double xT_brahms_rcp[] = { 0.36, 0.40, 0.44, 0.49, 0.52, 0.50, 0.61};
//   double xT_brahms_rdau[] = { 0.30, 0.40, 0.41, 0.52, 0.55, 0.65, 0.70};

  double q_to_xt = 2./200.*exp(-3.2);
  double xT_BRAHMS[] = { 0.25*q_to_xt, 0.75*q_to_xt, 1.25*q_to_xt, 1.75*q_to_xt, 
			 2.25*q_to_xt, 2.75*q_to_xt, 3.25*q_to_xt, 3.75*q_to_xt };

  N = 8;

  TGraph *xT_Q2_BRAHMS = new TGraph( N, xT_BRAHMS, Q2_BRAHMS);
  xT_Q2_BRAHMS->SetTitle("xT_Q2_BRAHMS");
  xT_Q2_BRAHMS->SetName("xT_Q2_BRAHMS");
  setMarkerLineType(xT_Q2_BRAHMS, brahms_marker, brahms_col, brahms_size);
  //xT_Q2_BRAHMS->SetHistogram(myframe);
  xT_Q2_BRAHMS->SetLineWidth(20);
  xT_Q2_BRAHMS->SetLineColor(brahms_col);
  xT_Q2_BRAHMS->SetLineStyle(1);
  xT_Q2_BRAHMS->Draw("C");

  legend2->AddEntry(xT_Q2_BRAHMS, brahms_leg, "P");

  //

  int phnx_marker = 21; //28;
  double phnx_size = 2.; //2.4;
  int phnx_col = 93;//16;
  char phnx_leg[30];
  sprintf(phnx_leg, "PHENIX h^{#pm} (#eta = 1.8)");

  double Q2_PHENIX[] = { 0.75*0.75, 1.25*1.25, 1.75*1.75, 2.25*2.25, 2.75*2.75 };

//   double xT_phnx_rcp[] = { 0.63, 0.70, 0.75, 0.80, 0.70 };
//   double xT_phnx_rdau[] = { 0.63, 0.80, 0.67, 0.75, 0.60 };

  q_to_xt = 2./200.*exp(-1.8);
  double xT_PHENIX[] = { 0.75*q_to_xt, 1.25*q_to_xt, 1.75*q_to_xt, 2.25*q_to_xt, 2.75*q_to_xt };

  N = 5;

  TGraph *xT_Q2_PHENIX = new TGraph( N, xT_PHENIX, Q2_PHENIX);
  xT_Q2_PHENIX->SetTitle("xT_Q2_PHENIX");
  xT_Q2_PHENIX->SetName("xT_Q2_PHENIX");
  setMarkerLineType(xT_Q2_PHENIX, phnx_marker, phnx_col, phnx_size);
  //xT_Q2_PHENIX->SetHistogram(myframe);

  xT_Q2_PHENIX->SetLineWidth(20);
  xT_Q2_PHENIX->SetLineColor(phnx_col);
  xT_Q2_PHENIX->SetLineStyle(1);
  xT_Q2_PHENIX->Draw("C");

  legend2->AddEntry(xT_Q2_PHENIX, phnx_leg, "P");

  c22->cd();
  legend2->Draw();

  c22->Update();

  //_____________________________________________________________________________

  c22->cd();
  legend->Draw();

  TLine *line = new TLine(xmin, 1., xmax, 1.);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw();

  TLatex *tex = new TLatex(1.4e-05, 4., "perturbative");
  tex->SetTextSize(0.03);
  tex->SetLineWidth(2);
  tex->Draw();

  TLatex *tex2 = new TLatex(1.4e-05, 0.26, "non-perturbative");
  tex2->SetTextSize(0.03);
  tex2->SetLineWidth(2);
  tex2->Draw();

}

//_____________________________________________________________________________
//
void
emcAnalyzer::plot_pp_cross_sections_vs_sqrts()
{

  char title[100];

  sprintf(title, "cross_sections_vs_sqrts");

  TCanvas *c22 = (TCanvas*)canvas(title, 750, 560);
  c22->Range( -5.8, -2.9, 0.0, 4.1);
  c22->SetLeftMargin(0.14);
  c22->SetRightMargin(0.0042);
  c22->SetTopMargin(0.019);

  Axis_t xmin = 0.002;
  Axis_t xmax = 250.; // 5000.; 
  int ncx = 100;
  Axis_t ymin = 2;
  Axis_t ymax = 500. ;
  int ncy = 100;

  TH2F *myframe = (TH2F*)frame(title, ncx, xmin, xmax, ncy, ymin, ymax,
                               "#sqrt{s} (GeV)", "#sigma (mb)");
  myframe->GetYaxis()->SetTitleOffset(1.1);
  myframe->GetXaxis()->SetLabelOffset(0.0);
  myframe->GetYaxis()->SetMoreLogLabels();
  myframe->Draw();
  c22->SetLogx();
  c22->SetLogy();
  c22->Update();

  TString label = "NN total and elastic cross-sections versus #sqrt{s}:";
  TLegend *le = new TLegend(0.54, 0.72, 0.90, 0.97, label, "brNDC");
  le->SetMargin(0.1);
  le->SetFillStyle(0);
  le->SetBorderSize(0);
  le->SetTextSize(0.035);
  //  legend->Draw();

  // Experimental data

 TGraphErrors *eband;  

 TGraphErrors *pp_tot = (TGraphErrors*)plot_spectrum_from_ascii("pp_sigma_tot.dat",eband);
 TGraphErrors *pp_el  = (TGraphErrors*)plot_spectrum_from_ascii("pp_sigma_el.dat",eband);
 TGraphErrors *np_tot = (TGraphErrors*)plot_spectrum_from_ascii("np_sigma_tot.dat",eband);
 TGraphErrors *np_el  = (TGraphErrors*)plot_spectrum_from_ascii("np_sigma_el.dat",eband);
 TGraphErrors *pn_tot = (TGraphErrors*)plot_spectrum_from_ascii("pn_sigma_tot.dat",eband);

 TGraphErrors *ppbar_tot = (TGraphErrors*)plot_spectrum_from_ascii("ppbar_sigma_tot.dat",eband);
 TGraphErrors *ppbar_el  = (TGraphErrors*)plot_spectrum_from_ascii("ppbar_sigma_el.dat",eband);

 setMarkerLineType(pp_tot, 20, 2, 1.8);
 setMarkerLineType(pp_el,  22, 2, 2.);
 setMarkerLineType(np_tot, 20, 4, 1.8);
 setMarkerLineType(pn_tot, 20, 4, 1.8);
 setMarkerLineType(np_el,  22, 4, 2.);

 setMarkerLineType(ppbar_tot, 24, 6, 1.8);
 setMarkerLineType(ppbar_el,  26, 6, 2.);

 c22->cd();
 pp_tot->Draw("P");
 pp_el->Draw("P");

 np_tot->Draw("P");
 np_el->Draw("P");
 pn_tot->Draw("P");

 ppbar_tot->Draw("P");
 ppbar_el->Draw("P");

 le->AddEntry(pp_tot, "p+p total", "P");
 le->AddEntry(pp_el , "p+p elastic", "P");
 le->AddEntry(np_tot, "n+p,p+n total", "P");
 //le->AddEntry(pn_tot, "p+n total", "P");
 le->AddEntry(np_el , "n+p elastic", "P");
 le->AddEntry(ppbar_tot, "p+#bar{p} total", "P");
 le->AddEntry(ppbar_el, "p+#bar{p} elastic", "P");
 le->Draw();

 TF1 *compete = (TF1*)plot_pp_sigma_parametrization("pp","compete");
 compete->SetLineColor(1);
 compete->SetLineStyle(1);

 TF1 *el_ln2s = new TF1("ln2s","[0]+[1]*(x*x)^[2]+[3]*pow(log(x*x),2.)",5,5000);
 el_ln2s->SetParameters(3.42,34.50,-0.5405,0.060);
 el_ln2s->SetLineStyle(2);

 c22->cd();
 compete->Draw("same");
 el_ln2s->Draw("same");

 c22->Update();

}

//_____________________________________________________________________________
//
void
emcAnalyzer::plot_ee_gg_gp_cross_sections_vs_sqrts()
{

  char title[100];

  sprintf(title, "ee_gg_gp_sigma_had_vs_sqrts");

  TCanvas *c22 = (TCanvas*)canvas(title, 750, 560);
  c22->Range( -5.8, -2.9, 0.0, 4.1);
  c22->SetLeftMargin(0.116);
  c22->SetRightMargin(0.005);
  c22->SetTopMargin(0.056);

  Axis_t ymin = 2.e-09;
  Axis_t ymax = 100.; 
  int ncx = 100;
  Axis_t xmin = 0.3;
  Axis_t xmax = 300. ;
  int ncy = 100;

  TH2F *myframe = (TH2F*)frame(title, ncx, xmin, xmax, ncy, ymin, ymax,"#sqrt{s} (GeV)", "#sigma (mb)");
  myframe->GetYaxis()->SetTitleOffset(1.1);
  myframe->GetXaxis()->SetLabelOffset(0.0);
  myframe->GetXaxis()->SetTitleOffset(1.2);
  //myframe->GetYaxis()->SetMoreLogLabels();
  myframe->Draw();
  c22->SetLogx();
  c22->SetLogy();
  c22->Update();

  TString label = "Hadronic cross-sections versus #sqrt{s}:";
  TLegend *le = new TLegend(0.521448,0.80566,0.969169,0.979245,label,"brNDC");
  //TLegend *le = new TLegend(0.54, 0.72, 0.90, 0.97, label, "brNDC");
  le->SetMargin(0.2);
  le->SetFillColor(0);
  le->SetFillStyle(1001);
  le->SetBorderSize(4);
  le->SetTextSize(0.035);
  //  legend->Draw();

  // Experimental data

 TGraphErrors *eband;  

 gStyle->SetOptFit(0);

 TGraphErrors *ee_tot = (TGraphErrors*)plot_spectrum_from_ascii("epem_sigma_tot.dat",eband);
 TGraphErrors *gg_tot = (TGraphErrors*)plot_spectrum_from_ascii("gg_sigma_tot.dat",eband);
 TGraphErrors *gp_tot = (TGraphErrors*)plot_spectrum_from_ascii("gp_sigma_tot.dat",eband);

 setMarkerLineType(ee_tot, 28, 51, 0.8, 1,51);
 setMarkerLineType(gg_tot, 24, 2, 1.2, 1,2);
 setMarkerLineType(gp_tot, 25, 95, 1.0, 1,95);

 // (c) 2002 COMPETE http://qcd.theo.phys.ulg.ac.be/compete/predictor.html 
 // GAMMAGAMMA_ECM = 0.000335 - 0.000060839/(ECM**2)**0.458 + 2.91e-6*(-3.3644+LOG(ECM**2))**2 
 // GAMMAP_ECM     = 0.109083 + 0.031650/(ECM**2)**0.45815 + 0.00094746*(-3.3644+LOG(ECM**2))**2 
 TF1 *fit_gg_compete = new TF1("fit_gg_compete","[0]+[1]*pow(x*x,[2])+[3]*pow([4]+log(x*x),2.)",1.4,175.);
 fit_gg_compete->SetParameters(0.00033565, -0.000060839, -0.45814, 2.9153e-6, -3.3644);
 fit_gg_compete->SetLineWidth(1);
 fit_gg_compete->SetLineColor(2);

 TF1 *fit_gp_compete = new TF1("fit_gp_compete","[0]+[1]*pow(x*x,[2])+[3]*pow([4]+log(x*x),2.)",1.7,230.);
 fit_gp_compete->SetParameters(0.109083, 0.031650, -0.45815, 0.00094746, -3.3644);
 fit_gp_compete->SetLineWidth(1);
 fit_gp_compete->SetLineColor(95);

 c22->cd();
 fit_gg_compete->Draw("same");
 fit_gp_compete->Draw("same");
 ee_tot->Draw("P");
 gg_tot->Draw("P");
 gp_tot->Draw("P");

 le->AddEntry(ee_tot, "e^{+} e^{-} #rightarrow hadrons (total)", "P");
 le->AddEntry(gg_tot, "#gamma #gamma #rightarrow hadrons (total)", "P");
 le->AddEntry(gp_tot, "#gamma p  #rightarrow hadrons (total)", "P");
 le->Draw();

 c22->Update();

 return;

 //TF1 *compete = (TF1*)plot_pp_sigma_parametrization("pp","compete");
 //compete->SetLineColor(1);
 //compete->SetLineStyle(1);

 TF1 *el_ln2s = new TF1("ln2s","[0]+[1]*(x*x)^[2]+[3]*pow(log(x*x),2.)",5,5000);
 el_ln2s->SetParameters(3.42,34.50,-0.5405,0.060);
 el_ln2s->SetLineStyle(2);

 c22->cd();
 //compete->Draw("same");
 el_ln2s->Draw("same");

 c22->Update();

}

//_____________________________________________________________________________
TGraphErrors*
emcAnalyzer::plot_RAA_RHIC_SPS_alphaISR( const int Cent1,
					 const int Cent2,
					 const int CentSPS,
					 const char* cut )
{

  TString cut_str = cut;

  bool bin_scaling = true;

  char title[100];
  sprintf(title, "RAA_RHIC_SPS_ISR_0_%d", CentSPS);

  TCanvas *c0 = (TCanvas*)canvas(title, 715, 560);
  c0->Range(-1.3754,-0.469388,10.5503,3.02041);
  c0->SetLeftMargin(0.115);
  c0->SetRightMargin(0.0042);
  c0->SetTopMargin(0.005);
  c0->SetBottomMargin(0.13);

  double max_RAA = 3.0;
  double min_RAA = 0.;
  double ptmin = 0.;
  double ptmax = 10.5;

  TH2F *myframe = (TH2F*)frame(title, 10, ptmin, ptmax, 20, min_RAA, max_RAA, "p_{T} (GeV/#font[72]{c})", "R_{AA}");
  myframe->GetYaxis()->SetTitleOffset(0.7);
  myframe->GetXaxis()->SetTitleOffset(0.85);
  myframe->GetYaxis()->SetLabelSize(0.055);
  myframe->GetXaxis()->SetLabelSize(0.055);
  //myframe->Draw();
  c0->Update();

  //

  TGraphErrors *RAA_200 = 0;

  fPlot = false;
  fFit = false;

  TGraphErrors *errBand;
  TGraph* eband_phnx200 = 0;

  if (cut_str.Contains("QM02"))
    {
      RAA_200 = (TGraphErrors*)plot_spectrum_from_ascii("RAA_0_10_QM02.txt", (TGraphErrors*&)errBand); // QM02
    }
  else
    {
      RAA_200 = (TGraphErrors*)plot_spectrum_from_ascii("RAA_emcal_0_10_tof1chisq1.txt", (TGraphErrors*&)errBand);
      TClonesArray* eb = (TClonesArray*)errorBand( errBand );
      eband_phnx200 = (TGraph*)(*eb)[0];
      //RAA_200 = (TGraphErrors*)plot_RAA(Cent1,Cent2, cut, true, true, true );
    }

  //TGraphErrors *RAA_130 = (TGraphErrors*)get_pi0_130_RAA();
  TGraphErrors *RAA_130 = (TGraphErrors*)plot_spectrum_from_ascii("PizeroAuAu130_RAA.dat", (TGraphErrors*&)errBand);

  //TGraphErrors *RAA_SPS = (TGraphErrors*)plot_spectrum_from_ascii("R_AA_wa98.txt",(TGraphErrors*&)errBand);
  //TGraphErrors *RAA_SPS = (TGraphErrors*)plot_klaus_pi0_WA98_RAA(c0);

  char sps_file[300];
  sprintf(sps_file, "RAA_blatt_wa98_0_%d.txt", CentSPS); // 0-1%,0-7%,0-13%, ...
  TGraphErrors *RAA_SPS = (TGraphErrors*)plot_spectrum_from_ascii(sps_file, (TGraphErrors*&)errBand);

  // error band (1)
  TClonesArray* eb = (TClonesArray*)errorBand( errBand );
  TGraph* eband_wa98 = (TGraph*)(*eb)[0];

  // error boxes (2)
  TClonesArray* eboxes_wa98 = (TClonesArray*)errorBoxes( errBand, 0.);

  TGraphErrors *RAA_ISR = (TGraphErrors*)plot_spectrum_from_ascii("RAA_pi0_ISR_31.txt", (TGraphErrors*&)errBand);

  if (!RAA_200 && !RAA_130 && !RAA_SPS && !RAA_ISR ) return 0;

  RAA_200->SetHistogram(myframe);
  RAA_130->SetHistogram(myframe);
  RAA_SPS->SetHistogram(myframe);
  RAA_ISR->SetHistogram(myframe);

  //TLegend *le = new TLegend(0.43,0.70,0.95,0.956,"   #pi^{0} 0-10% central:","brNDC");
  TLegend *le = new TLegend(0.45, 0.737, 0.944, 0.992, NULL, "brNDC");
  le->SetTextSize(0.0308);
  le->SetMargin(0.12);
  le->SetBorderSize(4);

  char leg_sps[100];
  sprintf(leg_sps, "Pb+Pb #rightarrow #pi^{0}+X @ #sqrt{s_{NN}} = 17.3 GeV (0-%d%% central)", CentSPS);
  le->AddEntry(RAA_SPS, leg_sps, "P");
  le->AddEntry(RAA_ISR, "#alpha + #alpha #rightarrow #pi^{0}+X @ #sqrt{s_{NN}} = 31.0 GeV (min.bias)", "P");
  le->AddEntry(RAA_130, "Au+Au #rightarrow #pi^{0}+X @ #sqrt{s_{NN}} = 130 GeV (0-10% central)", "P");
  le->AddEntry(RAA_200, "Au+Au #rightarrow #pi^{0}+X @ #sqrt{s_{NN}} = 200 GeV (0-10% central)", "P");
  le->SetFillColor(kWhite);

  // Not needed anymore: errors added to the point bar now.
  //   // Draw PHENIX 130 errors
  //   TBox *box = 0;
  //   TClonesArray *array_of_boxes = 0;
  //   double boxwidth = 0.1;
  //   array_of_boxes = (TClonesArray*)plot_pi0_130_errors(boxwidth);
  //   int boxes = array_of_boxes->GetLast()+1;
  //   for (int j=0; j<boxes;j++)
  //     {
  //       box = ((TBox*)(*array_of_boxes)[j]);
  //       c0->cd();
  //       box->Draw();
  //       box->SetFillStyle(3001);
  //       box->SetFillColor(1);
  //     }

  //   // Draw WA98 errors
  //   TLine *lu = 0; TLine *lul = 0 ; TLine *lur = 0 ;
  //   TLine *ll = 0; TLine *lll = 0 ; TLine *llr = 0 ;
  //   TClonesArray *array_of_lines = (TClonesArray*)get_klaus_pi0_WA98_RAA_errors();
  //   int lines = array_of_lines->GetLast()+1;
  //   for (int j=0; j<(lines-6);j++)
  //     {
  //       lu  = ((TLine *) (*array_of_lines)[j++]);
  //       lul = ((TLine *) (*array_of_lines)[j++]);
  //       lur = ((TLine *) (*array_of_lines)[j++]);
  //       ll  = ((TLine *) (*array_of_lines)[j++]);
  //       lll = ((TLine *) (*array_of_lines)[j++]);
  //       llr = ((TLine *) (*array_of_lines)[j]);

  //     }

  // PHENIX 200 GeV Ncoll errors

  const int N = RAA_200->GetN();

  double *extra_erelat = new double[N];

  double pp_abs_yield_erelat = 0.096; // Additional 9.6% error on pp yield (BBC efficiency + luminosity): AN184
  double FiduDeadWarnPlusAccRelatError = 0.05; // /Sqrt(2.) ;
  double ppAcceptanceError = 0.038;

  for (int i = 0; i < N; i++)
    {
      extra_erelat[i] = quadRelatError( pp_abs_yield_erelat, 1.,  // pp BBC,sigma normalization
                                        ppAcceptanceError, 1.,    // pp acceptance
                                        FiduDeadWarnPlusAccRelatError, 1.); // AuAu acceptance

      // draw PHENIX 200 GeV Ncoll errors linearly added to normal errors

      double x = 0.;
      double y = 0.;
      RAA_200->GetPoint(i, x, y);
      double ey = RAA_200->GetErrorY(i) + y*extra_erelat[i];
      double ex = RAA_200->GetErrorX(i);
      RAA_200->SetPointError(i, ex, ey);
    }

  //  // draw PHENIX 200 GeV Ncoll errors as bands

  //   bool useTAB = true;
  //   TH1F *h_eNcoll_RAA = (TH1F*)getPropagErrorNcollBand(RAA_200,Cent1,Cent2,0,0,extra_erelat,
  // 						      bin_scaling,useTAB,fVerbose);
  //   c0->cd();
  //   double exwidth = 0.35;
  //   gStyle->SetErrorX(exwidth);
  //   //h_eNcoll_RAA->Smooth(1);
  //   h_eNcoll_RAA->SetFillColor(1);
  //   h_eNcoll_RAA->SetFillStyle(3001);
  //   h_eNcoll_RAA->Draw("e2same");


  // // draw PHENIX 200 GeV Ncoll errors as boxes

  //   for (int i=0; i<Npoints; i++)
  //     {
  //       pT_corr_errors = new TBox(pT[i]-0.25,ratioAuAu_pp_Pi0[i]-eratioNcoll[i],
  // 				pT[i]+0.25,ratioAuAu_pp_Pi0[i]+eratioNcoll[i]);
  //       if (strcasecmp(type,"central_130") == 0) pT_corr_errors->SetFillColor(50);
  //       else pT_corr_errors->SetFillColor(kYellow);
  //       pT_corr_errors->Draw();
  //     }

  TLine *line = new TLine(ptmin, 1., ptmax, 1.);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);

  // Npart line
  double NcollAuAu = getNcoll(Cent1, Cent2, "value");
  double Ncoll_pp = getNcoll("pp");
  double NpartAuAu = getNpart(Cent1, Cent2, "value");
  double Npart_pp = getNpart("pp");

  double alternative_scaling = 0.;
  if (bin_scaling)
    {
      alternative_scaling = NpartAuAu*Ncoll_pp/(NcollAuAu*Npart_pp);
    }
  else
    {
      alternative_scaling = NcollAuAu*Npart_pp/(NpartAuAu*Ncoll_pp);
    }

  TLine *line2 = new TLine(ptmin, alternative_scaling, ptmax, alternative_scaling);
  line2->SetLineColor(1);
  line2->SetLineStyle(3);
  line2->SetLineWidth(3);

  c0->cd();

  double scale = 1.2;

  setMarkerLineType(RAA_SPS, 22, 51, 1.9*scale, 1, 1);
  RAA_SPS->Draw("AP");

  eband_wa98->SetFillColor(16);
  //eband_wa98->Draw("f"); // "standard" error band

  // box-errorband looks better in this case ...
  TBox *box = 0;
  for (int j = 0; j < eboxes_wa98->GetEntries();j++)
    {
      box = ((TBox*)(*eboxes_wa98)[j]);
      box->SetFillColor(16);
      box->Draw();
    }
  RAA_SPS->Draw("P");
  c0->Update();

  setMarkerLineType(RAA_ISR, 29, 6, 2.5, 1, 1);
  RAA_ISR->Draw("P");
  c0->Update();

  //   TGraphErrors *RAA_ISR2 = (TGraphErrors*)RAA_ISR->Clone();
  //   setMarkerLineType(RAA_ISR2,29,5,1.7);
  //   RAA_ISR2->Draw("P");

  //   TMarker *marker = new TMarker(4.62359,2.40915,29);
  //   marker->SetMarkerColor(5);
  //   marker->SetMarkerStyle(23);
  //   marker->SetMarkerSize(1.5);
  //   marker->Draw();

  setMarkerLineType(RAA_130, 21, 93, 1.5*scale, 1, 1);
  RAA_130->Draw("P");
  c0->Update();

  //   TGraph *RAA_130b = (TGraph*)RAA_130->Clone();
  //   setMarkerLineType(RAA_130b,21,5,1.3*scale);
  //   RAA_130b->Draw("P");

  //   TMarker *marker2 = new TMarker(4.62359,2.6025,23);
  //   marker2->SetMarkerColor(5);
  //   marker2->SetMarkerStyle(29);
  //   marker2->SetMarkerSize(1.7);
  //   marker2->Draw();

  setMarkerLineType(RAA_200, 20, 2, 1.6*scale, 1, 1);
  //eband_phnx200->Draw("f");
  RAA_200->Draw("P");

  line->Draw("same");
  le->Draw("same");
  line2->Draw("same");

  //TPave *pave = new TPave(3.78469,2.95,4.38756,3.0,0,"br");
  //pave->SetFillColor(10);
  //pave->Draw();

  c0->Update();

  return RAA_SPS;

}

//_____________________________________________________________________________
TGraphErrors*
emcAnalyzer::plot_RAA_RHIC_SPS_ISR( const int Cent1,
                                    const int Cent2,
                                    const int CentSPS,
                                    const char* cut )
{

  TString cut_str = cut;

  bool bin_scaling = true;

  char title[100];
  sprintf(title, "denterria_RAA_pi0_all_sqrts_%d_%d", Cent1, Cent2);

  TCanvas *c0 = (TCanvas*)canvas(title,808,572);
  c0->Range(-2.02629,-1.56152,15.5936,1.04264);
  c0->SetLeftMargin(0.115);
  c0->SetRightMargin(0.00531209);
  c0->SetTopMargin(0.0434783);
  c0->SetBottomMargin(0.130435);
  c0->SetLogy();

  double max_RAA = 8.5;
  double min_RAA = 0.06;
  double ptmin = 0.;
  double ptmax = 15.5;

  TH2F *myframe = (TH2F*)frame(title, 10, ptmin, ptmax, 20, min_RAA, max_RAA, "p_{T} (GeV/#font[72]{c})", "R_{AA}");
  myframe->GetYaxis()->SetTitleOffset(0.7);
  myframe->GetXaxis()->SetTitleOffset(0.85);
  myframe->GetYaxis()->SetLabelSize(0.055);
  myframe->GetXaxis()->SetLabelSize(0.055);
  c0->cd();
  myframe->Draw();
  c0->Update();

  //

  fPlot = false;
  fFit = false;

  TGraphErrors *errBand;

  TGraphErrors *RAA_200= (TGraphErrors*)plot_RAA(0,10,"emcalrun3",false,true,true);
  //emcAnalyzerUtils::setMarkerLineType(RAApi0cent, 20, 2, 1.8);

  //TGraphErrors *RAA_200 = (TGraphErrors*)plot_spectrum_from_ascii("RAA_emcal_0_10_tof1chisq1_extlvl2.txt",errBand);
  //TClonesArray *eb = (TClonesArray*)errorBand( errBand );
  //TGraph *eband_phnx200 = (TGraph*)(*eb)[0];
  TGraphErrors *RAA_200_bis = (TGraphErrors*)RAA_200->Clone();

  TGraphErrors *RAA_130 = (TGraphErrors*)plot_spectrum_from_ascii("PizeroAuAu130_RAA_shifted.dat",errBand);
  TGraphErrors *RAA_130_bis = (TGraphErrors*)RAA_130->Clone();

  char sps_file[300];
  sprintf(sps_file, "RAA_blatt_wa98_0_%d_rebin.txt", CentSPS); // 0-1%,0-7%,0-13%, ...
  TGraphErrors *RAA_SPS = (TGraphErrors*)plot_spectrum_from_ascii(sps_file, (TGraphErrors*&)errBand);
  TGraphErrors *RAA_SPS_bis = (TGraphErrors*)RAA_SPS->Clone();

  // error band (1)
  //eb = (TClonesArray*)errorBand( errBand );
  //TGraph* eband_wa98 = (TGraph*)(*eb)[0];
  // error boxes (2)
  //TClonesArray* eboxes_wa98 = (TClonesArray*)errorBoxes( errBand, 0.);

  TGraphErrors *RAA_ISR = (TGraphErrors*)plot_RAA_pi0_62GeV(0,10);
    //plot_spectrum_from_ascii("RAA_pi0_ISR_31.txt", (TGraphErrors*&)errBand);
  TGraphErrors *RAA_ISR_bis = (TGraphErrors*)RAA_ISR->Clone();

  if (!RAA_200 && !RAA_130 && !RAA_SPS && !RAA_ISR ) return 0;

  TLegend *le = new TLegend(0.46393,0.664122,0.978856,0.992366,"High p_{T} #pi^{0} in 0-10% central A+A:","brNDC");
  le->SetTextSize(0.0305343);
  le->SetBorderSize(4);
  le->SetMargin(0.2);
  le->SetFillColor(kWhite);

  char leg_sps[100];
  sprintf(leg_sps, "Pb+Pb @ #sqrt{s_{NN}} = 17.3 GeV (WA98)");
  le->AddEntry(RAA_SPS, leg_sps, "P");
  //le->AddEntry(RAA_ISR, "#alpha + #alpha #rightarrow #pi^{0}+X @ #sqrt{s_{NN}} = 31.0 GeV (min.bias)", "P");
  le->AddEntry(RAA_ISR, "Au+Au @ #sqrt{s_{NN}} = 62.4 GeV (PHENIX Preliminary)", "P");
  le->AddEntry(RAA_130, "Au+Au @ #sqrt{s_{NN}} = 130 GeV  (PHENIX)", "P");
  le->AddEntry(RAA_200, "Au+Au @ #sqrt{s_{NN}} = 200 GeV (PHENIX Preliminary)", "P");

  c0->cd();
  le->Draw();

  TGraphErrors *eband;
  TGraphErrors *vitev400 = (TGraphErrors*)plot_spectrum_from_ascii("R_AA_theor_vitev_sps_dndy400.txt",eband);
  TGraphErrors *vitev650 = (TGraphErrors*)plot_spectrum_from_ascii("R_AA_theor_vitev_sps_dndy650.txt",eband);
  TGraphErrors *vitev1100 = (TGraphErrors*)plot_spectrum_from_ascii("R_AA_theor_vitev_rhic_pi0_dNdy1100.txt",eband);
  setMarkerLineType(vitev400, 21,90,2, 1,7,12);
  setMarkerLineType(vitev650, 21,90,2, 1,29,12);
  setMarkerLineType(vitev1100, 21,90,2, 1,91,12);

//   le->AddEntry(vitev400, "GLV energy loss (dN^{g}/dy = 400) [I.Vitev]", "L");
//   le->AddEntry(vitev650, "GLV energy loss (dN^{g}/dy = 650) [I.Vitev]", "L");
//   le->AddEntry(vitev1100, "GLV energy loss (dN^{g}/dy = 1100) [I.Vitev]", "L");
  le->AddEntry(vitev400, "GLV energy loss (dN^{g}/dy = 400)", "L");
  le->AddEntry(vitev650, "GLV energy loss (dN^{g}/dy = 650)", "L");
  le->AddEntry(vitev1100, "GLV energy loss (dN^{g}/dy = 1100)", "L");

  c0->cd();
  vitev400->Draw("L");
  vitev650->Draw("L");
  vitev1100->Draw("L");

//   // PHENIX 200 GeV Ncoll errors

//   const int N = RAA_200->GetN();

//   double *extra_erelat = new double[N];

//   double pp_abs_yield_erelat = 0.096; // Additional 9.6% error on pp yield (BBC efficiency + luminosity): AN184
//   double FiduDeadWarnPlusAccRelatError = 0.05; // /Sqrt(2.) ;
//   double ppAcceptanceError = 0.038;

//   for (int i = 0; i < N; i++)
//     {
//       extra_erelat[i] = quadRelatError( pp_abs_yield_erelat, 1.,  // pp BBC,sigma normalization
//                                         ppAcceptanceError, 1.,    // pp acceptance
//                                         FiduDeadWarnPlusAccRelatError, 1.); // AuAu acceptance

//       // draw PHENIX 200 GeV Ncoll errors linearly added to normal errors

//       double x = 0.;
//       double y = 0.;
//       RAA_200->GetPoint(i, x, y);
//       double ey = RAA_200->GetErrorY(i) + y*extra_erelat[i];
//       double ex = RAA_200->GetErrorX(i);
//       //RAA_200->SetPointError(i, ex, ey);
//     }

  //  // draw PHENIX 200 GeV Ncoll errors as bands

  //   bool useTAB = true;
  //   TH1F *h_eNcoll_RAA = (TH1F*)getPropagErrorNcollBand(RAA_200,Cent1,Cent2,0,0,extra_erelat,
  // 						      bin_scaling,useTAB,fVerbose);
  //   c0->cd();
  //   double exwidth = 0.35;
  //   gStyle->SetErrorX(exwidth);
  //   //h_eNcoll_RAA->Smooth(1);
  //   h_eNcoll_RAA->SetFillColor(1);
  //   h_eNcoll_RAA->SetFillStyle(3001);
  //   h_eNcoll_RAA->Draw("e2same");


  // // draw PHENIX 200 GeV Ncoll errors as boxes

  //   for (int i=0; i<Npoints; i++)
  //     {
  //       pT_corr_errors = new TBox(pT[i]-0.25,ratioAuAu_pp_Pi0[i]-eratioNcoll[i],
  // 				pT[i]+0.25,ratioAuAu_pp_Pi0[i]+eratioNcoll[i]);
  //       if (strcasecmp(type,"central_130") == 0) pT_corr_errors->SetFillColor(50);
  //       else pT_corr_errors->SetFillColor(kYellow);
  //       pT_corr_errors->Draw();
  //     }

  TLine *line = new TLine(ptmin, 1., ptmax, 1.);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);

//   // Npart line
//   double NcollAuAu = getNcoll(Cent1, Cent2, "value");
//   double Ncoll_pp = getNcoll("pp");
//   double NpartAuAu = getNpart(Cent1, Cent2, "value");
//   double Npart_pp = getNpart("pp");

//   double alternative_scaling = 0.;
//   if (bin_scaling)
//     {
//       alternative_scaling = NpartAuAu*Ncoll_pp/(NcollAuAu*Npart_pp);
//     }
//   else
//     {
//       alternative_scaling = NcollAuAu*Npart_pp/(NpartAuAu*Ncoll_pp);
//     }

//   TLine *line2 = new TLine(ptmin, alternative_scaling, ptmax, alternative_scaling);
//   line2->SetLineColor(1);
//   line2->SetLineStyle(3);
//   line2->SetLineWidth(3);

  c0->cd();

  double scale = 1.2;

  setMarkerLineType(RAA_SPS, 22, 51, 1.9*scale, 1, 1);
  RAA_SPS->Draw("P");
  setMarkerLineType(RAA_SPS_bis, 26, 1, 1.9*scale, 1, 1);
  //RAA_SPS_bis->Draw("P");

//   eband_wa98->SetFillColor(16);
//   //eband_wa98->Draw("f"); // "standard" error band
//
//   // box-errorband looks better in this case ...
//   TBox *box = 0;
//   for (int j = 0; j < eboxes_wa98->GetEntries();j++)
//     {
//       box = ((TBox*)(*eboxes_wa98)[j]);
//       box->SetFillColor(16);
//       box->Draw();
//     }
//   RAA_SPS->Draw("P");
//   c0->Update();

  setMarkerLineType(RAA_ISR, 20, 92, 1.8, 1, 1);
  RAA_ISR->Draw("P");
  setMarkerLineType(RAA_ISR_bis, 24, 1, 1.8, 1, 1);
  RAA_ISR_bis->Draw("P");
  c0->Update();

  //   TGraphErrors *RAA_ISR2 = (TGraphErrors*)RAA_ISR->Clone();
  //   setMarkerLineType(RAA_ISR2,29,5,1.7);
  //   RAA_ISR2->Draw("P");

  //   TMarker *marker = new TMarker(4.62359,2.40915,29);
  //   marker->SetMarkerColor(5);
  //   marker->SetMarkerStyle(23);
  //   marker->SetMarkerSize(1.5);
  //   marker->Draw();

  setMarkerLineType(RAA_130, 21, 5, 1.5*scale, 1, 1);
  RAA_130->Draw("P");
  setMarkerLineType(RAA_130_bis, 25, 1, 1.5*scale, 1, 1);
  RAA_130_bis->Draw("P");
  c0->Update();

  //   TGraph *RAA_130b = (TGraph*)RAA_130->Clone();
  //   setMarkerLineType(RAA_130b,21,5,1.3*scale);
  //   RAA_130b->Draw("P");

  //   TMarker *marker2 = new TMarker(4.62359,2.6025,23);
  //   marker2->SetMarkerColor(5);
  //   marker2->SetMarkerStyle(29);
  //   marker2->SetMarkerSize(1.7);
  //   marker2->Draw();

  setMarkerLineType(RAA_200, 20, 2, 1.5*scale, 1, 1);
  //eband_phnx200->Draw("f");
  RAA_200->Draw("P");

  setMarkerLineType(RAA_200_bis, 24, 1, 1.5*scale, 1, 1);
  RAA_200_bis->Draw("P");

  line->Draw("same");
  //line2->Draw("same");

  //TPave *pave = new TPave(3.78469,2.95,4.38756,3.0,0,"br");
  //pave->SetFillColor(10);
  //pave->Draw();

  c0->Update();

  return RAA_SPS;

}

//_____________________________________________________________________________
void
emcAnalyzer::plot_RAA_charged_RHIC(int Cent1, int Cent2)
{

  char title[100];
  sprintf(title, "RAA_%d_%d_charged_RHIC", Cent1, Cent2);

  TCanvas *c0 = (TCanvas*)canvas(title, 700, 600);
  c0->Range( -1.23529, -9.51842, 9.28235, 2.69298);
  c0->SetLeftMargin(0.11745);
  c0->SetRightMargin(0.0268456);
  c0->SetTopMargin(0.0567485);

  double max_RAA = 1.5;
  double min_RAA = 0.;
  double ptmin = 0.;
  double ptmax = 10.5;

  TH2F *myframe = (TH2F*)frame(title, 20, ptmin, ptmax, 20, min_RAA, max_RAA, "p_{T} (GeV/#font[72]{c})", "R_{AA}");
  myframe->Draw();
  c0->Update();

  fPlot = false;
  fFit = false;

  TGraphErrors *errBand;

  TGraphErrors *RAA_phenix = (TGraphErrors*)plot_spectrum_from_ascii("RAA_chhad_phenix_0_10.txt", (TGraphErrors*&)errBand);
  TGraphErrors *RAA_star = (TGraphErrors*)plot_spectrum_from_ascii("RAA_chhad_star_0_5.txt", (TGraphErrors*&)errBand);
  TGraphErrors *RAA_phobos = (TGraphErrors*)plot_spectrum_from_ascii("RAA_chhad_phobos_0_6.txt", (TGraphErrors*&)errBand);
  TGraphErrors *RAA_brahms = (TGraphErrors*)plot_spectrum_from_ascii("RAA_chhad_brahms_0_10.txt", (TGraphErrors*&)errBand);

  if ( !RAA_phenix || !RAA_star || !RAA_phobos || !RAA_brahms ) return ;

  TLegend *le = new TLegend(0.43, 0.70, 0.95, 0.956,
                            " Au+Au #rightarrow h^{#pm}+X  @ #sqrt{s_{NN}} = 200 GeV", "brNDC");
  le->SetMargin(0.1);
  le->SetTextSize(0.045);
  le->AddEntry(RAA_star, "STAR (0-5%)", "P");
  le->AddEntry(RAA_phenix, "PHENIX (0-10%)", "P");
  le->AddEntry(RAA_phobos, "PHOBOS (0-6%)", "P");
  le->AddEntry(RAA_brahms, "BRAHMS (0-10%)", "P");
  le->SetFillColor(kWhite);
  le->SetBorderSize(0);
  le->Draw();

  // Not needed anymore: errors added to the point bar now.
  //   // Draw PHENIX 130 errors
  //   TBox *box = 0;
  //   TClonesArray *array_of_boxes = 0;
  //   double boxwidth = 0.1;
  //   array_of_boxes = (TClonesArray*)plot_pi0_130_errors(boxwidth);
  //   int boxes = array_of_boxes->GetLast()+1;
  //   for (int j=0; j<boxes;j++)
  //     {
  //       box = ((TBox*)(*array_of_boxes)[j]);
  //       c0->cd();
  //       box->Draw();
  //       box->SetFillStyle(3001);
  //       box->SetFillColor(1);
  //     }

  //   // Draw WA98 errors
  //   TLine *lu = 0; TLine *lul = 0 ; TLine *lur = 0 ;
  //   TLine *ll = 0; TLine *lll = 0 ; TLine *llr = 0 ;
  //   TClonesArray *array_of_lines = (TClonesArray*)get_klaus_pi0_WA98_RAA_errors();
  //   int lines = array_of_lines->GetLast()+1;
  //   for (int j=0; j<(lines-6);j++)
  //     {
  //       lu  = ((TLine *) (*array_of_lines)[j++]);
  //       lul = ((TLine *) (*array_of_lines)[j++]);
  //       lur = ((TLine *) (*array_of_lines)[j++]);
  //       ll  = ((TLine *) (*array_of_lines)[j++]);
  //       lll = ((TLine *) (*array_of_lines)[j++]);
  //       llr = ((TLine *) (*array_of_lines)[j]);

  //     }

  // PHENIX 200 GeV Ncoll errors

  //  const int N = RAA_phenix->GetN();

  //  double *extra_erelat = new double[N];

  //  double pp_abs_yield_erelat = 0.096; // Additional 9.6% error on pp yield (BBC efficiency + luminosity): AN184

  //   for (int i=0; i<N; i++)
  //     {
  //       extra_erelat[i] = quadRelatError( pp_abs_yield_erelat,1.);//, // pp BBC,sigma normalization

  //       // draw PHENIX 200 GeV Ncoll errors linearly added to normal errors

  //       double x = 0.;
  //       double y = 0.;
  //       RAA_phenix->GetPoint(i,x,y);
  //       double ey = RAA_phenix->GetErrorY(i)+y*extra_erelat[i];
  //       double ex = RAA_phenix->GetErrorX(i);
  //       RAA_phenix->SetPointError(i,ex,ey);
  //     }

  //  // draw PHENIX 200 GeV Ncoll errors as bands

  //   bool useTAB = true;
  //   TH1F *h_eNcoll_RAA = (TH1F*)getPropagErrorNcollBand(RAA_phenix,Cent1,Cent2,0,0,extra_erelat,
  // 						      bin_scaling,useTAB,fVerbose);
  //   c0->cd();
  //   double exwidth = 0.35;
  //   gStyle->SetErrorX(exwidth);
  //   //h_eNcoll_RAA->Smooth(1);
  //   h_eNcoll_RAA->SetFillColor(1);
  //   h_eNcoll_RAA->SetFillStyle(3001);
  //   h_eNcoll_RAA->Draw("e2same");


  // // draw PHENIX 200 GeV Ncoll errors as boxes

  //   for (int i=0; i<Npoints; i++)
  //     {
  //       pT_corr_errors = new TBox(pT[i]-0.25,ratioAuAu_pp_Pi0[i]-eratioNcoll[i],
  // 				pT[i]+0.25,ratioAuAu_pp_Pi0[i]+eratioNcoll[i]);
  //       if (strcasecmp(type,"central_130") == 0) pT_corr_errors->SetFillColor(50);
  //       else pT_corr_errors->SetFillColor(kYellow);
  //       pT_corr_errors->Draw();
  //     }

  double normerr = 0.15;  
  TH1F *eNormBox = new TH1F("eNormBox","",2,0.2,0.8);
  for (int j=0;j<=2;j++){eNormBox->SetBinContent(j,1.);eNormBox->SetBinError(j,normerr);}
  eNormBox->SetFillColor(16);
  //eNormBox->SetFillStyle(3001);
  eNormBox->Draw("e3same");

  TLine *line = new TLine(ptmin, 1., ptmax, 1.);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw("same");

  // Npart line
  double NcollAuAu = getNcoll(Cent1, Cent2, "value");
  double Ncoll_pp = getNcoll("pp");
  double NpartAuAu = getNpart(Cent1, Cent2, "value");
  double Npart_pp = getNpart("pp");

  double Npart_scaling = (NpartAuAu*Ncoll_pp)/(NcollAuAu*Npart_pp);

  TLine *line2 = new TLine(ptmin, Npart_scaling, ptmax, Npart_scaling);
  line2->SetLineColor(1);
  line2->SetLineStyle(3);
  line2->SetLineWidth(3);
  line2->Draw("same");

  c0->cd();

  //setMarkerLineType(RAA_brahms,23,6,1.5);
  setMarkerLineType(RAA_brahms, 23, 8, 1.7, 1, 50);
  RAA_brahms->Draw("P");

  //   TGraphErrors *RAA_brahms2 = (TGraphErrors*)RAA_brahms->Clone();
  //   setMarkerLineType(RAA_brahms2,29,5,1.7);
  //   RAA_brahms2->Draw("P");
  //   TMarker *marker = new TMarker(4.6,0.5,29);
  //   marker->SetMarkerColor(5);
  //   marker->SetMarkerStyle(29);
  //   marker->SetMarkerSize(1.7);
  //   marker->Draw();

  //setMarkerLineType(RAA_phobos,21,229,1.5); // fixme: color 229 ('gold') not working
  setMarkerLineType(RAA_phobos, 21, 9, 1.5, 1, 9);
  RAA_phobos->Draw("P");

  //setMarkerLineType(RAA_star,29,4,2.0);
  setMarkerLineType(RAA_star, 29, 46, 2.3, 1, 46);
  RAA_star->Draw("P");
  TGraph *RAA_starb = (TGraph*)RAA_star->Clone();
  setMarkerLineType(RAA_starb, 29, 5, 1.3);
  RAA_starb->Draw("P");
  TMarker *marker2 = new TMarker(4.62359, 0.92, 23);
  marker2->SetMarkerColor(5);
  marker2->SetMarkerStyle(29);
  marker2->SetMarkerSize(1.9);
  marker2->Draw();

  //setMarkerLineType(RAA_phenix,20,2,1.5);
  setMarkerLineType(RAA_phenix, 20, 2, 1.7, 1, 2);
  RAA_phenix->Draw("P");

  c0->Update();

  return ;

}

//_____________________________________________________________________________
TGraphErrors*
emcAnalyzer::plot_RAA_pi0_62GeV(int Cent1, int Cent2)
{

  char title[100];
  sprintf(title, "RAA_pi0_%d_%d_AuAu_62GeV", Cent1, Cent2);

  TCanvas *c0 = (TCanvas*)canvas(title, 700, 600);
  c0->Range( -1.23529, -9.51842, 9.28235, 2.69298);
  c0->SetLeftMargin(0.11745);
  c0->SetRightMargin(0.0268456);
  c0->SetTopMargin(0.0567485);

  double max_RAA = 1.5;
  double min_RAA = 0.;
  double ptmin = 0.;
  double ptmax = 7.6;

  TH2F *myframe = (TH2F*)frame(title, 20, ptmin, ptmax, 20, min_RAA, max_RAA, "p_{T} (GeV/#font[72]{c})", "R_{AA}");
  myframe->Draw();
  c0->Update();

  TLegend *le = new TLegend(0.43, 0.70, 0.95, 0.956," Au+Au #rightarrow #pi^{0}+X @ #sqrt{s_{NN}} = 62.4 GeV", "brNDC");
  le->SetMargin(0.1);
  le->SetFillColor(kWhite);
  le->SetBorderSize(4);
  le->SetTextSize(0.04);

  fPlot = false;
  fFit = false;

  TGraphErrors *eband;

  // pi0 p+p reference:
  TGraphErrors *RAA_pi0_62GeV = (TGraphErrors*)plot_spectrum_from_ascii("R_AA_pi0_emcal_0_10_auau62GeV_may04.txt",eband);
  if ( !RAA_pi0_62GeV ) return 0;
  TClonesArray* eboxes_62GeV = (TClonesArray*)errorBoxes( eband, 0.);  // error boxes
  setMarkerLineType(RAA_pi0_62GeV, 20, 2, 2., 1, 1);
  le->AddEntry(RAA_pi0_62GeV, "PHENIX Preliminary (0-10%)  [p+p #pi^{0} ref.]", "P");


  // h+/- p+p reference:
  TGraphErrors *RAA_pi0_62GeV_href = (TGraphErrors*)plot_spectrum_from_ascii("R_AA_pi0_emcal_0_10_auau62GeV_href.txt",eband);
  TClonesArray* eboxes_62GeV_href = (TClonesArray*)errorBoxes( eband, 0.);  // error boxes
  setMarkerLineType(RAA_pi0_62GeV_href, 24, 4, 1.8, 1, 1);
  le->AddEntry(RAA_pi0_62GeV_href, "PHENIX Preliminary (0-10%)  [p+p h^{#pm}/1.6 ref.]", "P");


  // Overall normalization error
  double normerr = 0.10;  
  TH1F *eNormBox = new TH1F("eNormBox","",2,0.2,0.8);
  for (int j=0;j<=2;j++){eNormBox->SetBinContent(j,1.);eNormBox->SetBinError(j,normerr);}
  eNormBox->SetFillColor(17);
  //eNormBox->SetFillStyle(3001);
  c0->cd();
  eNormBox->Draw("e3same");

  TLine *line = new TLine(ptmin, 1., ptmax, 1.);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  c0->cd();
  line->Draw("same");

  TGraphErrors *vitev650 = (TGraphErrors*)plot_spectrum_from_ascii("R_AA_theor_vitev_sps_dndy650.txt",eband);
  TGraphErrors *vitev800 = (TGraphErrors*)plot_spectrum_from_ascii("R_AA_theor_vitev_sps_dndy800.txt",eband);
  TClonesArray* eb=(TClonesArray*)emcAnalyzerUtils::errorBand(vitev650,vitev800);
  TGraph *shadeband = (TGraph*)(*eb)[0]; 
  setMarkerLineType(vitev650, 21,90,2, 1,94,2);
  setMarkerLineType(vitev800, 21,90,2, 1,94,2);
  le->AddEntry(vitev650, "GLV energy loss (dN^{g}/dy = 650 - 800) [I.Vitev]", "P");

  c0->cd();
  vitev650->Draw("L");
  vitev800->Draw("L");
  shadeband->Draw("f");
  myframe->Draw("same");
  le->Draw();

  // box-errorband looks better in this case ...
  TBox *box = 0;
  for (int j = 0; j < eboxes_62GeV_href->GetEntries();j++)
    {
      box = ((TBox*)(*eboxes_62GeV_href)[j]);
      box->SetFillColor(7);
      box->SetFillStyle(3001);
      c0->cd();
      box->Draw();
    }
  for (int j = 0; j < eboxes_62GeV->GetEntries();j++)
    {
      box = ((TBox*)(*eboxes_62GeV)[j]);
      //int ci = TColor::GetColor("#e6d2ff");
      box->SetFillColor(2);
      box->SetFillStyle(3001);
      c0->cd();
      box->Draw();
    }

  RAA_pi0_62GeV_href->Draw("P");
  RAA_pi0_62GeV->Draw("P");

  c0->Update();

  return RAA_pi0_62GeV;

}

//_____________________________________________________________________________
// type: "mesons", "baryons"

void
emcAnalyzer::plot_Rcp_RHIC(const char *type)
{

  TString type_str = type;

  char title[100];
  sprintf(title, "Rcp_rhic_%s", type);

  TCanvas *c0 = (TCanvas*)canvas(title, 700, 600);
  c0->Range( -1.23529, -9.51842, 9.28235, 2.69298);
  c0->SetLeftMargin(0.11745);
  c0->SetRightMargin(0.0268456);
  c0->SetTopMargin(0.0567485);

  double max_Rcp = 1.5;
  double min_Rcp = 0.;
  double ptmin = 0.;
  double ptmax = 7.5;

  fPlot = false;
  fFit = false;

  TGraphErrors *Rcp_phenix = 0;
  TGraphErrors *Rcp_phenix_eta = 0;
  TGraphErrors *Rcp_star = 0;

  TBox *eNormphnx = 0;
  TBox *eNormstar = 0;

  TGraphErrors *errBand;

  char legphnx[100];
  char legphnx_eta[100];
  char legstar[100];
  sprintf(title, " Au+Au #rightarrow %s @ #sqrt{s_{NN}} = 200 GeV", type);
  //sprintf(title," Au+Au (central) #sqrt{s_{NN}} = 200 GeV",type);

  if (type_str.Contains("meson", TString::kIgnoreCase))
    {
      Rcp_phenix = (TGraphErrors*)plot_spectrum_from_ascii("Rcp_emcal_pi0_010_6092.txt", (TGraphErrors*&)errBand);
      Rcp_star = (TGraphErrors*)plot_spectrum_from_ascii("Rcp_k0short_star", (TGraphErrors*&)errBand);
      Rcp_phenix_eta = (TGraphErrors*)plot_spectrum_from_ascii("Rcp_AuAu_eta_020_6092.txt", (TGraphErrors*&)errBand);
      sprintf(legphnx, "PHENIX  #pi^{0} (0-10%%/60-92%%)  ");
      sprintf(legstar, "STAR K^{0}_{s} (0-5%%/60-80%%)  ");
      sprintf(legphnx_eta, "PHENIX  #eta (0-20%%/60-92%%) preliminary");

      max_Rcp = 2.0;
      eNormphnx = new TBox(ptmax-0.6, 0.7, ptmax-0.4, 1.3);
      eNormstar = new TBox(ptmax-0.3, 0.6, ptmax-0.1, 1.4);
    }
  else if (type_str.Contains("baryon", TString::kIgnoreCase))
    {
      Rcp_phenix = (TGraphErrors*)plot_spectrum_from_ascii("Rcp_phenix_ppbar_010_6092.txt", (TGraphErrors*&)errBand);
      Rcp_star = (TGraphErrors*)plot_spectrum_from_ascii("Rcp_lambda_star", (TGraphErrors*&)errBand);
      sprintf(legphnx, "PHENIX p,#bar{p} (0-10%%/60-92%%)  ");
      sprintf(legstar, "STAR #Lambda,#bar{#Lambda} (0-5%%/60-80%%)  ");

      max_Rcp = 2.0;
      eNormphnx = new TBox(ptmax-0.6, 0.7, ptmax-0.4, 1.3);
      eNormstar = new TBox(ptmax-0.3, 0.6, ptmax-0.1, 1.4);
    }

  if ( !Rcp_star && !Rcp_phenix ) return ;

  TH2F *myframe = (TH2F*)frame(title, 20, ptmin, ptmax, 20, min_Rcp, max_Rcp, "p_{T} (GeV/#font[72]{c})", "R_{cp}");
  c0->cd();
  myframe->Draw();
  c0->Update();

  TLegend *le = new TLegend(0.43, 0.70, 0.95, 0.956, title, "brNDC");
  le->SetMargin(0.1);
  le->SetTextSize(0.045);
  le->AddEntry(Rcp_phenix, legphnx, "P");
  le->AddEntry(Rcp_star, legstar, "P");
  if (Rcp_phenix_eta)
    le->AddEntry(Rcp_phenix_eta, legphnx_eta, "P");
  le->SetFillColor(kWhite);
  le->Draw();

  // Normalization errors

  // const int N = Rcp_phenix->GetN();

  TLine *line = new TLine(ptmin, 1., ptmax, 1.);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw("same");

  //   // Npart line
  //   double NcollAuAu = getNcoll(Cent1,Cent2,"value");
  //   double Ncoll_pp  = getNcoll("pp");
  //   double NpartAuAu = getNpart(Cent1,Cent2,"value");
  //   double Npart_pp  = getNpart("pp");

  //   double Npart_scaling = (NpartAuAu*Ncoll_pp)/(NcollAuAu*Npart_pp);

  //   TLine *line2 = new TLine(ptmin,Npart_scaling,ptmax,Npart_scaling);
  //   line2->SetLineColor(1);
  //   line2->SetLineStyle(3);
  //   line2->SetLineWidth(3);
  //   line2->Draw("same");

  c0->cd();

  setMarkerLineType(Rcp_phenix, 20, 2, 1.7, 1, 2);
  Rcp_phenix->Draw("P");
  if (Rcp_phenix_eta)
    {
      setMarkerLineType(Rcp_phenix_eta, 4, 1, 1.7, 1, 2);
      Rcp_phenix_eta->Draw("P");
    }

  eNormphnx->Draw();
  eNormphnx->SetFillColor(2);
  eNormphnx->SetFillStyle(3001);

  setMarkerLineType(Rcp_star, 21, 9, 1.5, 1, 9);
  Rcp_star->Draw("P");

  eNormstar->Draw();
  eNormstar->SetFillColor(9);
  eNormstar->SetFillStyle(3001);

  c0->Update();

  return ;

}

//_____________________________________________________________________________
// eta = 0,1,2,3 (default = 4 is all pseudorapidities)

void
emcAnalyzer::plot_Rcp_brahms(const double eta )
{

  char title[100];
  sprintf(title, "Rcp_brahms_%d", (int)eta);

  TCanvas *c0 = (TCanvas*)canvas(title, 700, 600);
  c0->Range( -0.897766, -0.248322, 5.67955, 1.60403);
  c0->SetLeftMargin(0.136494);
  c0->SetRightMargin(0.0272989);
  c0->SetTopMargin(0.0561594);
  c0->SetBottomMargin(0.134058);

  double max_Rcp = 1.8;
  double min_Rcp = 0.;
  double ptmin = 0.;
  double ptmax = 4.5;

  double pseudorap[] = {0., 1., 2.2, 3.2};

  fPlot = false;
  fFit = false;

  const int sets = 4;

  TGraphErrors *Rcp_brahms[sets];
  TGraphErrors *errBand;

  TH2F *myframe = (TH2F*)frame(title, 20, ptmin, ptmax, 20, min_Rcp, max_Rcp, "p_{T} (GeV/#font[72]{c})", "R_{cp}");
  c0->cd();
  myframe->Draw();

  sprintf(title, " BRAHMS d+Au #rightarrow h^{#pm}+X @ #sqrt{s_{NN}} = 200 GeV");

  char file[100];
  char legbrahms[100];

  TLegend *le = new TLegend(0.43, 0.70, 0.95, 0.956, title, "brNDC");
  le->SetMargin(0.1);
  le->SetTextSize(0.045);
  le->SetFillColor(kWhite);
  c0->cd();
  le->Draw();

  // Normalization errors

  TBox *eNorm = new TBox(ptmin, 0.9, ptmax, 1.1);
  eNorm->SetFillColor(2);
  eNorm->SetFillStyle(3001);
  c0->cd();
  eNorm->Draw();

  TLine *line = new TLine(ptmin, 1., ptmax, 1.);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  c0->cd();
  line->Draw("same");

  c0->Update();

  for (int i = 0;i < sets;i++)
    {
      sprintf(file, "brahms_rcp_eta%d_cent.dat", i);
      Rcp_brahms[i] = (TGraphErrors*)plot_spectrum_from_ascii(file, (TGraphErrors*&)errBand);
      if (Rcp_brahms[i] && i != 1 && i != 2 )
        {
          sprintf(legbrahms, "#eta = %3.2f", pseudorap[i]);
          setMarkerLineType(Rcp_brahms[i], 20, 1, 1.7, 1, 1);
          le->AddEntry(Rcp_brahms[i], legbrahms, "P");
          c0->cd();
          Rcp_brahms[i]->Draw("P");
        }
    }

  c0->Update();

  return ;

}

//_____________________________________________________________________________
// type: "1.8_north", "-1.7_south"

void
emcAnalyzer::plot_Rcp_dAu_muons(const char *type)
{

  TString type_str = type;

  char title[100];
  sprintf(title, "Rcp_dAu_muons_%s", type);

  TCanvas *c0 = (TCanvas*)canvas(title, 700, 600);
  c0->Range( -1.23529, -9.51842, 9.28235, 2.69298);
  c0->SetLeftMargin(0.11745);
  c0->SetRightMargin(0.0268456);
  c0->SetTopMargin(0.0567485);

  //double max_Rcp = 1.5;
  double max_Rcp = 2.0;
  double min_Rcp = 0.;
  double ptmin = 0.;
  double ptmax = 3.5;

  fPlot = false;
  fFit = false;

  TGraphErrors *Rcp_ming = 0;
  TGraphErrors *Rcp_chun = 0;

  TGraphErrors *errBand_chun = 0;
  TGraphErrors *errBand_ming = 0;

  TBox *eNormchun = 0;
  TBox *eNormming = 0;

  char legchun[100];
  char legming[100];

  sprintf(title, " d+Au #rightarrow h^{#pm} #rightarrow #mu (y=%s) @ #sqrt{s_{NN}} = 200 GeV", type);

  sprintf(legchun, "Rcp_dAu_0_20_y_%s_chun.txt", type);
  sprintf(legming, "Rcp_dAu_0_20_y_%s_ming.txt", type);

  Rcp_chun = (TGraphErrors*)plot_spectrum_from_ascii(legchun, (TGraphErrors*&)errBand_chun);

  // error band read from file
  TClonesArray* eb = (TClonesArray*)errorBand( errBand_chun );
  TGraph* RdAeBand_chun = (TGraph*)(*eb)[0];

  Rcp_ming = (TGraphErrors*)plot_spectrum_from_ascii(legming, (TGraphErrors*&)errBand_ming);
  eb = (TClonesArray*)errorBand( errBand_ming );
  TGraph* RdAeBand_ming = (TGraph*)(*eb)[0];

  sprintf(legchun, "hadrons from stopped #mu (0-20%%/60-88%%)");
  sprintf(legming, "hadrons from decay #mu (0-20%%/60-88%%)");

  double norm_error_chun = 0.12;
  double norm_error_ming = 0.11;

  eNormchun = new TBox(ptmax-0.6, 1.-norm_error_chun, ptmax-0.4, 1.+norm_error_chun);
  eNormming = new TBox(ptmax-0.3, 1.-norm_error_ming, ptmax-0.1, 1.+norm_error_ming);

  if ( !Rcp_ming && !Rcp_chun ) return ;

  TH2F *myframe = (TH2F*)frame(title, 20, ptmin, ptmax, 20, min_Rcp, max_Rcp, "p_{T} (GeV/#font[72]{c})", "R_{cp}");
  c0->cd();
  myframe->Draw();
  c0->Update();

  TLegend *le = new TLegend(0.43, 0.70, 0.95, 0.956, title, "brNDC");
  le->SetMargin(0.1);
  le->SetTextSize(0.045);
  le->AddEntry(Rcp_chun, legchun, "P");
  le->AddEntry(Rcp_ming, legming, "P");
  le->SetFillColor(kWhite);
  le->Draw();

  // Normalization errors

  //const int N = Rcp_chun->GetN();

  //double *extra_erelat = new double[N];

  TLine *line = new TLine(ptmin, 1., ptmax, 1.);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw("same");

  c0->cd();

  setMarkerLineType(Rcp_chun, 20, 2, 1.7, 1, 2);
  Rcp_chun->Draw("P");
  //RdAeBand_chun->Draw("f");
  RdAeBand_chun->Print();

  eNormchun->Draw();
  eNormchun->SetFillColor(2);
  eNormchun->SetFillStyle(3001);

  setMarkerLineType(Rcp_ming, 21, 9, 1.5, 1, 9);
  Rcp_ming->Draw("P");
  //RdAeBand_ming->Draw("f");
  RdAeBand_ming->Print();

  eNormming->Draw();
  eNormming->SetFillColor(9);
  eNormming->SetFillStyle(3001);

  c0->Update();

  return ;

}

//_____________________________________________________________________________
void
emcAnalyzer::plot_RAA_2_centralities( const int Cent1,
                                      const int Cent2,
                                      const int Cent3,
                                      const int Cent4,
                                      const char* cut,
                                      const bool readAscii,
                                      const bool bin_scaling,
				      const bool fitted_pp_denom )
{

  int CC = getCentralityClass(Cent1, Cent2)*getCentralityClass(Cent3, Cent4);
  if ( CC < 0) return ;

  fPlot = false;

  TGraphErrors *RAA_1 = (TGraphErrors*)plot_RAA(Cent1, Cent2, cut, readAscii, bin_scaling, fitted_pp_denom); // uses T_AB by default
  TGraphErrors *RAA_2 = (TGraphErrors*)plot_RAA(Cent3, Cent4, cut, readAscii, bin_scaling, fitted_pp_denom); // idem

  if (!RAA_1 || !RAA_2) return ;

  char title[100];
  char title1[100];
  char title2[100];
  char ytitle[100];

  sprintf(title1, "Central #pi^{0} (%d-%d%%)", Cent1, Cent2);
  sprintf(title2, "Peripheral #pi^{0} (%d-%d%%)", Cent3, Cent4);
  sprintf(title, "R_AA_for_%d_%d_and_%d_%d_%s", Cent1, Cent2, Cent3, Cent4, cut);

  //if (bin_scaling) sprintf(ytitle,"R_{AA} = Yield_{AuAu}/( #LT N_{coll}#GT Yield_{pp} )");
  //else sprintf(ytitle,"R_{AA} = Yield_{AuAu}/( #LT N_{part}#GT Yield_{pp} )");
  if (bin_scaling)
    sprintf(ytitle, "R_{AA}");
  //sprintf(ytitle,"R_{AA} = d#sigma^{AuAu}/dp_{T}/( #LT N_{coll}#GT d#sigma^{pp}/dp_{T} )");
  else
    sprintf(ytitle, "R_{AA} = d#sigma^{AuAu}/dp_{T} /( #LT N_{part}#GT Yield^{pp} )");

  TCanvas *c5 = (TCanvas*)canvas(title, 600, 600);
  c5->Range( -0.624577, -0.370968, 8.52173, 2.59677);
  c5->SetLeftMargin(0.119128);
  c5->SetRightMargin(0.00838926);
  c5->SetTopMargin(0.0326087);

  double max_RAA = 1.8;
  double min_RAA = 0.;
  double ptmin = 0.;
  double ptmax = 10.5;

  TH2F *myframe = (TH2F*)frame(title, 20, ptmin, ptmax, 20, min_RAA, max_RAA, "p_{T} (GeV/#font[72]{c})", ytitle);
  myframe->Draw();

  TLegend *legend = new TLegend(0.483221, 0.82971, 0.971477, 0.961957, NULL, "brNDC");
  //TLegend *legend = new TLegend(0.550336,0.717391,0.974832,0.952899,"Au+Au #rightarrow #pi^{0}X @ 200 GeV","brNDC");
  legend->SetMargin(0.1);
  legend->SetTextSize(0.042);
  legend->SetLineColor(1);
  legend->SetLineStyle(1);
  legend->SetLineWidth(1);
  legend->SetFillColor(0); //(5);
  legend->SetFillStyle(1001);
  legend->SetBorderSize(4);
  legend->Draw();
  legend->AddEntry(RAA_1, title1, "P");
  legend->AddEntry(RAA_2, title2, "P");
  //legend->AddEntry(RAA_1,"#pi^{0} Central","P");
  //legend->AddEntry(RAA_2,"#pi^{0} Peripheral","P");

  TLine *line = new TLine(ptmin, 1., ptmax, 1.);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw("same");

  double NcollAuAu = getNcoll(Cent1, Cent2, "value");
  double Ncoll_pp = getNcoll("pp"); // equal to 1
  double NpartAuAu = getNpart(Cent1, Cent2, "value");
  double Npart_pp = getNpart("pp"); // equal to 2

  double alternative_scaling = 0.;
  if (bin_scaling)
    {
      alternative_scaling = NpartAuAu*Ncoll_pp/(NcollAuAu*Npart_pp);
    }
  else
    {
      alternative_scaling = NcollAuAu*Npart_pp/(NpartAuAu*Ncoll_pp);
    }

  TLine *line2 = new TLine(ptmin, alternative_scaling, ptmax, alternative_scaling);
  line2->SetLineColor(1);
  line2->SetLineStyle(3);
  line2->SetLineWidth(3);
  line2->Draw("same");

  // Ncoll errors drawn separately

  const int N1 = RAA_1->GetN();

  double pp_abs_yield_erelat = 0.096; // Additional 9.6% error on pp yield (BBC efficiency + luminosity): AN184
  double FiduDeadWarnPlusAccRelatError = 0.05; // /Sqrt(2.) ;
  double ppAcceptanceError = 0.038;

  double *extra_erelat = new double[N1];

  // Central R_AA absolute normalization errors: Single Ncoll bar centered at 0.33

  int N = 2;
  TGraphErrors *RAApoints1 = new TGraphErrors(N);

  double x = 0.5;
  double ycenter_band = 0.33; // error dashed band centered at R_AA=0.33 for 0-10%

  //for (int i=0; i<N1; i++)
  for (int i = 0; i < N; i++)
    {
      extra_erelat[i] = quadRelatError( pp_abs_yield_erelat, 1.,  // pp BBC,sigma normalization
                                        ppAcceptanceError, 1.,    // pp acceptance
                                        FiduDeadWarnPlusAccRelatError, 1.); // AuAu acceptance
      RAApoints1->SetPoint(i, x+i/2., ycenter_band); // center of error band
      double ex = RAA_1->GetErrorX(i);
      RAApoints1->SetPointError(i, ex, 0.); // error to 0 now. Will be extra_erelat[] in quadrature with T_AB errors later
    }

  bool useTAB = true;
  TH1F* eNcoll_RAA1 = (TH1F*)getPropagErrorNcollBand( RAApoints1, Cent1, Cent2, 0, 0, extra_erelat,
						      bin_scaling, useTAB, fVerbose);
  //TH1F* eNcoll_RAA1 = (TH1F*)getPropagErrorNcollBand(RAA_1,Cent1,Cent2,0,0,extra_erelat,bin_scaling,useTAB,fVerbose);
  //eNcoll_RAA1->Smooth(3);
  eNcoll_RAA1->SetFillColor(16);
  eNcoll_RAA1->Draw("e3same");
  //eNcoll_RAA1->Draw("e4same");

  int N2 = RAA_2->GetN();
  extra_erelat = new double[N2];

  // Peripheral R_AA absolute normalization errors: Single Ncoll bar centered at R_AA = 1.
  N2 = 2;
  ycenter_band = 1; // error dashed band centered at R_AA=1 for 80-92%

  TGraphErrors *RAApoints2 = new TGraphErrors(N2);

  for (int i = 0; i < N2; i++)
    {
      extra_erelat[i] = quadRelatError( pp_abs_yield_erelat, 1.,  // pp BBC,sigma normalization
                                        ppAcceptanceError, 1.,    // pp acceptance
                                        FiduDeadWarnPlusAccRelatError, 1.); // AuAU acceptance
      //double x = RAA_2->GetX()[i];
      RAApoints2->SetPoint(i, x+i/2., ycenter_band); // center of error band
      double ex = RAA_2->GetErrorX(i);
      RAApoints2->SetPointError(i, ex, 0.);  // error to 0 now. Will be extra_erelat[] in quadrature with T_AB errors later
    }

  TH1F* eNcoll_RAA2 = (TH1F*)getPropagErrorNcollBand( RAApoints2, Cent3, Cent4, 0, 0,
						      extra_erelat, bin_scaling, useTAB, fVerbose);
  //TH1F* eNcoll_RAA2 = (TH1F*)getPropagErrorNcollBand(RAA_2,Cent3,Cent4,0,0,extra_erelat,bin_scaling,fVerbose);
  //eNcoll_RAA2->Smooth(15);
  eNcoll_RAA2->SetFillColor(17);
  eNcoll_RAA2->Draw("e3same");
  //eNcoll_RAA2->Draw("e4same");

  // Now the points ...

  setMarker(RAA_1, Cent1, Cent2);
  RAA_1->Draw("P");
  RAA_1->SetMarkerSize(1.9); // modified size

  setMarker(RAA_2, Cent3, Cent4);
  setMarkerLineType(RAA_2, 21, 4, 1.9); // modified style
  RAA_2->Draw("P");

  RAA_1->Draw("P"); // redraw it

  TLatex *tex = new TLatex(0.940731, 1.70146, "PHENIX preliminary");
  tex->SetTextSize(0.0362319);
  tex->SetLineWidth(2);
  //tex->Draw();

  line->Draw("same");
  line2->Draw("same");

  c5->Update();

  saveCanvas(c5);

  return ;

}

//_____________________________________________________________________________
//

TGraphErrors*
emcAnalyzer::plot_all_RAA(const char* cut, bool readAscii, 
			  const bool bin_scaling, 
			  const bool fitted_pp_denom)
{

  int Cent1 = 0;
  int Cent2 = 0;

  TGraphErrors *allRAA = 0;

  //_____________________________________________________________________________
  // R_AA loop on all centralities

  int CCreduced = 9;

  TGraphErrors* RAA_[CCreduced] ;

  for (int i = 0; i <= CCreduced; i++)
    {
      getCentralityClassLimits2( i*2, Cent1, Cent2 );
      RAA_[i] = (TGraphErrors*)plot_RAA(Cent1, Cent2, cut, readAscii, bin_scaling, fitted_pp_denom);
    }

  //_____________________________________________________________________________
  // Let's plot it

  double max_RAA = 1.5;
  double min_RAA = 0.;

  char title[300];
  sprintf(title, "all_RAA");

  TCanvas *c10 = (TCanvas*)canvas(title, 620, 600);
  c10->Range( -1.7, -19.4, 12.6, 1.40);
  c10->SetLeftMargin(0.12);
  c10->SetRightMargin(0.009);
  c10->SetTopMargin(0.01);

  TH2F *myframe = (TH2F*)frame(title, 20, 0., 100., 10, min_RAA, max_RAA,
                               "#LT N_{part}#GT", "R_{AA} Nuclear modification factor");
  //myframe->SetXTitle("Centrality Class (%)");
  //myframe->GetXaxis()->SetDrawOption("<");
  //myframe->GetXaxis()->LabelsOption(">");
  myframe->Draw();

  setMarkerLineType(allRAA, 20, 2, 1.4, 1, 1, 1);
  allRAA->Draw("P");

  //   // decreasing labels
  //   myframe->GetXaxis()->SetLabelOffset(6);
  //   double ypos = -0.0525773;
  //   if (logy) ypos -= 1;
  //   TLatex *tex = new TLatex(10.6742,ypos,"80-92  70-80  60-70  50-60  40-50  30-40  20-30  10-20   0-10");
  //   tex->SetTextSize(0.032);
  //   tex->SetLineWidth(2);
  //   tex->Draw();

  TLine *line = new TLine(0., 1.0, 100., 1.0);
  //if (logy) line = new TLine(0.,Log(1.0),100.,Log(1.0));
  line->SetLineColor(2);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw("same");

  // Separated Ncoll errors draw sys. errors as boxes

  //c1->RedrawAxis();

  c10->Update();

  return allRAA;
}


//_____________________________________________________________________________
// type: "charged", "identified", "id_scaled" (const. quark scaled)
//
void
emcAnalyzer::plot_v2_RHIC( const char* type )
{

  TString type_str = type ;

  char title[100];
  sprintf(title, "v2_minbias_%s_RHIC", type);

  TCanvas *c0 = (TCanvas*)canvas(title, 700, 600);
  c0->Range( -1.23529, -9.51842, 9.28235, 2.69298);
  c0->SetLeftMargin(0.11745);
  c0->SetRightMargin(0.0268456);
  c0->SetTopMargin(0.0567485);

  double min_v2 = 0.;
  double max_v2 = 0.40;
  double ptmin = 0.;
  double ptmax = 10.;

  char *xtitle = "p_{T} (GeV/#font[72]{c})";
  char *ytitle = "v_{2}";
  char *legend = "v_{2} Au+Au @ s_{NN}^{1/2} = 200 GeV";

  if ( type_str.Contains("scaled", TString::kIgnoreCase) )
    {
      max_v2 /= 2;
      ptmax /= 2;
      xtitle = "p_{T}/n (GeV/#font[72]{c})";
      ytitle = "v_{2}/n";
    }
  else if ( type_str.Contains("chg", TString::kIgnoreCase) )
    {
      ptmax = 7.;
      legend = "Minimum bias Au+Au @ #sqrt{s_{NN}} = 200 GeV";
    }

  TH2F *myframe = (TH2F*)frame(title, 20, ptmin, ptmax, 20, min_v2, max_v2, xtitle, ytitle);
  myframe->Draw();
  c0->Update();

  //=============================================================================

  fFit = false;

  TGraphErrors *errBand = 0;

  TGraphErrors *v2_star = 0;
  TGraphErrors *v2_phenix = 0;

  TGraphErrors *v2_phenix_pi0 = 0;
  TGraphErrors *v2_phenix_pi = 0;
  TGraphErrors *v2_phenix_ka = 0;
  TGraphErrors *v2_phenix_pr = 0;

  TGraphErrors *v2_star_k0short = 0;
  TGraphErrors *v2_star_lambda = 0;

  TGraphErrors *v2_pi_hydro = 0;


  if ( type_str.Contains("ch", TString::kIgnoreCase) )
    {
      v2_star = (TGraphErrors*)plot_spectrum_from_ascii("v2_star_chg_minbias.dat", errBand);
      v2_phenix = (TGraphErrors*)plot_spectrum_from_ascii("v2_phenix_chg_minbias.dat", errBand);

      setMarkerLineType(v2_star, 29, 9, 2.3, 1, 46);
      setMarkerLineType(v2_phenix, 20, 2, 1.7, 1, 2);
    }
  if ( type_str.Contains("id", TString::kIgnoreCase) )
    {
      TString v2_pi0 = "v2_phenix_pi0_minbias";
      v2_pi0 += ( type_str.Contains("scaled", TString::kIgnoreCase) ) ? "_scaled.dat" : ".dat";
      TString v2_pi = "v2_phenix_pi_minbias";
      v2_pi += ( type_str.Contains("scaled", TString::kIgnoreCase) ) ? "_scaled.dat" : ".dat";
      TString v2_ka = "v2_phenix_ka_minbias";
      v2_ka += ( type_str.Contains("scaled", TString::kIgnoreCase) ) ? "_scaled.dat" : ".dat";
      TString v2_pr = "v2_phenix_pr_minbias";
      v2_pr += ( type_str.Contains("scaled", TString::kIgnoreCase) ) ? "_scaled.dat" : ".dat";

      v2_phenix_pi0 = (TGraphErrors*)plot_spectrum_from_ascii((char*)v2_pi0.Data(), errBand);
      v2_phenix_pi = (TGraphErrors*)plot_spectrum_from_ascii((char*)v2_pi.Data(), errBand);
      v2_phenix_ka = (TGraphErrors*)plot_spectrum_from_ascii((char*)v2_ka.Data(), errBand);
      v2_phenix_pr = (TGraphErrors*)plot_spectrum_from_ascii((char*)v2_pr.Data(), errBand);

      TString v2_k0s = "v2_k0short_star";
      v2_k0s += ( type_str.Contains("scaled", TString::kIgnoreCase) ) ? "_scaled.dat" : ".dat";
      TString v2_lambda = "v2_lambda_star";
      v2_lambda += ( type_str.Contains("scaled", TString::kIgnoreCase) ) ? "_scaled.dat" : ".dat";

      v2_star_k0short = (TGraphErrors*)plot_spectrum_from_ascii((char*)v2_k0s.Data(), errBand);
      v2_star_lambda = (TGraphErrors*)plot_spectrum_from_ascii((char*)v2_lambda.Data(), errBand);

      int cc = 0;
      setMarker(v2_phenix_pi0, cc);
      cc += 2;
      setMarker(v2_phenix_pi, cc);
      cc += 2;
      setMarker(v2_phenix_ka, cc);
      cc += 2;
      setMarker(v2_phenix_pr, cc);
      cc += 2;
      setMarker(v2_star_k0short, cc);
      cc += 2;
      setMarker(v2_star_lambda, cc);
      cc += 2;
    }

  v2_pi_hydro = (TGraphErrors*)plot_spectrum_from_ascii("v2_hydro_pi.dat", errBand);
  setMarkerLineType(v2_pi_hydro, 1, 1, 0, 1, 2, 3);

  //TGraphAsymmErrors *v2_star = new
  // TGraphAsymmErrors(Nv2hstar,v2hstar[0],v2hstar[3],v2hstar[1],v2hstar[2],v2hstar[4],v2hstar[5]);
  //TGraphAsymmErrors *v2_phenix = new
  //TGraphAsymmErrors(Nv2hphenix,v2hphenix[0],v2hphenix[3],v2hphenix[1],v2hphenix[2],v2hphenix[4],v2hphenix[4]);

  TLegend *le = new TLegend(0.43, 0.70, 0.95, 0.956, legend, "brNDC");
  le->SetMargin(0.1);
  le->SetTextSize(0.04);
  le->SetFillColor(kWhite);

  if (v2_phenix)
    {
      le->AddEntry(v2_star, "STAR v_{2} (2-partic. corr.)", "P");
      le->AddEntry(v2_phenix, "PHENIX v_{2} (reaction plane)", "P");
    }
  if (v2_phenix_pi0)
    {
      le->AddEntry(v2_phenix_pi0, "PHENIX #pi^{0}", "P");
      le->AddEntry(v2_phenix_pi, "PHENIX #pi^{#pm}", "P");
      le->AddEntry(v2_phenix_ka, "PHENIX K^{#pm}", "P");
      le->AddEntry(v2_star_k0short, "STAR K_{s}^{0}", "P");
      le->AddEntry(v2_phenix_pr, "PHENIX p,#bar{p}", "P");
      le->AddEntry(v2_star_lambda, "STAR #Lambda,#bar{#Lambda}", "P");
    }
  c0->cd();
  le->Draw();

  TLine *line = new TLine(ptmin, 1., ptmax, 1.);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  //line->Draw("same");

  c0->cd();

  if (v2_phenix)
    {
      v2_star->Draw("P");
      v2_phenix->Draw("P");
    }
  if (v2_phenix_pi0)
    {
      v2_phenix_pi0->Draw("P");
      v2_phenix_pi->Draw("P");
      v2_phenix_ka->Draw("P");
      v2_phenix_pr->Draw("P");
      v2_star_k0short->Draw("P");
      v2_star_lambda->Draw("P");
      v2_phenix_pr->Draw("P");
    }

  //   TGraph *v2_starb = (TGraph*)v2_star->Clone();
  //   setMarkerLineType(v2_starb,29,5,1.3);
  //   v2_starb->Draw("P");
  //   TMarker *marker2 = new TMarker(4.62359,0.92,23);
  //   marker2->SetMarkerColor(5);
  //   marker2->SetMarkerStyle(29);
  //   marker2->SetMarkerSize(2.1);
  //   marker2->Draw();

  v2_pi_hydro->Draw("l");

  c0->Update();

  return ;

}

//_____________________________________________________________________________
//
TGraphErrors*
emcAnalyzer::plot_ratio_cuts( const int Cent1, const int Cent2,
                              const char* cut1,
                              const char* cut2,
                              const double scale1,
                              const bool plotComparisonToo )
{

  double ptmin = 0.5;
  double ptmax = 10.5;
  int ptbins = (int)(ptmax - ptmin)*2;

  char title[100];
  sprintf(title, "cut_comparison_%s_%s_%d_%d", cut1, cut2, Cent1, Cent2);

  TCanvas *c10 = new TCanvas(title, title, 600, 600);
  TH2F *myframe = (TH2F*)frame(title, ptbins, ptmin, ptmax, 100, 5E-08, 15);
  myframe->Draw();
  c10->SetLogy();
  c10->Update();

  int binshift = 1;
  fPlot = false;

  // cut 1
  fFit = false;
  TGraphErrors *gcut1 = (TGraphErrors*)plot_spectrum(Cent1, Cent2, cut1, "full", binshift);
  setMarkerLineType(gcut1, 20, 1, 1.2);
  c10->cd();
  scale(*gcut1, scale1, 0, fVerbose);
  gcut1->Draw("P");

  // cut 2
  TGraphErrors *gcut2 = (TGraphErrors*)plot_spectrum(Cent1, Cent2, cut2, "full", binshift);
  setMarkerLineType(gcut2, 20, 4, 1.2);
  c10->cd();
  gcut2->Draw("P");

  // ratio
  TLegend *legend = new TLegend(0.295302, 0.778986, 0.899329, 0.947464, "Ratio of cuts:", "brNDC");
  legend->SetMargin(0.1);
  legend->SetTextSize(0.042);
  legend->SetLineColor(1);
  legend->SetLineStyle(1);
  legend->SetLineWidth(1);
  legend->SetFillColor(19);
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);

  TGraphErrors *ratio_cut1_cut2 = 0;
  ratio_cut1_cut2 = (TGraphErrors*)ratio(gcut1, gcut2); //,0,fVerbose);
  if (!ratio_cut1_cut2) return ratio_cut1_cut2;

  char title2[50];
  sprintf(title2, "%s/%s (%d-%d%%)", cut1, cut2, Cent1, Cent2);
  legend->AddEntry(ratio_cut1_cut2, title2, "P");
  c10->Update();

  char title3[50];
  sprintf(title3, "%s_%s_%d_%d", cut1, cut2, Cent1, Cent2);
  TCanvas *c23 = (TCanvas*)canvas(title3, 600, 600);
  c23->Range( -0.40566, -0.225873, 8.59057, 2.04107);
  c23->SetRightMargin(0.0100671);
  c23->SetTopMargin(0.0181159);
  c23->cd();

  TH2F *myframe2 = (TH2F*)frame(title2, ptbins, ptmin, ptmax, 20, 0., 2.0, "p_{T} (GeV/#font[72]{c})", "Ratio of spectra (diff. cuts)");
  myframe2->SetTitle(title);
  myframe2->Draw();

  TLine *line = new TLine(ptmin, 1., ptmax, 1.);
  line->SetLineColor(2);
  line->SetLineStyle(2);
  line->Draw();

  setMarkerLineType(ratio_cut1_cut2, 29, 4, 1.7, 1, 1, 1);
  ratio_cut1_cut2->Draw("P");

  legend->Draw();

  c23->Update();

  if (!plotComparisonToo) c10->Close();

  return ratio_cut1_cut2;
}

//_____________________________________________________________________________
void
emcAnalyzer::plot_all_ratios_cuts( const int Cent1, const int Cent2,
                                   double ptmax,
                                   const char* refcut )
{


  double ptmin = 0.5;

  int ptbins = (int)(ptmax - ptmin)*2;

  char title0[50];
  sprintf(title0, "cuts_comparison_%d_%d", Cent1, Cent2);

  TCanvas *c10 = (TCanvas*)canvas(title0, 600, 600);
  c10->Range( -0.828125, -0.232848, 10.8125, 2.06237);

  char title[50];
  sprintf(title, "ratio #pi^{0} yields diff. cuts (%d-%d%%)", Cent1, Cent2);
  TH2F *myframe = (TH2F*)frame(title, ptbins, ptmin, ptmax, 20, 0., 2.);
  myframe->Draw();

  TLine *line = new TLine(ptmin, 1., ptmax, 1.);
  line->SetLineColor(2);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw();

  TLegend *legend = new TLegend(0.295302, 0.742754, 0.899329, 0.949275, title, "brNDC");
  legend->SetMargin(0.1);
  legend->SetTextSize(0.042);
  legend->SetLineColor(1);
  legend->SetLineStyle(1);
  legend->SetLineWidth(1);
  legend->SetFillColor(19);
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);

  TGraphErrors *one = 0;
  TGraphErrors *two = 0;
  TGraphErrors *three = 0;
  TGraphErrors *four = 0;
  TGraphErrors *five = 0;

  bool plotComparisonToo = false;

  one = (TGraphErrors*)plot_ratio_cuts(Cent1, Cent2, "noPID", refcut, plotComparisonToo);
  two = (TGraphErrors*)plot_ratio_cuts(Cent1, Cent2, "chisq1", refcut, plotComparisonToo);
  three = (TGraphErrors*)plot_ratio_cuts(Cent1, Cent2, "chisq2", refcut, plotComparisonToo);
  four = (TGraphErrors*)plot_ratio_cuts(Cent1, Cent2, "tof1", refcut, plotComparisonToo);
  five = (TGraphErrors*)plot_ratio_cuts(Cent1, Cent2, "tof1chisq2", refcut, plotComparisonToo);

  c10->cd();
  setMarkerLineType(one, 20, 4, 1.2, 20, 1);
  one->Draw("P");
  setMarkerLineType(two, 20, 2, 1.2, 22, 2);
  two->Draw("P");
  setMarkerLineType(three, 20, 1, 1.2, 1, 29);
  three->Draw("P");
  setMarkerLineType(four, 20, 3, 1.2, 3, 23);
  four->Draw("P");
  setMarkerLineType(five, 20, 6, 1.2, 6, 8);
  five->Draw("P");

  char title2[50];
  sprintf(title2, "%s/%s", "noPID", refcut);
  legend->AddEntry(one, title2, "P");
  sprintf(title2, "%s/%s", "chisq1", refcut);
  legend->AddEntry(two, title2, "P");
  sprintf(title2, "%s/%s", "chisq2", refcut);
  legend->AddEntry(three, title2, "P");
  sprintf(title2, "%s/%s", "tof1", refcut);
  legend->AddEntry(four, title2, "P");
  sprintf(title2, "%s/%s", "tof1chisq2", refcut);
  legend->AddEntry(five, title2, "P");

  legend->Draw();

  c10->Update();

}

//_____________________________________________________________________________
// DESCRIPTION: Reads the fully corrected data tables for N centrality classes
//              and combines them into a single data table.
//              Plots the resulting spectrum and/or returns the TGraph
//              N = (Cent2 - Cent1)/10
//              e.g. Cent1=0 (0-10%) and Cent2=20 (10-20%) will give: 0-20%
//
//              set = "pbsc", "pbgl","emcal"
//

TGraphErrors*
emcAnalyzer::combine_centralities( const int Cent1min,
                                   const int Cent2max,
                                   const char *cut,
                                   const char *set )
{

  cout << " Calculating combined " << set << " pi0 spectrum for cut " << cut 
       << " and centrality class: " << " (" << Cent1min << " - " << Cent2max << "%)" << endl;

  TGraphErrors *combSpectrum = 0;

  int offset = 10;
  int NumOfCCs = (Cent2max - Cent1min)/offset;
  if (NumOfCCs == 1)
    {
      cout << "Spectrum for centrality class " << Cent1min << " - " << Cent2max
	   << "%) already exists in principle. Let's try combining 5% tables ..." << endl;
      NumOfCCs = 2;
      offset = 5;
    }

  char title[300];
  sprintf(title, "Centrality Class %d_%d", Cent1min, Cent2max);
  char name0[300];
  sprintf(name0, "pi0_combined_%s_spectra_%d_%d_%s", set, Cent1min, Cent2max, cut);

  char ffile[300];
  char name[300];

  //

  double EventsCC[CentClasses];
  double eEventsCC[CentClasses];
  double accumEvents = 0;
  double accum_eEvents = 0;

  int NpTbinsmin = 100; // initialized arbitrarily large
  int CentClass = Cent1min;
  int badCCs = 0;

  double *pT = 0;
  double *epT = 0;
  double *yield[NumOfCCs];
  double *eYield[NumOfCCs];
  double *eStat[NumOfCCs];
  double *eSyst[NumOfCCs];
  double *eCCuncorr[NumOfCCs];
  double *epTcorr[NumOfCCs];

  int cols = 0;
  int NpTbins = 0;
  int skiplines = 4; // default for PbSc files

  // loop and read on each centrality class
  for (int i = 0; i < NumOfCCs; i++)
    {

      TString cut_str = cut ;
      TString set_str = set ;
      if ( set_str.Contains("pbsc") )
        {
          if ( cut_str.Contains("QM02") )
            skiplines = 1; // For QM02 tests ...
          if (CentClass == 80)
            sprintf(name, "pi0_pbsc_%d_%d_%s", CentClass, CentClass+12, cut);
          else
	    // fixme: we combine bin shift corrected yields, OK ?
            sprintf(name, "pi0_pbsc_%d_%d_%s", CentClass, CentClass+offset, cut); 
          sprintf(ffile, "%s/pbsc_tables/%s.txt", fDataTablesPath->Data(), name);
        }
      else if ( set_str.Contains("pbgl") )
        {
          if (CentClass == 80)
            {
              //sprintf(name,"pbgl_year2_FitP3Cor2_%d-%d",CentClass,CentClass+13); // bin shift UNcorrected
	      // fixme: we combine bin shift corrected yields, OK ?
              sprintf(name, "pbgl_year2_FitP3Cor4_%d-%d", CentClass, CentClass+13); 
            }
          else
            {
              //sprintf(name,"pbgl_year2_FitP3Cor2_%d-%d",CentClass,CentClass+offset); // bin shift UNcorrected
              sprintf(name, "pbgl_year2_FitP3Cor4_%d-%d", CentClass, CentClass+offset);
            }
          sprintf(ffile, "%s/pbgl_tables/%s.dat", fDataTablesPath->Data(), name);
          skiplines = 1;
        }
      else if ( set_str.Contains("final") ) // FINAL Run-2
        {
	  // fixme: we combine bin shift corrected yields, OK ?
          if (CentClass == 80)
            sprintf(name, "phenix_pi0_final_emcal_Run2_AuAu_%d_%d", CentClass, CentClass+12);
          else
            sprintf(name, "phenix_pi0_final_emcal_Run2_AuAu_%d_%d", CentClass, CentClass+offset); 
          sprintf(ffile, "%s/pi0_emcal_tables/%s.txt", fDataTablesPath->Data(), name);
	  skiplines = 0;
       }
      else  // set = "emcal"
        {
	  // fixme: we combine bin shift corrected yields, OK ?
          if (CentClass == 80)
            sprintf(name, "pi0_emcal_%d_%d_%s", CentClass, CentClass+12, cut);
          else
            sprintf(name, "pi0_emcal_%d_%d_%s", CentClass, CentClass+offset, cut); 
          sprintf(ffile, "%s/pi0_emcal_tables/%s.txt", fDataTablesPath->Data(), name);
          skiplines = 2;
        }

      // let's read one by one each file ...
      double **vals = 0;
      vals = readAscii(ffile, cols, NpTbins, skiplines, fVerbose);
      if (!vals || !NpTbins)
        {
          if (!NpTbins)
            cout << " <E> Empty ascii. Not using " << ffile
		 << " in the combined spectrum ! " << endl;
          badCCs++;
          CentClass += offset;
        }

      if (!vals || !NpTbins)
        continue;

      pT = new double[NpTbins];
      pT = vals[0];
      epT = new double[NpTbins];

      int j = i - badCCs;

      yield[j] = new double[NpTbins];
      eYield[j] = new double[NpTbins];
      eStat[j] = new double[NpTbins];
      eSyst[j] = new double[NpTbins];
      eCCuncorr[j] = new double[NpTbins];
      epTcorr[j] = new double[NpTbins];


      if ( set_str.Contains("pbsc") )
        {
	  epT = vals[1];
	  yield[j] = vals[2];
	  eYield[j] = vals[3];
          eStat[j] = vals[5];
          eSyst[j] = vals[6];
          eCCuncorr[j] = vals[7];
          if ( !cut_str.Contains("QM02") )
            epTcorr[j] = vals[8];
          else
            epTcorr[j] = vals[7];
        }
      else if ( set_str.Contains("final") )
        {
	  yield[j] = vals[1];
	  eYield[j] = vals[2];
          eStat[j] = vals[2];
          eSyst[j] = vals[4];
          for (int i = 0;i < NpTbins;i++)
            {
	      //cout << "debug0: " << yield[j][i] << endl;
              epT[i] = 0.;
              eCCuncorr[j][i] = 0.;
              epTcorr[j][i] = 0.;
            }
        }
      else if ( set_str.Contains("emcal") )
        {
	  epT = vals[1];
	  yield[j] = vals[2];
	  eYield[j] = vals[3];
          eStat[j] = vals[4];
          eSyst[j] = vals[5];
          eCCuncorr[j] = vals[6];
	  epTcorr[j] = vals[7];

//           for (int i = 0;i < NpTbins;i++)
//             {
//               eStat[j][i] = 0.;
//               eSyst[j][i] = 0.;
//               eCCuncorr[j][i] = 0.;
//               epTcorr[j][i] = 0.;
//             }
        }
      else // pbgl
        {
	  epT = vals[1];
	  yield[j] = vals[2];
	  eYield[j] = vals[3];
          eStat[j] = vals[4];
          eSyst[j] = vals[5]; // this is actually eCCcorr[j] relat. (syst)
          eCCuncorr[j] = vals[6];
          epTcorr[j] = vals[7];

          // err_Ntot(PbGl) = sqrt(eStat_relat^2+eCCncorr_relat^2+eCCncorr_relat^2)*Ntot(PbGl)
          for (int i = 0;i < NpTbins;i++)
            {
              eStat[j][i] *= yield[j][i]; // stat is relat. error in PbGl tables
              eSyst[j][i] = quadRelatError(eCCuncorr[j][i], 1., eSyst[j][i], 1.) * yield[j][i];
              eCCuncorr[j][i] *= yield[j][i]; // CCuncorr is relat. error in PbGl tables
              epTcorr[j][i] *= yield[j][i]; // epTcorr is relat. error in PbGl tables
            }
        }

      // store mininum value of max. # of pT bins per CentClass
      NpTbinsmin = (NpTbinsmin < NpTbins) ? NpTbinsmin : NpTbins;

      EventsCC[j] = getEvents(CentClass, CentClass+offset, "value");
      eEventsCC[j] = getEvents(CentClass, CentClass+offset, "error");
      accumEvents += EventsCC[j];
      accum_eEvents += eEventsCC[j]*eEventsCC[j];

      if (fVerbose>1) cout << endl << "Centrality class: " << CentClass << " -- " << CentClass+offset 
			   << " will be scaled by its # of evts.: " << EventsCC[j] << " over total # combined: " 
			   << accumEvents << endl;

      CentClass += offset;
    }

  if (!pT) return combSpectrum;

  accum_eEvents = Sqrt(accum_eEvents);

  NpTbins = NpTbinsmin;

  double combYield[NpTbins];
  double ecombYield[NpTbins];
  double ecombStat[NpTbins];
  double ecombSyst[NpTbins];
  double ecombCCuncorr[NpTbins];
  double ecombpTcorr[NpTbins];

  //cout << "  pT   pT_err   Ntot   Ntot_err   (Rel_toterr %) ";
  //cout << "  Stat_err   Syst_err   CCuncorr_toterr   pTcorr_toterr" << endl;
  cout << "  pT   Stat_err  %Stat_err   Syst_err   %Syst_err" << endl;

  for (int pTbin = 0; pTbin < NpTbins ; pTbin++)
    {
      combYield[pTbin] = 0.;
      ecombYield[pTbin] = 0.;
      ecombStat[pTbin] = 0.;
      ecombSyst[pTbin] = 0.;
      ecombCCuncorr[pTbin] = 0.;
      ecombpTcorr[pTbin] = 0.;

      // Simple average (weighted by # of evts.): 1/Nevt*[Nevt_1*Npi0_1+ ...+ Nevt_n*Npi0_n]
      for (int j = 0; j < (NumOfCCs-badCCs) ; j++)
        {
          combYield[pTbin] += yield[j][pTbin]*EventsCC[j];
	  //cout << "debug: " << combYield[pTbin] << " " << yield[j][pTbin] << " " << EventsCC[j] << endl;
          // errors are basically 1/Nevt*[Nevt_1*errNpi0_1+ ...+ errNevt_n*Npi0_n] + other minor stat errors in Nevt's
          ecombYield[pTbin] += quadRelatError( EventsCC[j]*eYield[j][pTbin], 1.,
                                               yield[j][pTbin]*eEventsCC[j], 1.); // pretty small

          ecombStat[pTbin] += quadRelatError( EventsCC[j]*eStat[j][pTbin], 1.,
                                              yield[j][pTbin]*eEventsCC[j], 1.);

          ecombSyst[pTbin] += quadRelatError( EventsCC[j]*eSyst[j][pTbin], 1.,
                                              yield[j][pTbin]*eEventsCC[j], 1.);

          ecombCCuncorr[pTbin] += quadRelatError( EventsCC[j]*eCCuncorr[j][pTbin], 1.,
                                                  yield[j][pTbin]*eEventsCC[j], 1.);

          ecombpTcorr[pTbin] += quadRelatError( EventsCC[j]*epTcorr[j][pTbin], 1.,
                                                yield[j][pTbin]*eEventsCC[j], 1.);
        }

      combYield[pTbin] /= accumEvents;
      ecombYield[pTbin] = quadRelatError(ecombYield[pTbin], 1.,
                                         combYield[pTbin]*accum_eEvents, 1.); // pretty small
      ecombYield[pTbin] /= accumEvents;
      float erelat = ecombYield[pTbin]/combYield[pTbin]*100.;

      ecombStat[pTbin] /= accumEvents;
      ecombSyst[pTbin] /= accumEvents;
      ecombCCuncorr[pTbin] /= accumEvents;
      ecombpTcorr[pTbin] /= accumEvents;

//       printf("  %.2f    %.2f    %.3e    %.3e    %.2f    %.3e    %.3e    %.3e    %.3e\n",
//              pT[pTbin], epT[pTbin], combYield[pTbin], ecombYield[pTbin], erelat,
//              ecombStat[pTbin], ecombSyst[pTbin], ecombCCuncorr[pTbin], ecombpTcorr[pTbin]);
      printf("  %.2f    %.3e    %.3e    %.2f    %.3e    %.2f \n",
             pT[pTbin], combYield[pTbin],  ecombStat[pTbin], ecombStat[pTbin]/combYield[pTbin]*100.,
	     ecombSyst[pTbin], ecombSyst[pTbin]/combYield[pTbin]*100.);

    }

  //_____________________________________________________________________________

  TCanvas *c4 = new TCanvas(name0, name0, 600, 600);

  combSpectrum = new TGraphErrors( NpTbins, pT, combYield, epT, ecombYield);
  double max_yield = 10.*combYield[0];
  double min_yield = 0.1*combYield[NpTbins-1];

  combSpectrum->SetMaximum(max_yield);
  combSpectrum->SetMinimum(min_yield);
  setMarkerLineType(combSpectrum, 20, 2);
  combSpectrum->SetTitle(title);
  combSpectrum->GetYaxis()->SetTitleOffset(1.2);
  combSpectrum->GetYaxis()->SetTitle("1/[2#pi p_{T}] d^{2}N^{#pi^{0}}/dp_{ T}dy (GeV/#font[72]{c})^{-2}");
  combSpectrum->GetXaxis()->SetTitle("#pi^{0} p_{T} (GeV/#font[72]{c})");
  combSpectrum->Draw("AP");
  c4->SetLogy();
  c4->Update();

  //_____________________________________________________________________________

  double ptmin = 2.0;
  double ptmax = pT[NpTbins-1]; // <-- modify this if no fit

  TF1 *hagedorn = (TF1*)hagedornConstrainedFit(combSpectrum, "constrhagedorn", ptmin, ptmax, fVerbose);
  hagedorn->Draw("same");
  c4->Update();

  if ( !fFit )
    {
      combSpectrum->GetListOfFunctions()->Delete();
      //combSpectrum->GetListOfFunctions()->Remove(hagedorn);
      gStyle->SetOptFit(0);
      //delete hagedorn;
      c4->Update();
    }
  if ( !fPlot )
    {
      c4->Close();
    }
  else
    {
      saveCanvas(c4);
    }


  if (fdumpLaTeX)
    {
      dumpLaTeX(combSpectrum, ecombStat, ecombSyst, ecombCCuncorr, ecombpTcorr);
    }

  if (fdumpAscii)
    {
      dumpAscii(combSpectrum, ecombStat, ecombSyst, ecombCCuncorr, ecombpTcorr);
    }

  return combSpectrum;

}

//_____________________________________________________________________________
//
TGraphErrors*
emcAnalyzer::average_spectra( const char* file0,
                              const char* file1,
                              const char* Xtitle,
                              const char* Ytitle,
                              const char* title,
                              const int binshift,
                              const char* average_type )
{

  TGraphErrors *avSpectrum = 0;

  if ( !file0 || !file1) return avSpectrum;

  TString file0_str = file0;
  TString file1_str = file1;

  int NpTbins0 = 0;
  int NpTbins1 = 0;

  double *pT[2];
  double *epT[2];
  double *yield[2];
  double *eYield[2];
  double *eStat[2];
  double *eSyst[2];
  double *eCCuncorr[2];
  double *epTcorr[2];
  double *epTuncorr[2];
  double *euncorr[2]; // point-by-point error: the one that we are going to use for the average weight

  //double FiduDeadWarnPlusAccRelatError = 0.05;///Sqrt(2.) ; // let's take just the acc. error as the only one correlated

  double scalefactor = 1.; // for pp cross-section <--> yield

  //_____________________________________________________________________________
  // let's read each one of the 2 files ...

  for (int j = 0; j < 2; j++)
    {
      int cols = 0;
      int NpTbins = 0;
      int skiplines = 4; // default for pbsc files
      double **vals = 0;

      if (j == 0) // file0
        {
          if (file0_str.Contains("fullemcal", TString::kExact))
            skiplines = 2;
          if (file0_str.Contains("pbgl_year2", TString::kExact))
            skiplines = 1;
          else if (file0_str.Contains("pp", TString::kExact))
            skiplines = 0;
          vals = readAscii(file0, cols, NpTbins, skiplines, fVerbose);
          NpTbins0 = NpTbins;

          if (file0_str.Contains("cross", TString::kExact))
            scalefactor = 1./sigma_NN;
          else
            scalefactor = 1.;
        }
      else // file1
        {
          if (file1_str.Contains("fullemcal", TString::kExact))
            skiplines = 2;
          if (file1_str.Contains("pbgl_year2", TString::kExact))
            skiplines = 1;
          else if (file1_str.Contains("pp", TString::kExact))
            skiplines = 0;
          vals = readAscii(file1, cols, NpTbins, skiplines, fVerbose);
          NpTbins1 = NpTbins;

          if (file1_str.Contains("cross", TString::kExact))
            scalefactor = 1./sigma_NN;
          else
            scalefactor = 1.;
        }

      if (!vals || !NpTbins)
        {
          if (!NpTbins) cout << " <E> Empty ascii ! " << endl;
          return avSpectrum;
        }

      pT[j] = new double[NpTbins];
      epT[j] = new double[NpTbins];
      yield[j] = new double[NpTbins];
      eYield[j] = new double[NpTbins];
      eStat[j] = new double[NpTbins];
      eSyst[j] = new double[NpTbins];
      eCCuncorr[j] = new double[NpTbins];
      epTcorr[j] = new double[NpTbins];
      epTuncorr[j] = new double[NpTbins];
      euncorr[j] = new double[NpTbins];

      pT[j] = vals[0];

      if (skiplines == 2) // AuAu EMCal lvl2
        {
          epT[j] = vals[1];
          yield[j] = vals[2];
          eYield[j] = vals[3];
          eStat[j] = vals[4];
          eSyst[j] = vals[5];
          eCCuncorr[j] = vals[6];
          epTcorr[j] = vals[7];

          for (int i = 0;i < NpTbins;i++)
            {
              double eyrelat = eYield[j][i] / yield[j][i];
              double epTcorr_relat = epTcorr[j][i] / yield[j][i];
              epTuncorr[j][i] = eyrelat*eyrelat - epTcorr_relat*epTcorr_relat;

              // final decision: no correlated error ...
              euncorr[j][i] = eyrelat*eyrelat ; //- FiduDeadWarnPlusAccRelatError*FiduDeadWarnPlusAccRelatError;
              euncorr[j][i] = (euncorr[j][i] > 0) ? Sqrt(euncorr[j][i]) : 0. ;
              euncorr[j][i] *= yield[j][i];
              //cout << " " << euncorr[j][i] << " " ;
            }
        }
      if (skiplines == 4) // AuAu PbSc
        {
          epT[j] = vals[1];
          yield[j] = vals[2];
          eYield[j] = vals[3];
          eStat[j] = vals[5];
          eSyst[j] = vals[6];
          eCCuncorr[j] = vals[7];
          epTcorr[j] = vals[8];

          for (int i = 0;i < NpTbins;i++)
            {
              double eyrelat = eYield[j][i] / yield[j][i];
              double epTcorr_relat = epTcorr[j][i] / yield[j][i];
              epTuncorr[j][i] = eyrelat*eyrelat - epTcorr_relat*epTcorr_relat;

              // final decision: no correlated error ...
              euncorr[j][i] = eyrelat*eyrelat ; //- FiduDeadWarnPlusAccRelatError*FiduDeadWarnPlusAccRelatError;
              euncorr[j][i] = (euncorr[j][i] > 0) ? Sqrt(euncorr[j][i]) : 0. ;
              euncorr[j][i] *= yield[j][i];
              //cout << " " << euncorr[j][i] << " " ;
            }
        }
      else if (skiplines == 1) // AuAu PbGl
        {
          epT[j] = vals[1];
          yield[j] = vals[2];
          eYield[j] = vals[3];
          eStat[j] = vals[4];
          eSyst[j] = vals[5]; // this is actually eCCcorr[j] relat. (syst.)
          eCCuncorr[j] = vals[6]; // this is actually eCCuncorr[j] relat. (syst.)
          epTcorr[j] = vals[7];

          // Note: err_Ntot(PbGl) = sqrt(eStat_relat^2 + eCCncorr_relat^2 + eCCcorr_relat^2)*Ntot(PbGl)
          for (int i = 0;i < NpTbins;i++)
            {
              eSyst[j][i] = quadRelatError(eCCuncorr[j][i], 1., eSyst[j][i], 1.)*yield[j][i];
              // CCuncorr is relat. error in PbGl tables and does not include the statistical error
              eCCuncorr[j][i] = quadRelatError(eCCuncorr[j][i], 1., eStat[j][i], 1.)*yield[j][i];
              eStat[j][i] *= yield[j][i]; // stat is relat. error in PbGl tables
              epTcorr[j][i] *= yield[j][i]; // epTcorr is relat. error in PbGl tables

              double eyrelat = eYield[j][i] / yield[j][i];
              double epTcorr_relat = epTcorr[j][i] / yield[j][i];
              epTuncorr[j][i] = eyrelat*eyrelat - epTcorr_relat*epTcorr_relat;

              // final decision: no correlated error ...
              euncorr[j][i] = eyrelat*eyrelat; // - FiduDeadWarnPlusAccRelatError*FiduDeadWarnPlusAccRelatError;
              euncorr[j][i] = (euncorr[j][i] > 0) ? Sqrt(euncorr[j][i]) : 0. ;
              euncorr[j][i] *= yield[j][i];
              //cout << " " << euncorr[j][i] << " " ;
            }
        }
      else if (skiplines == 0) // pp
        {
          yield[j] = vals[1];
          eStat[j] = vals[2];
          eSyst[j] = vals[3];

          cout << " <W> Normalizing by 1/pT the pp spectrum ..." << endl;
          for (int i = 0;i < NpTbins;i++)
            {
              if (i == 0 && scalefactor != 1)
                cout << " <W> Normalizing by 1/42.(mb) the pp spectrum ..." << endl;
              // 1/pT invariant yield normalization lacking in final pbsc pbgl pp ascii files
              epT[j][i] = 0.05;
              yield[j][i] *= scalefactor / pT[j][i];
              eStat[j][i] *= scalefactor / pT[j][i];
              eSyst[j][i] *= scalefactor / pT[j][i];

              eYield[j][i] = quadRelatError(eStat[j][i], yield[j][i], eSyst[j][i], yield[j][i])*yield[j][i];
              eCCuncorr[j][i] = 0.;

              // FIXME: let's assume here that the pTcorr relat error is the same for pp as for AuAu

              double eyrelat = eYield[j][i] / yield[j][i];
              double epTcorr_relat = getpTCorrelatRelError(pT[j][i])*getpTCorrelatRelError(pT[j][i]);
              epTcorr[j][i] = epTcorr_relat;
              epTuncorr[j][i] = eyrelat*eyrelat - epTcorr_relat*epTcorr_relat;

              // final decision: no correlated error ...
              euncorr[j][i] = eyrelat*eyrelat; // - FiduDeadWarnPlusAccRelatError*FiduDeadWarnPlusAccRelatError;
              euncorr[j][i] = (euncorr[j][i] > 0) ? Sqrt(euncorr[j][i]) : 0.;
              euncorr[j][i] *= yield[j][i];
            }
        }

    }

  if (!pT) return avSpectrum;

  //_____________________________________________________________________________
  // Let's combine them

  //const int NpTbins = Max(NpTbins0,NpTbins1);
  const int NpTbins = NpTbins0+NpTbins1; // assuming completely non-coincident binning
  int unusedNpTbins = 0;

  double *avpT = new double[NpTbins];
  double *eavpT = new double[NpTbins];
  double *avYield = new double[NpTbins];
  double *eavYield = new double[NpTbins];

  double eavStat[NpTbins];
  double eavSyst[NpTbins];
  double eavCCuncorr[NpTbins];
  double eavpTcorr[NpTbins];

  double pTbinwidth = 0.25; // half 0.5 GeV/c

  double chi2 = 0;
  double chi2_uncorr = 0;
  double chi2_stat = 0;
  double Ndf = 2*NpTbins - NpTbins;

  if (fVerbose)
    {
      cout << "  pT   pT_err   Ntot   Ntot_err   (Rel_toterr %) ";
      cout << "  Stat_err   Syst_err   CCuncorr_toterr   pTcorr_toterr" //<< endl;
	   << "  Chi2_tot(pT)   Chi2_uncorr(pT)   Chi2_stat(pT)" << endl;
    }

  int i0 = 0;
  int i1 = 0;

  for (int i = 0; i < NpTbins ; i++)
    {
      avYield[i] = 0.;
      eavYield[i] = 0.;

      eavStat[i] = 0.;
      eavSyst[i] = 0.;
      eavCCuncorr[i] = 0.;
      eavpTcorr[i] = 0.;

      double chi2_pT = 0;
      double chi2_pT_uncorr = 0;
      double chi2_pT_stat = 0;

      bool notEnd0 = (i0 < NpTbins0);
      bool notEnd1 = (i1 < NpTbins1);

      bool fillAlone0 = false;
      bool fillAlone1 = false;

      // Not end of file 0 nor 1: check coincident binning
      if ( notEnd0 && notEnd1 )
        {
          if ( (pT[0][i0] - pT[1][i1]) <= -pTbinwidth )  // pT bin file0 lower than pT bin file1 (within pT bin width)
            {
              if (fVerbose)
                {
                  cout << " <W> pT0 = " << pT[0][i0] << " < pT1 = " << pT[1][i1] << endl;
                }
              fillAlone0 = true;
            }
          else if ( (pT[0][i0] - pT[1][i1]) >= pTbinwidth ) //  pT bin file0 larger than pT bin file1 (within pT bin width)
            {
              if (fVerbose)
                {
                  cout << " <W> pT(file0) = " << pT[0][i0] << " > pT(file1) = " << pT[1][i1] << endl;
                }
              fillAlone1 = true;
            }
        }
      else if ( !notEnd1 && !notEnd0 ) // end of both files
        {
          unusedNpTbins++;
          Ndf--;
        }

      //_____________________________________________________________________________
      // fill "average" values with the single values of file1

      if ( fillAlone1 ||               // if non-coindent binning or
           ( !notEnd0 && notEnd1 ) )  // empty file0 and non-empty file1
        {
          avpT[i] = pT[1][i1];
          eavpT[i] = epT[1][i1];
          avYield[i] = yield[1][i1];
          eavYield[i] = eYield[1][i1];

          eavStat[i] = eStat[1][i1];
          eavSyst[i] = eSyst[1][i1];
          eavCCuncorr[i] = eCCuncorr[1][i1];
          eavpTcorr[i] = epTcorr[1][i1];

          if (fVerbose)
            {
              cout << " <I> Filling pT = " << avpT[i] << " GeV/c with values of "
		   << file1 << " alone ..." << endl;
            }

          Ndf--;
        }

      // fill "average" values with the single values of file0

      else if ( fillAlone0 ||              // non-coindent binning or
                ( !notEnd1 && notEnd0 ) ) // empty file1 and non-empty file0
        {
          avpT[i] = pT[0][i0];
          eavpT[i] = epT[0][i0];
          avYield[i] = yield[0][i0];
          eavYield[i] = eYield[0][i0];

          eavStat[i] = eStat[0][i0];
          eavSyst[i] = eSyst[0][i0];
          eavCCuncorr[i] = eCCuncorr[0][i0];
          eavpTcorr[i] = epTcorr[0][i0];

          if (fVerbose)
            {
              cout << " <I> Filling pT = " << avpT[i] << " GeV/c with values of "
		   << file0 << " alone ..." << endl;
            }

          Ndf--;
        }

      // Average of yields of file0 and file1 weighted by their respective errors (PDG "unconstrained average")

      else if (!unusedNpTbins) // coincident binning and not end of both files
        {

          avpT[i] = (i0 <= i1) ? pT[0][i0] : pT[1][i1]; // whichever is the first of file0,1 pT bins
          eavpT[i] = (i0 <= i1) ? epT[0][i0] : epT[1][i1]; // idem

          //double weight0 = 1./(eYield[0][i0]*eYield[0][i0]);
          //double weight1 = 1./(eYield[1][i1]*eYield[1][i1]);
          //avYield[i] = yield[0][i0]*weight0;
          //avYield[i] += yield[1][i1]*weight1;
          //eavYield[i] = weight0 + weight1;

          // using total errors for the weight
          //if (eYield[0][i0]) eavYield[i]  = 1./(eYield[0][i0]*eYield[0][i0]);
          //if (eYield[1][i1]) eavYield[i] += 1./(eYield[1][i1]*eYield[1][i1]);
          //if (eYield[0][i0]) avYield[i]  = yield[0][i0]/(eYield[0][i0]*eYield[0][i0]);
          //if (eYield[1][i1]) avYield[i] += yield[1][i1]/(eYield[1][i1]*eYield[1][i1]);
          //if (eavYield[i]) avYield[i] /= eavYield[i];

          // using only uncorrelated error (tot. error - acc. error) for the weight

          //if (euncorr[0][i0]) eavYield[i]  = 1./(euncorr[0][i0]*euncorr[0][i0]);
          //if (euncorr[1][i1]) eavYield[i] += 1./(euncorr[1][i1]*euncorr[1][i1]);
          //if (euncorr[0][i0]) avYield[i]  = yield[0][i0]/(euncorr[0][i0]*euncorr[0][i0]);
          //if (euncorr[1][i1]) avYield[i] += yield[1][i1]/(euncorr[1][i1]*euncorr[1][i1]);
          //eavStat[i]  = (eStat[0][i0]*eStat[0][i0])/Power(eYield[0][i0]*eYield[0][i0],2.);
          //eavStat[i] += (eStat[1][i1]*eStat[1][i1])/Power(eYield[1][i1]*eYield[1][i1],2.);
          //if (eYield[0][i0]) eavStat[i]  = 0.5*(eStat[0][i0]*eStat[0][i0])/(eYield[0][i0]*eYield[0][i0]);
          //if (eYield[1][i1]) eavStat[i] += 0.5*(eStat[1][i1]*eStat[1][i1])/(eYield[1][i1]*eYield[1][i1]);

          double weight0 = 0.;
          double weight1 = 0.;
          double weightsum = 0.;

          if (strcasecmp(average_type, "PDG") == 0)  // "Particle Data Group"-like average
            {
              if (euncorr[0][i0])
                weight0 = 1. / (euncorr[0][i0]*euncorr[0][i0]);
              if (euncorr[1][i1])
                weight1 = 1. / (euncorr[1][i1]*euncorr[1][i1]);
              weightsum = weight0+weight1;

              avYield[i] = yield[0][i0]*weight0;
              avYield[i] += yield[1][i1]*weight1;

              // using total individual errors for the average error
              if (eYield[0][i0])
                eavYield[i] = 1. / (eYield[0][i0]*eYield[0][i0]);
              if (eYield[1][i1])
                eavYield[i] += 1. / (eYield[1][i1]*eYield[1][i1]);

              eavStat[i] = 0.5*(eStat[0][i0]*eStat[0][i0])*weight0;
              eavStat[i] += 0.5*(eStat[1][i1]*eStat[1][i1])*weight1;

              //eavSyst[i] = eavYield[i]*eavYield[i]-eavStat[i]*eavStat[i];
              eavSyst[i] = 0.5*(eSyst[0][i0]*eSyst[0][i0])*weight0;
              eavSyst[i] += 0.5*(eSyst[1][i1]*eSyst[1][i1])*weight1;

              eavCCuncorr[i] = 0.5*(eCCuncorr[0][i0]*eCCuncorr[0][i0])*weight0;
              eavCCuncorr[i] += 0.5*(eCCuncorr[1][i1]*eCCuncorr[1][i1])*weight1;

              eavpTcorr[i] = 0.5*(epTcorr[0][i0]*epTcorr[0][i0])*weight0;
              eavpTcorr[i] += 0.5*(epTcorr[1][i1]*epTcorr[1][i1])*weight1;

              // Final ...

              if (weightsum)
                avYield[i] /= weightsum;
              if (eavYield[i])
                eavYield[i] = 1. / Sqrt(eavYield[i]);

              if (weightsum)
                eavStat[i] /= weightsum;
              eavStat[i] = Sqrt(eavStat[i]);

              if (weightsum)
                eavSyst[i] /= weightsum;
              eavSyst[i] = Sqrt(eavSyst[i]);

              if (weightsum)
                eavCCuncorr[i] /= weightsum;
              eavCCuncorr[i] = Sqrt(eavCCuncorr[i]);

              if (weightsum)
                eavpTcorr[i] /= weightsum;
              eavpTcorr[i] = Sqrt(eavpTcorr[i]);
            }
          else // PPG03-like average
            {
              if (eStat[0][i0])
                eavStat[i] = 1. / (eStat[0][i0]*eStat[0][i0]);
              if (eStat[1][i1])
                eavStat[i] += 1. / (eStat[1][i1]*eStat[1][i1]);
              eavStat[i] = Sqrt(1. / eavStat[i]);

              if (eSyst[0][i0])
                eavSyst[i] = 1. / (eSyst[0][i0]*eSyst[0][i0]);
              if (eSyst[1][i1])
                eavSyst[i] += 1. / (eSyst[1][i1]*eSyst[1][i1]);
              eavSyst[i] = Sqrt(1. / eavSyst[i]);

              if (euncorr[0][i0])
                weight0 = 1. / (euncorr[0][i0]*euncorr[0][i0]);
              if (euncorr[1][i1])
                weight1 = 1. / (euncorr[1][i1]*euncorr[1][i1]);
              weightsum = weight0+weight1;

              avYield[i] = yield[0][i0]*weight0;
              avYield[i] += yield[1][i1]*weight1;
              if (weightsum)
                avYield[i] /= weightsum;

              // using total individual errors for the average error
              eavYield[i] = Sqrt(eavStat[i]*eavStat[i] + eavSyst[i]*eavSyst[i]);

              // 	      eavCCuncorr[i]  = 0.5*(eCCuncorr[0][i0]*eCCuncorr[0][i0])*weight0;
              // 	      eavCCuncorr[i] += 0.5*(eCCuncorr[1][i1]*eCCuncorr[1][i1])*weight1;

              // 	      eavpTcorr[i]  = 0.5*(epTcorr[0][i0]*epTcorr[0][i0])*weight0;
              // 	      eavpTcorr[i] += 0.5*(epTcorr[1][i1]*epTcorr[1][i1])*weight1;

            }

          // Chisquare check of the average error

          chi2_pT = 0.;
          if (eYield[0][i0])
            chi2_pT = Power( (avYield[i] - yield[0][i0]) / eYield[0][i0] , 2.);
          if (eYield[1][i1])
            chi2_pT += Power( (avYield[i] - yield[1][i1]) / eYield[1][i1] , 2.);
          chi2 += chi2_pT;

          chi2_pT_uncorr = 0.;
          if (euncorr[0][i0])
            chi2_pT_uncorr = Power( (avYield[i] - yield[0][i0]) / euncorr[0][i0] , 2.);
          if (euncorr[1][i1])
            chi2_pT_uncorr += Power( (avYield[i] - yield[1][i1]) / euncorr[1][i1] , 2.);
          chi2_uncorr += chi2_pT_uncorr;

          chi2_pT_stat = 0.;
          if (eStat[0][i0])
            chi2_pT_stat = Power( (avYield[i] - yield[0][i0]) / eStat[0][i0] , 2.);
          if (eStat[1][i1])
            chi2_pT_stat += Power( (avYield[i] - yield[1][i1]) / eStat[1][i1] , 2.);
          chi2_stat += chi2_pT_stat;

        } // end of "unconstrained average"

      // dump result
      if (!unusedNpTbins)
        {

          float erelat = eavYield[i]/avYield[i]*100.;

          if (fVerbose)
            {
              printf("  %.2f    %.2f    (%.3e,%.3e)%.3e     %.3e    %.2f    %.3e    %.3e    %.3e    %.3e    %.3f    %.3f    %.3f\n",
                     avpT[i], eavpT[i], yield[0][i0], yield[1][i1], avYield[i], eavYield[i], erelat,
                     eavStat[i], eavSyst[i], eavCCuncorr[i], eavpTcorr[i],
                     chi2_pT, chi2_pT_uncorr, chi2_pT_stat);
            }
          // 	  else
          // 	    {
          // 	      printf("  %.2f    %.2f    %.3e    %.3e    %.2f    %.3e    %.3e    %.3e    %.3e    %.3f    %.3f    %.3f\n",
          // 		     avpT[i],eavpT[i],avYield[i],eavYield[i],erelat,
          // 		     eavStat[i],eavSyst[i],eavCCuncorr[i],eavpTcorr[i],
          // 		     chi2_pT,chi2_pT_uncorr,chi2_pT_stat);
          // 	    }


          // 	  printf("  %.2f    %.2f    %.3e    %.3e    %.2f    %.3e(%.2f)    %.3e    %.3e    %.3e    %.3f    %.3f\n",
          // 		 avpT[i],eavpT[i],avYield[i],eavYield[i],erelat,
          // 		 eavStat[i],eavStat[i]*eavStat[i]/(eavYield[i]*eavYield[i])*100.,
          // 		 eavSyst[i],eavCCuncorr[i],eavpTcorr[i],
          // 		 chi2_pT,chi2_pT_stat);

          i0++;
          i1++;
          if (fillAlone1)
            i0--;
          if (fillAlone0)
            i1--;
        }

    } // loop on averaged pT bins

  if (fVerbose)
    {
      cout << endl << endl;

      cout << " <I> Global Chi2/Ndf of the average spectrum (tot): " << chi2 << "/" << Ndf
	   << " = " << chi2/Ndf << endl;
      cout << " <I> Global Chi2/Ndf of the average spectrum (uncorr): " << chi2_uncorr << "/" << Ndf
	   << " = " << chi2_uncorr/Ndf << endl;
      cout << " <I> Global Chi2/Ndf of the average spectrum (stat): " << chi2_stat << "/" << Ndf
	   << " = " << chi2_stat/Ndf << endl;
    }

  //_____________________________________________________________________________
  // final combined spectrum

  const int NpTbinsfinal = NpTbins - unusedNpTbins;

  TGraphErrors* avSpectrum_tmp = new TGraphErrors( NpTbins, avpT, avYield, eavpT, eavYield);

  double *avpTfinal = new double[NpTbinsfinal];
  double *eavpTfinal = new double[NpTbinsfinal];
  double *avYieldfinal = new double[NpTbinsfinal];
  double *eavYieldfinal = new double[NpTbinsfinal];
  //avSpectrum_tmp->Print();
  //avSpectrum = (TGraphErrors*)chopPointsFromGraph(avSpectrum_tmp,unusedNpTbins); // fixme: can't we use RemovePoint() ?
  //avSpectrum->Print();

  //_____________________________________________________________________________
  // bin shift

  bool undoInvYieldNormFirst = false; // default

  double ratio_shifted_over_unshifted_yield = 0.;

  if (binshift)
    {
      double ptmin = avSpectrum_tmp->GetX()[0]; // 1.0;
      double ptmax = avSpectrum_tmp->GetX()[NpTbinsfinal-1];

      cout << "<I> Bin shift correction: Fitting with a power-law in " << ptmin << " -- " << ptmax << " GeV/c" << endl;
      TF1 *pow = new TF1("pow", "[0]/x^[1]", ptmin, ptmax);
      pow->SetParameters(1, 10);
      avSpectrum_tmp->Fit("pow", "R", "", ptmin, ptmax);

      //TF1* hag = (TF1*)hagedornFit(avSpectrum_tmp,"hagedorn", ptmin,ptmax,fVerbose);

      if (binshift == 1) // shift along the y-axis (yield)
        {
          cout << " <W> Shifting spectrum along the y-axis (Yield) ... " << endl;

          shiftYieldPowLaw(avSpectrum_tmp, pow, pTbinwidth*2., undoInvYieldNormFirst);
          //shiftYield(avSpectrum_tmp,hag,pTbinwidth*2.,undoInvYieldNormFirst);

        }
      else if (binshift == 2) // shift along the x-axis (pT)
        {
          undoInvYieldNormFirst = true;
          cout << " <W> Shifting spectrum (after doing and undoing the 1/pT normalization) along the x-axis (pT) ... " << endl;
          shiftYieldPowLaw(avSpectrum_tmp, pow, pTbinwidth*2., undoInvYieldNormFirst);
          //shiftPt(avSpectrum_tmp,hag,pTbinwidth*2.,undoInvYieldNormFirst);
        }

      if (fVerbose)
	{
	  cout << endl << "  <I> Final spectrum shifted along the y-axis (Yield): " << endl;
	}
    }

  // dump result
  for ( int i = 0; i < NpTbinsfinal; i++ )
    {
      double x = avSpectrum_tmp->GetX()[i];
      double ex = avSpectrum_tmp->GetErrorX(i);
      double y = avSpectrum_tmp->GetY()[i];
      double ey = avSpectrum_tmp->GetErrorY(i);
      
      ratio_shifted_over_unshifted_yield = y/avYield[i];
      
      // for pT bin shift
      avpTfinal[i] = x;
      eavpTfinal[i] = ex;
      
      // for y-bin shift
      avYieldfinal[i] = y;
      eavYieldfinal[i] = ey;
      
      eavStat[i] *= ratio_shifted_over_unshifted_yield;
      eavSyst[i] *= ratio_shifted_over_unshifted_yield;
      
      eavCCuncorr[i] *= ratio_shifted_over_unshifted_yield;
      eavpTcorr[i] *= ratio_shifted_over_unshifted_yield;
      
      // ascii dump
      
      if (fVerbose)
	{
	  printf("  %.2f  %.2f    %.3e    %.3e    %.2f    %.3e    %.3e    %.3e    %.3e\n",
		 avpTfinal[i], eavpTfinal[i], avYieldfinal[i], eavYieldfinal[i],
		 eavYieldfinal[i]/avYieldfinal[i]*100.,
		 eavStat[i], eavSyst[i], eavCCuncorr[i], eavpTcorr[i]);
	}
    }
  
  // Final averaged spectrum after bin-shift

  avSpectrum = new TGraphErrors( NpTbinsfinal, avpTfinal, avYieldfinal, eavpTfinal, eavYieldfinal);

  //_____________________________________________________________________________
  // plot

  TCanvas *c4 = (TCanvas*)canvas(title, 600, 600);
  c4->Range( -1.46514, -8.1804, 10.8338, 2.35997);

  double max_yield = 10.*avYield[0];
  double min_yield = 0.1*avYield[NpTbinsfinal-1];

  avSpectrum->SetMaximum(max_yield);
  avSpectrum->SetMinimum(min_yield);
  setMarkerLineType(avSpectrum, 20, 2, 1.4);
  avSpectrum->SetTitle(title);
  avSpectrum->GetYaxis()->SetTitleOffset(1.2);
  avSpectrum->GetYaxis()->SetTitle("1/[2#pi p_{T}] d^{2}N^{#pi^{0}}/dp_{ T}dy (GeV/#font[72]{c})^{-2}");
  avSpectrum->GetXaxis()->SetTitle("#pi^{0} p_{T} (GeV/#font[72]{c})");
  avSpectrum->Draw("AP");
  c4->SetLogy();
  c4->Update();

  //_____________________________________________________________________________
  // fit

  double ptmin = 2.0;
  double ptmax = avpT[NpTbinsfinal-1]; // <-- modify this if no fit

  TF1 *hagedorn = (TF1*)hagedornConstrainedFit(avSpectrum, "constrhagedorn", ptmin, ptmax, fVerbose);
  hagedorn->Draw("same");
  c4->Update();

  if ( !fFit )
    {
      avSpectrum->GetListOfFunctions()->Delete();
      //avSpectrum->GetListOfFunctions()->Remove(hagedorn);
      gStyle->SetOptFit(0);
      //hagedorn->Delete();
      c4->Update();
    }
  if ( !fPlot )
    {
      c4->Close();
    }
  else
    {
      saveCanvas(c4);
    }

  if (fdumpLaTeX)
    {
      dumpLaTeX(avSpectrum, eavStat, eavSyst, eavCCuncorr, eavpTcorr);
    }

  if (fdumpAscii)
    {
      dumpAscii(avSpectrum, eavStat, eavSyst, eavCCuncorr, eavpTcorr);
    }


  saveCanvas(c4);

  return avSpectrum;

}

//_____________________________________________________________________________
//
// e.setVerbose(0);
// e.setDumpLaTeX();
// for (int i=0;i<100;i+=10){ int j=i+10; e.average_pbsc_pbgl(i,j); }; > latexout.txt
// e.average_pbsc_pbgl(60,80); >> latexout.txt
// e.average_pbsc_pbgl(70,92); >> latexout.txt
// e.average_pbsc_pbgl(80,92); >> latexout.txt
// e.average_pbsc_pbgl(0,100); >> latexout.txt

// for (int i=0;i<100;i+=10){ int j=i+10; e.plot_RAA(i,j); }; > latexoutRAA.txt
// e.plot_RAA(60,80); >> latexoutRAA.txt
// e.plot_RAA(70,92); >> latexoutRAA.txt
// e.plot_RAA(80,92); >> latexoutRAA.txt
// e.plot_RAA(0,100); >> latexoutRAA.txt

// Info: This function averages whatever PbSc & PbGl (Cent1 and Cent2) files existing on disk.
//       (no check on "validity" of files).

TGraphErrors*
emcAnalyzer::average_pbsc_pbgl( const int Cent1,
                                const int Cent2,
                                const char* cut,
                                const char* average_type)
{

  TString cut_str = cut;

  char title[200] ;
  char ytitle[] = "1/[2#pi p_{T}] d^{2}N/dp_{ T}dy (GeV/#font[72]{c})^{-2}";
  char xtitle[] = "p_{T} (GeV/#font[72]{c})";
  char pbsc_file[300];
  char pbgl_file[300];

  if ( !cut_str.Contains("pp") ) // AuAu
    {
      sprintf(title, "pi0_spectra_averaged_pbsc_pbgl_%d_%d_%s", Cent1, Cent2, cut); // final

      //sprintf(pbgl_file,"%s/pi0_ms_pbgl_%d-%d.dat",fDataTablesPath->Data(),Cent1,Cent2); // QM02
      sprintf(pbsc_file, "%s/pbsc_tables/pi0_pbsc_%d_%d_%s_noshift.txt", fDataTablesPath->Data(), Cent1, Cent2, cut); // bin shift UNcorrected
      sprintf(pbgl_file, "%s/pbgl_tables/pbgl_year2_FitP3Cor2_%d-%d.dat", fDataTablesPath->Data(), Cent1, Cent2); // bin shift UNcorrected
    }
  else // pp
    {
      sprintf(title, "pi0_spectra_averaged_pbsc_pbgl_pp");

      //sprintf(pbsc_file,"%s/pp_tables/pi0_pp_pbsc_200GeV_cross.txt",fDataTablesPath->Data());
      //sprintf(pbgl_file,"%s/pp_tables/pi0_pp_pbgl_shifty_fixbin_jan_14_2003.txt",fDataTablesPath->Data());
      //sprintf(pbgl_file,"%s/pp_tables/pi0_pp_pbgl_200GeV_yields.txt",fDataTablesPath->Data());

      // we normalize cross <---> yields later below
      sprintf(pbsc_file, "%s/pp_tables/pi0_pp_pbsc_200GeV_final_cross.txt", fDataTablesPath->Data());
      sprintf(pbgl_file, "%s/pp_tables/pi0_pp_pbgl_200GeV_final_yields.txt", fDataTablesPath->Data());
    }

  fPlot = true;
  return average_spectra(pbsc_file, pbgl_file, xtitle, ytitle, title, 1, average_type);

}

//_____________________________________________________________________________
//
// suppress_vs_what = "Npart", "ET", "CC", "EBj", "b" ...

void
emcAnalyzer::suppress_2scalings_vs_centrality( const char* cut,
					       double ptmin, double ptmax,
					       const bool UseIntHagFit )
{

  char title[300];
  char xtitle[300];
  char ytitle[300];

  sprintf(title, "suppr_2_above_%.1fGeVc2_vs_Npart_%s", ptmin, cut);
  sprintf(xtitle, "#LT N_{part}#GT");
  sprintf(ytitle, "R_{AA}(p_{T} > %.1f GeV/#font[72]{c})", ptmin);

  double max_RAA = 2.7;
  double min_RAA = 0.;
  int xmax = 375; // Npart max

  TCanvas *c10 = (TCanvas*)canvas(title, 620, 600);
  c10->Range( -53.3708, -0.370851, 379.213, 2.77853);

  TH2F *myframe = (TH2F*)frame(title, 20, 0., xmax, 10, min_RAA, max_RAA, xtitle, ytitle);
  //myframe->GetYaxis()->CenterTitle();
  myframe->Draw();

  bool bin_scaling = true;
  TGraphErrors *Suppr_vs_CentNcoll = suppress_vs_centrality("Npart", cut, ptmin, ptmax, bin_scaling, UseIntHagFit, c10);
  bin_scaling = false;
  TGraphErrors *Suppr_vs_CentNpart = suppress_vs_centrality("Npart", cut, ptmin, ptmax, bin_scaling, UseIntHagFit, c10);

  // Separated Ncoll errors

  //   Suppr_vs_CentNcoll->Print();
  //   Suppr_vs_CentNpart->Print();

  //   TGraphErrors* g_eNcoll = (TGraphErrors*)Suppr_vs_CentNcoll->GetListOfFunctions()->FindObject("g_eNcoll");
  //   TGraphErrors* g_eNpart = (TGraphErrors*)Suppr_vs_CentNpart->GetListOfFunctions()->FindObject("g_eNcoll");

  c10->cd();
  Suppr_vs_CentNcoll->Draw("P");
  c10->cd();
  Suppr_vs_CentNpart->Draw("P");
  setMarkerLineType(Suppr_vs_CentNpart, 28, 1, 2.6, 1, 1, 1);

  TLine *line = new TLine(0., 1., xmax, 1.);
  //if (logy) line = new TLine(0.,Log(1.0),100.,Log(1.0));
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw("same");

  TLine *line2 = new TLine(0., 2., xmax, 2.);
  //if (logy) line = new TLine(0.,Log(1.0),100.,Log(1.0));
  line2->SetLineColor(1);
  line2->SetLineStyle(3);
  line2->SetLineWidth(3);
  //line2->Draw("same");

  c10->RedrawAxis();
  line->Draw("same");
  //line2->Draw("same");
  Suppr_vs_CentNcoll->Draw("P");
  Suppr_vs_CentNpart->Draw("P");

  //  legends
  TLegend *leg = new TLegend(0.5, 0.357, 0.932, 0.4, NULL, "brNDC");
  leg->SetMargin(0.1);
  leg->SetTextSize(0.035);
  leg->SetLineColor(1);
  leg->SetFillColor(0);
  leg->SetLineStyle(1);
  leg->SetLineWidth(1);
  leg->SetBorderSize(0);

  char label1[50];
  //sprintf(label1,"R_{AA}: Eq. (1)");
  sprintf(label1, "R_{AA} = #frac{dN_{AA}^{inv. #pi^{0}}/dy}{#LT N_{coll}^{AuAu}#GT #times dN_{pp}^{inv. #pi^{0}}/dy}");
  //leg->AddEntry(Suppr_vs_CentNcoll, label1, "P");
  leg->AddEntry(Suppr_vs_CentNcoll, "", "P");
  leg->Draw();

  TLatex* l = new TLatex(200.8, 0.767, label1);
  l->SetTextSize(0.04);
  l->Draw("same");

  TLegend *leg2 = new TLegend(0.495, 0.889, 0.529, 0.938, NULL, "brNDC");
  leg2->SetMargin(0.1);
  leg2->SetTextSize(0.055);
  leg2->SetLineColor(1);
  leg2->SetFillColor(0);
  leg2->SetLineStyle(1);
  leg2->SetLineWidth(1);
  leg2->SetBorderSize(0);

  char label2[50];
  //leg2->AddEntry(Suppr_vs_CentNpart, label2, "P");
  leg2->AddEntry(Suppr_vs_CentNpart, "", "P");

  //sprintf(label2,"R_{AA}^{part}=Yield(Au+Au)/Yield(p+p) (2/N_{ part})");
  //sprintf(label2,"R_{AA}^{part} = #frac{#LT N_{part}^{pp}#GT#times (1/N_{AA}^{ evt} ) d^{2} N_{AA}^{#pi^{0}}/dp_{T}dy}{#LT N_{part}^{AuAu}#GT ");
  //sprintf(label2,"%s #times (1/N_{pp}^{ evt} )d^{2} N_{pp}^{#pi^{0}}/dp_{T}dy}",label2);
  //TLatex* l2 = new TLatex(115.16,2.42,label2);

  sprintf(label2, "R_{AA}^{part} = #frac{#LT N_{part}^{pp}#GT#times dN_{AA}^{inv. #pi^{0}}/dy}{#LT N_{part}^{AuAu}#GT ");
  sprintf(label2, "%s #times dN_{pp}^{inv. #pi^{0}}/dy}", label2);

  TLatex* l2 = new TLatex(178.36, 2.42, label2);
  l2->SetTextSize(0.04);
  l2->Draw("same");

  leg2->Draw();

  c10->Update();

}

//_____________________________________________________________________________
//
// suppress_3reacplane_vs_centrality

void
emcAnalyzer::suppress_3reacplane_vs_centrality( const char* cut,
						double ptmin, double ptmax,
						const bool bin_scaling,
						const bool UseIntHagFit )
{

  char title[300];
  char xtitle[300];
  char ytitle[300];

  sprintf(title, "suppr_3reacplane_above_%.1fGeVc2_vs_Npart_%s", ptmin, cut);
  sprintf(xtitle, "#LT N_{part}#GT");
  sprintf(ytitle, "R_{AA}(p_{T} > %.1f GeV/#font[72]{c})", ptmin);

  double max_RAA = 1.3;
  double min_RAA = 0.;
  int xmax = 375; // Npart max

  TCanvas *c10 = (TCanvas*)canvas(title, 620, 600);
  c10->Range( -53.3708, -0.370851, 379.213, 2.77853);

  TH2F *myframe = (TH2F*)frame(title, 20, 0., xmax, 10, min_RAA, max_RAA, xtitle, ytitle);
  //myframe->GetYaxis()->CenterTitle();
  myframe->Draw();

  TGraphErrors *Suppr_vs_CentNcoll = suppress_vs_centrality("Npart", cut, ptmin, ptmax, bin_scaling, UseIntHagFit, c10, 1, "");
  TGraphErrors *Suppr_vs_CentNcoll_1 = suppress_vs_centrality("Npart", "r012", ptmin, ptmax, bin_scaling, UseIntHagFit, c10, 0, "");
  TGraphErrors *Suppr_vs_CentNcoll_2 = suppress_vs_centrality("Npart", "r345", ptmin, ptmax, bin_scaling, UseIntHagFit, c10, 0, "");
  TGraphErrors *Suppr_vs_CentNcoll_3 = suppress_vs_centrality("Npart", "r678", ptmin, ptmax, bin_scaling, UseIntHagFit, c10, 0, "");

  // Separated Ncoll errors

  c10->cd();
  setMarkerLineType(Suppr_vs_CentNcoll, 8, 9, 2.0, 1, 9, 1);
  setMarkerLineType(Suppr_vs_CentNcoll_1, 29, 7, 2.6, 1, 7, 1);
  setMarkerLineType(Suppr_vs_CentNcoll_2, 23, 5, 2.3, 1, 5, 1);
  setMarkerLineType(Suppr_vs_CentNcoll_3, 21, 2, 2.1, 1, 2, 1);

  TLine *line = new TLine(0., 1., xmax, 1.);
  //if (logy) line = new TLine(0.,Log(1.0),100.,Log(1.0));
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw("same");

  c10->RedrawAxis();

  Suppr_vs_CentNcoll->Draw("P");
  Suppr_vs_CentNcoll_1->Draw("P");
  //Suppr_vs_CentNcoll_2->Draw("P");
  Suppr_vs_CentNcoll_3->Draw("P");

  //  legends

  char label[50];
  //sprintf(label,"R_{AA}(p_{T} > %.1f GeV/c):",ptmin);
  sprintf(label, "Suppression vs. centrality:");

  TLegend *leg2 = new TLegend(0.35, 0.73, 0.91, 0.98, label, "brNDC");
  //TLegend *leg2 = new TLegend(0.4,0.73,0.96,0.97,"","brNDC");
  leg2->SetMargin(0.1);
  leg2->SetTextSize(0.055);
  leg2->SetLineColor(1);
  leg2->SetFillColor(0);
  leg2->SetLineStyle(1);
  leg2->SetLineWidth(1);
  leg2->SetBorderSize(0);

  //char label2[50];
  //leg2->AddEntry(Suppr_vs_CentNpart, label2, "P");

  leg2->AddEntry(Suppr_vs_CentNcoll_1, "#pi^{0} in-plane", "P");
  leg2->AddEntry(Suppr_vs_CentNcoll, "Inclusive #pi^{0}", "P");
  //leg2->AddEntry(Suppr_vs_CentNcoll_2,"#pi^{0} in-between", "P");
  leg2->AddEntry(Suppr_vs_CentNcoll_3, "#pi^{0} out-of-plane", "P");

  leg2->Draw();

  c10->Update();

}

//_____________________________________________________________________________
//
// suppress_vs_what = "Npart", "ET", "CC", "EBj", "b", "dN/dy" ...

TGraphErrors*
emcAnalyzer::suppress_vs_centrality( const char *suppress_vs_what,
                                     const char* cut,
                                     double ptmin,
                                     double ptmax,
                                     const bool bin_scaling,
                                     const bool UseIntHagFit,
                                     TCanvas *c1,
                                     const bool print_Ncoll_errors,
                                     const TString additionalSuppr )
{

  TGraphErrors *Suppr_vs_Cent = 0;

  TString suppr_vs_what = suppress_vs_what ;
  TString cut_str = cut ;

  fPlot = false;

  double xmax = 0;

  char title[300];
  char xtitle[300];
  char ytitle[300];

  if (bin_scaling)
    {
      sprintf(ytitle, "R_{AA}(p_{T} > %.1f GeV/#font[72]{c})", ptmin);
    }
  else
    {
      sprintf(ytitle, "R_{AA}^{Npart} (p_{T} > %.1f GeV/#font[72]{c})", ptmin);
    }

  if ( suppr_vs_what.Contains("Npart") )
    {
      xmax = 375; // 350; // Npart
      sprintf(title, "suppr_above_%.1fGeVc2_vs_Npart_%s", ptmin, cut);
      sprintf(xtitle, "#LT N_{part}#GT");
    }
  else if (suppr_vs_what.Contains("ET") )
    {
      xmax = 600; // GeV
      sprintf(title, "suppr_above_%.1fGeVc2_vs_ET_%s", ptmin, cut);
      sprintf(xtitle, "E_{T} (GeV)");
    }
  else if (suppr_vs_what.Contains("CC") )
    {
      xmax = 100; // CC
      sprintf(title, "suppr_above_%.1fGeVc2_vs_centrality_%s", ptmin, cut);
      sprintf(xtitle, "Centrality bin (%%)");
    }
  else if (suppr_vs_what.Contains("EBj") )
    {
      xmax = 5.5; // GeV/fm^3
      sprintf(title, "suppr_above_%.1fGeVc2_vs_EBjorken_%s", ptmin, cut);
      sprintf(xtitle, "#epsilon_{Bjorken} (GeV/fm^{3})");
    }
  else if (suppr_vs_what.Contains("b") )
    {
      xmax = 20.; // fm
      sprintf(title, "suppr_above_%.1fGeVc2_vs_impactparam_%s", ptmin, cut);
      sprintf(xtitle, "Impact parameter #b (fm)");
    }
  else if (suppr_vs_what.Contains("dNdy") )
    {
      xmax = 700.; // dN/dy
      sprintf(title, "suppr_above_%.1fGeVc2_vs_dNdy_%s", ptmin, cut);
      sprintf(xtitle, "dN_{ch}/dy");
    }
  else
    {
      cout << " <E> Don't know this variable \"" << suppr_vs_what << endl
	   << "     Known options: \"Npart\", \"ET\", \"CC\", \"EBj\", \"b\", \"dN/dy\" " << endl;
      return Suppr_vs_Cent;
    }

  int Cent1 = 0;
  int Cent2 = 0;

  double extra_relat_pp = 0.096;
  double AuAuAcceptanceError = 0.05; // /Sqrt(2.);
  double ppAcceptanceError = 0.038;  // 0.05/Sqrt(2.);

  //_____________________________________________________________________________
  // integrate pp spectrum above ptmin

  TGraphErrors *errBand;

  TGraphErrors *Spectrum_pp = (TGraphErrors*)plot_spectrum_from_ascii("pi0_pp_emcal_200GeV_final_cross_sasha.txt",
								      (TGraphErrors*&)errBand);

  AddOrSubstractRelatErrors(Spectrum_pp, "pp", "-", fVerbose); // subract the ppAcceptanceError --> goes to the band

  double *pp_yield_above_ptmin = 0;
  pp_yield_above_ptmin = (double*)integral(Spectrum_pp, ptmin, ptmax);

  //   // for test purposes only
  //   TH1F* h_spectrum_pp = (TH1F*)graph2histo(Spectrum_pp);
  //   int binptmin = h_spectrum_pp->GetXaxis()->FindBin(ptmin);
  //   int binptmax = h_spectrum_pp->GetXaxis()->FindBin(ptmax);
  //   double test  = h_spectrum_pp->Integral(binptmin,binptmax,"width");

  // Integral of Hagedorn fit
  TF1 *hagfit = (TF1*)Spectrum_pp->GetListOfFunctions()->FindObject("hagedornascii");
  double hagInt = 0.;
  if (hagfit)
    {
      hagInt = hagfit->Integral(ptmin, ptmax);
    }

  //   printf("<I> Double check pp integral: From histo = %.4e - From graph = %.4e - From Hagedorn fit = %.4e\n"
  // 	 ,test,pp_yield_above_ptmin[0],hagInt);

  if (UseIntHagFit)
    {
      pp_yield_above_ptmin[0] = hagInt;
    }
  //_____________________________________________________________________________
  // Loop on all centralities

  char file[300];
  TGraphErrors* Spectrum_AuAu = 0;

  int CCreduced = 10;
  int CCunused = 0;

  double RAA_integ[CCreduced];
  double eRAA_integ[CCreduced];
  double Cent[CCreduced];
  double eCent[CCreduced];
  double eNcoll[CCreduced];

  // those 2 vars. for multiple axis plot
  double Cent0[CCreduced];
  double npart[CCreduced];
  double ebjork[CCreduced];

  for (int i = 0; i < CCreduced; i++)
    {
      getCentralityClassLimits2( i*2, Cent1, Cent2);

      //       if ( !suppr_vs_what.Contains("E",TString::kExact) &&
      // 	   (i*2 == 14 || i*2 == 16) )
      // 	{
      // 	  CCunused++;
      // 	  continue; // skip those two now ...
      // 	}
      if ( !suppr_vs_what.Contains("E", TString::kExact) &&
           (i*2 > 16) )
        {
          CCunused++;
          continue; // skip those above 80-92% ...
        }
      if ( suppr_vs_what.Contains("E", TString::kExact) && (i*2 > 12) ) // "ET" and "EBj"
        {
          CCunused++;
          continue; // skip the most peripheral cases (60-70% is the last CC for PHENIX E_T)
        }

      if ( cut_str.Contains("emcal") )  // emcal
        {
          sprintf(file, "pi0_emcal_%d_%d_%s.txt", Cent1, Cent2, favoritecut); // Combined PbSc+PbGl is for tof1chisq1 (PbSc)
        }
      else if ( cut_str.Contains("pbgl") ) // pbgl
        {
          //sprintf(file,"pbgl_year2_FitP3Cor2_%d-%d.dat",Cent1,Cent2); // no y-shift
          sprintf(file, "pbgl_year2_FitP3Cor4_%d-%d.dat", Cent1, Cent2); // y-shift
        }
      else if ( cut_str.Contains("QM02") )
        {
          sprintf(file, "pi0_pbsc_%d_%d_QM02.txt", Cent1, Cent2); // QM02 spectra
          extra_relat_pp = 0.15;
        }
      else // pbsc
        {
          sprintf(file, "pi0_pbsc_%d_%d_%s.txt", Cent1, Cent2, cut);
        }

      Spectrum_AuAu = (TGraphErrors*)plot_spectrum_from_ascii((char*)file, (TGraphErrors*&)errBand);
      if (!Spectrum_AuAu)
        {
          cout << endl << " <W> Centrality Class: " << Cent1 << " - " << Cent2 << "% skipped !" << endl;
          CCunused++;
        }

      if (!Spectrum_AuAu)
        continue;

      AddOrSubstractRelatErrors(Spectrum_AuAu, "RAA", "-", fVerbose);  // The AuAu acceptance error goes into the band

      //_____________________________________________________________________________
      // y-axis

      double *AuAu_yield_above_ptmin = 0;
      AuAu_yield_above_ptmin = (double*)integral(Spectrum_AuAu, ptmin, ptmax);

      //       // for test purposes only
      //       TH1F* h_spectrum_auau = (TH1F*)graph2histo(Spectrum_AuAu);
      //       int binptmin = h_spectrum_auau->GetXaxis()->FindBin(ptmin);
      //       int binptmax = h_spectrum_auau->GetXaxis()->FindBin(ptmax);
      //       double integr = h_spectrum_auau->Integral(binptmin,binptmax,"width");

      // Integral of Hagedorn fit
      TF1 *hagfit = (TF1*)Spectrum_AuAu->GetListOfFunctions()->FindObject("hagedornascii");
      double hagInt = 0;
      if (hagfit)
        {
          hagInt = hagfit->Integral(ptmin, ptmax);
        }

      //       printf("<I> Double check pp integral: From histo = %.4e - From graph = %.4e - From Hagedorn fit = %.4e\n"
      // 	     ,integr,AuAu_yield_above_ptmin[0],hagInt);

      if (UseIntHagFit)
        {
          AuAu_yield_above_ptmin[0] = hagInt;
        }

      //_____________________________________________________________________________
      // scaling

      double NormalizedAuAu = 1.;
      double Normalized_pp = 1.;
      double eNormalizedAuAu = 0.;

      if (bin_scaling)
        {
          NormalizedAuAu = getTAB(Cent1, Cent2, "value"); //getNcoll(Cent1,Cent2,"value");
          eNormalizedAuAu = getTAB(Cent1, Cent2, "error"); //getNcoll(Cent1,Cent2,"error");

          Normalized_pp = getNcoll("pp"); // equal to 1

          if ( cut_str.Contains("QM02") )
            {
              NormalizedAuAu = getNcoll(Cent1, Cent2, "value_old"); // QM02 Glauber
              eNormalizedAuAu = getNcoll(Cent1, Cent2, "error_old"); // QM02 Glauber
            }
        }
      else // Npart scaling
        {
          NormalizedAuAu = getNpart(Cent1, Cent2, "value");
          eNormalizedAuAu = getNpart(Cent1, Cent2, "error");

          Normalized_pp = getNpart("pp"); // equal to 2

          // pp ascii data tables are in mb/(GeV/c)^{-2}
          Normalized_pp *= sigma_NN;

          if ( cut_str.Contains("QM02") )
            {
              NormalizedAuAu = getNpart(Cent1, Cent2, "value_old"); // QM02 Glauber
              eNormalizedAuAu = getNpart(Cent1, Cent2, "error_old"); // QM02 Glauber
            }
        }

      RAA_integ[i-CCunused] = (AuAu_yield_above_ptmin[0]*Normalized_pp)/(pp_yield_above_ptmin[0]*NormalizedAuAu);

      eRAA_integ[i-CCunused] = quadRelatError(AuAu_yield_above_ptmin[1], AuAu_yield_above_ptmin[0],
						pp_yield_above_ptmin[1], pp_yield_above_ptmin[0]);
      eRAA_integ[i-CCunused] *= RAA_integ[i-CCunused];

      // separated error band

      eNcoll[i-CCunused] = quadRelatError( eNormalizedAuAu, NormalizedAuAu,
					   extra_relat_pp, 1.,
					   AuAuAcceptanceError, 1.,
					   ppAcceptanceError, 1.);

      eNcoll[i-CCunused] *= RAA_integ[i-CCunused];
      //eNcoll[i-CCunused] *= 2.; // 2-sigma error

      //_____________________________________________________________________________
      // x-axis

      if ( suppr_vs_what.Contains("Npart") )
        {
          Cent[i-CCunused] = getNpart(Cent1, Cent2, "value");
          eCent[i-CCunused] = getNpart(Cent1, Cent2, "error");

          if ( cut_str.Contains("QM02") )
            {
              Cent[i-CCunused] = getNpart(Cent1, Cent2, "value_old"); // QM02 Glauber
              eCent[i-CCunused] = getNpart(Cent1, Cent2, "error_old"); // QM02 Glauber
            }
        }
      else if ( suppr_vs_what.Contains("ET") )
        {
          Cent[i-CCunused] = getET(Cent1, Cent2, "value");
          eCent[i-CCunused] = getET(Cent1, Cent2, "error");
        }
      else if ( suppr_vs_what.Contains("CC") )
        {
          eCent[i-CCunused] = Abs(Cent1 - Cent2)/2;     // width of CC
          Cent[i-CCunused] = xmax - (Cent1 + eCent[i-CCunused]); // "center" of CC (CC is plotted in decreasing values)

          // for multiple axis plots
          Cent0[i-CCunused] = Cent1 + eCent[i-CCunused];
          ebjork[i-CCunused] = getEBjorken(Cent1, Cent2, "value");
          npart[i-CCunused] = getNpart(Cent1, Cent2, "value");
        }
      else if ( suppr_vs_what.Contains("b") )
        {
          Cent[i-CCunused] = getImpactParam(Cent1, Cent2, "value");
          eCent[i-CCunused] = getImpactParam(Cent1, Cent2, "error");
        }
      else if ( suppr_vs_what.Contains("dNdy") )
        {
          Cent[i-CCunused] = getNch(Cent1, Cent2, 200, "value");
          eCent[i-CCunused] = getNch(Cent1, Cent2, 200, "error");
        }
      else if ( suppr_vs_what.Contains("EBj") ) // Bjorken estimate of energy density
        {
          Cent[i-CCunused] = getEBjorken(Cent1, Cent2, "value");
          eCent[i-CCunused] = getEBjorken(Cent1, Cent2, "error"); // fixme: shouldn't this error go the band ?

          if (fVerbose)
            {
              //cout << "ET       (GeV)    : " << Et << " +/- " << eEt << endl;
              cout << "EBjorken (GeV/fm3): " << Cent[i-CCunused] << " +/- " << eCent[i-CCunused] << endl;
            }
        }

      if ( suppr_vs_what.Contains("DeltaEloss") ) // plot versus DeltaE_loss instead of R_AA
        {
          double n = 8.1;
          double erelat = eRAA_integ[i-CCunused]/RAA_integ[i-CCunused];
          RAA_integ[i-CCunused] = abs(pow( RAA_integ[i-CCunused], 1./(n-1))-1.);
          eRAA_integ[i-CCunused] = RAA_integ[i-CCunused]*erelat;
        }

      cout << " ****************** " << Cent[i-CCunused] << " +/- " << eCent[i-CCunused]
	   << " --> " << RAA_integ[i-CCunused] << " +/- " << eRAA_integ[i-CCunused]
	   << " +/- " << eNcoll[i-CCunused] << "(band) " << endl;
    }

  int Nfinal = CCreduced - CCunused;
  Suppr_vs_Cent = new TGraphErrors(Nfinal, Cent, RAA_integ, eCent, eRAA_integ);
  //if ( suppr_vs_what.Contains("CC") ) reverse(*Suppr_vs_Cent); // change the order of the entries: periph->central
  Suppr_vs_Cent->Print();

  //   TGraphErrors* Suppr_vs_Cent2 = (TGraphErrors*)chopPointsFromGraph(Suppr_vs_Cent,CCunused);
  //   Suppr_vs_Cent2->Print();
  //   delete Suppr_vs_Cent;

  //_____________________________________________________________________________
  // Let's plot it

  double max_RAA = 1.4;
  double min_RAA = 0.;

  if (!bin_scaling) max_RAA = 5.; //2.5;

  TCanvas *c10 = (TCanvas*)canvas(title, 620, 600);
  c10->Range( -1.7, -19.4, 12.6, 1.40);

  TH2F *myframe = (TH2F*)frame(title, 20, 0., xmax, 10, min_RAA, max_RAA, xtitle, ytitle);
  //myframe->GetYaxis()->CenterTitle();
  myframe->Draw();

  setMarkerLineType(Suppr_vs_Cent, 20, 97, 2.1, 1, 97, 1);
  Suppr_vs_Cent->Draw("P");

  if ( suppr_vs_what.Contains("CC") )
    {
      // decreasing labels
      //myframe->GetXaxis()->SetDrawOption("<");
      //myframe->GetXaxis()->LabelsOption(">");
      myframe->GetXaxis()->SetLabelOffset(6); // "delete" them
      double ypos = -0.0525773;
      //if (logy) ypos -= 1;
      TLatex *tex = new TLatex(10.6742, ypos, "80-92   70-80   60-70   50-60   40-50   30-40   20-30   10-20   0-10");
      tex->SetTextFont(52);
      tex->SetTextSize(0.032);
      tex->SetLineWidth(2);
      tex->Draw();

      //
      // Determine the functional relation between CC and Npart
      //

      double npart_min = 0.; //npart[0];
      double npart_max = 350.; //npart[Nfinal-1];

      TCanvas *c20 = new TCanvas("c20", "c20", 600, 600);
      c20->cd();
      //TGraph *cent_npart  = new TGraph(Nfinal+1, Cent0, npart);
      TGraph *cent_npart = new TGraph(Nfinal, npart, Cent);
      cent_npart->SetMarkerStyle(20);
      cent_npart->Draw("AP");

      TF1 CC_vs_Npart("CC_vs_Npart", "[0]+[1]*x+[2]*sqrt(x)+[3]*x^2", npart_min, npart_max);
      //TF1 CC_vs_Npart("CC_vs_Npart","pol3",0,100); // npart_min,npart_max);
      cent_npart->Fit("CC_vs_Npart");
      CC_vs_Npart.SetRange(npart_min, npart_max);

      // Apply xmax-x to agree with the decreasing "CC" x-axis

      TF1 CC_vs_Npart2("CC_vs_Npart2", "[0]+[1]*x+[2]*sqrt(x)+[3]*x^2", npart_min, npart_max);
      //TF1 CC_vs_Npart2("CC_vs_Npart2","pol3",npart_min,npart_max);
      CC_vs_Npart2.SetParameters( -7.647, -2.9721e-01, 9.74943, 2.24753e-04);

      //       CC_vs_Npart2.SetParameters(CC_vs_Npart.GetParameter(0),
      // 				 CC_vs_Npart.GetParameter(1),
      // 				 CC_vs_Npart.GetParameter(2),
      // 				 CC_vs_Npart.GetParameter(3));

      //       for (int i=0; i<=100;i+=10) {cout << "Npart=" << CC_vs_Npart.Eval(i)
      // 					<< "      " << CC_vs_Npart2.Eval(i) << " for CC=" << i << endl;}

      TGaxis *axis_Npart = new TGaxis(0, -0.2, 100., -0.2, "CC_vs_Npart2", 510, "");
      axis_Npart->SetName("axis_Npart");
      axis_Npart->SetTitle("#LT N_{part}#GT");
      axis_Npart->SetTitleSize(0.05);
      axis_Npart->SetTitleOffset(0.8);

      c10->cd();
      axis_Npart->Draw();

      //       TGaxis *axis_Npart2 = new TGaxis(0,-0.2,100.,-0.2,"CC_vs_Npart2",510,"");
      //       axis_Npart2->SetName("axis_Npart2");
      //       axis_Npart2->SetTitle("N_{part2}");
      //       axis_Npart2->SetTitleSize(0.05);
      //       axis_Npart2->SetTitleOffset(0.8);

      //       c10->cd();
      //       axis_Npart2->Draw();

      // Determine the functional relation between Npart and E_Bjorken

      //double ebjork_min = 0.4;//ebjork[0];
      double ebjork_max = 5.3; //ebjork[Nfinal-3];

      TCanvas *c2 = new TCanvas("c2", "c2", 600, 600);
      c2->cd();
      //TGraph *cent_ebj = new TGraph(Nfinal+1, Cent0, ebjork);
      TGraph *cent_ebj = new TGraph(Nfinal - 2, ebjork, npart);
      cent_ebj->SetMarkerStyle(20);
      cent_ebj->Draw("AP");
      TF1 CC_vs_EBj("CC_vs_EBj", "pol3", 0.7, 5.3); //30,100); // starts at CC~30 to coincide with the lowest E_Bj
      cent_ebj->Fit("CC_vs_EBj");

      // Apply xmax-x to agree with the decreasing "CC" x-axis

      TF1 CC_vs_EBj2("CC_vs_EBj2", "[0]+[1]*([4]-x)+[2]*([4]-x)^2.+[3]*([4]-x)^3.", 30, 100); //ebjork_min,ebjork_max);
      CC_vs_EBj2.SetParameters( CC_vs_EBj.GetParameter(0),
                                CC_vs_EBj.GetParameter(1),
                                CC_vs_EBj.GetParameter(2),
                                CC_vs_EBj.GetParameter(3),
                                ebjork_max);

      TGaxis *axis_EBj = new TGaxis(32.5, -0.3, 100., -0.3, "CC_vs_EBj", 510, ""); // starts at CC~30 to coincide with the lowest E_Bj
      axis_EBj->SetName("axis_EBj");
      axis_EBj->SetTitle("#epsilon_{Bjorken} (GeV/fm^{3})");
      axis_EBj->SetTitleSize(0.05);
      axis_EBj->SetTitleOffset(1.0);

      c10->cd();
      //axis_EBj->Draw();

    }

  TLine *line = new TLine(0., 1., xmax, 1.);
  //if (logy) line = new TLine(0.,Log(1.0),100.,Log(1.0));
  line->SetLineColor(2);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw("same");

  // Separated Ncoll errors

  TGraphErrors* g_eNcoll = new TGraphErrors(Nfinal, Cent, RAA_integ, 0, eNcoll);
  cout << endl;
  g_eNcoll->Print();
  c10->cd();

  g_eNcoll->SetLineColor(17);
  g_eNcoll->SetLineWidth(20);
  if (print_Ncoll_errors) g_eNcoll->Draw("PZ");

  if (c1)
    {
      c1->cd();
      g_eNcoll->Draw("PZ");
    }

  c10->cd();
  c10->RedrawAxis();
  line->Draw("same");
  Suppr_vs_Cent->Draw("P");

  //  Suppr_vs_Cent->GetListOfFunctions()->Add(g_eNcoll);

  c10->Update();

  //____________________________________________________________________________
  // Additional suppressions: "charged", 3-reaction-planes pi0, ...

  TGraphErrors *chhad_supp = 0;

  if (additionalSuppr.Contains("charged", TString::kIgnoreCase))
    {
      //____________________________________________________________________________
      //
      // Charged-particle suppression above 4.5 GeV/c (PPG023)

      const int NCent_h = 11;
      int i = 0;
      double xshift = 0.;
      double data_h[][NCent_h] =
        {
          {245.969, 230.367, 201.893, 182.423, 152.544, 112.966, 70.7813, 42.2729, 20.8497, 10.1306, 3.90988}, // [0] ratio to pp
          {0.036166, 0.0353971, 0.0355332, 0.0349231, 0.0259474, 0.0289224, 0.0348838,
           0.0449459, 0.0626835, 0.0905392, 0.123706}, // [1] stat error.
          {1065.4, 845.4, 672.4, 532.7, 373.8, 219.8, 120.3, 61, 28.5, 12.4, 4.9}, // [2] ncoll
          {0.099, 0.097, 0.1, 0.098, 0.106, 0.103, 0.114, 0.162, 0.267, 0.339, 0.245}, // [3] ncoll err
          {351.4, 299, 253.9, 215.3, 166.6+xshift, 114.2+xshift, 74.4+xshift, 45.5+xshift, 25.7+xshift, 13.4+xshift, 6.3+xshift}, // [4] npart
          {0.008, 0.013, 0.017, 0.025, 0.032, 0.039, 0.051, 0.073, 0.148, 0.224, 0.19}, // [5] npart err
          {328.934, 279.685, 234.015, 195.397, 147.982, 97.0473, 59.5192, 33.1974, 16.9113, 8.18845, 3.44166},  //
          {0.000351704, 0.000377636, 0.000402126, 0.00042869, 0.000343106, 0.000418224,
           0.000521459, 0.000697086, 0.000969852, 0.00139486, 0.00198996},  //
          {0.05, 0.05, 0.05, 0.05, 0.036, 0.036, 0.036, 0.036, 0.023, 0.023, 0.02} //0.02}, // [8] mult. err (?)
        };

      double x[20], xe[20], y[20], ye[20];

      for (i = 0;i < NCent_h;i++)
        {
          x[i] = data_h[4][i];
          xe[i] = 0.;

          y[i] = data_h[0][i];
          if (bin_scaling)
            y[i] /= data_h[2][i];
          else
            y[i] /= (data_h[4][i]/2.); // Npart scaling
          ye[i] = data_h[1][i]*y[i];
        }

      if ( !suppr_vs_what.Contains("Npart") )
        {
          cout << "<E> Centrality dependence for charged-hadron data"
	       << "only implemented as a function of Npart so far ..." << endl;
        }

      chhad_supp = new TGraphErrors(NCent_h, x, y, xe, ye);
      setMarkerLineType(chhad_supp, 29, 9, 2.7, 1, 9, 1);
      chhad_supp->Print();
      chhad_supp->Draw("P");

      // Normalization errors as horizontal lines

      double ppnorm = 0.1053; //9.6+2.4+3.8;
      double chsys = 0.114;  //charged systematic errors
      double ppchn = 0.14;   //pp rest of systematic error for charged
      double norm_err = 0.;

      TGraphErrors *chhad_err = (TGraphErrors*)chhad_supp->Clone();
      for (i = 0;i < NCent_h;i++)
        {
          if (bin_scaling)
            norm_err = quadRelatError(ppnorm, 1., chsys, 1., ppchn, 1., data_h[3][i], 1., data_h[8][i], 1.);
          else
            norm_err = quadRelatError(ppnorm, 1., chsys, 1., ppchn, 1., data_h[5][i], 1., data_h[8][i], 1.); // Npart scaling
          norm_err *= y[i];
          chhad_err->SetPointError(i, 0., norm_err);
        }

      gStyle->SetEndErrorSize(5);
      setMarkerLineType(chhad_err, 1, 9, 2.1, 1, 9, 4);
      chhad_err->Print();
      chhad_err->Draw("P[]");

      //   for(i=0;i<NCent_h;i++)
      //     {
      //       x[i] = data_h[4][i];
      //       y[i] = 1.+quadRelatError(ppnorm,1.,data_h[3][i],1.);//error band
      //     }
      //   TGraph *Ncollerr_sup = new TGraphErrors(NCent_h,x,y);
      //   Ncollerr_sup->SetLineStyle(3);
      //   Ncollerr_sup->SetFillColor(19);
      //   Ncollerr_sup->Draw("L");
      //   Ncollerr_sup->Print();

      //   for(i=0;i<NCent_h;i++)
      //     {
      //       x[i] = data_h[4][i];
      //       y[i] = 1.-quadRelatError(ppnorm,1.,data_h[3][i],1.);//error band
      //    }
      //   TGraphErrors *Ncollerr_inf = new TGraphErrors(NCent_h,x,y);
      //   Ncollerr_inf->SetLineStyle(3);
      //   Ncollerr_inf->Draw("L");
      //   Ncollerr_inf->Print();
    }
  else if (additionalSuppr.Contains("reac", TString::kIgnoreCase))
    {
      //____________________________________________________________________________
      // Recursive thing here ...
      // let's plot the 3-reaction-plane pi0 suppression

      chhad_supp = (TGraphErrors*)suppress_vs_centrality("Npart", "r012", 4.5, 100., true, false, c10, false, "");
      //for (int i=0; i<chhad_supp->GetN(); i++) chhad_supp->SetPointError(i,0.,0.);
      setMarkerLineType(chhad_supp, 28, 1, 2.5, 1, 1, 1);
      chhad_supp->Draw("P");
    }

  TLegend *leg = new TLegend(0.5, 0.8, 1.0, 1.1, NULL, "brNDC");
  leg->SetMargin(0.1);
  leg->SetTextSize(0.06);
  leg->SetLineColor(1);
  leg->SetFillColor(0);
  leg->SetLineStyle(1);
  leg->SetLineWidth(1);
  leg->SetBorderSize(0);

  char label0[50];
  char label1[50];
  sprintf(label0, "#pi^{0} (p_{T} > %.1f GeV/#font[72]{c})", ptmin);
  sprintf(label1, "h^{#pm} (p_{T} > 4.5 GeV/#font[72]{c})");
  leg->AddEntry(Suppr_vs_Cent, label0, "P");
  if (chhad_supp)
    leg->AddEntry(chhad_supp, label1, "P");
  leg->Draw();

  c10->Update();

  return Suppr_vs_Cent;
}

//_____________________________________________________________________________

TGraphErrors*
emcAnalyzer::yield_vs_centrality( const char *yield_vs_what,
                                  const char* cut,
                                  double pt,
                                  bool part_scaling )
{

  TGraphErrors *Yield_vs_Cent = 0;

  TString Yield_vs_what = yield_vs_what ;
  TString cut_str = cut ;

  double xmax = 0.;
  char title[300];
  char xtitle[300];
  char ytitle[300];

  if (!part_scaling)
    {
      sprintf(ytitle, "Yield(p_{T} = %.1f GeV/#font[72]{c})/(#LT N_{coll}#GT", pt);
    }
  else
    {
      sprintf(ytitle, "Yield(p_{T} = %.1f GeV/#font[72]{c})/(#LT N_{part}#GT/2.)", pt);
    }

  if ( Yield_vs_what.Contains("Npart") )
    {
      xmax = 350.; // Npart
      sprintf(title, "yield_%.1fGeVc_vs_Npart_%s", pt, cut);
      sprintf(xtitle, "#LT N_{part}#GT");
    }
  else if (Yield_vs_what.Contains("ET") )
    {
      xmax = 600.; // GeV
      sprintf(title, "yield_%.1fGeVc_vs_ET_%s", pt, cut);
      sprintf(xtitle, "E_{T} (GeV)");
    }
  else if (Yield_vs_what.Contains("CC") )
    {
      xmax = 100.; // CC
      sprintf(title, "yield_%.1fGeVc_vs_centrality_%s", pt, cut);
      sprintf(xtitle, "Centrality");
    }
  else if (Yield_vs_what.Contains("EBj") )
    {
      xmax = 5.5; // GeV/fm3
      sprintf(title, "yield_%.1fGeVc_vs_EBjorken_%s", pt, cut);
      sprintf(xtitle, "#epsilon_{Bjorken} (GeV/fm^{3})");
    }
  else
    {
      cout << " <E> Don't know this variable \"" << Yield_vs_what << endl
	   << "     Known options: \"Npart\", \"ET\",\"CC\" " << endl;
      return Yield_vs_Cent;
    }

  // Loop on all centralities

  int Cent1 = 0;
  int Cent2 = 0;

  char file[300];
  TGraphErrors* Spectrum_AuAu = 0;

  int CCreduced = 10;
  int CCunused = 0;

  double Yield[CCreduced];
  double eYield[CCreduced];
  double Cent[CCreduced];
  double eCent[CCreduced];
  double eNcoll[CCreduced];

  double yield = 0.;
  double eyield = 0.;
  double NormalizedAuAu = 0.;
  double eNormalizedAuAu = 0.;
  double Normalized_pp = 0.;

  double extra_relat_pp = 0.096;
  double AuAuAcceptanceError = 0.05;
  double ppAcceptanceError = 0.038;  // 0.05/Sqrt(2.);

  for (int i = 0; i < CCreduced; i++)
    {
      getCentralityClassLimits2( i*2, Cent1, Cent2);

      if ( i*2 == 14 || i*2 == 16 )
        {
          CCunused++;
          continue; // skip those two now ...
        }

      if ( cut_str.Contains("emcal") )  // emcal
        {
          sprintf(file, "pi0_emcal_%d_%d_%s.txt", Cent1, Cent2, favoritecut); // Combined PbSc+PbGl is for tof1chisq1 (PbSc)
        }
      else if ( cut_str.Contains("pbgl") ) // pbgl
        {
          //sprintf(file,"pbgl_year2_FitP3Cor2_%d-%d.dat",Cent1,Cent2); // no y-shift
          sprintf(file, "pbgl_year2_FitP3Cor4_%d-%d.dat", Cent1, Cent2); // y-shift
        }
      else if ( cut_str.Contains("QM02") )
        {
          sprintf(file, "pi0_pbsc_%d_%d_QM02.txt", Cent1, Cent2); // QM02 spectra
        }
      else // pbsc
        {
          sprintf(file, "pi0_pbsc_%d_%d_%s.txt", Cent1, Cent2, cut);
        }

      TGraphErrors *errBand;

      Spectrum_AuAu = (TGraphErrors*)plot_spectrum_from_ascii((char*)file, (TGraphErrors*&)errBand);
      if (!Spectrum_AuAu)
        {
          cout << endl << " <W> Centrality Class: " << Cent1 << " - " << Cent2 << "% skipped !" << endl;
          CCunused++;
          continue;
        }

      //_____________________________________________________________________________
      // y-axis

      TH1F* h_spectrum_auau = (TH1F*)graph2histo(Spectrum_AuAu);
      int binpt = h_spectrum_auau->GetXaxis()->FindBin(pt) - 1; // bin shift -1 between histo bin and TGraph
      yield = Spectrum_AuAu->GetY()[binpt];
      eyield = Spectrum_AuAu->GetErrorY(binpt);

      NormalizedAuAu = 0.;
      eNormalizedAuAu = 0.;

      if (!part_scaling)
        {
          NormalizedAuAu = getTAB(Cent1, Cent2, "value"); //getNcoll(Cent1,Cent2,"value");
          eNormalizedAuAu = getTAB(Cent1, Cent2, "error"); //getNcoll(Cent1,Cent2,"error");

          Normalized_pp = getNcoll("pp");

          if ( cut_str.Contains("QM02") )
            {
              NormalizedAuAu = getNcoll(Cent1, Cent2, "value_old"); // QM02 Glauber
              eNormalizedAuAu = getNcoll(Cent1, Cent2, "error_old"); // QM02 Glauber
            }
        }
      else
        {
          NormalizedAuAu = getNpart(Cent1, Cent2, "value");
          eNormalizedAuAu = getNpart(Cent1, Cent2, "error");

          Normalized_pp = getNpart("pp");

          // pp ascii data tables are in mb/(GeV/c)^{-2}
          Normalized_pp *= sigma_NN;

          if ( cut_str.Contains("QM02") )
            {
              NormalizedAuAu = getNpart(Cent1, Cent2, "value_old"); // QM02 Glauber
              eNormalizedAuAu = getNpart(Cent1, Cent2, "error_old"); // QM02 Glauber
            }
        }

      Yield[i-CCunused] = (yield*Normalized_pp)/NormalizedAuAu;
      eYield[i-CCunused] = (eyield*Normalized_pp)/NormalizedAuAu;
      eNcoll[i-CCunused] = quadRelatError( eNormalizedAuAu, NormalizedAuAu,
					   extra_relat_pp, 1.,
					   AuAuAcceptanceError, 1.,
					   ppAcceptanceError, 1.);
      
      eNcoll[i-CCunused] *= Yield[i-CCunused];

      //_____________________________________________________________________________
      // x-axis

      if ( Yield_vs_what.Contains("Npart") )
        {
          Cent[i-CCunused] = getNpart(Cent1, Cent2, "value");
          eCent[i-CCunused] = getNpart(Cent1, Cent2, "error");

          if ( cut_str.Contains("QM02") )
            {
              Cent[i-CCunused] = getNpart(Cent1, Cent2, "value_old"); // QM02 Glauber
              eCent[i-CCunused] = getNpart(Cent1, Cent2, "error_old"); // QM02 Glauber
            }

        }
      else if ( Yield_vs_what.Contains("ET") )
        {
          Cent[i-CCunused] = getET(Cent1, Cent2, "value");
          eCent[i-CCunused] = getET(Cent1, Cent2, "error");
        }
      else if ( Yield_vs_what.Contains("CC") )
        {
          eCent[i-CCunused] = Abs(Cent1 - Cent2)/2; // "center" of CC
          Cent[i-CCunused] = Cent1 + eCent[i-CCunused];    // width of CC
        }

      else if ( Yield_vs_what.Contains("EBj") ) // Bjorken estimate of energy density
        {

          Cent[i-CCunused] = getEBjorken(Cent1, Cent2, "value");
          eCent[i-CCunused] = getEBjorken(Cent1, Cent2, "error"); // fixme: shouldn't this error go the band ?

          if (fVerbose)
            {
              //cout << "ET       (GeV)    : " << Et << " +/- " << eEt << endl;
              cout << "EBjorken (GeV/fm3): " << Cent[i-CCunused] << " +/- " << eCent[i-CCunused] << endl;
            }
        }

      cout << " ****************** " << Cent[i-CCunused] << " +/- " << eCent[i-CCunused]
	   << " --> " << Yield[i-CCunused] << " +/- " << eYield[i-CCunused] << " +/- "
	   << eNcoll[i-CCunused] << "(band) " << endl;
    }

  int Nfinal = CCreduced - CCunused;
  Yield_vs_Cent = new TGraphErrors(Nfinal, Cent, Yield, eCent, eYield);

  // set arbitrarily the yield at 2 for the most peripheral

  double setScaleAt2 = 2./((yield*Normalized_pp)/NormalizedAuAu);
  cout << "<I> Scaling all yields by a factor: " << setScaleAt2 << endl;
  scale(*Yield_vs_Cent, setScaleAt2, 0, fVerbose);

  Yield_vs_Cent->Print();

  //_____________________________________________________________________________
  // Plot

  double max_Yield = 5.5;
  double min_Yield = 0.;

  TCanvas *c10 = (TCanvas*)canvas(title, 620, 600);
  c10->Range( -1.7, -19.4, 12.6, 1.40);

  TH2F *myframe = (TH2F*)frame(title, 20, 0., xmax, 10, min_Yield, max_Yield, xtitle, ytitle);
  myframe->Draw();

  setMarkerLineType(Yield_vs_Cent, 20, 2, 1.4, 1, 1, 1);
  Yield_vs_Cent->Draw("P");

  TLine *line = new TLine(0., 1., xmax, 1.);
  //if (logy) line = new TLine(0.,Log(1.0),100.,Log(1.0));
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw("same");

  // Separated Ncoll errors
  TGraphErrors* g_eNcoll = new TGraphErrors(Nfinal, Cent, Yield, 0, eNcoll);
  scale(*g_eNcoll, setScaleAt2, 0, fVerbose);
  cout << endl;
  g_eNcoll->Print();
  c10->cd();
  g_eNcoll->SetLineColor(kYellow);
  g_eNcoll->SetLineWidth(20);
  g_eNcoll->Draw("PZ");

  c10->RedrawAxis();
  line->Draw("same");

  Yield_vs_Cent->Draw("P");

  c10->Update();

  return Yield_vs_Cent;

}

//_____________________________________________________________________________
//
TGraphErrors*
emcAnalyzer::dNdyAuAu_over_dNdypp_vs_centrality( const char *cent_type,
						 const char* cut,
						 double pT,
						 bool part_scaling )
{

  TGraphErrors *dNdyAuAu_over_dNdypp_vs_Cent = 0;


  TString centrality_type = cent_type ;
  TString cut_str = cut ;

  double xmax = 0;
  char title[300];
  char xtitle[300];
  char ytitle[300];

  if (part_scaling)
    {
      sprintf(ytitle, "(dN_{AuAu}/dy/dN_{pp}/dy) #times (#LT N_{part}^{pp}#GT/#LT N_{part}^{AuAu}#GT)");
      //sprintf(ytitle,"(dN_{AuAu}/dy #times #LT N_{part}^{pp}#GT)/(dN_{pp}/dy #times #LT N_{part}^{pp}#GT");
      //sprintf(ytitle,"dN_{AuAu}/dy/dN_{pp}/dy(p_{T} = %.1f GeV/#font[72]{c})/(#LT N_{part}#GT/2",pT);
    }
  else
    {
      sprintf(ytitle, "dN_{AuAu}/dy/(dN_{pp}dy #times #LT N_{coll}#GT)");
      //sprintf(ytitle,"dNdy_{AuAu}/dNdy_{pp}(p_{T} = %.1f GeV/#font[72]{c})/(#LT N_{coll}#GT",pT);
    }

  if ( centrality_type.Contains("Npart") )
    {
      xmax = 350; // Npart
      sprintf(title, "dNdyAuAu_over_dNdypp_%.1fGeVc_vs_Npart_%s", pT, cut);
      sprintf(xtitle, "#LT N_{part}#GT");
    }
  else if (centrality_type.Contains("ET") )
    {
      xmax = 600; // GeV
      sprintf(title, "dNdyAuAu_over_dNdypp_%.1fGeVc_vs_ET_%s", pT, cut);
      sprintf(xtitle, "E_{T} (GeV)");
    }
  else if (centrality_type.Contains("CC") )
    {
      xmax = 100; // CC
      sprintf(title, "dNdyAuAu_over_dNdypp_%.1fGeVc_vs_centrality_%s", pT, cut);
      sprintf(xtitle, "Centrality");
    }
  else if (centrality_type.Contains("EBj") )
    {
      xmax = 5.5; // GeV/fm3
      sprintf(title, "dNdyAuAu_over_dNdypp_%.1fGeVc_vs_EBjorken_%s", pT, cut);
      sprintf(xtitle, "#epsilon_{Bjorken} (GeV/fm^{3})");
    }
  else
    {
      cout << " <E> Don't know this variable \"" << centrality_type << endl
	   << "     Known options: \"Npart\", \"ET\",\"CC\" " << endl;

      return dNdyAuAu_over_dNdypp_vs_Cent;
    }

  // Loop on all centralities

  int Cent1 = 0;
  int Cent2 = 0;

  char file[300];
  TGraphErrors* Spectrum_AuAu = 0;
  TGraphErrors* Spectrum_pp = 0;

  int CCreduced = 10;
  int CCunused = 0;

  double dNdyAuAu_over_dNdypp[CCreduced];
  double edNdyAuAu_over_dNdypp[CCreduced];

  double Cent[CCreduced];
  double eCent[CCreduced];
  double eNorm[CCreduced];

  double dNdyAuAu = 0.;
  double edNdyAuAu = 0.;
  double dNdypp = 0.;
  double edNdypp = 0.;

  double NormalizedAuAu = 0.;
  double eNormalizedAuAu = 0.;
  double Normalized_pp = 0.;

  double extra_relat_pp = 0.096;
  double AuAuAcceptanceError = 0.05;
  double ppAcceptanceError = 0.038;

  //_____________________________________________________________________________
  // pp "dN/dy"

  TGraphErrors *errBand;

  if (pT != 0) // PHENIX dNpi0(pp)/dy for a given pT
    {
      Spectrum_pp = (TGraphErrors*)plot_spectrum_from_ascii("pi0_pp_emcal_200GeV_final_cross_sasha.txt",
							    (TGraphErrors*&)errBand);

      dNdypp = getYatX( Spectrum_pp, pT, "value" );
      edNdypp = getYatX( Spectrum_pp, pT, "error" );

      if (part_scaling) // for Ncoll scaling we use T_AB = Ncoll/sigma_NN
        {
          // pp ascii data tables are in mb/(GeV/c)^{-2}
          dNdypp /= sigma_NN;
          edNdypp /= sigma_NN;
        }

      cout << " ****** dNpi0/dy(pp) = " << dNdypp << " +/- " << edNdypp
	   << " (at pT = " << pT << " GeV/c)" << endl;
    }
  else // UA5 dNch(ppbar)/dy inclusive
    {
      //_____________________________________________________________________________

      dNdypp = getNch_ppbar(200, "value"); // UA5 value p+pbar @ 200 GeV
      edNdypp = getNch_ppbar(200, "error"); //

      cout << " ****** dNch/dy(ppbar) = " << dNdypp << " +/- " << edNdypp << endl;
    }

  //_____________________________________________________________________________
  // dNdy for AuAu (diff. centralities)

  for (int i = 0; i < CCreduced; i++)
    {
      getCentralityClassLimits2( i*2, Cent1, Cent2);

      if ( i*2 == 14 || i*2 == 16 )
        {
          CCunused++;
          continue; // skip those two for now ...
        }
      if ( (pT == 0) && (i*2 > 12) )
        {
          CCunused++;
          continue; // skip the most peripheral cases (60-70% is the last CC for PHENIX inclusive dNch/dy)
        }

      if ( cut_str.Contains("emcal") )  // emcal
        {
          sprintf(file, "pi0_emcal_%d_%d_%s.txt", Cent1, Cent2, favoritecut); // Combined PbSc+PbGl is for tof1chisq1 (PbSc)
        }
      else if ( cut_str.Contains("pbgl") ) // pbgl
        {
          sprintf(file, "pbgl_year2_FitP3Cor4_%d-%d.dat", Cent1, Cent2); // y-shift
        }
      else // pbsc
        {
          sprintf(file, "pi0_pbsc_%d_%d_%s.txt", Cent1, Cent2, cut);
        }

      //_____________________________________________________________________________
      // AuAu "dNpi0/dy"

      if (pT != 0) // PHENIX dNpi0(AuAu)/dy for a given pT
        {
          Spectrum_AuAu = (TGraphErrors*)plot_spectrum_from_ascii((char*)file, (TGraphErrors*&)errBand);
          if (!Spectrum_AuAu)
            {
              cout << endl << " <W> Centrality Class: " << Cent1 << " - " << Cent2 << "% skipped !" << endl;
              CCunused++;
              continue;
            }

          dNdyAuAu = getYatX( Spectrum_AuAu, pT, "value" );
          edNdyAuAu = getYatX( Spectrum_AuAu, pT, "error" );

          cout << " ****** dNpi0/dy(AuAu) = " << dNdyAuAu << " +/- " << edNdyAuAu
	       << " (at pT = " << pT << " GeV/c)" << endl;
        }
      else // PHENIX dNch(AuAu)/dy inclusive
        {
          dNdyAuAu = getNch(Cent1, Cent2, 200, "value"); // PHENIX dNch/dy @ 200 GeV
          edNdyAuAu = getNch(Cent1, Cent2, 200, "error"); //

          cout << " ****** dNch/dy(AuAu) = " << dNdyAuAu << " +/- " << edNdyAuAu << endl;
        }

      //_____________________________________________________________________________
      // Npart Normalization

      if (part_scaling)
        {
          NormalizedAuAu = getNpart(Cent1, Cent2, "value");
          eNormalizedAuAu = getNpart(Cent1, Cent2, "error");

          Normalized_pp = getNpart("pp"); // equal to 2
        }
      else
        {
          NormalizedAuAu = getTAB(Cent1, Cent2, "value");
          eNormalizedAuAu = getTAB(Cent1, Cent2, "error");

          Normalized_pp = getNcoll("pp"); // equal to 1
        }

      //_____________________________________________________________________________
      // dN/dy_AuAu over dN/dy_pp ratio

      dNdyAuAu_over_dNdypp[i-CCunused] = (dNdyAuAu*Normalized_pp)/(dNdypp*NormalizedAuAu);
      edNdyAuAu_over_dNdypp[i-CCunused] = quadRelatError( edNdyAuAu, dNdyAuAu,
							  edNdypp, dNdypp );
      
      edNdyAuAu_over_dNdypp[i-CCunused] *= dNdyAuAu_over_dNdypp[i-CCunused];

      if (pT != 0)
        {
          eNorm[i-CCunused] = quadRelatError( eNormalizedAuAu, NormalizedAuAu,
					      extra_relat_pp, 1.,
					      AuAuAcceptanceError, 1.,
					      ppAcceptanceError, 1. );
        }
      else
        {
          eNorm[i-CCunused] = quadRelatError( eNormalizedAuAu, NormalizedAuAu );
        }
      eNorm[i-CCunused] *= dNdyAuAu_over_dNdypp[i-CCunused];

      //_____________________________________________________________________________
      // x-axis

      if ( centrality_type.Contains("Npart") )
        {
          Cent[i-CCunused] = getNpart(Cent1, Cent2, "value");
          eCent[i-CCunused] = getNpart(Cent1, Cent2, "error");
        }
      else if ( centrality_type.Contains("ET") )
        {
          Cent[i-CCunused] = getET(Cent1, Cent2, "value");
          eCent[i-CCunused] = getET(Cent1, Cent2, "error");
        }
      else if ( centrality_type.Contains("CC") )
        {
          eCent[i-CCunused] = Abs(Cent1 - Cent2)/2; // "center" of CC
          Cent[i-CCunused] = Cent1 + eCent[i-CCunused];    // width of CC
        }

      else if ( centrality_type.Contains("EBj") ) // Bjorken estimate of energy density
        {
          Cent[i-CCunused] = getEBjorken(Cent1, Cent2, "value");
          eCent[i-CCunused] = getEBjorken(Cent1, Cent2, "error"); // fixme: shouldn't this error go the band ?

          if (fVerbose)
            {
              cout << "EBjorken (GeV/fm3): " << Cent[i-CCunused] << " +/- " << eCent[i-CCunused] << endl;
            }
        }

      cout << " ****************** " << Cent[i-CCunused] << " +/- " << eCent[i-CCunused]
	   << " --> " << dNdyAuAu_over_dNdypp[i-CCunused] << " +/- " << edNdyAuAu_over_dNdypp[i-CCunused]
	   << " +/- " << eNorm[i-CCunused] << "(band)" << endl;
    }

  int Nfinal = CCreduced - CCunused;

  dNdyAuAu_over_dNdypp_vs_Cent = new TGraphErrors(Nfinal, Cent, dNdyAuAu_over_dNdypp, eCent, edNdyAuAu_over_dNdypp);

  dNdyAuAu_over_dNdypp_vs_Cent->Print();

  //_____________________________________________________________________________
  // Plot

  double max_dNdyAuAu_over_dNdypp = 2.5;
  double min_dNdyAuAu_over_dNdypp = 0.;

  TCanvas *c10 = (TCanvas*)canvas(title, 620, 600);
  c10->Range( -1.7, -19.4, 12.6, 1.40);

  TH2F *myframe = (TH2F*)frame(title, 20, 0., xmax, 10, min_dNdyAuAu_over_dNdypp, max_dNdyAuAu_over_dNdypp, xtitle, ytitle);
  myframe->Draw();

  setMarkerLineType(dNdyAuAu_over_dNdypp_vs_Cent, 8, 1, 2.6, 1, 1, 1);
  dNdyAuAu_over_dNdypp_vs_Cent->Draw("P");

  TLine *line = new TLine(0., 1., xmax, 1.);
  //if (logy) line = new TLine(0.,Log(1.0),100.,Log(1.0));
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw("same");

  // Separated Ncoll errors
  TGraphErrors* g_eNcoll = new TGraphErrors(Nfinal, Cent, dNdyAuAu_over_dNdypp, 0, eNorm);
  cout << endl;
  g_eNcoll->Print();
  c10->cd();
  g_eNcoll->SetLineColor(17);
  g_eNcoll->SetLineWidth(20);
  g_eNcoll->Draw("PZ");

  // High pT suppression (Npart scaling) plotted here too

  TGraphErrors *Suppr_vs_CentNpart = suppress_vs_centrality("Npart", cut, 4., 100., false, false, c10);
  Suppr_vs_CentNpart->SetName("R_AA_Npart_high_pt_pi0");
  c10->cd();
  setMarkerLineType(Suppr_vs_CentNpart, 29, 1, 2.9, 1, 1, 1);
  Suppr_vs_CentNpart->Draw("P");

  //  legends
  TLegend *leg = new TLegend(0.5, 0.8, 0.9, 0.95, NULL, "brNDC");
  leg->SetMargin(0.1);
  leg->SetTextSize(0.052);
  leg->SetLineColor(1);
  leg->SetFillColor(0);
  leg->SetLineStyle(1);
  leg->SetLineWidth(1);
  leg->SetBorderSize(0);

  char label0[50];
  char label1[50];
  sprintf(label0, "#pi^{0} (p_{T} > 4 GeV/c)");
  sprintf(label1, "Inclusive hadrons");
  leg->AddEntry(Suppr_vs_CentNpart, label0, "P");
  leg->AddEntry(dNdyAuAu_over_dNdypp_vs_Cent, label1, "P");

  c10->RedrawAxis();
  line->Draw("same");

  dNdyAuAu_over_dNdypp_vs_Cent->Draw("PL");
  Suppr_vs_CentNpart->Draw("PL");
  leg->Draw();

  c10->Update();

  return dNdyAuAu_over_dNdypp_vs_Cent;

}


//_____________________________________________________________________________
//
TGraphErrors*
emcAnalyzer::plot_correction( const char* type,
                              const int Cent1,
                              const int Cent2,
                              const char* cut,
                              const double ptmin,
                              const double ptmax )
{

  TGraphErrors *Corr = 0;

  int CC = getCentralityClass(Cent1, Cent2);
  if ( CC == -1) return Corr;

  TString type_str = type;

  cout << " <I> Plotting " << type << " for " << fEfficParticle->Data()
       << " -- CentClass: " << CC << " and cut: " << cut << endl;

  char ytitle[300];
  sprintf(ytitle,"%s #%s (%d - %d%%) %s", type, fEfficParticle->Data(), Cent1, Cent2, cut);
  char title[300];
  sprintf(title,"%s_%s_%d-%d%%_%s", type, fEfficParticle->Data(), Cent1, Cent2, cut);

  double ptbinwidth = 0.5; // GeV/c
  size_t NpTbins = (size_t)((ptmax - ptmin)/ptbinwidth);

  double *pT = new double[NpTbins];
  double *epT = new double[NpTbins];
  double *corr = new double[NpTbins];
  double *ecorr = new double[NpTbins];

  for (size_t i = 0; i < NpTbins; i++)
    {
      pT[i] = i*ptbinwidth;
      epT[i] = 0.05;
      if (type_str.Contains("eff"))
        {
          corr[i] = efficiency(pT[i], CC, cut, "value", fEfficParticle->Data());
          ecorr[i] = efficiency(pT[i], CC, cut, "error", fEfficParticle->Data());
        }
      else if (type_str.Contains("acc"))
        {
          corr[i] = acceptance(pT[i], "value", fEfficParticle->Data());
          ecorr[i] = acceptance(pT[i], "error", fEfficParticle->Data());
        }
      else
        {
          return Corr;
        }

      if (fVerbose)
        {
          printf("  %.2f %.2f -->  %5.2f %5.2f \n", pT[i], epT[i], corr[i], ecorr[i]);
        }
    }

  TCanvas *c1 = (TCanvas*)canvas(title, 550, 610);
  c1->cd();

  Corr = new TGraphErrors( NpTbins, pT, corr, epT, ecorr);
  Corr->SetName(title);
  Corr->Draw("AP");
  Corr->GetYaxis()->SetTitle(ytitle);
  Corr->GetYaxis()->SetTitleOffset(1.4);
  Corr->GetXaxis()->SetTitle("p_{T} (GeV/#font[72]{c})");
  Corr->GetXaxis()->SetTitleOffset(0.95);
  Corr->SetMaximum(1.2);
  Corr->SetMinimum(0.);
  setMarker(Corr, Cent1, Cent2);

  Corr->Draw("AP");

  return Corr;
}

//_____________________________________________________________________________
//
int
emcAnalyzer::plot_corr_factors( const int Cent1,
                                const int Cent2,
                                const char* cut,
                                const char* type)
{

  int CC = getCentralityClass(Cent1, Cent2);
  if ( CC == -1) return -1;

  cout << " <I> Plotting Correction Factors (acc., eff., bin-shift) for CentClass: " << CC << endl;

  TGraphErrors *full = (TGraphErrors *)plot_spectrum(Cent1, Cent2, cut, "full");
  TGraphErrors *raw = (TGraphErrors *)plot_spectrum(Cent1, Cent2, cut, "raw");

  TGraphErrors *corr = (TGraphErrors*)ratio(full, raw, fVerbose, 0.);

  TCanvas *c1 = (TCanvas*)canvas("corr_factors", 550, 610);
  c1->cd();
  corr->Draw("AP");

  //   char titleglobal[300];
  //   if (Cent1<100) sprintf(titleglobal,"Centrality Class %d_%d %s", Cent1, Cent2, cut);
  //   else sprintf(titleglobal,"Centrality Class: min. bias %s",cut);

  //   double corr_factor[MaxPTbins];
  //   double err_corr_factor[MaxPTbins];
  //   double err_corr_factormin[MaxPTbins];
  //   double err_corr_factormax[MaxPTbins];

  //   double pT[MaxPTbins];
  //   double epT[MaxPTbins];


  //   int NumPTbins[CentClasses] = { 10, 13, //  0- 5,  5-10 // 14:  0-10
  // 				 16, 14, // 10-15, 15-20 // 14: 10-20

  // 				 14, 14, // 20-30
  // 				 14, 14, // 30-40
  // 				 14, 14, // 40-50
  // 				 12, 12, // 50-60
  // 				 11, 11, // 60-70
  // 				 11, 11, // 70-80
  // 				      9, // 80-92,
  // 				     14, // 60-92,
  // 				     14, // 60-80
  // 				     18};// min. bias


  //   int NpTbins = NumPTbins[CC];
  //   if ( (Cent1 == 0 || Cent1 == 10 ) && (Cent2 == 10 || Cent2 == 20 ) )
  //     {
  //       NpTbins = 14; // Those 2 mixed classes have longer ranges
  //     }

  //   double pTstep = 0.75;

  //   for (int pTbin = 0; pTbin < NpTbins ; pTbin++)
  //     {
  //       //pTstep += pTbinwidth - mean_pt_corr;
  //       pTstep = mean_pt_corr[pTbin];
  //       pT[pTbin] = pTstep;
  //       epT[pTbin] = 0.050 ; // const pT bin width = 0.05 GeV/c
  //       corr_factor[pTbin] = 1/(CorrectionFactor(pTstep,CC,cut,"value"));
  //       err_corr_factor[pTbin] = (CorrectionFactor(pTstep,CC,cut,"error"))*corr_factor[pTbin]*corr_factor[pTbin];
  //       err_corr_factormin[pTbin] = corr_factor[pTbin]-err_corr_factor[pTbin];
  //       err_corr_factormax[pTbin] = corr_factor[pTbin]+err_corr_factor[pTbin];
  //       cout << pTstep << "  " << corr_factor[pTbin] << endl;
  //     }

  //   char title0[100];
  //   //sprintf(title0," Full Correction Factors for Cent. Class %i_%i",Cent1,Cent2);
  //   sprintf(title0,"Efficiency#times Acceptance Correction Factors");
  //   char title[100];
  //   sprintf(title,"corr_factors_centclass_%i_%i",Cent1,Cent2);
  //   TCanvas *c0 = new TCanvas(title,title,600,600);
  //   //fCanvasList->Add(c0 = new TCanvas(name,name));
  //   c0->cd();

  //   //TGraphErrors *CorrFactPi0 = new TGraphErrors( NpTbins, pT, corr_factor, epT, err_corr_factor);
  //   TGraph *CorrFactPi0 = new TGraph( NpTbins, pT, corr_factor);
  //   TGraph *CorrFactPi0min = new TGraph( NpTbins, pT, err_corr_factormin);
  //   TGraph *CorrFactPi0max = new TGraph( NpTbins, pT, err_corr_factormax);

  //   TF1 *fitmin = new TF1("fitmin","pol7", 1.0, pTstep+0.2);
  //   CorrFactPi0min->Fit("fitmin","QR","");
  //   TF1 *fitmax = new TF1("fitmax","pol7", 1.0, pTstep+0.2);
  //   CorrFactPi0max->Fit("fitmax","QR","");

  //   double width_myframe = Abs(1.2*pTstep);
  //   int width_myframe2 = (int)(2*width_myframe);
  //   //double height_myframe = Abs(1.2*err_corr_factormax[0]);
  //   //TH2F *myframe = (TH2F*)frame("myframe",width_myframe2,0.,width_myframe,height_myframe,5,height_myframe);
  //   TH2F *myframe = (TH2F*)frame(titleglobal,width_myframe2,0.,width_myframe,25,5,30);
  //   myframe->Draw();

  //   TLegend *le = new TLegend(4.4,18.85,11.37,28.8," #pi^{0} spectra correction factors:","brNDC");
  //   le->AddEntry(myframe,"<Acceptance> #sim 1/23.0%","P");
  //   le->AddEntry(myframe,"<Efficiency> #sim 1/20.0% (low p_{T}) - 1/35.0% (high p_{T} ","P");
  //   le->AddEntry(myframe,"<Off-vertex #pi^{0}: #sim -6.0%","P");
  //   le->SetFillColor(kWhite);
  //   le->Draw();
  //   fitmax->SetFillColor(2);
  //   fitmax->SetFillStyle(3006);
  //   fitmax->SetLineColor(28);
  //   fitmax->SetLineWidth(3);
  //   fitmax->Draw("FCsame");

  //   fitmin->SetFillColor(10);
  //   fitmin->SetLineColor(28);
  //   fitmin->SetLineWidth(3);
  //   fitmin->Draw("FCsame");
  //   myframe->Draw("sameaxis");
  //   CorrFactPi0->SetMarkerStyle(8);
  //   CorrFactPi0->Draw("P");
  //   //c0->SetLogy();
  //   c0->Update();

  return 0;
}

//_____________________________________________________________________________
//
void
emcAnalyzer::plot_all_corr_factors(const char* cut )
{
  int Cent1 = 0;
  int Cent2 = 10;

  for (int i = 0; i <= CentClasses; i+=2)
    {
      if (i == 20)
        {
          plot_corr_factors(100, 100);
        }
      else if (Cent1 == 80)
        {
          plot_corr_factors(Cent1, 92);
        }
      else if (Cent1 != 90)
        {
          plot_corr_factors(Cent1, Cent2);
        }
      Cent1 += 10;
      Cent2 = Cent1+10;
    }
}

//_____________________________________________________________________________
//

//double emcAnalyzer::weighted_average( const std::vector<double> vals, const std::vector<double> errs )

double* 
emcAnalyzer::weighted_average( const double *vals, const double *errs, const int N )
{

  const int M = 4;
  double *av_err_chi2 = new double[M];

  for (int i=0 ; i<M; i++) { av_err_chi2[i] = -999.; }

  if (!vals || !errs || !N) return av_err_chi2;

  double w_average = 0., norm_average = 0. , err_average = 0., norm_err_average = 0., chi2 = 0. ;

  for (int i=0 ; i<N ; i++)
    {
      err_average += 1./(errs[i]*errs[i]);
      w_average += vals[i]/(errs[i]*errs[i]);
      norm_average += vals[i];
      norm_err_average += errs[i]*errs[i];
    }

  w_average /= err_average;
  norm_average /= N;
  err_average = 1./sqrt(err_average);
  norm_err_average = sqrt(norm_err_average);

  for (int i=0 ; i<N ; i++)
    {
      chi2 += pow( w_average - vals[i] , 2. )/(errs[i]*errs[i]);
    }

  av_err_chi2[0] = w_average;
  av_err_chi2[1] = err_average;
  av_err_chi2[2] = norm_average;
  av_err_chi2[3] = chi2/(N-1);

  cout << " Weighted average = " << av_err_chi2[0] << " +/- " << av_err_chi2[1] 
       << " ---   chi2/(N-1) = " << av_err_chi2[3]
       << " --- Std. average = " << av_err_chi2[2] << " +/- " << norm_err_average << endl;

  return av_err_chi2;

}

//_____________________________________________________________________________
//

double emcAnalyzer::chi2( TF1 *ref, TGraphErrors *data )
{

  double chiSq = -999.;

  if (!ref || !data) return chiSq;

  double x, y, ey, fy;

  size_t N = data->GetN();

  for (size_t ii = 0; ii < N; ii++)
    {
      data->GetPoint(ii, x, y);
      ey = data->GetErrorY(ii);
      fy = ref->Eval(x);
      chiSq += (fy - y)*(fy - y)/(ey*ey);
    }

  chiSq /= N;

  return chiSq;
}

double emcAnalyzer::chi2( TF1 *ref, TGraphErrors*data, double start )
{

  double chiSq = -999.;

  if (!ref || !data) return chiSq;

  double x, y, ey, fy;

  size_t N = data->GetN();

  for (size_t ii = 0; ii < N; ii++)
    {
      data->GetPoint(ii, x, y);
      ey = data->GetErrorY(ii);
      fy = ref->Eval(x);
      if (x > start)
        chiSq += (fy - y)*(fy - y)/(ey*ey);
    }

  chiSq /= N;

  return chiSq;
}

//_____________________________________________________________________________
//
void
emcAnalyzer::saveCanvas( TCanvas *c1 )
{

  if (!c1) return ;

  char output[300];
  char *title = (char*)c1->GetTitle();

  if (fSaveGifs)
    {
      sprintf(output, "%s.gif", title);
      c1->SaveAs(output);
    }
  if (fSaveEps)
    {
      sprintf(output, "%s.eps", title);
      c1->SaveAs(output);
    }
}

//_____________________________________________________________________________
//
void
emcAnalyzer::dumpLaTeX( TGraphErrors *g1,
                        double *eStat, double *eSyst,
                        double *eCCuncorr, double *epTcorr)
{
  if (!g1) return ;

  char *title = (char*)g1->GetTitle();
  char *space2 = "\\hspace{2mm}";
  char *space5 = "\\hspace{5mm}";
  char *space9 = "\\hspace{9mm}";

  bool AddOneError = false; // for eNcoll case
  bool fullErrors = false;

  printf("\n\\begin{table}[ht] \n");
  printf("\\begin{center} \n");

  // 2 loops: one for absolute errors, the second for relative

  for (int loop = 1; loop <= 2; loop++)
    {
      if (eStat && eSyst && eCCuncorr && epTcorr)
        {
          fullErrors = true;

          if (loop == 1)
            {
              printf("\\begin{tabular}{|c|c|c|c|c|c|c|}\\hline\\hline \n");
              printf("%s $p_T$ (GeV/c) %s & %s $N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}_{stat.}$ %s & %s $\\Delta N^{\\pi^0}_{sys.}$ %s & %s $\\Delta N^{\\pi^0}_{CC-uncorr.}$ %s & %s $\\Delta N^{\\pi^0}_{p_T-corr.}$ %s \\\\\\hline \n"
                     , space2, space2, space2, space2, space2, space2, space2, space2, space2, space2, space2, space2, space2, space2);

              // simpler errors output
              //       printf("%s $p_T$ (GeV/c) %s & %s $N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}_{stat.}$ %s & %s $\\Delta N^{\\pi^0}_{sys.}$ %s & %s $\\Delta N^{\\pi^0}_{CC-uncorr.}$ %s & %s $\\Delta N^{\\pi^0}_{p_T-corr.}$ %s \\\\\\hline \n"
              // 	     ,space5,space5,space5,space5,space9,space9,space5,space5,space5,space5,space5,space5,space5,space5);
            }
          else if (loop == 2)
            {
              printf("\\hline \n");
              printf("%s $p_T$ (GeV/c) %s & %s $N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}_{stat.}/N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}_{sys.}/N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}_{CC-uncorr.}/N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}_{p_T-corr.}/N^{\\pi^0}$ %s \\\\\\hline \n"
                     , space2, space2, space2, space2, space2, space2, space2, space2, space2, space2, space2, space2, space2, space2);
            }

        }

      else if (eStat) // for eNcoll case
        {
          AddOneError = true;

          if (loop == 1)
            {
              printf("\\begin{tabular}{|c|c|c|c|c|c|}\\hline\\hline \n");
              printf("%s $p_T$ (GeV/c) %s & %s $R_{AA}$ %s  & %s $\\Delta R_{AA}$ %s  & %s $\\Delta R_{AA}/R_{AA}$ %s & %s $\\Delta N_{norm}$ %s & %s $\\Delta N_{norm}/R_{AA}$ %s \\\\\\hline \n"
                     , space5, space5, space5, space5, space5, space5, space5, space5, space5, space5, space5, space5);
            }
        }

      else // 2 errors only
        {
          if (loop == 1)
            {
              printf("\\begin{tabular}{|c|c|c|}\\hline\\hline \n");
              printf(" %s $p_T$ (GeV/c) %s & %s $N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}$ %s \\\\\\hline \n"
                     , space5, space5, space5, space5, space9, space9);
            }
        }

      // loop on the error index
      for (int i = 0; i < g1->GetN(); i++)
        {
          double x = 0.;
          double y = 0.;
          g1->GetPoint(i, x, y);
          double ex = g1->GetErrorX(i);
          double ey = g1->GetErrorY(i);
          double eyrelat = ey/y*100.;

          if (fullErrors)
            {
              // simpler error output
              //printf("  %.2f $\\pm$  %.2f  &  %.3e & %.3e (%.1f\\%%) & %.3e & %.3e & %.3e & %.3e \\\\ \n",
              //	 x,ex,y,ey,eyrelat,eStat[i],eSyst[i],eCCuncorr[i],epTcorr[i]);

              if (loop == 1)
                {
                  printf("  %.2f $\\pm$  %.2f  &  %.3e & %.3e & %.3e & %.3e & %.3e & %.3e \\\\ \n",
                         x, ex, y, ey, eStat[i], eSyst[i], eCCuncorr[i], epTcorr[i]);
                }
              else if (loop == 2)
                {
                  printf("  %.2f $\\pm$  %.2f  &  %.3e & %.1f\\%% & %.1f\\%% & %.1f\\%% & %.1f\\%% & %.1f\\%% \\\\ \n",
                         x, ex, y, eyrelat, eStat[i]/y*100., eSyst[i]/y*100., eCCuncorr[i]/y*100., epTcorr[i]/y*100.);
                }
            }
          else if (AddOneError && (loop == 1) )
            {
              printf("  %.2f $\\pm$  %.2f  &  %.3f & %.3f & %.1f\\%% & %.3f  & %.1f\\%% \\\\ \n",
                     x, ex, y, ey, eyrelat, eStat[i], eStat[i]/y*100.);
            }
          else if (loop == 1)
            {
              printf("  %.2f $\\pm$  %.2f  &  %.3e & %.3e (%.1f\\%%) \\\\ \n",
                     x, ex, y, ey, eyrelat);
            }
        }
    }

  printf("\\hline\\hline \n");
  //  printf("\\vspace{5mm} \n");
  printf("\\end{tabular} \n");
  printf("\\caption{$%s$.}\n", title);
  printf("\\label{tab:%s_yields} \n", title);
  printf("\\end{center} \n");
  printf("\\end{table} \n");

}

//_____________________________________________________________________________
//
void
emcAnalyzer::dumpAscii( TGraphErrors *g1,
                        double *eStat, double *eSyst,
                        double *eCCuncorr, double *epTcorr)
{
  if (!g1) return ;

  bool AddOneError = false; // for eNcoll case
  bool fullErrors = false;

  // 2 loops: one for absolute errors, the second for relative

  for (int loop = 1; loop <= 2; loop++)
    {
      if (eStat && eSyst && eCCuncorr && epTcorr)
        {
          fullErrors = true;

          if (loop == 1)
            {
              printf("   pT    epT        Npi0       eNpi0    eNpi0_stat    eNpi0_sys    eNpi0_ccuncorr    eNpi0_pTcorr.\n");
            }
          else if (loop == 2)
            {
              printf(" \n\n");
              printf("   pT    epT        Npi0       eNpi0%%    eNpi0_stat%%    eNpi0_sys%%   eNpi0_ccuncorr%%   eNpi0_pTcorr%%\n");
            }

        }

      else if (eStat) // for eNcoll case
        {
          AddOneError = true;

          if (loop == 1)
            {
              //printf(" \n");
              printf("   pT    epT    R_AA     eR_AA    eR_AA(%%)    eNcoll    eNcoll(%%)\n");
            }
        }

      else // 2 errors only
        {
          if (loop == 1)
            {
              printf(" \n");
              printf("   p_T    epT     Npi0    eNpi0  \n");
            }
        }

      // loop on the error index
      for (int i = 0; i < g1->GetN(); i++)
        {
          double x = 0.;
          double y = 0.;
          g1->GetPoint(i, x, y);
          double ex = g1->GetErrorX(i);
          double ey = g1->GetErrorY(i);
          double eyrelat = ey/y*100.;

          if (fullErrors)
            {
              if (loop == 1)
                {
                  printf("  %.2f   %.2f      %.3e    %.3e    %.3e    %.3e    %.3e    %.3e  \n",
                         x, ex, y, ey, eStat[i], eSyst[i], eCCuncorr[i], epTcorr[i]);
                }
              else if (loop == 2)
                {
                  printf("  %.2f   %.2f      %.3e    %.2f    %.2f    %.2f    %.2f    %.2f  \n",
                         x, ex, y, eyrelat, eStat[i]/y*100., eSyst[i]/y*100., eCCuncorr[i]/y*100., epTcorr[i]/y*100.);
                }
            }
          else if (AddOneError && (loop == 1) )
            {
              printf("  %.2f   %.2f      %.3f    %.3f    %.2f    %.3f    %.2f  \n",
                     x, ex, y, ey, eyrelat, eStat[i], eStat[i]/y*100.);
            }
          else if (loop == 1)
            {
              printf("  %.2f   %.2f      %.3e    %.3e    %.2f  \n",
                     x, ex, y, ey, eyrelat);
            }
        }
    }

  cout << endl;

}

//_____________________________________________________________________________
//
// K.Reygers R_AA plot from Hirschegg: It covers pT = 0.63 - 3.1 GeV/c
//                                     Rebinned by a factor 2 in pT = 2.0 - 3.1 GeV/c
//                                     Normalization error bands: +/- 32% (Ncoll + pp-param)
//

TGraphErrors*
emcAnalyzer::plot_klaus_pi0_WA98_RAA( TCanvas *c10 )
{

  char file[200];
  sprintf(file, "%s/wa98_tables/read_for_graphs/wa98_pi0_et09_bin200MeV_above_2GeV_klaus.dat", fDataTablesPath->Data());

  // pp param. at cms-energies of 17 GeV

  TF1* fpp17 = new TF1("fpp17", "[0]*pow([1]/(x+[1]),[2])", 0.5, 4.);
  fpp17->SetParameters(4.125, 9.02, 55.77);

  // number of coll. in central Pb+Pb WA98
  double nccen_wa98 = 651.;

  //
  // get WA98 central pi0 spectrum
  //

  const int nWa98 = 35; // total number of data points

  // spectrum
  double xWa98[nWa98];
  double yWa98[nWa98];
  double xeWa98[nWa98];
  double yeWa98[nWa98];

  // R_AA
  double xnWa98[nWa98];
  double ynWa98[nWa98];
  double xneWa98[nWa98];
  double yneWa98[nWa98];

  cout << " <I> reading " << file << endl;

  ifstream fWa98(file);
  if (!fWa98)
    {
      cout << " <E> Can't open file: " << file << endl;
      return 0;
    }

  for (int i = 0; i < nWa98; i++)
    {
      fWa98 >> xWa98[i] >> yWa98[i] >> xeWa98[i] >> yeWa98[i];

      xnWa98[i] = xWa98[i];
      xneWa98[i] = xeWa98[i];

      ynWa98[i] = yWa98[i]/(nccen_wa98*fpp17->Eval(xWa98[i], 0., 0.));
      yneWa98[i] = yeWa98[i]/(nccen_wa98*fpp17->Eval(xWa98[i], 0., 0.));

      yeWa98[i] = ynWa98[i]*0.32; // fixed 32% abs. normalization error
    }
  fWa98.close();

  // create WA98 central pi0 graph (reduced number of points)

  int nWa98Min = 6;  // pTmin = 0.6 GeV/c
  int nWa98Max = 25; // pTmax = 3.1 GeV/c
  int npWa98 = nWa98Max - nWa98Min + 1; // 15 points kept only

  TGraphErrors* gWa98Nc = new TGraphErrors(npWa98,
                          &xnWa98[nWa98Min], &ynWa98[nWa98Min],
                          &xneWa98[nWa98Min], &yneWa98[nWa98Min]);

  TGraphErrors* gWa98Nc2 = new TGraphErrors(npWa98,
                           &xnWa98[nWa98Min], &ynWa98[nWa98Min],
                           &xneWa98[nWa98Min], &yeWa98[nWa98Min]);

  TH2F* myframe = 0;

  if (!c10)
    {

      c10 = (TCanvas*)canvas("klaus_wa98_RAA", 700, 600);
      myframe = (TH2F*)frame("klaus_wa98_RAA", 2, 0., 4.5, 2, 0., 3.2, "p_{T} (GeV/#font[72]{c})", "R_{AA}");
      myframe->Draw();
    }

  c10->cd();

  gWa98Nc->SetMarkerStyle(21);
  gWa98Nc->Draw("P");
  gWa98Nc2->SetLineColor(2);
  //gWa98Nc2->Draw("P");

  // WA98 errors bars (Ncoll + pp-param uncertainty)
  // draw sys. errors as boxes for WA98 data points

  for (int i = 0; i < npWa98; i++)
    {

      double v = 0.02;
      double h = 0.05;

      TLine* lu = new TLine(xnWa98[i + nWa98Min] - h, 1.32 * ynWa98[i + nWa98Min],
                            xnWa98[i + nWa98Min] + h, 1.32 * ynWa98[i + nWa98Min]);
      TLine* lul = new TLine(xnWa98[i + nWa98Min] - h, 1.32 * ynWa98[i + nWa98Min] - v,
                             xnWa98[i + nWa98Min] - h, 1.32 * ynWa98[i + nWa98Min]);
      TLine* lur = new TLine(xnWa98[i + nWa98Min] + h, 1.32 * ynWa98[i + nWa98Min] - v,
                             xnWa98[i + nWa98Min] + h, 1.32 * ynWa98[i + nWa98Min]);

      TLine* ll = new TLine(xnWa98[i + nWa98Min] - h, 0.68 * ynWa98[i + nWa98Min],
                            xnWa98[i + nWa98Min] + h, 0.68 * ynWa98[i + nWa98Min]);
      TLine* lll = new TLine(xnWa98[i + nWa98Min] - h, 0.68 * ynWa98[i + nWa98Min] + v,
                             xnWa98[i + nWa98Min] - h, 0.68 * ynWa98[i + nWa98Min]);
      TLine* llr = new TLine(xnWa98[i + nWa98Min] + h, 0.68 * ynWa98[i + nWa98Min] + v,
                             xnWa98[i + nWa98Min] + h, 0.68 * ynWa98[i + nWa98Min]);

      c10->cd();
      lu->SetLineColor(9);
      lul->SetLineColor(9);
      lur->SetLineColor(9);
      ll->SetLineColor(9);
      lll->SetLineColor(9);
      llr->SetLineColor(9);

      lu->SetLineWidth(2);
      lul->SetLineWidth(2);
      lur->SetLineWidth(2);
      ll->SetLineWidth(2);
      lll->SetLineWidth(2);
      llr->SetLineWidth(2);

      lu->Draw();
      lul->Draw();
      lur->Draw();
      ll->Draw();
      lll->Draw();
      llr->Draw();
    }

  return gWa98Nc;
}

//_____________________________________________________________________________

TGraphErrors*
emcAnalyzer::get_pi0_WA98_RAA()
{

  // WA98 pi0 spectrum
  fPlot = false;
  fFit = false;
  TGraphErrors *errBand;

  TGraphErrors *RAA_SPS = (TGraphErrors*)plot_spectrum_from_ascii("wa98_pi0_cent.dat", (TGraphErrors*&)errBand);

  // pp param. at cms-energies of 17 GeV

  TF1* fpp17 = new TF1("fpp17", "[0]*pow([1]/(x+[1]),[2])", 0.5, 4.);
  fpp17->SetParameters(4.125, 9.02, 55.77);
  double efpp17 = 0.20; // 20% error of p+p --> pi0 ref.

  double ncoll_pbpb = 651.;

  double pT = 0.;
  double epT = 0.;
  double ey = 0.;
  double y = 0.;

  double RAA = 0.;
  double eRAA = 0.;

  int N = RAA_SPS->GetN();
  for (int i = 0;i < N;i++)
    {
      RAA_SPS->GetPoint(i, pT, y);

      epT = RAA_SPS->GetErrorX(i);
      ey = RAA_SPS->GetErrorY(i);

      RAA = y/(ncoll_pbpb*fpp17->Eval(pT));
      eRAA = ey/(ncoll_pbpb*fpp17->Eval(pT));
      eRAA = quadRelatError(eRAA, RAA, efpp17, 1.); // p+p --> pi0 ref. error

      RAA_SPS->SetPoint(i, pT, RAA);
      RAA_SPS->SetPointError(i, epT, eRAA);
    }

  //   RAA_SPS->Print();
  //   // "cleaner" (reduced) version of WA98 R_AA

  //   for(int i=0;i<N;i++)
  //     {
  //       RAA_SPS->GetPoint(i,pT,y);

  //       if ( (pT < 1) || (i%2) )
  // 	{
  // 	  cout << "Removing point " << i << " pT=" << pT << " - yield=" << y << endl;
  // 	  RAA_SPS->RemovePoint(i);
  // 	}
  //     }

  //   RAA_SPS->Print();

  return RAA_SPS;

}


//_____________________________________________________________________________
//

TCanvas* emcAnalyzer::canvas(const char *title, const int sizex, const int sizey)

{

  TCanvas *c1 = new TCanvas(title, title, sizex, sizey);
  c1->Range( -0.48, -8.37, 8.0, 0.21);
  c1->SetFillColor(0);
  c1->SetBorderMode(0);
  c1->SetBorderSize(0);
  c1->SetLeftMargin(0.12);
  c1->SetRightMargin(0.010);
  c1->SetTopMargin(0.02);
  c1->SetFrameBorderMode(0);
  c1->SetFrameBorderMode(0);

  c1->Range( -0.9, -0.25, 5.7, 1.6);
  c1->SetLeftMargin(0.137);
  c1->SetRightMargin(0.0273);
  c1->SetTopMargin(0.0562);
  c1->SetBottomMargin(0.134);

  //fCanvasList->Add(c1);

  return c1;

}

//_____________________________________________________________________________
//

TH2F* emcAnalyzer::frame(const char *title,
                         const int xbins, const double xmin, const double xmax,
                         const int ybins, const double ymin, const double ymax,
                         const char *xtitle, const char *ytitle)
{

  TH2F *myframe = new TH2F(title, title, xbins, xmin, xmax, ybins, ymin, ymax);

  myframe->SetStats(0);
  myframe->SetTitle(title);
  myframe->SetXTitle(xtitle);
  myframe->SetYTitle(ytitle);

  myframe->GetXaxis()->SetTitleSize(0.05);
  myframe->GetXaxis()->SetTitleOffset(1.);
  myframe->GetXaxis()->SetLabelSize(0.05);

  myframe->GetYaxis()->SetTitleSize(0.05);
  myframe->GetYaxis()->SetTitleOffset(1.);
  myframe->GetYaxis()->SetLabelSize(0.05);

  myframe->Draw();

  return myframe;
}


// emcAnalyzer::plot( TGraphErrors* g1, TGraphErrors* g2,
// 		   const char* basename,
// 		   const char* method)
// {
//   double xmax = 0;
//   double xmin = 1e30;
//   double ymax = 0;
//   double ymin = 1e30;

//   double npts = g1->GetN();

//   if ( g2->GetN() < npts )
//     {
//       npts = g2->GetN();
//     }

//   bool samepts = true;

//   for ( int i = 0; i < npts; i++ )
//     {
//       if ( g1->GetX()[i] != g2->GetX()[i] )
// 	{
// 	  samepts=false;
// 	}

//       if ( g1->GetX()[i] > xmax )
// 	{
// 	  xmax = g1->GetX()[i];
// 	}
//       if ( g2->GetX()[i] > xmax )
// 	{
// 	  xmax = g2->GetX()[i];
// 	}
//        if ( g1->GetX()[i] < xmin )
// 	{
// 	  xmin = g1->GetX()[i];
// 	}
//       if ( g2->GetX()[i] < xmin )
// 	{
// 	  xmin = g2->GetX()[i];
// 	}

//       if ( g1->GetY()[i] > ymax )
// 	{
// 	  ymax = g1->GetY()[i];
// 	}
//       if ( g2->GetY()[i] > ymax )
// 	{
// 	  ymax = g2->GetY()[i];
// 	}
//        if ( g1->GetY()[i] < ymin )
// 	{
// 	  ymin = g1->GetY()[i];
// 	}
//       if ( g2->GetY()[i] < ymin )
// 	{
// 	  ymin = g2->GetY()[i];
// 	}
//     }

//   char name[200];

//   sprintf(name,"%s_%s",method,basename);

//   TCanvas* c1;

//   fCanvasList->Add(c1 = new TCanvas(name,name));

//   c1->cd();

//   TPad* ptitle = new TPad("ptitle","ptitle",0.01,0.885,0.99,0.98);

//   TPad* p1 = new TPad("1","1",0.01,0.01,0.499,0.88);
//   p1->SetLeftMargin(0.2);
//   p1->SetRightMargin(0.01);

//   TPad* p2 = new TPad("2","2",0.50,0.01,0.999,0.88);
//   p2->SetLeftMargin(0.2);
//   p2->SetRightMargin(0.01);

//   ptitle->Draw();
//   p1->Draw();
//   p2->Draw();

//   // ----- Make the title

//   ptitle->cd();

//   std::string sname;

//   if ( !fPlotTitle )
//     {
//       sname = basename;
//       size_t i = sname.find_first_of('_')+1;
//       if ( i < 0 ) i=0;

//       sname = sname.substr(i);
//     }
//   else
//     {
//       sname = fPlotTitle->Data();
//     }

//   TPaveText* text = new TPaveText(0,0,1,1);
//   text->SetTextAlign(22);
//   text->SetTextSize(0.5);
//   text->SetBorderSize(0);
//   text->SetFillColor(0);
//   text->AddText(sname.c_str());
//   text->Draw();

//   // ----- The two single spectra

//   TH2* hmyframe = (TH2F*)frame("hmyframe",10,0,xmax*1.2,10,ymin*0.1,ymax*10);

//   // TH2* hmyframe = (TH2F*)frame("hmyframe",10,0,10,10,0,1e+6);
//   p1->cd();
//   p1->SetLogy();
//   hmyframe->Draw();
//   hmyframe->GetYaxis()->SetTitle("#frac{dN^{#pi^{0}}}{2#piNevents p_{ T}dp_{T}dy}");
//   g1->SetMarkerStyle(20);
//   g2->SetMarkerStyle(25);

//   g1->SetMarkerColor(2);
//   g1->SetLineColor(2);

//   g1->Draw("P");

//   g2->Draw("P");

//   if ( fLegend1 )
//     {
//       TLegend* legend = new TLegend(0.25,0.14,0.66,0.3);
//       legend->SetBorderSize(1);
//       legend->SetFillColor(0);
//       legend->AddEntry(g1,fLegend1->Data(),"p");
//       legend->AddEntry(g2,fLegend2->Data(),"p");
//       legend->Draw();
//     }

//   // ---- Ratio of two spectra

//   p2->cd();

//   if ( !samepts )
//     {
//       TF1* f1 = g1->GetFunction("hagedorn2");
//       TF1* f2 = g2->GetFunction("hagedorn2");

//       if ( f1 && f2 )
// 	{
// 	  TH1* hdiv = new TH1F("hdiv","hdiv",50,0,f2->GetXmax());

// 	  for ( int i = 1; i <= hdiv->GetXaxis()->GetNbins(); ++i )
// 	    {
// 	      double pt = hdiv->GetXaxis()->GetBinCenter(i);
// 	      hdiv->SetBinContent(i, f1->Eval(pt)/f2->Eval(pt));
// 	    }
// 	  hdiv->Draw("hist");
// 	}
//       else
// 	{
// 	  std::cerr << "Could not find hagedorn2" << endl;
// 	}
//     }
//   else
//     {
//       int N = g1->GetN();

//       TGraphErrors* gdiv = new TGraphErrors(N);

//       for ( int i = 0; i < N; i++ )
// 	{
// 	  double x1,y1,x1err,y1err;
// 	  double x2,y2,y2err;
// 	  g1->GetPoint(i,x1,y1);
// 	  g2->GetPoint(i,x2,y2);
// 	  double r = y1/y2;
// 	  gdiv->SetPoint(i,x1,y1/y2);
// 	  x1err = g1->GetErrorX(i);
// 	  y1err = g1->GetErrorY(i);
// 	  y2err = g2->GetErrorY(i);
// 	  gdiv->SetPointError(i,x1err,r*quadraticRelativeError
// 			      (y1,y1err,y2,y2err));
// 	}

//       TH2* h = new TH2F("h","h",10,0,gdiv->GetX()[N-1],10,0,2);

//       h->GetXaxis()->SetTitle("p_{T} (GeV/c");
//       h->GetYaxis()->SetTitle("Ratio");
//       h->GetYaxis()->SetTitleOffset(1.3);
//       h->GetYaxis()->SetTitleSize(0.06);

//       h->Draw();

//       gdiv->SetMarkerStyle(24);

//       gdiv->Draw("P");
//     }

//   sprintf(name,"%s.eps",c1->GetName());
//   c1->Print(name);
// }

// //_____________________________________________________________________________
// void
// emcAnalyzer::plot( TH1* h1, TH1* h2,
// 		   const char* basename, const char* method,
// 		   bool limits, bool fit)
// {
//   TString stitle = h1->GetName();

//   double maximum = h1->GetMaximum();
//   double minimum = h1->GetMinimum();

//   if ( h2->GetMaximum() > maximum )
//     {
//       maximum = h2->GetMaximum();
//     }

//   if ( h2->GetMinimum() < minimum )
//     {
//       minimum = h2->GetMinimum();
//     }

//   maximum*=1.2;
//   //  minimum*=0.8;

//   h1->SetMaximum(maximum);
//   h2->SetMaximum(maximum);
//   h1->SetMinimum(minimum);
//   h2->SetMinimum(minimum);

//   char name[200];

//   sprintf(name,"%s_%s",method,basename);

//   TCanvas* c1;

//   fCanvasList->Add(c1 = new TCanvas(name,name));

//   c1->cd();

//   TPad* ptitle = new TPad("ptitle","ptitle",0.01,0.885,0.99,0.98);

//   TPad* p1 = new TPad("1","1",0.01,0.01,0.499,0.88);
//   p1->SetLeftMargin(0.15);
//   p1->SetRightMargin(0.01);

//   TPad* p2 = new TPad("2","2",0.50,0.01,0.999,0.88);
//   p2->SetLeftMargin(0.15);
//   p2->SetRightMargin(0.01);

//   ptitle->Draw();
//   p1->Draw();
//   p2->Draw();

//   // ----- Make the title

//   ptitle->cd();

//   std::string sname;

//   if ( !fPlotTitle )
//     {
//       sname = basename;
//       size_t i = sname.find_first_of('_')+1;
//       if ( i < 0 ) i=0;

//       sname = sname.substr(i);
//     }
//   else
//     {
//       sname = fPlotTitle->Data();
//     }

//   TPaveText* text = new TPaveText(0,0,1,1);
//   text->SetTextAlign(22);
//   text->SetTextSize(0.5);
//   text->SetBorderSize(0);
//   text->SetFillColor(0);
//   text->AddText(sname.c_str());
//   text->Draw();

//   // ----- The two single spectra

//   h1->SetMarkerStyle(20);
//   h1->SetMarkerColor(1);

//   sprintf(name,basename);
//   h1->SetTitle(name);
//   h1->GetXaxis()->SetTitle("p_{T} (GeV/c)");
//   int b1=h1->GetXaxis()->FindBin(1.0);
//   int b2=h1->GetXaxis()->FindBin(7.0);

//   h1->GetXaxis()->SetRange(b1,b2);

//   if ( stitle.Contains("hEfficiency") )
//     {
//       h1->GetYaxis()->SetTitle("#epsilon");
//     }
//   else if ( stitle.Contains("Peak") )
//     {
//       h1->GetYaxis()->SetTitle("#pi^{0} peak position (GeV/c^{2})");
//       h1->SetMaximum(0.160);
//       h1->SetMinimum(0.120);
//     }
//   else if ( stitle.Contains("Sigma") )
//     {
//       h1->GetYaxis()->SetTitle("#pi^{0} peak #sigma (GeV/c^{2})");
//       h1->SetMaximum(0.030);
//       h1->SetMinimum(0.000);
//      }

//   h1->GetYaxis()->SetTitleOffset(1.0);
//   h1->GetYaxis()->SetTitleSize(0.06);
//   h2->GetYaxis()->SetTitleOffset(0.5);
//   h2->GetYaxis()->SetTitleSize(0.06);

//   h1->GetXaxis()->SetTitleSize(0.04);
//   h2->GetXaxis()->SetTitleSize(0.04);

//   p1->cd();

//   p1->SetLogy(fLogy);

//   h1->Draw("P");
//   h2->SetMarkerStyle(25);
//   h2->SetMarkerColor(1);
//   h2->SetLineColor(1);
//   h2->Draw("PSAME");

//   if ( fLegend1 )
//     {
//       TLegend* legend = new TLegend(0.25,0.14,0.66,0.3);
//       legend->SetBorderSize(1);
//       legend->SetFillColor(0);
//       legend->AddEntry(h1,fLegend1->Data(),"p");
//       legend->AddEntry(h2,fLegend2->Data(),"p");
//       legend->Draw();
//     }

//   // ---- Ratio of two spectra

//   TH1* hdiv = (TH1*)h1->Clone();
//   hdiv->SetMarkerColor(1);
//   hdiv->SetMarkerStyle(27);
//   hdiv->SetTitle("");
//   hdiv->GetYaxis()->SetTitle("Ratio");
//   p2->cd();
//   hdiv->Divide(h2);

//   minmax(hdiv,minimum,maximum,1,7);

//   //  if ( maximum>2 ) maximum=2.0;

//   maximum *= 1.2;

//   if ( minimum > 1 ) minimum=1.0;

//   if ( minimum < 0 ) minimum=0.0;

//   minimum *= 0.8;
//   hdiv->SetMaximum(maximum);
//   hdiv->SetMinimum(minimum);
//   hdiv->Draw("histe");

//   if ( fFit )
//     {
//       hdiv->Fit(fFit->Data(),"QR","",1,7);
//     }

//   if (limits)
//     {
//       minmax(hdiv,minimum,maximum,1,5);

//       TLine* lmin = new TLine(1,minimum,5,minimum);
//       TLine* lmax = new TLine(1,maximum,5,maximum);
//       lmin->Draw();
//       lmax->Draw();
//     }

//   sprintf(name,"%s.eps",c1->GetName());
//   c1->Print(name);
// }

