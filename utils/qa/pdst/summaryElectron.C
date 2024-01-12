#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "summaryQA.h"

#include <fstream>
#include <iostream>
#include <cmath>

TH1F* getNetElectronHisto(TH1D* elhisto, TH1D* selhisto);

using namespace std;

int QASummary::processElectron(const float min_mom, const float max_mom)
{
  TH2 *elcut = (TH2F*) qafile->Get("elcut");
  TH2 *eldep = (TH2F*) qafile->Get("eldep");
  TH2 *elsdep = (TH2F*) qafile->Get("elsdep");
  TH2 *eln0 = (TH2F*) qafile->Get("eln0");
  TH2 *elsn0 = (TH2F*) qafile->Get("elsn0");
  TH2 *elchi2npe0 = (TH2F*) qafile->Get("elchi2npe0");
  TH2 *elschi2snpe0 = (TH2F*) qafile->Get("elschi2snpe0");
  //  TH3 *elemcmatch = (TH3F*) qafile->Get("elemcmatch");
  TH3 *elemcmatch_e = (TH3F*) qafile->Get("elemcmatch_e");
  TH3 *elemcmatch_se = (TH3F*) qafile->Get("elemcmatch_se");
  //  TH3 *elacceptance = (TH3F*) qafile->Get("elacceptance");
  //  TH3 *elacceptance_X1orX2 = (TH3F*) qafile->Get("elacceptance_X1orX2");
  TH2 *elacceptance_e = (TH2F*) qafile->Get("elacceptance_e");
  TH2 *elacceptance_se = (TH2F*) qafile->Get("elacceptance_se");
  TH2 *eldisp = (TH2F*) qafile->Get("eldisp");
  TH2 *elsdisp = (TH2F*) qafile->Get("elsdisp");
  TH2 *elricht0 = (TH2F*) qafile->Get("elricht0");
  TH2 *elemct0 = (TH2F*) qafile->Get("elemct0");
  TH2 *elsricht0 = (TH2F*) qafile->Get("elsricht0");
  TH2 *elsemct0 = (TH2F*) qafile->Get("elsemct0");

  if (!elcut)
    return -1; // file has no histograms;
  char parname_upd[50];
  int Nevents = (int)elcut->ProjectionX()->GetBinContent(3);  // with |Z vertex|<30
  int NGoodTracks = (int)elcut->ProjectionX()->GetBinContent(6);

  //------------------------------  Dep  and NElectrons ----------------------------------------------------
  int min_bin = eldep->ProjectionY()->FindBin(min_mom);
  int max_bin = eldep->ProjectionY()->FindBin(max_mom); // momentum binning is the same for all histos

  TH1F* eldep_net = getNetElectronHisto(eldep->ProjectionX("eldep_x", min_bin, max_bin),
                                        elsdep->ProjectionX("elsdep_x", min_bin, max_bin));

  TF1* l = new TF1("l", "landau", -5, 5); // couting electrons fitting peak around dep=0 above background
  TF1* g = new TF1("g", "gaus", -2, 2);
  TF1 *lg = new TF1("lg", "landau(0)+gaus(3)", -5, 5);
  elsdep->ProjectionX("elsdep_x", min_bin, max_bin)->Fit(l, "RQN");
  eldep_net->Fit(g, "RQN");
  lg->SetParameters(l->GetParameter(0),
                    l->GetParameter(1),
                    l->GetParameter(2),
                    g->GetParameter(0),
                    g->GetParameter(1),
                    g->GetParameter(2));
  eldep_net->Fit(lg, "RQN");
  Double_t fitparams[6];
  lg->GetParameters(fitparams);
  lg->SetParameter(0, 0);  // subtract the remaining landau contribution
  lg->SetParameter(1, 0);
  lg->SetParameter(2, 0);
  float dep_mean = fitparams[4];
  float entries = (float)lg->Integral( -5, 5) / eldep_net->GetBinWidth(10);
  float dep_error = (float)fitparams[5];
  delete l;
  delete g;
  delete lg;
  if (entries > 0 && dep_mean > -2. && dep_mean < 2.)
    {
      float NElectrons = entries;
      float NElectronsError = sqrt(NElectrons);
      float electrons_event_error = sqrt(1. / NElectrons + 1. / Nevents) * NElectrons / Nevents;
      float electrons_track_error = sqrt(1. / NElectrons + 1. / NGoodTracks) * NElectrons / NGoodTracks;

      sprintf (parname_upd, "<DEP> (%.1f<mom<%.1f)", min_mom, max_mom);
      CommitToQADatabase("Electron", parname_upd, dep_mean, dep_error);
      sprintf (parname_upd, "N. Electrons (%.1f<mom<%.1f)", min_mom, max_mom);
      CommitToQADatabase("Electron", parname_upd, NElectrons, NElectronsError);
      sprintf (parname_upd, "Electrons/Event (%.1f<mom<%.1f)", min_mom, max_mom);
      CommitToQADatabase("Electron", parname_upd, NElectrons / Nevents, electrons_event_error);
      sprintf (parname_upd, "Electrons/Good track (%.1f<mom<%.1f)", min_mom, max_mom);
      CommitToQADatabase("Electron", parname_upd, NElectrons / NGoodTracks, electrons_track_error);
    }
  else    // if there is no net electrons, don't continue
    {
      cout << "No electrons in this run" << endl;
      return -1;
    }

  // ------------------------------- Electron/Positron ratio ---------------------------------------------------------------------


  //alpha > 0
  int min_bin_alpha = 0;
  int max_bin_alpha = 100;

  if (max_mom > 0)
    min_bin_alpha = elacceptance_e->ProjectionY()->FindBin(0.110674 / max_mom);
  if (min_mom > 0)
    max_bin_alpha = elacceptance_e->ProjectionY()->FindBin(0.110674 / min_mom); // find alpha bin


  TH1F *elphiacceptpos_net = getNetElectronHisto(elacceptance_e->ProjectionX("elacceptance_net_x", min_bin_alpha, max_bin_alpha),
					      elacceptance_se->ProjectionX("elacceptance_se_x", min_bin_alpha, max_bin_alpha));

  float Nentries_plus = elphiacceptpos_net->Integral();

  // alpha < 0

  if (min_mom > 0)
    min_bin_alpha = elacceptance_e->ProjectionY()->FindBin( -0.110674 / min_mom);
  if (max_mom > 0)
    max_bin_alpha = elacceptance_e->ProjectionY()->FindBin( -0.110674 / max_mom); // find alpha bin

  TH1F *elphiacceptneg_net = getNetElectronHisto(elacceptance_e->ProjectionX("elacceptance_net_x", min_bin_alpha, max_bin_alpha),
                                        elacceptance_se->ProjectionX("elacceptance_se_x", min_bin_alpha, max_bin_alpha));

  float Nentries_minus = elphiacceptneg_net->Integral();
  float plus_minus_ratio = 0;
  float sigma_plus_minus_ratio = 0;
  if (Nentries_minus > 0 && Nentries_plus > 0)
    {
      plus_minus_ratio = Nentries_plus / Nentries_minus;
      sigma_plus_minus_ratio = sqrt(1. / Nentries_plus + 1. / Nentries_minus) * plus_minus_ratio;
    }

  sprintf (parname_upd, "<alpha_positive/alpha_negative> (%.1f<mom<%.1f)", min_mom, max_mom);
  CommitToQADatabase("Electron", parname_upd, plus_minus_ratio, sigma_plus_minus_ratio);

  //---------------------------------------  East/West ratio   ----------------------------------------------

  elphiacceptpos_net->Add(elphiacceptneg_net, 1.);

  float east_west_ratio = 0.;
  float sigma_east_west_ratio = 0.;
  float west_entries = elphiacceptpos_net->Integral(1,50);
  float east_entries = elphiacceptpos_net->Integral(51,100);
  if (east_entries > 0 && west_entries > 0)
    {
      east_west_ratio = east_entries / west_entries;
      sigma_east_west_ratio = sqrt(1. / east_entries + 1. / west_entries) * east_west_ratio;
    }

  sprintf (parname_upd, "<East/West Nelectrons> (%.1f<mom<%.1f)", min_mom, max_mom);
  CommitToQADatabase("Electron", parname_upd, east_west_ratio, sigma_east_west_ratio);

  //---------------------------------------    EmCal Match     ----------------------------------------------

  TH2F* elemcphimatch_e = (TH2F*) elemcmatch_e->Project3D("xz");
  TH2F* elemcphimatch_se = (TH2F*) elemcmatch_se->Project3D("xz");
  TH1F* elemcphimatch_net = getNetElectronHisto(elemcphimatch_e->ProjectionY("elemcphi_y", min_bin, max_bin),
						elemcphimatch_se->ProjectionY("elemcsphi_y", min_bin, max_bin));

  float emcmatch_e_phi = elemcphimatch_net->GetMean();
  entries = elemcphimatch_net->Integral();

  if (entries > 0)
    {
      float emcmatch_e_phi_error = elemcphimatch_net->GetRMS();// / sqrt(entries);
      sprintf (parname_upd, "<EmCal Phi Match> (sigma) (%.1f<mom<%.1f)", min_mom, max_mom);
      CommitToQADatabase("Electron", parname_upd, emcmatch_e_phi, emcmatch_e_phi_error);
    }

  TH2F* elemczmatch_e = (TH2F*) elemcmatch_e->Project3D("yz");
  TH2F* elemczmatch_se = (TH2F*) elemcmatch_se->Project3D("yz");
  TH1F* elemczmatch_net = getNetElectronHisto(elemczmatch_e->ProjectionY("elemcz_y", min_bin, max_bin),
					      elemczmatch_se->ProjectionY("elemcsz_y", min_bin, max_bin));

  float emcmatch_e_z = elemczmatch_net->GetMean();
  entries = elemczmatch_net->Integral();

  if (entries > 0)
    {
      float emcmatch_e_z_error = elemczmatch_net->GetRMS();// / sqrt(entries);
      sprintf (parname_upd, "<EmCal Z Match> (sigma) (%.1f<mom<%.1f)", min_mom, max_mom);
      CommitToQADatabase("Electron", parname_upd, emcmatch_e_z, emcmatch_e_z_error);
    }
  //------------------------------  E/P-1  -------------------------------------------------------------------
  char name[10], title[30];
  for (int isect = 0; isect < 8; isect++)
    {
      sprintf(name, "elep_%d", isect);
      TH2F *elep = (TH2F*) qafile->Get(name);
      sprintf(name, "elsep_%d", isect);
      TH2F *elsep = (TH2F*) qafile->Get(name);
      TH1F* elep_net = getNetElectronHisto(elep->ProjectionX("epx", min_bin, max_bin),
                                           elsep->ProjectionX("epsx", min_bin, max_bin));

      float ep_mean = elep_net->GetMean();
      float ep_error = elep_net->GetRMS();
      entries = elep_net->Integral();

      if (entries > 0 && ep_mean > elep_net->GetXaxis()->GetXmin() && ep_mean < elep_net->GetXaxis()->GetXmax())
        {
          int iarm = isect / 4;
          int sector = isect % 4;
          if (iarm == 0)
            sprintf(title, "<E/P-1> E%d (%.1f<mom<%.1f)", sector, min_mom, max_mom);
          if (iarm == 1)
            sprintf(title, "<E/P-1> W%d (%.1f<mom<%.1f)", sector, min_mom, max_mom);
          CommitToQADatabase("Electron", title, ep_mean, ep_error);
        }
    }
  //--------------------------- RICH N0 ---------------------------------------------------------------------
  TH1F* eln0_net = getNetElectronHisto(eln0->ProjectionX("eln0_x", min_bin, max_bin),
                                       elsn0->ProjectionX("elsn0_x", min_bin, max_bin));

  if (eln0_net->Integral() > 0)
    {
      float mean_n0 = eln0_net->GetMean();
      float mean_n0_error = eln0_net->GetRMS();
      sprintf (parname_upd, "<Rich N0> (%.1f<mom<%.1f)", min_mom, max_mom);
      CommitToQADatabase("Electron", parname_upd, mean_n0, mean_n0_error);
    }
  //--------------------------- chi2/npe0  ---------------------------------------------------------------------
  TH1F* elchi2npe0_net = getNetElectronHisto(elchi2npe0->ProjectionX("elchi2npe0_x", min_bin, max_bin),
					     elschi2snpe0->ProjectionX("elschi2snpe0_x", min_bin, max_bin));

  float mean_chi2npe0 = elchi2npe0_net->GetMean();
  entries = elchi2npe0_net->Integral();
  if (entries > 0)
    {
      float mean_chi2npe0_error = elchi2npe0_net->GetRMS();
      sprintf (parname_upd, "<chi2/npe0> (%.1f<mom<%.1f)", min_mom, max_mom);
      CommitToQADatabase("Electron", parname_upd, mean_chi2npe0, mean_chi2npe0_error);
    }
  //--------------------------- disp  ---------------------------------------------------------------------
  TH1F* eldisp_net = getNetElectronHisto(eldisp->ProjectionX("eldisp_x", min_bin, max_bin),
                                         elsdisp->ProjectionX("elsdisp_x", min_bin, max_bin));

  float mean_disp = eldisp_net->GetMean();
  entries = eldisp_net->Integral();

  if (entries > 0)
    {
      float mean_disp_error = eldisp_net->GetRMS();
      sprintf (parname_upd, "<RICH ring displacement> (%.1f<mom<%.1f)", min_mom, max_mom);
      CommitToQADatabase("Electron", parname_upd, mean_disp, mean_disp_error);
    }
  //--------------------------- RICH T0  ---------------------------------------------------------------------
  TH1F* elricht0_net = getNetElectronHisto(elricht0->ProjectionX("elricht0_x", min_bin, max_bin),
					   elsricht0->ProjectionX("elsricht0_x", min_bin, max_bin));

  float mean_richt0 = elricht0_net->GetMean();
  entries = elricht0_net->Integral();

  if (entries > 0)
    {
      float mean_richt0_error = elricht0_net->GetRMS();
      sprintf (parname_upd, "<RICH T0> (%.1f<mom<%.1f)", min_mom, max_mom);
      CommitToQADatabase("Electron", parname_upd, mean_richt0, mean_richt0_error);
    }
  //--------------------------- EmCal T0  ---------------------------------------------------------------------
  TH1F* elemct0_net = getNetElectronHisto(elemct0->ProjectionX("elemct0_x", min_bin, max_bin),
                                          elsemct0->ProjectionX("elsemct0_x", min_bin, max_bin));

  float mean_emct0 = elemct0_net->GetMean();
  entries = elemct0_net->Integral();

  if (entries > 0)
    {
      float mean_emct0_error = elemct0_net->GetRMS();
      sprintf (parname_upd, "<EmCal T0> (%.1f<mom<%.1f)", min_mom, max_mom);
      CommitToQADatabase("Electron", parname_upd, mean_emct0, mean_emct0_error);
    }
  //----------------------------------------------------------------------------------------------------------------------
  cout << "...done" << endl;
  return 0;
}

TH1F* getNetElectronHisto(TH1D* elhisto, TH1D* selhisto)
{
  TH1F* hret = (TH1F*) elhisto->Clone();
  if (!(elhisto->Integral() > 0))
    return hret;
  hret->Add(selhisto, -1.);
  for (int i = 1; i <= hret->GetNbinsX(); i++)
    if (hret->GetBinContent(i) < 0)
      hret->SetBinContent(i, 0.);
  return hret;
}

