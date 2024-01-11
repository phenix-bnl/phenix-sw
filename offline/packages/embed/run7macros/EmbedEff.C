#include "EmbedEff.h"
#include <TROOT.h>
#include <TStyle.h>
#include <TDirectory.h>
#include <iostream>
#include <assert.h>

using std::cout;
using std::endl;
using std::flush;

EmbedEff::EmbedEff(int ncentbins, int nmombins, 
		   std::string outfile, std::string option) :
  fNcent(ncentbins),
  fNmom(nmombins),
  fHmom(boost::extents[fNflavor][fNtrackType][ncentbins]),
  fHcen(boost::extents[fNflavor][fNtrackType][nmombins])
{
  fVerbosity = 0;
  fPC2sigmaCut = 0;
  fPC3sigmaCut = 0;
  fEvalType = 0;
  fZedCut = 0;
  fQual1 = 0;
  fQual2 = 0;

  fLetter[TYPE_G] = "G";
  fLetter[TYPE_S] = "S";
  fLetter[TYPE_R] = "R";
  fLetter[TYPE_E] = "E";

  fLabel[PION] = "pion";
  fLabel[KAON] = "kaon";
  fLabel[PROT] = "prot";
  fLabel[ALL3] = "all3";

  fCut[TYPE_S] = "";
  fCut[TYPE_R] = "";

  gROOT->SetStyle("Plain");
  gStyle->SetOptStat(0);

  if (outfile != "") {
    fShouldSave = true;
    fOutFile = new TFile(outfile.c_str(), option.c_str());
  }
  else {
    fShouldSave = false;
    fOutFile = 0;
  }
}

EmbedEff::~EmbedEff() {}

void 
EmbedEff::setEvalType(int type) { fEvalType = type; }

void 
EmbedEff::setPC2sigmaCut(float sig) { fPC2sigmaCut = sig; }

void 
EmbedEff::setPC3sigmaCut(float sig) { fPC3sigmaCut = sig; }

void 
EmbedEff::setDchZedCut(float zed) { fZedCut = zed; }

void 
EmbedEff::setDchQualCut(int qual1) { fQual1 = qual1; fQual2 = 0; }

void 
EmbedEff::setDchQual1orQual2Cuts(int q1, int q2) { fQual1 = q1; fQual2 = q2;}

int
EmbedEff::setupCentBins(int nCentBins, float centLo[], float centHi[]) 
{
  assert(nCentBins==fNcent);
  for (int i=0; i<nCentBins; i++) {
    fCentBin[i] = std::pair<float, float>(centLo[i], centHi[i]);
    if (fVerbosity > QUIET) 
      cout << "fCentBin[" << i << "]: " 
	   << fCentBin[i].first << " - " << fCentBin[i].second << endl;
  }
  return 0;
}

int
EmbedEff::setupMomBins(int nMomBins, float momLo[], float momHi[]) 
{
  assert(nMomBins==fNmom);
  for (int i=0; i<nMomBins; i++) {
    fMomBin[i] = std::pair<float, float>(momLo[i], momHi[i]);
    if (fVerbosity > QUIET) 
      cout << "fMomBin[" << i << "]: " 
	   << fMomBin[i].first << " - " << fMomBin[i].second << endl;
  }
  return 0;
}

int
EmbedEff::addNtuple(std::string fileLocation, std::string ntName, EFlavor fl)
{
  TFile* f = TFile::Open(fileLocation.c_str(), "read");
  assert(f);
  TNtuple* nt = (TNtuple*)f->Get(ntName.c_str());
  assert(nt);
  fNtup[fl] = nt;
  if (fVerbosity==FULL) fNtup[fl]->Show(0);
  return 0;
}

void
EmbedEff::setTrackCutExpression(ETrackType tt)
{
  // Evaluation type must always be set:
  assert(fEvalType > 0);
  fss.str(""); 
  fss << "type==" << fEvalType;

  // Append all other cuts here...
  if (fPC2sigmaCut > 0) fss << " && pc2sdphi" << fLetter.at(tt) << "<"  << fPC2sigmaCut;
  if (fPC2sigmaCut > 0) fss << " && pc2sdz"   << fLetter.at(tt) << "<"  << fPC2sigmaCut;
  if (fPC3sigmaCut > 0) fss << " && pc3sdphi" << fLetter.at(tt) << "<"  << fPC3sigmaCut;
  if (fPC3sigmaCut > 0) fss << " && pc3sdz"   << fLetter.at(tt) << "<"  << fPC3sigmaCut;
  if (fZedCut > 0)      fss << " && abs(zed"  << fLetter.at(tt) << ")<" << fZedCut;
  if (fQual1 > 0 && fQual2==0)
    fss << " && dctrkQual" << fLetter.at(tt) << "==" << fQual1;
  if (fQual1 > 0 && fQual2 > 0) 
    fss << " && (dctrkQual" << fLetter.at(tt) << "==" << fQual1
	<< " || dctrkQual"  << fLetter.at(tt) << "==" << fQual2 << ")";
  fCut[tt] = fss.str();

  if (fVerbosity > QUIET)
    cout << fLetter.at(tt) << " cuts: " << fCut[tt] << endl;
  return;
}

void
EmbedEff::fillMomHisto(EFlavor fl, ETrackType tt, int centbin, 
		       std::string binning) 
{
  if (fCut[tt]=="") setTrackCutExpression(tt);
  std::string var = "mom" + fLetter.at(tt);
  std::string varexp = "", hname = "", cuts = "";

  fss.str("");   // Set histo name.
  fss << "h" << var << "_cb" << centbin << "_" << fLabel.at(fl);
  hname = fss.str();

  fss.str("");   // Set first arg to TTree::Draw().
  fss << var << ">>" << hname << binning;
  varexp = fss.str();

  fss.str("");   // Append centrality cut to existing cuts (but only store locally).
  fss << fCut[tt]
      << " && bbccent>=" << fCentBin[centbin].first
      << " && bbccent<"  << fCentBin[centbin].second;
  cuts = fss.str();

  if (fVerbosity > QUIET) cout << varexp << "\n" << cuts << endl;

  // TODO: add pT weighting to model momentum resolution
  if (fl==ALL3) { // Combined species
    TH1F* h = new TH1F(hname.c_str(), "h", 10, 0, 10);
    h->SetTitle(cuts.c_str());
    int n1, n2, n3;
    std::string s1 = var + ">>"  + hname;
    std::string s2 = var + ">>+" + hname; // TODO: add weights for different species
    n1 = fNtup[PION]->Draw(s1.c_str(), cuts.c_str(), "goff");
    n2 = fNtup[KAON]->Draw(s2.c_str(), cuts.c_str(), "goff");
    n3 = fNtup[PROT]->Draw(s2.c_str(), cuts.c_str(), "goff");

    if (fVerbosity > QUIET) 
      cout << n1 << " pion, " << n2 << " kaon, " << n3 << " prot. " << endl;
  }
  else { // Individual species
  fNtup[fl]->Draw(varexp.c_str(), cuts.c_str(), "goff");
  }
  fHmom[fl][tt][centbin] = (TH1F*)gDirectory->Get(hname.c_str());
  assert(fHmom[fl][tt][centbin]);
  fHmom[fl][tt][centbin]->Sumw2();

  if (fVerbosity > QUIET) 
    cout << fHmom[fl][tt][centbin]->GetEntries() << " in "
	 << fHmom[fl][tt][centbin]->GetName() << endl;

  if (fShouldSave) {
    fOutFile->cd();
    fHmom[fl][tt][centbin]->Write();
  }
  return;
}

void
EmbedEff::fillCenHisto(EFlavor fl, ETrackType tt, int mombin, 
		       std::string binning) 
{
  if (fCut[tt]=="") setTrackCutExpression(tt);
  std::string var = "bbccent";
  std::string varexp = "", hname = "", cuts = "";

  fss.str("");  // Set histo name.
  fss << "hcen" << fLetter.at(tt) << "_mom" << mombin << "_" << fLabel.at(fl);
  hname = fss.str();

  fss.str("");   // Set first arg to TTree::Draw().
  fss << var << ">>" << hname << binning;
  varexp = fss.str();

  fss.str("");   // Append momentum cut to existing cuts (but only store locally).
  fss << fCut[tt]
      << " && mom" << fLetter.at(tt) << ">=" << fMomBin[mombin].first
      << " && mom" << fLetter.at(tt) << "<"  << fMomBin[mombin].second;
  cuts = fss.str();

  if (fVerbosity > QUIET) cout << varexp << "\n" << cuts << endl;
  
  if (fl==ALL3) { // Combined species
    TH1F* h = new TH1F(hname.c_str(), "h", 10, 0, 100);
    h->SetTitle(cuts.c_str());
    int n1, n2, n3;
    std::string s1 = var + ">>"  + hname;
    std::string s2 = var + ">>+" + hname; // TODO: add weights for different species
    n1 = fNtup[PION]->Draw(s1.c_str(), cuts.c_str(), "goff");
    n2 = fNtup[KAON]->Draw(s2.c_str(), cuts.c_str(), "goff");
    n3 = fNtup[PROT]->Draw(s2.c_str(), cuts.c_str(), "goff");

    if (fVerbosity > QUIET) 
      cout << n1 << " pion, " << n2 << " kaon, " << n3 << " prot. " << endl;
  }
  else { // Individual species
    fNtup[fl]->Draw(varexp.c_str(), cuts.c_str(), "goff");
  }
  fHcen[fl][tt][mombin] = (TH1F*)gDirectory->Get(hname.c_str());
  assert(fHcen[fl][tt][mombin]);
  fHcen[fl][tt][mombin]->Sumw2();
  
  if (fVerbosity > QUIET) 
    cout << fHcen[fl][tt][mombin]->GetEntries() << " in "
	 << fHcen[fl][tt][mombin]->GetName() << endl;

  if (fShouldSave) {
    fOutFile->cd();
    fHcen[fl][tt][mombin]->Write();
  }
  return;
}

void
EmbedEff::fillHistos() 
{
  for (int i=0; i<fNcent; i++) { // cent bin loop

    fillMomHisto(PION, TYPE_S, i);
    fillMomHisto(PION, TYPE_R, i);
    makeMomEffHisto(PION, i);

    fillMomHisto(ALL3, TYPE_S, i);
    fillMomHisto(ALL3, TYPE_R, i);
    makeMomEffHisto(ALL3, i);
  }
  for (int i=0; i<fNmom; i++) { // momentum bin loop

    fillCenHisto(PION, TYPE_S, i);
    fillCenHisto(PION, TYPE_R, i);
    makeCenEffHisto(PION, i);

    fillCenHisto(KAON, TYPE_S, i);
    fillCenHisto(KAON, TYPE_R, i);
    makeCenEffHisto(KAON, i);

    fillCenHisto(PROT, TYPE_S, i);
    fillCenHisto(PROT, TYPE_R, i);
    makeCenEffHisto(PROT, i);

    fillCenHisto(ALL3, TYPE_S, i);
    fillCenHisto(ALL3, TYPE_R, i);
    makeCenEffHisto(ALL3, i);
  }
  return;
}

// TODO: use BayesDivide to get correct error (both mom and cen)
void
EmbedEff::makeMomEffHisto(EFlavor fl, int i) // i = centbin
{
  assert(fHmom[fl][TYPE_R][i]->GetEntries());
  assert(fHmom[fl][TYPE_S][i]->GetEntries());

  fss.str("");  // Name the new histo
  fss << "hmomE_cb" << i << "_" << fLabel.at(fl);
  std::string hname = fss.str();
  
  // Clone the numerator, then E = R/S.
  fHmom[fl][TYPE_E][i] = (TH1F*)fHmom[fl][TYPE_R][i]->Clone(hname.c_str());
  assert(fHmom[fl][TYPE_E][i]->GetEntries());
  fHmom[fl][TYPE_E][i]->Divide(fHmom[fl][TYPE_R][i], fHmom[fl][TYPE_S][i]);
  fHmom[fl][TYPE_E][i]->SetLineColor(2*fl + 2);

  if (fShouldSave) {
    fOutFile->cd();
    fHmom[fl][TYPE_E][i]->Write();
  }
  return;
}

void
EmbedEff::makeCenEffHisto(EFlavor fl, int i) // i = mombin 
{
  assert(fHcen[fl][TYPE_R][i]->GetEntries());
  assert(fHcen[fl][TYPE_S][i]->GetEntries());

  fss.str("");  // Name the new histo
  fss << "hcenE_mom" << i << "_" << fLabel.at(fl);
  std::string hname = fss.str();

  // Clone the numerator, then E = R/S.
  fHcen[fl][TYPE_E][i] = (TH1F*)fHcen[fl][TYPE_R][i]->Clone(hname.c_str());
  assert(fHcen[fl][TYPE_E][i]->GetEntries());
  fHcen[fl][TYPE_E][i]->Divide(fHcen[fl][TYPE_R][i], fHcen[fl][TYPE_S][i]);
  fHcen[fl][TYPE_E][i]->SetLineColor(2*fl + 2);

  if (fShouldSave) {
    fOutFile->cd();
    fHcen[fl][TYPE_E][i]->Write();
  }
  return;
}

void
EmbedEff::drawHistos()
{
// TODO: add legend, labels

  // Draw efficiency vs. momentum
  TCanvas* cMom = new TCanvas("cMom", "cMom", 1);
  cMom->Divide(2, 2);
  for (Index i=0; i<fNcent; ++i) {
    cMom->cd(i+1);
    fHmom[PION][TYPE_E][i]->Draw("ep");
    fHmom[ALL3][TYPE_E][i]->Draw("epsame");
  }
  cMom->Print("cMom.gif");
  if (fShouldSave) {
    fOutFile->cd();
    cMom->Write();
  }
  // Draw efficiency vs. centrality
  TCanvas* cCen = new TCanvas("cCen", "cCen", 1);
  gPad->SetGridy();
  fHcen[PION][TYPE_E][0]->Draw("ep");
  fHcen[KAON][TYPE_E][0]->Draw("epsame");
  fHcen[PROT][TYPE_E][0]->Draw("epsame");
  cCen->Print("cCen.gif");

  TCanvas* cCenMix = new TCanvas("cCenMix", "cCenMix", 1);
  gPad->SetGridy();
  fHcen[ALL3][TYPE_E][0]->Draw("ep");
  cCenMix->Print("cCenMix.gif");

  if (fShouldSave) {
    fOutFile->cd();
    cCen->Write();
    cCenMix->Write();
  }
  return;
}

// TODO: build into a dyamic lib, split main() out as a driver macro.
int
main() 
{
  // Centrality bins are for efficiency vs. momentum histos.
  int nCentBins = 4;  
  float centLo[] = {0,  20, 40, 60};
  float centHi[] = {20, 40, 60, 100};

  // Momentum bins are for efficiency vs. centrality histos.
  int nMomBins = 1;  
  float momLo[] = {1.0};
  float momHi[] = {10.0};

  EmbedEff ee(nCentBins, nMomBins, "embedEff.root", "RECREATE");
  ee.setVerbosity(EmbedEff::MEDIUM); // QUIET, MEDIUM, or FULL

  // Read in Ntuples to the internal map.
  ee.addNtuple("/phenix/hp/data82/phnxhp01/andrew/emb/eval/pi/pions.root",
	       "EmbedMcRecoTrack", EmbedEff::PION);
  ee.addNtuple("/phenix/hp/data82/phnxhp01/andrew/emb/eval/k/kaons.root",
	       "EmbedMcRecoTrack", EmbedEff::KAON);
  ee.addNtuple("/phenix/hp/data82/phnxhp01/andrew/emb/eval/p/ppbar.root",
	       "EmbedMcRecoTrack", EmbedEff::PROT);

  ee.setupCentBins(nCentBins, centLo, centHi);
  ee.setupMomBins(nMomBins, momLo, momHi);

  // Assign cut values.
  // Setting any cut value <= 0 means the cut will not be applied.
  //
  // Note on PC3 sigma cut: matching dists. are not calibrated. 
  // Gaussian fits to pc3sdphi and z show widths of 0.55 - 0.57 
  // for MB centrality and all momentum.
  // They should be 1 by construction.
  // A sigma(pT, cent) function should be evaluated
  // at the given binning to give correct cut values.

  ee.setEvalType(2);           // Always needs to be set (1 or 2).
  ee.setPC2sigmaCut(0);        // 0 = No cut.
  ee.setPC3sigmaCut(3.0*0.56); // Needs work!!
  ee.setDchZedCut(80.0);
  ee.setDchQual1orQual2Cuts(31, 63);

  ee.fillHistos();
  ee.drawHistos();

  return 0;
}
