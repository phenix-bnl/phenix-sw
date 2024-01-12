#include "TFile.h"
#include "TF1.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"

#include <fstream>
#include <iostream>
#include <cmath>

#include "summaryQA.h"

using namespace std;

int QASummary::processDch()
{
  cout << "Dch..." << endl;
fstream textFile(outputName1, ios::in); if (textFile) { textFile.close(); textFile.open(outputName1,ios::app|ios::out); }
 fstream statusFile(outputName2, ios::in); if (statusFile) {statusFile.close();  statusFile.open(outputName2,ios::app|ios::out); }

  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- DCH QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;

   	TH1* dchprof1;
	  TProfile* dchprof2;
	  TProfile* dchprof3;

   TH1F* dchTrkMult = (TH1F *) qafile->Get ("dchTrkMult");
   TH2F* dchTrkVHitsW = (TH2F *) qafile->Get ("dchTrkVHitsW");
   TH2F* dchTrkVHitsE = (TH2F *) qafile->Get ("dchTrkVHitsE");
   TH2F* dchTrkWVE = (TH2F *) qafile->Get ("dchTrkWVE");
   TH1F* dchTrkQualityW = (TH1F *) qafile->Get ("dchTrkQualityW");
   TH1F* dchTrkQualityE = (TH1F *) qafile->Get ("dchTrkQualityE");
   TH1F* dchTrkNHits = (TH1F *) qafile->Get ("dchTrkNHits");
   TH1F* dchGoodEvent = (TH1F *) qafile->Get ("dchGoodEvent");
   TH1F* dchGoodTrkPhiW = (TH1F *) qafile->Get ("dchGoodTrkPhiW");
   TH1F* dchGoodTrkPhiE = (TH1F *) qafile->Get ("dchGoodTrkPhiE");
   TH1F* dchGoodTrkMultE = (TH1F *) qafile->Get ("dchGoodTrkMultE");
   TH1F* dchGoodTrkMultW = (TH1F *) qafile->Get ("dchGoodTrkMultW");
   TH1F* dchGoodTrkMomE = (TH1F *) qafile->Get ("dchGoodTrkMomE");
   TH1F* dchGoodTrkMomW = (TH1F *) qafile->Get ("dchGoodTrkMomW");
   TH1F* dchGoodTrkNHitsE = (TH1F *) qafile->Get ("dchGoodTrkNHitsE");
   TH1F* dchGoodTrkNHitsW = (TH1F *) qafile->Get ("dchGoodTrkNHitsW");

  Float_t dchtrkmean, dchhitmean;
  Float_t dchmean, dchgoodmean; 
  Float_t dchgoodmeanmult_e, dchgoodmeanmult_w;
  Float_t dchgoodmeanmom_e, dchgoodmeanmom_w;
  Float_t dchgoodmeannhits_e, dchgoodmeannhits_w;
  Float_t dchp0_e, dchp1_e;
  Float_t dchp0_w, dchp1_w;
  Float_t dchp0_t, dchp1_t;
  Float_t dchhratio, dchzratio;
  Int_t dchcont;
  Int_t dchntot = 0, dchnum1 = 0;
  Int_t maxcont, high1, high2, high3;
  Float_t phibins;
  Float_t eff_w, eff_e;

  dchmean = dchTrkMult->GetMean ();
  dchtrkmean = dchTrkMult->GetMean ();
  dchgoodmean = dchGoodEvent->GetMean ();
  dchmean = dchTrkNHits->GetMean ();
  dchhitmean = dchTrkNHits->GetMean ();
  dchgoodmeanmult_e = dchGoodTrkMultE->GetMean ();
  dchgoodmeanmult_w = dchGoodTrkMultW->GetMean ();
  dchgoodmeanmom_e = dchGoodTrkMomE->GetMean ();
  dchgoodmeanmom_w = dchGoodTrkMomW->GetMean ();
  dchgoodmeannhits_e = dchGoodTrkNHitsE->GetMean ();
  dchgoodmeannhits_w = dchGoodTrkNHitsW->GetMean ();
 
  for (Int_t ic = 0; ic < 40; ic++)
  {
    dchcont = (Int_t)dchTrkNHits->GetBinContent (ic + 1);
    if (ic < 8)
      dchnum1 += dchcont;
    dchntot += dchcont;
  }
  if ( dchntot>0 ) {
    dchhratio = 100. * (Float_t) (dchntot - dchnum1) / (Float_t) dchntot;
  }
  else {
    dchhratio = 0.0; 
  }
  dchntot = 0;
  dchnum1 = 0;
  for (Int_t ic = 1; ic < 36; ic++)
  {
    dchcont = (Int_t)dchTrkQualityE->GetBinContent(ic + 1) +
              (Int_t)dchTrkQualityW->GetBinContent(ic + 1);
    if (ic < 5)
      dchnum1 += dchcont;
    dchntot += dchcont;
  }
  if ( dchntot>0 ) {
    dchzratio = 100. * (Float_t) (dchntot - dchnum1) / (Float_t) dchntot;
  }
  else {
    dchzratio = 0.0;
  }
  dchprof1 = dchTrkVHitsW->ProfileX ("dchprof1", 1, 5000);
  dchprof1->Fit ("pol1", "Q0", " ");

	dchp0_w = ((dchprof1->GetFunction ("pol1"))->GetParameter (0));
  dchp1_w = ((dchprof1->GetFunction ("pol1"))->GetParameter (1));

  dchprof2 = dchTrkVHitsE->ProfileX ("dchprof2", 1, 5000);
  dchprof2->Fit ("pol1", "Q0", " ");
  dchp0_e = ((dchprof2->GetFunction ("pol1"))->GetParameter (0));
  dchp1_e = ((dchprof2->GetFunction ("pol1"))->GetParameter (1));

  dchprof3 = dchTrkWVE->ProfileX ("dchprof3", 1, 5000);
  dchprof3->Fit ("pol1", "Q0", " ");
  dchp0_t =(float) ((dchprof3->GetFunction ("pol1"))->GetParameter (0));
  dchp1_t =(float) ((dchprof3->GetFunction ("pol1"))->GetParameter (1));


  phibins = 0.5*acos(-1.)/0.05;
  high1 = 0;
  high2 = 0;
  high3 = 0;
  maxcont = (int) dchGoodTrkPhiW->GetMaximum();
  for (Int_t ic = 0; ic < 100; ic++)
  {
    dchcont = (Int_t)dchGoodTrkPhiW->GetBinContent(ic + 1);
    if ( dchcont<maxcont ) {
      if ( dchcont>=high1 ) {
	high3 = high2;
	high2 = high1;
	high1 = dchcont;
      }
      else {
	if ( dchcont>=high2 ) {
	  high3 = high2;
	  high2 = dchcont;
	}
	else {
	  if ( dchcont>=high3 ) high3 = dchcont;
	}
      }
    }
  }
  if ( high1+high2+high3 > 0 ) {
    eff_w = (dchGoodTrkPhiW->Integral(1,100))/(phibins*(high1+high2+high3)/3.0);
  }
  else {
    eff_w = 0.0;
  }

  high1 = 0;
  high2 = 0;
  high3 = 0;
  maxcont = (Int_t)dchGoodTrkPhiE->GetMaximum();
  for (Int_t ic = 0; ic < 100; ic++)
  {
    dchcont = (Int_t)dchGoodTrkPhiE->GetBinContent(ic + 1);
    if ( dchcont<maxcont ) {
      if ( dchcont>=high1 ) {
	high3 = high2;
	high2 = high1;
	high1 = dchcont;
      }
      else {
	if ( dchcont>=high2 ) {
	  high3 = high2;
	  high2 = dchcont;
	}
	else {
	  if ( dchcont>=high3 ) high3 = dchcont;
	}
      }
    }
  }
  if ( high1+high2+high3 > 0 ) {
    eff_e = (dchGoodTrkPhiE->Integral(1,100))/(phibins*(high1+high2+high3)/3.0);
  }
  else {
    eff_e = 0.0;
  }

  int dchstatusw = 0, dchstatuse = 0;

  if (dchgoodmeanmult_e>0.5 && dchgoodmeanmom_e>0.4 &&
    dchgoodmeannhits_e>10) {
    dchstatuse = 0; // good
  }
  else {
    dchstatuse = 1; // bad
  }

  if (dchgoodmeanmult_w>0.5 && dchgoodmeanmom_w>0.4 &&
    dchgoodmeannhits_w>10) {
    dchstatusw = 0; // good
  }
  else {
    dchstatusw = 1; // bad
  }

// for the moment we set the DCH to GOOD by default
//  dchstatusw = 0;
//  dchstatuse = 0;

//write out statistics
  textFile << " Average Number of xhits on DC track = " << dchhitmean << endl;
  textFile << " Average DC track multiplicity = " << dchtrkmean << endl;
  textFile <<
    " Average DC track multiplicity for Event within bbcvertex[-30,30] = " <<
    dchgoodmean << endl;
  textFile << " Percentage of Tracks with >= 8 xhits = " << dchhratio << endl;
  textFile << " Percentage of Tracks with z information = " << dchzratio <<
    endl;
  textFile << " West Arm Track vs Hit Fit parameters = ";
  textFile << dchp0_w << " " << dchp1_w << endl;
  textFile << " East Arm Track vs Hit Fit parameters = ";
  textFile << dchp0_e << " " << dchp1_e << endl;
  textFile << " East Track vs West Track Fit parameters = ";
  textFile << dchp0_t << " " << dchp1_t << endl;
  textFile << " Active area fraction for good tracks West = " << eff_w << endl;
  textFile << " Active area fraction for good tracks East = " << eff_e << endl;
  textFile << " DCH West Status = " << dchstatusw << endl;
  textFile << " DCH East Status = " << dchstatuse << endl;
  statusFile << dchstatusw <<" "<< dchstatuse << " ";

  CommitToQADatabase("Dch", "Track Multiplicity East", dchgoodmeanmult_e, 0.0);
  CommitToQADatabase("Dch", "Track Multiplicity West", dchgoodmeanmult_w, 0.0);
  CommitToQADatabase("Dch", "P East", dchgoodmeanmom_e, 0.0);
  CommitToQADatabase("Dch", "P Weast", dchgoodmeanmom_w, 0.0);
  CommitToQADatabase("Dch", "Xhits East", dchgoodmeannhits_e, 0.0);
  CommitToQADatabase("Dch", "Xhits Weast", dchgoodmeannhits_w, 0.0);
  CommitToQADatabase("Dch", "Xhits on DC track", dchhitmean, 0.0);
  CommitToQADatabase("Dch", "Track Multiplicity", dchtrkmean, 0.0);
  CommitToQADatabase("Dch", "Track Multiplicity |vtx<30cm|", dchgoodmean, 0.0);
  CommitToQADatabase("Dch", "Tracks w/ xhits>=8 (%)", dchhratio, 0.0);
  CommitToQADatabase("Dch", "Tracks w/ Z information (%)", dchzratio, 0.0);
  CommitToQADatabase("Dch", "Active Area fraction East", eff_e, 0.0);
  CommitToQADatabase("Dch", "Active Area fraction Weast", eff_w, 0.0);

  cout << "    ...done." << endl;
  return 0 ;
}















