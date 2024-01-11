#include <TFile.h>
#include <TCanvas.h>
#include <TH1.h>
#include <TF1.h>
#include <iostream.h>

void AcceptancePlots(char InFile[60]="pi0_Acceptance_Full_EMCal_Run2.root", Float_t pTmax = 8. )
{
  
  cout << endl << " <W> We sure of the maximum value of your pT in your input rawrel file. "
       << "Now we take pTmax(GeV/c)= " << pTmax << " !!";

  TFile* in = new TFile(InFile,"READ");

  TH1F * hPtAcceptance = (TH1F *) in->Get("hPtAcceptance");

  gROOT->SetStyle("Plain");
//  gStyle->SetOptStat(0);
//  gStyle->SetOptTitle(1);

  TCanvas * Acceptance2 = new TCanvas("Acceptance2","Acceptance2");

  hPtAcceptance->SetTitle(InFile);
  hPtAcceptance->SetXTitle("p_{T} (GeV/c)");
  hPtAcceptance->SetYTitle("Acceptance (|y|<0.5, 0<\\phi<2\\pi)");
  hPtAcceptance->Draw();

  TF1 * AcceptanceFit = new TF1("AcceptanceFit","([0]+[1]*x)*(1.0-exp(-(x-[2])/[3]))",0.,pTmax);
  AcceptanceFit->SetParameter(0,0.25);
  AcceptanceFit->SetParameter(1,0.);
  AcceptanceFit->SetParameter(2,-0.2);
  AcceptanceFit->SetParameter(3,1.);
  AcceptanceFit->Draw("same");

  hPtAcceptance->Fit(AcceptanceFit,"R","",0.7,pTmax);

  cout << "================================================================================"<<endl;
  cout << InFile << endl;
  cout << "================================================================================"<<endl;
  cout << "A(pT) = ( A + B*pT ) * (1.0 - exp (-(pT-a)/b))" << endl;

  Float_t RelError_A = TMath::Abs( 100.*AcceptanceFit->GetParError(0)/AcceptanceFit->GetParameter(0));
  Float_t RelError_B = TMath::Abs( 100.*AcceptanceFit->GetParError(1)/AcceptanceFit->GetParameter(1));
  Float_t RelError_a = TMath::Abs( 100.*AcceptanceFit->GetParError(2)/AcceptanceFit->GetParameter(2));
  Float_t RelError_b = TMath::Abs( 100.*AcceptanceFit->GetParError(3)/AcceptanceFit->GetParameter(3));

  cout << "A = " << AcceptanceFit->GetParameter(0) << " +/- " << AcceptanceFit->GetParError(0) 
       <<"  Relative Error = " <<  RelError_A <<"%" << endl;
  cout << "B = " << AcceptanceFit->GetParameter(1) << " +/- " << AcceptanceFit->GetParError(1) 
       <<"  Relative Error = " <<  RelError_B <<"%" << endl;
  cout << "a = " << AcceptanceFit->GetParameter(2) << " +/- " << AcceptanceFit->GetParError(2)
       <<"  Relative Error = " <<  RelError_a <<"%" << endl;
  cout << "b = " << AcceptanceFit->GetParameter(3) << " +/- " << AcceptanceFit->GetParError(3) 
       <<"  Relative Error = " <<  RelError_b <<"%" << endl;

  cout << "================================================================================"<<endl;
  Float_t Error, RelError, xpT;
  Int_t pTBin;
  Float_t pT[7] = { 0.71, 1.21, 1.71, 2.22, 2.72, 3.22, 3.73 };

  for(Int_t ipT=0; ipT<7; ipT++) 
    {
      xpT      =  pT[ipT];
      pTBin    =  hPtAcceptance->FindBin(xpT);
      Error    =  hPtAcceptance->GetBinError(pTBin);
      RelError =  100.* hPtAcceptance->GetBinError(pTBin)/hPtAcceptance->GetBinContent(pTBin);
      cout << "Acc(pT="<<xpT<<" GeV/c)=" << AcceptanceFit->Eval(xpT) << " +/- " <<  Error 
	   << " (Relative Error = " << RelError <<"%)" << endl;
  }

  //_____________________________________________________________________________

  cout << endl  ;
  cout << "================================================================================"<<endl;
  cout << "================================================================================"<<endl;

  for(Int_t ipT=0; ipT<(2*(pTmax-1)); ipT++) 
    {
      xpT      =  ( (Float_t) ipT) *0.5 + 1.250;
      pTBin    =  hPtAcceptance->FindBin(xpT);
      Error    =  hPtAcceptance->GetBinError(pTBin);
      RelError =  100.* hPtAcceptance->GetBinError(pTBin)/hPtAcceptance->GetBinContent(pTBin);
      cout << "Acc(pT="<<xpT<<" GeV/c) = " << AcceptanceFit->Eval(xpT) << " +/- " <<  Error 
	   << " (Relative Error = " << RelError <<"%)" << endl;
    }
  cout << "================================================================================"<<endl;


}
