#include "comparator.h"
#include <iostream>
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TCanvas.h"

//_____________________________________________________________________________
comparator::comparator(const char* file1, const char* file2)
{
  fFile1 = new TFile(file1);
  fFile2 = new TFile(file2);
}

//_____________________________________________________________________________
comparator::~comparator()
{
  delete fFile1;
  delete fFile2;
}

//_____________________________________________________________________________
void 
comparator::plot(const char* histoname, const char* cent, 
		 const char* cutname)
{
  std::string scutname;

  if ( !cutname ) 
    {
      scutname = fCurrentCutName;
    }
  else
    {
      scutname = cutname;
      fCurrentCutName = scutname;
    }
  cout << "Plotting histogram " << histoname
       << " for cut " << fCurrentCutName << endl;

  fFile1->cd(cutname);
  gDirectory->cd(cent);

  TH1* h1 = (TH1*)gDirectory->Get(histoname);

  fFile2->cd(cutname);
  gDirectory->cd(cent);

  TH1* h2 = (TH1*)gDirectory->Get(histoname);

  if ( h1 && h2 ) 
    {
      double maximum = h1->GetMaximum();
      double minimum = h1->GetMinimum();

      if ( h2->GetMaximum() > maximum ) 
	{
	  maximum = h2->GetMaximum();
	}

      if ( h2->GetMinimum() < minimum ) 
	{
	  minimum = h2->GetMinimum();
	}

      maximum*=1.2;
      minimum*=0.8;

      h1->SetMaximum(maximum);
      h2->SetMaximum(maximum);
      h1->SetMinimum(minimum);
      h2->SetMinimum(minimum);

      h1->SetMarkerStyle(20);
      h1->SetMarkerColor(2);
      h1->Draw("P");
      h2->SetMarkerStyle(25);
      h2->SetMarkerColor(4);
      h2->Draw("PSAME");

      TH1* hdiv = (TH1*)h1->Clone();
      hdiv->SetMaximum(2.0);
      new TCanvas();
      hdiv->Divide(h2);
      hdiv->Draw("histe");
    }

}
