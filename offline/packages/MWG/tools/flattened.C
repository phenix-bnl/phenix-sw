#include "Tools.h"
#include <TH1.h>
#include <iostream>
using namespace std;

double Tools::flattened(TH1 * histo, Double_t obs) // from Mike Leitch
{
    // return a number 0 and 1 distributed flatly according the histogram bin contents.
    // This function checks if the bins integral exists. If not, the integral
    // is evaluated, normalized to one.
    // The integral is automatically recomputed if the number of entries
    // is not the same then when the integral was computed.
    // NB Only valid for 1-d histograms.
    //  
    Int_t nbinsx = histo->GetNbinsX();
    Double_t integral;
    Double_t* fIntegral=histo->GetIntegral();
    Stat_t Entries=histo->GetEntries();
    if (fIntegral) {
	if (fIntegral [nbinsx+1] != Entries) integral = (histo->ComputeIntegral());
    } else {
	integral =histo->ComputeIntegral();
	fIntegral=histo->GetIntegral();
	if (integral == 0 || fIntegral == 0) { cout<<"i="<<integral<<" f="<<fIntegral<<endl; return -999;}
    }
    if ((obs>(histo->GetXaxis()->GetXmax()))||(obs<(histo->GetXaxis()->GetXmin()))) return -100;
    Float_t frac = (obs-(histo->GetXaxis()->GetXmin()))/(histo->GetXaxis()->GetXmax()-histo->GetXaxis()->GetXmin());
    Int_t ibin= (Int_t) (frac*nbinsx);
    return (fIntegral[ibin]);
}
