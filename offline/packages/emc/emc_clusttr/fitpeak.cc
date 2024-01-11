
#include "fitpeak.hh"

#define MIN(x,y) x<y ? x : y
#define MAX(x,y) x>y ? x : y

TF1* fitpeak(TH1* h,float min,float max,float width,char* addname,char* fitopt,float upwidth){
  char* opt_q;
  opt_q = strstr(fitopt,"Q");
  if(! opt_q ) opt_q = strstr(fitopt,"q");
  char* opt_0;
  opt_0 = strstr(fitopt,"0");

  if(! opt_q )cout<<" ===================== fitpeak ====================== "<<endl;
  char opt[32];
  float height,mean,sigma;
  float calcmin,calcmax;
  TF1* fit = new TF1("fit","gaus");

  int firstbin = h->FindBin(min);
  int lastbin = h->FindBin(max);
  h->GetXaxis()->SetRange(firstbin,lastbin);
  height = h->GetMaximum();
  mean = h->GetMean();
  sigma = fabs(h->GetRMS());
  h->GetXaxis()->UnZoom();
  calcmin = MAX(min,mean - sigma*width );
  if( upwidth > 0 )
    calcmax = MIN(max,mean + sigma*upwidth );
  else
    calcmax = MIN(max,mean + sigma*width );
  fit->SetRange(calcmin,calcmax);
  sprintf(opt,"RQ0%s",fitopt);
  h->Fit("fit",opt);
  height = fit->GetParameter(0);
  mean = fit->GetParameter(1);
  sigma = fabs(fit->GetParameter(2));
  calcmin = MAX(min, mean - sigma*width );
  if( upwidth > 0 )
    calcmax = MIN(max, mean + sigma*upwidth );
  else
    calcmax = MIN(max, mean + sigma*width );
  fit->SetRange(calcmin,calcmax);
  sprintf(opt,"R%s",fitopt);
  h->Fit("fit",opt);

  char fitname[64];
  sprintf(fitname,"%s%s",h->GetName(),addname);
  TObject* obj = gROOT->FindObject(fitname);
  if( obj != 0 ){
    if(!opt_q) cout<<" Delete "<<fitname<<endl;
    obj->Delete();
  }
  fit->SetName(fitname);

  if(! opt_q ) cout<<" ============================= ====================== "<<endl;

  return fit;
}

//
