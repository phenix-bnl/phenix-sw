#include "Riostream.h"

TH1D* compute_ET_from_spec( const char *filename = "pi0_pQCD_test.txt",
			    const double norm_y = 1.e-9, // pb --> mb for pQCD files
			    const double TAB = 25.37,
			    const double RAA = 0.20,
			    const double ptmin = 1.,
			    const double ptmax = 100.,
			    const bool antinucleon = false )
{
  ifstream in;
  in.open(filename, ios::in);

  double x,y;
  double ey=0.;
  double x1,x2;
  int nbins = 0;
  double xmin = 0.;
  double xmax = 0.;
  double halfbinwidth = 0.;

  int rows = -1;

  // first read to determine number of bins
  // we don't care about format (as long as 1st column is x ...)
  while (1) {

    in >> x >> y;

    if (nbins==0) xmin = x; // assuming ascii file is ordered increasingly
    if (nbins==1) halfbinwidth = 0.5*(x-xmin); // assuming ascii file has fixed binwidth

    if (!in.good()) break;
    nbins++;
  }
  cout << endl << " <I> Found " << nbins << " points ..." ;
  if (!nbins)
    {
      cout << endl
      in.close();
      return 0;
    }
  in.close();

  xmax = x; // last value read
  
  char *hname = (char*)gSystem->BaseName(filename);
  
  TH1D *hspec = new TH1D(hname,hname,nbins,xmin-halfbinwidth,xmax+halfbinwidth);
  printf(" Booking histo %s with %d bins: xmin=%8f, xmax=%8f (binwidth=%8f) \n",
	 hname,nbins,xmin,xmax,halfbinwidth*2.);

  // second read to fill histo

  in.open(filename, ios::in);
  while (1) {

    in >> x >> y;
    if (!in.good()) break;
    
    int bin = hspec->GetXaxis()->FindBin(x);
    hspec->SetBinContent(bin,y*norm_y);
    if (ey==0) ey=1.e-20; // set a minimum error
    hspec->SetBinError(bin,ey);
  }

  in.close();

  // let's compute E_T for different centralities

  double ET = 0.;
  double mN = 0.940;
  int binptmin=0, binptmax=0;

  // firs/last bin histo: 0=underflow, (nbins+1)=overflow
  for(int i=1;i<=nbins;i++)
    {
      x = hspec->GetBinCenter(i);
      y = hspec->GetBinContent(i);

      binptmin = hspec->GetXaxis()->FindBin(ptmin);
      binptmax = hspec->GetXaxis()->FindBin(ptmax);

      // Eq. (17) of http://arxiv.org/abs/hep-ph/0404209

      ET = y*x*x*TAB*RAA;
      if (antinucleon) ET=y*x*TAB*RAA*(sqrt(mN*mN+x*x)+mN)/sqrt(mN*mN+x*x);
      hspec->SetBinContent(i,ET);

    }
  hspec->Draw("h");

  cout << endl << " <I> The E_T for spectrum of file " << filename << " (TAB=" << TAB << ", RAA=" << RAA 
       << ") is: " << hspec->Integral(binptmin,binptmax) << " [ptmin=" << ptmin << ", ptmax=" << ptmax 
       << "]" << endl << endl;

  return hspec;
 
}
