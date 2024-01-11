#include "comparator.h"
#include "corrections.h"

#include <iostream>
#include <fstream>
#include <cstdio>
#include <string>
#include <cstdlib>
#include <cmath>

#include "TDirectory.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TCanvas.h"
#include "TString.h"
#include "TLegend.h"
#include "TStyle.h"
#include "TPaveText.h"
#include "TKey.h"
#include "TLine.h"
#include "TLegend.h"
#include "TF1.h"
#include "TGraphErrors.h"
#include "TMath.h"

//
//  Example of using this code (requires ROOT 3.02 or higher for TDirectory):
/*
 .L corrections.C++  
 .L comparator.C++
 comparator comp("~/wa/efficiency3_644_2nd_bckup.root","~/wa/efficiency3_644_2nd.root");
 comp.ls();
 comp.efficiencyFit("FiduNoW3DeadWarnEnergyAsym1Cut");
 comp.efficiencyFit("FiduNoW3DeadWarnEnergyAsym2Chi2ToF1Cut");

*/
//

static const int CentClasses = 20;

static const double Events[CentClasses] = { 1325956, 1633974, //  0-10
					    1640068, 1671280, // 10-20
					    3276173/2, 3276173/2, // 20-30
					    3265392/2, 3265392/2, // 30-40
					    3324897/2, 3324897/2, // 40-50
					    3285326/2, 3285326/2, // 50-60
					    3331919/2, 3331919/2, // 60-70
					    3264595/2, 3264595/2, // 70-80
					    4022149/2, 0, 0, // 80-92, -
					    30041726};  // last = min. bias

static const double TWOPI = TMath::Pi()*2.0;

//_____________________________________________________________________________
comparator::comparator(const char* file1, const char* file2)
{
  if ( file1 ) 
    {
      fFile1 = new TFile(file1);
    }
  else
    {
      fFile1=0;
    }

  if (file2)
    {
      fFile2 = new TFile(file2);
    }
  else
    {
      fFile2=0;
    }
  fCanvasList = new TList;
  fLegend1 = 0;
  fLegend2 = 0;

  fPlotTitle = 0;

  fFit = 0;

  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);
  gStyle->SetOptFit(1);

  fLogy=false;

  setEfficiencyScale();
  setVerbose();
}

//_____________________________________________________________________________
comparator::~comparator()
{
  delete fFile1;
  delete fFile2;
  delete fCanvasList;
  delete fPlotTitle;
  delete fLegend2;
  delete fLegend1;
  delete fFit;
}

//_____________________________________________________________________________
void
comparator::cd1(const char* dir)
{
  if ( fFile1 ) 
    {
      fFile1->cd(dir);
    }
}

//_____________________________________________________________________________
void
comparator::cd2(const char* dir)
{
  if ( fFile2 ) 
    {
      fFile2->cd(dir);
    }
}

//_____________________________________________________________________________
void 
comparator::compare2raw(const char* cut1, const char* cut2, 
			int cent1, int cent2,
			const char* type1, const char* type2,
			bool fit)
{
  TGraphErrors* g1 = getRealSpectrum(cent1,cent2,type1,cut1);
  TGraphErrors* g2 = getRealSpectrum(cent1,cent2,type2,cut2);

  if ( g1 && g2 ) 
    {
      if ( fit ) 
	{
	  double ptmin = 1.0;
	  double ptmax = g1->GetX()[g1->GetN()-1];
	  
	  TF1* f1 = hagedornFit(g1,ptmin,ptmax);
	  
	  shiftPt(*g1,f1);
	  
	  hagedornFit(g1,ptmin,ptmax,"hagedorn2");
	  
	  TF1* f2 = hagedornFit(g2,ptmin,ptmax);
	  
	  shiftPt(*g2,f2);
	  
	  hagedornFit(g2,ptmin,ptmax,"hagedorn2");
	}

      char name[200];
      char l1[200];
      char l2[200];
      sprintf(l1,"w/ %s %s",type1,cut1);
      sprintf(l2,"w/ %s %s",type2,cut2);
      setLegends(l1,l2);
      sprintf(name,"c_%s%s_%s%s_%d_%d",type1,cut1,type2,cut2,cent1,cent2);
      plot(g1,g2,name,"compare2raw");
    }
}

//_____________________________________________________________________________
void 
comparator::getCentralityClassLimits(const char* cent, int& c1, int& c2)
{
  c1=c2=-1;
  if ( strcmp(cent,"C0")==0  ) { c1=  0; c2= 10; }
  if ( strcmp(cent,"C1")==0  ) { c1= 10; c2= 20; }
  if ( strcmp(cent,"C2")==0  ) { c1= 20; c2= 30; }
  if ( strcmp(cent,"C3")==0  ) { c1= 30; c2= 40; }
  if ( strcmp(cent,"C4")==0  ) { c1= 40; c2= 50; }
  if ( strcmp(cent,"C5")==0  ) { c1= 50; c2= 60; }
  if ( strcmp(cent,"C6")==0  ) { c1= 60; c2= 70; }
  if ( strcmp(cent,"C7")==0  ) { c1= 70; c2= 80; }
  if ( strcmp(cent,"C8")==0  ) { c1= 80; c2= 92; }
  if ( strcmp(cent,"C10")==0 ) { c1=100; c2=100; }
}

//_____________________________________________________________________________
void
comparator::comparetoraw(const char* cutname,
			 const char* cent,
			 bool ptshift)
{
  fFile1->cd(cutname);
  gDirectory->cd(cent);

  TH1* h1 = getRecoPt(gDirectory);

  int c1 = -1;
  int c2 = -1;

  getCentralityClassLimits(cent,c1,c2);

  if ( c1 < 0 || c2 < 0 )
    {
      std::cerr << "Incorrect centrality" << endl;
      return;
    }
  else
    {
      cout << "Centrality " << cent << " = " << c1 << "-" << c2 << "%"
	   << endl;
    }

  TGraphErrors* g1 = getRealSpectrum(c1,c2,"raw");

  if (!g1) return;

  double ptmin = 1.0;
  // use max=7.0 GeV/c as in the embedding, simulation goes only up to 8
  double ptmax = 7.0;
  
  TF1* f1 = hagedornFit(g1,ptmin,ptmax);

  if ( ptshift ) 
    {
      shiftPt(*g1,f1);
    }

  hagedornFit(g1,ptmin,ptmax,"hagedorn2");
  
  TGraphErrors* g2 = getGraphErrors(h1,g1);
  

  double xint1 = integral(*g1,2,6);
  double xint2 = integral(*g2,2,6);
  
  scale(*g2,xint1/xint2);

  TF1* f2 = hagedornFit(g2,ptmin,ptmax);

  if ( ptshift ) 
    {
      shiftPt(*g2,f2);
    }

  hagedornFit(g2,ptmin,ptmax,"hagedorn2");

  if ( g1 && g2 ) 
    {
      char name[200];
      sprintf(name,"%s_%s",cutname,cent);
      plot(g1,g2,name,"comparetoraw");
    }
}

//_____________________________________________________________________________
void
comparator::efficiencyFit(const char* cutname, ostream& out)
{
  char cent[10];

  std::vector<double> p0;
  std::vector<double> p0err;
  std::vector<double> p1;
  std::vector<double> p1err;

  for ( size_t i = 0; i < 11; ++i ) 
    {
      sprintf(cent,"C%d",i);
      TF1* f1 = efficiencyFit(cutname,cent);      
      p0.push_back(f1->GetParameter(0));
      p0err.push_back(f1->GetParError(0));
      p1.push_back(f1->GetParameter(1));
      p1err.push_back(f1->GetParError(1));
    }



  out << "double P0_" << cutname << "[] = { " << endl;
  makeCode(out,p0);
  out << "double P0ERR_" << cutname << "[] = { " << endl;
  makeCode(out,p0err);
  out << "double P1_" << cutname << "[] = { " << endl;
  makeCode(out,p1);
  out << "double P1ERR_" << cutname << "[] = { " << endl;
  makeCode(out,p1err);  
}

//_____________________________________________________________________________
void
comparator::makeCode(ostream& out, std::vector<double>& p0)
{
  std::ostream::fmtflags oldflags = out.flags();

  for ( size_t i = 0 ; i < p0.size(); ++i ) 
    {
      out.setf(ostream::fixed);
      out.precision(6);
      out << string(10,' ') << p0[i];
      if ( i < p0.size()-1 ) out << ",";
      out << endl;
    }

  out << "};" << endl;

  out.setf(oldflags);
}

//_____________________________________________________________________________
TF1*
comparator::efficiencyFit(const char* cutname, const char* cent)
{
  if ( !fFile1 ) return 0;

  fFile1->cd(cutname);
  gDirectory->cd(cent);

  TH1* h = getCombined(gDirectory);

  h->Scale(fScale);

  TF1* f1 = new TF1("f1","pol1",1.2,7);

  h->Fit(f1,"QR","",1.2,7);

  return f1;
}

//_____________________________________________________________________________
TGraphErrors*
comparator::getGraphErrors(TH1* h1, TGraphErrors* g1)
{
  // Make a TGraphErrors out of h1, using yerr from g1

  int N = g1->GetN();

  double* x = new double[N];
  double* xerr = new double[N];
  double* y = new double[N];
  double* yerr = new double[N];

  for ( int i = 0; i < N; i++ )
    {
      double xi,yi,xierr,yierr;

      g1->GetPoint(i,xi,yi);
      xierr = g1->GetErrorX(i);
      yierr = g1->GetErrorY(i);

      int bin = h1->GetXaxis()->FindBin(xi);

      y[i] = h1->GetBinContent(bin);
      yerr[i] = y[i] * (yierr/yi);
      xerr[i] = xierr;

      x[i] = xi;
    }

  TGraphErrors* g2 
    = new TGraphErrors(N,x,y,xerr,yerr);

  return g2;
}

//_____________________________________________________________________________
double
comparator::getMeanPt(double ptini, double ptbinwidth, TF1* hagedorn)
{
  TF1* xhagedorn = new TF1("xhagedorn", "[0]*x*([1]/(x+[1])^[2])", 
			   hagedorn->GetXmin(),hagedorn->GetXmax());

  xhagedorn->SetParameters(hagedorn->GetParameters());

  double s = ptbinwidth/2.0;

  double ptmean = xhagedorn->Integral(ptini-s,ptini+s)/hagedorn->Integral(ptini-s,ptini+s);

  return ptmean;
}

//_____________________________________________________________________________
TGraphErrors*
comparator::getRealSpectrum(int c1, int c2, 
			    const char* type, const char* cut,
			    bool ptshift)
{
  char effcut[1024];

  if ( strcmp(cut,"t0a0")== 0 )
    {
      strcpy(effcut,"FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut");
    }
  else if ( strcmp(cut,"t1a0")==0 ) 
    {
      strcpy(effcut,"FiduNoW3DeadWarnEnergyAsym1Chi2ToF2Cut");
    }
  else if ( strcmp(cut,"t0a1")==0 ) 
    {
      strcpy(effcut,"FiduNoW3DeadWarnEnergyAsym2Chi2ToF1Cut");
    }
  else if ( strcmp(cut,"t1a1")==0 ) 
    {
      strcpy(effcut,"FiduNoW3DeadWarnEnergyAsym2Chi2ToF2Cut");
    }
  else
    {
      std::cerr << "Don't know how to treat cut " << cut
		<< std::endl;
      return 0;
    }

  const char* basename = "/home/aphecetc/phenix/data/ptarjan_yields";

  //const char* basename = "/home/aphecetc/phenix/data/david_tables";
  //const char* basename = "/afs/rhic/phenix/users/enterria/CORRECTED_RUN2/tables/";

  char filename[200];

  if ( c1!= 100 && c2!=100 ) 
    {
      sprintf(filename,"%s/yieldfinal_%s_cent%d_%d.txt",
	      basename,cut,c1,c2);
    }
  else
    {
      sprintf(filename,"%s/yieldfinal_%s_centminbias.txt",
	      basename,cut);
    }

  static const int MaxPTbins = 18;

  ifstream rawfile(filename);

  if ( !rawfile ) 
    {
      std::cerr << "Cannot open " << filename << std::endl;
      return 0;
    }
  else
    {
      std::cout << "Reading from file " << filename << std::endl;
    }

  double pTpizero[MaxPTbins];
  double epTpizero[MaxPTbins];
  double countspizero[MaxPTbins];
  double ecountspizero[MaxPTbins];
  double bidon[MaxPTbins];

  int i = 0;
  // Read pT, countsPi0 from the file
  while ( rawfile >> 
	  pTpizero[i] >> 
	  countspizero[i] >> 
	  ecountspizero[i] >>
	  bidon[i]
	  )
    { 
      i++;
    }

  rawfile.close();

  int NpTbins = i;
  double raw_yield[NpTbins];
  double eraw_yield[NpTbins];

  double eventsCC = getEvents(c1,c2);

  int CC = getCentralityClass(c1,c2);

  cout << "Centrality class " << CC << " effCut " 
       << effcut << endl;

  double PTBINWIDTH = 0.5; // GeV/c

  for (int ptbin = 0; ptbin < NpTbins; ptbin++ ) 
    {
      epTpizero[ptbin] = 0.05;

      raw_yield[ptbin] = countspizero[ptbin]/(eventsCC*PTBINWIDTH);
      eraw_yield[ptbin]  = ecountspizero[ptbin]/(eventsCC*PTBINWIDTH);
    }

  double yield[NpTbins];
  double eyield[NpTbins];


  for (int ptbin = 0; ptbin < NpTbins; ptbin++ ) 
    {
      yield[ptbin] = raw_yield[ptbin];
      eyield[ptbin] = eraw_yield[ptbin];
    }
  
  if (ptshift)
    {
      TGraphErrors* g1 = 
	new TGraphErrors(NpTbins, pTpizero, yield, epTpizero, eyield);
      
      int N = g1->GetN();
      double ptmin = 1.0;
      double ptmax = g1->GetX()[N-1]-1;
      
      TF1* f1 = hagedornFit(g1,ptmin,ptmax,"hagedorn",1);
      
      shiftPt(*g1,f1);
      
      for ( int i = 0; i < N; i++ ) 
	{
	  pTpizero[i] = g1->GetX()[i];
	}
      
      delete g1;
    }

  if (strcmp(type,"acc")==0 || strcmp(type,"eff") == 0 ||
      strcmp(type,"full")==0 )
    {
      for (int ptbin = 0; ptbin < NpTbins; ptbin++ ) 
	{
	  double acc = corrections::acceptance(pTpizero[ptbin],"value");
	  double eacc = corrections::acceptance(pTpizero[ptbin],"error");
	  
	  yield[ptbin] = raw_yield[ptbin]/acc;

	  cout.setf(ostream::scientific);

	  eyield[ptbin] = yield[ptbin] *
	    quadraticRelativeError(raw_yield[ptbin],eraw_yield[ptbin],
				   acc,eacc);

	  if ( strcmp(type,"eff")==0  || strcmp(type,"full")==0) 
	    {
	      double eff = corrections::efficiency
		(pTpizero[ptbin],CC,effcut,"value");
	      double eeff = corrections::efficiency
		(pTpizero[ptbin],CC,effcut,"error");

	      double y = yield[ptbin];
	      double ey = eyield[ptbin];

	      yield[ptbin] = y/eff;
	      eyield[ptbin] = yield[ptbin] * 
		quadraticRelativeError(y,ey,eff,eeff);

	      if ( strcmp(type,"full")==0 ) 
		{
		  double y = yield[ptbin];
		  double ey = eyield[ptbin];

		  double corr = 
		    corrections::nonVertex("value")/(pTpizero[ptbin]*TWOPI);
		  double ecorr = corr*corrections::nonVertex("error");

		  yield[ptbin] = y * corr;
		  eyield[ptbin] = yield[ptbin] *		    
		    quadraticRelativeError(y,ey,corr,ecorr);
		}
	    }
	}
    }
  else if ( strcmp(type,"raw") ) 
    {
      std::cerr << "comparator::getRealSpectrum : unknown type "
		<< type << std::endl;
      return 0;
    }

  return new TGraphErrors(NpTbins, pTpizero, yield, epTpizero, eyield);
}

//_____________________________________________________________________________
double
comparator::quadraticRelativeError(double a, double ea, 
				   double b, double eb)
{
  return sqrt( (ea/a)*(ea/a) + (eb/b)*(eb/b) );
}

//_____________________________________________________________________________
void 
comparator::compare2files(const char* histoname, 
			  const char* cutname,
			  const char* cent)
{
  cout << "Plotting histogram " << histoname
       << " for cut " << cutname 
       << " and centrality " << cent << endl;

  fFile1->cd(cutname);
  gDirectory->cd(cent);

  TH1* h1 = getHistogram(gDirectory,histoname);

  fFile2->cd(cutname);
  gDirectory->cd(cent);

  TH1* h2 = getHistogram(gDirectory,histoname);

  if ( h1 && h2 ) 
    {
      char name[200];
      sprintf(name,"%s_%s_%s",histoname,cutname,cent);
      plot(h1,h2,name,"compare2files");
    }
}

//_____________________________________________________________________________
void 
comparator::compare2centralities(const char* histoname, 
				 const char* cutname,
				 const char* cent1, const char* cent2)
{
  cout << "Plotting histogram " << histoname
       << " for cut " << cutname 
       << " and for centralities " << cent1
       << " and " << cent2 << endl;

  fFile1->cd(cutname);
  gDirectory->cd(cent1);

  TH1* h1 = getHistogram(gDirectory,histoname);

  fFile1->cd(cutname);
  gDirectory->cd(cent2);

  TH1* h2 = getHistogram(gDirectory,histoname);

  if ( h1 && h2 ) 
    {
      char name[200];

      sprintf(name,"%s_%s_%s_%s",histoname,cutname,
	      cent1,cent2);
      
      plot(h1,h2,name,"compare2centralities");
    }
}


//_____________________________________________________________________________
void 
comparator::compare2cuts(const char* histoname, 
			 const char* cutname1, 
			 const char* cutname2,
			 const char* cent,
			 bool limits)
{
  cout << "Plotting histogram " << histoname
       << " for cut " << cutname1 
       << " and for cut " << cutname2 
       << " and for centrality " << cent << endl;

  fFile1->cd(cutname1);
  gDirectory->cd(cent);

  TH1* h1 = getHistogram(gDirectory,histoname);

  fFile1->cd(cutname2);
  gDirectory->cd(cent);

  TH1* h2 = getHistogram(gDirectory,histoname);

  if ( h1 && h2 ) 
    {
      char name[200];
      sprintf(name,"%s_%s_%s_%s",histoname,cutname1,cutname2,cent);
      plot(h1,h2,name,"compare2cuts",limits);
    }

}

//_____________________________________________________________________________
TH1*
comparator::getCombined(TDirectory* dir)
{
  const char *hcounts = "hEfficiency_counts";
  const char *hfit    = "hEfficiency_fit";

  TKey* k1 = (TKey*)dir->FindKey(hcounts);
  TKey* k2 = (TKey*)dir->FindKey(hfit);

  if ( !k1 )
    {
      std::cerr << "Cannot find key hEfficiency_counts "
		<< " in dir " << dir->GetPath()
		<< std::endl;
      return 0;
    }

  if ( !k2 )
    {
      std::cerr << "Cannot find key hEfficiency_fit "
		<< " in dir " << dir->GetPath()
		<< std::endl;
      return 0;
    }

  TH1* h1 = (TH1*)k1->ReadObj();
  TH1* h2 = (TH1*)k2->ReadObj();

  TH1* h = (TH1*)h1->Clone();
  h->SetName("hEfficiency_combined");
  h->Reset();

  for ( size_t i = 1; i <= (size_t)h1->GetXaxis()->GetNbins(); ++i ) 
    {

      double c1 = h1->GetBinContent(i);
      double c2 = h2->GetBinContent(i);

      h->SetBinContent(i, 0.5*(c1+c2) );

      double max;
      double min;

      if ( c1 < c2 )
	{
	  min = c1;
	  max = c2;
	}
      else
	{
	  min = c2;
	  max = c1;
	}

      h->SetBinError(i, (max-min)/2.0);
   }
  return h;
}

//_____________________________________________________________________________
int 
comparator::getCentralityClass(const int Cent1, const int Cent2)
{
  //CentClass =  0 :  0- 5%
  //CentClass =  1 :  5-10%
  //CentClass =  2 : 10-15%
  //CentClass =  3 : 15-20%
  //CentClass =  4 : 20-25%
  //CentClass =  5 : 25-30%
  //CentClass =  6 : 30-35%
  //CentClass =  7 : 35-40%
  //CentClass =  8 : 40-45%
  //CentClass =  9 : 45-50%
  //CentClass = 10 : 50-55%
  //CentClass = 11 : 55-60%
  //CentClass = 12 : 60-65%
  //CentClass = 13 : 65-70%
  //CentClass = 14 : 70-75%
  //CentClass = 15 : 75-80%
  //CentClass = 16 : 80-85%
  //CentClass = 17 : 85-92%
  //CentClass = 18 : --
  //CentClass = 19 : min. bias

  int CC = Cent1/5;

  if ( Cent1==100 ) // min. bias
    {
      return 19;
    }
//   if ( Cent1==80 ) // 80-92%
//     {
//       return 16;
//     }
  if ( (CC>19) || (CC<0) || (Cent1==Cent2) || (Cent2>(Cent1+12)) ) //(Cent1<0) || (Cent1>100) || (Cent2<Cent1) || (Cent2>100) ) 
    {
      cout << " <E> I don't know this Centrality Class :" << Cent1 << " - " << Cent2 << "%" << endl;
      return -1;
    }
  else 
    {
      return CC;
    }

}

//_____________________________________________________________________________
double 
comparator::getEvents(const int Cent1, const int Cent2)
{
  int CC = getCentralityClass(Cent1, Cent2);

  if ( (Cent1 == Cent2-5) || (Cent1 == 100) ) // case 0-5, 15-20 ... or (last) min. bias
    {
      return Events[CC];
    }
  else if ( (Cent1 == Cent2-10) || (Cent1 == Cent2-12) ) // case 0-10, 30-40 ... or (last) 80-92
    {
      return Events[CC]+Events[CC+1];
    }
  else return -1.;
}

//_____________________________________________________________________________
TH1*
comparator::getRecoPt(TDirectory* dir)
{
  const char *hRecoParticleMinvPt = "hRecoParticleMinvPt";

  TKey* k1 = (TKey*)dir->FindKey(hRecoParticleMinvPt);

  if ( !k1 )
    {
      std::cerr << "Cannot find key hRecoParticleMinvPt"
		<< " in dir " << dir->GetPath()
		<< std::endl;
      return 0;
    }

  TH2* h2 = (TH2*)k1->ReadObj();

  int b1 = h2->GetXaxis()->FindBin(0.05);
  int b2 = h2->GetXaxis()->FindBin(0.20);

  TH1* h = h2->ProjectionY("hRecoPt",b1,b2,"E");

  hagedornFit(h);
  
  return h;
}

//_____________________________________________________________________________
TF1*
comparator::hagedornFit(TH1* h, double ptmin, double ptmax)
{
  TF1 *hagedorn = new TF1("hagedorn","[0]*([1]/(x+[1])^[2])", 0, 100);

  //  hagedorn->SetParameters(h->GetMaximum(),1.72,10);

  double p0 = 1.72;
  double n_exp = 10.5;

  hagedorn->SetParameters(1E5,p0,n_exp);

  //  hagedorn->SetParLimits(0, 1, 20000000);
  hagedorn->SetParLimits(1, p0, p0);
  hagedorn->SetParLimits(2, n_exp-4, n_exp+4);

  h->Fit(hagedorn,"0+","",ptmin,ptmax);

  h->GetListOfFunctions()->Add(hagedorn);

  return hagedorn;
}

//_____________________________________________________________________________
TF1*
comparator::hagedornFit(TGraphErrors* g, double ptmin, double ptmax,
			const char* functionName, int verbose)
{
  if ( verbose )
    {
      cout << "ptmin=" << ptmin << " ptmax=" << ptmax << endl;
    }

  TF1 *hagedorn = new TF1(functionName,"[0]*([1]/(x+[1])^[2])", ptmin, ptmax);

  double p0 = 1.72;
  double n_exp = 10.5;

  hagedorn->SetParameters(1E5,p0,n_exp);

  //hagedorn->SetParLimits(0, 1, 20000000);
  hagedorn->SetParLimits(1, p0, p0);
  hagedorn->SetParLimits(2, n_exp-4, n_exp+4);

  g->Fit(hagedorn,"0+");

  //  g->GetListOfFunctions()->Add(hagedorn);

  if ( verbose ) 
    {
      printf("A = %e+-%e p0 = %7.4f+- %7.4f n = %7.4f+-%7.4f Chi2/ndf=%7.4f\n",
	     hagedorn->GetParameter(0),hagedorn->GetParError(0),
	     hagedorn->GetParameter(1),hagedorn->GetParError(1),
	     hagedorn->GetParameter(2),hagedorn->GetParError(2),
	     hagedorn->GetChisquare()/hagedorn->GetNDF());
    }
  return hagedorn;
}

//_____________________________________________________________________________
TH1*
comparator::getHistogram(TDirectory* dir, const char* histoname)
{
  if ( strcmp(histoname,"hEfficiency_combined")==0 )
    {
      return getCombined(dir);
    }
  else if ( strcmp(histoname,"hRecoPt")==0 )
    {
      return getRecoPt(dir);
    }

  TKey* k = (TKey*)dir->FindKey(histoname);
  if (!k ) 
    {
      std::cerr << "Cannot find key " << histoname
		<< " in dir " << dir->GetPath()
		<< std::endl;
      return 0;
    }

  return (TH1*)k->ReadObj();
}

//_____________________________________________________________________________
void
comparator::plot(TGraphErrors* g1, TGraphErrors* g2, 
		 const char* basename, const char* method)
{
  double xmax = 0;
  double xmin = 1E30;
  double ymax = 0;
  double ymin = 1E30;

  double npts = g1->GetN();

  if ( g2->GetN() < npts ) 
    {
      npts = g2->GetN();
    }

  bool samepts = true;

  for ( int i = 0; i < npts; i++ )
    {
      if ( g1->GetX()[i] != g2->GetX()[i] )
	{
	  samepts=false;
	}

      if ( g1->GetX()[i] > xmax ) 
	{
	  xmax = g1->GetX()[i];
	}
      if ( g2->GetX()[i] > xmax )
	{
	  xmax = g2->GetX()[i];
	}
       if ( g1->GetX()[i] < xmin ) 
	{
	  xmin = g1->GetX()[i];
	}
      if ( g2->GetX()[i] < xmin )
	{
	  xmin = g2->GetX()[i];
	}

      if ( g1->GetY()[i] > ymax ) 
	{
	  ymax = g1->GetY()[i];
	}
      if ( g2->GetY()[i] > ymax )
	{
	  ymax = g2->GetY()[i];
	}
       if ( g1->GetY()[i] < ymin ) 
	{
	  ymin = g1->GetY()[i];
	}
      if ( g2->GetY()[i] < ymin )
	{
	  ymin = g2->GetY()[i];
	}
    }

  char name[200];
  
  sprintf(name,"%s_%s",method,basename);
  
  TCanvas* c1;

  fCanvasList->Add(c1 = new TCanvas(name,name));

  c1->cd();

  TPad* ptitle = new TPad("ptitle","ptitle",0.01,0.885,0.99,0.98);
  
  TPad* p1 = new TPad("1","1",0.01,0.01,0.499,0.88);
  p1->SetLeftMargin(0.2);
  p1->SetRightMargin(0.01);
  
  TPad* p2 = new TPad("2","2",0.50,0.01,0.999,0.88);
  p2->SetLeftMargin(0.2);
  p2->SetRightMargin(0.01);
  
  ptitle->Draw();
  p1->Draw();
  p2->Draw();
  
  // ----- Make the title
  
  ptitle->cd();
  
  std::string sname;
  
  if ( !fPlotTitle ) 
    {
      sname = basename;
      size_t i = sname.find_first_of('_')+1;
      if ( i < 0 ) i=0;
      
      sname = sname.substr(i);
    }
  else
    {
      sname = fPlotTitle->Data();
    }

  TPaveText* text = new TPaveText(0,0,1,1);
  text->SetTextAlign(22);
  text->SetTextSize(0.5);
  text->SetBorderSize(0);
  text->SetFillColor(0);
  text->AddText(sname.c_str());
  text->Draw();
  
  // ----- The two single spectra
  
  TH2* hframe = new TH2F("hframe","hframe",10,0,xmax*1.2,10,ymin*0.1,ymax*10);
  
  // TH2* hframe = new TH2F("hframe","hframe",10,0,10,10,0,1E6);
  
  p1->cd();
  p1->SetLogy();
  
  hframe->Draw();

  hframe->GetXaxis()->SetTitle("p_{T} (GeV/c)");
  hframe->GetYaxis()->SetTitle("#frac{dN^{#pi^{0}}}{2#piNevents p_{T}dp_{T}d#eta}");
  hframe->GetYaxis()->SetTitleOffset(1.3);
  hframe->GetYaxis()->SetTitleSize(0.06);
  g1->SetMarkerStyle(20);
  g2->SetMarkerStyle(25);

  g1->SetMarkerColor(2);
  g1->SetLineColor(2);

  g1->Draw("P");
  
  g2->Draw("P");

  if ( fLegend1 ) 
    {
      TLegend* legend = new TLegend(0.25,0.14,0.66,0.3);
      legend->SetBorderSize(1);
      legend->SetFillColor(0);
      legend->AddEntry(g1,fLegend1->Data(),"p");
      legend->AddEntry(g2,fLegend2->Data(),"p");
      legend->Draw();
    }
  
  // ---- Ratio of two spectra
  
  p2->cd();

  if ( !samepts ) 
    {
      TF1* f1 = g1->GetFunction("hagedorn2");
      TF1* f2 = g2->GetFunction("hagedorn2");
      
      if ( f1 && f2 ) 
	{
	  TH1* hdiv = new TH1F("hdiv","hdiv",50,0,f2->GetXmax());

	  for ( int i = 1; i <= hdiv->GetXaxis()->GetNbins(); ++i ) 
	    {
	      double pt = hdiv->GetXaxis()->GetBinCenter(i);
	      hdiv->SetBinContent(i, f1->Eval(pt)/f2->Eval(pt));	      
	    }
	  hdiv->Draw("hist");    
	}
      else
	{
	  std::cerr << "Could not find hagedorn2" << endl;
	}
    }
  else
    {
      int N = g1->GetN();

      TGraphErrors* gdiv = new TGraphErrors(N);

      for ( int i = 0; i < N; i++ )
	{
	  double x1,y1,x1err,y1err;
	  double x2,y2,y2err;
	  g1->GetPoint(i,x1,y1);
	  g2->GetPoint(i,x2,y2);
	  double r = y1/y2;
	  gdiv->SetPoint(i,x1,y1/y2);
	  x1err = g1->GetErrorX(i);
	  y1err = g1->GetErrorY(i);
	  y2err = g2->GetErrorY(i);
	  gdiv->SetPointError(i,x1err,r*quadraticRelativeError
			      (y1,y1err,y2,y2err));
	}

      TH2* h = new TH2F("h","h",10,0,gdiv->GetX()[N-1],10,0,2);

      h->GetXaxis()->SetTitle("p_{T} (GeV/c");
      h->GetYaxis()->SetTitle("Ratio");
      h->GetYaxis()->SetTitleOffset(1.3);
      h->GetYaxis()->SetTitleSize(0.06);
  
      h->Draw();

      gdiv->SetMarkerStyle(24);

      gdiv->Draw("P");
    }
  
  sprintf(name,"%s.eps",c1->GetName());
  c1->Print(name);
}

//_____________________________________________________________________________
void
comparator::plot(TH1* h1, TH1* h2, const char* basename, const char* method,
		 bool limits, bool fit)
{
  TString stitle = h1->GetName();
  if ( stitle.Contains("hEfficiency") ) 
    {
      cout << "Applying DeadWarn scaling factor of " 
	   << fScale << endl;
      h1->Scale(fScale);
      h2->Scale(fScale);
    }

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
  //  minimum*=0.8;
  
  h1->SetMaximum(maximum);
  h2->SetMaximum(maximum);
  h1->SetMinimum(minimum);
  h2->SetMinimum(minimum);
  
  char name[200];
  
  sprintf(name,"%s_%s",method,basename);
  
  TCanvas* c1;

  fCanvasList->Add(c1 = new TCanvas(name,name));

  c1->cd();

  TPad* ptitle = new TPad("ptitle","ptitle",0.01,0.885,0.99,0.98);

  TPad* p1 = new TPad("1","1",0.01,0.01,0.499,0.88);
  p1->SetLeftMargin(0.15);
  p1->SetRightMargin(0.01);

  TPad* p2 = new TPad("2","2",0.50,0.01,0.999,0.88);
  p2->SetLeftMargin(0.15);
  p2->SetRightMargin(0.01);

  ptitle->Draw();
  p1->Draw();
  p2->Draw();

  // ----- Make the title

  ptitle->cd();

  std::string sname;

  if ( !fPlotTitle ) 
    {
      sname = basename;
      size_t i = sname.find_first_of('_')+1;
      if ( i < 0 ) i=0;
      
      sname = sname.substr(i);
    }
  else
    {
      sname = fPlotTitle->Data();
    }

  TPaveText* text = new TPaveText(0,0,1,1);
  text->SetTextAlign(22);
  text->SetTextSize(0.5);
  text->SetBorderSize(0);
  text->SetFillColor(0);
  text->AddText(sname.c_str());
  text->Draw();

  // ----- The two single spectra

  h1->SetMarkerStyle(20);
  h1->SetMarkerColor(1);

  sprintf(name,basename);
  h1->SetTitle(name);
  h1->GetXaxis()->SetTitle("p_{T} (GeV/c)");
  int b1=h1->GetXaxis()->FindBin(1.0);
  int b2=h1->GetXaxis()->FindBin(7.0);

  h1->GetXaxis()->SetRange(b1,b2);

  if ( stitle.Contains("hEfficiency") ) 
    {
      h1->GetYaxis()->SetTitle("#epsilon");
    }
  else if ( stitle.Contains("Peak") )
    {
      h1->GetYaxis()->SetTitle("#pi^{0} peak position (GeV/c^{2})");
      h1->SetMaximum(0.160);
      h1->SetMinimum(0.120);
    }
  else if ( stitle.Contains("Sigma") )
    {
      h1->GetYaxis()->SetTitle("#pi^{0} peak #sigma (GeV/c^{2})");
      h1->SetMaximum(0.030);
      h1->SetMinimum(0.000);
     }

  h1->GetYaxis()->SetTitleOffset(1.0);
  h1->GetYaxis()->SetTitleSize(0.06);
  h2->GetYaxis()->SetTitleOffset(0.5);
  h2->GetYaxis()->SetTitleSize(0.06);

  h1->GetXaxis()->SetTitleSize(0.04);
  h2->GetXaxis()->SetTitleSize(0.04);

  p1->cd();

  p1->SetLogy(fLogy);

  h1->Draw("P");
  h2->SetMarkerStyle(25);
  h2->SetMarkerColor(1);
  h2->SetLineColor(1);
  h2->Draw("PSAME");

  if ( fLegend1 ) 
    {
      TLegend* legend = new TLegend(0.25,0.14,0.66,0.3);
      legend->SetBorderSize(1);
      legend->SetFillColor(0);
      legend->AddEntry(h1,fLegend1->Data(),"p");
      legend->AddEntry(h2,fLegend2->Data(),"p");
      legend->Draw();
    }

  // ---- Ratio of two spectra

  TH1* hdiv = (TH1*)h1->Clone();
  hdiv->SetMarkerColor(1);
  hdiv->SetMarkerStyle(27);
  hdiv->SetTitle("");
  hdiv->GetYaxis()->SetTitle("Ratio");
  p2->cd();
  hdiv->Divide(h2);

  minmax(hdiv,minimum,maximum,1,7);

  //  if ( maximum>2 ) maximum=2.0;

  maximum *= 1.2;
  
  if ( minimum > 1 ) minimum=1.0;

  if ( minimum < 0 ) minimum=0.0;

  minimum *= 0.8;
  hdiv->SetMaximum(maximum);
  hdiv->SetMinimum(minimum);
  hdiv->Draw("histe");

  if ( fFit ) 
    {
      hdiv->Fit(fFit->Data(),"QR","",1,7);
    }

  if (limits) 
    {
      minmax(hdiv,minimum,maximum,1,5);
      
      TLine* lmin = new TLine(1,minimum,5,minimum);
      TLine* lmax = new TLine(1,maximum,5,maximum);     
      lmin->Draw();
      lmax->Draw();
    }

  sprintf(name,"%s.eps",c1->GetName());
  c1->Print(name);
}

//_____________________________________________________________________________
void
comparator::ls(void)
{
  if ( fFile1 ) 
    {
      fFile1->ls();
    }
}

//_____________________________________________________________________________
void 
comparator::minmax(TH1* h, double& minimum, double& maximum,
		   double xmin, double xmax)
{
  minimum=1E30;
  maximum=0;

  int b1 = h->GetXaxis()->FindBin(xmin);
  int b2 = h->GetXaxis()->FindBin(xmax);

  for ( int i = b1; i <= b2 ; ++i ) 
    {
      double content = h->GetBinContent(i);
      double error = h->GetBinError(i);
      if ( content+error > maximum ) maximum = content+error;
      if ( content-error < minimum && content > 0 ) minimum = content-error;
    }
}

//_____________________________________________________________________________
double
comparator::integral(TGraphErrors& g, double xmin, double xmax)
{
  double xint=0.0;

  for ( int i = 0; i < g.GetN(); ++i ) 
    {
      double x = g.GetX()[i];
      if ( x >= xmin && x <= xmax ) 
	{
	  xint += g.GetY()[i];
	}
    }
  return xint;
}

//_____________________________________________________________________________
void 
comparator::setEfficiencyScale(double scale)
{
  fScale=scale;
}

//_____________________________________________________________________________
void
comparator::setFit(const char* function)
{
  fFit = new TString(function);
}

//_____________________________________________________________________________
void
comparator::setLegends(const char* legend1, const char* legend2)
{
  delete fLegend2;
  delete fLegend1;

  fLegend1= new TString(legend1);
  fLegend2= new TString(legend2);  
}

//_____________________________________________________________________________
void
comparator::setTitle(const char* title)
{
  delete fPlotTitle;
  fPlotTitle = new TString(title);
}

//_____________________________________________________________________________
void
comparator::scale(TGraphErrors& g, double a)
{
  for ( int i = 0; i < g.GetN(); ++i ) 
    {
      double xi,yi,xierr,yierr;
      g.GetPoint(i,xi,yi);
      g.SetPoint(i,xi,yi*a);
      xierr = g.GetErrorX(i);
      yierr = g.GetErrorY(i);
      g.SetPointError(i,xierr,yierr*a);
    }
}

//_____________________________________________________________________________
double
comparator::shiftPt(TGraphErrors& g, TF1* hagedorn)
{
  double xi,yi;

  for ( int i = 0; i < g.GetN(); i++ )
    {
      g.GetPoint(i,xi,yi);
      xi = getMeanPt(xi,0.5,hagedorn);
      g.SetPoint(i,xi,yi);
    }

  return xi;
}
