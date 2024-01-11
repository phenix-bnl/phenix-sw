#include <iostream>
#include <iomanip>
#include <fstream>
#include "TMath.h"
#include "TF1.h"
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TGraphErrors.h"

//_____________________________________________________________________________

using namespace std;

static const Int_t CentClasses = 20;
static const Int_t MaxPTbins = 18;

static const Float_t Events[CentClasses] = { 1325956, 1633974, //  0-10
					     1640068, 1671280, // 10-20
					     3276173/2, 3276173/2, // 20-30
					     3265392/2, 3265392/2, // 30-40
					     3324897/2, 3324897/2, // 40-50
					     3285326/2, 3285326/2, // 50-60
					     3331919/2, 3331919/2, // 60-70
					     3264595/2, 3264595/2, // 70-80
					     4022149/2, 0, 0, // 80-92, -
					     30041726};  // last = min. bias

const Float_t mean_pt_corr = 0.04;
const Float_t twoPi = 2.*TMath::Pi();
const Float_t pt_binwidth = 0.5 ;

//_____________________________________________________________________________
Int_t getCentralityClass(const Int_t Cent1, const Int_t Cent2)
{

  Int_t CC = Cent1/5;

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
Float_t getEvents(const Int_t Cent1, const Int_t Cent2)
{
  Int_t CC = getCentralityClass(Cent1, Cent2);

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

void get_pT_mean_corr( const Int_t Cent1 = 0, const Int_t Cent2 = 5)
{

  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  Int_t CC = getCentralityClass(Cent1,Cent2);
  Float_t eventsCC = getEvents(Cent1,Cent2);
  cout << " Calculating mean bin pT of the pi0 spectrum for centrality class: " << CC 
       << " (" << Cent1 << " - " << Cent2 << "%)" << endl;

  char title[300];
  sprintf(title,"Centrality Class %d_%d",Cent1,Cent2);
  //if (Cent1==100) sprintf(title,"Centrality Class: min. bias");

  //_____________________________________________________________________________
  
  char ffile[300];
  char name1[300]; 
  char name4[300]; 
  const char* basename = "/afs/rhic/phenix/users/enterria/CORRECTED_RUN2/";
  sprintf(name1,"yield_raw_pure_cent%d_%d",Cent1,Cent2);
  sprintf(name4,"yieldfinal_t0a0_cent%d_%d",Cent1,Cent2);

  sprintf(ffile,"%s%s.txt",basename,name4);
  ifstream ifiledata(ffile);
  if (!ifiledata)
    {
      cout << " Can't open file: " << ffile << endl;
      cout << " Are you sure that the centrality class: " << Cent1 << " - " << Cent2 << "% exists ?" << endl;
      return ;
    }
  else cout << " Reading " << ffile  << " ..." << endl;

  Float_t pTpizero[MaxPTbins];
  Float_t epTpizero[MaxPTbins];
  Float_t countspizero[MaxPTbins];
  Float_t ecountspizero[MaxPTbins];
  Float_t bidon[MaxPTbins];

  Int_t i = 0;
  // Read pT, countsPi0 from the file
  while ( ifiledata >> 
	  pTpizero[i] >> 
	  countspizero[i] >> 
	  ecountspizero[i] >>
	  bidon[i]
	  ){ 
//     cout << "  " << pTpizero[i]
// 	 << "  " << countspizero[i] << " +/- " << ecountspizero[i] << endl;
    i++;
  }
  ifiledata.close();

  const Int_t NpTbins = i;
   
  Float_t raw_yield[NpTbins];
  Float_t eraw_yield[NpTbins];

  Float_t pt_mean = 0.;

  for (Int_t pTbin = 0; pTbin < NpTbins ; pTbin++) 
    { 

      pt_mean = pTpizero[pTbin]-mean_pt_corr;
      epTpizero[pTbin] = 0.01 ; // const pT bin width = 0.5 GeV/c

      // Pure raw spectrum
      raw_yield[pTbin]   = countspizero[pTbin]/(eventsCC*pt_binwidth*twoPi);
      eraw_yield[pTbin]  = TMath::Power(ecountspizero[pTbin]/countspizero[pTbin],2) ; // stat. error
      eraw_yield[pTbin]  = TMath::Sqrt( eraw_yield[pTbin] )*raw_yield[pTbin];
      
      cout << "  " << pt_mean << "+/-" << epTpizero[pTbin] 
	   << "  " << countspizero[pTbin] << " +/- " << ecountspizero[pTbin];

	  cout << " --> " << setprecision(4) << raw_yield[pTbin] << " +/- " <<  eraw_yield[pTbin] << endl;
	  cout << " --> " << setprecision(4) << raw_yield[pTbin] << " +/- " <<  eraw_yield[pTbin] << endl;

    }
  
  //_____________________________________________________________________________

  TCanvas *c1; TGraphErrors *RawPi0;

  c1 = new TCanvas(name1,name1,600,600);
  Float_t max_yield4 = 10.*raw_yield[0];
  Float_t min_yield4 = 0.1*raw_yield[NpTbins-1];
  
  RawPi0 = new TGraphErrors( NpTbins, pTpizero, raw_yield, epTpizero, eraw_yield);
  RawPi0->SetMaximum(max_yield4);
  RawPi0->SetMinimum(min_yield4);
  RawPi0->SetMarkerStyle(20);
  RawPi0->SetMarkerColor(2);
  //RawPi0->SetMarkerStyle(8);
  RawPi0->Draw("AP");
  RawPi0->SetTitle(title);
  RawPi0->GetYaxis()->SetTitle("RAW dN^{#pi^{0}}/dp_{T}");
  RawPi0->GetXaxis()->SetTitle("#pi^{0} p_{T} (GeV/c)");
  c1->SetLogy();
  c1->Update();

 //_____________________________________________________________________________
  
  Float_t ptmin = 1.0;
  Float_t ptmax = pt_mean;
  Float_t p0 = 1.72;
  Float_t n_exp = 10.5;
  TF1 *hagedorn = new TF1("hagedorn","[0]*([1]/(x+[1])^[2])", ptmin, ptmax);
  hagedorn->SetParameters(10000., p0, n_exp);
  hagedorn->SetParLimits(0, 1, 20000000);
  hagedorn->SetParLimits(1, p0, p0);
  //hagedorn->SetParLimits(1, 1.9, 3.36);
  hagedorn->SetParLimits(2, n_exp-4, n_exp+4);
  //hagedorn->SetParLimits(2, 9.408,9.408); 
  hagedorn->SetParNames("A Hag","p_o", "n");
  hagedorn->Draw("same");

  c1->Update(); 
  hagedorn->SetParLimits(1, 0.5, 1.5);
  RawPi0->Fit("hagedorn","QREIL","E");

  Float_t A_Hag, p_0, n, more ;  
  cout << "Fit result: " << endl; 
  A_Hag = hagedorn->GetParameter(0) ;   cout << "A Hag =" << A_Hag <<"+-" << hagedorn->GetParError(0) << endl ;
  p_0   = hagedorn->GetParameter(1) ;   cout << "p_0   =" << p_0 <<"+-" <<  hagedorn->GetParError(1) << endl;
  n     = hagedorn->GetParameter(2) ;   cout << "n     =" << n <<"+-" <<  hagedorn->GetParError(2) << endl;
  cout << "Chi2/Ndf  =" << hagedorn->GetChisquare()/hagedorn->GetNumberFitPoints() << endl;
  hagedorn->Draw("same");
  
  TF1 *localhagedorn = new TF1("localhagedorn","[0]*([1]/(x+[1])^[2])", ptmin, ptmax);
  localhagedorn->SetParameters(10000., p0, n_exp,0.);
  localhagedorn->SetParLimits(0, 1, 20000000);
  localhagedorn->SetParLimits(1, p0, p0);
  localhagedorn->SetParLimits(2, n_exp-4, n_exp+4);
  localhagedorn->SetParNames("A Hag","p_o", "n");
  localhagedorn->Draw("same");
  
  TF1 *xhagedorn  = new TF1("xhagedorn", "[0]*x*([1]/(x+[1])^[2])", ptmin, ptmax);
  xhagedorn->SetParameter(0,A_Hag);
  xhagedorn->SetParameter(1,p_0);
  xhagedorn->SetParameter(2,n);
  xhagedorn->SetLineColor(2);
  TF1 *xhagedorn1  = new TF1("xhagedorn1", "[0]*x*([1]/(x+[1])^[2])", ptmin, ptmax);
  xhagedorn1->SetParameter(0,A_Hag);
  xhagedorn1->SetParameter(1,p_0);
  xhagedorn1->SetParameter(2,n+hagedorn->GetParError(2));
  xhagedorn1->SetLineColor(2);
  TF1 *xhagedorn2  = new TF1("xhagedorn2", "[0]*x*([1]/(x+[1])^[2])", ptmin, ptmax);
  xhagedorn2->SetParameter(0,A_Hag);
  xhagedorn2->SetParameter(1,p_0);
  xhagedorn2->SetParameter(2,n-hagedorn->GetParError(2));
  xhagedorn2->SetLineColor(2);
  //    xhagedorn->Draw("same");
  TF1 *hagedorn1  = new TF1("hagedorn1", "[0]*([1]/(x+[1])^[2])", ptmin, ptmax);
  hagedorn1->SetParameter(0,A_Hag);
  hagedorn1->SetParameter(1,p_0);
  hagedorn1->SetParameter(2,n+hagedorn->GetParError(2));
  hagedorn1->SetLineColor(2);
  hagedorn1->Draw("same");
  TF1 *hagedorn2  = new TF1("hagedorn2", "[0]*([1]/(x+[1])^[2])", ptmin, ptmax);
  hagedorn2->SetParameter(0,A_Hag);
  hagedorn2->SetParameter(1,p_0);
  hagedorn2->SetParameter(2,n-hagedorn->GetParError(2));
  hagedorn2->SetLineColor(4);
  hagedorn2->Draw("same");
  
  Float_t pT;
  
  for (Int_t pTbin = 0; pTbin < NpTbins ; pTbin++) 
    {
      cout << "pTbin " << pTbin << " pT " << pTpizero[pTbin] << " Mean is " << 
	xhagedorn->Integral(pTpizero[pTbin]-0.25,pTpizero[pTbin]+0.25)/hagedorn->Integral(pTpizero[pTbin]-0.25,pTpizero[pTbin]+0.25) 
	   << " "  <<
	xhagedorn1->Integral(pTpizero[pTbin]-0.25,pTpizero[pTbin]+0.25)/hagedorn1->Integral(pTpizero[pTbin]-0.25,pTpizero[pTbin]+0.25) 
	   << " "  <<
	xhagedorn2->Integral(pTpizero[pTbin]-0.25,pTpizero[pTbin]+0.25)/hagedorn2->Integral(pTpizero[pTbin]-0.25,pTpizero[pTbin]+0.25) <<
	endl;
    }

  c1->Update();
  
} 

//_____________________________________________________________________________

void all_pT_mean_corr()
{
  Int_t Cent1 = 0;
  Int_t Cent2 = 10;

  for (Int_t i=0; i<=CentClasses; i+=2)
    {
      if (i==20) 
	{
	  get_pT_mean_corr(100,100);
	}
      else if (Cent1 == 80)
	{ 
	  get_pT_mean_corr(Cent1,92);
	}
      else
	{ 
	  get_pT_mean_corr(Cent1,Cent2);
	}
      Cent1 +=10;
      Cent2 = Cent1+10;
    }
}
//_____________________________________________________________________________

