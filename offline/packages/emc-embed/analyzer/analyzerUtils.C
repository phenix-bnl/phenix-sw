// $Header
//-----------------------------------------------------------------------------
//
//  Author: D. d'Enterria
//  Date: 2005
//  Nevis Labs. Columbia University
//
//-----------------------------------------------------------------------------

#include <iostream>
#include <fstream>
#include <cassert>
#include <strstream>
#include <cstring>
#include <iomanip>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <climits>
#include <vector>

#include "TH1.h"
#include "TH2.h"
#include "TF1.h"
#include "TMath.h"
#include "TGraphErrors.h"
#include "TClonesArray.h"
#include "TString.h"
#include "TSystem.h"
#include "TCanvas.h"
#include "TBox.h"

using namespace std;
using namespace TMath;

//static const int MAXCOLS = 2;

namespace analyzerUtils
{

static const double crazyValue = -99999.;

//_____________________________________________________________________________
//
double 
chi2( TF1 *ref, TGraphErrors *data )
{

  double chiSq = -999.;

  if (!ref || !data) return chiSq;

  double x, y, ey, fy;

  size_t N = data->GetN();

  for (size_t ii = 0; ii < N; ii++)
    {
      data->GetPoint(ii, x, y);
      ey = data->GetErrorY(ii);
      fy = ref->Eval(x);
      chiSq += (fy - y)*(fy - y)/(ey*ey);
    }

  chiSq /= N;

  return chiSq;
}

//_____________________________________________________________________________
//
double 
chi2( TF1 *ref, TGraphErrors*data, double start )
{

  double chiSq = -999.;

  if (!ref || !data) return chiSq;

  double x, y, ey, fy;

  size_t N = data->GetN();

  for (size_t ii = 0; ii < N; ii++)
    {
      data->GetPoint(ii, x, y);
      ey = data->GetErrorY(ii);
      fy = ref->Eval(x);
      if (x > start)
        chiSq += (fy - y)*(fy - y)/(ey*ey);
    }

  chiSq /= N;

  return chiSq;
}

//_____________________________________________________________________________
//
double
quadRelatError(  const double ea = 0., const double a = 1.,
		 const double eb = 0., const double b = 1.,
		 const double ec = 0., const double c = 1.,
		 const double ed = 0., const double d = 1.,
		 const double ee = 0., const double e = 1. )
{

  double qrelaterror = 0.;

  if (a!=0)
    {
      qrelaterror = (ea/a)*(ea/a);
      if (b!=0) qrelaterror += (eb/b)*(eb/b);
      if (c!=0) qrelaterror += (ec/c)*(ec/c);
      if (d!=0) qrelaterror += (ed/d)*(ed/d);
      if (e!=0) qrelaterror += (ee/e)*(ee/e);
    }

  return Sqrt(qrelaterror);

}

//_____________________________________________________________________________
//
// Given an array xx[0..n-1], and given a value x, returns a value j such that x is between
// xx[j] and xx[j+1].  xx must be monotonic, either increasing or decreasing.  j=-1 or j = n 
// is returned to indicate that x is out of range.
//
long valueToArrayIndex(const double xx[], const unsigned long n, const double x)
{

  long ju, jm, jl;
  int ascnd;

  /* Initialze lower and upper limits */

  jl = -1;
  ju = n;

  ascnd = (xx[n-1] >= xx[0]);

  while (ju-jl > 1) {
    jm = (ju+jl) >> 1;
    if (x >= xx[jm] == ascnd)
      jl=jm;
    else
      ju=jm;
  }
  if (x == xx[0]) return 0;
  else if (x == xx[n-1]) return (n-1);
  else return jl;

}

//_____________________________________________________________________________
//
// 3 ways to define a matrix:
// (1) double* matrix = new double[rows*cols]; double elem_ij = matrix[i*rows+j]; // [row][col]
// (2) double** matrix = new double*[10]; for(int i=0; i<10;++i) matrix[i] = new double[10]; double elem_ij = matrix[i][j]
// (3) vector<vector<double> > matrix.
//
// but we are doing something else here ... 
// Original array is filled as:
// double* array[2];                                                                           
// int k = 0; for(int i=0;i<2;i++){ array[i] = new double[3]; for(int j=0;j<3;j++) { array[i][j] = k++;};}
// so we have to transpose it the following way ...
// 
void transposeDoubleArray(double* array[2], double *array2[], const int N)
{

  for(short i = 0; i < N; i++)
    {
      array2[i] = new double[2];

      for(short j = 0; j < 2; j++)
	{
	  array2[i][j] = array[j][i];
	  //cout << "(row,col)=(" << i << "," << j << ") : " << array[j][i] << " ---> " << array2[i][j] << endl;
	}                             
    }

  return;
}

//_____________________________________________________________________________
//double weighted_average( const std::vector<double> vals, const std::vector<double> errs )
//
double* 
weighted_average( const double *vals, 
		  const double *errs, const int N )
{

  const int M = 4;
  double *av_err_chi2 = new double[M];

  for (int i=0 ; i<M; i++) { av_err_chi2[i] = -999.; }

  if (!vals || !errs || !N) return av_err_chi2;

  double w_average = 0., norm_average = 0. , err_average = 0., norm_err_average = 0., chi2 = 0. ;

  for (int i=0 ; i<N ; i++)
    {
      err_average += 1./(errs[i]*errs[i]);
      w_average += vals[i]/(errs[i]*errs[i]);
      norm_average += vals[i];
      norm_err_average += errs[i]*errs[i];
    }

  w_average /= err_average;
  norm_average /= N;
  err_average = 1./sqrt(err_average);
  norm_err_average = sqrt(norm_err_average);

  for (int i=0 ; i<N ; i++)
    {
      chi2 += pow( w_average - vals[i] , 2. )/(errs[i]*errs[i]);
    }

  av_err_chi2[0] = w_average;
  av_err_chi2[1] = err_average;
  av_err_chi2[2] = norm_average;
  av_err_chi2[3] = chi2/(N-1);

  cout << " Weighted average = " << av_err_chi2[0] << " +/- " << av_err_chi2[1] << endl
       << " ---   chi2/(N-1) = " << av_err_chi2[3] << endl
       << " --- Std. average = " << av_err_chi2[2] << " +/- " << norm_err_average << endl;

  return av_err_chi2;

}

//_____________________________________________________________________________
//
//
double* 
weighted_average_2measurements( const double *vals, 
				const double *errs, 
				double *stat_errs = 0, 
				double *fully_corr_err = 0, 
				double *syst_errA = 0, 
				double *syst_errB = 0,
				double *syst_errC = 0,
				int verbose = 0 )
{

  double *total_array = new double[8];
  for (int i=0 ; i<8; i++) { total_array[i] = 0.; }

  if (!vals || !errs) 
    {
      //return total_array;
      cout << "<I> Example (D0 top-quark mass): " << endl 
	   << "double vals[2] = {168.4,180.1}; double errs[2] = {12.8,5.3};" << endl
	   << "double stat_errs[2] = {12.3,3.6}; double fully_corr_err[2] = {3.4,3.4};" << endl
	   << "analyzerUtils::weighted_average_2measurements(vals,errs,stat_errs,fully_corr_err);" << endl;
      return total_array;
    }

  // initialization

  const int N = 2;
  if (!stat_errs)
    {
      stat_errs = new double[N];
      for (int i=0 ; i<N ; i++){ stat_errs[i] = 0.; }
    }
  if (!fully_corr_err) 
    {
      fully_corr_err = new double[N];      
      for (int i=0 ; i<N ; i++){ fully_corr_err[i] = 0.; }
    }
  if (!syst_errA)
    {
      syst_errA = new double[N];      
      for (int i=0 ; i<N ; i++){ syst_errA[i] = 0.; }
    }
  if (!syst_errB)
    {
      syst_errB = new double[N];      
      for (int i=0 ; i<N ; i++){ syst_errB[i] = 0.; }
    }
  if (!syst_errC)
    {
      syst_errC = new double[N];      
      for (int i=0 ; i<N ; i++){ syst_errC[i] = 0.; }
    }

  // consistency-check first:

  for (int i=0 ; i<N ; i++){ 
    if (fully_corr_err[i]>errs[i])
      {
	cout << "<E> Correlated error bigger than total error !?!? Doing nothing ..." << endl;
	return total_array;
      }
     if ( (stat_errs[i]>errs[i]) ) 
       {
	 cout << "<E> Stat. error bigger than total error !?!? Doing nothing ..." << endl;
	return total_array;
      }
    if ( (syst_errA[i]>errs[i]) || (syst_errB[i]>errs[i]) || (syst_errC[i]>errs[i]) )
      {
	cout << "<E> Syst. error(s) bigger than total error !?!? Doing nothing ..." << endl;
	return total_array;
      }
  }

  // total correlated and uncorrelated errors

  double corr_err_sq = (fully_corr_err[0]*fully_corr_err[1]);

  double uncorr_err1_sq = abs(errs[0]*errs[0]-corr_err_sq);
  double uncorr_err2_sq = abs(errs[1]*errs[1]-corr_err_sq);

  // weights

  double w_1 = uncorr_err2_sq/(uncorr_err1_sq+uncorr_err2_sq);
  double w_2 = uncorr_err1_sq/(uncorr_err1_sq+uncorr_err2_sq);

  // weighted average and error

  double w_average = w_1*vals[0]+w_2*vals[1];

  double err_average = (errs[0]*errs[0]*errs[1]*errs[1]-corr_err_sq*corr_err_sq)/(uncorr_err1_sq+uncorr_err2_sq);
  err_average = sqrt(err_average);

  // combined statistical error

  //double stat_err_average = uncorr_err2_sq*(stat_errs[0]*stat_errs[0]);
  //stat_err_average += uncorr_err1_sq*(stat_errs[1]*stat_errs[1]);
  //stat_err_average = sqrt(stat_err_average)/(uncorr_err1_sq+uncorr_err2_sq) ;

  double stat_err_average = pow(w_1*stat_errs[0],2.)+pow(w_2*stat_errs[1],2.);
  stat_err_average = sqrt(stat_err_average);

  // combined systematical error

  double syst_err_average = sqrt(err_average*err_average-stat_err_average*stat_err_average);

  // combined systematical errors

  double syst_errA_average = w_1*syst_errA[0]+w_2*syst_errA[1];
  double syst_errB_average = w_1*syst_errB[0]+w_2*syst_errB[1];
  double syst_errC_average = w_1*syst_errC[0]+w_2*syst_errC[1];

 // chi2 test

  double chi2 = (errs[1]*errs[1])*pow(w_average - vals[0],2.);
  chi2 += (errs[0]*errs[0])*pow(w_average - vals[1],2.);
  chi2 -= 2*corr_err_sq*(w_average - vals[0])*(w_average - vals[1]);
  chi2 /= (errs[0]*errs[0]*errs[1]*errs[1]-corr_err_sq*corr_err_sq);

  // final complete array 

  total_array[0] = w_average;
  total_array[1] = err_average;
  total_array[2] = stat_err_average;
  total_array[3] = syst_err_average;
  total_array[4] = syst_errA_average;
  total_array[5] = syst_errB_average;
  total_array[6] = syst_errC_average;
  total_array[7] = chi2;

  // dump

  if (verbose)
    {
      cout << " Weighted average = " << w_average << " +/- " << err_average << endl
	   << " --- stat error   = +/- " << stat_err_average << endl
	   << " --- syst error   = +/- " << syst_err_average << endl
	   << " --- syst error1  = +/- " << syst_errA_average << endl
	   << " --- syst error2  = +/- " << syst_errB_average << endl
	   << " --- syst error3  = +/- " << syst_errC_average << endl
	   << " ---        chi2  = " << chi2 << endl;
    }

  return total_array;

}

//_____________________________________________________________________________
// returns val[line][column]
// e.g.:   val[0][1] = 2nd value of 1st column

double**
readAscii( const char* datafile, 
	   int &ncols,
	   int &nlines,
	   int skiplines,
	   const int verbose )
{

  // accessing ncols and nlines directly not recommended for now: set to zero ...
  ncols = 0;
  nlines = 0;

  // default values
  int const maxcols  =  50;
  int const maxlines = 1000;
  int const maxlinewidth = 256;

  int emptylines = 0;

  char file[400];
  sprintf(file,"%s",datafile);

  strstream strline;

  char line[maxlinewidth];
  int linesize = sizeof(line);

  int totlinecounter = 0, totcolcounter = 0;
  int k = 0;

  double firstline[maxcols];

  double **val = 0;

  ifstream ifiledata(file);
  if (!ifiledata)
    {
	  cout << " <E> analyzerUtils::readAscii : Can't open file: " << file << endl;
	  return val;
    }
  ifiledata.close();
  if (verbose) 
    {
      cout << " <I> analyzerUtils::readAscii : Trying to read " << file  
	   << " (skipping the first " << skiplines << " lines) ..." << endl;
    }

  // input ncols and/or nlines given
  if ((!(ncols > 0)) || (!(nlines > 0))) 
    {
      ifiledata.open(file, ios::in);

      // Reading first line and determining the number of columns
      if (!(ncols > 0)) 
	{
	  while (k++<=skiplines) // skip first line(s)
	    {
	      ifiledata.getline(line,linesize);
	      totlinecounter++;
	    }
	  strline << line;

	  do{
	    if  (totcolcounter>maxcols) 
	      {
		cout << " <E> First line unreadable : " << file 
		     << " (Try skipping a few lines ? Too long rows/columns ?)" << endl;
		return val; // strange files
	      }
	    strline >> firstline[totcolcounter++];
	  } while (!strline.eof());

	  ncols = totcolcounter-1;
	  strline.clear();

	}
      
      // Reading the rest of the file and determining the number of lines
      if (!(nlines > 0)) 
	{
	  do {
	    if  (totlinecounter>maxlines)
	      {
		cout << " <E> Unreadable file: " << file 
		     << " (Try skipping a few lines ?)" << endl;
		return val; // strange files
	      }
	    ifiledata.getline(line,linesize);
	    //if (verbose > 1) cout << ": " << line << endl;
	    //printf("line length: %d\n",strlen(line));
	    if ( (!ifiledata.eof()) && (strlen(line) <= 2) ) emptylines++; // empty line (= line with <= 2 characters)
	    totlinecounter++;
	  } while ( (!ifiledata.eof()) && (emptylines<2) ); // we stop reading if we find >=2 empty lines in file

	  if (ifiledata.eof()) nlines = totlinecounter-1;
	  else nlines = totlinecounter;

	  strline.clear();
	}
      
      ifiledata.close();
    }
  
  if (nlines<skiplines) skiplines = nlines;

  // reopening the file for real read
  ifiledata.open(file, ios::in);

  // allocating memory
  val = new double*[ncols];
  for (int j=0;j<ncols;j++)
    val[j] = new double[nlines-skiplines-emptylines];

  // reading file
  for (int i=0; i<nlines; i++) 
    {
      ifiledata.getline(line,linesize);

      if ( strlen(line) > 2 ) // skip empty line(s)
	{
	  if (i>=skiplines)  // skip the first line(s)
	    {
	      strline << line;
	      for (int j=0;j<ncols;j++) 
		{
		  strline >> val[j][i-skiplines];
		  
		  if (verbose>1) 
		    { 
		      printf("[%d][%d]=%.1e ",j,i-skiplines,val[j][i-skiplines]);
			     //cout << "[" << j << "][" << i-skiplines << "]=" 
			     //	   << val[j][i-skiplines] << "  " ;
		    }
		}
	      if (verbose>1) { cout << endl; }
	    }
	}
      strline.clear();
	  
      if ( i && (i%1000)==0) { cout << i << endl; }
    }

  ifiledata.close();

  if (verbose)
    {
      cout << " <I> analyzerUtils::readAscii: " << file << endl;
      cout << "  -- ncols: " << ncols << endl;
      cout << "  -- nlines (total): " << nlines << endl;
      cout << "  -- skipped lines (on top): " << skiplines << endl;
      cout << "  -- empty lines (on bottom): " << emptylines ;
      if (emptylines>1) cout << " (didn't actually read the file beyond the 2 last empty lines).";
      cout << endl;
    }

  nlines-=(skiplines+emptylines);

  return val;

}

//_____________________________________________________________________________
//
// function to read data from an ascii file and create an histogram
// the file can have 2-3-4 columns of float data.
// it assumes that the values in the file are ordered increasingly
// IMPORTANT: only works if the data values have fixed binwidth !

TH1F* readAsciiToHisto( const char *filename = "data.txt", 
			const int ncols = 3,
			const char *format = "x-y-ex-ey",
			const int verbose = 1 )
{

#include "Riostream.h"

   ifstream in;
   in.open(filename, ios::in);

   float x,y,ex;
   float ey = 0;
   int nbins = 0;
   double xmin = 0.;
   double xmax = 0.;
   double halfbinwidth = 0.;

   // first read to determine number of bins
   // we don't care about format (as long as 1st column is x ...)
   while (1) {

     if ( ncols==2 )
       {
	 in >> x >> y;
	 if (verbose) printf(" x=%8f, x2=%8f\n",x,y);
       }
     else if (ncols==3)
       {
	 in >> x >> y >> ey;
	 if (verbose) printf(" x=%8f, x2=%8f, x3=%8f\n",x,y,ey);
       }
     else if (ncols==4)
       {
	 in >> x >> y >> ex >> ey;
	 if (verbose) printf(" x=%8f, x2=%8f, x3=%8f, x4=%8f\n",x,y,ex,ey);
       }

     if (nbins==0) xmin = x; // assuming ascii file is ordered increasingly
     if (nbins==1) halfbinwidth = 0.5*(x-xmin); // assuming ascii file has fixed binwidth

      if (!in.good()) break;
      nbins++;
   }
   printf(" found %d points\n",nbins);
   in.close();

   xmax = x; // last value read

   char *hname = (char*)gSystem->BaseName(filename);

   TH1F *hascii = new TH1F(hname,hname,nbins,xmin-halfbinwidth,xmax+halfbinwidth);
   printf(" booking histo %s with %d bins: xmin=%8f, xmax=%8f (binwidth=%8f) \n",
	  hname,nbins,xmin,xmax,halfbinwidth*2.);

   TString format_str = format;

   // second read to fill histo
   in.open(filename, ios::in);
   while (1) {

     if ( format_str.Contains("x-y-ex-ey") )
       {
	 if (ncols==2)
	   in >> x >> y;
	 else if (ncols==3)
	   in >> x >> y >> ex;
	 else if (ncols==4)
	   in >> x >> y >> ex >> ey;
       }
     else if ( format_str.Contains("x-ex-y-ey") )
       {
	 if (ncols==2)
	   in >> x >> ex;
	 else if (ncols==3)
	   in >> x >> ex >> y;
	 else if (ncols==4)
	   in >> x >> ex >> y >> ey;
       }
     else
       {
	 cout << " <E> unknown format: " << format << endl;
	 break;
       }

      if (!in.good()) break;

      int bin = hascii->GetXaxis()->FindBin(x);
      hascii->SetBinContent(bin,y);
      if (ey==0) ey=1.e-20; // set a minimum error
      hascii->SetBinError(bin,ey);
   }

   in.close();

   if (verbose) hascii->Print("all");

   float xmax2 = hascii->GetBinCenter(nbins); // last valid value of the histo (0=underflow, (nbins+1)=overflow)
   float epslon = xmax*10e-5;
   if (abs(xmax-xmax2)>epslon) // checking quasi-equality with floats
     {
       cout << endl << " <W> Problems filling the histo. "
	    << "probably: (a) some bin missing, or (b) some bin with different width ..." << endl ;
     }

   return hascii;
}

//_____________________________________________________________________________
// 
void dumpHisto( const TH1 *h, 
		const char *format = "x-y-ex-ey" )
{
  if (!h) return;
  
  const size_t N = h->GetNbinsX();
  TString format_str = format;

  for (size_t i=1;i<=N;i++) // we start at i=1 since i=0 is for underflows
    {
      double x = h->GetBinCenter(i); 
      double y = h->GetBinContent(i);
      double ex = h->GetBinWidth(i)/2.;
      double ey = h->GetBinError(i);

      if ( format_str.Contains("x-y-ex-ey") )
	{
	  printf("%4.2f   %g   %g   %g\n",x,y,ex,ey);
	}
      else if ( format_str.Contains("x-ex-y-ey") )
	{
	  printf("%4.2f   %g   %g   %g\n",x,ex,y,ey);
	}
      else if ( format_str.Contains("x-y-ey") )
	{
	  printf("%4.2f   %g   %g\n",x,y,ey);
	}
    }

}

//_____________________________________________________________________________
// 
void dumpGraph( TGraphErrors *gr, 
		const char *format = "x-y-ex-ey" )
{
  if (!gr) return;
  
  size_t N = gr->GetN();
  TString format_str = format;

  double X[N],Y[N],eX[N],eY[N];
  double x,y,ex,ey;

  for (size_t i=0;i<N;i++) 
    {
      gr->GetPoint(i,x,y);
      ex = gr->GetErrorX(i);
      ey = gr->GetErrorY(i);

      if ( format_str.Contains("x-y-ex-ey") )
	{
	  printf("%4.2f   %g   %g   %g \n",x,y,ex,ey);
	}
      else if ( format_str.Contains("x-ex-y-ey") )
	{
	  printf("%4.2f   %g   %g   %g \n",x,ex,y,ey);
	}
      else if ( format_str.Contains("x-y-ey") )
	{
	  printf("%4.2f   %g   %g \n",x,y,ey);
	}
      else if ( format_str.Contains("x") )
	{
	  printf("%4.2f \n",x);
	}
      else if ( format_str.Contains("array") )
	{
	  X[i] = x;
	  Y[i] = y;
	  eX[i] = ex;
	  eY[i] = ey;
	}
      else if ( format_str.Contains("y") )
	{
	  printf("%g \n",y);
	}
    }

  if ( format_str.Contains("array") )
    {
      size_t i = 0;
      printf("double x[%d]={",N); for (i=0;i<N-1;i++) printf("%4.3f, ",X[i]); printf("%4.3f};\n",X[N-1]);
      printf("double y[%d]={",N); for (i=0;i<N-1;i++) printf("%.2e, ",Y[i]); printf("%.2e};\n",Y[N-1]);
      printf("double ex[%d]={",N); for (i=0;i<N-1;i++) printf("%4.2f, ",eX[i]); printf("%4.2f};\n",eX[N-1]);
      printf("double ey[%d]={",N); for (i=0;i<N-1;i++) printf("%.2e, ",eY[i]); printf("%.2e};\n",eY[N-1]);
    }

}

//_____________________________________________________________________________
//
void 
dumpFunction( TF1 *f1 )
{
  if (!f1) return;

  int N = f1->GetNpar();
  double *params = (double*)f1->GetParameters();

  cout << "TF1 *" << f1->GetName() << " = new TF1(\"" << f1->GetName() << "\",\""
       << f1->GetExpFormula() << "\","
       << f1->GetXmin() << "," << f1->GetXmax() << ");" << endl;

  cout << f1->GetName() << "->SetParameters(";
  for(int i=0;i<N;i++)
    {
      cout << params[i];
      if (i<(N-1))cout << ",";
    }
  cout << ");" << endl;

}

//_____________________________________________________________________________
//
void
dumpLaTeX( TGraphErrors *g1,
	   double *eStat, double *eSyst,
	   double *eCCuncorr, double *epTcorr)
{
  if (!g1) return ;

  char *title = (char*)g1->GetTitle();
  char *space2 = "\\hspace{2mm}";
  char *space5 = "\\hspace{5mm}";
  char *space9 = "\\hspace{9mm}";

  bool AddOneError = false; // for eNcoll case
  bool fullErrors = false;

  printf("\n\\begin{table}[ht] \n");
  printf("\\begin{center} \n");

  // 2 loops: one for absolute errors, the second for relative

  for (int loop = 1; loop <= 2; loop++)
    {
      if (eStat && eSyst && eCCuncorr && epTcorr)
        {
          fullErrors = true;

          if (loop == 1)
            {
              printf("\\begin{tabular}{|c|c|c|c|c|c|c|}\\hline\\hline \n");
              printf("%s $p_T$ (GeV/c) %s & %s $N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}_{stat.}$ %s & %s $\\Delta N^{\\pi^0}_{sys.}$ %s & %s $\\Delta N^{\\pi^0}_{CC-uncorr.}$ %s & %s $\\Delta N^{\\pi^0}_{p_T-corr.}$ %s \\\\\\hline \n"
                     , space2, space2, space2, space2, space2, space2, space2, space2, space2, space2, space2, space2, space2, space2);

              // simpler errors output
              //       printf("%s $p_T$ (GeV/c) %s & %s $N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}_{stat.}$ %s & %s $\\Delta N^{\\pi^0}_{sys.}$ %s & %s $\\Delta N^{\\pi^0}_{CC-uncorr.}$ %s & %s $\\Delta N^{\\pi^0}_{p_T-corr.}$ %s \\\\\\hline \n"
              // 	     ,space5,space5,space5,space5,space9,space9,space5,space5,space5,space5,space5,space5,space5,space5);
            }
          else if (loop == 2)
            {
              printf("\\hline \n");
              printf("%s $p_T$ (GeV/c) %s & %s $N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}_{stat.}/N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}_{sys.}/N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}_{CC-uncorr.}/N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}_{p_T-corr.}/N^{\\pi^0}$ %s \\\\\\hline \n"
                     , space2, space2, space2, space2, space2, space2, space2, space2, space2, space2, space2, space2, space2, space2);
            }

        }

      else if (eStat) // for eNcoll case
        {
          AddOneError = true;

          if (loop == 1)
            {
              printf("\\begin{tabular}{|c|c|c|c|c|c|}\\hline\\hline \n");
              printf("%s $p_T$ (GeV/c) %s & %s $R_{AA}$ %s  & %s $\\Delta R_{AA}$ %s  & %s $\\Delta R_{AA}/R_{AA}$ %s & %s $\\Delta N_{norm}$ %s & %s $\\Delta N_{norm}/R_{AA}$ %s \\\\\\hline \n"
                     , space5, space5, space5, space5, space5, space5, space5, space5, space5, space5, space5, space5);
            }
        }

      else // 2 errors only
        {
          if (loop == 1)
            {
              printf("\\begin{tabular}{|c|c|c|}\\hline\\hline \n");
              printf(" %s $p_T$ (GeV/c) %s & %s $N^{\\pi^0}$ %s & %s $\\Delta N^{\\pi^0}$ %s \\\\\\hline \n"
                     , space5, space5, space5, space5, space9, space9);
            }
        }

      // loop on the error index
      for (int i = 0; i < g1->GetN(); i++)
        {
          double x = 0.;
          double y = 0.;
          g1->GetPoint(i, x, y);
          double ex = g1->GetErrorX(i);
          double ey = g1->GetErrorY(i);
          double eyrelat = ey/y*100.;

          if (fullErrors)
            {
              // simpler error output
              //printf("  %.2f $\\pm$  %.2f  &  %.3e & %.3e (%.1f\\%%) & %.3e & %.3e & %.3e & %.3e \\\\ \n",
              //	 x,ex,y,ey,eyrelat,eStat[i],eSyst[i],eCCuncorr[i],epTcorr[i]);

              if (loop == 1)
                {
                  printf("  %.2f $\\pm$  %.2f  &  %.3e & %.3e & %.3e & %.3e & %.3e & %.3e \\\\ \n",
                         x, ex, y, ey, eStat[i], eSyst[i], eCCuncorr[i], epTcorr[i]);
                }
              else if (loop == 2)
                {
                  printf("  %.2f $\\pm$  %.2f  &  %.3e & %.1f\\%% & %.1f\\%% & %.1f\\%% & %.1f\\%% & %.1f\\%% \\\\ \n",
                         x, ex, y, eyrelat, eStat[i]/y*100., eSyst[i]/y*100., eCCuncorr[i]/y*100., epTcorr[i]/y*100.);
                }
            }
          else if (AddOneError && (loop == 1) )
            {
              printf("  %.2f $\\pm$  %.2f  &  %.3f & %.3f & %.1f\\%% & %.3f  & %.1f\\%% \\\\ \n",
                     x, ex, y, ey, eyrelat, eStat[i], eStat[i]/y*100.);
            }
          else if (loop == 1)
            {
              printf("  %.2f $\\pm$  %.2f  &  %.3e & %.3e (%.1f\\%%) \\\\ \n",
                     x, ex, y, ey, eyrelat);
            }
        }
    }

  printf("\\hline\\hline \n");
  //  printf("\\vspace{5mm} \n");
  printf("\\end{tabular} \n");
  printf("\\caption{$%s$.}\n", title);
  printf("\\label{tab:%s_yields} \n", title);
  printf("\\end{center} \n");
  printf("\\end{table} \n");

}

//_____________________________________________________________________________
//
void
dumpAscii( TGraphErrors *g1,
	   double *eStat, double *eSyst,
	   double *eCCuncorr, double *epTcorr)
{
  if (!g1) return ;

  bool AddOneError = false; // for eNcoll case
  bool fullErrors = false;

  // 2 loops: one for absolute errors, the second for relative

  for (int loop = 1; loop <= 2; loop++)
    {
      if (eStat && eSyst && eCCuncorr && epTcorr)
        {
          fullErrors = true;

          if (loop == 1)
            {
              printf("   pT    epT        Npi0       eNpi0    eNpi0_stat    eNpi0_sys    eNpi0_ccuncorr    eNpi0_pTcorr.\n");
            }
          else if (loop == 2)
            {
              printf(" \n\n");
              printf("   pT    epT        Npi0       eNpi0%%    eNpi0_stat%%    eNpi0_sys%%   eNpi0_ccuncorr%%   eNpi0_pTcorr%%\n");
            }

        }

      else if (eStat) // for eNcoll case
        {
          AddOneError = true;

          if (loop == 1)
            {
              //printf(" \n");
              printf("   pT    epT    R_AA     eR_AA    eR_AA(%%)    eNcoll    eNcoll(%%)\n");
            }
        }

      else // 2 errors only
        {
          if (loop == 1)
            {
              printf(" \n");
              printf("   p_T    epT     Npi0    eNpi0  \n");
            }
        }

      // loop on the error index
      for (int i = 0; i < g1->GetN(); i++)
        {
          double x = 0.;
          double y = 0.;
          g1->GetPoint(i, x, y);
          double ex = g1->GetErrorX(i);
          double ey = g1->GetErrorY(i);
          double eyrelat = ey/y*100.;

          if (fullErrors)
            {
              if (loop == 1)
                {
                  printf("  %.2f   %.2f      %.3e    %.3e    %.3e    %.3e    %.3e    %.3e  \n",
                         x, ex, y, ey, eStat[i], eSyst[i], eCCuncorr[i], epTcorr[i]);
                }
              else if (loop == 2)
                {
                  printf("  %.2f   %.2f      %.3e    %.2f    %.2f    %.2f    %.2f    %.2f  \n",
                         x, ex, y, eyrelat, eStat[i]/y*100., eSyst[i]/y*100., eCCuncorr[i]/y*100., epTcorr[i]/y*100.);
                }
            }
          else if (AddOneError && (loop == 1) )
            {
              printf("  %.2f   %.2f      %.3f    %.3f    %.2f    %.3f    %.2f  \n",
                     x, ex, y, ey, eyrelat, eStat[i], eStat[i]/y*100.);
            }
          else if (loop == 1)
            {
              printf("  %.2f   %.2f      %.3e    %.3e    %.2f  \n",
                     x, ex, y, ey, eyrelat);
            }
        }
    }

  cout << endl;

}

//_____________________________________________________________________________
//
void
getXminmax( TGraphErrors* g, 
	    double& Xmin, double& Xmax,
	    const bool consider_errors = false )
{
  Xmin=1.0e+30;
  Xmax=-1.0e+30;

  if (!g) return;

  for ( int i = 0; i < g->GetN(); ++i ) 
    { 
      double x,y;
      g->GetPoint(i, x, y);
      double ex = g->GetErrorX(i);
      if (consider_errors)
	{
	  if ( x+ex > Xmax ) Xmax = x+ex;
	  if ( x-ex < Xmin && x > 0 ) Xmin = x-ex;
	  else if ( x < Xmin ) Xmin = x;
	}
      else
	{
	  if ( x > Xmax ) Xmax = x;
	  if ( x < Xmin ) Xmin = x;
	}
    }

  //cout << "getXminmax of " << g->GetTitle() <<  " : (Xmin,Xmax)=" << Xmin << "," << Xmax << endl; 
}

//_____________________________________________________________________________
//
void
getYminmax( TGraphErrors* g, 
	    double& Ymin, double& Ymax )
{
  Ymin=1.0e+30;
  Ymax=-1.0e+30;

  if (!g) return;

  int b1 = 0;
  int b2 = g->GetN()-1;

  for ( int i = b1; i <= b2 ; ++i ) 
    {
      double x,y;
      double ey = g->GetErrorY(i);
      g->GetPoint(i, x, y);

      if ( y+ey > Ymax ) Ymax = y+ey;
      if ( y-ey < Ymin && y-ey > 0 ) Ymin = y-ey;
      else if ( y < Ymin ) Ymin = y;
    }

  //c5->Close();

  //cout << "getYminmax of " << g->GetTitle() <<  " : (Ymin,Ymax)=" << Ymin << "," << Ymax << endl; 

}

//_____________________________________________________________________________
//
void
getXYminmax( TGraphErrors* g,
	     double& Xmin, double& Xmax,
	     double& Ymin, double& Ymax )
{

  if (!g) return;

  getXminmax( g, Xmin, Xmax);

  //cout << "getXYminmax: (Xmin,Xmax)=" << Xmin << "," << Xmax << endl; 

  getYminmax( g, Ymin, Ymax);//, xmin1, xmax1 );

  //cout << "getXYminmax: (Ymin,Ymax)=" << Ymin << "," << Ymax << endl; 

}

//_____________________________________________________________________________
//
void
getXYminmax( TGraphErrors* g1,
	     TGraphErrors* g2,
	     double& Xmin, double& Xmax,
	     double& Ymin, double& Ymax )
{

  if (!g1 || !g2) return;

  double xmin1 = 0.5; 
  double xmax1 = 12.5;
  getXminmax( g1, xmin1, xmax1);

  double xmin2 = 0.5;
  double xmax2 = 12.5;
  getXminmax( g2, xmin2, xmax2);

  Xmin = Min(xmin1,xmin2);
  Xmax = Max(xmax1,xmax2);

  //cout << "getXYminmax: (Xmin,Xmax)=" << Xmin << "," << Xmax << endl; 

  double ymin1 = 0.; 
  double ymax1 = 30.;
  getYminmax( g1, ymin1, ymax1);//, xmin1, xmax1 );

  double ymin2 = 0.;
  double ymax2 = 30.;
  getYminmax( g2, ymin2, ymax2);//, xmin2, xmax2);

  Ymin = Min(ymin1,ymin2);
  Ymax = Max(ymax1,ymax2);

  //cout << "getXYminmax: (Ymin,Ymax)=" << Ymin << "," << Ymax << endl; 

}

//_____________________________________________________________________________
//
double 
getYatX( TGraphErrors* g, 
	 const double X,
	 const char *type = "value" )
{

  if (!g) return crazyValue;
 
  const int N = g->GetN();
  double x = 0., y = 0.;
  double prevx = 0., prevy = 0.;

  TString type_str = type ;

  for ( int i = 0; i < N; ++i ) 
    { 
      g->GetPoint(i, x, y);
      double ey = g->GetErrorY(i);
      if (i>0) g->GetPoint(i-1, prevx, prevy);
      //cout << " Looking for Yvalue at X = " << X 
      //	   <<  " ... (x,y)=(" << x << "," << y << "+/-" << ey << ")" << endl;

      double xbinwidth = (x-prevx)/2;

      if ( (x-xbinwidth <= X) && (x+xbinwidth > X) )
	{
	  cout << " Found Yvalue at X = " << X 
	       <<  " --> (x,y)=(" << x << "," << y << "+/-" << ey << ")" << endl;
	  if (type_str.Contains("value")) return y; 
	  else if (type_str.Contains("error")) return ey;
	}
    }

  cout << " <E> analyzerUtils::getYAtX: No y value found for x = " << X 
       << " of " << g->GetName() << ". Returning " << crazyValue << endl;

  return crazyValue;
}

//_____________________________________________________________________________
// Make a TGraphErrors out of h0 (if g0 is there, use its errors)
//
TGraphErrors*
histo2graph( TH1* h0, 
	     TGraphErrors* g0 )
{

  if (!h0) return 0;

  int N = 0;

  if (g0)
    {
      N = g0->GetN();
    }
  else
    {
      N = h0->GetNbinsX();
    }

  double* x = new double[N];
  double* ex = new double[N];
  double* y = new double[N];
  double* ey = new double[N];

  for ( int i = 0; i < N; i++ )
    {
      double xi = 0; double yi = 0;
      double exi = 0; double eyi = 0;

      if (g0)
	{
	  xi = g0->GetX()[i];
	  int bin = h0->GetXaxis()->FindBin(xi);
	  yi = h0->GetBinContent(bin);
	  exi = g0->GetErrorX(i);
	  eyi = g0->GetErrorY(i);
	}
      else
	{
	  xi = h0->GetBinCenter(i); 
	  yi = h0->GetBinContent(i);
	  exi = h0->GetBinWidth(i)/2.;
	  eyi = h0->GetBinError(i);
	}

      x[i] = xi;
      y[i] = yi;
      ey[i] = y[i] * (eyi/yi);
      ex[i] = exi;

    }

  TGraphErrors* g2 = new TGraphErrors(N,x,y,ex,ey);

  delete x ;
  delete ex ;
  delete y ;
  delete ey ;

  return g2;
}

//_____________________________________________________________________________
//
TF1*
hagedornInit( const char* funcname = "hagedorn",
	      const double ptmin = 2., 
	      const double ptmax = 10.,
	      const int verbose = 0,
	      const double AHag = 1000.,
	      const double p0 = 1.72, 
	      const double n_exp = 10.5 )
{

  if ( verbose )
    {
      cout << " <I> (unconstrained) hagedornInit: ptmin=" << ptmin << " ptmax=" << ptmax
	   << " A_input=" << AHag << " p0_fixed=" << p0 << " n_input=" << n_exp << endl;
    }

  TF1 *hagedorn = new TF1(funcname,"[0]/(1+x/[1])^[2]", ptmin, ptmax);
  //hagedorn = new TF1(funcname,"[0]/(x+[1])^[2])", ptmin, ptmax); // fit more sensitive to initial pars. values
  
  hagedorn->SetParameters(AHag, p0, n_exp);
  hagedorn->SetParNames("A_{Hag}", "p_{0}", "n");
  
  hagedorn->SetParLimits(0, 0.0001, 200000000);
  hagedorn->SetParLimits(1, p0, p0);
  hagedorn->SetParLimits(2, n_exp-6., n_exp+6.);

  hagedorn->SetLineWidth(1);

  //hagedorn->Print();

  return hagedorn;

}
//_____________________________________________________________________________
//
TF1*
hagedornConstrainedInit( const char* funcname = "constrhagedorn",
			 const double ptmin = 2., 
			 const double ptmax = 10.,
			 const int verbose = 0,
			 const double AHag = 100.,
			 const double ptmean = 0.39, 
			 const double n_exp = 10.5  )
{

  //cout << " <I> <p_T> for the fit ?" << endl; 
  //cin >> ptmean;

  if ( verbose )
    {
      cout << " <I> hagedornConstrainedInit: ptmin=" << ptmin << " ptmax=" << ptmax
	   << " A_input=" << AHag << ", <pT>_fixed=" << ptmean << " [p0 = <pT>/2*(n-3)], n_input=" << n_exp << endl;
    }

  TF1 *hagedorn =  new TF1(funcname,"[0]/(1+x/([1]/2*([2]-3)))^[2]", ptmin, ptmax);

  hagedorn->SetParameters(AHag, ptmean, n_exp);
  hagedorn->SetParNames("A_{Hag}", "p_{T}", "n");
  
  hagedorn->SetParLimits(0, 0.0001, 200000000);
  hagedorn->SetParLimits(1, ptmean, ptmean);
  hagedorn->SetParLimits(2, n_exp-6., n_exp+6.);

  hagedorn->SetLineWidth(1);

  //hagedorn->Print();

  return hagedorn;

}

//_____________________________________________________________________________
//
TF1*
hagedornConstrainedFit( TH1* h, 
			const char* funcname = "constrhagedorn",
			const double ptmin = 2., 
			const double ptmax = 10.,
			const int verbose = 1,
			const double AHag = 1000.,
			const double ptmean = 0.39,
			const double n_exp = 10.5 )
{

  if (!h) return 0;

  TF1 *hagedorn = (TF1*)hagedornConstrainedInit(funcname, ptmin, ptmax, verbose, AHag, ptmean, n_exp);

  if (verbose>1) h->Fit(hagedorn,"REIM0+","",ptmin, ptmax);
  else h->Fit(hagedorn,"QREIM0+","",ptmin, ptmax); // Quiet

  h->GetListOfFunctions()->Add(hagedorn);

  double A  = hagedorn->GetParameter(0);
  double eA = hagedorn->GetParError(0);
  double p_0  = hagedorn->GetParameter(1);
  double ep_0 = hagedorn->GetParError(1);
  double n  = hagedorn->GetParameter(2);
  double en = hagedorn->GetParError(2);
  double chi2 = hagedorn->GetChisquare()/hagedorn->GetNDF();

  // constrained fit: p0 = <pT>/2*(n-3) with <pT>=0.4 GeV/c
  p_0  = p_0/2*(n-3); 
  ep_0 = ep_0/2*(n-3);

  if ( verbose ) 
    {
      printf(" <I> hagedornFit (pT=%.2f-%.2f GeV/c2):",ptmin,ptmax);
      printf(" A = %e +- %e p0 = %7.4f +- %7.4f n = %7.4f +- %7.4f Chi2/ndf = %7.4f\n",
	     A,eA,p_0,ep_0,n,en,chi2);
    }

  return hagedorn;

}

//_____________________________________________________________________________
//
TF1*
hagedornFit( TH1* h, 
	     const char* funcname="hagedorn",
	     const double ptmin = 2., 
	     const double ptmax = 10.,
	     const int verbose = 1,
	     const double AHag = 1000.,
	     const double p0 = 1.72,
	     const double n_exp = 10.5 )
{

  if (!h) return 0;

  TF1 *hagedorn = (TF1*)hagedornInit(funcname, ptmin, ptmax, verbose, AHag, p0, n_exp);

  if (verbose>1) h->Fit(hagedorn,"REIM0+","",ptmin, ptmax);
  else h->Fit(hagedorn,"QREIM0+","",ptmin, ptmax); // Quiet

  h->GetListOfFunctions()->Add(hagedorn);

  double A  = hagedorn->GetParameter(0);
  double eA = hagedorn->GetParError(0);
  double p_0  = hagedorn->GetParameter(1);
  double ep_0 = hagedorn->GetParError(1);
  double n  = hagedorn->GetParameter(2);
  double en = hagedorn->GetParError(2);
  double chi2 = hagedorn->GetChisquare()/hagedorn->GetNDF();

  if ( verbose ) 
    {
      printf(" <I> hagedornFit (pT=%.2f-%.2f GeV/c2):",ptmin,ptmax);
      printf(" A = %e +- %e p0 = %7.4f +- %7.4f n = %7.4f +- %7.4f Chi2/ndf = %7.4f\n",
	     A,eA,p_0,ep_0,n,en,chi2);
    }

  return hagedorn;

}

//_____________________________________________________________________________
//
TF1*
hagedornConstrainedFit( TGraphErrors* g, 
			const char* funcname = "constrhagedorn",
			const double ptmin = 2., 
			const double ptmax = 10.,
			const int verbose = 1,
			const double AHag = 1000.,
			const double ptmean = 0.39,
			const double n_exp = 10.5 )
{

  if (!g) return 0;

  TF1 *hagedorn = (TF1*)hagedornConstrainedInit(funcname, ptmin, ptmax, verbose, AHag, ptmean, n_exp);
 
  if (verbose>1) g->Fit(hagedorn,"0+","",ptmin, ptmax); // Option "R" or ptmin-ptmax needed even deleting previous functions ...
  else g->Fit(hagedorn,"Q0+","",ptmin, ptmax); // Quiet

  g->GetListOfFunctions()->Add(hagedorn);

  double A  = hagedorn->GetParameter(0);
  double eA = hagedorn->GetParError(0);
  double p_0  = hagedorn->GetParameter(1);
  double ep_0 = hagedorn->GetParError(1);
  double n  = hagedorn->GetParameter(2);
  double en = hagedorn->GetParError(2);
  double chi2 = hagedorn->GetChisquare()/hagedorn->GetNDF();

  // constrained fit: p0 = <pT>/2*(n-3) with <pT>=0.4 GeV/c
  p_0  = p_0/2*(n-3); 
  ep_0 = ep_0/2*(n-3);

  if ( verbose ) 
    {
      printf(" <I> hagedornFit (pT=%.2f-%.2f GeV/c2): A = %e +- %e p0 = %7.4f +- %7.4f n = %7.4f +- %7.4f Chi2/ndf = %7.4f\n",
	     ptmin,ptmax,A,eA,p_0,ep_0,n,en,chi2);
    }

  return hagedorn;

}

//_____________________________________________________________________________
//
TF1*
hagedornFit( TGraphErrors* g, 
	     const char* funcname = "hagedorn",
	     const double ptmin = 2., 
	     const double ptmax = 10.,
	     const int verbose = 1,
	     const double AHag = 1000.,
	     const double p0 = 1.72,
	     const double n_exp = 10.5 )
{

  if (!g) return 0;

  //g->GetListOfFunctions()->Delete(); // the option "+" in Fit deletes any previous function

  TF1 *hagedorn = (TF1*)hagedornInit(funcname, ptmin, ptmax, verbose, AHag, p0, n_exp);
  
  if (verbose>1) g->Fit(hagedorn,"0+","",ptmin, ptmax); // Option "R" or ptmin-ptmax needed even deleting previous functions ...
  else g->Fit(hagedorn,"Q0+","",ptmin, ptmax); // Quiet

  g->GetListOfFunctions()->Add(hagedorn);

  double A  = hagedorn->GetParameter(0);
  double eA = hagedorn->GetParError(0);
  double p_0  = hagedorn->GetParameter(1);
  double ep_0 = hagedorn->GetParError(1);
  double n  = hagedorn->GetParameter(2);
  double en = hagedorn->GetParError(2);
  double chi2 = hagedorn->GetChisquare()/hagedorn->GetNDF();

  if ( verbose ) 
    {
      printf(" <I> hagedornFit (pT=%.2f-%.2f GeV/c2): A = %e +- %e p0 = %7.4f +- %7.4f n = %7.4f +- %7.4f Chi2/ndf = %7.4f\n",
	     ptmin,ptmax,A,eA,p_0,ep_0,n,en,chi2);
    }

  return hagedorn;

}

//_____________________________________________________________________________
// Simplified version of the function above. Changes:
// (1) Keep one single bin-shift method
// (2) USE the new Hagedorn function: "[0]/(1+x/[1])^[2]" instead of "[0]/(x+[1])^[2])"

void
getBinShiftSimple( const double ptini, 
		   const double ptbinwidth, 
		   TF1* hagedorn,
		   std::vector<double> &xyshift)
{

  if (!hagedorn) return;
  if (!ptbinwidth) return;

  double ptmean = 0.;
  double yup = 0.;

  double s = ptbinwidth/2.0;

  double *hagedPars = (double*)hagedorn->GetParameters();
  double p_0  = hagedorn->GetParameter(1);
  double n  = hagedorn->GetParameter(2);

  // inverse of hagedorn

  //TF1 invhagedorn("invhagedorn", "([0]/x)^(1/[2])-[1]",0.,100.); // for Hagedorn defined as "[0]/(x+[1])^[2])"

  TF1 invhagedorn("invhagedorn", "[1]*([0]/x)^(1/[2])-[1]",0.,100.); // for Hagedorn defined as "[0]/(1+x/[1])^[2]"
  invhagedorn.SetParameters(hagedPars);

  // For "constrained fit" where: p0 = <pT>/2*(n-3) with <pT>~0.3 GeV/c

  TString hag_type = hagedorn->GetName();
  if (hag_type.Contains("constr"))
    {
      invhagedorn.SetParameter(1,p_0/2*(n-3));
    }

  yup = hagedorn->Integral(ptini-s,ptini+s);
  yup /= ptbinwidth;

  ptmean = invhagedorn.Eval(yup);

  double yini = hagedorn->Eval(ptini);
  double relatshift = yini/yup*100.;
  printf(" (pTmean,yieldshift) = ( %.2f,%.1f%% ) \n",ptmean,relatshift);

  xyshift.push_back(ptmean);
  xyshift.push_back(yup);

  return;
}

//_____________________________________________________________________________
//
void
getBinShiftPowLaw( const double ptini, 
		   const double ptbinwidth, 
		   TF1* pow,
		   std::vector<double> &xyshift)
{

  if (!pow) return;
  if (!ptbinwidth) return;

  double ptmean = 0.;
  double yup = 0.;

  double s = ptbinwidth/2.0;

  double *hagedPars = (double*)pow->GetParameters();

  // inverse of power-law

  TF1 invpow("invpow", "([0]/x)^(1/[1])",0.,100.);
  invpow.SetParameters(hagedPars);

  yup = pow->Integral(ptini-s,ptini+s);
  yup /= ptbinwidth;

  ptmean = invpow.Eval(yup);

  double yini = pow->Eval(ptini);
  double relatshift = yini/yup*100.;
  printf(" (pTmean,yieldshift) = ( %.2f,%.1f%% ) \n",ptmean,relatshift);

  xyshift.push_back(ptmean);
  xyshift.push_back(yup);

  return;
}

//_____________________________________________________________________________
//
double
shiftPt( TGraphErrors *g, 
	 TF1* hagedorn,
	 const double ptbinwidth = 0.5,
	 const bool undoInvYieldNormFirst = false )
{
  double xin = 0.;
  double yin = 0.;
  std::vector<double> xyshift(2);
  double xout = 0.;

  // WARNING: This function as it is now does *NOT* change the x-errors when shifting along the x-axis (It's OK like this)

  for ( int i = 0; i < g->GetN(); i++ )
    {
      g->GetPoint(i,xin,yin);

      if (undoInvYieldNormFirst)
	{
	  yin *= xin; // we multiply by pT the inv. yield (1/pT dN/dpT) first
	}

      xyshift.clear();
      getBinShiftSimple(xin,ptbinwidth,hagedorn,xyshift);
      xout = xyshift[0];

      if (undoInvYieldNormFirst)
	{
	  yin /= xin; // we divide back by pT to get the inv. yield (1/pT dN/dpT)
	}

      g->SetPoint(i,xout,yin);
    }

  return xin;
}

//_____________________________________________________________________________
//
double
shiftPtPowLaw( TGraphErrors *g, 
	       TF1* pow,
	       const double ptbinwidth = 0.5,
	       const bool undoInvYieldNormFirst = false )
{
  double xin = 0.;
  double yin = 0.;
  std::vector<double> xyshift(2);
  double xout = 0.;

  // WARNING: This function as it is now does *NOT* change the x-errors when shifting along the x-axis (It's OK like this)

  for ( int i = 0; i < g->GetN(); i++ )
    {
      g->GetPoint(i,xin,yin);

      if (undoInvYieldNormFirst)
	{
	  yin *= xin; // we multiply by pT the inv. yield (1/pT dN/dpT) first
	}

      xyshift.clear();
      getBinShiftPowLaw(xin,ptbinwidth,pow,xyshift);
      xout = xyshift[0];

      if (undoInvYieldNormFirst)
	{
	  yin /= xin; // we divide back by pT to get the inv. yield (1/pT dN/dpT)
	}

      g->SetPoint(i,xout,yin);
    }

  return xin;
}

//_____________________________________________________________________________
//
double
shiftYield( TGraphErrors *g, 
	    TF1* hagedorn,
	    const double ptbinwidth = 0.5,
	    const bool undoInvYieldNormFirst = false )
{

  double xin = 0.;
  double yin = 0.;
  double exin = 0.;
  double eyin = 0.;
  std::vector<double> xyshift(2);
  double yout = 0.;
  double eyout = 0.;

  if (!g) return 0.;

//   // for local hagedorn fit correction at very high-pT
//   bool hiptspectrum = false;
//   double hipt = g->GetX()[g->GetN()-1];
//   if ( hipt > 9. ) hiptspectrum = true;

  for ( int i = 0; i < g->GetN(); i++ )
    {
      g->GetPoint(i,xin,yin);

      // check binwidth argument coincides with true binwidth
      if (i>0) 
	{
	  double xtmp = 0, ytmp = 0;
	  g->GetPoint(i-1,xtmp,ytmp);
	  if ((xin-xtmp)!=ptbinwidth) 
	    cout << "<W> shiftYield(...): pT binwidth is not " << ptbinwidth 
		 << " ! Check your bin-shift correction for bin pT=" << xin << endl;
	}

      exin = g->GetErrorX(i);
      eyin = g->GetErrorY(i);

      if (undoInvYieldNormFirst)
	{
	  yin *= xin; // we multiply by pT the inv. yield (1/pT dN/dpT) first
	  eyin *= xin; // idem for the error

	  g->SetPoint(i,xin,yin);
	  g->SetPointError(i,exin,eyin);
	}

      xyshift.clear();
      getBinShiftSimple(xin,ptbinwidth,hagedorn,xyshift);
      double yup = xyshift[1];
      
      //if ( xin>1.5 && yup)
      //{
	  yout = yin*(hagedorn->Eval(xin)/yup); // standard method
	  //printf(" yout = %.3e*(%.3e/%.3e) = %.3e  ---- ",yin,hagedorn->Eval(xin),xyshift[1],yout);
      //}

//       // WARNING: yup *is* already the ratio expo->Eval(xin)/yup in this case !!
//       if ( xin<1.5 ) // || (hiptspectrum && xin >= 7.0) )
// 	{
// 	  yout = yin*yup;      
// 	  //printf(" yout  = %.3e*%.3e =  %.3e  ---- ",yin,yup,yout);
// 	}

      if (undoInvYieldNormFirst)
	{
	  yout /= xin; // we divide back by pT to get the inv. yield (1/pT dN/dpT)
	}

      if (yin) eyout = eyin*(yout/yin);
      //printf(" eyout = %.3e*(%.3e/%.3e) =  %.3e\n",eyin,yout,yin,eyout);

      g->SetPoint(i,xin,yout);
      g->SetPointError(i,exin,eyout);
    }

  return yout;

}
//_____________________________________________________________________________
//
double
shiftYieldPowLaw( TGraphErrors *g, 
		  TF1* pow,
		  const double ptbinwidth = 0.5,
		  const bool undoInvYieldNormFirst = false )
{

  double xin = 0.;
  double yin = 0.;
  double exin = 0.;
  double eyin = 0.;
  std::vector<double> xyshift(2);
  double yout = 0.;
  double eyout = 0.;

  if (!g) return 0.;

  for ( int i = 0; i < g->GetN(); i++ )
    {
      g->GetPoint(i,xin,yin);
      exin = g->GetErrorX(i);
      eyin = g->GetErrorY(i);

      if (undoInvYieldNormFirst)
	{
	  yin *= xin; // we multiply by pT the inv. yield (1/pT dN/dpT) first
	  eyin *= xin; // idem for the error

	  g->SetPoint(i,xin,yin);
	  g->SetPointError(i,exin,eyin);
	}

      xyshift.clear();
      getBinShiftPowLaw(xin,ptbinwidth,pow,xyshift);
      double yup = xyshift[1];
      
      yout = yin*(pow->Eval(xin)/yup); // standard method

      if (undoInvYieldNormFirst)
	{
	  yout /= xin; // we divide back by pT to get the inv. yield (1/pT dN/dpT)
	}

      if (yin) eyout = eyin*(yout/yin);

      g->SetPoint(i,xin,yout);
      g->SetPointError(i,exin,eyout);
    }

  return yout;

}

//_____________________________________________________________________________
//
void
scale( TGraphErrors& g, 
       const double a = 1.,
       const double ea = 0., 
       const bool verbose = true )
{

  if (verbose)
    {
      cout << " <I> analyzerUtils::scale : Scaling graph " 
	   << g.GetName() << " by factor: " ;
      if (a<1.) cout << " 1/" << 1/a << endl ;
      else cout << a << endl ;
     }

  if (a==1) return; // no need to scale

  for ( int i = 0; i < g.GetN(); ++i ) 
    {
      double xi,yi,exi,eyi;
      g.GetPoint(i,xi,yi);
      exi = g.GetErrorX(i);
      eyi = g.GetErrorY(i);

      eyi = quadRelatError(eyi,yi,ea,a);

      g.SetPoint(i,xi,yi*a);
      g.SetPointError(i,exi,yi*a*eyi);
    }

  // refit the scaled TGraph

  if ( g.GetFunction("hagedorn") || g.GetFunction("haged") ||
       g.GetFunction("hagedornpp") || g.GetFunction("hagedornascii") ||
       g.GetFunction("constrhagedorn") || g.GetFunction("constrhagedornascii") ||
       g.GetFunction("scaledhagedorn") ) // contains a fit
    {

      g.GetListOfFunctions()->Delete();
      double Xmin,Xmax;
      getXminmax((TGraphErrors*)(&g),Xmin,Xmax);
      int verbose = 0;
      hagedornFit((TGraphErrors*)(&g),"scaledhagedorn",Xmin,Xmax,verbose);
    }

}

//_____________________________________________________________________________
//
TGraphErrors*
rebin( TGraphErrors* g, 
       const int rebin = 2 )
{

  if (!g) return 0;

  int N = g->GetN();

  int Nrebin = (int)(N/rebin);
  TGraphErrors *grebin = new TGraphErrors(Nrebin);

  for ( int i=0; i<Nrebin; i++ ) // i loops over rebinned indices
    {
      double x=0;//g->GetX()[i];
      double y=0;//g->GetY()[i];
      double ex=0;//g->GetErrorX(i);
      double ey=0;//g->GetErrorY(i)*g->GetErrorY(i)/(y*y);

      for ( int j = i*rebin; j < rebin*(i+1); j++ ) // j loops over original indices
	{
	  //cout << " going from j= " << j << " up to " << rebin*(i+1) << endl;
	  if ((i+1) < N)
	    {
	      x+=g->GetX()[j]; // FIXME: correct x for steep spectrum
	      y+=g->GetY()[j];
	      ex+=g->GetErrorX(j);
	      ey+=g->GetErrorY(j)*g->GetErrorY(j)/(y*y);
	    }
	}
      cout << "setting the sum of points " << i*rebin << "-" << rebin*(i+1)-1 << " to new point " << i << " : ";
      grebin->SetPoint(i,x/rebin,y/rebin);
      grebin->SetPointError(i,ex/rebin,y/rebin*Sqrt(ey));
      //cout << "(" << x << "" << << "" << << "" << << 
    }

  grebin->SetMarkerColor(g->GetMarkerColor());
  grebin->SetMarkerSize(g->GetMarkerSize());
  grebin->SetMarkerStyle(g->GetMarkerStyle());
  
  return grebin;
}

//_____________________________________________________________________________
//
TGraphErrors*
normalizeYieldbyPt( TGraphErrors* g, 
		    const bool verbose = true)
{

  if (!g) return 0;

  int N = g->GetN();

  TGraphErrors *gnorm = new TGraphErrors(N);

  for ( int i=0; i<N; i++ ) // i loops over rebinned indices
    {
      double x=g->GetX()[i]; // FIXME: correct x for steep spectrum
      double y=g->GetY()[i];
      double ex=g->GetErrorX(i);
      double ey=g->GetErrorY(i)/y; //*g->GetErrorY(j)/(y*y);

      if (verbose) cout << "normalizeYieldbyPt(): pT=" << "" << x << " --> " << y/x << "+/-" << (y/x)*ey << endl;
      gnorm->SetPoint(i,x,y/x);
      gnorm->SetPointError(i,ex,(y/x)*ey);
    }

  gnorm->SetMarkerColor(g->GetMarkerColor());
  gnorm->SetMarkerSize(g->GetMarkerSize());
  gnorm->SetMarkerStyle(g->GetMarkerStyle());
  
  return gnorm;
}

//_____________________________________________________________________________
// TGraphErrors sum:
// sum = g1 + a*g2

TGraphErrors*
add( TGraphErrors *g,
     TF1 *f, 
     const double a = 1,
     const int verbose = 0 ) 

{

  if (!g) return 0;
  if (!f) return g;

  if ( abs(a)<1e-05 )
    {
      cout << "<W> analyzerUtils::add : scale-factor==0. Nothing changed ... " << endl;
      return g;
    }

  int N = g->GetN();

  TGraphErrors *gnew = new TGraphErrors(N);

  for ( int i=0; i<N; i++ )
    {
      double x=g->GetX()[i];
      double y=g->GetY()[i];
      double ex=g->GetErrorX(i);
      double ey=g->GetErrorY(i)*g->GetErrorY(i)/(y*y);
      
      double fval = a*f->Eval(x);

      if (verbose) cout << "point " << i << " changed : " << y << " --> " << y+fval << endl;
      gnew->SetPoint(i,x,y+fval);
      gnew->SetPointError(i,ex,(y+fval)*Sqrt(ey));
    }

  gnew->SetMarkerColor(g->GetMarkerColor());
  gnew->SetMarkerSize(g->GetMarkerSize());
  gnew->SetMarkerStyle(g->GetMarkerStyle());
  
  return gnew;
}

//_____________________________________________________________________________
// TGraphErrors sum:
// sum = g1 + a*g2

TGraphErrors*
add( TGraphErrors *g1,
     TGraphErrors *g2, 
     const double a = 1,
     const int verbose = 1 ) 

{

  if (!g1) return g2;
  if (!g2) return g1;

  if (abs(a)<1e-05)
    {
      cout << "<W> analyzerUtils::add : scale-factor==0. Nothing changed ... " << endl;
      return g1;
    }

  int n1 = g1->GetN();
  int n2 = g2->GetN();
  int N = Max(n1,n2);;
  int nmin = 0;

  TGraphErrors *min = 0;
  TGraphErrors *max = 0;
  TGraphErrors *sum = 0;
  
  if (n1 >= n2)
    {
      sum = (TGraphErrors*)g1->Clone();
      max = (TGraphErrors*)g1->Clone();

      min = (TGraphErrors*)g2->Clone();
      nmin = n2;
    }
  else
    {
      sum = (TGraphErrors*)g2->Clone();
      max = (TGraphErrors*)g2->Clone();

      min = (TGraphErrors*)g1->Clone();
      nmin = n1;
    }

  for ( int i = 0; i < N ; ++i ) 
    {
      double xmax = 0, ymax = 0, exmax = 0, eymax = 0;
      double xmin = 0, ymin = 0, exmin = 0, eymin = 0;

      max->GetPoint(i,xmax,ymax);
      exmax = max->GetErrorX(i);
      eymax = max->GetErrorY(i);

      if (i<nmin)
	{
	  min->GetPoint(i,xmin,ymin);
	  exmin = min->GetErrorX(i);
	  eymin = min->GetErrorY(i);
	}

      if ( ((xmax-xmin)<(exmax/2.)) || ((xmax-xmin)==0.) ) // fixme: binwidth ??
	{
	  double ysum = ymax+a*ymin;
	  double eysum = ysum*quadRelatError(eymax,ymax,eymin,ymin);

	  sum->SetPoint(i,xmax,ysum);
	  sum->SetPointError(i,exmax,eysum); // fixme: x-axis error fixed 

	  if (verbose)
	    {
	      cout << "<I> analyzerUtils::add : Summing contents at (x1,x2)=(" 
		   << xmax << "," << xmin << ") --> (" << ymax << "+" << a << "*" << ymin << ")=" 
		   << ysum << endl;
	    }
	}
      else // fixme: sum should be g1 & g2 in each independent non-coincident bin ...
	{
	  sum->SetPoint(i,xmax,ymax);
	  sum->SetPointError(i,exmax,eymax);
	  if (verbose)
	    {
	      cout << "<I> analyzerUtils::add : Non coincident binning (binwidth=" << exmax/2.
		   << ") at (x1,x2)=(" << xmax << "," << xmin << "). Nothing added ..." << endl;
	    }
	}
    }

  return sum;

}

//_____________________________________________________________________________
// reverse the order of entries in a TGraphErrors
//
void
reverse( TGraphErrors& g )
{

  const int N = g.GetN();
  double xi[N],yi[N],exi[N],eyi[N];
  int j = N-1;

  // fill the temporary arrays
  for ( int i = 0; i < N; ++i ) 
    {
      g.GetPoint(i,xi[j],yi[j]);
      exi[j] = g.GetErrorX(i);
      eyi[j] = g.GetErrorY(i);
      j--;
    }

  // refill the TGraphErrors with them
  for ( int i = 0; i < N; ++i ) 
    {
      g.SetPoint(i,xi[i],yi[i]);
      g.SetPointError(i,exi[i],eyi[i]);
    }

}

//_____________________________________________________________________________
// displace (slightly) the x points of a TGraphErrors (for clarity purposes)
//
void
displaceXvalues( TGraphErrors& g, 
		 const double dx = 0. )
{

  const int N = g.GetN();
  double x_in[N],ex_in[N];
  double y_in[N],ey_in[N];

  // fill the temporary arrays
  for ( int i = 0; i < N; ++i ) 
    {
      g.GetPoint(i, x_in[i], y_in[i]);
      ex_in[i] = g.GetErrorX(i);
      ey_in[i] = g.GetErrorY(i);
    }

  // refill the TGraphErrors with them
  for ( int i = 0; i < N; ++i ) 
    {
      g.SetPoint(i,x_in[i]+dx,y_in[i]);
      g.SetPointError(i,ex_in[i]+dx,ey_in[i]);
    }

}


//_____________________________________________________________________________
// add/substract a given % to the TGraphErrors
//
void
AddOrSubstractRelatError( TGraphErrors &g,
			  const double eAddOrSubRelat = 0.,
			  const int verbose = 1 )
{

  if (!eAddOrSubRelat)
    {
      cout << " <W> AddOrSubstractRelatError: zero relative error added. Nothing changed " << endl;
      return;
    }

  if (verbose) cout << endl;
  if (verbose>1) g.Print();

  double x,y;
  double ex,ey;
  double eyrelat = 0.;
  int N = g.GetN();

  for ( int i = 0; i < N; i++ ) 
    {
      g.GetPoint(i,x,y);
      ex = g.GetErrorX(i); 
      ey = g.GetErrorY(i);
      eyrelat = ey/y;

      double e0 = eyrelat;

      if ( eAddOrSubRelat<0 ) // we substract the error
	{
	  eyrelat = eyrelat*eyrelat - eAddOrSubRelat*eAddOrSubRelat;
	  if (eyrelat<0)
	    {
	      cout << " <E>  AddOrSubstractRelatError: New error negative ?!?!" << endl;
	    }
	  else
	    {
	      eyrelat = Sqrt(eyrelat);
	    }
	}
      else // we add the error
	{
	  eyrelat = quadRelatError(eyrelat,1.,eAddOrSubRelat,1.);
	}
      
      if (verbose)
	{
	  printf(" <I> pT = %.2f Error modified from: ey = %.2f%%  to ---> %.2f%%\n",
		 x,e0*100.,eyrelat*100.);
	}

      eyrelat *= y;

      g.SetPointError(i,ex,eyrelat);
    }

  if (verbose>1) g.Print();

}


//_____________________________________________________________________________
//
double*
integral( TGraphErrors *g, 
	  const double xinf, 
	  const double xsup )
{

  double *graphint = new double[2];
  graphint[0] = 0.;
  graphint[1] = 0.;

  if ( !g ) return graphint;

  g->Print();
  int N = g->GetN();

  double binwidth = 0;  //g->GetXaxis()->GetBinWidth(0); // doesn't seem to give the good bin width

  for ( int i = 0; i < N; ++i ) 
    {
      double x = g->GetX()[i];
      if (!binwidth)
	{
	  double xnext = g->GetX()[i+1];
	  binwidth = Abs(x-xnext);//2.;  
	  printf(" <I> analyzerUtils::integral(g,xinf,xsup): xinf = %.3f, xsup = %.3f Bin width = %.4e\n"
		 ,xinf,xsup,binwidth);

	}
      if ( x > (xinf-binwidth/2.) && x < (xsup+binwidth/2.) ) 
	{
	  double y = g->GetY()[i];
	  double yerr = g->GetErrorY(i);

	  graphint[0] += y;
	  graphint[1] += yerr*yerr;
	  printf(" (x,y) = (%.4f,%.4e) -->  integ = %.4e +/- %.4e\n",x,y,graphint[0],graphint[1]);
	}
    }

  graphint[0]*= binwidth;
  graphint[1] = Sqrt(graphint[1])*binwidth;

  //c5->Close();
  //delete c5;

  return graphint;
}

//_____________________________________________________________________________
// 
double*
getHagedornYieldIntegral( TF1* hagedorn,
			  const double xinf, 
			  const double xsup )
{

  double *hagint = new double[2];
  hagint[0] = 0.;
  hagint[1] = 0.;

  if (!hagedorn) return hagint;

  double *hagedPars  = (double*)hagedorn->GetParameters();
  double *ehagedPars = (double*)hagedorn->GetParErrors();

  // hagedorn integral (analytical)
  TF1 integhagedorn("integhagedorn","([0]/(1-[2]))*(x+[1])^(1-[2])",xinf,xsup);
  integhagedorn.SetParameters(hagedPars);
  for (int i=0;i<3;i++)
    {
      integhagedorn.SetParError(i,ehagedPars[i]);
    }

  hagint[0] = integhagedorn.Integral(xinf,xsup);
  hagint[1] = 0;

  delete hagedPars;
  delete ehagedPars;

  return hagint;
}

//_____________________________________________________________________________
//
void 
setMarkerLineType( TGraph *Spectrum,
		   const int style = 0 , 
		   const int color = 0 ,
		   const double size = 0 ,
		   const int linstyle = 0, 
		   const int lincolor = 0,
		   const int linwidth = 0 )
{

  if (!Spectrum) return;

  if (style) Spectrum->SetMarkerStyle(style);
  if (color) Spectrum->SetMarkerColor(color);
  if (size)  Spectrum->SetMarkerSize(size);

  if (linstyle) Spectrum->SetLineStyle(linstyle);
  if (lincolor) Spectrum->SetLineColor(lincolor);
  if (linwidth) Spectrum->SetLineWidth(linwidth);

}

//_____________________________________________________________________________
//
TGraphErrors*
ratio( TGraphErrors *g0, 
       TGraphErrors *g1,
       const int verbose = 1,
       const double coincidentBinningWidth = 0.25) //to force the ratio of g0 and g1 to be done only for coincident bins set it to zero
{

  TGraphErrors *ratio_g0_g1 = 0;

  if (!g0 || !g1) return ratio_g0_g1;

  char name[300];
  sprintf(name,"ratio_%s_over_%s",g0->GetName(),g1->GetName());

  const int Npoints0 = g0->GetN();
  const int Npoints1 = g1->GetN();

  // we take the max. in case of (some) non-coincident binning and total same size
  int Npoints = Max(Npoints0,Npoints1);
  int unusedNpoints = 0;

  double *pT  = new double[Npoints];
  double *epT = new double[Npoints];

  double *ratio_graphs = new double[Npoints];
  double *eratio_graphs = new double[Npoints];

  double xbinwidth = coincidentBinningWidth; // default is 0.25 (half 0.5 GeV/c)

  xbinwidth = (xbinwidth == 0.) ? 0.001 : xbinwidth; // if 0. set it to ~0.

  if (verbose)
    {
      cout << " <I> analyzerUtils::ratio of " << g0->GetName()
	   << " over " << g1->GetName() << endl;
      cout << "    pT                pT_err   Ratio    errRatio " << endl; 
    }

  double pT0;
  double pT1;
  double epT0;
  double epT1;
  double yield0;
  double yield1;
  double e_y0;
  double e_y1;

  int i0 = -1;
  int i1 = -1;
  
  for( int i=0; i<Npoints; i++)  // i runs over ratio
    {
      i0++; // i0 runs over 1st file
      i1++; // i1 runs over 2nd file

      yield0 = 0.;
      yield1 = 0;;
      e_y0 = 0.;
      e_y1 = 0.;
      epT0 = 0.;
      epT1 = 0.;
      
      bool notEnd0 = (i0<Npoints0);
      bool notEnd1 = (i1<Npoints1);

      if (notEnd0)
	{
	  g0->GetPoint(i0,pT0,yield0);
	  epT0 = g0->GetErrorX(i0);
	  if (yield0) e_y0 = g0->GetErrorY(i0)/yield0;
	}
      if (notEnd1)
	{
	  g1->GetPoint(i1,pT1,yield1);
	  epT1 = g1->GetErrorX(i1);
	  if (yield1) e_y1 = g1->GetErrorY(i1)/yield1;
	}
      
      // Not end of file 0 nor 1: check coincident binning
      if (notEnd0 && notEnd1)
	{
	  
	  if ( (pT0-pT1)<=-xbinwidth ) // pT0 bin value lower than pT1 bin (within xbinwidth)
	    {
	      if (verbose)
		{
		  cout << " <W> pt0=" << pT0 << " < pt1=" << pT1 
		       << " --> Skipping to next pT bin of graph 0 ... " << endl;
		}
	      while( ((pT0-pT1)<=-xbinwidth) && i0<Npoints0 ) // let's try to find the first equal x bin 
		{
		  i0++;
		  g0->GetPoint(i0,pT0,yield0);
		  epT0 = g0->GetErrorX(i0);
		  if (yield0) e_y0 = g0->GetErrorY(i0)/yield0;
		}
	      if (i0==Npoints0) // coincident binning not found
		{
		  if (verbose)
		    {
		      cout << " <W> TGraphs with different x-axis values at pT=" 
			   << pT0 << " GeV/c. Cannot do further the ratio !" << endl;
		    }
		  while( i<=Npoints0 ) // fill the rest with zeros
		    {
		      //pT[i] = xbinwidth*2.*i;
		      //epT[i] = 0.;
		      //ratio_graphs[i] = crazyValue;//0.;
		      //eratio_graphs[i] = crazyValue;//0.;
		      cout << " <W> Skipping point " << xbinwidth*2.*i << " from graph " << g0->GetName() << endl;
		      unusedNpoints++;
		      i++;
		    }
		  break;
		}
	    }
	  else if ( (pT0-pT1)>=xbinwidth ) //  pT0 bin larger than pT1 bin (within xbinwidth)
	    {
	      if (verbose)
		{
		  cout << " <W> pt0=" << pT0 << " > pt1=" << pT1 
		       << " --> Skipping to next pT bin of graph 1 ... " << endl;
		}
	      while( ((pT0-pT1)>=xbinwidth) && i1<Npoints1 ) // let's try to find the first equal x bin 
		{
		  i1++;
		  g1->GetPoint(i1,pT1,yield1);
		  epT1 = g1->GetErrorX(i1);
		  if (yield1) e_y1 = g1->GetErrorY(i1)/yield1;
		}
	      if (i1==Npoints1) // coincident binning not found
		{
		  if (verbose)
		    {
		      cout << " <W> TGraphs with different x-axis values at pT=" 
			   << pT1 << " GeV/c. Cannot do further the ratio !" << endl;
		    }
		  while( i<=Npoints1 ) // fill the rest with zeros
		    {
		      //pT[i] = xbinwidth*2.*i;
		      //epT[i] = 0.;
		      //ratio_graphs[i] = crazyValue;//0.;
		      //eratio_graphs[i] = crazyValue;//0.;
		      cout << " <W> Skipping point " << xbinwidth*2.*i << " from graph " << g1->GetName() << endl;
		      unusedNpoints++;
		      i++;
		    }
		  break;
		}
	    }
	  
	  // Now pT0 and pT1 should be equal (within binwidth)
	  
	  pT[i]  = pT0;  
	  epT[i] = epT0; 
	  
	  // let's take the ratio
	  
	  if (yield1) ratio_graphs[i] = yield0/yield1;
	  else ratio_graphs[i] = 0.;

	  eratio_graphs[i] = quadRelatError(e_y0,1.,e_y1,1.);
	  eratio_graphs[i] *= ratio_graphs[i];
	  
	  if (verbose || (pT0 =! pT1) ) // output when verbose or x's are different
	    {
	      printf("    %.2f (%.2f,%.2f)   %.2f    %.3f    %.3f\n",
		     pT[i],pT0,pT1,epT[i],ratio_graphs[i],eratio_graphs[i]);
	    }
	}
      else // end of one or both files
	{
	  unusedNpoints++;
	}
      
    } // loop on ratio bins

  // Fill the TGraph

  const int Npointsfinal = Npoints-unusedNpoints;
  
  double *pTfinal = pT; // pT0 and pT1 should be equal (within binwidth)
  double *epTfinal = epT;
  double *ratio_final = ratio_graphs;
  double *eratio_final = eratio_graphs; 

  ratio_g0_g1 = new TGraphErrors(Npointsfinal,pTfinal,ratio_final,epTfinal,eratio_final);
  ratio_g0_g1->SetName(name);
  ratio_g0_g1->SetTitle(name);
  setMarkerLineType(ratio_g0_g1, 20,9,1.2);

//   for ( int i = 0; i < Npointsfinal; i++ )
//     {
//       double x, y;
//       ratio_g0_g1->GetPoint(i,x,y);
//       if ((int)y==(int)crazyValue) ratio_g0_g1->RemovePoint(i);
//     }

  //ratio_g0_g1->Print();

  delete pT;
  delete epT;

  delete ratio_graphs;
  delete eratio_graphs;

  return ratio_g0_g1;

}

//_____________________________________________________________________________
//
TGraphErrors*
ratio( TGraphErrors *g, 
       TF1 *f,
       const int option = 0,
       const int verbose = 0 )
{

  if (!g || !f) return 0;

  int N = g->GetN();

  TGraphErrors *ratio = new TGraphErrors(N);

  char name[300];
  sprintf(name,"ratio_%s_over_%s",g->GetName(),f->GetName());
  if (option==1) sprintf(name,"product_%s_times_%s",g->GetName(),f->GetName());
  if (option==2) sprintf(name,"ratio_%s_over_%s",f->GetName(),g->GetName());

  ratio->SetName(name);
  ratio->SetTitle(name);

  for ( int i = 0; i < N; i++ )
    {
      double x, y;
      double ex, ey;

      g->GetPoint(i,x,y);
      ex = g->GetErrorX(i);
      ey = g->GetErrorY(i);

      if (!option)
	{
	  y/=f->Eval(x);
	  ey/=f->Eval(x);
	}
      else if (option == 1)
	{
	  y*=f->Eval(x);
	  ey*=f->Eval(x);
	}
      else if (option == 2)
	{
	  double eyrel = ey/y;
	  y=f->Eval(x)/y;
	  ey=eyrel*y;
	}

      ratio->SetPoint(i,x,y);
      ratio->SetPointError(i,ex,ey); // FIXME: shouldn't we include f->GetParErrors() ?
    }

  setMarkerLineType(ratio,20,9,1.2);

  if (verbose) ratio->Print();

  return ratio;

}

//_____________________________________________________________________________
//
TGraphErrors* 
data_minus_fit_over_fit( TGraphErrors *g, 
			 TF1 *f,
			 const int verbose = 0)
{

  // TO BE IMPLEMENTED !!

  if (!g || !f) return 0;

  int N = g->GetN();

  TGraphErrors *ratio = new TGraphErrors(N);

  for ( int i = 0; i < N; i++ )
    {
      double x, y;
      double ex, ey;

      g->GetPoint(i,x,y);
      ex = g->GetErrorX(i);
      ey = g->GetErrorY(i);

      y/=f->Eval(x);
      ey/=f->Eval(x);

      ratio->SetPoint(i,x,y);
      ratio->SetPointError(i,ex,ey);
    }

  setMarkerLineType(ratio,20,9,1.2);

  if (verbose) ratio->Print();

  return ratio;

}

//_____________________________________________________________________________
//
TGraphErrors*
ratioFits( TGraphErrors *g0, 
	   TGraphErrors *g1,
	   const int verbose = 0 )
{

  TGraphErrors *ratio_g0_g1 = 0;

  if (!g0 || !g1) return ratio_g0_g1;

  TF1 *f0 = 0;
  TF1 *f1 = 0;
  f0 = (TF1*)g0->GetFunction("hagedorn");
  if (!f0) f0 = (TF1*)g0->GetFunction("hagedornascii"); 

  f1 = (TF1*)g1->GetFunction("hagedorn");
  if (!f1) f1 = (TF1*)g1->GetFunction("hagedornascii");
  if (!f0 || !f1) return ratio_g0_g1;

  double xmin0, xmax0;
  f0->GetRange(xmin0, xmax0);
  double xmin1, xmax1;
  f1->GetRange(xmin1, xmax1);

  double xmin = Min(xmin0,xmin1);
  double xmax = Max(xmax0,xmax1);
  f0->SetRange(xmin,xmax);
  f1->SetRange(xmin,xmax);

  TH1F *h0 = (TH1F*)f0->GetHistogram();
  TH1F *h1 = (TH1F*)f1->GetHistogram();
  if (!h0 || !h1) return ratio_g0_g1;

  TH1F *hratio = (TH1F*)h0->Clone();
  hratio->Divide(h1);
  if (verbose)
    {
      hratio->Print("all");
    }

  ratio_g0_g1 = histo2graph(hratio,g0);

  setMarkerLineType(ratio_g0_g1, 20,9,1.2);

  //ratio_g0_g1->Print();

  return ratio_g0_g1;

}

//_____________________________________________________________________________
// Make a TH1F out of TGraphErrors (for plotting purposes only !)
//
TH1F*
graph2histo( TGraphErrors *g0 )
{

  if (!g0) return 0;

  int N = g0->GetN();

  double Xmin = 0.;
  double Xmax = 0.;
  //Xmax = g0->GetXaxis()->GetXmax();
  //Xmin = g0->GetXaxis()->GetXmin();
  //cout << "graph2histo: xmin=" << Xmin << " xmax=" << Xmax << endl;
  getXminmax( g0, Xmin, Xmax);
  //cout << "graph2histo: xmin=" << Xmin << " xmax=" << Xmax << endl;

  double halfbinwidth = 0.5*(Xmax-Xmin)/N;

  TString name = (TString)g0->GetTitle();

  TH1F *h1 = new TH1F(name,name,N,Xmin-halfbinwidth,Xmax+halfbinwidth); // fixme: does not take into account variable bin TGraphs

  for ( int i = 0; i < N; i++ )
    {
      double xi,yi,eyi;

      g0->GetPoint(i,xi,yi);
      eyi = g0->GetErrorY(i);

      int bin = h1->GetXaxis()->FindBin(xi);
      h1->SetBinContent(bin,yi);
      h1->SetBinError(bin,eyi);
    }

  return h1;
}

//_____________________________________________________________________________
//
// "Last" attempt to get some easy way of plotting "shading" bands:
// (i)  fixed % error per bin (if NormRelatErr!=0)
// (ii) as given by TGraphErrors itself (if NormRelatErr==0)
//
// Example of use:
/*
  TGraphErrors *err = ...
  TClonesArray* errBand = (TClonesArray*)analyzerUtils::errorBand( err );
  TGraph *errshade = (TGraph*)(*errBand)[0];
  TGraph *errmin = (TGraph*)(*errBand)[1];
  TGraph *errmax = (TGraph*)(*errBand)[2];
  errshade->Draw("f");
  errmin->Draw("l");
  errmax->Draw("l");

*/

TClonesArray* 
errorBand( TGraphErrors *g,
	   const double NormRelatErr = 0 )
{

  if (!g) return 0;

  TClonesArray *array = new TClonesArray("TGraph",3);

  int n = g->GetN();
  double x[n];
  double y[n];
  double ymax[n];
  double ymin[n];

   for (int i=0;i<n;i++) 
     {
       x[i] = g->GetX()[i];
       y[i] = g->GetY()[i];

       if (NormRelatErr!=0.)
	 {
	   ymax[i] = y[i]*(1+NormRelatErr);
	   ymin[i] = y[i]*(1-NormRelatErr);
	 }
       else // attempt to do the same as next function ...
	 {
	   ymax[i] = y[i]+g->GetErrorY(i);
	   ymin[i] = y[i]-1.*g->GetErrorY(i);
	 }
     }

  TGraph *gshade = new TGraph(2*n);
  TGraph *gmin = new TGraph(n,x,ymin);
  TGraph *gmax = new TGraph(n,x,ymax);

  for (int i=0;i<n;i++) 
    {
      gshade->SetPoint(i,x[i],ymax[i]);
      gshade->SetPoint(n+i,x[n-i-1],ymin[n-i-1]);
    }

  //gshade->SetFillStyle(3002);
  gshade->SetFillColor(kYellow);
  gmin->SetLineColor(9);
  gmax->SetLineColor(9);

  new((*array)[0]) TGraph(*gshade);
  new((*array)[1]) TGraph(*gmin);
  new((*array)[2]) TGraph(*gmax);

  return array;

}

//_____________________________________________________________________________
//
// plot error band between 2 tgraphs ...

TClonesArray* 
errorBand( TGraphErrors *g0, TGraphErrors *g1 )
{

  if (!g0 || !g1) return 0;

  TClonesArray *array = new TClonesArray("TGraph",3);
  
  int n = g0->GetN();
  double x0[n];
  double y0[n];
  double x1[n];
  double y1[n];

  double ymax[n];
  double ymin[n];

   for (int i=0;i<n;i++) 
     {
       x0[i] = g0->GetX()[i];
       y0[i] = g0->GetY()[i];
       x1[i] = g1->GetX()[i];
       y1[i] = g1->GetY()[i];

       if (x0[i]==x1[i])
	 {
	   ymax[i] = max(y0[i],y1[i]);
	   ymin[i] = min(y0[i],y1[i]);
	 }
       else
	 {
	   cout << " <W> TClonesArray* errorBand(g0,g1): non-coincident binning ..." << endl;
	 }
     }

  TGraph *gshade = new TGraph(2*n);
  TGraph *gmin = new TGraph(n,x0,ymin);
  TGraph *gmax = new TGraph(n,x0,ymax);

  for (int i=0;i<n;i++) 
    {
      gshade->SetPoint(i,x0[i],ymax[i]);
      gshade->SetPoint(n+i,x0[n-i-1],ymin[n-i-1]);
    }

  //gshade->SetFillStyle(3002);
  gshade->SetFillColor(90);
  gmin->SetLineColor(94);
  gmax->SetLineColor(94);

  new((*array)[0]) TGraph(*gshade);
  new((*array)[1]) TGraph(*gmin);
  new((*array)[2]) TGraph(*gmax);

  return array;

}

//_____________________________________________________________________________
// Typical use of error-boxes:
/* 
   TGraphErrors *eband = ... ;
   TClonesArray* eboxes = (TClonesArray*)analyzerUtils::errorBoxes( eband, 0.);  // error boxes
   TBox *box = 0;
   for (int j = 0; j < eboxes->GetEntries();j++)
   {
   box = ((TBox*)(*eboxes)[j]);
   box->SetFillColor(7);
   box->SetFillStyle(3001);
   c->cd();
   box->Draw();
   }
*/
TClonesArray* 
errorBoxes( TGraphErrors *g,
	    const double NormRelatErr = 0.,
	    const double width = 0.5,
	    const bool logy = false )
{

  if (!g) return 0;

  int n = g->GetN();
  TClonesArray *array = new TClonesArray("TBox",n);

  double x[n];
  double y[n];
  double ex[n];
  double ymax[n];
  double ymin[n];
  double gwidth = 0.;

  for (int i=0;i<n;i++) 
    {
      x[i] = g->GetX()[i];
      y[i] = g->GetY()[i];
      ex[i] = g->GetErrorX(i)*width;

      // let's set a fixed minimum width of the error boxes in case the TGraph has no x-errors
      if (ex[i]==0.)
	{
	  gwidth = abs(g->GetX()[1]-g->GetX()[0])*width;
	  ex[i] = gwidth;
	}

      if (NormRelatErr!=0.)
	{
	  ymax[i] = y[i]*(1+NormRelatErr);
	  ymin[i] = y[i]*(1-NormRelatErr);
	}
      else
	{
	  ymax[i] = y[i]+g->GetErrorY(i);
	  ymin[i] = y[i]-1.*g->GetErrorY(i);
	}

      if (logy)
	{
	  ymax[i] = log10(ymax[i]);
	  ymin[i] = log10(ymin[i]);
	}

      //printf("x=%4.2f   y=%g   ex=%g   ymin=%g   ymax=%g\n",x[i],y[i],ex[i],ymin[i],ymax[i]);
    }
  
  TBox *box;
  
  for (int i=0;i<n;i++) 
    {
      box = new TBox(x[i]-ex[i],ymin[i],
		     x[i]+ex[i],ymax[i]);
      box->SetFillColor(kYellow);

      new((*array)[i]) TBox(*box);
    }

  //array->Print();

  return array;

}

//_____________________________________________________________________________
//
TCanvas* 
canvas( const char *title="canvas", 
	const int sizex = 600, const int sizey = 500 )

{

  TCanvas *c1 = new TCanvas(title, title, sizex, sizey);
  c1->Range( -0.48, -8.37, 8.0, 0.21);
  c1->SetFillColor(0);
  c1->SetBorderMode(0);
  c1->SetBorderSize(0);
  c1->SetLeftMargin(0.12);
  c1->SetRightMargin(0.010);
  c1->SetTopMargin(0.02);
  c1->SetFrameBorderMode(0);
  c1->SetFrameBorderMode(0);

  c1->Range( -0.9, -0.25, 5.7, 1.6);
  c1->SetLeftMargin(0.137);
  c1->SetRightMargin(0.0273);
  c1->SetTopMargin(0.0562);
  c1->SetBottomMargin(0.134);

  return c1;

}

//_____________________________________________________________________________
//
TH2F* 
frame( const char *title="frame", 
       const int xbins=100, const double xmin=0., const double xmax=100.,
       const int ybins=100, const double ymin=0., const double ymax=100.,
       const char *xtitle="p_{T} (GeV/c)", 
       const char *ytitle="1/[2#pi p_{T}] d^{2}N/dp_{ T}dy (GeV/c)^{-2}" )
{

  TH2F *myframe = new TH2F(title, title, xbins, xmin, xmax, ybins, ymin, ymax);

  myframe->SetStats(0);
  myframe->SetTitle(title);
  myframe->SetXTitle(xtitle);
  myframe->SetYTitle(ytitle);

  myframe->GetXaxis()->SetTitleSize(0.045);
  myframe->GetXaxis()->SetTitleOffset(1.);
  myframe->GetXaxis()->SetLabelSize(0.045);

  myframe->GetYaxis()->SetTitleSize(0.045);
  myframe->GetYaxis()->SetTitleOffset(1.);
  myframe->GetYaxis()->SetLabelSize(0.045);

  myframe->Draw();

  return myframe;
}

//_____________________________________________________________________________
//
void
saveCanvas( TCanvas *c1, const char *type = "gif" )
{

  if (!c1) return ;

  char output[300];
  char *title = (char*)c1->GetTitle();

  TString type_str = type ;

  if (type_str.Contains("gif"))
    {
      sprintf(output, "%s.gif", title);
      c1->SaveAs(output);
    }
  if (type_str.Contains("eps"))
    {
      sprintf(output, "%s.eps", title);
      c1->SaveAs(output);
    }
}


}// end of namespace
