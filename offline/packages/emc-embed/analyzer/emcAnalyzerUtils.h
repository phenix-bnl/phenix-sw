// $Header
//-----------------------------------------------------------------------------
//
//  (c) PHENIX Collaboration 2002-2003
//
//  Author: D. d'Enterria
//
//  Nevis Labs. Columbia University
//
//-----------------------------------------------------------------------------


#ifndef __emcAnalyzerUtils_h__
#define __emcAnalyzerUtils_h__

//#ifndef __CINT__
#include <vector>
//#endif

class TH1;
class TH1F;
class TF1;
class TMath;
class TGraph;
class TGraphErrors;
class TClonesArray;
class TPolyLine;

namespace emcAnalyzerUtils

{

  // not used now (partic argument added to general functions instead) ...  
  //   int getEtaCentralityClass( const int Cent1, const int Cent2);
  //   void getEtaCentralityClassLimits( const int CC, 
  // 				    int& c1, int& c2);
  //   double getEtaNcoll( const int Cent1, const int Cent2, 
  // 		      const char *type_and_glauber = "value_new");

  int getCentralityClass( const int Cent1, const int Cent2 );
  
  void getCentralityClassLimits( const int CC, 
				 int& c1, int& c2 );

  void getCentralityClassLimits2( const int CC, 
				  int& c1, int& c2 );

  void getCentralityClassLimits( const char* cent, 
				 int& c1, int& c2 );
  
  double getEvents( const int Cent1, const int Cent2, 
		    const char *type="value" );

  double getEvents62GeV( const int Cent1, const int Cent2, 
			 const char *type="value" );

  double getEventsReacPlane( const int Cent1, const int Cent2, 
			     const char *type="value");

  double getEtaEvents( const int Cent1, const int Cent2, 
		       const char *type="value" );
  
  double getImpactParam( const int Cent1, const int Cent2, 
			 const char *type_and_glauber = "value_new" );

  double getNcoll( const char *type="pp" );

  double getNpart( const char *type="pp" );
  
  double getNcoll( const int Cent1, const int Cent2, 
		   const char *type_and_glauber = "value_new" );
  
  double getNpart( const int Cent1, const int Cent2, 
		   const char *type_and_glauber ="value_new" );

  void dumpCentDependParams( int step=2, 
			     const char *what_to_dump = "all", 
			     const char *glauber = "new",
			     const char *style = "latex" );
  
  double getEccentricity( const int Cent1, const int Cent2, 
			  const char *type="value" );

  double getOverlapArea( const int Cent1, const int Cent2, 
			 const char *type="value" );

  double getNormOverlapArea( const int Cent1, const int Cent2, 
			     const char *type="value" );

  double getTAB( const int Cent1, const int Cent2, 
		      const char *type="value" );
  
  double getET( const int Cent1, const int Cent2, 
		const char *type="value" );

  double getNch( const int Cent1, const int Cent2,
		 const int sqrts = 200, 
		 const char *type="value" );
  
  double getNch_ppbar( const double sqrts = 200., 
		       const char *type="value" );
  
  double getEBjorken( const int Cent1, const int Cent2, 
		      const char *type="value" );

  double
  getPropagNormRelatError( const int Cent1, 
			   const int Cent2,
			   const int Cent3, 
			   const int Cent4,
			   const double extra_erelat,
			   const bool bin_scaling,
			   const bool useTAB,
			   const int verbose );

  TH1F* getPropagErrorNcollBand( TGraphErrors *g,
				 const int Cent1, 
				 const int Cent2,
				 const int Cent3, 
				 const int Cent4,
				 const double *extra_erelat = 0,
				 const bool bin_scaling = true,
				 const bool useTABinsteadOfNcoll = true,
				 const int verbose = 1 );

  double* getPropagErrorNcoll( TGraphErrors *g,
			       const int Cent1=0, 
			       const int Cent2=10,
			       const int Cent3=0, 
			       const int Cent4=0,
			       const double *extra_erelat = 0,
			       const bool bin_scaling = true,
			       const int verbose = 1 );
  
  double* getPropagErrorTAB( TGraphErrors *g,
			     const int Cent1=0, 
			     const int Cent2=10,
			     const int Cent3=0, 
			     const int Cent4=0,
			     const double *extra_erelat = 0,
			     const int verbose = 1 );

  double** readAscii( const char* datafile, 
		      int &ncols,
		      int &nlines,
		      int skiplines = 0,
		      const int verbose = 0 );

  TH1F* readAsciiToHisto(const char *filename = "data.txt", 
			 const int ncols = 3,
			 const char *format = "x-y-ex-ey",
			 const int verbose = 1 );

  void dumpHisto( const TH1 *h, const char *format = "x-y-ex-ey" );

  void dumpGraph( TGraph *gr, const char *format = "x-y-ex-ey" );

  double* readErrorNcollAscii(const char *file);
  
  double quadRelatError( const double ea = 0., const double a = 1.,
			 const double eb = 0., const double b = 1.,
			 const double ec = 0., const double c = 1.,
			 const double ed = 0., const double d = 1.,
			 const double ee = 0., const double e = 1. );
  
  TF1* hagedornConstrainedInit( const char* funcname = "constrhagedorn",
				const double ptmin = 2., 
				const double ptmax = 10.,
				const int verbose = 0,
				const double AHag = 100.,
				const double ptmean = 0.39, 
				const double n_exp = 10.5 );

  TF1* hagedornInit( const char* funcname = "hagedorn",
		     const double ptmin = 2., 
		     const double ptmax = 10.,
		     const int verbose = 0,
		     const double AHag = 1000.,
		     const double p0 = 1.72, 
		     const double n_exp = 10.5 );
  
  TF1* hagedornConstrainedFit( TH1* h, 
			       const char* funcname = "constrhagedorn",
			       const double ptmin = 2., 
			       const double ptmax = 10.,
			       const int verbose = 1,
			       const double AHag = 1000.,
			       const double pTmean = 0.39,
			       const double n_exp = 10.5 );
  
  TF1* hagedornFit( TH1* h, 
		    const char* funcname="hagedorn",
		    const double ptmin = 2., 
		    const double ptmax = 10.,
		    const int verbose = 1,
		    const double AHag = 1000.,
		    const double p0 = 1.72,
		    const double n_exp = 10.5 );

  TF1* hagedornConstrainedFit( TGraphErrors* g, 
			       const char* funcname = "constrhagedorn",
			       const double ptmin = 2., 
			       const double ptmax = 10.,
			       const int verbose = 1,
			       const double AHag = 1000.,
			       const double ptmean = 0.39,
			       const double n_exp = 10.5 );
  
  TF1* hagedornFit( TGraphErrors* g, 
		    const char* funcname = "hagedorn",
		    const double ptmin = 2., 
		    const double ptmax = 10.,
		    const int verbose = 1,
		    const double AHag = 1000.,
		    const double p0 = 1.72,
		    const double n_exp = 10.5 );

  void getBinShift( const double ptini, 
		    const double ptbinwidth, 
		    TF1* hagedorn,
		    TGraphErrors* g,
		    std::vector<double> &xyshift );

  void getBinShiftSimple( const double ptini, 
			  const double ptbinwidth, 
			  TF1* hagedorn,
			  TGraphErrors* g,
			  std::vector<double> &xyshift );
  
  double shiftPt( TGraphErrors* g, 
		  TF1* hagedorn,
		  const double ptbinwidth = 0.5,
		  const bool undoInvYieldNormFirst = false );

  double shiftYield( TGraphErrors* g, 
		     TF1* hagedorn,
		     const double ptbinwidth = 0.5,
		     const bool undoInvYieldNormFirst = false );
  
  void getBinShiftPowLaw( const double ptini, 
			  const double ptbinwidth, 
			  TF1* pow,
			  TGraphErrors* g,
			  std::vector<double> &xyshift );
  
  double shiftPtPowLaw( TGraphErrors* g, 
			TF1* pow,
			const double ptbinwidth = 0.5,
			const bool undoInvYieldNormFirst = false );
  
  double shiftYieldPowLaw( TGraphErrors* g, 
			   TF1* pow,
			   const double ptbinwidth = 0.5,
			   const bool undoInvYieldNormFirst = false );
  
  void reverse( TGraphErrors& g );

  void scale( TGraphErrors& g, 
	      const double a = 1.,
	      const double ea = 0., 
	      const bool verbose = true );

  void displaceXvalues( TGraphErrors& g, 
			const double dx = 0. );

  void AddOrSubstractRelatError( TGraphErrors &g,
				 const double eAddOrSubRelat = 0.,
				 const int verbose = 1 );

  TGraphErrors *normalizeYieldbyPt( TGraphErrors* g, 
				    const bool verbose = true );

  TGraphErrors * Ed3sigmad3p_to_dsigmadpT( TGraphErrors* g, 
					   const bool verbose = true );

  TGraphErrors *rebin( TGraphErrors* g, 
		       const int rebin = 2 ); 
  
  double* integral( TGraphErrors *g, 
		    const double xmin, const double xmax );
  
  double* getHagedornYieldIntegral( TF1 *hag, 
				    const double xmin, const double xmax );

  void getXYminmax( TGraphErrors* g1,
		    TGraphErrors* g2,
		    double& Xmin, double& Xmax,
		    double& Ymin, double& Ymax );

  void getXYminmax( TGraphErrors* g,
		    double& Xmin, double& Xmax,
		    double& Ymin, double& Ymax );
  
  void getXminmax( TGraphErrors* g, 
		   double& Xmin, double& Xmax,
		   const bool consider_errors = false );
  
  void getYminmax( TGraphErrors* g, 
		   double& Ymin, double& Ymax);
                   //const double Xmin = 0., 
                   //const double Xmax = 1e30 );
  
  //void getYminmax( TH1* h, 
  //		   double& Ymin, double& Ymax,
  //		   const double Xmin, const double Xmax );

  double getYatX( TGraphErrors* g, 
		  const double X ,
		  const char *type = "value" );
  
  TGraphErrors* ratio( TGraphErrors *g1, 
		       TGraphErrors *g2,
		       const int verbose = 1,
		       const double coincidentBinningWidth = 0.25 );

  TGraphErrors* ratio( TGraphErrors *g, 
		       TF1 *f,
		       const int option = 0,
		       const int verbose = 0 );

  TGraphErrors* data_minus_fit_over_fit( TGraphErrors *g, 
					 TF1 *f,
					 const int verbose = 0 );

  TGraphErrors* add( TGraphErrors *g,
		     TF1 *f, 
		     const double a = 1.,
		     const int verbose = 0 );

  TGraphErrors* add( TGraphErrors *g1,
		     TGraphErrors *g2, 
		     const double a = 1.,
		     const int verbose = 0 );

  TGraphErrors* ratioFits( TGraphErrors *g0, 
			   TGraphErrors *g1,
			   const int verbose = 0 );
  
  TGraphErrors* histo2graph( TH1* h1, 
			     TGraphErrors* g1 = 0 );

  TH1F* graph2histo( TGraphErrors *g1 );
  
  TGraphErrors* chopPointsFromGraph( TGraphErrors* gin, 
				     int Nunused );

  void setMarker( TGraph *Spectrum, const int CC );

  void setMarker( TGraph *Spectrum,
		  const int Cent1, const int Cent2 );
  
  void setMarkerLineType( TGraph *Spectrum,
			  const int style = 0 , 
			  const int color = 0 ,
			  const double size = 0 ,
			  const int linstyle = 0, 
			  const int lincolor = 0,
			  const int linwidth = 0 );

  TClonesArray* errorBand( TGraph *g ,
			   const double NormRelatErr = 0. );

  TClonesArray* errorBoxes( TGraphErrors *g ,
			    const double NormRelatErr = 0., 
			    const double width = 0.5, 
			    const bool logy = false );

  TClonesArray* errorBand( TGraph *g0 , TGraph *g1 );

  TGraph* plot_shaded_area( TF1 *f1, double xmin = 0., double xmax = 100. );
  TGraph* plot_shaded_area( TF1 *f1, TF1 *f2, double xmin = 0., double xmax = 100. );

  TPolyLine* plot_contour_area( TF1 *f1, double plusminus = 0.2 );
  //TPolyLine* plot_contour_area( TGraph *g1, TGraph *g2 );

  void dump_function( TF1 *f1 );

} // end of namespace

#endif
