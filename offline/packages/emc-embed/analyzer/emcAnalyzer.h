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

#ifndef __emcAnalyzer_h__
#define __emcAnalyzer_h__

//#include <vector>

class TFile;
class TList;
class TH1;
class TF1;
class TH2F;
class TString;
class TDirectory;
class TGraphErrors;
class TCanvas;

//static const char *favoritecut = "tof1chisq1_extlvl2";
static const char *favoritecut = "chisq2"; // final Run-2 pi0
//static const char *favoritecut = "tof2chisq2"; // eta 
//static const char *favoritecut = "tof1chisq1"; // PPG014 pi0
//static const char *favoritecut = "tof2chisq2"; // Dec.03 pi0 repass

class emcAnalyzer
{

public:

  emcAnalyzer();
  ~emcAnalyzer();

//_____________________________________________________________________________

  TGraphErrors* plot_spectrum( const int Cent1 = 0,
			       const int Cent2 = 10,
			       const char *cut = favoritecut, 
			       const char *type = "full", 
			       const float binshift = 1 );
  
  TGraphErrors* plot_spectrum_from_ascii( char *file,
					  TGraphErrors* &errBand );
  
  TGraphErrors* plot_spectrum_from_ascii( const int Cent1 = 0, 
					  const int Cent2 = 10, 
					  const char *cut = "emcal" );
  
  void plot_all_spectra_run2( const char *cut = "emcal",
			      const char *spectra = "full", 
			      const char *type_of_plot = "global",
			      double relativeScaling = 0.1, 
			      bool readAscii = false );
  
  TH1* plot_interpolated_pp_spectrum_130(const double scale = 1.);

  double sigma_pp_tot( const double sqrts = 200. );
  double sigma_pp_el( const double sqrts = 200. );
  double sigma_pp_inel( const double sqrts = 200. );

  TF1* plot_pp_sigma_parametrization( const char* partic = "pi0", 
				      const char* type = "blattnig",
				      double sqrts = 19.4,
				      const double scale = 1.,
				      double eta = 0.0 );

  void plot_xT_scaling_130_200();
  
  void plot_spectra_RHIC_SPS( const int Cent1 = 0,
			      const int Cent2 = 10,
			      const char* cut = "emcal",
			      const bool readAscii = false );

  void plot_pp_picharged_spectra_ISR( const TString type = "corr" );

  TGraphErrors* plot_pp_pi0_spectra_ISR( const TString type = "corr" );
  void plot_pp_charged_spectra_ISR( );

  TGraphErrors* plot_gamma_over_pi0( const double sqrts = 62.4, TString type = "tot" );
  TGraphErrors* plot_ratio_gamma( const double sqrts = 200., 
				  const TString num = "dir", const TString den = "tot",
				  TCanvas *currCanvas = 0 );

  void plot_pp_charged_vs_nlo_RHIC( const TString dataset = "star", const int eta = 0 );

  void plot_DIS_nuclear();
  void plot_pp_cross_sections_vs_sqrts();
  void plot_ee_gg_gp_cross_sections_vs_sqrts();

  void plot_comparison_spectra( const char* file1, 
				const char* file2,
				const char* file3 = 0,
				const double scale1 = 1.,
				const double scale2 = 1.,
				const double scale3 = 1.,
				const char* Xtitle = "p_{T} (GeV/c)",
				const char* Ytitle = "1/[2#pi p_{T}N_{evt} ] d^{2}N/dp_{T}d#eta (GeV/c)^{-2}",
				const char* title = "comparison_1_2" );
  
  void plot_comparison_AuAu_pp( const int Cent1 = 0, 
				const int Cent2 = 10,
				const char*cut = "emcal" );

  void plot_comparison_pbsc_pbgl( const int Cent1 = 0, 
				  const int Cent2 = 10,
				  const char* cut = favoritecut,
				  const bool correctedyields = true );
  
  void plot_comparison_qm_new( const int Cent1 = 0,
			       const int Cent2 = 10,
			       const char* cut = favoritecut );
  
  TGraphErrors* plot_ratio_spectra( const char* file1, 
				    const char* file2,
				    const char* Xtitle = "p_{T} (GeV/c)",
				    const char* Ytitle = "ratio",
				    const char* title = "ratio_1_2", 
				    const bool ratiofits = false );

  
  TGraphErrors* plot_ratio_centralities( const int Cent1 =  0, 
					 const int Cent2 = 10, 
					 const int Cent3 = 80, 
					 const int Cent4 = 92,
					 const char* cut = "emcal",
					 bool readAscii = true,
					 const bool bin_scaling = true);
  
  void plot_all_ratios_centralities( const char* cut = "emcal",
				     const bool readAscii = true,
				     const bool bin_scaling = true);
  
  void plot_ratio_pbsc_pbgl( const int Cent1 = 0, 
			     const int Cent2 = 10,
			     const char* cut = favoritecut,
			     const bool correctedyields = true );
  
  void plot_ratio_qm_new( const int Cent1 = 0, 
			  const int Cent2 = 10, 
			  const char* cut = favoritecut);

  void plot_ratio_pi0_others_all_cent( const char* others = "pi", 
				       const char* cut = "emcal" );

  TGraphErrors*  plot_ratio_pi0_others( const int Cent1 = 0 , const int Cent2 = 10,
					const char* others = "pi",
					const char* cut = "emcal" );
  
  TGraphErrors* plot_RAA( const int Cent1 = 0, 
			  const int Cent2 = 10,
			  const char* cut = "emcal",
			  bool readAscii = true,
			  const bool bin_scaling = true,
			  const bool return_RAA_w_fitted_pp_denom = false,
			  TString additionalRAA = "",
			  const bool useTABinsteadOfNcoll = true );
  
  TGraphErrors* plot_RAA_RHIC_SPS_alphaISR( const int Cent1 = 0, 
					    const int Cent2 = 10,
					    const int CentSPS = 1,
					    const char* cut = "emcal" );
  
  TGraphErrors* plot_RAA_RHIC_SPS_ISR( const int Cent1 = 0, 
				       const int Cent2 = 10,
				       const int CentSPS = 7,
				       const char* cut = "emcal" );

  TGraphErrors* plot_RAA_pi0_62GeV(const int Cent1 = 0,const int Cent2 = 10);

  void plot_RAA_charged_RHIC(const int Cent1 = 0,const int Cent2 = 10);

  void plot_Rcp_RHIC(const char *type="mesons");

  void plot_Rcp_brahms(const double eta = 4.);

  void plot_Rcp_dAu_muons(const char *type="1.8_north");

  void plot_RAA_2_centralities( const int Cent1 = 0 , 
				const int Cent2 = 10,
				const int Cent3 = 80, 
				const int Cent4 = 92,
				const char* cut = "emcal",
				const bool readAscii = true,
				const bool bin_scaling = true,
				const bool return_RAA_w_fitted_pp_denom = false );

  TGraphErrors* plot_all_RAA( const char* cut = "emcal",
			      bool readAscii = true,
			      const bool bin_scaling = true,
			      const bool return_RAA_w_fitted_pp_denom = false );

  void plot_v2_RHIC(const char *type="id");

  TGraphErrors* plot_RdA( const int Cent1 = 0,
			  const int Cent2 = 88, // sigma(d+Au min.bias) = 88 +/- 4%
			  const char* cut = "emcal",
			  const bool bin_scaling = true,
			  TString additionalRdA = "",
			  const bool useTABinsteadOfNcoll = true,
			  const bool readAscii = true );

  TGraphErrors* plot_ratio_cuts( const int Cent1 = 0 , 
				 const int Cent2 = 10, 
				 const char* cut1 = favoritecut, 
				 const char* cut2 = "noPID",
				 const double scale1 = 1.,
				 const bool plotComparisonToo = true );
  
  void plot_all_ratios_cuts( const int Cent1 = 0 , 
			     const int Cent2 = 10,
			     double ptmax = 10.5, 
			     const char* refcut = favoritecut );
  
  TGraphErrors* combine_centralities( const int Cent1min = 0, 
				      const int Cent2max = 20,
				      const char *cut = favoritecut,
				      const char *set = "emcal" );

  TGraphErrors *average_spectra( const char* file1,
				 const char* file2,
				 const char* Xtitle = "p_{T} (GeV/c)",
				 const char* Ytitle = "1/[2#pi p_{T}N_{evt} ] d^{2}N/dp_{T}d#eta (GeV/c)^{-2}",
				 const char* title = "average_1_2", 
				 const int binshift = 1,
				 const char* average_type = "PDG");

  TGraphErrors *average_pbsc_pbgl( const int Cent1 = 0, 
				   const int Cent2 = 10,
				   const char* cut = favoritecut,
				   const char* average_type = "PDG" );
  
  TGraphErrors *suppress_vs_centrality( const char *suppress_vs_what = "Npart",
					const char* cut = "emcal",
					double pTmin = 4.,
					double ptmax = 100.,
					const bool bin_scaling = true,
					const bool UseIntHagFit = false,
					TCanvas *c1 = 0,
					const bool print_Ncoll_errors = true,
					const TString additionalSuppr = "charged" );

  void suppress_2scalings_vs_centrality( const char* cut = "emcal", 
					 double ptmin = 4., double ptmax = 100.,
					 const bool UseIntHagFit = false );
  
  void suppress_3reacplane_vs_centrality( const char* cut= "emcal", 
					  double ptmin = 4., 
					  double ptmax = 100.,
					  const bool bin_scaling = true,
					  const bool UseIntHagFit = false);

  TGraphErrors* yield_vs_centrality( const char *suppress_vs_what = "Npart",
				     const char* cut = "emcal", 
				     double pt = 4.,
				     bool part_scaling = true );

  TGraphErrors* dNdyAuAu_over_dNdypp_vs_centrality( const char *centrality_type = "Npart",
						    const char* cut = "emcal", 
						    double pt = 0.,
						    bool part_scaling = true);
  
  int plot_corr_factors( const int Cent1 = 0, 
			 const int Cent2 = 10,
			 const char* cut = favoritecut,
			 const char* type = "total" );

  TGraphErrors* plot_correction( const char* type = "eff",
				 const int Cent1 = 0, 
				 const int Cent2 = 10,
				 const char* cut = favoritecut,
				 const double ptmin = 0.,
				 const double ptmax = 20. );
  
  void plot_all_corr_factors( const char* cut = favoritecut );

  double chi2( TF1 *ref, TGraphErrors*data);

  double chi2( TF1 *ref, TGraphErrors*data, double start );

  //double* weighted_average( const std::vector<double> vals, const std::vector<double> errs );
  double* weighted_average( const double *vals, const double *errs, const int N );

//_____________________________________________________________________________

  void keepPlot(bool plot=true) { fPlot=plot; }
  void setLogy(bool logy) { fLogy=logy; }
  void setXaxis(const TString xaxis_type ) { fXaxis = xaxis_type; }
  void setFitFunc(const TString function="pol0") { fFitFunc = function; }
  void setFit(bool fit = true ) { fFit = fit; }
  void setConstrainedHagFit( bool constr = true ) { fConstrainedHagFit = constr; }

  void print();

  void saveCanvas( TCanvas *c1 );

  void setTitle(const char* title);
  void setLegends(const char* legend1, const char* legend2);
  void setText(const char* text);

  void setDataTablesPath(const char* datapath);

  void setEfficParticle(const char* partic);

  void setVerbose(int verbose=1) { fVerbose=verbose; }
  void setSaveGifs(bool saveGifs=true) { fSaveGifs=saveGifs; }
  void setSaveEps(bool saveEps=true ) { fSaveEps=saveEps; }
  void setDumpLaTeX(bool dumpLaTeX=true ) { fdumpLaTeX=dumpLaTeX; }
  void setDumpAscii(bool dumpAscii=true ) { fdumpAscii=dumpAscii; }

  void dumpLaTeX( TGraphErrors *g1,
		  double *eStat=0, double *eSyst=0,
		  double *eCCindep=0, double *epTcorr=0 );

  void dumpAscii( TGraphErrors *g1,
		  double *eStat=0, double *eSyst=0,
		  double *eCCindep=0, double *epTcorr=0 );

  TGraphErrors* get_pi0_WA98_RAA();

  TGraphErrors* plot_klaus_pi0_WA98_RAA( TCanvas *c10 = 0 );

  TCanvas* canvas(const char *title="canvas", const int sizex = 600, const int sizey = 500);

  TH2F* frame(const char *title="frame", 
	      const int xbins=100, const double xmin=0., const double xmax=100.,
	      const int ybins=100, const double ymin=0., const double ymax=100.,
	      const char *xtitle="p_{T} (GeV/c)", 
	      const char *ytitle="1/[2#pi p_{T}] d^{2}N/dp_{ T}dy (GeV/c)^{-2}");

//_____________________________________________________________________________

private:

  // private functions here

private:

  TList* fCanvasList;
  TString* fPlotTitle;
  TString* fLegend1;
  TString* fLegend2;
  TString* fText;

  TString* fDataTablesPath;

  TString* fEfficParticle; // "pi0" or "eta"

  bool fPlot;
  bool fLogy;
  bool fFit;
  bool fConstrainedHagFit;

  TString fFitFunc;
  TString fXaxis; // "pT", "xT", or "mT"
  int fVerbose;
  bool fSaveGifs;
  bool fSaveEps;
  bool fdumpLaTeX;
  bool fdumpAscii;
};

#endif
