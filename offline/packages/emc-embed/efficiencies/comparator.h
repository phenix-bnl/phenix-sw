#ifndef __comparator_h__
#define __comparator_h__

class TFile;
class TList;
class TH1;
class TF1;
class TString;
class TDirectory;
class TGraphErrors;

#include <vector>

class comparator
{

public:

  comparator(const char* file1=0, const char* file2=0);
  ~comparator();

  void compare2centralities(const char* histoname, 
			    const char* cutname,
			    const char* cent1, const char* cent2);

  void compare2cuts(const char* histoname, 
		    const char* cutname1,
		    const char* cutname2,
		    const char* cent,
		    bool limits=false);

  void compare2files(const char* histoname, 
		     const char* cutname,
		     const char* cent);

  void comparetoraw(const char* cutname, const char* cent, bool ptshift=false);

  void compare2raw(const char* cut1, const char* cut2, 
		   int cent1, int cent2,
		   const char* type="raw",
		   const char* type="raw",
		   bool fit=false);

  TF1* efficiencyFit(const char* cutname, const char* cent);

  void efficiencyFit(const char* cutname, ostream& out = std::cout);

  double efficiencyScale(void) const { return fScale; }

  static void getCentralityClassLimits(const char* cent, 
				       int& cent1, int& cent2);

  static int getCentralityClass(int cent1, int cent2);

  static double getEvents(const int Cent1, const int Cent2);

  TH1* getCombined(TDirectory* dir);

  TH1* getHistogram(TDirectory* dir, const char* histoname);

  TH1* getRecoPt(TDirectory* dir);

  TF1* hagedornFit(TH1* h, double ptmin=1, double ptmax=7);

  static TF1* hagedornFit(TGraphErrors* g, double ptmin=1, double ptmax=7,
			  const char* functionName="hagedorn",
			  int verbose=0);

  static TGraphErrors* getRealSpectrum(int c1, int c2, 
				       const char* type,
				       const char* cut="t0a0",
				       bool ptshift=false);
  
  void setEfficiencyScale(double scale=0.8);

  void setLegends(const char* legend1, const char* legend2);

  void setLogy(bool logy) { fLogy=logy; }

  void setTitle(const char* title);

  void setFit(const char* function="pol0");

  void setVerbose(int verbose=0) { fVerbose=verbose; }

  void cd(const char* dir) { cd1(dir); }

  void ls(void);

  void cd1(const char* dir);
  void cd2(const char* dir);
 
  static double quadraticRelativeError(double a, double ea, 
				       double b, double eb);

private:

  void makeCode(ostream& out, std::vector<double>& p0);

  TGraphErrors* getGraphErrors(TH1* h1, TGraphErrors* g1);

  static double getMeanPt(double ptini, double ptbinwidth, TF1* hagedorn);

  double integral(TGraphErrors& g, double xmin, double xmax);

  void plot(TH1* h1, TH1* h2, const char* basename, const char* method,
	    bool limits=false,bool fit=false);

  void plot(TGraphErrors* g1, TGraphErrors* g2, 
	    const char* basename, const char* method);

  void minmax(TH1* h, double& minimum, double& maximum,
	      double xmin, double xmax);

  void scale(TGraphErrors& g, double a);

  static double shiftPt(TGraphErrors& g, TF1* hagedorn);

private:
  TFile* fFile1;
  TFile* fFile2;
  TList* fCanvasList;
  TString* fLegend1;
  TString* fLegend2;
  TString* fPlotTitle;
  TString* fFit;
  double fScale;
  bool fLogy;
  int fVerbose;
};

#endif
