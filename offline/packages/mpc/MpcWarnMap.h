
#ifndef __MPCWARNMAP_H__
#define __MPCWARNMAP_H__

#include <string>
#include <math.h>
#include <phool.h>
#include <MpcNoise.h>
#include <MpcMap.h>
#include <TF1.h>
#include <TGraph.h>
#include <TString.h>
#include <cstring>


/*-----------------------------------------------------------------------------
Beau Meredith 08-13-08
   This class uses the MpcNoise object to access the hit data stored in the 
calibrations database (the number of hits for each tower as well as the 
number of triggers are stored here).  Hit data for four different thresholds
is stored in the database and is accessible in the MpcNoise object.
   N/Ntrig vs eta for each tower is plotted using a LST linear least squares 
regression w/ the option "ROB=0.x", 0.x is the % of data you want to use in 
the fit (it is used so outliers are not included).
   A list of towers outside 4sigma (by default) are then produced and these 
are considered to be the "noisy" towers.  This list can easily be dumped into 
a text file using one of the FilePrint methods 
*/


class MpcWarnMap
{
  private:
  //I declare these 1st so they can be used in public:
   static const int NTHRESH = 4;
   // chris P.: no idea why, but gcc4.1 barfs on those two maxch vars
   // moved them into implementation
//   static const int MAXCH = 576;
//   static const int HALF_MAXCH = 288;
   static const int NMPC = 2;
  
 public:
  
  MpcWarnMap();
  MpcWarnMap(const std::string infile);
  MpcWarnMap(int runNumber);
  MpcWarnMap(int runNumber, float chisqcut, float fitarg);
  virtual ~MpcWarnMap(){}
  
  //use Reset() before re-initialization; else there will be hanging pointers
  void Initialize(int runNumber); 
  void Initialize(const std::string infile); 
  void Reset();
  void ResetNoise(){ mpcnoise->Reset();}
  
  int CreateGraphs(int debug=0);
  int FitGraphs();  //need to create graphs 1st
  void DrawGraphs(const char* file, int mpc/*0=>south, 1=>north*/, int save=1);  //need to create graphs 1st
  void DrawGraphsBounds(const char* file, int mpc, int save=1);  //need to create graphs and fit them 1st
  
  void Summarize(); //prints summary noisy tower info to stdout
  
  //PointVal means the N/N_tot value for a tower
  //LineDiff means y-displacement of PointVal from LTS regression line
  //LineDiffRatio means LineDiff/sqrt(ChiSqPerDOF)
  //LineDiffRatioCorrected means LineDiff/sigma, sigma = sqrt(ChiSqPerDOF)/correction
  //IsOk => LineDiff < SigmaCut*sigma

  //Printing format for all print functions is
  //  ch   N_1   N_2   N_3   N_4
  // where N_1-4 correspond to values for thresholds 1-4

  void PrintPointVal();
  void PrintLineDiff();
  void PrintLineDiffRatio();
  void PrintLineDiffRatioCorrected(); //This might be the most useful
  void PrintIsOk();
  
  void FilePrintPointVal(const char* fname);
  void FilePrintLineDiff(const char* fname);
  void FilePrintLineDiffRatio(const char* fname);
  void FilePrintLineDiffRatioCorrected(const char* fname);  //This might be the most useful
  void FilePrintIsOk(const char* fname);
 
  //This parameter (FitArg) specifies the % of data to use in the LTS fit
  //e.g. if you have use FitArg=0.7, the fit only uses the innermost 70%
  //This enables one to exclude outliers
  void NormGraphs(){norm_graphs = 1;}
  void UnNormGraphs(){norm_graphs = 0;}
  void SetFitArg( float fitarg  ); //default is 0.7
  void SetSigmaCut( float cut ){SigmaCut = fabs(cut);} //default is 4; Used in the IsOk function
  void SetEta(float lo=3.0, float hi=4.0){eta_lo = lo; eta_hi = hi;}
  float GetEtaLo(){return eta_lo;}
  float GetEtaHi(){return eta_lo;}
  void SetPercentNoisy(float pct, int impc, int thr);
  void CalcPercentNoisy(int mpc, int thr);
  void CalcAllPercentNoisy();
    
  TString GetFitOpt(){ return FitString;}
  float GetFitArg(){return FitArg;}
  float GetSigmaCut(){ return SigmaCut;} 
  float GetPercentNoisy(int mpc, int thr);

  float GetChiSq(int mpc, int thr); //These are all fit parameters 
  float GetNDF(int mpc, int thr);
  float GetChiSqPerDOF(int mpc, int thr);
  float GetSigma(int mpc, int thr);
  float GetFitSlope(int mpc, int thr);
  float GetFitIntercept(int mpc, int thr);

  float GetPointVal(int thr, int ch); //These are used in the print functions
  float GetLineDiff(int thr, int ch);
  int IsOk(int thr, int ch);
  
  int GetNbad(int mpc, int thr); //Returns total # of "noisy towers" for a given threshold
  int GetNtowers(int mpc){
    if(mpc<0 || mpc>1) {std::cout << PHWHERE << "Bad mpc integer\n"; return -9999;}
    return N_towers[mpc];
  }
    
  int GetMpc(int ch); //Returns 0 for south, 1 for north
  
  float GetCorrection(float fitarg, int mpc, int thr);
  float GetCorrection_fast(float fitarg, int mpc, int thr);
 
  const TGraph* GetGraph(int mpc, int thr); //const in this context means the return value cannot be modified
  const TF1* GetFit(int mpc, int thresh);   //i.e. the returned pointer cannot be changed
  const TF1* GetFitBound(int mpc, int thresh, int bound);  //0 for lower bound, 1 for upper bound
 
  MpcNoise* mpcnoise;
  float PseudoRapidity(int ch);
  void InitBadList();
  int GetRun(){ return mpcnoise->getRun();}
 private:
  
  TGraph* NvsEta[NMPC][NTHRESH];       
  TF1* FitPol1Bounds[NMPC][NTHRESH][2];
  TF1* FitPol1[NMPC][NTHRESH];
  
  //old correction
  TF1* FitCorr;
  
  //new correction
  TF1* FitRMS;
  
  MpcMap* mpcmap;
  
  TString FitString;// ="ROB=0.7"; defaults
  float FitArg; // 0.7 by default
  float SigmaCut;// = 4.0;
  float PercentNoisy[NMPC][NTHRESH]; //# between 0,1
  int N_towers[NMPC]; //total # of mpc towers in N,S mpcs
  
  bool norm_graphs;
  float eta_lo;
  float eta_hi;

  int badlist[NMPC][18][18];
  
};



#endif	// __MPCWARNMAP_H__

