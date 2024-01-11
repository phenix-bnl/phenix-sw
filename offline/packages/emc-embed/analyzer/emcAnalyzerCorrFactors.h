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


#ifndef __emcAnalyzerCorrFactors_h__
#define __emcAnalyzerCorrFactors_h__

class TGraphErrors;

namespace emcAnalyzerCorrFactors
{
  
  double acceptance( const double pT, const char* type="value", const char* partic="pi0");
  
  double acceptance_old( const double pT, const char* type="value");
  
  double efficiency( const double pT, 
		     const int CentClass, 
		     const char* cut,
		     const char* type, 
		     const char* partic="pi0");
  
  double efficiencyForCut( const double pT, 
			   const int centClass, 
			   const char* cut, 
			   const char* type, 
			   const char* partic="pi0");
  
  double offVertex( const char* type);
  
  void getParamsPointers( const char* cut,
			  double*& p0,
			  double*& p0err,
			  double*& p1,
			  double*& p1err,
			  double*& p2,
			  double*& p2err,
			  double*& p3,
			  double*& p3err,
			  double*& p4,
			  double*& p4err, 
			  double*& p5,
			  double*& p5err,
			  double*& p6,
			  double*& p6err, 
			  const char* partic="pi0");
  
  double getEnScaleRelError( const double pT );

  double getCentCorrelatRelError( const double pT );

  double getpTCorrelatRelError( const double pT );
  
  void AddOrSubstractRelatErrors( TGraphErrors *g = 0,
				  TString error_type = "",
				  TString option = "",
				  const int verbose = 0 );

  double corrFactor( const double pT, 
	             const int CentClass, 
		     const char *cut="tof1chisq1",
		     const char *type="value",
		     const char *partic="pi0");
  
  void dump(const char* cut, const char* partic);
  
} // end of namespace

#endif
