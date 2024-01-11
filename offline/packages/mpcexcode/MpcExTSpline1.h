
// Derived from TMVA::TSpline1 by J. Lajoie - 8/26/2016

// @(#)root/tmva $Id: MpcExTSpline1.h,v 1.1 2016/09/01 12:27:58 lajoie Exp $
// Author: Andreas Hoecker, Joerg Stelzer, Helge Voss, Kai Voss 

#ifndef MPCEX_TSpline1
#define MPCEX_TSpline1

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TSpline1                                                             //
//                                                                      //
// Linear interpolation class                                           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

class TGraph; 

class MpcExTSpline1 {

 public:
  
  MpcExTSpline1( TGraph* theGraph );
  virtual ~MpcExTSpline1( void );

  virtual  Double_t Eval( Double_t x ) const;

  const TGraph* GetGraph() const { return fGraph; }
      
 private:

  TGraph *fGraph;  // graph that is splined
    
};


#endif 


