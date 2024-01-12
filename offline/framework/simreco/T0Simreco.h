#ifndef __T0Simreco_H__
#define __T0Simreco_H__

// $Id: T0Simreco.h,v 1.3 2011/12/05 16:56:58 pinkenbu Exp $

/*! 
  \file T0Simreco.h
  \brief get simulated vertex and store into BBC node
  \version $Revision: 1.3 $
  \date $Date: 2011/12/05 16:56:58 $
*/

#include <preco/T0Reco.h>

#ifndef __CINT__
#include <gsl/gsl_rng.h>
#endif

class PHCompositeNode;

//! store simulated event time in T0Out node 
/*!
  get simulated vertex (either from fixed value or from PISA header),
  possibly add some gaussian smearing to z and t0,
  and store into BBC node
*/
class T0Simreco: public T0Reco
{
 public:
  
  //! constructor
  T0Simreco(const std::string &name = "T0SIMRECO");
  
  //! destructor
  virtual ~T0Simreco();
  
  //! module initialization
  int Init(PHCompositeNode* );
 
  //! event mode
  int process_event(PHCompositeNode* );
  
  //! T0 fixed value
  void T0(const double rval) 
  { _t0 = rval;}
  
  //! T0 smearing
  void T0Sigma(const double rval) 
  { _t0_sigma = rval;}
   
  //! set to true if BBCOut contents should be overwritten by sim header
  /*! this is for compatibility with the old behavior */
  void OverwriteBBC( const bool value )
  { _overwrite_bbc = value; }

  protected:
  
  //! fixed vertex t0
  double _t0;

  //! vertex t0 smearing
  double _t0_sigma;
  
  //! set to true if BBCOut contents should be overwritten by sim header
  /*! this is for compatibility with the old behavior */
  bool _overwrite_bbc; 
      
#ifndef __CINT__
  gsl_rng *rng;
#endif

};

#endif /* __T0Simreco_H__ */
