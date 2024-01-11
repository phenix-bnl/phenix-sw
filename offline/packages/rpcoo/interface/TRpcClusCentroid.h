// $Id: TRpcClusCentroid.h,v 1.2 2008/08/28 00:50:16 kempel Exp $

#ifndef __TRpcClusCentroid_h__
#define __TRpcClusCentroid_h__

/*!
	\file TRpcClusCentroid.h
	\brief Rpc cluster centroid
	\author H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2008/08/28 00:50:16 $
*/

#include <TObject.h>
#include <iostream>

/*! @ingroup interface */
//!  Rpc cluster centroid
class TRpcClusCentroid: public TObject
{
	
	public:
			
  //! @name Constructors/Destructors
	//@{

  //! Construct with peak strip, w, w_error, and q 
  TRpcClusCentroid(UShort_t peak_strip=0 )
  {}

  //! destructor
  virtual ~TRpcClusCentroid()
  {}

  //@}

  //! @name Functional Interface
  //@{    
  //! Peak strip number 
  virtual UShort_t get_peak_strip() const 
  {return 0;}
   
  //! Peak charge associated with this centroid fit 
  virtual Float_t get_q_peak() const 
  {return 0;}
  
  //! Total charge associated with this centroid fit 
  virtual Float_t get_q_tot() const 
  {return 0;}
   
  //! Error on total charge 
  virtual Float_t get_q_tot_error() const 
  {return 0;}
  
  //! time associated with this centroid fit 
  virtual Float_t get_t() const 
  {return 0;}
   
  //! Error on centroid time 
  virtual Float_t get_t_error() const 
  {return 0; }

	//! centroid x
	virtual Float_t get_x() const
	{return 0;}
  
	//! centroid y
	virtual Float_t get_y() const
	{return 0;}
	
	//! error on xy
	virtual Float_t get_covar( UShort_t i, UShort_t j ) const
	{ return 0; }
	 
  //! Peak strip number 
  virtual void set_peak_strip(UShort_t strip) 
  {}
  
  //! Peak charge associated with this centroid fit 
  virtual void set_q_peak(Float_t q_peak) 
  {}
  
  //! Total charge associated with this centroid fit 
  virtual void set_q_tot(Float_t q_tot) 
  {}
  
  //! Error on total charge 
  virtual void set_q_tot_error( Float_t value )
  {}
   
  //! time associated with this centroid fit 
  virtual void set_t(Float_t t) 
  {}
  
  //! error on time associated with this centroid fit 
  virtual void set_t_error( Float_t value )
  {}
  
	//! centroid x
	virtual void set_x( Float_t )
	{}
  
	//! centroid y
	virtual void set_y( Float_t )
	{}
	
	//! error on xy
	virtual void set_covar( UShort_t i, UShort_t j, Float_t )
	{}
  
  //@}

  //! @name Dumpers
  //@{    
  //! Print centroid data
  virtual void print(std::ostream& os = std::cout) const 
  {}
  //@}

	//! covariance matrix size
  enum { COVAR_ROW=2, COVAR_SIZE=4 };
  
  ClassDef(TRpcClusCentroid,1)
	
};

#endif
