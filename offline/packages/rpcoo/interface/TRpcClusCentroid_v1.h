// $Id: TRpcClusCentroid_v1.h,v 1.2 2008/08/28 00:50:17 kempel Exp $

#ifndef __TRpcClusCentroid_v1_h__
#define __TRpcClusCentroid_v1_h__

/*!
	\file TRpcClusCentroid_v1.h
	\brief Rpc cluster centroid
	\author H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2008/08/28 00:50:17 $
*/

#include <PHException.h>

#include "TRpcClusCentroid.h"

/*! @ingroup interface */
//! Rpc cluster centroid
class TRpcClusCentroid_v1: public TRpcClusCentroid
{
	
	public:

  //! @name Constructors/Destructors
	//@{
  //! Constructor
  TRpcClusCentroid_v1(UShort_t strip = 0 ):
			_peak_strip(strip),
			_q_peak(0),
			_q_tot(0),
			_q_tot_error(0), 
			_t(0),
			_t_error(0),
			_x(0),
			_y(0)
  {
    std::fill(_covar,_covar+COVAR_SIZE,0);
	}

  //! Constructor
  TRpcClusCentroid_v1( const TRpcClusCentroid& ref ):
			TRpcClusCentroid( ref ),
			_peak_strip( ref.get_peak_strip() ),
			_q_peak( ref.get_q_peak() ),
			_q_tot( ref.get_q_tot() ),
			_q_tot_error( ref.get_q_tot_error() ), 
			_t( ref.get_t() ),
			_t_error( ref.get_t_error() ),
			_x( ref.get_x() ),
			_y( ref.get_y() )
  {
    for( unsigned int i=0; i<COVAR_ROW; i++ )
    for( unsigned int j=0; j<COVAR_ROW; j++ )
		set_covar( i, j, ref.get_covar(i,j) );
	}

  //! Constructor
  TRpcClusCentroid_v1( const TRpcClusCentroid* ptr ):
			TRpcClusCentroid( *ptr ),
			_peak_strip( ptr->get_peak_strip() ),
			_q_peak( ptr->get_q_peak() ),
			_q_tot( ptr->get_q_tot() ),
			_q_tot_error( ptr->get_q_tot_error() ), 
			_t( ptr->get_t() ),
			_t_error( ptr->get_t_error() ),
			_x( ptr->get_x() ),
			_y( ptr->get_y() )
  {
    for( unsigned int i=0; i<COVAR_ROW; i++ )
    for( unsigned int j=0; j<COVAR_ROW; j++ )
		set_covar( i, j, ptr->get_covar(i,j) );
	}

  //! destructor
  virtual ~TRpcClusCentroid_v1()
  {}

  //@}

  //! @name Functional Interface
  //@{    
  //! Peak strip number 
  virtual UShort_t get_peak_strip() const 
  {return _peak_strip;}
   
  //! Peak charge associated with this centroid fit 
  virtual Float_t get_q_peak() const 
  {return _q_peak;}
  
  //! Total charge associated with this centroid fit 
  virtual Float_t get_q_tot() const 
  {return _q_tot;}
   
  //! Error on total charge 
  virtual Float_t get_q_tot_error() const 
  {return _q_tot_error;}
  
  //! time associated with this centroid fit 
  virtual Float_t get_t() const 
  {return _t;}
   
  //! Error on centroid time 
  virtual Float_t get_t_error() const 
  {return _t_error;}

	//! centroid x
	virtual Float_t get_x() const
	{return _x;}
  
	//! centroid y
	virtual Float_t get_y() const
	{return _y;}
	
	//! error on xy
	virtual Float_t get_covar( UShort_t i, UShort_t j ) const
	{ 
	  UShort_t index = i*COVAR_ROW + j;
    BOUNDS_CHECK(index,COVAR_SIZE);
    return _covar[index];
	}
  
  //! Peak strip number 
  virtual void set_peak_strip(UShort_t strip) 
  { _peak_strip = strip; }
  
  //! Peak charge associated with this centroid fit 
  virtual void set_q_peak(Float_t q_peak) 
  { _q_peak = q_peak; }
  
  //! Total charge associated with this centroid fit 
  virtual void set_q_tot(Float_t q_tot) 
  { _q_tot = q_tot; }
  
  //! error on total charge 
  virtual void set_q_tot_error( Float_t value )
  { _q_tot_error = value; }
    
  //! time associated with this centroid fit 
  virtual void set_t(Float_t t) 
  { _t = t; }
  
  //! error on time associated with this centroid fit 
  virtual void set_t_error( Float_t value )
  { _t_error = value; }
 
	//! centroid x
	virtual void set_x( Float_t x )
	{ _x = x; }
  
	//! centroid y
	virtual void set_y( Float_t y )
	{ _y =  y; }
	
	//! covariance matrix on xy
	virtual void set_covar( UShort_t i, UShort_t j, Float_t value )
	{
		UShort_t index = i*COVAR_ROW+j;
    BOUNDS_CHECK(index,COVAR_SIZE);
    _covar[index] = value;
	}
  
  //@}

  //! @name Dumpers
  //@{    
  //! Print centroid data
  virtual void print(std::ostream& os = std::cout) const;
  //@}

	private:
	
	//! peak strip
	UShort_t _peak_strip;
	
	//! peak charge
	Float_t _q_peak;
	
	//! total charge
	Float_t _q_tot;
	
	//! error on total charge
	Float_t _q_tot_error;
	
	//! time
	Float_t _t;
	
	//! error on time
	Float_t _t_error;
	
	//! postion along x
	Float_t _x;
	
	//! position along y
	Float_t _y;
	
	//! covariance matrix
  Float_t _covar[COVAR_SIZE];
			
  ClassDef(TRpcClusCentroid_v1,1)
	
};

#endif
