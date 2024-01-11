// $Id: TRpcClus_v1.h,v 1.2 2008/08/28 00:50:18 kempel Exp $

#ifndef __TRpcClus_v1_h__
#define __TRpcClus_v1_h__

/*!
   \file    TRpcClus_v1.h
   \brief   Rpc cluster interface object
   \author  H. Pereira Da Costa
   \version $Revision: 1.2 $
   \date    $Date: 2008/08/28 00:50:18 $
*/

/*! @ingroup interface */
//!  Rpc cluster object 
/*!  Rpc cluster object.  This interface object represents groups of
  contiguous strips in the Rpc detector.  The TRpcHit object are associated using
  the standard MUTOO object association interface.  In addition to the associated hits
  cluster objects are composed of a list of TRpcClusCentroid objects.  These objects
  represent the centroid information in x,y coordinates that is derived
  after the cluster has been fit.  The chi-squared statistics associated with the cluster
  is the only additional data member
*/

#include "TRpcClus.h"

class TRpcClus_v1 : public TRpcClus
{
  
	public:
	  
  //! @name Constructors/Destructors
  //@{    
  
  //! constructor 
  TRpcClus_v1();
  
  //! Construct with key 
  TRpcClus_v1(const Key& key, UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg, UShort_t index );
	   
  //! constructor 
  TRpcClus_v1(const TRpcClus& base_ref);

  //! constructor 
  TRpcClus_v1(const TRpcClus* base_ptr);
	  
  //! Destructor 
  virtual ~TRpcClus_v1() 
	{ clear_centroid_list(); }
  //@}
  
  //! @name Functional Interface
  //@{    

  //! The merit statistic associated with cluster fit 
  virtual Float_t get_chi_square() const 
  { return _chi_square;}
    
  //! The merit statistic associated with cluster fit 
  virtual void set_chi_square( Float_t chi_square ) 
  { _chi_square = chi_square;}  

  //! construct a new centroid adds to the list, return pointer 
  virtual TRpcClusCentroid* insert_new_centroid( UShort_t peak_strip  )
  {
		TRpcClusCentroid *centroid = new centroid_value_type( peak_strip );
		_centroid_list.push_back( centroid );
		return centroid;
  }  

  //! clear centroid list 
  virtual void clear_centroid_list()
  {
		for( centroid_iterator iter = _centroid_list.begin(); iter != _centroid_list.end(); iter++ )
		if( *iter ) delete *iter;
		_centroid_list.clear();			
	}			     

  //! number of centroids in this cluster 
  virtual size_t get_n_centroid() const 
  { return _centroid_list.size(); }

  //! read only access to centroid list 
  virtual centroid_list get_centroid_list( void ) const 
  { return _centroid_list; }
  
  //@}

  //! @name Locators
  //@{  
  //! Arm [0,1] 
  virtual UShort_t  get_arm() const 
  {return _arm;}
  
  //! Station [0,2] 
  virtual UShort_t  get_station() const 
  {return _station;}
  	
  /*! Octant [0,7] */
  virtual UShort_t get_octant() const 
  {return _octant;}
  
  /*! Half Octant [0,1] */
  virtual UShort_t get_half_octant() const 
  {return _halfoctant;}
	
  /*! Radial Segment (see RPCGEOM.h) */
  virtual UShort_t get_rseg() const 
  {return _rseg;}

  //! Index
  virtual UShort_t get_index() const 
  {return _index;}  
  
  //! Arm [0,1] 
  virtual void set_arm(UShort_t arm)
  { _arm = arm;}
  
  //! Station [0,2] 
  virtual void set_station(UShort_t station)
  { _station = station; }               	
  
  /*! Octant [0,7] */
  virtual void set_octant(UShort_t octant)
  { _octant = octant; }	

  /*! Half Octant [0,1] */
  virtual void set_half_octant(UShort_t halfoctant)
  { _halfoctant = halfoctant; }		

  /*! Radial Segment (see RPCGEOM.h) */
  virtual void set_rseg(UShort_t rseg)
  { _rseg = rseg; }

  //! Index
  virtual void set_index(UShort_t index) 
  { _index = index; }  

  //@}

  //! @name Cluster Status
  //@{  

  //! Get the status word 
  virtual ULong_t get_status() const 
  { return _status;}
  
  //! Clear the status word 
  virtual void clear_status() 
  { _status = 0; }
  //@}
  
  //! @name Dumpers
  //@{    
  //! Print cluster contents 
  virtual void print(std::ostream& os = std::cout) const;

  //@}

	private:
	
	//! arm 
	UShort_t _arm;
	
	//! station
	UShort_t _station;

	//! octant
	UShort_t _octant;
	
	//!halfoctant
	UShort_t _halfoctant;
	
	//radial segment
	UShort_t _rseg;

	//! index
	UShort_t _index;
	
	//! chisquare
	Float_t _chi_square;
	
	//! status
	ULong_t _status;
	
	//! centroid list
	centroid_list _centroid_list;
			
  ClassDef(TRpcClus_v1,1)
};


#endif 







