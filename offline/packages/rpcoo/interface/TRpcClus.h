// $Id: TRpcClus.h,v 1.2 2008/08/28 00:50:17 kempel Exp $

#ifndef __TRpcClus_h__
#define __TRpcClus_h__

/*!
   \file    TRpcClus.h
   \brief   Rpc cluster interface object
   \author  H. Pereira Da Costa
   \version $Revision: 1.2 $
   \date    $Date: 2008/08/28 00:50:17 $
*/

// CINT compatible headers
#include <TDataType.h>
#include <PHKey.hh>
#include <PHException.h>
#include <TRpcClusCentroid_v1.h>

#include<vector>

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

class TRpcClus : public PHKey {
  
public:
  
  //! typedef for reference to the current centroid version
  typedef TRpcClusCentroid_v1 centroid_value_type;

  //! typedef for pointer to the current centroid version
  typedef TRpcClusCentroid_v1* centroid_pointer;
  
	//! list of pointers to centroids
	typedef std::vector< TRpcClusCentroid* > centroid_list;
	
	//! iterator over centroids
	typedef centroid_list::iterator centroid_iterator;
	  
  //! @name Constructors/Destructors
  //@{    
  
  //! Default constructor 
  TRpcClus()
	{}
  
  //! Construct with key 
  TRpcClus(const Key& key) : PHKey(key) 
	{}
  
  //! Destructor 
  virtual ~TRpcClus() {;}
  //@}
  
  //! @name Functional Interface
  //@{    

  //! The merit statistic associated with cluster fit 
  virtual Float_t get_chi_square() const 
  { return 0;}

  //! number of hits associated to the cluster
  virtual UShort_t get_n_hits() const;
    
  //! The merit statistic associated with cluster fit 
  virtual void set_chi_square(Float_t chi_square) 
  {;}  

  //! construct a new centroid adds to the list, return pointer 
  virtual TRpcClusCentroid* insert_new_centroid( UShort_t peak_strip  )
  {
    throw std::logic_error( DESCRIPTION( "invalid access to baseclass method" ) );
    return 0;
  }  

  //! clear centroid list 
  virtual void clear_centroid_list()
  {}			     

  //! number of centroids in this cluster 
  virtual size_t get_n_centroid() const 
  { return 0; }

  //! read only access to centroid list 
  virtual centroid_list get_centroid_list( void ) const 
  {
    return centroid_list();
  }
  
  //@}

  //! @name Locators
  //@{  
  //! Arm [0,1] 
  virtual UShort_t  get_arm() const 
  {return 0;}
  
  //! Station [0,2] 
  virtual UShort_t  get_station() const 
  {return 0;}
  	 
  //! Octant [0,7] 
  virtual UShort_t get_octant() const 
  {return 0;}
  
  //! Half Octant [0,1] 
  virtual UShort_t get_half_octant() const 
  {return 0;}
  
  //! Radial Segment (see RPCGEOM.h)
  virtual UShort_t get_rseg() const 
  {return 0;}

  //! Index
  virtual UShort_t  get_index() const 
  {return 0;}  
  
  //! Arm [0,1] 
  virtual void set_arm(UShort_t arm)
  {;}
  
  //! Station [0,2] 
  virtual void set_station(UShort_t station)
  {}               	
  
  //! Octant [0,7] 
  virtual void set_octant(UShort_t octant) 
  {}
	
  //! Half Octant [0,1]
  virtual void set_half_octant(UShort_t half_octant) 
  {}
	
  //! Radial Segment (see RPCGEOM.h)
  virtual void set_rseg(UShort_t rseg) 
  {}

  //! Index
  virtual void set_index(UShort_t index) 
  {}  

  //@}

  //! @name Cluster Status
  //@{  

  //! Get the status word 
  virtual ULong_t get_status() const 
  { return 0;}
  
  //! Clear the status word 
  virtual void clear_status() 
  {}
  //@}
  
  //! @name Dumpers
  //@{    
  //! Print cluster contents 
  virtual void print(std::ostream& os = std::cout) const 
  {;}
  //@}

  ClassDef(TRpcClus,1)
};


#endif 







