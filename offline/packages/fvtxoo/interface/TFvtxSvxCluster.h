#ifndef __TFvtxSvxCluster_h__
#define __TFvtxSvxCluster_h__

// $Id: TFvtxSvxCluster.h,v 1.4 2011/12/01 04:16:20 slash Exp $

/*!
  \file TFvtxSvxCluster.h
  \brief empty interface to store SvxClusters into a map and enable standard PHKey association
  \author Dave Winter
  \version $Revision: 1.4 $
  \date $Date: 2011/12/01 04:16:20 $
*/

#include <PHKey.hh> 
#include <PHPoint.h>
#include <PHLine.h>

// forward declaration of embedded class
class SvxCluster;


/*! @ingroup interface */
//! Empty interface to store SvxSvxClusters into a map and enable standard PHKey association
class TFvtxSvxCluster : public PHKey
{
  
  public:
  
  //! @name Constructors/Destructor
  //@{		
  //! Default
  TFvtxSvxCluster()
  {}

  //! Construct with key
  TFvtxSvxCluster(const Key& key): 
    PHKey(key)
  {}

  //! Destructor
  virtual ~TFvtxSvxCluster() 
  {}

  //! index
  virtual unsigned short get_index() const 
  {return 0;}  
  
  //! index 
  virtual void set_index( const unsigned short& index)
  {}
  
  //! set pisa hit
  virtual void set_cluster( const SvxCluster* hit )
  {}
  
  //! retrieve cluster
  virtual const SvxCluster* get_cluster( void ) const
  { return 0; }

  //! Returns a point defining the small-phi edge of the cluster in the x-y plane
  virtual PHPoint get_begin() const { return PHPoint(); }

  //! Returns a point defining the large-phi edge of the cluster in the x-y plane
  virtual PHPoint get_end() const { return PHPoint(); }

//   //! Sets a point defining the small-phi edge of the cluster in the x-y plane
//   virtual void set_begin(const PHPoint& p) {}

//   //! Sets a point defining the large-phi edge of the cluster in the x-y plane
//   virtual void set_end(const PHPoint& p) {}

  //! Returns a line representing the pixel cross-section in the x-y plane
  virtual PHLine get_line() const { return PHLine(); }

  //! Sets a line representing the pixel cross-section in the x-y plane
  virtual void set_line(const PHLine& l) {}

  virtual void set_r_error(const double val) {}
  virtual void set_phi_error(const double val) {}
  virtual void set_z_error(const double val) {}

  virtual double get_r_error() const { return 0; }
  virtual double get_phi_error() const { return 0; }
  virtual double get_z_error() const { return 0; }

  //! @name Dumpers
  //@{		
  //! Set Charge associated with this strip
  virtual void print(std::ostream& os = std::cout) const 
  {}
  //!@}
  
  ClassDef(TFvtxSvxCluster,1)
  
};
  
#endif
