#ifndef __TFvtxSvxCluster_v1_h__
#define __TFvtxSvxCluster_v1_h__

// $Id: TFvtxSvxCluster_v1.h,v 1.6 2011/12/01 04:16:21 slash Exp $

/*!
   \file TFvtxSvxCluster_v1.h
   \brief Empty interface to store SvxSvxClusters into a map and enable standard PHKey association
   \author Dave Winter
   \version $Revision: 1.6 $
   \date $Date: 2011/12/01 04:16:21 $
*/

#include <TFvtxSvxCluster.h>
#include <SvxClusterv1.h>

/*! @ingroup interface */
//! Empty interface to store SvxSvxClusters into a map and enable standard PHKey association
class TFvtxSvxCluster_v1 : public TFvtxSvxCluster 
{
  
public:
  
  //! shortcut to SvxSngSvxCluster implementation
  typedef SvxClusterv1 value_type;
  
  //! @name Constructors/Destructor
  //@{		
  //! Default
  TFvtxSvxCluster_v1();

  //! Construct with key
  TFvtxSvxCluster_v1(const Key& key, const unsigned short& index);
 
  //! Construct from base class
  TFvtxSvxCluster_v1(const TFvtxSvxCluster& base_ref);
  
  //! Construct from base class
  TFvtxSvxCluster_v1(const TFvtxSvxCluster* base_pointer);

  //! Default
  virtual ~TFvtxSvxCluster_v1() {}
  
  //!@}

  //! index
  virtual unsigned short  get_index() const { return _index; }
  
  //! index 
  virtual void set_index(const unsigned short& index) { _index = index; }

  //! @name captured pisa hit
  //@{
  
  //! set cluster. can't be const, because the underlying
  //  ctor (value_type) doesn't support it. Also, since the body
  //  of SvxClusterv1::SvxClusterv1(SvxClusterv1*) is empty, we 
  //  can't use that ctor.
  virtual void set_cluster(const SvxCluster* hit)
  {
    _cluster = value_type( const_cast<SvxCluster*>(hit) ); 
    _rerr = 0.0;
    _phierr = 0.0;
    _zerr = 0.0;
  }
  
  //! retrieve pisa hit
  virtual const SvxCluster* get_cluster( void ) const { return &_cluster; }
  
  //! Returns a point defining the small-phi edge of the cluster in the x-y plane
  virtual PHPoint get_begin() const { return PHPoint(_point0[0],_point0[1],_point0[2]); }

  //! Returns a point defining the large-phi edge of the cluster in the x-y plane
  virtual PHPoint get_end() const { return PHPoint(_point1[0],_point1[1],_point1[2]); }

  //! Returns a line representing the pixel cross-section in the x-y plane
  virtual PHLine get_line() const { return PHLine(get_end(),get_begin()); }

  //! Sets a line representing the pixel cross-section in the x-y plane
  virtual void set_line(const PHLine& l);

  virtual void set_r_error(const double val) { _rerr = val; }
  virtual void set_phi_error(const double val) { _phierr = val; }
  virtual void set_z_error(const double val) { _zerr = val; }

  virtual double get_r_error() const { return _rerr; }
  virtual double get_phi_error() const { return _phierr; }
  virtual double get_z_error() const { return _zerr; }

  //@}
 
  //! @name Dumpers
  //@{		
  //! Set Charge associated with this strip
  virtual void print(std::ostream& os = std::cout) const;
  //!@}
  
private:
  // Some numbers to base the error calculations on
  static double PIXEL_ZLENGTH;   // Pixel length in Z direction (cm)
  static double PIXEL_PHILENGTH; // Pixel width in Phi direction (cm)
  static double PIXEL_RLENGTH;   // Pixel thickness in R direction (cm)

  
  //! hit index
  unsigned short _index;
  
  //! captured cluster
  value_type _cluster;
  
  double _rerr; // error in r direction (cm)
  double _phierr; // error in phi direction (cm)
  double _zerr; // error in z direction (cm)

  double _point0[3]; // x,y,z of point 0
  double _point1[3]; // x,y,z of point 1

  ClassDef(TFvtxSvxCluster_v1,1) 
};

#endif
