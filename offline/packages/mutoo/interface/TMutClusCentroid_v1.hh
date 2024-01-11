// Class : TMutClusCentroid
// Author: S.Kelly 
// Date: 4/02
// Description: Cluster centroid object

#ifndef __TMUTCLUSCENTROID_V1_H__
#define __TMUTCLUSCENTROID_V1_H__

/*! @ingroup classes */

//!  The Muon tracker cluster centroid 
/*!  The Muon tracker cluster centroid. This object provides the
interface to centroids associated with a TMutClus. Each track that
contributes charge to the strips associated with this cluster will
after cluster fitting have an associated centroid.  The centroid
coordinate is in the so-called w space which is defined relative
to the end point strip normal in the direction normal to the strip
direction.*/

#include<TDataType.h>
#include<TObject.h>
#include<iostream>
#include<TMutClusCentroid.hh>

class TMutClusCentroid_v1 : public TMutClusCentroid
{

public:

  //! @name Constructors/Destructors
  //@{    
  /*! Default constructor */
  TMutClusCentroid_v1():
    _q_tot_error( 0 )
  {;}

  /*! Construct with peak strip */
  TMutClusCentroid_v1(UShort_t peak_strip):
    TMutClusCentroid( peak_strip ),
    _q_tot_error( 0 )
  {;}

  //! destructor
  virtual ~TMutClusCentroid_v1()
  {;}

  //! constructor from base class reference
  TMutClusCentroid_v1(const TMutClusCentroid& base_ref);

  //! constructor from base class pointer
  TMutClusCentroid_v1(const TMutClusCentroid* base_ptr);

  //@}
  
  virtual double get_q_tot_error() const
  { return _q_tot_error; }
  
  virtual void set_q_tot_error( double value )
  { _q_tot_error = value; }
  
  virtual void print(std::ostream& os = std::cout) const 
  {
    os 
      << " TMutClusCentroid_v1 - "
      << " peak strip: " << get_peak_strip()
      << " w: " << get_w() << " w_error: " << get_w_error()
      << " q_peak: " << get_q_peak() 
      << " q_tot: " << get_q_tot()  << " q_tot_error: " << get_q_tot_error()
      << std::endl;
  }

private:
  
  //! error total charge (from fit)
  double _q_tot_error;
  
  ClassDef(TMutClusCentroid_v1,1)

};

#endif
