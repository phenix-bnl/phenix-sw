
#ifndef __TMUTCLUS_H__
#define __TMUTCLUS_H__

// $Id: TMutClus.hh,v 1.24 2011/12/29 20:19:29 slash Exp $

/*!
\file    TMutClus.hh
\brief   Interface Object Class : TMutClus
\author  S.Kelly
\version $Revision: 1.24 $
\date    $Date: 2011/12/29 20:19:29 $
*/

// CINT compatible headers
#include<TDataType.h>
#include<PHKey.hh>
#include<MUTOO.h>
#include<TMutClusCentroid_v1.hh>
#include<vector>

#include <PHException.h>

/*! @ingroup interface */
//!  The Muon tracker cluster object
/*!  The Muon tracker cluster object.  This interface object represents groups of
contiguous hit strips in the MUTR detector.  The TMutHit object are associated using
the standard MUTOO object association interface.  In addition to the associated hits
cluster objects are composed of a list of TMutClusCentroid objects.  These objects
represent the centroid information in strip local (w-space) coordinates that is derived
after the cluster has been fit.  The chi-squared statistics associated with the cluster
is the only additional data member
*/

class TMutClus : public PHKey {

  public:

  //! typedef for reference to the current centroid version
  typedef TMutClusCentroid_v1 centroid_value_type;

  //! typedef for pointer to the current centroid version
  typedef TMutClusCentroid_v1* centroid_pointer;

  //! Enumeration for cluster status bits
  enum Status {

    PEAK_BOUND,

    //! set to true if cluster charge is below min_charge cut
    LOW_CHARGE,

    //! set to true if cluster charge is above max_charge cut
    HIGH_CHARGE,

    //! set to true if clusters was refitted to less coordinates at coordinate matching stage
    REFIT_LESS,

    //! set to true if clusters was refitted to more coordinates at coordinate matching stage
    REFIT_MORE,

    //! set to true if mathieson lookup table was used for this cluster
    USE_LOOKUP,

    //! set to true when there is a attenuated strip
    ATTENUATED_STRIP,

    //! set to true when there is a bad strip
    BAD_STRIP,

    //! cluster is on detector edge
    /*!
    note: this flag actually covers only a small fraction of the clusters that are on
    three edge of the detector, because it is based on the strip index, whereas depending on the
    orientation of the strips, a much larger number of strips can actually hit the edge of the detector.
    */

    EDGE_STRIP,

    //! set to true when there is saturated strip
    SATURATED_STRIP

  };

  //! @name Constructors/Destructors
  //@{

  //! Default constructor
  TMutClus( void ){;}

  //! Construct with key
  TMutClus(const Key& key) : PHKey(key) {;}

  //! Destructor
  virtual ~TMutClus( void ) {;}
  //@}

  //! @name Functional Interface
  //@{

  //! The merit statistic associated with cluster fit
  virtual double get_chi_square( void ) const
  { return 0;}

  //! number of associated strips
  virtual UShort_t  get_n_strip( void ) const;

  //! The merit statistic associated with cluster fit
  virtual void set_chi_square(double chi_square)
  {;}

  //! construct a new centroid adds to the list, return pointer
  virtual TMutClusCentroid* insert_new_centroid( UShort_t peak_strip  )
  {
    throw std::logic_error( DESCRIPTION( "invalid access to baseclass method" ) );
    return 0;
  }

  //! clear centroid list
  virtual void clear_centroid_list( void )
  {}

  //! number of centroids in this cluster
  virtual size_t get_n_centroid( void ) const
  { return 0; }

  //! read only access to centroid list
  virtual const TMutClusCentroid* get_centroid( size_t index ) const
  {
    throw std::logic_error( DESCRIPTION( "invalid call to baseclass method" ) );
    return 0;
  }

  //@}

  //! @name Locators
  //@{

  #ifndef __CINT__
  //! return class ID, mapped from class name
  virtual PHClassId::id_type get_class_id( void ) const
  { return (_class_id) ? _class_id : (_class_id = PHClassId::get( GetName() ) ); }
  #endif

  //! Arm [0,1]
  virtual UShort_t  get_arm( void ) const
  {return 0;}

  //! Station [0,2]
  virtual UShort_t  get_station( void ) const
  {return 0;}

  //! Octant [0,7]
  virtual UShort_t  get_octant( void ) const
  {return 0;}

  //! Half octant [0,1]
  virtual  UShort_t  get_half_octant( void ) const
  {return 0;}

  //! Gap [0,2]
  virtual UShort_t  get_gap( void ) const
  {return 0;}

  //! Cathode [0,1]
  virtual UShort_t  get_cathode( void ) const
  {return 0;}

  //! Index [0,1023]
  virtual UShort_t  get_index( void ) const
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

  //! Half octant [0,1]
  virtual void set_half_octant(UShort_t half_octant)
  {}

  //! Gap [0,2]
  virtual void set_gap(UShort_t gap)
  {}

  //! Cathode [0,1]
  virtual void set_cathode(UShort_t cathode)
  {}

  //! Index [0,1023]
  virtual void set_index(UShort_t index)
  {}

  #ifndef __CINT__
  //! Returns a boost tuple of (arm, station, octant, half_octant, gap, plane)
  virtual MUTOO::cathode_locator get_location( void ) const
  { return boost::make_tuple( get_arm(), get_station(), get_octant(), get_half_octant(), get_gap(), get_cathode() ); }
  #endif

  //@}

  //! @name Cluster Status
  //@{

  //! Set the peak_bound bit
  virtual void set_peak_bound( void )
  { set_status( get_status() | (1<<PEAK_BOUND) ); }

  //! Set the peak_bound bit
  virtual bool get_peak_bound( void ) const
  { return get_status() & (1<<PEAK_BOUND); }

  //! Set the low_charge bit
  virtual void set_low_charge( void )
  { set_status( get_status() | (1<<LOW_CHARGE) ); }

  //! Set the low_charge bit
  virtual bool get_low_charge( void ) const
  { return get_status() & (1<<LOW_CHARGE); }


  //! Set the high_charge bit
  virtual void set_high_charge( void )
  { set_status( get_status() | (1<<HIGH_CHARGE) ); }

  //! Set the high_charge bit
  virtual bool get_high_charge( void ) const
  { return get_status() & (1<<HIGH_CHARGE); }

  //! Set the refit_less bit
  virtual void set_refit_less( void )
  { set_status( get_status() | (1<<REFIT_LESS) ); }

  //! Get the refit_less bit
  virtual bool get_refit_less( void ) const
  { return get_status() & (1<<REFIT_LESS); }

  //! Set the refit_more bit
  virtual void set_refit_more( void )
  { set_status( get_status() | (1<<REFIT_MORE) ); }

  //! Get the refit_more bit
  virtual bool get_refit_more( void ) const
  { return get_status() & (1<<REFIT_MORE); }

  //! Set the use_lookup bit
  virtual void set_use_lookup( void )
  { set_status( get_status() | (1<<USE_LOOKUP) ); }

  //! Get the use_lookup bit
  virtual bool get_use_lookup( void ) const
  { return get_status() & (1<<USE_LOOKUP); }

  //! Set the attenuated_strip bit
  virtual void set_attenuated_strip( void )
  { set_status( get_status() | (1<<ATTENUATED_STRIP) ); }

  //! Get the attenuated_strip bit
  virtual bool get_attenuated_strip( void ) const
  { return get_status() & (1<<ATTENUATED_STRIP); }

  //! Set the bad_strip bit
  virtual void set_bad_strip( void )
  { set_status( get_status() | (1<<BAD_STRIP) ); }

  //! Get the bad_strip bit
  virtual bool get_bad_strip( void ) const
  { return get_status() & (1<<BAD_STRIP); }

  //! Set the edge_strip bit
  virtual void set_edge_strip( void )
  { set_status( get_status() | (1<<EDGE_STRIP) ); }

  //! get edge_hit bit
  virtual bool get_edge_strip( void ) const
  { return get_status() & (1<<EDGE_STRIP); }

  //! saturated strip bit
  virtual void set_saturated_strip( void )
  { set_status( get_status() | (1<<SATURATED_STRIP) ); }

  //! saturated strip bit
  virtual bool get_saturated_strip( void ) const
  { return get_status() & (1<<SATURATED_STRIP); }

  //! Get the status word
  virtual ULong_t get_status( void ) const
  { return 0;}
  //@}

  //! @name Dumpers
  //@{
  //! Print cluster contents
  virtual void print(std::ostream& os = std::cout) const
  {;}
  //@}

  protected:

  //! status
  virtual void set_status( ULong_t )
  {}

  //! Clear the status word
  virtual void clear_status( void )
  {}

  private:

#ifndef __CINT__

  //! static class ID
  static PHClassId::id_type _class_id;

#endif

  ClassDef(TMutClus,1)
};

#endif
