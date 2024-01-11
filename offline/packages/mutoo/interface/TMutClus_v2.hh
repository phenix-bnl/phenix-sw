// Interface Object Class : TMutClus
// Author: S.Kelly
// Date: 1/2/21
// Description: Class for Muon Tracker Clusters.

#ifndef __TMUTCLUS_V2H__
#define __TMUTCLUS_V2H__

// CINT compatible headers
//
#include<TDataType.h>
#include<PHKey.hh>
#include<MUTOO.h>
#include<TMutClus.hh>

// CINT non-compatible headers
//
#ifndef __CINT__
#include<PHConstKeyIterator.h>
#endif

class TMutClus_v2 : public TMutClus
{

 public:

  TMutClus_v2();

  TMutClus_v2(const Key& key,
	      UShort_t arm,
	      UShort_t station,
	      UShort_t octant,
	      UShort_t half_octant,
	      UShort_t gap,
	      UShort_t cathode,
	      UShort_t index);

  TMutClus_v2(const TMutClus& base_ref);
  TMutClus_v2(const TMutClus* base_ptr);

  virtual ~TMutClus_v2()
  {;}

  double   get_chi_square() const
  { return _chi_square;}

  void set_chi_square(double chi_square)
  { _chi_square = chi_square; }

  /*! construct a new centroid adds to the list, return pointer */
  virtual TMutClusCentroid* insert_new_centroid( UShort_t peak_strip  )
  {
    _centroid_list.push_back( TMutClusCentroid( peak_strip ) );
    return &_centroid_list.back();
  }

  void clear_centroid_list()
  { _centroid_list.clear(); }

  size_t get_n_centroid() const
  { return _centroid_list.size(); }

  virtual const TMutClusCentroid* get_centroid( size_t index ) const
  {
    BOUNDS_CHECK( index, _centroid_list.size() );
    return &_centroid_list[index];
  }

  UShort_t  get_arm() const
  {return _arm;}

  UShort_t  get_station() const
  {return _station;}

  UShort_t  get_octant() const
  {return _octant;}

  UShort_t  get_half_octant() const
  {return _half_octant;}

  UShort_t  get_gap() const
  {return _gap;}

  UShort_t  get_cathode() const
  {return _cathode;}

  UShort_t  get_index() const
  {return _index;}

  void set_arm(UShort_t arm)
  { _arm = arm; }

  void set_station(UShort_t station)
  { _station = station; }

  void set_octant(UShort_t octant)
  { _octant = octant; }

  void set_half_octant(UShort_t half_octant)
  { _half_octant = half_octant; }

  void set_gap(UShort_t gap)
  { _gap = gap; }

  void set_cathode(UShort_t cathode)
  { _cathode = cathode; }

  void set_index(UShort_t index)
  {_index = index;}

  ULong_t get_status() const
  {return _status;}

  void print(std::ostream& os = std::cout) const;

  protected:

  void set_status( ULong_t status )
  {_status = status;}

  void clear_status()
  { _status=0;}

  private:

  //! cluster arm
  UShort_t _arm;

  //! cluster station
  UShort_t _station;

  //! cluster octant
  UShort_t _octant;

  //! cluster half_octant
  UShort_t _half_octant;

  //! cluster gap
  UShort_t _gap;

  //! cluster cathode
  UShort_t _cathode;

  //! cluster index in cathode
  UShort_t _index;

  //! list of associated centroids
  std::vector<TMutClusCentroid> _centroid_list;

  //! fit chisquare
  double  _chi_square;

  //! 32 bits status word
  ULong_t _status;

  ClassDef(TMutClus_v2,1)
};

#endif



