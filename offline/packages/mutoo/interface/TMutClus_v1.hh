// $Id: TMutClus_v1.hh,v 1.7 2011/12/29 20:19:29 slash Exp $

/*!
   \file    TMutClus_v1.hh
   \brief   Interface Object Class : TMutClus
   \author  S.Kelly 
   \version $Revision: 1.7 $
   \date    $Date: 2011/12/29 20:19:29 $
*/

#ifndef __TMUTCLUS_V1H__
#define __TMUTCLUS_V1H__

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

class TMutClus_v1 : public TMutClus {
  
 public:

  TMutClus_v1();

  TMutClus_v1(const Key& key, 
	      UShort_t arm, 
	      UShort_t station, 
	      UShort_t octant, 
	      UShort_t half_octant, 
	      UShort_t gap, 
	      UShort_t cathode,
	      UShort_t index); 
	   
  TMutClus_v1(const TMutClus& base_ref);
  TMutClus_v1(const TMutClus* base_ptr);

  virtual ~TMutClus_v1(){;}

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

  void print(std::ostream& os = std::cout) const;

 private:	

  UShort_t _arm;
  UShort_t _station;
  UShort_t _octant;
  UShort_t _half_octant;
  UShort_t _gap;
  UShort_t _cathode;  
  UShort_t _index;
  
  // where to define the origin of w -space (perp to strips)
  // a) first strip in octant
  // b) first strip in cluster
  // c) peak strip in cluster

  std::vector<TMutClusCentroid> _centroid_list;
  double  _chi_square;			 // fit error		    
  
  ClassDef(TMutClus_v1,1)
};

#endif /* __TMUTCLUS_V1H__*/



