// $Id: TMutClus_v1.cxx,v 1.8 2011/12/29 20:19:29 slash Exp $

/*!
   \file    TMutClus_v1.cxx
   \brief   Interface Object Class : TMutClus
   \author  H.Pereira
   \version $Revision: 1.8 $
   \date    $Date: 2011/12/29 20:19:29 $
*/

#include<cmath>
#include<MUTOO.h>
#include<TMutClus_v1.hh>
#include<TMutHitMap.h>
#include<PHConstKeyIterator.h>
#include<TMutGeo.h>

ClassImp(TMutClus_v1)
using namespace std;
  
//________________________________________________________
TMutClus_v1::TMutClus_v1() : 
  _arm(0),
  _station(0),
  _octant(0),
  _half_octant(0),
  _gap(0),
  _cathode(0),
  _chi_square(0)
{;}

//________________________________________________________
TMutClus_v1::TMutClus_v1(const Key& key,       
			 UShort_t arm,         
			 UShort_t station,     
			 UShort_t octant,      
			 UShort_t half_octant, 
			 UShort_t gap,         
			 UShort_t cathode,     
			 UShort_t index) :     
  
  TMutClus(key), 
  _arm(arm), 
  _station(station),
  _octant(octant),
  _half_octant(half_octant),
  _gap(gap),
  _cathode(cathode),
  _index(index),
  _chi_square(0)
  {;}

//________________________________________________________
TMutClus_v1::TMutClus_v1(const TMutClus* base_ptr) :       
  TMutClus(*base_ptr), 
  _arm(base_ptr->get_arm()), 
  _station(base_ptr->get_station()),
  _octant(base_ptr->get_octant()),
  _half_octant(base_ptr->get_half_octant()),
  _gap(base_ptr->get_gap()),
  _cathode(base_ptr->get_cathode()),
  _index(base_ptr->get_index()),
  _chi_square(base_ptr->get_chi_square())
{
  
  // copy centroid list
  for( size_t index = 0; index < base_ptr->get_n_centroid(); index++ ) 
  _centroid_list.push_back( TMutClusCentroid( *base_ptr->get_centroid( index ) ) );
  
}

//________________________________________________________
TMutClus_v1::TMutClus_v1(const TMutClus& base_ref) :       
  TMutClus(base_ref), 
  _arm(base_ref.get_arm()), 
  _station(base_ref.get_station()),
  _octant(base_ref.get_octant()),
  _half_octant(base_ref.get_half_octant()),
  _gap(base_ref.get_gap()),
  _cathode(base_ref.get_cathode()),
  _index(base_ref.get_index()),
  _chi_square(base_ref.get_chi_square())
{
  
  // copy centroid list
  for( size_t index = 0; index < base_ref.get_n_centroid(); index++ ) 
  _centroid_list.push_back( TMutClusCentroid( *base_ref.get_centroid( index ) ) );

}

//________________________________________________________
void TMutClus_v1::print(ostream& os) const 
{
  using namespace std;
  MUTOO::PRINT(os,GetName());
  os << " arm: " << _arm
     << "  station: " << _station
     << "  octant: " << _octant
     << "  half octant: " << _half_octant
     << "  gap: " << _gap
     << "  cathode: " << _cathode
     << "  index: " << _index << endl;

  // dump the cluster width
  os << " cluster width: " << get_n_strip() << endl; 

  // dump associated strips
  os << " strips associated with this cluster: "; 
  TMutHitMap::const_key_iterator hit_iter = get_associated<TMutHit>();
  while(TMutHitMap::const_pointer hit_ptr = hit_iter.next())
  os << hit_ptr->get()->get_strip() << ":" << hit_ptr->get()->get_q() << "  ";
  os << endl;
  
//    double stereo_angle = TMutGeo::get_angle_cathode_anode(get_location());
//    int degrees = static_cast<int>(floor(MUTOO::RAD_TO_DEG*stereo_angle + 0.5));
//    os << " stereo angle: " << degrees << endl;

  // dump centroid fit data
  for( 
    vector<TMutClusCentroid>::const_iterator iter = _centroid_list.begin();
    iter!=_centroid_list.end();
    ++iter) 
  {
    os << " cluster width: " << get_n_strip();
    iter->print(os);
  }
  
  MUTOO::PRINT(os,"**");
}






