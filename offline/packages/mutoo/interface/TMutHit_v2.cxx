#include<algorithm>
#include"TMutHit_v2.hh"

ClassImp(TMutHit_v2)

using namespace std;

//________________________________________________________
/*! default constructor */
TMutHit_v2::TMutHit_v2() :
  _arm(0),
  _station(0),
  _octant(0),
  _half_octant(0),
  _gap(0),
  _cathode(0),
  _strip(0),
  _q(0),
  _error_q(0),
  _t(0),
  _status(0)
{
  std::fill(_amu, _amu+AMU_SIZE,0);
  std::fill(_adc, _adc+ADC_SIZE,0);
}

//________________________________________________________
TMutHit_v2::TMutHit_v2(const Key& key,
  UShort_t arm,
  UShort_t station,
  UShort_t octant,
  UShort_t half_octant,
  UShort_t gap,
  UShort_t cathode,
  UShort_t strip) :
  TMutHit(key),
  _arm(arm),
  _station(station),
  _octant(octant),
  _half_octant(half_octant),
  _gap(gap),
  _cathode(cathode),
  _strip(strip),
  _q(0),
  _error_q(0),
  _t(0),
  _status(0)
{
  std::fill(_amu, _amu+AMU_SIZE,0);
  std::fill(_adc, _adc+ADC_SIZE,0);
}

//________________________________________________________
TMutHit_v2::TMutHit_v2(const TMutHit* base_ptr) :
  TMutHit(*base_ptr),
  _arm(base_ptr->get_arm()),
  _station(base_ptr->get_station()),
  _octant(base_ptr->get_octant()),
  _half_octant(base_ptr->get_half_octant()),
  _gap(base_ptr->get_gap()),
  _cathode(base_ptr->get_cathode()),
  _strip(base_ptr->get_strip()),
  _q(base_ptr->get_q()),
  _error_q(base_ptr->get_error_q()),
  _t(base_ptr->get_t()),
  _status( base_ptr->get_status() )
{
  for(int i=0;i<AMU_SIZE;++i) _amu[i]=base_ptr->get_amu(i);
  for(int i=0;i<ADC_SIZE;++i) _adc[i]=base_ptr->get_adc(i);
}

//________________________________________________________
TMutHit_v2::TMutHit_v2(const TMutHit& base_ref) :
  TMutHit(base_ref),
  _arm(base_ref.get_arm()),
  _station(base_ref.get_station()),
  _octant(base_ref.get_octant()),
  _half_octant(base_ref.get_half_octant()),
  _gap(base_ref.get_gap()),
  _cathode(base_ref.get_cathode()),
  _strip(base_ref.get_strip()),
  _q(base_ref.get_q()),
  _error_q(base_ref.get_error_q()),
  _t(base_ref.get_t()),
  _status( base_ref.get_status() )
{
  for(int i=0;i<AMU_SIZE;++i) _amu[i]=base_ref.get_amu(i);
  for(int i=0;i<ADC_SIZE;++i) _adc[i]=base_ref.get_adc(i);
}

//________________________________________________________
TMutHit_v2::~TMutHit_v2()
{
  // std::cout << "TMutHit_v2::~TMutHit_v2" << std::endl;
}

//________________________________________________________
void TMutHit_v2::print(std::ostream& os) const {
  MUTOO::PRINT(os,GetName());
  os
    << " key: " << get_key().get_obj_key()
    << " arm: " << _arm
    << " station: " << _station
    << " octant: " << _octant
    << " half octant: " << _half_octant
    << " gap: " << _gap
    << " cathode: " << _cathode
    << " strip: " << _strip << std::endl;

  // dump the status
  os << " status: ";
  if(get_is_edge()) os << "EDGE ";
  if(get_is_saturated()) os << "SATURATED ";
  if(get_is_attenuated()) os << "ATTENUATED ";
  os << endl;

  os << " q: " << _q << " q_error: " << _error_q << "	t: " << _t << std::endl;

  // print ADC values
  os << " adc = {";
  for(int i=0;i<ADC_SIZE;++i){
    BOUNDS_CHECK(i,ADC_SIZE);
    if( i ) os << ",";
    os << _adc[i];
  }

  // print amu values
  os << "}" << std::endl;
  os << " amu = {";
  for(int i=0;i<AMU_SIZE;++i){
    BOUNDS_CHECK(i,AMU_SIZE);
    if( i ) os << ",";
    os << _amu[i];
  }
  os << "}" << std::endl;

  MUTOO::PRINT(os,"**");
}

//________________________________________________________
inline void TMutHit_v2::test_invariant() const
{}
