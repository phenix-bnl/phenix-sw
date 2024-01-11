#ifndef _TMUTHIT_V1H_
#define _TMUTHIT_V1H_

// CINT compatible headers//
#include<TDataType.h>
#include<PHException.h>
#include<TMutHit.hh>

class TMutHit_v1 :  public TMutHit
{  
public:

  TMutHit_v1();

  TMutHit_v1(const Key& key, 
	  UShort_t arm, 
	  UShort_t station, 
	  UShort_t octant, 
	  UShort_t half_octant, 
	  UShort_t gap, 
	  UShort_t cathode, 
	  UShort_t strip); 

  // Construct from base class pointer (ref)
  //
  TMutHit_v1(const TMutHit* base_ptr);
  TMutHit_v1(const TMutHit& base_ref);

  virtual ~TMutHit_v1(){;}

  Float_t   get_q() const { return _q; }

  Float_t   get_t() const { return _t; }

  Float_t   get_error_q() const { return _error_q; }                

  UShort_t  get_adc(const UShort_t index) const { 
    BOUNDS_CHECK(index,ADC_SIZE); return _adc[index]; 
  }

  UShort_t get_amu(const UShort_t index) const { 
    BOUNDS_CHECK(index,AMU_SIZE); return _amu[index]; 
  } 

  void  set_q(float q){ _q = q; }                   

  void  set_t(float t){ _t = t; }                   

  void  set_error_q(float error_q){ _error_q = error_q; }                  

  void  set_adc(UShort_t index, UShort_t adc){
    BOUNDS_CHECK(index,ADC_SIZE);_adc[index] = adc;
  }    

  void  set_amu(UShort_t index, UShort_t amu){
    BOUNDS_CHECK(index,AMU_SIZE);_amu[index] = amu;
  }      

  UShort_t  get_arm() const {return _arm;}

  UShort_t  get_station() const {return _station;}

  UShort_t  get_octant() const {return _octant;}

  UShort_t  get_half_octant() const {return _half_octant;}

  UShort_t  get_gap() const {return _gap;}

  UShort_t  get_cathode() const {return _cathode;}

  UShort_t  get_strip() const { return _strip;}                

  void set_arm(UShort_t arm){ _arm = arm; }

  void  set_station(UShort_t station){ _station = station; }               	

  void  set_octant(UShort_t octant){ _octant = octant; }               

  void  set_half_octant(UShort_t half_octant){ _half_octant = half_octant; }               

  void  set_gap(UShort_t gap){ _gap = gap; }               

  void  set_cathode(UShort_t cathode){ _cathode = cathode; }               

  void  set_strip(UShort_t strip){ _strip = strip; }               

  void print(std::ostream& os = std::cout) const; 

  private:

  void test_invariant() const;

  UShort_t _arm;
  UShort_t _station;
  UShort_t _octant;
  UShort_t _half_octant;
  UShort_t _gap;
  UShort_t _cathode;
  UShort_t _strip;
  Float_t _q;
  Float_t _error_q;
  Float_t _t;

  // c style array bounds
  enum { ADC_SIZE=4, AMU_SIZE=4}; 
  UShort_t _amu[ADC_SIZE]; 
  UShort_t _adc[AMU_SIZE]; 
  
  ClassDef(TMutHit_v1,1)
};

#endif
