// $Id: TMutCoord_v1.hh,v 1.12 2011/12/29 20:19:29 slash Exp $

// Interface Object Class : TMutCoord
// Author: S.Kelly 
// Date: 4/12/02
// Description: Class for Muon Tracker Coordinate.

#ifndef __TMUTCOORD_V1H__
#define __TMUTCOORD_V1H__

#include<TMutCoord.hh>
#include<PHKey.hh>
#include<PHLine.h>
#include<MUTOO.h>
#include<map>


class TMutCoord_v1 : public TMutCoord
{
  
 public:

  TMutCoord_v1();

  TMutCoord_v1(const Key& key,
	       UShort_t arm,
	       UShort_t station, 
	       UShort_t octant, 
	       UShort_t half_octant, 
	       UShort_t gap,
	       UShort_t cathode,
	       UShort_t index);

  TMutCoord_v1(const TMutCoord& base_ref);

  TMutCoord_v1(const TMutCoord* base_ptr);

  virtual ~TMutCoord_v1(){;}

  PHLine get_coord() const;
  PHPoint get_coord_end() const;
  PHPoint get_coord_midpoint() const;
  PHPoint get_coord_begin() const;
  virtual double get_mean_z() const;

  Float_t get_q_peak() const {return _q_peak;}
  Float_t get_q_tot() const {return _q_tot;}
  UShort_t get_peak_strip() const {return _peak_strip;}
  Float_t get_w() const {return _w;}
  Float_t get_cos_theta_wire() const {return _cos_theta_wire;}
  virtual Float_t get_error() const {return _error;}
  void set_coord(const PHLine& coord);
  void set_q_peak(Float_t q_peak) { _q_peak = q_peak;}
  void set_q_tot(Float_t q_tot) { _q_tot = q_tot;}
  void set_peak_strip(UShort_t peak_strip) { _peak_strip = peak_strip;}
  void set_w(Float_t w) { _w = w;}
  void set_cos_theta_wire(Float_t cos_theta) { _cos_theta_wire = cos_theta;}
  void set_error(double error){_error = error;}

  UShort_t  get_arm() const {return _arm;}
  UShort_t  get_station() const {return _station;}
  UShort_t  get_octant() const {return _octant;}
  UShort_t  get_half_octant() const {return _half_octant;}
  UShort_t  get_gap() const {return _gap;}
  UShort_t  get_cathode() const {return _cathode;}
  UShort_t  get_index() const {return _index;}  
  void set_arm(UShort_t arm){ _arm = arm; }
  void set_station(UShort_t station){ _station = station; }               	
  void set_octant(UShort_t octant){ _octant = octant; }               
  void set_half_octant(UShort_t half_octant){ _half_octant = half_octant; }               
  void set_gap(UShort_t gap){ _gap = gap; }               
  void set_cathode(UShort_t cathode){ _cathode = cathode; }               
  void set_index(UShort_t index) {_index = index;}  

  void set_peak_bound() {_status |= 1<<TMutCoord::PEAK_BOUND;}  
  bool get_peak_bound() const { return (_status & 1<<TMutCoord::PEAK_BOUND) != 0; }
  void set_low_charge() {_status |= 1<<TMutCoord::LOW_CHARGE;}  
  bool get_low_charge() const { return (_status & 1<<TMutCoord::LOW_CHARGE) != 0; }
  void set_high_charge() {_status |= 1<<TMutCoord::HIGH_CHARGE;}  
  bool get_high_charge() const { return (_status & 1<<TMutCoord::HIGH_CHARGE) != 0; }
  void set_status(ULong_t status) { _status = status; }
  ULong_t get_status() const {return _status;}  
  void clear_status() { _status=0;}
  void print(std::ostream& os = std::cout) const; 

 private:	

  UShort_t _arm;
  UShort_t _station;
  UShort_t _octant;
  UShort_t _half_octant;
  UShort_t _gap;
  UShort_t _cathode;
  UShort_t _index;
  double _q_peak;
  double _q_tot;
  UShort_t _peak_strip;
  double _cos_theta_wire;
  double _error;		
  double _w;
  ULong_t _status;

  enum {POINT_SIZE=3};
  double _point_1[POINT_SIZE]; 
  double _point_2[POINT_SIZE];

  ClassDef(TMutCoord_v1,1)
};

		
#endif /* __TMUTCOORD_V1H__*/










