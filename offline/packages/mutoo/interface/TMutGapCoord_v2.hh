// Interface Object Class : TMutGapCoord
// Author: S.Kelly 
// Date: 1/2/21
// Description: Class for Muon Tracker GapCoord.

#ifndef __TMUTGAPCOORD_V2H__
#define __TMUTGAPCOORD_V2H__

#include<PHKey.hh>
#include<PHPoint.h>
#include<MUTOO.h>
#include<PHVector.h>
#include<TMutGapCoord.hh>

/*! @ingroup interface */

//!  The Muon tracker gap coord object 
/*!  The Muon tracker gap coord object */


class TMutGapCoord_v2 : public TMutGapCoord
{
  
 public:

  TMutGapCoord_v2();

  TMutGapCoord_v2(const Key& key,
	       UShort_t arm,
	       UShort_t station, 
	       UShort_t octant, 
	       UShort_t half_octant, 
	       UShort_t gap,
	       UShort_t index);

  TMutGapCoord_v2(const TMutGapCoord*);
  TMutGapCoord_v2(const TMutGapCoord&);

  virtual ~TMutGapCoord_v2(){;}

  
  void set_coord(const PHPoint& coord){_x = coord.getX();
                                       _y = coord.getY();
				       _z = coord.getZ();}  

  PHPoint get_coord() const { return PHPoint(_x,_y,_z); }


  UShort_t get_anode() const { return _anode; }  

  PHVector get_anode_direction() const;

  void set_anode(UShort_t anode) { _anode = anode;}

  double get_anode_dca() const { return _anode_dca; }

  void set_anode_dca(double anode_dca) { _anode_dca = anode_dca;}  

  double get_charge_corr() const { return _charge_corr; }

  void set_charge_corr(double charge_corr) { _charge_corr = charge_corr;}  

  UShort_t  get_arm() const {return _arm;}

  UShort_t  get_station() const {return _station;}

  UShort_t  get_octant() const {return _octant;}

  UShort_t  get_half_octant() const {return _half_octant;}

  UShort_t  get_gap() const {return _gap;}

  UShort_t  get_index() const {return _index;}  

  void set_arm(UShort_t arm){ _arm = arm; }

  void set_station(UShort_t station){ _station = station; }               	

  void set_octant(UShort_t octant){ _octant = octant; }               

  void set_half_octant(UShort_t half_octant){ _half_octant = half_octant; }               

  void set_gap(UShort_t gap){ _gap = gap; }               

  void set_index(UShort_t index) {_index = index;}  

  void print(std::ostream& os = std::cout) const; 

 private:	

  UShort_t _arm;
  UShort_t _station;
  UShort_t _octant;
  UShort_t _half_octant;
  UShort_t _gap;
  UShort_t _index;

  double _x;
  double _y;
  double _z;
  UShort_t _anode;
  double _anode_dca;
  double _charge_corr;  // q2 = (1+_charge_corr)*q1

  ClassDef(TMutGapCoord_v2,1)
};

#endif /* __TMUTGAPCOORD_V2H__*/










