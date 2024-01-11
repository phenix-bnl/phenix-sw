// Interface Object Class : TMutMCHit
// Author: S.Kelly/D.Fields
// Date: 1/2/21

#ifndef __TFvtxMCHit_v1_h__
#define __TFvtxMCHit_v1_h__

#include<TDataType.h>
#include<PHKey.hh>
#include<TMutMCStrip.hh>
#include<PHException.h>
#include<PHPoint.h>
#include<TMutFitPar.hh>
#include<TMutMCHit.hh>

class TMutMCHit_v1 : public TMutMCHit
{
	
public:

	TMutMCHit_v1();

	TMutMCHit_v1(const Key& key, UShort_t arm, UShort_t station, UShort_t octant,
						UShort_t half_octant, UShort_t gap, UShort_t index);

	TMutMCHit_v1(const TMutMCHit* base_ptr);
	TMutMCHit_v1(const TMutMCHit& base_ref);

	virtual ~TMutMCHit_v1() {;}

	void add_strip(UShort_t cathode, UShort_t strip, Float_t q){
		_strip_list.push_back(TMutMCStrip(cathode,strip,q));
	}	

	void clear_strip_list(){
		_strip_list.clear();
	}					 

	size_t get_n_strip() const {
		return _strip_list.size();
	}

	const strip_list* get_strip_list() const 
	{return &_strip_list;}

	Long_t get_track_id() const {return _track_id;}

	Float_t get_tof() const {return _tof;}

	Float_t get_eloss() const {return _eloss;}

	Float_t get_x() const {return _x;}

	Float_t get_y() const {return _y;}

	Float_t get_z() const {return _z;}

	Float_t get_px() const {return _px;}

	Float_t get_py() const {return _py;}

	Float_t get_pz() const {return _pz;}

	TMutFitPar get_fit_par() const { return TMutFitPar(_x,_y,_z,_px/_pz,_py/_pz,0); }

	Float_t get_w_true(UShort_t index) const {
		BOUNDS_CHECK(index,W_SIZE);return _w_true[index];
	}

	Float_t get_w_digit(UShort_t index) const {
		BOUNDS_CHECK(index,W_SIZE);return _w_digit[index];
	}	
	void set_track_id(Long_t track_id) { _track_id = track_id;}

	void set_tof(Float_t tof) { _tof = tof; }

	void set_eloss(Float_t eloss) { _eloss = eloss; }

	void set_x(Float_t x) { _x = x; }

	void set_y(Float_t y) { _y = y; }

	void set_z(Float_t z) { _z = z; }

	void set_px(Float_t px) { _px = px; }

	void set_py(Float_t py) { _py = py; }

	void set_pz(Float_t pz) { _pz = pz; }

	void set_w_true(UShort_t index, Float_t w_true) {		
		BOUNDS_CHECK(index,W_SIZE); _w_true[index] = w_true; 
	}

	void set_w_digit(UShort_t index, Float_t w_digit) { 
		BOUNDS_CHECK(index,W_SIZE); _w_digit[index] = w_digit; 
	}
	
	UShort_t get_arm() const {return _arm;}

	UShort_t get_station() const {return _station;}

	UShort_t get_octant() const {return _octant;}

	UShort_t get_half_octant() const {return _half_octant;}

	UShort_t get_gap() const {return _gap;}

	UShort_t get_wire() const {return _wire;}

	UShort_t get_index() const {return _index;}

	void set_arm(UShort_t arm) { _arm = arm; }

	void set_station(UShort_t station) { _station = station; }

	void set_octant(UShort_t octant) { _octant = octant; }

	void set_half_octant(UShort_t half_octant) { _half_octant = half_octant; }

	void set_gap(UShort_t gap) { _gap = gap; }

	void set_wire(UShort_t wire) { _wire = wire; }

	void set_index(UShort_t index) { _index = index; }
	
	void print(std::ostream& os = std::cout) const;
	
private:	
	
	UShort_t _arm;
	UShort_t _station;
	UShort_t _octant;
	UShort_t _half_octant;
	UShort_t _gap;
	UShort_t _wire;
	UShort_t _index;
	Long_t _track_id;
	Float_t _tof;
	Float_t _eloss;
	Float_t _x;
	Float_t _y;
	Float_t _z;
	Float_t _px;
	Float_t _py;
	Float_t _pz;
	enum { W_SIZE=2 };
	Float_t _w_true[W_SIZE];
	Float_t _w_digit[W_SIZE];
	strip_list _strip_list;
	ClassDef(TMutMCHit_v1,1)
};

#endif /* __TMutMCHit_V1_H__*/



