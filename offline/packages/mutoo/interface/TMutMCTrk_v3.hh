// Interface Object Class : TMutMCTrk
// Author: D.Fields
// Date: 
// Description: Class for Muon Tracker Monte-Carlo Track.

#ifndef __TMUTMCTRK_V3H__
#define __TMUTMCTRK_V3H__

#include<TDataType.h>
#include<PHKey.hh>
#include<PHException.h>
#include<PHVector.h>
#include<MUTOO.h>
#include<TMutMCTrk.hh>

class TMutMCTrk_v3 : public TMutMCTrk
{
  
 public:

  TMutMCTrk_v3();

  TMutMCTrk_v3(const Key& key, UShort_t arm, UShort_t index);

  TMutMCTrk_v3(const TMutMCTrk*);  

  TMutMCTrk_v3(const TMutMCTrk&);

  virtual ~TMutMCTrk_v3(){;}

  ULong_t get_file_key() const {return _file_key;}

  Long_t get_track_id() const {return _track_id;}

  UShort_t get_pid() const {return _pid;}

  Float_t get_charge() const {return _charge;}

  Int_t get_parent_id() const { return _parent_id;}

  Int_t get_parent_track_id() const { return _parent_track_id;}

  void set_pid(UShort_t pid) { _pid = pid; }

  void set_file_key(ULong_t file_key) { _file_key = file_key;}

  void set_track_id(Long_t track_id) { _track_id = track_id;}

  void set_charge(Float_t charge) { _charge = charge;}
  
  void set_parent_id(Int_t parent_id) { _parent_id = parent_id;}

  void set_parent_track_id(Int_t parent_track_id) {_parent_track_id = parent_track_id;}


  Float_t get_x_orig() const {return _x_orig;}

  Float_t get_y_orig() const {return _y_orig;}

  Float_t get_z_orig() const {return _z_orig;}

  Float_t get_px_orig() const {return _px_orig;}

  Float_t get_py_orig() const {return _py_orig;}

  Float_t get_pz_orig() const {return _pz_orig;}

  Float_t get_ptot_orig() const {return std::sqrt(MUTOO::SQUARE(_px_orig) +
						  MUTOO::SQUARE(_py_orig) +
						  MUTOO::SQUARE(_pz_orig));}

  void set_x_orig(Float_t x_orig) { _x_orig = x_orig; }

  void set_y_orig(Float_t y_orig) { _y_orig = y_orig; }

  void set_z_orig(Float_t z_orig) { _z_orig = z_orig; }

  void set_px_orig(Float_t px_orig) { _px_orig = px_orig; }

  void set_py_orig(Float_t py_orig) { _py_orig = py_orig; }

  void set_pz_orig(Float_t pz_orig) { _pz_orig = pz_orig; }

  Float_t get_x_us_gap() const {return _x_us_gap;}

  Float_t get_y_us_gap() const {return _y_us_gap;}

  Float_t get_z_us_gap() const {return _z_us_gap;}

  Float_t get_px_us_gap() const {return _px_us_gap;}

  Float_t get_py_us_gap() const {return _py_us_gap;}

  Float_t get_pz_us_gap() const {return _pz_us_gap;}

  Float_t get_ptot_us_gap() const {return std::sqrt(MUTOO::SQUARE(_px_us_gap) +
						    MUTOO::SQUARE(_py_us_gap) +
						    MUTOO::SQUARE(_pz_us_gap));}
  
  void set_x_us_gap(Float_t x_us_gap) { _x_us_gap = x_us_gap; }

  void set_y_us_gap(Float_t y_us_gap) { _y_us_gap = y_us_gap; }

  void set_z_us_gap(Float_t z_us_gap) { _z_us_gap = z_us_gap; }

  void set_px_us_gap(Float_t px_us_gap) { _px_us_gap = px_us_gap; }

  void set_py_us_gap(Float_t py_us_gap) { _py_us_gap = py_us_gap; }

  void set_pz_us_gap(Float_t pz_us_gap) { _pz_us_gap = pz_us_gap; }

  PHVector get_us_tangent() const { 
    PHVector temp(_px_us_gap, _py_us_gap, _pz_us_gap);
    temp.normalize();
    return temp;
  }

  Float_t get_x_ds_gap() const {return _x_ds_gap;}

  Float_t get_y_ds_gap() const {return _y_ds_gap;}

  Float_t get_z_ds_gap() const {return _z_ds_gap;}

  Float_t get_px_ds_gap() const {return _px_ds_gap;}

  Float_t get_py_ds_gap() const {return _py_ds_gap;}

  Float_t get_pz_ds_gap() const {return _pz_ds_gap;}

  Float_t get_ptot_ds_gap() const {return std::sqrt(MUTOO::SQUARE(_px_ds_gap) +
						    MUTOO::SQUARE(_py_ds_gap) +
						    MUTOO::SQUARE(_pz_ds_gap));}

  void set_x_ds_gap(Float_t x_ds_gap) { _x_ds_gap = x_ds_gap; }

  void set_y_ds_gap(Float_t y_ds_gap) { _y_ds_gap = y_ds_gap; }

  void set_z_ds_gap(Float_t z_ds_gap) { _z_ds_gap = z_ds_gap; }

  void set_px_ds_gap(Float_t px_ds_gap) { _px_ds_gap = px_ds_gap; }

  void set_py_ds_gap(Float_t py_ds_gap) { _py_ds_gap = py_ds_gap; }

  void set_pz_ds_gap(Float_t pz_ds_gap) { _pz_ds_gap = pz_ds_gap; }

  UShort_t get_arm() const {return _arm;}

  UShort_t get_index() const {return _index;}

  void set_arm(UShort_t arm) { _arm = arm; }

  void set_index(UShort_t index) { _index = index; }

  void print(std::ostream& os = std::cout) const;

private:	
  
  UShort_t _arm;
  UShort_t _index;
  ULong_t _file_key;
  Long_t _track_id;
  UShort_t _pid;
  Int_t _parent_id;
  Float_t _charge;
  
  Float_t _x_orig;
  Float_t _y_orig;
  Float_t _z_orig;
  Float_t _px_orig;
  Float_t _py_orig;
  Float_t _pz_orig;

  Float_t _x_us_gap;
  Float_t _y_us_gap;
  Float_t _z_us_gap;
  Float_t _px_us_gap;
  Float_t _py_us_gap;
  Float_t _pz_us_gap;

  Float_t _x_ds_gap;
  Float_t _y_ds_gap;
  Float_t _z_ds_gap;
  Float_t _px_ds_gap;
  Float_t _py_ds_gap;
  Float_t _pz_ds_gap;

  Int_t _parent_track_id;
  
  ClassDef(TMutMCTrk_v3,1)
};
		
#endif /* __TMutMCTrk_V3H__*/












