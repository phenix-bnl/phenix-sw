// Interface Object Class : TMuiMCHitO
// Author: J.L.Nagle
// Date: 6/23/2003

#ifndef __TMUIMCHIT_V1_H__
#define __TMUIMCHIT_V1_H__

#include<TDataType.h>
#include<PHKey.hh>
#include<TMuiMCHitO.h>
#include<TMuiMCTwoPack.h>
#include<PHException.h>
#include<PHPoint.h>

class TMuiMCHitO_v1 : public TMuiMCHitO
{
  
public:

  TMuiMCHitO_v1();

  TMuiMCHitO_v1(const Key& key, UShort_t arm, UShort_t plane, UShort_t index);

  TMuiMCHitO_v1(const TMuiMCHitO* base_ptr);
  TMuiMCHitO_v1(const TMuiMCHitO& base_ref);

  virtual ~TMuiMCHitO_v1() {;}

  void add_twopack(UShort_t orient, UShort_t panel, UShort_t twopack_index){
    _twopack_list.push_back(TMuiMCTwoPack(orient, panel,twopack_index));
  }  

  void clear_twopack_list(){
    _twopack_list.clear();
  }			     

  size_t get_n_twopack() const {
    return _twopack_list.size();
  }

  const twopack_list* get_twopack_list() const {return &_twopack_list;}

  twopack_list* get_mutable_twopack_list() {return &_twopack_list;}

  Int_t get_track_id() const {return _track_id;}

  Float_t get_x() const {return _x;}

  Float_t get_y() const {return _y;}

  Float_t get_z() const {return _z;}

  Float_t get_px() const {return _px;}

  Float_t get_py() const {return _py;}

  Float_t get_pz() const {return _pz;}

  Short_t get_pid() const { return _pid;}
  
  UShort_t get_file_key() const { return _file_key;}

  void set_track_id(Int_t track_id) { _track_id = track_id;}

  void set_x(Float_t x) { _x = x; }

  void set_y(Float_t y) { _y = y; }

  void set_z(Float_t z) { _z = z; }

  void set_px(Float_t px) { _px = px; }

  void set_py(Float_t py) { _py = py; }

  void set_pz(Float_t pz) { _pz = pz; }

  void set_pid(Short_t pid) {_pid = pid;}

  void set_file_key(UShort_t file_key) {_file_key = file_key;} 

  UShort_t get_arm() const {return _arm;}

  UShort_t get_plane() const {return _plane;}

  UShort_t get_index() const {return _index;}

  void set_arm(UShort_t arm) { _arm = arm; }

  void set_plane(UShort_t plane) { _plane = plane; }

  void set_index(UShort_t index) { _index = index; }
  
  void print(std::ostream& os = std::cout) const;
  
private:	
  
  UShort_t _arm;
  UShort_t _plane;
  UShort_t _index;
  Int_t _track_id;
  Float_t _x;
  Float_t _y;
  Float_t _z;
  Float_t _px;
  Float_t _py;
  Float_t _pz;
  Short_t _pid;
  UShort_t _file_key;
  twopack_list _twopack_list;
  ClassDef(TMuiMCHitO_v1,1)
};

#endif /* __TMUIMCHITO_V1_H__*/



