// Interface Object Class : TMCPrimary
// Author: S.Kelly
// Date: 9/07/03
// Description: Class for Monte-Carlo Primary 

#ifndef __TMCPRIMARY_V3H__
#define __TMCPRIMARY_V3H__

#include<TDataType.h>
#include<PHKey.hh>
#include<PHException.h>
#include<PHVector.h>
#include<MUTOO.h>
#include<cmath>

#include<TMCPrimary.hh>

/*! @ingroup interface */
//! Monte-Carlo Primary Object
/*! Monte-Carlo Primary Object */

class TMCPrimary_v3 : public TMCPrimary
{
  
 public:

  TMCPrimary_v3();
  TMCPrimary_v3(const Key& key, UShort_t index);
  TMCPrimary_v3(const TMCPrimary*);  
  TMCPrimary_v3(const TMCPrimary&);
  virtual ~TMCPrimary_v3(){;}

  ULong_t get_file_key() const {return _file_key;}
  Long_t get_pisa_process_id() const {return _pisa_process_id;}
  UShort_t get_pid() const {std::cout << "TMCPrimary_v3::get_pid() - WARNING! THIS METHOD IS DEPRECATED. USE get_pidG4()..." << std::endl; return 0;}
  Int_t get_pidG4() const {return _pid;}
  Int_t get_trk_id() const {return _trk_id;}
  Float_t get_imp() const { return _imp;}
  Int_t get_Ncoll() const { return _Ncoll;}

  void set_pid(Int_t pid) { _pid = pid; }
  void set_trk_id(Int_t trk_id) { _trk_id = trk_id;}
  void set_file_key(ULong_t file_key) { _file_key = file_key;}
  void set_pisa_process_id(Long_t pisa_process_id) { _pisa_process_id = pisa_process_id;}
  void set_Ncoll(Int_t ncoll) {_Ncoll = ncoll;}
  void set_imp(Float_t imp) {_imp = imp;}


  Float_t get_x_orig() const {return _x_orig;}
  Float_t get_y_orig() const {return _y_orig;}
  Float_t get_z_orig() const {return _z_orig;}
  Float_t get_px_orig() const {return _px_orig;}
  Float_t get_py_orig() const {return _py_orig;}
  Float_t get_pz_orig() const {return _pz_orig;}
  Float_t get_energy_orig() const {return _energy_orig;}

  Float_t get_rapidity_orig() const {return 0.5*std::log((_energy_orig + _pz_orig)/
							 (_energy_orig - _pz_orig)); }
  
  Float_t get_pt_orig() const {return std::sqrt(MUTOO::SQUARE(_px_orig) +
						MUTOO::SQUARE(_py_orig));}
  
  Float_t get_ptot_orig() const {return std::sqrt(MUTOO::SQUARE(_px_orig) +
						  MUTOO::SQUARE(_py_orig) +
						  MUTOO::SQUARE(_pz_orig));}
  
  void set_x_orig(Float_t x_orig) { _x_orig = x_orig; }
  void set_y_orig(Float_t y_orig) { _y_orig = y_orig; }
  void set_z_orig(Float_t z_orig) { _z_orig = z_orig; }
  void set_px_orig(Float_t px_orig) { _px_orig = px_orig; }
  void set_py_orig(Float_t py_orig) { _py_orig = py_orig; }
  void set_pz_orig(Float_t pz_orig) { _pz_orig = pz_orig; }
  void set_energy_orig(Float_t energy_orig) { _energy_orig = energy_orig; }
  void set_index(UShort_t index) { _index = index; }
  void print(std::ostream& os = std::cout) const;
  Int_t get_version() const {return 3;}

private:	
  
  UShort_t _arm;
  UShort_t _index;
  ULong_t _file_key;
  Long_t _pisa_process_id;
  Int_t _pid;
  Int_t _trk_id;
  Float_t _imp;
  Int_t _Ncoll;
  
  Float_t _x_orig;
  Float_t _y_orig;
  Float_t _z_orig;
  Float_t _px_orig;
  Float_t _py_orig;
  Float_t _pz_orig;
  Float_t _energy_orig;  

  ClassDef(TMCPrimary_v3,1)
};
		

		
#endif /* __TMCPRIMARY_V3H__*/












