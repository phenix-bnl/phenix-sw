// Interface Object Class : TMCPrimary
// Author: S.Kelly
// Date: 9/07/03
// Description: Class for Monte-Carlo Primary 

#ifndef __TMCPRIMARY_V1H__
#define __TMCPRIMARY_V1H__

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

class TMCPrimary_v1 : public TMCPrimary
{
  
 public:

  TMCPrimary_v1();
  TMCPrimary_v1(const Key& key, UShort_t index);
  TMCPrimary_v1(const TMCPrimary*);  
  TMCPrimary_v1(const TMCPrimary&);
  virtual ~TMCPrimary_v1(){;}

  ULong_t get_file_key() const {return _file_key;}
  Long_t get_pisa_process_id() const {return _pisa_process_id;}
  UShort_t get_pid() const {return _pid;}

  void set_pid(UShort_t pid) { _pid = pid; }
  void set_file_key(ULong_t file_key) { _file_key = file_key;}
  void set_pisa_process_id(Long_t pisa_process_id) { _pisa_process_id = pisa_process_id;}

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

private:	
  
  UShort_t _arm;
  UShort_t _index;
  ULong_t _file_key;
  Long_t _pisa_process_id;
  UShort_t _pid;
  
  Float_t _x_orig;
  Float_t _y_orig;
  Float_t _z_orig;
  Float_t _px_orig;
  Float_t _py_orig;
  Float_t _pz_orig;
  Float_t _energy_orig;  

  ClassDef(TMCPrimary_v1,1)
};
		

		
#endif /* __TMCPRIMARY_V1_H__*/












