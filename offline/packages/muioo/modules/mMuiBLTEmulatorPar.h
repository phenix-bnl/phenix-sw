#ifndef __MMUIBLTEMULATORPAR_HH__
#define __MMUIBLTEMULATORPAR_HH__

#include<TMuiParBase.h>
#include<TDataType.h>
#include<PHException.h>
#include<MUIOO.h>

//! Runtime parameter object for mMuiResponse analysis module.
/*! 
Runtime parameter object for mMuiResponse analysis module
*/
class mMuiBLTEmulatorPar : public TMuiParBase
{  
 public: 
  /*! select input data. PRDF/DST */
  enum mode {FROMPRDF,FROMDST};

  /*! default constructor */
  mMuiBLTEmulatorPar():
    _mlu_map(0),
    _mod(FROMDST),
    _debug(0)
    {}
  
  /*! destructor */
  ~mMuiBLTEmulatorPar(){;}

  //! input mode
  void set_mode(mode mod) 
  { _mod = mod;}
  
  //! mlu map selection
  void set_mlu_map(int map_selection) 
  { _mlu_map = map_selection;}
  
  //! debug flag
  void set_debug_flag(int debug) 
  {_debug = debug;}
  
  //! input mode
  mode get_mode() const 
  { return _mod;}
  
  //! mlu map selection
  int get_mlu_map() const 
  { return _mlu_map;}
  
  //! debug flag
  int get_debug_flag() const 
  { return _debug;}

	//! dump all parameters  
	void print( std::ostream& out = std::cout ) 
	{
		MUIOO::PRINT( out, "mMuiBLTEmulatorPar" );
    out << "_mlu_map = " << _mlu_map << std::endl;
    out << "_mod = " << _mod << std::endl;
    out << "_debug = " << _debug << std::endl;
    MUIOO::PRINT( out, "**" );
  }
  
  private:
  
  //! mlu map selection
  int _mlu_map;
  
  //! input mode
  mode _mod;
  
  //! debug mode
  int _debug;
};

#endif /* __MMUIBLTEMULATORPAR_HH__ */







