#ifndef __MRXNPCALXANGPAR_HH__
#define __MRXNPCALXANGPAR_HH__

#include <TRxnpParBase.h>
#include <MUTOO.h>
#include <cassert>
// boost
#include<boost/array.hpp>

//!Runtime parameter object for mRxnpCalXang analysis module
/*! 
*/
class mRxnpCalXangPar : public TRxnpParBase
{

 public: 

  /*! default constructor */
  mRxnpCalXangPar() : 
    _check_detector_id( true ),
    _check_user_word( true ),
    _iter_state(std::make_pair(0,0))
    { }
  
  /*! destructor */
  virtual ~mRxnpCalXangPar(){;}

  //! set check_detector_id flag
  void set_check_detector_id( bool value ) 
  { _check_detector_id = value; }
  
  //! get check_detector_id flag
  bool get_check_detector_id( void ) const
  { return _check_detector_id; }

  //! set check_detector_id flag
  void set_check_user_word( bool value ) 
  { _check_user_word = value; }
  
  //! get check_detector_id flag
  bool get_check_user_word( void ) const
  { return _check_user_word; }
  
  //! set the iteration parameter
  void set_iteration(int iter) 
    {
      _iter_state.second = iter;
      if (_iter_state.second==2 &&
          _iter_state.first==0) {
        _iter_state.first = -1;
      }
    }
  
  //! get the iteration parameter
  int get_iteration(int which=2) const 
    { 
      if(which == 1)
	return _iter_state.first;
      if(which == 2)
	return _iter_state.second;
      assert (which > 0 && which < 3);
      return -1; // make the compiler happy
    }
  //! check if iteration state has been flipped
  bool state_change() 
    {
      if(_iter_state.second != _iter_state.first)
	{
	  return true;
	}
      return false;
    }

  // sychronize state
  //
  void sychroniz_iter_state()
    {
      _iter_state.first = _iter_state.second;
    }

  //! print
  void print( void )
  {
    MUTOO::PRINT( std::cout, "mRxnpcalXangPar" );
    std::cout << "_verbosity = " << _verbosity << ".\n";
    std::cout << "_check_detector_id = " << _check_detector_id << ".\n";
    std::cout << "_check_user_word = " << _check_user_word << ".\n";    
    MUTOO::PRINT( std::cout, "**" );
  } 
   
 private:  

  //! if true (default) detector ID is checked to be 0xb00 in the PRDF packet
  bool _check_detector_id;

  //! if true (default) user word 7 checked to be 0x8080000 in the PRDF packet
  bool _check_user_word;
  //! flattening needs iterate the data many times, a field is used to remember the iteration.
  std::pair<int,int> _iter_state;
  

};

#endif /* __MRXNPCALXANG_HH__ */







