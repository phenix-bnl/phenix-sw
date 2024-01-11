#ifndef __MMUTUNPACKPAR_HH__
#define __MMUTUNPACKPAR_HH__

#include "TMutParBase.h"
#include <MUTOO.h>

// boost
#include<boost/array.hpp>



//!Runtime parameter object for mMutUnpack analysis module
/*! 
*/
class mMutUnpackPar : public TMutParBase
{

 public: 

  /*! default constructor */
  mMutUnpackPar() : 
    _check_detector_id( true ),
    _check_user_word( true )
    { }
  
  /*! destructor */
  ~mMutUnpackPar(){;}

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
  
  //! print
  void print( void )
  {
    MUTOO::PRINT( std::cout, "mMutUnpackPar" );
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

};

#endif /* __MMUTUNPACK_HH__ */







