#ifndef __MRXNPUNPACKPRDFPAR_HH__
#define __MRXNPUNPACKPRDFPAR_HH__

#include "TRxnpParBase.h"
#include <MUTOO.h>

// boost
#include<boost/array.hpp>

//!Runtime parameter object for mRxnpUnpackPRDF analysis module
/*! 
*/
class mRxnpUnpackPRDFPar : public TRxnpParBase
{

 public: 

  /*! default constructor */
  mRxnpUnpackPRDFPar() : 
    _check_detector_id( true ),
    _check_user_word( true )
    { }
  
  /*! destructor */
  ~mRxnpUnpackPRDFPar(){;}

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
    MUTOO::PRINT( std::cout, "mRxnpunpackPRDFPar" );
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

#endif /* __MRXNPUNPACKPRDF_HH__ */







