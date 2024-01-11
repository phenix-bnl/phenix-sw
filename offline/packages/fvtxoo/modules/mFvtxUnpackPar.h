// $Id: mFvtxUnpackPar.h,v 1.3 2014/01/26 16:47:16 bbannier Exp $

#ifndef __MFVTXUNPACKPAR_HH__
#define __MFVTXUNPACKPAR_HH__

/*!
        \file mFvtxUnpackPar.h
        \brief class for fvtx unpack parameter objects. 
        \author Zhengyun You
        \version $Revision: 1.3 $
        \date $Date: 2014/01/26 16:47:16 $
*/

#include "TFvtxParBase.h"
#include <FVTXOO.h>

//#include<boost/array.hpp>

//!Runtime parameter object for mFvtxUnpack analysis module
/*! 
*/
class mFvtxUnpackPar : public TFvtxParBase
{

 public: 

  /*! default constructor */
  mFvtxUnpackPar() : 
    _check_detector_id( true ),
    _check_user_word( true )
    { }
  
  /*! destructor */
  ~mFvtxUnpackPar(){;}

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
  void print(std::ostream& out = std::cout) const {
    FVTXOO::PRINT(out, "mFvtxUnpackPar");
    out << "_verbosity = " << _verbosity << ".\n";
    out << "_check_detector_id = " << _check_detector_id << ".\n";
    out << "_check_user_word = " << _check_user_word << ".\n";
    FVTXOO::PRINT(out, "**");
  }

 private:  

  //! if true (default) detector ID is checked to be 0xb00 in the PRDF packet
  bool _check_detector_id;

  //! if true (default) user word 7 checked to be 0x8080000 in the PRDF packet
  bool _check_user_word;

  ClassDef(mFvtxUnpackPar, 1);
};

#endif /* __MFVTXUNPACK_HH__ */

