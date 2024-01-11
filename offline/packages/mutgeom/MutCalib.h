#ifndef __MUTCALIB_H__
#define __MUTCALIB_H__

// $Id: MutCalib.h,v 1.7 2011/09/08 18:26:23 slash Exp $

/*!
  \file MutCalib.cc
  \brief wrapper around muon tracker calibrations initialization
  \author Douglas Fields, Nicki Bruner, Hugo Pereira
  \version $Revision: 1.7 $
  \date $Date: 2011/09/08 18:26:23 $
*/

#include <cassert>

#include "MutCalibStrip.hh"

//! return pointer to all calibrations
/*!
the actual calibration object is stored in the MutCalibSingleton object.
This function is kept for backward compatibility
UseNewCalibrationMethod is set only in the initialization. 
If called after it it will not change the calibration option.
*/
MutCalibStrip* MutCalib();

//! singleton to handle Mut Calibrations
class MutCalibSingleton
{

  public:

  //! return singleton
  static MutCalibSingleton& get( void );

  //! initialize
  void initialize();

  //! true if initialized
  bool initialized( void ) const
  { return _initialized; }

  //! reset
  void reset( void );

  void setUseNewCalibrationMethod( bool value )
  {
    assert( !initialized() );
    _calibrations.setUseNewCalibrationMethod( value );
  }

  //! decide if gain corrections must be applied when reading from DB
  void setUseGainCorrections( bool value )
  {
    assert( !initialized() );
    _calibrations.setUseGainCorrections( value );
  }

  //! decide if using the new calibration scheme or not
  bool getUseNewCalibrationMethod( void ) const
  { return _calibrations.getUseGainCorrections(); }

  //! decide if gain corrections must be applied when reading from DB
  bool getUseGainCorrections( void ) const
  { return _calibrations.getUseGainCorrections(); }

  //! return calibrations
  MutCalibStrip& get_calibrations( void )
  { return _calibrations; }

  //! destructor
  virtual ~MutCalibSingleton( void );

  protected:

  //! constructor
  MutCalibSingleton( void );

  //! initialization flag
  void set_initialized( bool value )
  { _initialized = value; }

  private:

  //! Mut calibration object
  MutCalibStrip _calibrations;

  //! true if initialized
  bool _initialized;

};

#endif /*__MUTCALIB_H__*/
