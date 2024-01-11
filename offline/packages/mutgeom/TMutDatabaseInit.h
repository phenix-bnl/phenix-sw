// $Id: TMutDatabaseInit.h,v 1.11 2010/05/07 18:19:57 hpereira Exp $
#ifndef __TMUTDATABASEINIT_H__
#define __TMUTDATABASEINIT_H__

/*!
  \file TMutDatabaseInit.h
  \brief Statically scoped class for muon database initialization
  \author Sean Kelly
  \version $Revision: 1.11 $
  \date $Date: 2010/05/07 18:19:57 $
*/

#include <PHTimeStamp.h>

class PHCompositeNode;

/*! \ingroup classes */
//! Statically scoped class for muon database initialization
/*!
  Statically scoped class for initializing mut/mutoo. Currently this
  includes initializing the calibrations, geomtry, bfield and GEANT.
*/
class TMutDatabaseInit
{
 public:

  //! reset initialization flag
  static void reset( void );

  //! do initialization
  static void initialize( PHCompositeNode* dummy_node = 0 );

  //! returns true if initialization was done
  static bool init_done( void )
  { return _init_done; }

  //!@name timestamp and alignment bank id
  //@{

  //! current run timestamp
  static void set_time_stamp( PHTimeStamp time_stamp )
  {
    _time_stamp_set = true;
    _time_stamp = time_stamp;
  }

  //! current run timestamp
  static PHTimeStamp get_time_stamp()
  { return _time_stamp; }

  //! version number for alignment file.
  static void set_alignment_bank_id(int version)
  { _alignment_bank_id = version; }

  //! version number for alignment file.
  static int get_alignment_bank_id()
  { return _alignment_bank_id; }

  //@}

  private:

  //! make the initialization
  static bool do_init( void );

  //! true when initialization has been done
  static bool _init_done;

  //! true if timestamp is set manually
  static bool _time_stamp_set;

  //! time stamp
  static PHTimeStamp _time_stamp;

  //! alignment bank version (used in MutGeom)
  static int _alignment_bank_id;

};

#endif	//__TMUTDATABASEINIT_H__
