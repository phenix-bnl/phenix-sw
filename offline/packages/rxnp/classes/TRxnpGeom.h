#ifndef __TRXNPGEOM_H__
#define __TRXNPGEOM_H__

// $Id: TRxnpGeom.h,v 1.4 2007/03/11 16:08:02 esumi Exp $
//////////////////////////////////////////////////////////////////
/*
        \file TRxnpGeom.h
        \brief Utility class to store and fetch geom info from database
        \author Chun Zhang
        \version $Revision: 1.4 $
        \date    $Date: 2007/03/11 16:08:02 $
*/
//////////////////////////////////////////////////////////////////

// stl input
//#include <string>

// boost include
//#include <boost/thread.hpp>
#include <boost/shared_ptr.hpp>
// PHENIX include
//
#include <PHTimeStamp.h>

// RXNP include
#include <RXNP.h>

#include "TRxnpScintGeom.h"

class TRxnpGeom
{
  //  typedef boost::mutex Mutex;
  //  typedef Mutex::scoped_lock Lock;

 public:
  // public instance method
  static boost::shared_ptr<TRxnpGeom> instance();

  // destructor, public is cool, because we used share_ptr
  virtual ~TRxnpGeom();

  // public accessor to access phi, theta, z of each scintilator
  //
  // setter
  void set_phi_at(int arm, int ring, int scint, int ipos, float phi)
    {
      _scint[arm][ring][scint]->set_phi(ipos, phi);
    }
  void set_theta_at(int arm, int ring, int scint, int ipos, float theta)
    {
      _scint[arm][ring][scint]->set_theta(ipos, theta);
    }
  void set_z_at(int arm, int ring, int scint, int ipos, float z)
    {
      _scint[arm][ring][scint]->set_z(ipos, z);
    }
  // getter
  //
  PHTimeStamp get_t() const 
    { 
      //      Lock lock(_mutex);
      return _t;
    }
  float get_phi_at(int arm, int ring, int scint, int ipos) const
    {
      //      Lock lock(_mutex);	    
      return _scint[arm][ring][scint]->get_phi(ipos);
    }
  float get_theta_at(int arm, int ring, int scint, int ipos) const
    {
      //      Lock lock(_mutex);
      return _scint[arm][ring][scint]->get_theta(ipos);
    }
  float get_z_at(int arm, int ring, int scint, int ipos) const
    {
      //      Lock lock(_mutex);
      return _scint[arm][ring][scint]->get_z(ipos);
    }

  // take average of the survey positions. Note middle is shared
  //
  float get_theta_at(int arm, int ring, int scint) const
    { 
      //      Lock lock(_mutex);
      return _scint[arm][ring][scint]->get_theta();
    }
  float get_phi_at(int arm, int ring, int scint) const
    {
      //      Lock lock(_mutex);
      return _scint[arm][ring][scint]->get_phi();
    }

  float get_z_at(int arm, int ring, int scint) const
    { 
      //      Lock lock(_mutex);
      return _scint[arm][ring][scint]->get_z();
    }

  // init TRxnpGeom
  //
  bool init(const PHTimeStamp& T);
  // _use_db flag 
  //
  void set_use_db(bool use) { _use_db = use;}
  bool is_use_db() const {return _use_db;}

 private:

  // constructor
  TRxnpGeom();
  // also hide copy constructor
  TRxnpGeom(const TRxnpGeom& g) {;}
  // fetch the geom infor from PdbRxNPSurvey
  //
  bool fetch();
  // read geomtry from file
  //
  bool read();
  // dummy pointer, will be set to NULL and used to initialize myInstance
  static TRxnpGeom* dummy_ptr;
  // the instance 
  static boost::shared_ptr<TRxnpGeom> myInstance;
  // mutex lock for thread safe
  //  static Mutex _mutex;
  // multi-arry for geomtry of each scintilator
  TRxnpScintGeom* _scint[RXNP::NARM][RXNP::NRING][RXNP::NCHANNEL_PERRING];
  // has a time stamp
  //
  PHTimeStamp _t;
  // use DB flag
  //
  bool _use_db;
};

#endif
