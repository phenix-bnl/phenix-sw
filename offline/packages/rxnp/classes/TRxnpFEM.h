#ifndef __TRXNPFEM_H__
#define __TRXNPFEM_H__

// $Id: TRxnpFEM.h,v 1.6 2007/04/08 02:54:52 phnxmuid Exp $
//////////////////////////////////////////////////////////////////
/*
        \file TRxnpFEM.h
        \brief Utility class to store and fetch fem calibration consts info from database
        \author Chun Zhang
        \version $Revision: 1.6 $
        \date    $Date: 2007/04/08 02:54:52 $
*/
//////////////////////////////////////////////////////////////////

// stl input
#include <map>
#include <list>

// boost include
//#include <boost/thread.hpp>
#include <boost/shared_ptr.hpp>

// RXNP include
#include <RXNP.h>

#include "TRxnpFEMChannel.h"

class TRxnpFEM
{
  //  typedef boost::mutex Mutex;
  //  typedef Mutex::scoped_lock Lock;
  typedef boost::shared_ptr<TRxnpFEMChannel> data_type;
  typedef std::map<int, data_type>::iterator private_iter;
  typedef std::map<int, data_type>::const_iterator const_private_iter;

 public:
  typedef std::list<int> ChanList;

  // public instance method
  static boost::shared_ptr<TRxnpFEM> instance();

  // destructor, public is cool, because we used share_ptr
  virtual ~TRxnpFEM();

  // public accessor to access phi, theta, z of each scintilator
  //
  // getter
  //
  int get_run() const 
    { 
      //      Lock lock(_mutex);
      return _run;
    }
  // locator
  //
  int get_arm(int channel) const
    {
      //  Lock lock(_mutex);
      const_private_iter iter = _chan_map.find(channel);
      return (iter->second)->get_arm();       
    }
  int get_ring(int channel) const
    {
      //      Lock lock(_mutex);
      const_private_iter iter = _chan_map.find(channel);
      return (iter->second)->get_ring();      
    }
  int get_scint(int channel) const
    {
      //      Lock lock(_mutex);
      const_private_iter iter = _chan_map.find(channel);
      return (iter->second)->get_scint();      
    }

  // slop parameters
  //
  float get_high_slop(int channel, int cent) const 
    {
      //      Lock lock(_mutex);	    
      const_private_iter iter = _chan_map.find(channel);
      return  (iter->second)->get_high_slop(cent);
    }
  float get_low_slop(int channel, int cent) const
    {
      //      Lock lock(_mutex);	    
      const_private_iter iter = _chan_map.find(channel);
      return  (iter->second)->get_low_slop(cent);
    }
  float get_tdc_slop(int channel, int cent) const
    {
      //      Lock lock(_mutex);	    
      const_private_iter iter = _chan_map.find(channel);
      return (iter->second)->get_tdc_slop(cent);
    }
  // pedestal
  float get_high_int(int channel, int amu) const
    {
      //      Lock lock(_mutex);	    
      const_private_iter iter = _chan_map.find(channel);
      return (iter->second)->get_high_int(amu);      
    }
  float get_low_int(int channel, int amu) const
    {
      //      Lock lock(_mutex);	    
      const_private_iter iter = _chan_map.find(channel);
      return (iter->second)->get_low_int(amu);      
    }
  float get_tdc_int(int channel, int amu) const
    {
      //      Lock lock(_mutex);	    
      const_private_iter iter = _chan_map.find(channel);
      return (iter->second)->get_tdc_int(amu);      
    }
  // width of pedestal
  float get_high_wid(int channel, int amu) const
    {
      //      Lock lock(_mutex);	    
      const_private_iter iter = _chan_map.find(channel);
      return (iter->second)->get_high_wid(amu);      
    }
  float get_low_wid(int channel, int amu) const
    {
      //      Lock lock(_mutex);	    
      const_private_iter iter = _chan_map.find(channel);
      return (iter->second)->get_low_wid(amu);      
    }
  float get_tdc_wid(int channel, int amu) const
    {
      //      Lock lock(_mutex);	    
      const_private_iter iter = _chan_map.find(channel);
      return (iter->second)->get_tdc_wid(amu);      
    }
  float get_tdc_lg_slewcoeff(int channel, int iadc)
    {
      const_private_iter iter = _chan_map.find(channel);
      return (iter->second)->get_tdc_lg_slewcoeff(iadc);
    }


  // init TRxnpFEM
  //
  bool init(int run);

  // _use_db flag 
  //
  void set_use_db(bool use) { _use_db = use;}
  bool is_use_db() const { return _use_db;}
  // return a channel list
  //
  const ChanList* get_chan_list() const 
    {
      return &_chan_list;
    }

  // public accessor for NBbc
  // 
  inline int get_NBbc() const { return _NBbc;}

 private:

  // constructor
  TRxnpFEM();
  // also hide copy constructor
  TRxnpFEM(const TRxnpFEM& g) {;}
  // fetch the geom infor from PdbRxNPSurvey
  //
  bool fetch();
  // read geomtry from file
  //
  bool read();
  // public accessor for NBbc
  // 
  inline void set_NBbc(int n) { _NBbc = n;}
  // dummy pointer, will be set to NULL and used to initialize myInstance
  static TRxnpFEM* dummy_ptr;
  // the instance 
  static boost::shared_ptr<TRxnpFEM> myInstance;
  // mutex lock for thread safe1
  //  static Mutex _mutex;
  //! map used for mapping chanid to geometry location of scintilator
  //
  std::map<int, data_type> _chan_map;

  //! channel list
  std::list<int> _chan_list;

  // has a time stamp
  //
  int _run;
  // use DB flag
  //
  bool _use_db;
  // NBBC bins in calibration
  int _NBbc;
  
};

#endif
