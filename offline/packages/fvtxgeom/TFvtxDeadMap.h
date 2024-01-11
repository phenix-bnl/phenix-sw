// $Id: TFvtxDeadMap.h,v 1.4 2014/02/27 16:36:31 jinhuang Exp $                                                                                             

/*!
 * \file TFvtxDeadMap.h
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.4 $
 * \date $Date: 2014/02/27 16:36:31 $
 */

#ifndef TFVTXDEADMAP_H_
#define TFVTXDEADMAP_H_

#include <PHTimeStamp.h>
#include <PHString.h>
#include <PdbFvtxDeadMap.hh>

#include <vector>

#ifndef __CINT__
#include <boost/smart_ptr.hpp>
#endif

namespace odbc
{
  class Connection;
}

#include <TObject.h>

/*!
 * \brief Managne the dead maps and io to database/files
 *
 * See examples in example_fvtxdb2calib_db and example_deadchan2calib_db
 */
class TFvtxDeadMap : public TObject
{

public:
  TFvtxDeadMap();
  virtual
  ~TFvtxDeadMap();

  void
  Print(Option_t *option = "") const;

  ////////////////////////////////////////////////////////
  //!@name access the a dead channel record with in the maps
  //@{

public:

  typedef PdbFvtxDeadMap::dead_map_record dead_map_record;

  /*! \brief Decoder/Encoder for the dead channel records in database
   *
   record is an integer, packed in the form abcddeffff,
   where a=arm, b=cage, c=station, dd=wedge, e=side, ffff=strip.
   Values that are 'too high', for example strip=9999 means 'all strips'.
   Thus the last number in the example is 1132189999,
   which is arm=1, cage=1, station=3, wedge=21, side=8 (both sides), and strip=9999 (all strips).

   A copy of FVTXGEOM constants were defined here again to make dead_map_coder independent of other libs
   */
  class dead_map_coder
  {

  public:
    //! dead_map_record -> arm, cage, ...
    dead_map_coder(dead_map_record record);

    //! arm, cage, ... -> dead_map_record
    static dead_map_record
    encoder( //
        int arm = -1, //
        int cage = -1, //
        int station = -1, //
        int sector = -1, //
        int side = -1, //
        int strip = -1 //
        );

    void
    print() const;

    int orig_record;
    int arm;
    int cage;
    int station;
    int sector;
    int side;
    int strip;

  };

  //@}

  ////////////////////////////////////////////////////////
  //!@name access the maps
  //@{

#ifndef __CINT__

public:

  typedef boost::shared_ptr<PdbFvtxDeadMap> dead_map_ptr;

  typedef std::vector<dead_map_ptr> dead_map_vec;

  //! get the ith map
  virtual dead_map_ptr
  get_dead_map(size_t i)
  {
    return dead_maps[i];
  }

  //! get the ith map
  virtual size_t
  get_ndead_map()
  {
    return dead_maps.size();
  }

  //! add a new map
  virtual dead_map_ptr
  add_dead_map()
  {
    dead_maps.push_back(boost::make_shared<PdbFvtxDeadMap>());

    return dead_maps.back();
  }

  //! add a new map
  virtual void
  erase_dead_map(int i)
  {
    dead_maps.erase(dead_maps.begin() + i);
  }

  //! add a new map
  virtual dead_map_ptr
  add_dead_map(const PdbFvtxDeadMap & map)
  {
    dead_maps.push_back(boost::make_shared<PdbFvtxDeadMap>(map));

    return dead_maps.back();
  }

  //! add a new dead channel to existing map
  static void
  add_dead_chan(dead_map_ptr ptr, //
      int arm = -1, //
      int cage = -1, //
      int station = -1, //
      int sector = -1, //
      int side = -1, //
      int strip = -1 //
      )
  {
    ptr->add_record(dead_map_coder::encoder(
    /*int*/arm, //
        /*int*/cage, //
        /*int*/station, //
        /*int*/sector, //
        /*int*/side, //
        /*int*/strip //
        ));
  }

  // array of dead maps
  virtual dead_map_vec &
  get_dead_maps()
  {
    return dead_maps;
  }

#endif

  //! # of dead maps
  virtual size_t
  get_n_dead_maps() const
  {
    return dead_maps.size();
  }

  //! remove all dead maps in memory
  virtual void
  clear_all_dead_maps()
  {
    return dead_maps.clear();
  }

protected:

#ifndef __CINT__

  //! list of dead maps
  dead_map_vec dead_maps;

#endif

  //@}

  ////////////////////////////////////////////////////////
  //!@name apply dead map for analysis
  //@{

public:

  //! simple function to call at the start of run in Fun4All cycle
  //! which take care loading and applying dead maps as configured in TFvtxDatabaseCntrl
  static void
  init_run();

  //! use current stored dead map in analysis
  void
  apply_dead_maps(void) const;

private:
  //! apply one dead channel in analysis
  static void
  apply_dead_chan(const dead_map_coder &map, const int nrows);

  //@}

  ////////////////////////////////////////////////////////
  //!@name interface with calibration database  - read / write
  //@{

public:

  //! list of band ids
  enum enu_band_id
  {

    //! Use for produciton, usually contains dead map or maps for one run
    BAND_ID_RUN_SINGLE = 1,

    //! Use for produciton, usually contains dead map or maps for many runs, eg. for all run12
    BAND_ID_RUN_PERIOD = 2,

    //! Use for simulation.
    BAND_ID_SIM = 101,

    //! for test purpose. Will not be used for production or simulation by default
    BAND_ID_TEST = 201

  };

  //! read calibration objects from database
  int
  dbGetAll(int runnumber, bool is_sim = false);

  //! read calibration objects from database
  int
  dbGetAll(PHTimeStamp tsearch, bool is_sim = false)
  {
    int ret = 0;

    if (is_sim)
      { // for simulations
        ret += dbGet(tsearch, BAND_ID_SIM);
      }
    else
      { // for production

        ret += dbGet(tsearch, BAND_ID_RUN_SINGLE);
        ret += dbGet(tsearch, BAND_ID_RUN_PERIOD);

      }

    return ret;
  }

  //! read calibration objects from database for specific band_id
  int
  dbGet(PHTimeStamp tsearch, enu_band_id band_id);

  //! write calibration objects to database in specific band_id
  int
  dbPutAll(int beginrun, int endrun, PHString descriptor, //
      enu_band_id band_id = BAND_ID_RUN_SINGLE) const;

  //! write calibration objects to database in specific band_id
  int
  dbPutAll(PHTimeStamp start, PHTimeStamp stop, PHString descriptor, //
      enu_band_id band_id = BAND_ID_RUN_SINGLE) const;

private:

  static const PHString calibname;

  static const PHString classname;

  //@}

  ////////////////////////////////////////////////////////
  //!@name interface with fvtx database for historical compatibility - read only
  //@{

public:

  //! automatic select and load the dead channel map according to runnum
  void
  automatic_load_dead_chan_map_fvtxdb(int run_num = 0);

  //! load the dead channel map with given name
  void
  load_dead_chan_map_fvtxdb(const std::string &, int run_num = 0);

  //! close db connection
  void
  close_con_fvtxdb();

private:

  // database connection
  odbc::Connection * _con;

  //@}

  ////////////////////////////////////////////////////////
  //!@name interface with text files  - read / write
  //@{

public:

  //! read calibration from ASCII text
  int
  txtGetAll(const char* infile, const char * comment = NULL);

  //! write calibration to ASCII text
  int
  txtPutAll(const char* outfile) const;

  //@}

  ////////////////////////////////////////////////////////
  //!@name examples of how to use this TFvtxDeadMap in your code
  //!
  //! to compile with your code, please also #include "TFvtxDeadMap.h"
  //!
  //@{

public:

  //! read fvtx database and put into calibration database
  static void
  example_fvtxdb2calib_db(int run);

  //! write a single dead channel into calibration database
  static void
  example_deadchan2calib_db(int run);

  //! Print what's in calibration database for a single run
  static void
  example_print_calib_db(int run);

  //! Save an empty dead map for a long run period in band_id BAND_ID_RUN_PERIOD,
  //! so dbGetAll() do not report error if long period map is not filled.
  static void
  example_make_dummy_deadmap();

  //@}
ClassDef(TFvtxDeadMap,1)

};

#endif /* TFVTXDEADMAP_H_ */
