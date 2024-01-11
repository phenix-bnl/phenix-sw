// $Id: FvtxGeom.h,v 1.32 2015/08/31 14:10:30 snowball Exp $

#ifndef _FvtxGeom_h_
#define _FvtxGeom_h_

/*!
 \file FvtxGeom.h
 \brief Forward vertex geometry manager singleton.
 \Initialize and provide access to any Forward vertex strip geometry.
 \author Hugo Pereira da costa
 \version $Revision: 1.32 $
 \date $Date: 2015/08/31 14:10:30 $
 */

#include <vector>
#include <map>
#include <utility>
#include <string>

#include <TObject.h>
#include <TGeoShape.h>
#include <TGeoVolume.h>
#include <TGeoNode.h>
#include <TGeoMatrix.h>
#include <TGeoPhysicalNode.h>

#include <PHTimeStamp.h>
#include <PHString.h>

#ifndef __CINT__
#include <boost/array.hpp>
#include <boost/shared_ptr.hpp>
#endif

#include "FVTXGEOM.h"
#include "FvtxArm.h"
#include "FvtxDCMChannelMap.h"

//! Forward vertex geometry manager singleton.
/*!
 Initialize and provide access to any Forward vertex strip geometry.
 */
class FvtxGeom : public TObject
{
#ifdef __CINT__
  typedef FvtxArm* FVTXARM_PTR;
  typedef TGeoManager* TGEOMANAGER_PTR;
#else
  typedef boost::shared_ptr<FvtxArm> FVTXARM_PTR;
  typedef boost::shared_ptr<TGeoManager> TGEOMANAGER_PTR;
#endif

  //!@name Geometry IOs
  //@{

public:

  //! load geometry configuration based on TFvtxDatabaseCntrl settings
  // return true is loaded as expected, false if geometry is already loaded
  static bool
  load_geometry();

  //! load Root geometry representation from file
  static void
  load_root_geometry();

  //! save current configuration to file
  static int
  save_root_geometry(const std::string& file = "fvtxgeom_new.root");

  //! bank id to save/load geometry in calibration database
  enum enu_band_id
  {
    //! default bank id for production
    BANK_ID_RUN = 1,

    //! default bank id for simulation
    BANK_ID_SIM = 101,

    //! default bank id for tests. Not used by any analysis by default
    BANK_ID_TEST = 201,

    //! dummy to flag max band ID
    BANK_ID_MAX = 1000
  };

  //! bank id category
  //! The final band id number is enu_band_id + enu_band_type.
  //! Therefore, enu_band_type's interval should be at least BANK_ID_MAX
  enum enu_band_type
  {
    //! offset for TGeo format of database data
    BANK_ID_TYPE_TGEO = 0,

    //! offset for numerical format of database data
    BANK_ID_TYPE_NUMBERIC = BANK_ID_MAX * 1
  };

  static
  enu_band_type
  bank_id_type_TGeo()
  {
    return BANK_ID_TYPE_TGEO;
  }

  static
  enu_band_type
  bank_id_type_numberic()
  {
    return BANK_ID_TYPE_NUMBERIC;
  }

  //! show how enu_band_type and enu_band_id should be used
  static inline int
  make_band_id(enu_band_id id, enu_band_type type)
  {
    return type + id;
  }

  //! read geometry representation (_fGeom) from database
  static int
  load_pdb_geometry(int runnumber, bool is_sim = false, enu_band_type type =
      BANK_ID_TYPE_NUMBERIC);

  //! read geometry representation (_fGeom) from database
  static int
  load_pdb_geometry(PHTimeStamp tsearch, bool is_sim = false,
      enu_band_type type = BANK_ID_TYPE_NUMBERIC)
  {
    int ret = 0;

    if (is_sim)
      { // for simulations
        ret += load_pdb_geometry(tsearch, BANK_ID_SIM, type);
      }
    else
      { // for production

        ret += load_pdb_geometry(tsearch, BANK_ID_RUN, type);

      }

    return ret;
  }

  //! read geometry representation (_fGeom) from database for specific band_id
  static int
  load_pdb_geometry(PHTimeStamp tsearch, enu_band_id band_id,
      enu_band_type type = BANK_ID_TYPE_NUMBERIC);

  //! write geometry representation (_fGeom) to database in specific band_id
  static int
  save_pdb_geometry(int beginrun, int endrun, PHString descriptor, //
      enu_band_id band_id = BANK_ID_RUN, enu_band_type type =
          BANK_ID_TYPE_NUMBERIC);

  //! write geometry representation (_fGeom) to database in specific band_id
  static int
  save_pdb_geometry(PHTimeStamp start, PHTimeStamp stop, PHString descriptor, //
      enu_band_id band_id = BANK_ID_RUN, enu_band_type type =
          BANK_ID_TYPE_NUMBERIC);

  //! GeoData_t -> FvtxGeomObject
  static void
  apply_geometry_data(const FvtxGeomObject::GeoData_t & data, std::string description = "Numeric array");

  //! FvtxGeomObject -> GeoData_t
  static void
  save_geometry_data(FvtxGeomObject::GeoData_t & data);

  static const std::string
  set_public_file_path(std::string path);

private:

  //! database table name
  static const PHString calibname;

  //! database class name
  static const PHString classname;

  //! database class name
  static const PHString classname_numeric;

  //! is reorganized geom
  static bool _isReorg;

  //!@}

  //!@name accessors
  //@{

public:
  //! retrieve arm from index
  static FvtxArm*
  get_arm(unsigned int index);

  //! retrieve pointer to north arm
  static FvtxArm*
  north_arm(void)
  {
    return get_arm(FVTXGEOM::North);
  }

  //! retrieve pointer to south arm
  static FvtxArm*
  south_arm(void)
  {
    return get_arm(FVTXGEOM::South);
  }

  //! verbosity
  static const FVTXGEOM::Verbosity&
  get_verbosity(void)
  {
    return _verbosity;
  }

  //! verbosity
  static void
  set_verbosity(const FVTXGEOM::Verbosity& verbosity)
  {
    _verbosity = verbosity;
  }

  //! retrieve cage TGeoPhyscialNode from index
  static TGeoPhysicalNode*
  get_phy_cage(unsigned int arm, unsigned int cage);

  //! retrieve station TGeoPhyscialNode from index
  static TGeoPhysicalNode*
  get_phy_station(unsigned int arm, unsigned int cage, unsigned int station);

  //! retrieve sector TGeoPhyscialNode from index
  static TGeoPhysicalNode*
  get_phy_sector(unsigned int arm, unsigned int cage, unsigned int station,
      unsigned int sector);

  //! retrieve column TGeoPhyscialNode from index
  static TGeoPhysicalNode*
  get_phy_column(unsigned int arm, unsigned int cage, unsigned int station,
      unsigned int sector, unsigned int column);

  //! retrieve node
  static TGeoNode*
  get_node_hall()
  {
    return _node_hall;
  }
  static TGeoNode*
  get_node_sien()
  {
    return _node_sien;
  }
  static TGeoNode*
  get_node_sicg()
  {
    return _node_sicg;
  }
  static TGeoNode*
  get_node_arm(unsigned int arm)
  {
    return _node_arm[arm];
  }
  static TGeoNode*
  get_node_cage(unsigned int arm, unsigned int cage)
  {
    return _node_cage[arm][cage];
  }
  static TGeoNode*
  get_node_station(unsigned int arm, unsigned int cage, unsigned int station)
  {
    return _node_station[arm][cage][station];
  }
  static TGeoNode*
  get_node_sector(unsigned int arm, unsigned int cage, unsigned int station,
      unsigned int sector)
  {
    return _node_sector[arm][cage][station][sector];
  }

  // Feb 2012, Jin Huang <jhuang@bnl.gov>
  // Redesign the mappings
//    //! pointer to south arm DCM channels
//    /*! the map is initialized at first call */
//    static FvtxDCMChannelMap* south_dcm_map();
//
//    //! pointer to north arm DCM channels
//    /*! the map is initialized at first call */
//    static FvtxDCMChannelMap* north_dcm_map();
  static FvtxDCMChannelMap*
  get_dcm_map();

  //! return number of strips on a given column on station
  /*! 5*128 strips on station1. All others have 13*128 */
  static unsigned int
  get_number_of_strip(const unsigned int& station_id)
  {
    if (station_id == FVTXGEOM::Station1)
      return FVTXGEOM::NumberOfStripsSt1;
    else
      return FVTXGEOM::NumberOfStripsSt2;
  }

  //! print
  static void
  print(std::ostream& out = std::cout)
  {
    FVTXGEOM::PRINT(out, std::string("FvtxGeom::print"));
    for (std::vector<FVTXARM_PTR>::const_iterator iter = _arms.begin();
        iter != _arms.end(); iter++)
      if (*iter)
        (*iter)->print(out);
    FVTXGEOM::PRINT(out, "**");
  }

  //! geometry description
  static PHString
  get_geom_description()
  {
    return _geom_description;
  }

  //! geometry description
  static void
  set_geom_description(PHString d);

  //!@}

  //!@name Geometry init
  //@{

  //! init geometry base on _fGeom (_fGeom -> nodes)
  static void
  init_TGeo_geometry();

  //! init geometry base on _fGeom (_fGeom -> nodes) after reorganization
  static void
  init_TGeo_geometry_reorg();

  //! make physical nodes (nodes -> physics nodes)
  static void
  make_phy_nodes();

  //! make physical nodes (nodes -> physics nodes) after reorganization
  static void
  make_phy_nodes_reorg();

  //! set physical nodes apply to all FVTX geom objects
  static void
  set_phy_nodes();

  //! set physical nodes apply to all FVTX geom objects after reorganization
  static void
  set_phy_nodes_reorg();

  // refresh all phys nodes after alignment
  static void
  refresh_geometry(void);

  //! arm 'local' initialization
  static void
  create_arms(void);

  //! strip width
  static const double
  get_strip_width(void)
  {
    return _strip_width;
  }

  //! check for reorganization
  static bool CheckForReorg(void);


  //! \brief Hot fix for TGeoManager's problem of not supporting multiple occurence
  //! restore _fGeom to TGeoManager at constructor, and recover it at the end
  class UseFvtxGeomManager
  {
  public:

    UseFvtxGeomManager();
    virtual
    ~UseFvtxGeomManager();

  private:
    TGeoManager * _g_old;
  };


public:
  //! geometry loaded
  static bool
  is_geometry_loaded()
  {
    if (_geometry_loaded)
      return true;
    else
      return false;
  }

private:

  //! assign arm
  static void
  set_arm(const unsigned int& index, FvtxArm* arm)
  {
    if (_arms.size() <= index)
      throw std::runtime_error("FvtxGeom::set_arm - invalid index");
    if (_arms[index])
      throw std::runtime_error("FvtxGeom::set_arm - already initialized");
    _arms[index] = FVTXARM_PTR(arm);
  }

  //! create cages in given arm
  static void
  create_cages(FvtxArm*);

  //! create stations in given cage
  static void
  create_stations(FvtxCage*);

  //! create sectors in given station
  static void
  create_sectors(FvtxStation*);

  //! create columns in given sector
  static void
  create_columns(FvtxSector*);

  //! create strips in given column
  static void
  create_strips(FvtxColumn*);

  //!@}

  //! arm static pointers
  static std::vector<FVTXARM_PTR> _arms;

  //! verbosity
  static FVTXGEOM::Verbosity _verbosity;

  //! Geom manager. Not used directly, but placed in a shared pointer for destruction
  //  at program end
  static TGEOMANAGER_PTR _fGeom;

  static PHString _geom_description;

  static bool _geometry_loaded;

  //! physical node to identify each unique arm; [arm]
  static TGeoPhysicalNode* _phy_arm[2];

  //! physical node to identify each unique cage; [arm][cage]
  static TGeoPhysicalNode* _phy_cage[2][2];

  //! physical node to identify each unique station; [arm][cage][station]
  static TGeoPhysicalNode* _phy_station[2][2][4];

  //! physical node to identify each unique sector; [arm][cage][station][sector]
  static TGeoPhysicalNode* _phy_sector[2][2][4][24];

  //! physical node to identify each unique column; [arm][cage][station][sector][column]
  static TGeoPhysicalNode* _phy_column[2][2][4][24][2];

  //! TGeoNode
  static TGeoNode* _node_hall;
  static TGeoNode* _node_sien;
  static TGeoNode* _node_sicg;
  static TGeoNode* _node_arm[2];
  static TGeoNode* _node_cage[2][2];
  static TGeoNode* _node_station[2][2][4];
  static TGeoNode* _node_sector[2][2][4][24];

  // Feb 2012, Jin Huang <jhuang@bnl.gov>
  // Redesign the mappings
  static FvtxDCMChannelMap* _dcm_map;
//
//    //! south arm DCM map
//    static FvtxDCMChannelMap* _south_dcm_map;
//
//    //! north arm DCM map
//    static FvtxDCMChannelMap* _north_dcm_map;

  //!@name prefect geometry definition
  //@{

#ifndef __CINT__

  //! strip width [cm]
  static double _strip_width;

#endif
  //@}

//  static std::string _public_file_path;

ClassDef(FvtxGeom,1)

};

#endif
