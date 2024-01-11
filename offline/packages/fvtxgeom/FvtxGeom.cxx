// $Id: FvtxGeom.cxx,v 1.59 2015/08/31 14:10:30 snowball Exp $

/*!
 \file FvtxGeom.cxx
 \brief FVTX geometry manager singleton.
 \Initialize and provide access to any FVTX single pad geometry.
 \author Hugo Pereira da costa
 \version $Revision: 1.59 $
 \date		$Date: 2015/08/31 14:10:30 $
 */

#include <boost/array.hpp>
#include <boost/scoped_ptr.hpp>
#include <fstream>
#include <iostream>
#include <ctime>
#include <stdexcept>

#include <TROOT.h>
#include <TFile.h>
#include <TGeoManager.h>
#include <TSystem.h>

#include <PHCompositeNode.h>

#include <RunToTime.hh>
#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbFvtxAlignment.hh>
#include <PdbFvtxAlignmentNumeric.hh>

#include <recoConsts.h>
#include <RunToTime.hh>
#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbFvtxAlignment.hh>
#include "TFvtxGlobalParCntrl.h"

#include "FvtxGeom.h"

using namespace std;

ClassImp(FvtxGeom)

//___________________________________________________________
vector<FvtxGeom::FVTXARM_PTR> FvtxGeom::_arms(FVTXGEOM::NumberOfArms,
    FvtxGeom::FVTXARM_PTR(static_cast<FvtxArm *>(NULL)));
FVTXGEOM::Verbosity FvtxGeom::_verbosity = FVTXGEOM::NONE;
bool FvtxGeom::_geometry_loaded = false;
TGeoPhysicalNode* FvtxGeom::_phy_arm[2];
TGeoPhysicalNode* FvtxGeom::_phy_cage[2][2];
TGeoPhysicalNode* FvtxGeom::_phy_station[2][2][4];
TGeoPhysicalNode* FvtxGeom::_phy_sector[2][2][4][24];
TGeoPhysicalNode* FvtxGeom::_phy_column[2][2][4][24][2];
TGeoNode* FvtxGeom::_node_hall;
TGeoNode* FvtxGeom::_node_sien;
TGeoNode* FvtxGeom::_node_sicg;
TGeoNode* FvtxGeom::_node_arm[2];
TGeoNode* FvtxGeom::_node_cage[2][2];
TGeoNode* FvtxGeom::_node_station[2][2][4];
TGeoNode* FvtxGeom::_node_sector[2][2][4][24];
bool FvtxGeom::_isReorg = false;

// Feb 2012, Jin Huang <jhuang@bnl.gov>
// Redesign the mappings
FvtxDCMChannelMap* FvtxGeom::_dcm_map = NULL;
//FvtxDCMChannelMap* FvtxGeom::_south_dcm_map;
//FvtxDCMChannelMap* FvtxGeom::_north_dcm_map;

FvtxGeom::TGEOMANAGER_PTR FvtxGeom::_fGeom(static_cast<TGeoManager *>(NULL));
PHString FvtxGeom::_geom_description("NOT initalized");

//___________________________________________________________
/*
 geometry descrition.
 This is the minimal set of parameters needed to describe the fvtx spectrometer needed on top
 of what's read in the root geometry file
 */

// strip width [cm]
double FvtxGeom::_strip_width = 0.0075;
// set to the public area while running FVTX
//std::string FvtxGeom::_public_file_path = std::string(
//    "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/simulation/run12/");

//___________________________________________________________
const PHString FvtxGeom::calibname = "calib.fvtx.alignment";

//___________________________________________________________
const PHString FvtxGeom::classname = "PdbFvtxAlignmentBank";

//___________________________________________________________
const PHString FvtxGeom::classname_numeric = "PdbFvtxAlignmentNumericBank";


void
FvtxGeom::
set_geom_description(PHString d)
{
  _geom_description = d;
  if (_fGeom.get())
    {
      _fGeom->SetTitle(d.getString());
    }
  return;
}

//___________________________________________________________
bool
FvtxGeom::load_geometry()
{


  FVTXGEOM::PRINT(cout, "FvtxGeom::load_geometry");

  TFvtxGlobalParCntrl::init_run();

  // check if arms are already created
  if (is_geometry_loaded())
    {
      cout
          << "FvtxGeom::load_geometry - WARNING - arms are already created. load_geometry canceled."
          << endl;
      return false;
    }

  if (TFvtxGlobalParCntrl::get_bool_par("geom_auto_load"))
    {
      if (TFvtxGlobalParCntrl::get_bool_par("geom_use_calibration_database"))
        {
          cout
              << "FvtxGeom::load_geometry - will download geometry from calibration database."
              << endl;

          const bool is_sim = TFvtxGlobalParCntrl::get_bool_par("is_sim");
          if (is_sim)
            {
              cout << "TFvtxDeadMap::init_run - load geometry for simulation."
                  << endl;
            }
          else
            {
              cout << "TFvtxDeadMap::init_run - load geometry for production."
                  << endl;
            }

          if (TFvtxGlobalParCntrl::get_bool_par(
              "geom_calibration_database_TGeo_format"))
            {
              cout
                  << "TFvtxDeadMap::init_run - use TGeo format of calibration database. This may cause error in newer version of ROOT > v.4.30"
                  << endl;
              load_pdb_geometry(TFvtxGlobalParCntrl::get_pdb_time(), is_sim,
                  BANK_ID_TYPE_TGEO);
              init_TGeo_geometry();
            }
          else
            {
              cout
                  << "TFvtxDeadMap::init_run - use numeric format of calibration database. "
                  << endl;
              load_pdb_geometry(TFvtxGlobalParCntrl::get_pdb_time(), is_sim,
                  BANK_ID_TYPE_NUMBERIC);
            }

        } //       if (TFvtxGlobalParCntrl::get_bool_par("geom_use_calibration_database"))
      else
        {
          cout << "FvtxGeom::load_geometry - will load geometry from root files"
              << endl;

          load_root_geometry();
	  _isReorg = CheckForReorg();
	  if( _isReorg ) init_TGeo_geometry_reorg();
	  else init_TGeo_geometry();
	  //init_TGeo_geometry();
        }
    } //  if (TFvtxGlobalParCntrl::get_bool_par("geom_auto_load"))
  else
    {

      cout
          << "FvtxGeom::load_geometry - I am configured not to automatically load geometry according to TFvtxGlobalParCntrl::get_bool_par(\"geom_auto_load\")" << endl;

    }

  FVTXGEOM::PRINT(cout, "**");

  return true;
}

//___________________________________________________________
void
FvtxGeom::init_TGeo_geometry()
{

  if (!_fGeom.get())
    {
      std::cerr
          << "FvtxGeom::init_geometry - FVTX geometry was not loaded. Complain loudly..."
          << std::endl;
      throw runtime_error(
          "FvtxGeom::init_geometry - FVTX geometry was not loaded");
    }

  // preserve gGeoManager, which will be overwritten during loading
  UseFvtxGeomManager _use;

  _geom_description = _fGeom->GetTitle();

  TGeoVolume *vol_hall = _fGeom->GetVolume("HALL");
  if (!vol_hall)
    {
      std::cout
          << "FvtxGeom::load_root_geometry: ERROR: Failed to find HALL volume in "
          << _fGeom->GetName() << ", exiting ... " << std::endl;
      exit(1);
    }

  if (_verbosity >= FVTXGEOM::ALOT)
    {
      std::cout << "HALL's daughters are: " << std::endl;
      for (int i = 0; i < vol_hall->GetNdaughters(); i++)
        std::cout << i << "  " << vol_hall->GetNode(i)->GetName() << std::endl;
    }

  _node_hall = _fGeom->GetTopNode();
  if (!_node_hall)
    {
      std::cout
          << "FvtxGeom::load_root_geometry: ERROR: Failed to get top node from HALL volume"
          << ", exiting ... " << std::endl;
      exit(1);
    }

  _node_sien = vol_hall->GetNode("SIEN_1");
  if (!_node_sien)
    {
      std::cout
          << "FvtxGeom::load_root_geometry: ERROR: Failed to get node SIEN_1 from "
          << vol_hall->GetName() << ", exiting ... " << std::endl;
    }

  _node_sicg = _node_sien->GetVolume()->GetNode("SICG_1");
  if (!_node_sicg)
    {
      std::cout
          << "FvtxGeom::load_root_geometry: ERROR: Failed to get node SICG_1 from volume "
          << _node_sien->GetName() << ", exiting ... " << std::endl;
      exit(1);
    }

  TGeoVolume *vol_sicg = _node_sicg->GetVolume();
  if (!vol_sicg)
    {
      std::cout
          << "FvtxGeom::load_root_geometry: ERROR: Failed to get volume from node "
          << _node_sicg->GetName() << std::endl;
      std::cout << "Exiting..." << std::endl;
      exit(1);
    }

  //for( unsigned int i_vol = 0; i_vol < volSICG->GetNode("SI09_1")->GetVolume()->GetNode("SIPS_1")->GetVolume()->GetNdaughters(); i_vol++ ) 
  //  cout << "*** config " << volSICG->GetNode("SI09_1")->GetVolume()->GetNode("SIPS_1")->GetVolume()->GetNode(i_vol)->GetName() << endl;

  for (unsigned int arm_id = 0; arm_id < FVTXGEOM::NumberOfArms; arm_id++)
    {
      // /HALL/SIEN_1/SICG_1/SCMN (SCMS)
      TString name_arm("SCM");
      if (arm_id == 0)
        name_arm += "S";
      else if (arm_id == 1)
        name_arm += "N";
      name_arm += "_1";
      _node_arm[arm_id] = vol_sicg->GetNode(name_arm);
      TGeoVolume *vol_arm = _node_arm[arm_id]->GetVolume();

      for (unsigned int cage_id = 0; cage_id < FVTXGEOM::NumberOfCages;
          cage_id++)
        {
          // /HALL/SIEN_1/SICG_1/SCMN/SCM1
          unsigned int cage_no = arm_id * FVTXGEOM::NumberOfCages + cage_id + 1;
          TString name_cage("SCM");
          name_cage += cage_no;
          name_cage += "_1";
          _node_cage[arm_id][cage_id] = vol_arm->GetNode(name_cage);
          TGeoVolume *vol_cage = _node_cage[arm_id][cage_id]->GetVolume();

          for (unsigned int station_id = 0;
              station_id < FVTXGEOM::NumberOfStations; station_id++)
            {
              // /HALL/SIEN_1/SICG_1/SCMN/SCM1/SI05
              unsigned int station_no = 4;
              if (arm_id == 0)
                station_no += (3 - station_id) * FVTXGEOM::NumberOfCages
                    + cage_id + 1;
              else if (arm_id == 1)
                station_no += (4 + station_id) * FVTXGEOM::NumberOfCages
                    + cage_id + 1;
              TString name_station("SI");
              if (station_no < 10)
                name_station += "0";
              name_station += station_no;
              name_station += "_1";
              _node_station[arm_id][cage_id][station_id] = vol_cage->GetNode(
                  name_station);
              if (!_node_station[arm_id][cage_id][station_id])
                {
                  std::cout << name_station << " Node arm " << arm_id
                      << " cage " << cage_id << " station " << station_id
                      << " not found" << std::endl;
                  std::cout << "Exiting ... " << std::endl;
                  exit(1);
                }
              TGeoVolume *vol_station =
                  _node_station[arm_id][cage_id][station_id]->GetVolume();

              for (unsigned int sector_id = 0;
                  sector_id < FVTXGEOM::NumberOfSectors; sector_id++)
                {
                  // /HALL/SIEN_1/SICG_1/SCMN/SCM1/SI05/SIPB (SIPS)
                  TString name_sector("");
                  if (station_id > 0)
                    name_sector += "SIPB";
                  if (station_id == 0)
                    name_sector += "SIPS";
                  name_sector += "_";
                  //Remove sector numbering inversion for South Arm, which is incorrect for real data, 5/4/12
                  //if (arm_id == 0) name_sector += (FVTXGEOM::NumberOfSectors-sector_id);
                  //else if (arm_id == 1) name_sector += (sector_id+1);
                  name_sector += (sector_id + 1);
                  if (_verbosity >= FVTXGEOM::MAX)
                    std::cout << "name_sector " << name_sector << std::endl;

                  _node_sector[arm_id][cage_id][station_id][sector_id] =
                      vol_station->GetNode(name_sector);
                  if (!_node_sector[arm_id][cage_id][station_id][sector_id])
                    {
                      std::cout << name_sector << " Node arm " << arm_id
                          << " cage " << cage_id << " station " << station_id
                          << " sector " << sector_id << " not found"
                          << std::endl;
                      std::cout << "Exiting... " << std::endl;
                      exit(1);
                    }
                  //TGeoVolume *vol_sector = _node_sector[arm_id][cage_id][station_id][sector_id]->GetVolume();
                }
            }
        }
    }

  if (_verbosity >= FVTXGEOM::SOME)
    std::cout << "end of geometry load " << std::endl;

  make_phy_nodes();
  set_phy_nodes();
}



//___________________________________________________________
void
FvtxGeom::init_TGeo_geometry_reorg()
{


  if (!_fGeom.get())
    {
      std::cerr
          << "FvtxGeom::init_geometry - FVTX geometry was not loaded. Complain loudly..."
          << std::endl;
      throw runtime_error(
          "FvtxGeom::init_geometry - FVTX geometry was not loaded");
    }

  // preserve gGeoManager, which will be overwritten during loading
  UseFvtxGeomManager _use;

  _geom_description = _fGeom->GetTitle();

  TGeoVolume *vol_hall = _fGeom->GetVolume("HALL");
  if (!vol_hall)
    {
      std::cout
          << "FvtxGeom::load_root_geometry: ERROR: Failed to find HALL volume in "
          << _fGeom->GetName() << ", exiting ... " << std::endl;
      exit(1);
    }

  if (_verbosity >= FVTXGEOM::ALOT)
    {
      std::cout << "HALL's daughters are: " << std::endl;
      for (int i = 0; i < vol_hall->GetNdaughters(); i++)
        std::cout << i << "  " << vol_hall->GetNode(i)->GetName() << std::endl;
    }

  _node_hall = _fGeom->GetTopNode();
  if (!_node_hall)
    {
      std::cout
          << "FvtxGeom::load_root_geometry: ERROR: Failed to get top node from HALL volume"
          << ", exiting ... " << std::endl;
      exit(1);
    }

  _node_sien = vol_hall->GetNode("SIEN_1");
  if (!_node_sien)
    {
      std::cout
          << "FvtxGeom::load_root_geometry: ERROR: Failed to get node SIEN_1 from "
          << vol_hall->GetName() << ", exiting ... " << std::endl;
    }

  _node_sicg = _node_sien->GetVolume()->GetNode("SICG_1");
  if (!_node_sicg)
    {
      std::cout
          << "FvtxGeom::load_root_geometry: ERROR: Failed to get node SICG_1 from volume "
          << _node_sien->GetName() << ", exiting ... " << std::endl;
      exit(1);
    }

  TGeoVolume *vol_sicg = _node_sicg->GetVolume();
  if (!vol_sicg)
    {
      std::cout
          << "FvtxGeom::load_root_geometry: ERROR: Failed to get volume from node "
          << _node_sicg->GetName() << std::endl;
      std::cout << "Exiting..." << std::endl;
      exit(1);
    }

  //for( unsigned int i_vol = 0; i_vol < volSICG->GetNode("SI09_1")->GetVolume()->GetNode("SIPS_1")->GetVolume()->GetNdaughters(); i_vol++ ) 
  //  cout << "*** config " << volSICG->GetNode("SI09_1")->GetVolume()->GetNode("SIPS_1")->GetVolume()->GetNode(i_vol)->GetName() << endl;

  for (unsigned int arm_id = 0; arm_id < FVTXGEOM::NumberOfArms; arm_id++)
    {
      // /HALL/SIEN_1/SICG_1/SCMN (SCMS)
      TString name_arm("SCM");
      if (arm_id == 0)
        name_arm += "S";
      else if (arm_id == 1)
        name_arm += "N";
      name_arm += "_1";
      _node_arm[arm_id] = vol_sicg->GetNode(name_arm);
      TGeoVolume *vol_arm = _node_arm[arm_id]->GetVolume();

      for (unsigned int cage_id = 0; cage_id < FVTXGEOM::NumberOfCages;
          cage_id++)
        {
          // /HALL/SIEN_1/SICG_1/SCMN/<now gone>SCM1</now gone>
          unsigned int cage_no = arm_id * FVTXGEOM::NumberOfCages + cage_id + 1;
          TString name_cage("SCM");
          name_cage += cage_no;
          name_cage += "_1";
          //_node_cage[arm_id][cage_id] = vol_arm->GetNode(name_cage);
          //TGeoVolume *vol_cage = _node_cage[arm_id][cage_id]->GetVolume();

          for (unsigned int station_id = 0;
              station_id < FVTXGEOM::NumberOfStations; station_id++)
            {
              // /HALL/SIEN_1/SICG_1/SCMN/SCM1/SI05
              unsigned int station_no = 4;
              if (arm_id == 0)
                station_no += (3 - station_id) * FVTXGEOM::NumberOfCages
                    + cage_id + 1;
              else if (arm_id == 1)
                station_no += (4 + station_id) * FVTXGEOM::NumberOfCages
                    + cage_id + 1;
              TString name_station("SI");
              if (station_no < 10)
                name_station += "0";
              name_station += station_no;
              name_station += "_1";
              _node_station[arm_id][cage_id][station_id] = vol_arm->GetNode(
                  name_station);
              if (!_node_station[arm_id][cage_id][station_id])
                {
                  std::cout << name_station << " Node arm " << arm_id
                      << " cage " << cage_id << " station " << station_id
                      << " not found" << std::endl;
                  std::cout << "Exiting ... " << std::endl;
                  exit(1);
                }
              TGeoVolume *vol_station =
                  _node_station[arm_id][cage_id][station_id]->GetVolume();

              for (unsigned int sector_id = 0;
                  sector_id < FVTXGEOM::NumberOfSectors; sector_id++)
                {
                  // /HALL/SIEN_1/SICG_1/SCMN/SCM1/SI05/SIPB (SIPS)
                  TString name_sector("");
                  if (station_id > 0)
                    name_sector += "SIPB";
                  if (station_id == 0)
                    name_sector += "SIPS";
                  name_sector += "_";
                  //Remove sector numbering inversion for South Arm, which is incorrect for real data, 5/4/12
                  //if (arm_id == 0) name_sector += (FVTXGEOM::NumberOfSectors-sector_id);
                  //else if (arm_id == 1) name_sector += (sector_id+1);
                  name_sector += (sector_id + 1);
                  if (_verbosity >= FVTXGEOM::MAX)
                    std::cout << "name_sector " << name_sector << std::endl;

                  _node_sector[arm_id][cage_id][station_id][sector_id] =
                      vol_station->GetNode(name_sector);
                  if (!_node_sector[arm_id][cage_id][station_id][sector_id])
                    {
                      std::cout << name_sector << " Node arm " << arm_id
                          << " cage " << cage_id << " station " << station_id
                          << " sector " << sector_id << " not found"
                          << std::endl;
                      std::cout << "Exiting... " << std::endl;
                      exit(1);
                    }
                  //TGeoVolume *vol_sector = _node_sector[arm_id][cage_id][station_id][sector_id]->GetVolume();
                }
            }
        }
    }

  if (_verbosity >= FVTXGEOM::SOME)
    std::cout << "end of geometry load " << std::endl;

  make_phy_nodes_reorg();
  set_phy_nodes();
}








//___________________________________________________________
void
FvtxGeom::load_root_geometry()
{

  FVTXGEOM::PRINT(cout, "FvtxGeom::load_root_geometry");

  const std::string file = TFvtxGlobalParCntrl::get_string_par(
      "geom_root_file_name");

  std::string file_name ;

  // local file is not available, try to find file in public area
  file_name = TFvtxGlobalParCntrl::get_string_par("geom_root_file_path");
  file_name.append(file);
  bool public_file = gSystem->AccessPathName(file_name.c_str());
  if (public_file)
    {
      // file not found in public area, exit
      std::ostringstream s;
      s << "Failed to open file " << file_name;
      std::cout << "FvtxGeom::load_root_geometry: WARNING: " << s.str()
          << ", bailing out ..." << std::endl;
      std::cout << "Please locate this file and try again." << std::endl;
      exit(1);
    }

  TFile fFile(file_name.c_str());
  if (!fFile.IsOpen())
    {
      std::cout << "fvtxgeom file " << file_name << " not opened." << std::endl;
      exit(1);
    }
  else
    {
      /*if( _verbosity >= FVTXGEOM::SOME )*/
      std::cout << file_name << " opened " << std::endl;
    }

  // preserve gGeoManager, which will be overwritten during loading
  TGeoManager * g_old = gGeoManager;
  gGeoManager = NULL;

  //TGeoManager *fGeom = (TGeoManager*)fFile.Get("PHENIX");
  _fGeom = TGEOMANAGER_PTR((TGeoManager*) fFile.Get("PHENIX"));

  gGeoManager = g_old; // recover gGeoManager
  if (!_fGeom)
    {
      std::cout
          << "FvtxGeom::load_root_geometry: ERROR: Failed to find PHENIX TGeoManager in "
          << fFile.GetName() << ", exiting ... " << std::endl;
      throw std::runtime_error("Failed to find PHENIX TGeoManager");
    }
  else
    {
      std::cout << "FvtxGeom::load_root_geometry: FVTX Geometry Loaded from "
          << file_name << " with description" << std::endl;
      std::cout << std::endl << _fGeom->GetTitle() << std::endl << std::endl;
    }
  //_fGeom->SetVisLevel(6);

}

//___________________________________________________________
int
FvtxGeom::save_root_geometry(const std::string& file)
{
  if (!_fGeom.get())
    {
      std::cerr
          << "FvtxGeom::save_root_geometry - FVTX TGeo geometry was not loaded. No action taken."
          << std::endl;

      return 0;
    }

  std::cerr << "FvtxGeom::save_root_geometry - Saving FVTX geometry to " << file
      << std::endl;

  return _fGeom->Export(file.c_str(), "PHENIX");
}

//___________________________________________________________
int
FvtxGeom::load_pdb_geometry(int runnumber, bool is_sim, enu_band_type type)
{
  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(runnumber));
  PHTimeStamp Tstart = *ts;
  delete ts;

  return load_pdb_geometry(Tstart, is_sim, type);
}

//___________________________________________________________
int
FvtxGeom::load_pdb_geometry(PHTimeStamp tsearch, FvtxGeom::enu_band_id band_id,
    enu_band_type type)
{

  // start out fresh
  // Access the database, pull data into the internal structure
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  PdbBankID bankid((int) make_band_id(band_id, type));
  const PHString class_n =
      type == BANK_ID_TYPE_NUMBERIC ? classname_numeric : classname;

  int ret = 0;

  // Open the Objectivity database for reading and pull all the values for
  // a given bank (i.e. a strip) and put it into our set
  if (application->startRead())
    {
      // preserve gGeoManager, which will be overwritten during loading
      TGeoManager * g_old = gGeoManager;
      gGeoManager = NULL;

      PdbCalBank *fvtxBank = bankManager->fetchBank(class_n.getString(), bankid,
          calibname.getString(), tsearch);

      if (fvtxBank)
        {

          //          printf("FvtxGeom::load_pdb_geometry - start validity : %s (%u)\n", fvtxBank->getStartValTime().formatTimeString(), (unsigned int)fvtxBank->getStartValTime().getTics());
          //          printf("FvtxGeom::load_pdb_geometry - end validity   : %s (%u)\n", fvtxBank->getEndValTime().formatTimeString(), (unsigned int)fvtxBank->getEndValTime().getTics());
          //          printf("FvtxGeom::load_pdb_geometry - insertion      : %s (%u)\n", fvtxBank->getInsertTime().formatTimeString(), (unsigned int) fvtxBank->getInsertTime().getTics());
          //          printf("FvtxGeom::load_pdb_geometry - description    : %s\n", fvtxBank->getDescription().getString());

          //fvtxBank->print();
          int length = fvtxBank->getLength();

          if (length == 1)
            {
              if (type == BANK_ID_TYPE_NUMBERIC)
                {
                  cout
                      << "FvtxGeom::load_pdb_geometry load numeric type of geometry database. "
                      << endl;

                  PdbFvtxAlignmentNumeric & dba =
                      (PdbFvtxAlignmentNumeric &) fvtxBank->getEntry(0);

                  apply_geometry_data(dba.get_data(), fvtxBank->getDescription().getString());
                }
              else
                {
                  cout
                      << "FvtxGeom::load_pdb_geometry load TGeo type of geometry database. "
                      << "This is obsolete, since it has known version issue with upgrade of ROOT"
                      << endl;

                  PdbFvtxAlignment & dba =
                      (PdbFvtxAlignment &) fvtxBank->getEntry(0);

                  if (dba.GetGeometry())
                    {
                      _fGeom = TGEOMANAGER_PTR(dba.GetGeometry());

                      ret++;

                    }

                }

              cout
                  << "FvtxGeom::load_pdb_geometry sucessfully loaded fvtxgeometry from band_id = "
                  << band_id <<", type = "<<type<< " with description "
                  << fvtxBank->getDescription() << endl;
            }
          else
            {

              cout << "FvtxGeom::load_pdb_geometry invalid FVTX geometry band"
                  << ", # of geometry object = " << length << ", band_id = "
                  << band_id <<", type = "<<type<< ", description = " << fvtxBank->getDescription()
                  << endl;
              throw runtime_error("FvtxGeom::load_pdb_geometry invalid FVTX geometry band");

            }

          delete fvtxBank;

        }
      else
        {
          cout
              << "FvtxGeom::load_pdb_geometry - Error - failed to load geometry on band_id = "
              << band_id <<", type = "<<type<< endl;
          throw runtime_error("FvtxGeom::load_pdb_geometry - Error - failed to load geometry");
        }

      gGeoManager = g_old; // recover gGeoManager

    }
  else
    {

      application->abort();
      cout << ("FvtxGeom::load_pdb_geometry - Error - Transaction aborted.\n");
      throw runtime_error("FvtxGeom::load_pdb_geometry - Error - Transaction aborted.");
    }

  return ret;
}

//___________________________________________________________
int
FvtxGeom::save_pdb_geometry(int beginrun, int endrun, PHString descriptor,
    FvtxGeom::enu_band_id band_id, enu_band_type type)
{
  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(beginrun));

  if (!ts)
    {
      cout <<"FvtxGeom::save_pdb_geometry - ERROR - failed to find time stamp for beginrun "<<beginrun<<endl;
      return 0;
    }

  PHTimeStamp Tstart = *ts;
  delete ts;
  PHTimeStamp Tstop;

  // The runnumber is encoded into PHTimeStamp.
  if (endrun > 0)
    {
      ts = runTime->getEndTime(endrun);
      if (!ts)
        {
          cout <<"FvtxGeom::save_pdb_geometry - ERROR - failed to find time stamp for endrun "<<endrun<<endl;
          return 0;
        }
      Tstop = *ts;
      delete ts;
    }
  else
    {
      Tstop.setToFarFuture();
    }

  return save_pdb_geometry(Tstart, Tstop, descriptor, band_id, type);
}

//___________________________________________________________
int
FvtxGeom::save_pdb_geometry(PHTimeStamp start, PHTimeStamp stop,
    PHString descriptor, FvtxGeom::enu_band_id band_id, enu_band_type type)
{

  PdbBankID bankid(make_band_id(band_id, type));
  const PHString class_n =
      type == BANK_ID_TYPE_NUMBERIC ? classname_numeric : classname;

  descriptor += "(";
  descriptor += _geom_description;
  descriptor += ")";

  cout << "FvtxGeom::save_pdb_geometry - startvaltime: " << start << endl;
  cout << "FvtxGeom::save_pdb_geometry - endvaltime: " << stop << endl;
  cout << "FvtxGeom::save_pdb_geometry - description: " << descriptor << endl;
  cout << "FvtxGeom::save_pdb_geometry - band id: " << (int) band_id << endl;
  cout << "FvtxGeom::save_pdb_geometry - band type: " << (int) type << endl;

  //timestamp values: sanity check
  if (start > stop)
    {
      cout << "FvtxGeom::save_pdb_geometry - invalid start and stop:" << start
          << stop << endl;
      cout << "ignored" << endl;
      return -1;
    }

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (application->startUpdate())
    {

      // preserve gGeoManager, which will be overwritten during loading
      TGeoManager * g_old = gGeoManager;
      gGeoManager = NULL;

      PdbCalBank *fvtxBank = bankManager->createBank(class_n.getString(),
          bankid, descriptor.getString(), start, stop, calibname.getString());

      if (fvtxBank)
        {

          fvtxBank->setLength(1);

          if (type == BANK_ID_TYPE_NUMBERIC)
            {

              if (!_fGeom.get())
                {
                  std::cerr
                      << "FvtxGeom::save_pdb_geometry - FVTX geometry was not loaded. No action taken."
                      << std::endl;

                  return 0;
                }

              cout
                  << "FvtxGeom::save_pdb_geometry - new bank created, uploading the current geometry titled"
                  << _fGeom->GetTitle() << endl;

              PdbFvtxAlignmentNumeric & pdb =
                  dynamic_cast<PdbFvtxAlignmentNumeric &>(fvtxBank->getEntry(0));

              PdbFvtxAlignmentNumeric::GeoData_t & data = pdb.get_data();
              save_geometry_data(data);

              fvtxBank->print();

            } //          if (enu_band_type == BANK_ID_TYPE_NUMBERIC)
          else
            {
              if (!_fGeom.get())
                {
                  std::cerr
                      << "FvtxGeom::save_pdb_geometry - FVTX geometry was not loaded. No action taken."
                      << std::endl;

                  return 0;
                }

              cout
                  << "FvtxGeom::save_pdb_geometry - new bank created, uploading the current geometry titled"
                  << _fGeom->GetTitle() << endl;

              PdbFvtxAlignment & pdb =
                  dynamic_cast<PdbFvtxAlignment &>(fvtxBank->getEntry(0));

              pdb.SetGeometry(_fGeom.get());

              fvtxBank->print();
            }

          // commit new bank to the db and delete
          application->commit(fvtxBank);
          delete fvtxBank;

        }
      else
        {
          cout
              << ("FvtxGeom::save_pdb_geometry - bankManager returned zero-pointer\n");
        }

      gGeoManager = g_old; // recover gGeoManager

    }
  else
    cout
        << "FvtxGeom::save_pdb_geometry - failed to start application for update"
        << endl;

  cout << "FvtxGeom::save_pdb_geometry - done." << endl;
  cout << endl;

  return 0;
}

//___________________________________________________________
//! GeoData_t -> FvtxGeomObject
void
FvtxGeom::apply_geometry_data(const FvtxGeomObject::GeoData_t & data, std::string description)
{
  cout << "FvtxGeom::apply_geometry_data - applying geometry data to FVTX with description: "<<description
      << endl;
  try
    {
      for (unsigned int arm_id = 0; arm_id < FVTXGEOM::NumberOfArms; arm_id++)
        {
          FVTXARM_PTR arm = _arms[arm_id];
          if (!arm.get())
            {
              cout << "FvtxGeom::apply_geometry_data - ERROR - missing arm "
                  << arm_id << endl;
            }
          else
            {
              arm->apply_geometry_data(data);
            }
        }

    }
  catch (std::exception& e)
    {
      cout << "FvtxGeom::apply_geometry_data - ERROR - " << e.what() << endl;

      throw;
    }

  _geom_description = description.c_str();
  _geometry_loaded = true;
}

//___________________________________________________________
//! FvtxGeomObject -> GeoData_t
void
FvtxGeom::save_geometry_data(FvtxGeomObject::GeoData_t & data)
{
  cout << "FvtxGeom::save_geometry_data - extracting FVTX geometry data"
      << endl;

  data.clear();

  try
    {
      for (unsigned int arm_id = 0; arm_id < FVTXGEOM::NumberOfArms; arm_id++)
        {
	  //	  const FVTXARM_PTR::pointer arm(_arms[arm_id].get());
	  const boost::shared_ptr<FvtxArm> arm(_arms[arm_id].get());
          if (!arm)
            {
              cout << "FvtxGeom::save_geometry_data - ERROR - missing arm "
                  << arm_id << endl;
            }
          else
            {
              arm->save_geometry_data(data);
            }
        }
    }
  catch (std::exception& e)
    {
      cout << "FvtxGeom::save_geometry_data - ERROR - " << e.what() << endl;

      throw;
    }
}

//___________________________________________________________
void
FvtxGeom::make_phy_nodes(void)
{

  if (!_fGeom.get())
    {
      std::cerr
          << "FvtxGeom::make_phy_nodes - FVTX geometry was not loaded. Complain loudly..."
          << std::endl;
      throw runtime_error(
          "FvtxGeom::make_phy_nodes - FVTX geometry was not loaded");
    }

  // preserve gGeoManager, which will be overwritten during loading
  UseFvtxGeomManager _use;

  if (_verbosity >= FVTXGEOM::SOME)
    std::cout << "SetPhysicalNode..." << std::endl;

  for (unsigned int arm_id = 0; arm_id < FVTXGEOM::NumberOfArms; arm_id++)
    {
      _phy_arm[arm_id] = _fGeom->MakePhysicalNode(
          TString("/") + _node_hall->GetName() + TString("/")
              + _node_sien->GetName() + TString("/") + _node_sicg->GetName()
              + TString("/") + _node_arm[arm_id]->GetName());

      for (unsigned int cage_id = 0; cage_id < FVTXGEOM::NumberOfCages;
          cage_id++)
        {
          _phy_cage[arm_id][cage_id] = _fGeom->MakePhysicalNode(
              TString("/") + _node_hall->GetName() + TString("/")
                  + _node_sien->GetName() + TString("/") + _node_sicg->GetName()
                  + TString("/") + _node_arm[arm_id]->GetName() + TString("/")
                  + _node_cage[arm_id][cage_id]->GetName());

          for (unsigned int station_id = 0;
              station_id < FVTXGEOM::NumberOfStations; station_id++)
            {
              _phy_station[arm_id][cage_id][station_id] =
                  _fGeom->MakePhysicalNode(
                      TString("/") + _node_hall->GetName() + TString("/")
                          + _node_sien->GetName() + TString("/")
                          + _node_sicg->GetName() + TString("/")
                          + _node_arm[arm_id]->GetName() + TString("/")
                          + _node_cage[arm_id][cage_id]->GetName()
                          + TString("/")
                          + _node_station[arm_id][cage_id][station_id]->GetName());

              for (unsigned int sector_id = 0;
                  sector_id < FVTXGEOM::NumberOfSectors; sector_id++)
                {
                  _phy_sector[arm_id][cage_id][station_id][sector_id] =
                      _fGeom->MakePhysicalNode(
                          TString("/") + _node_hall->GetName() + TString("/")
                              + _node_sien->GetName() + TString("/")
                              + _node_sicg->GetName() + TString("/")
                              + _node_arm[arm_id]->GetName() + TString("/")
                              + _node_cage[arm_id][cage_id]->GetName()
                              + TString("/")
                              + _node_station[arm_id][cage_id][station_id]->GetName()
                              + TString("/")
                              + _node_sector[arm_id][cage_id][station_id][sector_id]->GetName());

                  for (unsigned int column_id = 0;
                      column_id < FVTXGEOM::NumberOfColumns; column_id++)
                    {
                      TString name_sis("");
                      if (station_id > 0)
                        name_sis += "SISB";
                      else if (station_id == 0)
                        name_sis += "SISS";
                      name_sis += "_1";
                      TGeoNode *_node_sis =
                          _node_sector[arm_id][cage_id][station_id][sector_id]->GetVolume()->GetNode(
                              name_sis);

                      TString name_column("SISI_");
                      unsigned int column_no = column_id;

//            if( sector_id % 2 == 0 ) column_no = 1 - column_id; // Jin : no need for swapping

                      if (station_id > 0)
                        name_column += (column_no + 1);
                      else if (station_id == 0)
                        name_column += (column_no + 3);
                      //std::cout << name_column << std::endl;
                      TGeoNode *node_column = _node_sis->GetVolume()->GetNode(
                          name_column);
                      if (!node_column)
                        {
                          std::cout << name_column << " Node arm " << arm_id
                              << " cage " << cage_id << " station "
                              << station_id << " sector " << sector_id
                              << " cloumn " << column_id << " not found"
                              << std::endl;
                          std::cout << "Exiting ... " << std::endl;
                          exit(1);
                        }

                      TGeoPhysicalNode *a_phy_node =
                          _fGeom->MakePhysicalNode(
                              TString("/") + _node_hall->GetName()
                                  + TString("/") + _node_sien->GetName()
                                  + TString("/") + _node_sicg->GetName()
                                  + TString("/") + _node_arm[arm_id]->GetName()
                                  + TString("/")
                                  + _node_cage[arm_id][cage_id]->GetName()
                                  + TString("/")
                                  + _node_station[arm_id][cage_id][station_id]->GetName()
                                  + TString("/")
                                  + _node_sector[arm_id][cage_id][station_id][sector_id]->GetName()
                                  + TString("/") + _node_sis->GetName()
                                  + TString("/") + node_column->GetName());
                      _phy_column[arm_id][cage_id][station_id][sector_id][column_id] =
                          a_phy_node;
                    }
                }
            }
        }
    }

  //gGeoManager->Export("geom_new.root","PHENIX");
  if (_verbosity >= FVTXGEOM::SOME)
    std::cout << "end of set physical node " << std::endl;

}


//___________________________________________________________
void
FvtxGeom::make_phy_nodes_reorg(void)
{

  if (!_fGeom.get())
    {
      std::cerr
          << "FvtxGeom::make_phy_nodes - FVTX geometry was not loaded. Complain loudly..."
          << std::endl;
      throw runtime_error(
          "FvtxGeom::make_phy_nodes - FVTX geometry was not loaded");
    }

  // preserve gGeoManager, which will be overwritten during loading
  UseFvtxGeomManager _use;

  if (_verbosity >= FVTXGEOM::SOME)
    std::cout << "SetPhysicalNode..." << std::endl;

  for (unsigned int arm_id = 0; arm_id < FVTXGEOM::NumberOfArms; arm_id++)
    {
      _phy_arm[arm_id] = _fGeom->MakePhysicalNode(
          TString("/") + _node_hall->GetName() + TString("/")
              + _node_sien->GetName() + TString("/") + _node_sicg->GetName()
              + TString("/") + _node_arm[arm_id]->GetName());

      for (unsigned int cage_id = 0; cage_id < FVTXGEOM::NumberOfCages;
        cage_id++)
        {
	  _phy_cage[arm_id][cage_id] = NULL;
          //_phy_cage[arm_id][cage_id] = _fGeom->MakePhysicalNode(
	  //  TString("/") + _node_hall->GetName() + TString("/")
	  //      + _node_sien->GetName() + TString("/") + _node_sicg->GetName()
	  //      + TString("/") + _node_arm[arm_id]->GetName() + TString("/")
	  //      + _node_cage[arm_id][cage_id]->GetName());

          for (unsigned int station_id = 0;
              station_id < FVTXGEOM::NumberOfStations; station_id++)
            {
              _phy_station[arm_id][cage_id][station_id] =
                  _fGeom->MakePhysicalNode(
                      TString("/") + _node_hall->GetName() + TString("/")
                          + _node_sien->GetName() + TString("/")
		          + _node_sicg->GetName() + TString("/")
                          + _node_arm[arm_id]->GetName() + TString("/")
		          //+ _node_cage[arm_id][cage_id]->GetName()
                          //+ TString("/")
                          + _node_station[arm_id][cage_id][station_id]->GetName());

              for (unsigned int sector_id = 0;
                  sector_id < FVTXGEOM::NumberOfSectors; sector_id++)
                {
                  _phy_sector[arm_id][cage_id][station_id][sector_id] =
                      _fGeom->MakePhysicalNode(
                          TString("/") + _node_hall->GetName() + TString("/")
                              + _node_sien->GetName() + TString("/")
                              + _node_sicg->GetName() + TString("/")
                              + _node_arm[arm_id]->GetName() + TString("/")
			      //+ _node_cage[arm_id][cage_id]->GetName()
                              //+ TString("/")
                              + _node_station[arm_id][cage_id][station_id]->GetName()
                              + TString("/")
                              + _node_sector[arm_id][cage_id][station_id][sector_id]->GetName());

                  for (unsigned int column_id = 0;
                      column_id < FVTXGEOM::NumberOfColumns; column_id++)
                    {
                      TString name_sis("");
                      if (station_id > 0)
                        name_sis += "SISB";
                      else if (station_id == 0)
                        name_sis += "SISS";
                      name_sis += "_1";
                      TGeoNode *_node_sis =
                          _node_sector[arm_id][cage_id][station_id][sector_id]->GetVolume()->GetNode(
                              name_sis);

                      TString name_column("SISI_");
                      unsigned int column_no = column_id;

//            if( sector_id % 2 == 0 ) column_no = 1 - column_id; // Jin : no need for swapping

                      if (station_id > 0)
                        name_column += (column_no + 1);
                      else if (station_id == 0)
                        name_column += (column_no + 3);
                      //std::cout << name_column << std::endl;
                      TGeoNode *node_column = _node_sis->GetVolume()->GetNode(
                          name_column);
                      if (!node_column)
                        {
                          std::cout << name_column << " Node arm " << arm_id
                              << " cage " << cage_id << " station "
                              << station_id << " sector " << sector_id
                              << " cloumn " << column_id << " not found"
                              << std::endl;
                          std::cout << "Exiting ... " << std::endl;
                          exit(1);
                        }

		      TGeoPhysicalNode *a_phy_node =
                          _fGeom->MakePhysicalNode(
                              TString("/") + _node_hall->GetName()
                                  + TString("/") + _node_sien->GetName()
                                  + TString("/") + _node_sicg->GetName()
                                  + TString("/") + _node_arm[arm_id]->GetName()
                                  + TString("/")
			          //+ _node_cage[arm_id][cage_id]->GetName()
                                  //+ TString("/")
                                  + _node_station[arm_id][cage_id][station_id]->GetName()
                                  + TString("/")
                                  + _node_sector[arm_id][cage_id][station_id][sector_id]->GetName()
                                  + TString("/") + _node_sis->GetName()
                                  + TString("/") + node_column->GetName());
                      _phy_column[arm_id][cage_id][station_id][sector_id][column_id] =
			a_phy_node;
		      //std::cout << "Setting node: " << a_phy_node->GetName() << std::endl;
                    }
                }
            }
        }
    }

  //gGeoManager->Export("geom_new.root","PHENIX");
  if (_verbosity >= FVTXGEOM::SOME)
    std::cout << "end of set physical node " << std::endl;

}










//___________________________________________________________
//! set physical nodes apply to all FVTX geom objects
void
FvtxGeom::set_phy_nodes()
{
  cout
      << "FvtxGeom::set_phy_nodes - apply geometry from TGeo physics nodes to FVTX"
      << endl;

  try
    {
      for (unsigned int arm_id = 0; arm_id < FVTXGEOM::NumberOfArms; arm_id++)
        {
          FvtxArm* arm = get_arm(arm_id);
          arm->set_phy_node(_phy_arm[arm_id]);
	  
          for (unsigned int cage_id = 0; cage_id < FVTXGEOM::NumberOfCages;
              cage_id++)
            {
              FvtxCage* cage = arm->get_cage(cage_id);
	      if(!_isReorg) cage->set_phy_node(get_phy_cage(arm->index().arm(), cage_id));

              for (unsigned int station_id = 0;
                  station_id < FVTXGEOM::NumberOfStations; station_id++)
                {
                  FvtxStation* station = cage->get_station(station_id);

                  station->set_phy_node(
                      get_phy_station(cage->index().arm(), cage->index().cage(),
                          station_id));


                  for (unsigned int sector_id = 0;
                      sector_id < FVTXGEOM::NumberOfSectors; sector_id++)
                    {
		      
                      FvtxSector* sector = station->get_sector(sector_id);

                      TGeoPhysicalNode *phy_sector = get_phy_sector(
                          sector->index().arm(), sector->index().cage(),
                          sector->index().station(), sector_id);
                      if (!phy_sector)
                        {
                          std::cerr << "no such index" << " arm "
                              << sector->index().arm() << " cage "
                              << sector->index().cage() << " station "
                              << sector->index().station() << " sector "
                              << sector_id << std::endl;
                        }

                      sector->set_phy_node(phy_sector);

                      for (unsigned int column_id = 0;
                          column_id < FVTXGEOM::NumberOfColumns; column_id++)
                        {

                          FvtxColumn *column = sector->get_column(column_id);

                          column->set_phy_node(
                              get_phy_column(sector->index().arm(),
                                  sector->index().cage(),
                                  sector->index().station(),
                                  sector->index().sector(), column_id));

                          for (unsigned int strip_id = 0;
                              strip_id < column->get_n_strips(); strip_id++)
                            {

                              // create strip
                              FvtxStrip* strip = column->get_strip(strip_id);
			      
			      if(!strip) std::cout << "strip is null!" << std::endl;

			      //std::cout << "setting strip: " << column->index().arm() << " " << column->index().cage() << " " << column->index().station() << " " << column->index().sector() << " " << column->index().column() << std::endl;

                              strip->set_phy_node(
                                  get_phy_column(column->index().arm(),
                                      column->index().cage(),
                                      column->index().station(),
                                      column->index().sector(),
				      column->index().column()));

			      //std::cout << "strip set: " << column->index().arm() << " " << column->index().cage() << " " << column->index().station() << " " << column->index().sector() << " " << column->index().column() << std::endl;
                            }
                        }
                    }
                }
            }
        }
    }
  catch (std::exception& e)
    {
      cout << "FvtxGeom::set_phy_nodes - ERROR - " << e.what() << endl;

      throw;
    }

  _geometry_loaded = true;
}


//___________________________________________________________
//! set physical nodes apply to all FVTX geom objects
void
FvtxGeom::set_phy_nodes_reorg()
{
  cout
      << "FvtxGeom::set_phy_nodes - apply geometry from TGeo physics nodes to FVTX"
      << endl;

  try
    {
      for (unsigned int arm_id = 0; arm_id < FVTXGEOM::NumberOfArms; arm_id++)
        {
          FvtxArm* arm = get_arm(arm_id);
          arm->set_phy_node(_phy_arm[arm_id]);

          for (unsigned int cage_id = 0; cage_id < FVTXGEOM::NumberOfCages;
              cage_id++)
            {
              FvtxCage* cage = arm->get_cage(cage_id);
              cage->set_phy_node(get_phy_cage(arm->index().arm(), cage_id));

              for (unsigned int station_id = 0;
                  station_id < FVTXGEOM::NumberOfStations; station_id++)
                {
                  FvtxStation* station = cage->get_station(station_id);

                  station->set_phy_node(
                      get_phy_station(cage->index().arm(), cage->index().cage(),
                          station_id));

                  for (unsigned int sector_id = 0;
                      sector_id < FVTXGEOM::NumberOfSectors; sector_id++)
                    {
                      FvtxSector* sector = station->get_sector(sector_id);

                      TGeoPhysicalNode *phy_sector = get_phy_sector(
                          sector->index().arm(), sector->index().cage(),
                          sector->index().station(), sector_id);
                      if (!phy_sector)
                        {
                          std::cerr << "no such index" << " arm "
                              << sector->index().arm() << " cage "
                              << sector->index().cage() << " station "
                              << sector->index().station() << " sector "
                              << sector_id << std::endl;
                        }

                      sector->set_phy_node(phy_sector);

                      for (unsigned int column_id = 0;
                          column_id < FVTXGEOM::NumberOfColumns; column_id++)
                        {

                          FvtxColumn *column = sector->get_column(column_id);

                          column->set_phy_node(
                              get_phy_column(sector->index().arm(),
                                  sector->index().cage(),
                                  sector->index().station(),
                                  sector->index().sector(), column_id));

                          for (unsigned int strip_id = 0;
                              strip_id < column->get_n_strips(); strip_id++)
                            {
                              // create strip
                              FvtxStrip* strip = column->get_strip(strip_id);

                              strip->set_phy_node(
                                  get_phy_column(column->index().arm(),
                                      column->index().cage(),
                                      column->index().station(),
                                      column->index().sector(),
                                      column->index().column()));
                            }
                        }
                    }
                }
            }
        }
    }
  catch (std::exception& e)
    {
      cout << "FvtxGeom::set_phy_nodes - ERROR - " << e.what() << endl;

      throw;
    }

  _geometry_loaded = true;
}




//___________________________________________________________
// refresh all phys nodes after alignment
void
FvtxGeom::refresh_geometry(void)
{
  if (!_fGeom.get())
    {
      std::cerr
          << "FvtxGeom::refresh_geometry - FVTX geometry was not loaded. Complain loudly..."
          << std::endl;
      throw runtime_error(
          "FvtxGeom::refresh_geometry - FVTX geometry was not loaded");
    }

  // preserve gGeoManager, which will be overwritten during loading
  TGeoManager * g_old = gGeoManager;
  gGeoManager = _fGeom.get();

  _fGeom->RefreshPhysicalNodes(kFALSE);

  set_phy_nodes();

  gGeoManager = g_old; // recover gGeoManager
}

//___________________________________________________________
void
FvtxGeom::create_arms(void)
{

  // create arms
  for (unsigned int arm_id = 0; arm_id < FVTXGEOM::NumberOfArms; arm_id++)
    {
      if (_arms[arm_id])
        continue;

      // create new arm and asign
      if (_verbosity >= FVTXGEOM::SOME)
        {
          cout << "FvtxGeom::create_arms - initializing arm " << arm_id << " ("
              << ((arm_id == FVTXGEOM::South) ? "south" : "north") << ")"
              << endl;
        }

      FvtxArm* arm = new FvtxArm(TFvtxIndex().set_arm(arm_id));
      set_arm(arm_id, arm);

      // create stations
      create_cages(arm);

      // Feb 2012, Jin Huang <jhuang@bnl.gov>
      // Redesign the mappings
//    for( unsigned int cage_id=0; cage_id < FVTXGEOM::NumberOfCages; cage_id++ )
//    {
//      if( _verbosity >= FVTXGEOM::SOME ) cout << "FvtxGeom::create_arms init DCM channel maps" << endl;
//
//      // create cage
//      FvtxCage* cage = arm->get_cage(cage_id);

//      cage->getDCMChannels();
//      }
    }

  // try load geometry
  load_geometry();

  if (_verbosity >= FVTXGEOM::SOME)
    std::cout << "end of create_arms " << std::endl;
}

//___________________________________________________________
void
FvtxGeom::create_cages(FvtxArm* arm)
{
  // create cage for this arm
  for (unsigned int cage_id = 0; cage_id < FVTXGEOM::NumberOfCages; cage_id++)
    {

      if (_verbosity >= FVTXGEOM::ALOT)
        cout << "FvtxGeom::create_cages - arm: " << arm->index().arm()
            << " cage: " << cage_id << endl;

      // create cage
      FvtxCage* cage = new FvtxCage(arm->index().set_cage(cage_id));
//      cage->set_phy_node(get_phy_cage(arm->index().arm(), cage_id));

      // store in geometry tree
      arm->set_cage(cage_id, cage);

      // create sector
      create_stations(cage);
    }
}

//___________________________________________________________
void
FvtxGeom::create_stations(FvtxCage* cage)
{
  // create station for this cage
  for (unsigned int station_id = 0; station_id < FVTXGEOM::NumberOfStations;
      station_id++)
    {

      if (_verbosity >= FVTXGEOM::ALOT)
        cout << "FvtxGeom::create_stations - arm: " << cage->index().arm()
            << " cage: " << cage->index().cage() << " station: " << station_id
            << endl;

      // create station
      FvtxStation* station = new FvtxStation(
          cage->index().set_station(station_id));
//      station->set_phy_node(
//          get_phy_station(cage->index().arm(), cage->index().cage(),
//              station_id));

      // store in geometry tree
      cage->set_station(station_id, station);

      // create sector
      create_sectors(station);
    }
}

//___________________________________________________________
void
FvtxGeom::create_sectors(FvtxStation *station)
{
  for (unsigned int sector_id = 0; sector_id < FVTXGEOM::NumberOfSectors;
      sector_id++)
    {
      if (_verbosity >= FVTXGEOM::ALOT)
        cout << "FvtxGeom::create_sectors - arm: " << station->index().arm()
            << " cage: " << station->index().cage() << " station: "
            << station->index().station() << " sector: " << sector_id << endl;

      // retrieve detector index
      TFvtxIndex index(station->index());
      index.set_sector(sector_id);

      // create sector
      FvtxSector* sector = new FvtxSector(
          station->index().set_sector(sector_id));

//      TGeoPhysicalNode *phy_sector = get_phy_sector(sector->index().arm(),
//          sector->index().cage(), sector->index().station(), sector_id);
//      if (!phy_sector)
//        {
//          std::cerr << "no such index" << " arm " << sector->index().arm()
//              << " cage " << sector->index().cage() << " station "
//              << sector->index().station() << " sector " << sector_id
//              << std::endl;
//        }
//
//      sector->set_phy_node(phy_sector);

      // add to plane
      station->set_sector(sector_id, sector);

      // create columns
      create_columns(sector);
    }
}

//_______________________________________________________
void
FvtxGeom::create_columns(FvtxSector* sector)
{
  for (unsigned int column_id = 0; column_id < FVTXGEOM::NumberOfColumns;
      column_id++)
    {
      if (_verbosity >= FVTXGEOM::ALOT)
        cout << "FvtxGeom::create_columns -" << " arm: "
            << sector->index().arm() << " cage: " << sector->index().cage()
            << " station: " << sector->index().station() << " sector: "
            << sector->index().sector() << " column: " << column_id << endl;

      unsigned int n_strip = FVTXGEOM::NumberOfStripsSt2;
      if (sector->index().station() == 0)
        n_strip = FVTXGEOM::NumberOfStripsSt1;
      // create column
      FvtxColumn *column = new FvtxColumn(sector->index().set_column(column_id),
          n_strip);
//      column->set_phy_node(
//          get_phy_column(sector->index().arm(), sector->index().cage(),
//              sector->index().station(), sector->index().sector(), column_id));

      // store
      sector->set_column(column_id, column);

      // create strips in column
      create_strips(column);
    }
}

//___________________________________________________________
void
FvtxGeom::create_strips(FvtxColumn* column)
{
  for (unsigned int strip_id = 0; strip_id < column->get_n_strips(); strip_id++)
    {
      // create strip
      FvtxStrip* strip = new FvtxStrip(column->index(), strip_id);
//      strip->set_phy_node(
//          get_phy_column(column->index().arm(), column->index().cage(),
//              column->index().station(), column->index().sector(),
//              column->index().column()));

      // add to column
      column->set_strip(strip_id, strip);
    }
}

//___________________________________________________________
/*
 IO interface.
 This part of the code should not need to be changed when implementing
 new versions of the geometry.
 */
FvtxArm*
FvtxGeom::get_arm(unsigned int index)
{
  if (_arms.size() <= index)
    throw runtime_error("FvtxGeom::get_arm - invalid index");
  if (!_arms[index])
    create_arms();
  if (!_arms[index])
    throw runtime_error("FvtxGeom::get_arm - not initialized");
  return _arms[index].get();
}

//___________________________________________________________
TGeoPhysicalNode*
FvtxGeom::get_phy_cage(unsigned int arm, unsigned int cage)
{
  TGeoPhysicalNode *aCage = _phy_cage[arm][cage];
  if (!aCage)
    throw runtime_error("FvtxGeom::get_phy_cage - not initialized");
  return aCage;
}

//___________________________________________________________ 
TGeoPhysicalNode*
FvtxGeom::get_phy_station(unsigned int arm, unsigned int cage,
    unsigned int station)
{
  TGeoPhysicalNode *aStation = _phy_station[arm][cage][station];
  if (!aStation)
    throw runtime_error("FvtxGeom::get_phy_station - not initialized");
  return aStation;
}

//___________________________________________________________ 
TGeoPhysicalNode*
FvtxGeom::get_phy_sector(unsigned int arm, unsigned int cage,
    unsigned int station, unsigned int sector)
{
  TGeoPhysicalNode *aSector = _phy_sector[arm][cage][station][sector];
  if (!aSector)
    throw runtime_error("FvtxGeom::get_phy_sector - not initialized");
  return aSector;
}

//___________________________________________________________ 
TGeoPhysicalNode*
FvtxGeom::get_phy_column(unsigned int arm, unsigned int cage,
    unsigned int station, unsigned int sector, unsigned int column)
{
  TGeoPhysicalNode *aColumn = 0;
  aColumn = _phy_column[arm][cage][station][sector][column];
  if (!aColumn)
    throw runtime_error("FvtxGeom::get_phy_column - not initialized");
  return aColumn;
}

// Feb 2012, Jin Huang <jhuang@bnl.gov>
// Redesign the mappings
//______________________________________________
FvtxDCMChannelMap*
FvtxGeom::get_dcm_map()
{
  if (!_dcm_map)
    {
      FVTXGEOM::PRINT(cout, "FvtxGeom::Load FvtxDCMChannelMap");
      _dcm_map = new FvtxDCMChannelMap();
      if (!_dcm_map)
        cout << "FvtxGeom::get_dcm_map() - Mapping Error" << endl;
      FVTXGEOM::PRINT(cout, "FvtxGeom::Load FvtxDCMChannelMap Finished");
    }
  return _dcm_map;
}
////______________________________________________
//FvtxDCMChannelMap* FvtxGeom::south_dcm_map()
//{
//  if( !_south_dcm_map ) _south_dcm_map = new FvtxDCMChannelMap(south_arm());
//  return _south_dcm_map;
//}
//
////______________________________________________
//FvtxDCMChannelMap* FvtxGeom::north_dcm_map()
//{
//  if( !_north_dcm_map ) _north_dcm_map = new FvtxDCMChannelMap(north_arm());
//  return _north_dcm_map;
//}
const std::string
FvtxGeom::set_public_file_path(std::string path)
{

  std::cout
      << "FvtxGeom::set_public_file_path - WARNING - this function is obsolete, use TFvtxGlobalParCntrl."
      << std::endl;

  TFvtxGlobalParCntrl::set_string_par("geom_root_file_path", path);

  return path;
}

//___________________________________________________________
FvtxGeom::UseFvtxGeomManager::UseFvtxGeomManager()
{
  // preserve gGeoManager, which will be overwritten during fvtx geo operations
  _g_old = gGeoManager;
  gGeoManager = FvtxGeom::_fGeom.get();
}

//___________________________________________________________
FvtxGeom::UseFvtxGeomManager::~UseFvtxGeomManager()
{
  gGeoManager = _g_old; // recover gGeoManager
}

/*
static TGeoNode* FvtxGeom::GetNodePointer(TString nodeName, TGeoNode* topNode)
{
  return  topNode->GetNode(nodeName);
}

void FvtxGeom::GetNodeFailed(TString nodeName, TString volumeName)
{
  std::cout
    << "FvtxGeom - ERROR! Failed to get node " << nodeName << " from volume "
    << volumeName << ", exiting ... " << std::endl;
  exit(1);
}
*/

bool FvtxGeom::CheckForReorg()
{

  bool reorg = false;

  TGeoVolume *vol_hall_tmp = _fGeom->GetVolume("HALL");
  if (!vol_hall_tmp)
    {
      std::cout
          << "FvtxGeom::CheckForReorg: ERROR: Failed to find HALL volume in "
          << _fGeom->GetName() << ", exiting ... " << std::endl;
      exit(1);
    }

  TGeoNode *node_hall_tmp = _fGeom->GetTopNode();
  if (!node_hall_tmp)
    {
      std::cout
          << "FvtxGeom::CheckForReorg: ERROR: Failed to get top node from HALL volume"
          << ", exiting ... " << std::endl;
      exit(1);
    }

  TGeoNode *node_sien_tmp = vol_hall_tmp->GetNode("SIEN_1");
  if (!node_sien_tmp)
    {
      std::cout
          << "FvtxGeom::CheckForReorg: ERROR: Failed to get node SIEN_1 from "
          << vol_hall_tmp->GetName() << ", exiting ... " << std::endl;
    }

  TGeoNode *node_sicg_tmp = node_sien_tmp->GetVolume()->GetNode("SICG_1");
  if (!node_sicg_tmp)
    {
      std::cout
          << "FvtxGeom::CheckForReorg: ERROR: Failed to get node SICG_1 from volume "
          << node_sien_tmp->GetName() << ", exiting ... " << std::endl;
      exit(1);
    }

  TGeoNode *node_scms_tmp = node_sicg_tmp->GetVolume()->GetNode("SCMS_1");
  if (!node_scms_tmp)
    {
      std::cout
          << "FvtxGeom::CheckForReorg: ERROR: Failed to get node SCMS_1 from node "
          << node_sicg_tmp->GetName() << std::endl;
      std::cout << "Exiting..." << std::endl;
      exit(1);
    }

  TGeoNode *node_scm1 = node_scms_tmp->GetVolume()->GetNode("SCM1_1");
  if (!node_scm1)
    {
      reorg = true;
    }


  return reorg;


}


