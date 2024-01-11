// $Id: FvtxGlobalAlign.cxx,v 1.20 2015/09/09 01:50:47 jinhuang Exp $

/*!
 \file  FvtxGlobalAlign.cxx
 \brief   fvtx global alignment module
 \author  Zhengyun You
 \version $Revision: 1.20 $
 \date  $Date: 2015/09/09 01:50:47 $
 */

#include "FvtxGlobalAlign.h"

#include <MuonUtil.h>
#include <recoConsts.h>
#include <Fun4AllReturnCodes.h>
#include <PHTFileServer.h>

#include <PHCylPoint.h>
#include <PHGeometry.h>
#include <PHException.h>

#include <TMutGeo.h>
#include <FvtxGeom.h>

#include <FVTXOO.h>
#include <TFvtxCoordMap.h>
#include <TFvtxHitMap.h>
#include <VtxOut.h>
#include <TFvtxHitMap.h>
#include <TFvtxClusMap.h>
#include <TFvtxHitMap.h>
#include <TFvtxSvxClusterMap.h>

// MUTOO
#include <TMutCoordMap.h>
#include <TMutGapCoordMap.h>
#include <TMutNode.h>
#include <TMutClusMap.h>
#include <TMutTrackUtil.h>
#include <TMutTrkMap.h>
#include <TMutVtxMap.h>

// MUIOO
#include <TMuiHitMapO.h>
#include <TMuiRoadMapO.h>
#include <TMuiClusterMapO.h>
#include <TMuiGeo.h>
#include <MuiCommon.hh>

#include <TFvtxMPTrack.h>

#include <TBranch.h>
#include <TBranchElement.h>
#include <TTree.h>
#include <TMath.h>
#include <TH1F.h>
#include <TVectorD.h>
#include <TMatrixD.h>
#include <TRandom3.h>
#include <TClass.h>

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <cassert>
#include <cmath>

using namespace std;

//! convert PHPoint to TVector3
const TVector3
convert(const PHPoint & p)
{
  return TVector3(p.getX(), p.getY(), p.getZ());
}

//____________________________________________________________________
FvtxGlobalAlign::FvtxGlobalAlign(const char* name) :
    MuonSubsysReco(name), // reconstruction
    TFvtxMILLEPEDE(), // millepede backbone

// maps
    _trk_map(NULL), _cluster_map(NULL), _coord_map(NULL), _trk_mutr_map(NULL), //
    _coord_mutr_map(NULL), _vtxout_node(NULL),

// evaluation
    _misalignment_filename("FvtxGlobalAlign.root"), _dumpfile_name(
        "millepede.log"), //
    _AlignDST_filename("FvtxGlobalAlign_AlignDST.root"), //
    _AlignDST_tree(0), _AlignDST_tree_read(0), _h_beam_x(NULL), _h_beam_y(NULL), //
    _container(NULL), _timer(PHTimeServer::get()->insert_new(name)),

    //track projector
    _integrator()
{

  MUTOO::TRACE("FvtxGlobalAlign::FvtxGlobalAlign");

  // default Alignment flags
  _flags |= DO_ALIGNMENT | DO_ALIGN_DST | DOUBLE_HIT_PER_STA | USE_PHI_ACPT_CUT
      | USE_CONSTRAINTS_STATIONS | USE_FVTX_ALONE_TRACK;
//      | SCALE_MUTR_KALMAN_MOMENTUM;

  set_flag(USE_CUTS, false);

  _run_header = NULL;
  _event_header = NULL;
  _n_good_muon_trk = 0;
  _n_good_trk = 0;
  _run_num = 0;
  _event_num = 0;

  _vtxx = 0; //! final vertex used
  _vtxy = 0; //! final vertex used
  _vtxz = 0; //! final vertex used
  _vtxxp = 0; //! vtx preceise vertex
  _vtxyp = 0; //! vtx preceise vertex
  _vtxzp = 0; //! vtx preceise vertex
  _bbcz = 0; //! bbc z vertex
  _accept_evt = false;

  _track_index = 0;

  _track = NULL;
  _track_class = "TFvtxMPTrack";

  // indexes
  _n_tracks_total = 0;
  _n_tracks_minimized = 0;
  _n_tracks_analyzed = 0;
  _n_events_total = 0;
  _n_events_acpt = 0;
  _n_events_analyzed = 0;
  _n_mutr_tracks_total = 0;
  _n_mutr_tracks_analyzed = 0;

  _pz_min = 1;
  _pz_max = 8000;

  // constraints
  _vertex_lateral_constraint = .0001;
  _vertex_acceptance = 15;
  _vertex_z_resolution_cut = 400e-4;
  _vtx_dca_constraint = .05;
  _vtx_dca_shift = 0;
  _svx_hit_sigma_r = 0.2; // 2000 um
  _svx_hit_sigma_phi = 0.2; // 2000 um
  _fvtx_hit_sigma_factor = 1; // one times the DST sigma
  _mutr_hit_sigma_min = 1; // 10 mm
  _muid_hist_sigma_scale_factor = 2;
  _z_ref = 80; //cm

//  _sigma_dr_pt = 1;
//  _sigma_dr_lateral = 2;
//  _sigma_dtheta_pt = .01;
//  _sigma_dtheta_lateral = .01;

  _sigma_dr_pt = 1.3255;
  _sigma_dr_lateral = 1.3;
  _sigma_dtheta_pt = 0.0147696;
  _sigma_dtheta_lateral = .0139;

  _vtx_vertex_name = "SVX_PRECISE";
}

//____________________________________________________________________
//! destructor
FvtxGlobalAlign::~FvtxGlobalAlign()
{
  if (_AlignDST_tree)
    _AlignDST_tree->ResetBranchAddresses();
  if (_track)
    delete _track;
  if (_container)
    delete _container;
  _track = NULL;
}

//____________________________________________________________________
int
FvtxGlobalAlign::Init(PHCompositeNode *top_node)
{

  // call base class initialization
  // this is needed to get the module row (in list of registered modules) set properly
  MuonSubsysReco::Init(top_node);

  MUTOO::PRINT(cout, "FvtxGlobalAlign::Init with top_node");

  // initialize reconstruction modules
  createNodeTree(top_node);

  MUTOO::PRINT(cout, "**");

  init();

  return 0;
}

//____________________________________________________________________
int
FvtxGlobalAlign::process_event(PHCompositeNode *top_node)
{
  if (Verbosity() >= 1)
    MUTOO::TRACE("FvtxGlobalAlign::process_event");

  _timer.get()->restart();

  try
    {

      // get pointers to needed map
      set_interface_pointers(top_node);

      // minimization
//      if (get_flag(DO_ALIGNMENT) || get_flag(DO_ALIGN_DST))
//        {
      event();

//        }

    }
  catch (exception &e)
    {

      cout << e.what() << endl;

    }

  // needed to make sure maps are always written whatever SubsysReco are in the macro
  MuonSubsysReco::write_maps_if_needed();

  _timer.get()->stop();

  _n_events_total++;

  if (FVTXOO::special_event_num(_n_events_total))
    {
      cout << "FvtxGlobalAlign::process_event - " << _n_events_acpt << " / "
          << _n_events_analyzed << " / " << _n_events_total
          << " events accepted/analyzed/total. "
          << "FVTX Tracks minimized / analyzed / total = "
          << _n_tracks_minimized << " / " << _n_tracks_analyzed << " / "
          << _n_tracks_total;

      if (get_flag(USE_MUTR_HITS) || get_flag(USE_MUTR_ALONE_TRACK))
        cout << ", " << _n_mutr_tracks_analyzed << " / " << _n_mutr_tracks_total
            << " MuTr tracks analyzed";

//          << _n_tracks_minimized << " out of "
//          << _n_tracks_total << " tracks used for minimization."

      cout << endl;
    }

  return evaluate_event();
}

//____________________________________________________________________
int
FvtxGlobalAlign::End(PHCompositeNode *top_node)
{
  MUTOO::PRINT(cout, "FvtxGlobalAlign::End");

  // print timing statistics
  _timer.get()->print_stat();

  end();

  return 0;

}

//____________________________________________________________________
int
FvtxGlobalAlign::createNodeTree(PHCompositeNode* top_node)
{
  MUTOO::PRINT(cout, "FvtxGlobalAlign::createNodeTree");

  // Instantiate nodes for fvtxoo containers
  PHCompositeNode* fvtxoo_node(0);
    {
      PHNodeIterator nodeItr(top_node);
      fvtxoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst(
          "PHCompositeNode", "FVTXOO"));
      if (!fvtxoo_node)
        {
          cout << "FvtxGlobalAlign::createNodeTree - adding FVTXOO node"
              << endl;
          fvtxoo_node = new PHCompositeNode("FVTXOO");
          top_node->addNode(fvtxoo_node);
        }
    }

  // instanciate DST node
  PHCompositeNode* dst_node(0);
    {
      PHNodeIterator nodeItr(top_node);
      dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst(
          "PHCompositeNode", "DST"));
      if (!dst_node)
        {
          cout << "FvtxGlobalAlign::createNodeTree - adding DST node" << endl;
          dst_node = new PHCompositeNode("DST");
          top_node->addNode(dst_node);
        }
    }
//
//  // alignment parameter node
//  if (!get_flag(READ_MEM))
//    {
//
//      // create new AlignPar node and make persistent
//      TMutNode<TFvtxAlignParMap>::new_node(fvtxoo_node, "TFvtxAlignParMap")->make_persistant(
//          dst_node, "TFvtxAlignPar");
//
//    }

  MUTOO::PRINT(cout, "FvtxGlobalAlign::createNodeTree - done.");
  return 0;

}

//____________________________________________________________________
void
FvtxGlobalAlign::initialize_AlignDST_tree(bool read)
{
  MUTOO::TRACE("FvtxGlobalAlign::initialize_AlignDST_tree");

  if (!_track)
    {
      cout
          << "FvtxGlobalAlign::initialize_AlignDST_tree - Error - the track object was will be initialized by TTree."
          << endl;
      return;
    }

  if (read)
    { // read alignment DST file outside the Fun4All cycle

      cout
          << "FvtxGlobalAlign::initialize_AlignDST_tree - Init Read AlignDST_tree  "
          << _AlignDST_tree_read->GetName() << " : "
          << _AlignDST_tree_read->GetTitle() << " with "
          << _AlignDST_tree_read->GetEntries() << " entries." << endl;

      // open TFile
//      TFile * align_dst = new TFile("_AlignDST_filename");
//      assert(align_dst);
//
//      _AlignDST_tree = align_dst->GetObjectChecked("alignment", "TTree");
//      assert(_AlignDST_tree);

      if (!_AlignDST_tree_read)
        {
          cout
              << "FvtxGlobalAlign::initialize_AlignDST_tree - Error - Cannot find AlignDST_tree."
              << endl;
          return;
        }

      _AlignDST_tree_read->ResetBranchAddresses();

      TBranch * b = _AlignDST_tree_read->GetBranch("track.");
      assert(b);

      TBranchElement* be = dynamic_cast<TBranchElement*>(b);
      assert(be);

      string sbranch_class_name = be->GetClassName();

      cout
          << "FvtxGlobalAlign::initialize_AlignDST_tree - expected branch class name "
          << sbranch_class_name << " VS " << "track->ClassName()="
          << _track->ClassName() << endl;

      assert(sbranch_class_name == _track->ClassName());
      // strict class matching check

      _AlignDST_tree_read->SetBranchAddress("track.", &_track);
    }
  else
    { // build alignment DST file

      cout
          << "FvtxGlobalAlign::initialize_AlignDST_tree - Initializing AlignDST file "
          << _AlignDST_filename << endl;

      // open TFile
      PHTFileServer::get().open(_AlignDST_filename, "RECREATE");

      // create tree (for now the tree is empty)
      _AlignDST_tree = new TTree("alignment", "global alignment tree");

      _AlignDST_tree->Branch("track.", _track_class.c_str(), &_track,
          BUFFER_SIZE);

      // Disable this feature for now
//      _container = new TClonesArray("TFvtxCoord_v1", 600);
      if (_container)
        _AlignDST_tree->Branch("container", &_container, BUFFER_SIZE, 99);

      _AlignDST_tree->SetAutoSave(AUTO_SAVE);
    }
}

//____________________________________________________________________
void
FvtxGlobalAlign::fill_container()
{
  if (!_container)
    return;
  if (!_coord_map)
    return;

  _container->Clear();

  int i = 0;

  TFvtxCoordMap::iterator iter(_coord_map->range());
  while (TFvtxCoordMap::pointer ptr = iter.next())
    {
      new ((*_container)[i]) TFvtxCoord_v1(ptr->get());
      i++;
    }
}

//____________________________________________________________________
void
FvtxGlobalAlign::initialize_alignment_tree(void)
{
  MUTOO::TRACE("FvtxGlobalAlign::initialize_alignment_tree");

  // open TFile
  PHTFileServer::get().open(_misalignment_filename, "RECREATE");

  build_misalignment_tree();

}

//____________________________________________________________________
void
FvtxGlobalAlign::set_interface_pointers(PHCompositeNode* top_node)
{

  if (Verbosity() >= 1)
    MUTOO::TRACE("FvtxGlobalAlign::set_interface_pointers");

//  _align_par_map = TMutNode<TFvtxAlignParMap>::find_node(top_node,
//      "TFvtxAlignParMap");

  _event_header = TMutNode<EventHeader>::find_io_node(top_node, "EventHeader");

  try
    {
      _run_header = TMutNode<RunHeader>::find_io_node(top_node, "RunHeader");
    }
  catch (...)
    {
      _run_header = NULL;
    }

  // retrieve track map
  _trk_map = TMutNode<TFvtxTrkMap>::find_node(top_node, "TFvtxTrkMap");

  if (get_flag(USE_MUTR_HITS) || get_flag(USE_MUTR_ALONE_TRACK))
    {
      try
        {
          _trk_mutr_map = TMutNode<TMutTrkMap>::find_node(top_node,
              "TMutTrkMap");
        }
      catch (const std::exception& e)
        {
          FVTXOO::TRACE(e.what());
          _trk_mutr_map = NULL;
        }
    }
  else
    {
      _trk_mutr_map = NULL;
    }

  if (get_flag(USE_MUTR_HITS) || get_flag(USE_MUTR_ALONE_TRACK))
    {
      try
        {
          _coord_mutr_map = TMutNode<TMutCoordMap>::find_node(top_node,
              "TMutCoordMap");
        }
      catch (const std::exception& e)
        {
          FVTXOO::TRACE(e.what());
          _coord_mutr_map = NULL;
        }
    }
  else
    {
      _coord_mutr_map = NULL;
    }

  // retrieve coordinate map
  _coord_map = TMutNode<TFvtxCoordMap>::find_node(top_node, "TFvtxCoordMap");

  // vtxout node
  _vtxout_node = MuonUtil::find_io_node<VtxOut>("VtxOut");
}

//____________________________________________________________________
void
FvtxGlobalAlign::init(void)
{

  MUTOO::PRINT(cout, "FvtxGlobalAlign::init initializing minimizer");

  // flag checks

  //  _track_class = "TFvtxMPTrack";
  _track_class = "TForwardMPTrack";
  set_n_track_para(MILLEPEDE::NPARTRK * 2);

  if (get_flag(ALIGN_FVTX_STATION))
    {
      cout << "FvtxGlobalAlign::Init - Info : " << "Align FVTX Stations."
          << endl;

      if (get_flag(USE_CONSTRAINTS_STATIONS))
        {
          cout << "FvtxGlobalAlign::Init - Info : "
              << "Will use constraints on station locations when they are aligned."
              << endl;
        }
      if (get_flag(USE_CONSTRAINTS_CAGE_Z) and get_flag(ALIGN_Z))
        {
          cout << "FvtxGlobalAlign::Init - Info : "
              << "USE_CONSTRAINTS_CAGE_Z is set. Constraint the cage to move together in Z."
              << endl;
        }
    }

  if (get_flag(ALIGN_FVTX_WEDGE))
    {
      cout << "FvtxGlobalAlign::Init - Info : " << "Align FVTX Wedges." << endl;
      if (get_flag(USE_CONSTRAINTS))
        {
          cout << "FvtxGlobalAlign::Init - Info : " << "Constraint wedge halfs."
              << endl;
        }
      if (get_flag(USE_AZIMUTHAL_MOD_CONSTRAINTS))
        {
          cout << "FvtxGlobalAlign::Init - Info : "
              << "Constraint 3rd station wedges using constant azimuthal modulation."
              << endl;
        }
      if (get_flag(USE_CONSTRAINTS_CAGE_Z) and get_flag(ALIGN_Z))
        {
          cout << "FvtxGlobalAlign::Init - Warning : "
              << "USE_CONSTRAINTS_CAGE_Z is set. Therefore, wedge Z will NOT be aligned."
              << endl;
        }

    }

  cout << "FvtxGlobalAlign::Init - Info : " << "FVTX hit sigma = "
      << _fvtx_hit_sigma_factor << " * TFvtxCoord::error" << endl;
  if (get_flag(USE_VTX_HITS))
    {
      cout << "FvtxGlobalAlign::Init - Info : "
          << "VTX Hits will be used in the alignment. sigma (r, phi) = ("
          << _svx_hit_sigma_r << "," << _svx_hit_sigma_phi << ")" << endl;
    }
  if (get_flag(USE_MUTR_HITS))
    {
      cout << "FvtxGlobalAlign::Init - Info : "
          << "MuTr Hits will be used in the alignment. sigma_w >=  "
          << _mutr_hit_sigma_min << " cm, z ref = " << _z_ref << " cm, |pz| = "
          << _pz_min << " - " << _pz_max << " GeV/c" << endl;
    }
  if (get_flag(USE_MUID_HITS))
    {
      cout << "FvtxGlobalAlign::Init - Info : "
          << "MuonID Hits will be used in the alignment. Additional scale factor = "
          << _muid_hist_sigma_scale_factor << endl;
    }
  if (get_flag(USE_MUTR_HITS) or get_flag(ALIGN_MU_ARM))
    {
      cout
          << "FvtxGlobalAlign::Init - use full forward track analysis TForwardMPTrack"
          << endl;
//      _track_class = "TForwardMPTrack";
      assert(_track_class == "TForwardMPTrack");
      cout << "FvtxGlobalAlign::Init - multiple scattering sigma :" << endl;
      cout << "\t _sigma_dr_pt = " << _sigma_dr_pt << endl;
      cout << "\t _sigma_dr_lateral = " << _sigma_dr_lateral << endl;
      cout << "\t _sigma_dtheta_pt = " << _sigma_dtheta_pt << endl;
      cout << "\t _sigma_dtheta_lateral = " << _sigma_dtheta_lateral << endl;
      if (get_flag(SCALE_MUTR_KALMAN_MOMENTUM))
        cout
            << "FvtxGlobalAlign::Init - Scale MuTr kalman fit total momentum to be consistent with the joint fit"
            << endl;
      else
        cout
            << "FvtxGlobalAlign::Init - do NOT Scale MuTr kalman fit total momentum to be consistent with the joint fit"
            << endl;
    }
  if (get_flag(USE_FVTX_ALONE_TRACK))
    {
      cout << "FvtxGlobalAlign::Init - Info : "
          << "Tracks with FVTX hits only will be accepted for alignment."
          << endl;
    }
  if (get_flag(USE_MUTR_ALONE_TRACK))
    {
      cout << "FvtxGlobalAlign::Init - Info : "
          << "Tracks with MuTr hits only will be accepted for alignment."
          << endl;
    }
  if (!get_flag(USE_VTX_HITS) and !get_flag(USE_MUTR_HITS)
      and !get_flag(USE_FVTX_ALONE_TRACK) and !get_flag(USE_MUTR_ALONE_TRACK))
    {

      cout << "FvtxGlobalAlign::Init - WARNING : "
          << "Both VTX and MuTr was disabled in the alignment, therefore we have to allow USE_FVTX_ALONE_TRACK"
          << endl;

      set_flag(USE_FVTX_ALONE_TRACK, true);
    }

  // use VTX constraint
  if (get_flag(USE_SVTX_CONSTRAINT))
    {
      cout << "FvtxGlobalAlign::Init - Info : " << "Including VTX vertex ("
          << _vtx_vertex_name << ") in track fit, allowed sigma in XY plane = "
          << _vtx_dca_constraint << " cm; shift in PT direction = "
          << _vtx_dca_shift << " cm" << endl;
      cout << "FvtxGlobalAlign::Init - Info : " << "vertex resolution cut  = "
          << _vertex_z_resolution_cut << " cm" << endl;

      if (!get_flag(TRACK_LATCON_FIT))
        {

          cout
              << "FvtxGlobalAlign::Init - Warning : Requiring TRACK_LATCON_FIT with USE_SVTX_CONSTRAINT, setting TRACK_LATCON_FIT"
              << endl;

          set_flag(TRACK_LATCON_FIT, true);
        }

      if (get_flag(USE_CONSTRAINTS_CAGE_POS))
        {

          cout
              << "FvtxGlobalAlign::Init - Warning : we choose to relieve USE_CONSTRAINTS_CAGE_POS with USE_SVTX_CONSTRAINT, setting TRACK_LATCON_FIT"
              << endl;

          set_flag(USE_CONSTRAINTS_CAGE_POS, false);
        }
    }

  // track fit flag check
  if (get_flag(TRACK_1D_Fit) and get_flag(TRACK_LATCON_FIT))
    {
      cout
          << "FvtxGlobalAlign::Init - Error : Conflicting in choosing track fitting method, TRACK_LATCON_FIT will be used instead."
          << endl;
      set_flag(TRACK_1D_Fit, false);
      set_flag(TRACK_LATCON_FIT, true);
    }

  if (get_flag(TRACK_1D_Fit))
    {
      cout << "FvtxGlobalAlign::Init - Warning : "
          << "Use special fit in reduced w-z spaced" << endl;
    }

  if (get_flag(TRACK_LATCON_FIT))
    {
      cout << "FvtxGlobalAlign::Init - Info : "
          << "Use fit with constraint on vertex lateral displacement from center of phi acceptance window, sigma = "
          << _vertex_lateral_constraint << " cm" << endl;
    }

  if (get_flag(USE_MILLEPEDE_TRACK_FIT))
    {
      cout << "FvtxGlobalAlign::Init - Warning : "
          << "Using millepede to re-fit the track for initial residuals. It do NOT work for bended tracks."
          << endl;
    }

  cout << "FvtxGlobalAlign::Init - Info : "
      << (get_flag(DOUBLE_HIT_PER_STA) ? "Accept" : "Reject")
      << " tracks with two hits on same station, one on each side" << endl;

  cout << "FvtxGlobalAlign::Init - Info : " << "vertex acceptance = +/- "
      << _vertex_acceptance << " cm" << endl;

  // initialize track module, make sure it is the first time to initialize it
  assert(!_track);
  if (!_track)
    {
      cout
          << "FvtxGlobalAlign::Init - initialize track anaylsis module with class name "
          << _track_class << " and # of internal fit parameter = "
          << get_n_track_para() << endl;
//      _track = new TFvtxMPTrack();
      TClass * c = TClass::GetClass(_track_class.c_str());
      assert(c);

      _track = static_cast<TFvtxMPTrack *>(c->New());
      assert(_track);
    }

  // initialize evaluation ntuple
  if (get_flag(DO_ALIGN_DST))
    initialize_AlignDST_tree();

  // initialize parameters for Millepede
  if (get_flag(DO_ALIGNMENT))
    {
      assert(get_flag(ALIGN_FVTX) or get_flag(ALIGN_MUTR));

      cout
          << "FvtxGlobalAlign::Init - Perform alignment, resale milleped sigma by x"
          << _sigma_rescaling << endl;

      if (get_flag(ALIGN_FVTX))
        {
          cout << "FvtxGlobalAlign::Init - Setup to align FVTX " << endl;
        }
      if (get_flag(ALIGN_MUTR))
        {
          cout << "FvtxGlobalAlign::Init - Setup to align MuTr " << endl;

          if (get_flag(USE_CONSTRAINTS_MU_ARM) //
          and !get_flag(ALIGN_PHI_MU_ARM))
            {

              cout << "FvtxGlobalAlign::Init - Warning : "
                  << "USE_CONSTRAINTS_MU_ARM is intended for MuTr phi alignment only."
                  << " Since this is not a MuTr phi alignment, reset USE_CONSTRAINTS_MU_ARM to false."
                  << endl;

              set_flag(USE_CONSTRAINTS_MU_ARM, false);
            }

          if (get_flag(USE_CONSTRAINTS_MUTR_ZeroStation))
            {
              cout
                  << "FvtxGlobalAlign::Init - constraint NO station in MuTr by default"
                  << endl;
            }
          else if (get_flag(USE_CONSTRAINTS_MU_ARM_OneStation))
            {
              cout
                  << "FvtxGlobalAlign::Init - constraint one station in MuTr by default"
                  << endl;
            }
          else
            {
              cout
                  << "FvtxGlobalAlign::Init - constraint two stations in MuTr by default"
                  << endl;
            }

          if (get_flag(USE_CONSTRAINTS_MU_ARM_WithInStation))
            {
              cout
                  << "FvtxGlobalAlign::Init - Apply rotational constraint within MuTr stations"
                  << endl;
            }
          if (!get_flag(USE_CONSTRAINTS_MU_ARM_LastStation))
            {
              cout
                  << "FvtxGlobalAlign::Init - Do NOT apply rotational constraint on MuTr Last Station"
                  << endl;
            }

          cout << "FvtxGlobalAlign::Init - Setup to align MuTr: " << endl;
          cout << "                        ALIGN_W_MU_ARM = "
              << get_flag(ALIGN_W_MU_ARM) << endl;
          cout << "                        ALIGN_Z_MU_ARM = "
              << get_flag(ALIGN_Z_MU_ARM) << endl;
          cout << "                        ALIGN_PHI_MU_ARM = "
              << get_flag(ALIGN_PHI_MU_ARM) << endl;
          cout << "                        USE_CONSTRAINTS_MU_ARM = "
              << get_flag(USE_CONSTRAINTS_MU_ARM) << endl;
          cout << "                        USE_CONSTRAINTS_MUTR_ZeroStation = "
              << get_flag(USE_CONSTRAINTS_MUTR_ZeroStation) << endl;
          cout << "                        USE_CONSTRAINTS_MU_ARM_OneStation = "
              << get_flag(USE_CONSTRAINTS_MU_ARM_OneStation) << endl;
          cout
              << "                        USE_CONSTRAINTS_MU_ARM_WithInStation = "
              << get_flag(USE_CONSTRAINTS_MU_ARM_WithInStation) << endl;
          cout
              << "                        USE_CONSTRAINTS_MU_ARM_LastStation = "
              << get_flag(USE_CONSTRAINTS_MU_ARM_LastStation) << endl;
        }
      if (get_flag(ALIGN_MUID))
        {
          cout << "FvtxGlobalAlign::Init - Setup to align MuID " << endl;
          if (get_flag(USE_CONSTRAINTS_MUID_WithInPanel))
            {
              cout
                  << "FvtxGlobalAlign::Init - Apply rotational constraint within MuID panels"
                  << endl;
            }
          if (get_flag(USE_CONSTRAINTS_MUID_ZeroStation))
            {
              cout
                  << "FvtxGlobalAlign::Init - constraint NO station in MuID by default"
                  << endl;
            }
          else if (get_flag(USE_CONSTRAINTS_MU_ARM_OneStation))
            {
              cout
                  << "FvtxGlobalAlign::Init - constraint one station in MuID by default"
                  << endl;
            }
          else
            {
              cout
                  << "FvtxGlobalAlign::Init - constraint two stations in MuID by default"
                  << endl;
            }
        }

      initialize_alignment_tree();
      init_parameters(true);
      init_minimize();

      if (get_flag(ALIGN_FVTX))
        {
          // Fix parameters
          if (get_flag(ALIGN_FVTX_STATION))
            {

              if (get_flag(USE_CONSTRAINTS_STATIONS))
                {
//            fix_fvtx_stations();
//              if (!get_flag(USE_SVTX_CONSTRAINT))
//                {
                  constrain_fvtx_stations();
//                }
//              else
//                {
//                  cout << "FvtxGlobalAlign::Init - Info : "
//                      <<"Since using VTX vertex in track fit, no constraint on stations."
//                      <<endl;
//                }
                }

              if (get_flag(USE_CONSTRAINTS_CAGE_Z) and get_flag(ALIGN_Z))
                {
//                  constrain_fvtx_cage_z();
                  constrain_fvtx_cage_z_shear();
                }

            } //          if (get_flag(ALIGN_FVTX_STATION))
          else
            {

              fix_fvtx_all_stations();

            }

          if (get_flag(ALIGN_FVTX_WEDGE))
            {

//              if (_fixed_fvtx_wedges.empty())
//                fix_fvtx_2station_wedges();

              if (get_flag(USE_CONSTRAINTS))
                {
                  MUTOO::TRACE(
                      "FvtxGlobalAlign::Init - set constrain_halfs to all");
                  for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
                    for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
                      for (int station = 0;
                          station < FVTXGEOM::NumberOfStations; station++)
                        for (int sector = 0; sector < FVTXGEOM::NumberOfSectors;
                            sector++)
                          {
                            constraint_halfs(arm, cage, station, sector);
                          }
                }
              if (get_flag(USE_AZIMUTHAL_MOD_CONSTRAINTS))
                {
                  MUTOO::TRACE(
                      "FvtxGlobalAlign::Init - set constraint azimuthal modulation to station 3");
                  for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
                    for (int station = 3; station < FVTXGEOM::NumberOfStations;
                        station++)
                      {
                        constraint_azimuthal_modulation(arm, station);
                      }
                }

              if (get_flag(USE_CONSTRAINTS_CAGE_Z) and get_flag(ALIGN_Z))
                {
                  fix_fvtx_all_wedges(PAR_Z);
                }
            } //  if (get_flag(ALIGN_FVTX_WEDGE))
          else
            {

              fix_fvtx_all_wedges();
            }

        } // if (ALIGN_FVTX)

      // Fix parameters for MuTr and alignment
      if (get_flag(ALIGN_MUTR))
        {
          if (_fixed_mutr_detectors.empty())
            fix_mutr_2gaps();

          // fix station2 gap2 in both arms since the detector does not exists
          fix_mutr_gap(0, 2, 2, ALL);
          fix_mutr_gap(1, 2, 2, ALL);

        }
      if (get_flag(ALIGN_MUID))
        {
          if (_fixed_muid_detectors.empty())
            fix_muid_2planes();
        }
//      else
//        {
//
//          fix_mutr_all();
//
//        }

      register_fixed_detectors();
      print_fixed_parameters(cout);

    } // if (get_flag(DO_ALIGNMENT))

  MUTOO::PRINT(cout, "**");
}

//____________________________________________________________________
void
FvtxGlobalAlign::end(void)
{

  MUTOO::PRINT(cout,
      "FvtxGlobalAlign::end - global minimization and output results");

  if (get_flag(USE_MUTR_HITS))
    check_w_sign_mutr_halfocts();

  // end minimize
  if (get_flag(DO_ALIGNMENT))
    {
      end_minimize();// run milipede magic

      export_misalignment(_dumpfile_name);

//      PHTFileServer::get().cd(_misalignment_filename);
//      _alignment_tree->Write();

      PHTFileServer::get().cd(_misalignment_filename);

      const int N = MILLEPEDE::NGLB;
      TH1F * hGlobalParamFill = new TH1F("hGlobalParamFill",
          "Filled Global Parameters;Detector Parameter Index;Num of Tracks", N, -.5, N - .5);

      for (int i = 0; i < N; i++)
        {
          hGlobalParamFill->SetBinContent(i + 1, (double) (_n_filled_par[i]));
        }
      //  hGlobalParamFill->Write();

      PHTFileServer::get().write(_misalignment_filename);

    }

  // close evaluation tfile
  if (get_flag(DO_ALIGN_DST))
    {
      assert(_AlignDST_tree);
      PHTFileServer::get().cd(_AlignDST_filename);
      _AlignDST_tree->Write();

      if (_h_beam_x)
        _h_beam_x->Write();
      if (_h_beam_y)
        _h_beam_y->Write();

//      PHTFileServer::get().write(_AlignDST_filename);
    }

  // always clean up
  if (_AlignDST_tree)
    {
      _AlignDST_tree->ResetBranchAddresses();

      if (_track)
        delete _track;
      _track = NULL;
    }

  MUTOO::PRINT(cout, "**");

}

//____________________________________________________________________
void
FvtxGlobalAlign::event(void)
{

  if (Verbosity() >= 1)
    MUTOO::TRACE("FvtxGlobalAlign::event");

  if (!_track)
    cout << "FvtxGlobalAlign::event - Error - track object was not initialized."
        << endl;

  if (!process_evt())
    return;
  else
    {
      _n_events_analyzed++;
    }

  if (!get_flag(DO_ALIGNMENT) and !get_flag(DO_ALIGN_DST))
    {
      // no need to run track analysis
      return;
    }

  fill_container();

  _track_index = -1;
  _n_good_trk = 0;

  TFvtxTrkMap::iterator trk_iter(_trk_map->range());

  if (Verbosity() >= 1)
    cout << "------------------------------------------" << endl
        << "FvtxGlobalAlign::event - " << "Processing run " << _run_num
        << " event " << _event_num << " total FVTX track count : "
        << trk_iter.count() << endl;

  while (TFvtxTrkMap::pointer trk_ptr = trk_iter.next())
    {

      _track_index++;

      // never save ghost or reco failed tracks
      if (trk_ptr->get()->get_ghost() || !trk_ptr->get()->get_reco_success())
        continue;

      const std::vector<TMutTrkPar> &trk_par_vect(
          *trk_ptr->get()->get_trk_par_list());
      if (!trk_par_vect.size())
        return;

      _n_tracks_total++;

      if (Verbosity() >= 1)
        cout << "------------------------------------------" << endl
            << "FvtxGlobalAlign::event - " << "Processing run " << _run_num
            << " event " << _event_num << " track #" << _track_index << endl;

      bool acp_track = process_trk(trk_ptr);
      if (acp_track)
        {
          _n_tracks_analyzed++;

          _n_good_trk++;
        }
      //          const bool accepted(accept_trk(trk_ptr));
      if (acp_track && // whether process_trk think this track is OK
          (!_track->reject_track || !get_flag(USE_CUTS)) // use cuts?
          && get_flag(DO_ALIGNMENT) // do alignment?
              )
        {
          // perform local minimization
          if (is_TForwardMPTrack())
            {
              assert(get_n_track_para() == 8);
              TForwardMPTrack * ftrack = (TForwardMPTrack *) (_track);
              assert(ftrack);
              MILLEPEDE_Fit(ftrack, Verbosity());
            }
          else
            {
              assert(get_n_track_para() <= 4);
              MILLEPEDE_Fit(_track, Verbosity());
            }

          _n_tracks_minimized++;
        }

    } //   while (TFvtxTrkMap::pointer trk_ptr = trk_iter.next())

  if (get_flag(USE_MUTR_ALONE_TRACK) && _trk_mutr_map)
    {

      _track_index = 100 - 1;
      TMutTrkMap::iterator trk_iter(_trk_mutr_map->range());

      if (Verbosity() >= 1)
        cout << "FvtxGlobalAlign::event (MuTr Alone) - " << "Processing run "
            << _run_num << " event " << _event_num << " with "
            << trk_iter.count() << " MuTr tracks." << endl;

      if (Verbosity() >= 1)
        cout << "------------------------------------------" << endl
            << "FvtxGlobalAlign::event - " << "Processing run " << _run_num
            << " event " << _event_num << " total MuTr track count : "
            << trk_iter.count() << endl;

      while (TMutTrkMap::pointer trk_ptr = trk_iter.next())
        {

          _track_index++;

          // never save ghost or reco failed tracks
          if (trk_ptr->get()->get_ghost()
              || !trk_ptr->get()->get_reco_success())
            continue;

          const std::vector<TMutTrkPar> &trk_par_vect(
              *trk_ptr->get()->get_trk_par_list());
          if (!trk_par_vect.size())
            return;

          _n_tracks_total++;

          if (Verbosity() >= 1)
            cout << "------------------------------------------" << endl
                << "FvtxGlobalAlign::event (MuTr Alone) - " << "Processing run "
                << _run_num << " event " << _event_num << " mutr track #"
                << _track_index << endl;

          bool acp_track = process_trk(trk_ptr);
          if (acp_track)
            {
              _n_tracks_analyzed++;

              _n_good_trk++;
            }
          //          const bool accepted(accept_trk(trk_ptr));
          if (acp_track && // whether process_trk think this track is OK
              (!_track->reject_track || !get_flag(USE_CUTS)) // use cuts?
              && get_flag(DO_ALIGNMENT) // do alignment?
                  )
            {
              // perform local minimization
//              MILLEPEDE_Fit(_track, Verbosity());

              if (is_TForwardMPTrack())
                {
                  assert(get_n_track_para() == 8);
                  TForwardMPTrack * ftrack = (TForwardMPTrack *) (_track);
                  assert(ftrack);
                  MILLEPEDE_Fit(ftrack, Verbosity());
                }
              else
                {
                  assert(get_n_track_para() <= 4);
                  MILLEPEDE_Fit(_track, Verbosity());
                }

              _n_tracks_minimized++;
            }

        } //   while (TFvtxTrkMap::pointer trk_ptr = trk_iter.next())
    }

  return;

}

//____________________________________________________________________
void
FvtxGlobalAlign::minimize_alignment_DST(TTree * AlignDST_tree, int nEvent)
{

  _AlignDST_tree_read = AlignDST_tree;
  if (!_AlignDST_tree_read)
    {
      cout
          << "FvtxGlobalAlign::minimize_alignment_DST - Error - Cannot find AlignDST_tree."
          << endl;
      return;
    }

  // init in reading mode
  Int_t n_track = (Int_t) _AlignDST_tree_read->GetEntries();
  initialize_AlignDST_tree(true);

  if (nEvent <= 0)
    {
      cout << "FvtxGlobalAlign::minimize_alignment_DST - Process all events"
          << endl;
      nEvent = n_track;
    }
  else if (nEvent > n_track)
    nEvent = n_track;

  int ntrack_invalid = 0;

  cout << "FvtxGlobalAlign::minimize_alignment_DST - loop " << nEvent
      << " tracks" << endl;
//  if (Verbosity() >= 1)
//    _track->Print();
  for (Int_t i_track = 0; i_track < nEvent; i_track++)
    {
      assert(_AlignDST_tree_read);
      _AlignDST_tree_read->GetEntry(i_track);

      assert(_track);

      if (i_track == 0)
        {

          cout << "FvtxGlobalAlign::minimize_alignment_DST - first event: "
              << "_track->Class()->GetName()=" << _track->Class()->GetName()
              << "; " << "track->ClassName()=" << _track->ClassName() << "; "
              << "_track->Class()->Class_Name()::Class_Version()="
              << _track->Class_Name() << "::" << _track->Class_Version() << "; "
              << endl;

          assert(string(_track->Class_Name()) == string("TFvtxMPTrack"));
          cout
              << "FvtxGlobalAlign::minimize_alignment_DST - _AlignDST_tree_read->Show(0)"
              << endl;
          _AlignDST_tree_read->Show(i_track);
        }

      _n_tracks_total++;

      if (Verbosity() >= 1)
        _track->Print();

      if (get_flag(RECAL_CONSTRAINT))
        {
          process_after_filling();
//          accept_trk();
        }

//      bool accept = true;
//      if (get_flag(USE_CUTS))
//        accept = accept_trk();

      if (!_track->reject_track || !get_flag(USE_CUTS))
        {

          if (_track->IsValid())
            {

              if (get_flag(DO_ALIGN_DST))
                {
                  _AlignDST_tree->Fill();
                }

              if (Verbosity() >= MUTOO::ALOT)
                _track->Print();

              if (get_flag(DO_ALIGNMENT))
                {
                  if (is_TForwardMPTrack())
                    {
                      assert(get_n_track_para() == 8);
                      TForwardMPTrack * ftrack = (TForwardMPTrack *) (_track);
                      assert(ftrack);
                      MILLEPEDE_Fit(ftrack, Verbosity());
                    }
                  else
                    {
                      assert(get_n_track_para() <= 4);
                      MILLEPEDE_Fit(_track, Verbosity());
                    }
                  _n_tracks_minimized++;
                }
            }
          else //           if (_track->IsValid())
            {
              ntrack_invalid++;
              cout << "FvtxGlobalAlign::minimize_alignment_DST - ERROR - "
                  << "Ignore invalid track. Stat: " << _n_tracks_total
                  << " tracks analyzed. " << _n_tracks_minimized
                  << " used for minimization." << ntrack_invalid << " invalid."
                  << endl;
              _track->Print();
            }
        }

      _track->Clear();

      if (FVTXOO::special_event_num(_n_tracks_total))
        {
          cout << "FvtxGlobalAlign::minimize_alignment_DST - "
              << _n_tracks_total << " tracks analyzed. " << _n_tracks_minimized
              << " used for minimization." << ntrack_invalid << " invalid."
              << endl;
        }
    } //   for (Int_t i_track = 0; i_track < nEvent; i_track++)

  end();
}

//____________________________________________________________________
// Unified event information processor, should call before process_trk and minimize_magnets_off
bool
FvtxGlobalAlign::process_evt()
{

  if (_run_header)
    _run_num = _run_header->get_RunNumber();
  else
    {
      recoConsts *rc = recoConsts::instance();

      _run_num = rc->get_IntFlag("RUNNUMBER");
    }

  if (_event_header)
    _event_num = _event_header->get_EvtSequence();
  else
    _event_num = 0;

  _track_index = -1;

  double vtxx = -999;
  double vtxy = -999;
  double vtxz = -999;
  _vtxxp = -999;
  _vtxyp = -999;
  _vtxzp = -999;
  _bbcz = -999;

  if (_vtxout_node)
    {
      //cout << "vertex (" << (_vtxout_node->get_Vertex()).getX() << ", " << (_vtxout_node->get_Vertex()).getY() << ", " << (_vtxout_node->get_Vertex()).getZ() << ")" << endl;
      //cout << "Simvertex " << (_vtxout_node->get_Vertex("SIM")).getX() << ", " << (_vtxout_node->get_Vertex("SIM")).getY() << ", " << (_vtxout_node->get_Vertex("SIM")).getZ() << endl;
      vtxx = _vtxout_node->get_Vertex("SVX").getX();
      vtxy = _vtxout_node->get_Vertex("SVX").getY();
      vtxz = _vtxout_node->get_Vertex("SVX").getZ();

      _vtxxp = _vtxout_node->get_Vertex(_vtx_vertex_name.c_str()).getX();
      _vtxyp = _vtxout_node->get_Vertex(_vtx_vertex_name.c_str()).getY();
      _vtxzp = _vtxout_node->get_Vertex(_vtx_vertex_name.c_str()).getZ();

      _bbcz = _vtxout_node->get_Vertex("BBC").getZ();
      if (isnan(_bbcz))
        _bbcz = _vtxout_node->get_Vertex("SIM").getZ();

    }

  // selection for _vtx_point point
  if (isnan(_vtxxp) or isnan(_vtxyp) or isnan(_vtxzp) or abs(_vtxzp) > 50)
    {
      if (isnan(vtxz))
        _vtx_point.setZ(_bbcz);
      else
        _vtx_point.setZ(vtxz);

      _vtx_point.setX(0);
      _vtx_point.setY(0);
    }
  else
    {
      _vtx_point.setZ(_vtxzp);
      _vtx_point.setX(_vtxxp);
      _vtx_point.setY(_vtxyp);
    }

  // load average beam x-y position
  if (_beam_pos_xy.have_run(_run_num))
    {

      _vtx_point.setX(_beam_pos_xy.get_x(_run_num));
      _vtx_point.setY(_beam_pos_xy.get_y(_run_num));

    }
  else
    {
//      _vtx_point.setX(0);
//      _vtx_point.setY(0);

      if (get_flag(USE_SVTX_CONSTRAINT) && get_flag(DO_ALIGNMENT)
          && _vtx_vertex_name == "SVX_PRECISE")
        {
          cout << "FvtxGlobalAlign::process_evt() - Error -"
              << "cannot find average vertex x-y for run " << _run_num << endl;
        }
    }

  _vtxx = _vtx_point.getX();
  _vtxy = _vtx_point.getY();
  _vtxz = _vtx_point.getZ();

  if (Verbosity() >= 1)
    cout << "bbcz = " << _bbcz << ", "
        // bbc
        << "vtx (SVX) = (" << vtxx << ", " << vtxy << ", " << vtxz << "), "
        // vtx
        << "vtx (Best) = (" << _vtxx << ", " << _vtxy << ", " << _vtxz << "), "
        // vtx
        << "vtxp (" << _vtx_vertex_name << ") = (" << _vtxxp << ", " << _vtxyp
        << ", " << _vtxzp << "), "
        // vtxp
        << "using vertex = (" << _vtx_point.getX() << ", " << _vtx_point.getY()
        << ", " << _vtx_point.getZ() << "), " // vtxp
        << endl;

  // check mutr tracks
  if (get_flag(USE_MUTR_HITS) || get_flag(USE_MUTR_ALONE_TRACK))
    _n_good_muon_trk = inspect_mutr();

  //  accept event?
  _accept_evt = accept_evt();

  if (!_accept_evt and get_flag(USE_CUTS))
    {
      return false;
    }

  if (get_flag(DO_ALIGN_DST))
    {
      if (!_h_beam_x)
        {
          PHTFileServer::get().cd(_AlignDST_filename);

          _h_beam_x = new TH1F("h_beam_x", "Average Beam X;Average Beam X (cm)",
              400, -1, 1);
        }
      _h_beam_x->Fill(_vtxxp);

      if (!_h_beam_y)
        {
          PHTFileServer::get().cd(_AlignDST_filename);

          _h_beam_y = new TH1F("h_beam_y", "Average Beam Y;Average Beam Y (cm)",
              400, -1, 1);
        }
      _h_beam_y->Fill(_vtxyp);
    }

  return true;
}

//____________________________________________________________________
// Unified track information processor including filling alignment tree
bool
FvtxGlobalAlign::process_trk(TMutTrkMap::pointer trk_ptr)
{

  //-----------------------------------------
  // initialize the track object
  //-----------------------------------------
  if (!_track)
    {
      cout
          << "FvtxGlobalAlign::process_trk(TMutTrkMap::pointer) - Error - the track object was not initialized."
          << endl;

      return false;
    }

  _track->Clear();

  //-----------------------------------------
  // Check associations on FVTX hits/VTX hits/MuTr tracks
  //-----------------------------------------
  TMutCoordMap::key_iterator coord_iter = trk_ptr->get() //
  ->get_associated<TMutCoord>();
  TFvtxTrkMap::key_iterator trk_fvtx_iter = trk_ptr->get() //
  ->get_associated<TFvtxTrk>();
  const size_t n_trk_fvtx = trk_fvtx_iter.count();
  TMuiRoadMapO::const_key_iterator mui_iter = trk_ptr->get() //
  ->get_associated<TMuiRoadO>();
  const TMutTrkPar * trk_par_kalman = trk_ptr->get()->get_trk_par_station(0);

  if (Verbosity() >= FVTXOO::ALOT)
    {
      cout
          << "FvtxGlobalAlign::process_trk(TMutTrkMap::pointer) - Info - process track"
          << "\t run_num " << _run_num //
          << "\t event_num " << _event_num //
          << "\t track_index " << _track_index //
          << "\t n_HIT " << coord_iter.count() //
          << "\t n_trk_fvtx " << n_trk_fvtx //
          << "\t n_muID " << mui_iter.count() //
          << endl;
    }

  if (coord_iter.count() <= 0)
    {
      cout
          << "FvtxGlobalAlign::process_trk(TMutTrkMap::pointer) - Error - one track without MuTr hits: "
          << "\t run_num " << _run_num //
          << "\t event_num " << _event_num //
          << "\t track_index " << _track_index //
          << endl;

      return false;
    }
  if (n_trk_fvtx > 1)
    {
      cout
          << "FvtxGlobalAlign::process_trk(TMutTrkMap::pointer) - Error - more than one fvtx track associated with this track: "
          << "\t run_num " << _run_num //
          << "\t event_num " << _event_num //
          << "\t track_index " << _track_index //
          << endl;

      return false;
    }

  if (get_flag(USE_MUTR_HITS) && n_trk_fvtx >= 1) // already analyzed with fvtx tracks
    {
      if (Verbosity() >= FVTXOO::ALOT)
        {
          cout
              << "FvtxGlobalAlign::process_trk(TMutTrkMap::pointer) - Info - should already analyzed with FVTX track"
              << endl;
        }
      return false;
    }

  if (!trk_par_kalman)
    {
      cout
          << "FvtxGlobalAlign::process_trk(TMutTrkMap::pointer) - Error - Missing Fit @ station 0"
          << "\t run_num " << _run_num //
          << "\t event_num " << _event_num //
          << "\t track_index " << _track_index //
          << endl;

      return false;
    }

  _integrator.initialize(*trk_par_kalman);

  //-----------------------------------------
  // load event property
  //-----------------------------------------
  _track->run_num = _run_num;
  _track->event_num = _event_num;
  _track->track_index = _track_index;
//  _track->vtx_point = TVector3( _vtx_point.getX(),_vtx_point.getY(),_vtx_point.getZ());
  _track->vtx_point = convert(_vtx_point);
  _track->vtxzp = _vtxzp;
  _track->bbcz = _bbcz;
  _track->trk_par_kalman = (*trk_par_kalman);
//  _track->p_kalman.SetXYZ(trk_ptr->get()->get_trk_par_vtx()->get_px(), //X
//      trk_ptr->get()->get_trk_par_vtx()->get_py(), //Y
//      trk_ptr->get()->get_trk_par_vtx()->get_pz() //Z
//      );
  _track->reject_event = !_accept_evt;

  //-----------------------------------------
  //generic track property
  //-----------------------------------------
  _track->use_kalman_fit = !get_flag(USE_MILLEPEDE_TRACK_FIT);
  _track->chi_square_mutr = trk_ptr->get()->get_chi_square();
  _track->ndf_mutr = trk_ptr->get()->get_ndf();
  _track->trk_par_kalman_mutr = *trk_par_kalman;

  const double z_ref = MUTOO::SIGN(trk_par_kalman->get_pz()) * _z_ref;
  _track->z_ref = z_ref;

  const std::vector<TMutTrkPar> &trk_par_vect(
      *trk_ptr->get()->get_trk_par_list());
//   check fit parameters size
  if (!trk_par_vect.size())
    return false;

  if (!get_flag(TRACK_LATCON_FIT))
    {
      // pick up a set of track parameter, for instance we choose arbitrally le param of dect0
      _track->z0_fit = trk_par_vect.front().get_z();
    }
  else
    {
      // use 3-D fit with lateral constrains at vertex
      _track->z0_fit = _vtx_point.getZ();
    }

  //-----------------------------------------
  // add nodes
  //-----------------------------------------
  try
    {
//      fill_mutr_disp_vtx();

      //-----------------------------------------
      // add MuTr coordinates
      //-----------------------------------------
      _n_mutr_tracks_analyzed++;

      while (TMutCoordMap::pointer coord_ptr = coord_iter.next())
        {

          process_mutr_coord(NULL, trk_ptr, coord_ptr);

        }

      //-----------------------------------------
      // process MuID hits
      //-----------------------------------------
      process_muid_road(mui_iter, trk_ptr);

    }
  catch (std::exception &e)
    {

      cout
          << "FvtxGlobalAlign::process_trk(TMutTrkMap::pointer) - Error - failed when filling track nodes : "
          << e.what() << endl;
      return false;

    }

  _track->process_mutr_only_track();

  process_after_filling();

  //-----------------------------------------
  // whether it is accepted for MILLEPEDE analysis
  //-----------------------------------------
  const bool accept = accept_trk_mutr();

  if (!(accept || !get_flag(USE_CUTS)))
    return false;

  //-----------------------------------------
  // Save the track information to DST or screen
  //-----------------------------------------
  if (get_flag(DO_ALIGN_DST))
    {
      _AlignDST_tree->Fill();
    }

  if (Verbosity() >= MUTOO::ALOT)
    _track->Print();

  return true;
}

//____________________________________________________________________
// Unified track information processor including filling alignment tree
bool
FvtxGlobalAlign::process_trk(TFvtxTrkMap::pointer trk_ptr)
{

//  if (_event_num == 6454)
//    Verbosity() = 2;
//  else
//    Verbosity() = 0;

  //-----------------------------------------
  // initialize the track object
  //-----------------------------------------
  if (!_track)
    {
      cout
          << "FvtxGlobalAlign::process_trk - Error - the track object was not initialized."
          << endl;

      return false;
    }

  string track_class(_track->ClassName());
  if (Verbosity() >= FVTXOO::ALOT)
    {
      cout << "FvtxGlobalAlign::process_trk - Info - track class name "
          << track_class << endl;
    }
  assert(track_class == _track_class);

  if (Verbosity() >= FVTXOO::ALOT)
    {
      cout << "FvtxGlobalAlign::process_trk - Info - clearing track ..."
          << endl;
    }

  _track->Clear();

  if (Verbosity() >= FVTXOO::ALOT)
    {
      cout << "FvtxGlobalAlign::process_trk - Info - track inspection ..."
          << endl;
    }
  //-----------------------------------------
  // Check associations on FVTX hits/VTX hits/MuTr tracks
  //-----------------------------------------
  TFvtxCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<
      TFvtxCoord>();
  TFvtxSvxClusterMap::key_iterator trk_svx_iter =
      trk_ptr->get()->get_associated<TFvtxSvxCluster>();
  TMutTrkMap::key_iterator trk_mutr_iter = trk_ptr->get()->get_associated<
      TMutTrk>();
  const size_t n_trk_mutr = trk_mutr_iter.count();
  const TMutTrkPar* fvtxmutr_trk_par = trk_ptr->get()->get_trk_par_mutr();
  double p_tot = 0;
  if (fvtxmutr_trk_par)
    p_tot = fvtxmutr_trk_par->get_ptot();

  if (coord_iter.count() <= 0)
    {
      cout
          << "FvtxGlobalAlign::process_trk - Error - one track without FVTX hits: "
          << "\t run_num " << _run_num //
          << "\t event_num " << _event_num //
          << "\t track_index " << _track_index //
          << endl;

      return false;
    }
  if (n_trk_mutr > 1)
    {
      cout
          << "FvtxGlobalAlign::process_trk - Error - more than one mutr track associated with this FVTX track: "
          << "\t run_num " << _run_num //
          << "\t event_num " << _event_num //
          << "\t track_index " << _track_index //
          << endl;

      return false;
    }
  if (n_trk_mutr > 0 && p_tot == 0)
    {
      cout
          << "FvtxGlobalAlign::process_trk - Error - given matching MuTr track, but without joint track fit for FVTX track: "
          << "\t run_num " << _run_num //
          << "\t event_num " << _event_num //
          << "\t track_index " << _track_index //
          << endl;

      return false;
    }

  if (!get_flag(USE_FVTX_ALONE_TRACK) // ignore FVTX alone track
  && (trk_svx_iter.count() == 0 || !get_flag(USE_VTX_HITS)) // no VTX
      && (n_trk_mutr == 0 || !get_flag(USE_MUTR_HITS)) // no MuTr
      )
    {
      if (Verbosity() >= FVTXOO::ALOT)
        {
          cout
              << "FvtxGlobalAlign::process_trk - Info - one track not fit the need: "
              << "\t run_num " << _run_num //
              << "\t event_num " << _event_num //
              << "\t track_index " << _track_index //
              << "\t n_FVTX " << coord_iter.count() //
              << "\t n_VTX " << trk_svx_iter.count() //
              << "\t n_MuTr_trk " << n_trk_mutr //
              << endl;
        }
      return false;
    }
  else
    {

      if (Verbosity() >= FVTXOO::ALOT)
        {
          cout
              << "FvtxGlobalAlign::process_trk - Info - one track with proper hits: "
              << "\t run_num " << _run_num //
              << "\t event_num " << _event_num //
              << "\t track_index " << _track_index //
              << "\t n_FVTX " << coord_iter.count() //
              << "\t n_VTX " << trk_svx_iter.count() //
              << "\t n_MuTr_trk " << n_trk_mutr //
              << endl;
        }

    }

  const TMutTrkPar * trk_par_kalman = get_kalman_fit(trk_ptr);
  const double z_ref = MUTOO::SIGN(trk_par_kalman->get_pz()) * _z_ref;
  _track->z_ref = z_ref;

  if (Verbosity() >= FVTXOO::ALOT)
    {
      cout << "FvtxGlobalAlign::process_trk - track fit use :" << endl;
      trk_par_kalman->print(cout);
    }

  //-----------------------------------------
  // load event property
  //-----------------------------------------
  _track->run_num = _run_num;
  _track->event_num = _event_num;
  _track->track_index = _track_index;
//  _track->vtx_point = TVector3( _vtx_point.getX(),_vtx_point.getY(),_vtx_point.getZ());
  _track->vtx_point = convert(_vtx_point);
  _track->vtxzp = _vtxzp;
  _track->bbcz = _bbcz;
  _track->trk_par_kalman = (*trk_par_kalman);
  _track->trk_par_kalman_mutr = (*trk_par_kalman); // ! save same thing to MuTr trk par too, so it would not be invalid
//  _track->p_kalman.SetXYZ(trk_ptr->get()->get_trk_par_vtx()->get_px(), //X
//      trk_ptr->get()->get_trk_par_vtx()->get_py(), //Y
//      trk_ptr->get()->get_trk_par_vtx()->get_pz() //Z
//      );
  _track->reject_event = !_accept_evt;

  //-----------------------------------------
  //generic track property
  //-----------------------------------------
  _track->use_kalman_fit = !get_flag(USE_MILLEPEDE_TRACK_FIT);
  _track->chi_square_fvtx = trk_ptr->get()->get_chi_square();
  _track->ndf_fvtx = trk_ptr->get()->get_ndf();

  const std::vector<TMutTrkPar> &trk_par_vect(
      *trk_ptr->get()->get_trk_par_list());
//   check fit parameters size
  if (!trk_par_vect.size())
    return false;

  if (!get_flag(USE_SVTX_CONSTRAINT))
    {
      // pick up a set of track parameter, for instance we choose arbitrally le param of dect0
      _track->z0_fit = trk_par_vect.front().get_z();
//      _track->z0_fit = 20 * TMath::Sign( _track->trk_par_kalman.get_pz() ); // front disk
    }
  else
    {
      // use 3-D fit with lateral constrains at vertex
      _track->z0_fit = _vtx_point.getZ();
    }

  //-----------------------------------------
  // add nodes
  //-----------------------------------------
  try
    {

      _integrator.initialize(*trk_par_kalman);

      //-----------------------------------------
      // add VTX clusters
      //-----------------------------------------
      if (get_flag(USE_VTX_HITS))
        while (TFvtxSvxClusterMap::const_pointer trk_svx_ptr =
            trk_svx_iter.next())
          {
            process_vtx_cluster(trk_ptr, trk_svx_ptr, true);
            process_vtx_cluster(trk_ptr, trk_svx_ptr, false);
          }

      //-----------------------------------------
      // add FVTX coordinates
      //-----------------------------------------
      while (TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next())
        {
          process_fvtx_coord(trk_ptr, coord_ptr);
        }

      // calculate basic FVTX information for the track
      _track->process_fvtx_track();

      //-----------------------------------------
      // add MuTr coordinates
      //-----------------------------------------
      if (get_flag(USE_MUTR_HITS) and n_trk_mutr > 0)
//        while (TMutTrkMap::const_pointer trk_mutr_ptr = trk_mutr_iter.next())
          {
            TMutTrkMap::const_pointer trk_mutr_ptr = trk_mutr_iter.next();

            _track->trk_par_kalman_mutr =
                *(trk_mutr_ptr->get()->get_trk_par_station(0));


            // fix the difference in the amplitude of momentum
            if (get_flag(SCALE_MUTR_KALMAN_MOMENTUM))
              {
                TMutTrkPar extrap_trk_par;
                get_kalman_fit(_track->trk_par_kalman_mutr.get_z(),
                    extrap_trk_par);
                PHVector p_mutr = _track->trk_par_kalman_mutr.get_momentum();
                PHVector p_fvtxmutr = extrap_trk_par.get_momentum();

                p_mutr = p_mutr * (p_fvtxmutr.length() / p_mutr.length());

                _track->trk_par_kalman_mutr.set_px(p_mutr.getX());
                _track->trk_par_kalman_mutr.set_py(p_mutr.getY());
                _track->trk_par_kalman_mutr.set_pz(p_mutr.getZ());
              }

            _n_mutr_tracks_analyzed++;
            _track->chi_square_mutr = trk_mutr_ptr->get()->get_chi_square();
            _track->ndf_mutr = trk_mutr_ptr->get()->get_ndf();
            _integrator.initialize(_track->trk_par_kalman_mutr);

            TMutCoordMap::key_iterator coord_mutr_iter =
                trk_mutr_ptr->get()->get_associated<TMutCoord>();

            while (TMutCoordMap::pointer coord_ptr = coord_mutr_iter.next())
              {

                process_mutr_coord(trk_ptr, trk_mutr_ptr, coord_ptr);

              }

            //-----------------------------------------
            // process MuID hits
            //-----------------------------------------
            TMuiRoadMapO::const_key_iterator mui_iter = trk_mutr_ptr->get() //
            ->get_associated<TMuiRoadO>();
            process_muid_road(mui_iter, trk_mutr_ptr);
          }

      process_after_filling();

    }
  catch (std::exception &e)
    {

      cout
          << "FvtxGlobalAlign::process_trk - Error - failed when filling track nodes : "
          << e.what() << endl;
      return false;

    }

  //-----------------------------------------
  // whether it is accepted for MILLEPEDE analysis
  //-----------------------------------------
  const bool accept = accept_trk();

  if (!(accept || !get_flag(USE_CUTS)))
    return false;

  if (get_flag(USE_MUTR_HITS) and n_trk_mutr > 0)
    {
      // check again for the MuTr part

      const bool accept = accept_trk_mutr();

      if (!(accept || !get_flag(USE_CUTS)))
        return false;

    }

  //-----------------------------------------
  // Save the track information to DST or screen
  //-----------------------------------------
  if (get_flag(DO_ALIGN_DST))
    {
      _AlignDST_tree->Fill();
    }

  if (Verbosity() >= MUTOO::ALOT)
    _track->Print();

  return true;
}

//____________________________________________________________________
int
FvtxGlobalAlign::inspect_mutr()
{

  if (!_trk_mutr_map)
    {
      if (Verbosity() >= FVTXOO::ALOT)
        cout << "FvtxGlobalAlign::inspect_mutr() - cannot find TMutTrkMap"
            << endl;
      return 0;
    }

  TMutTrkMap::iterator trk_iter(_trk_mutr_map->range());

  if (Verbosity() >= FVTXOO::ALOT)
    cout << "FvtxGlobalAlign::inspect_mutr() - Event " << _event_num << " - "
        << "Found " << trk_iter.count() << " MuTr tracks, ";
  if (_coord_mutr_map)
    {

      TMutCoordMap::iterator cood_iter(_coord_mutr_map->range());
      if (Verbosity() >= FVTXOO::ALOT)
        cout << cood_iter.count() << " TMutCoord.";

    }
  else
    {
      if (Verbosity() >= FVTXOO::ALOT)
        cout << "cannot find TMutCoordMap!";
    }
  if (Verbosity() >= FVTXOO::ALOT)
    cout << endl;

  int track_index = -1;
  int good_track = 0;
  while (TMutTrkMap::pointer trk_ptr = trk_iter.next())
    {

      track_index++;
      _n_mutr_tracks_total++;

      TMutCoordMap::const_key_iterator coord_iter =
          trk_ptr->get()->get_associated<TMutCoord>();
      const TMutTrkPar * trk_par = trk_ptr->get()->get_trk_par_vtx();

      if (Verbosity() >= FVTXOO::ALOT)
        cout << "------- Track " << track_index << " - " << coord_iter.count()
            << " TMutCoord ";

      if (trk_ptr->get()->get_ghost())
        {
          if (Verbosity() >= FVTXOO::ALOT)
            cout << " - Ghots" << endl;
        }
      else if (!trk_ptr->get()->get_reco_success())
        {
          if (Verbosity() >= FVTXOO::ALOT)
            cout << " - Reco Failed" << endl;
        }
      else if (trk_ptr->get()->get_no_estimate())
        {
          if (Verbosity() >= FVTXOO::ALOT)
            cout << " - No estimate" << endl;
        }
      else if (trk_ptr->get()->get_low_mom())
        {
          if (Verbosity() >= FVTXOO::ALOT)
            cout << " - LOW_MOM" << endl;
        }
      else
        {
          good_track++;
          if (Verbosity() >= FVTXOO::ALOT)
            {
              cout << "with vertex par: " << endl;
              trk_par->print();
            }
        }
    }
  if (Verbosity() >= FVTXOO::ALOT)
    cout << "------- " << good_track << " / " << trk_iter.count()
        << " Tracks are good -------" << endl;

  return good_track;
}

//____________________________________________________________________
void
FvtxGlobalAlign::process_fvtx_coord(TFvtxTrkMap::pointer trk_ptr,
    TFvtxCoordMap::const_pointer coord_ptr)
{

  TFvtxMPNode_FVTX * node = dynamic_cast<TFvtxMPNode_FVTX *>(_track->add_node(
      TFvtxMPNode::FVTX));
  assert(node);

  if (Verbosity() >= FVTXOO::ALOT)
    cout << "FvtxGlobalAlign::process_fvtx_coord - process coord @ z = "
        << coord_ptr->get()->get_coord_midpoint().getZ() << endl;

  // Hit ID
  node->arm = coord_ptr->get()->get_arm();
  node->cage = coord_ptr->get()->get_cage();
  node->station = coord_ptr->get()->get_station();
  node->sector = coord_ptr->get()->get_sector();
  node->column = coord_ptr->get()->get_column();
  node->strip = coord_ptr->get()->get_peak_strip();

  TFvtxClusMap::key_iterator clus_iter = coord_ptr->get()->get_associated<
      TFvtxClus>();
  TFvtxClusMap::const_pointer clus_ptr = clus_iter.current();
  TFvtxHitMap::key_iterator hit_iter =
      clus_ptr->get()->get_associated<TFvtxHit>();
  node->hit_size = hit_iter.count();

  node->q_total = 0;
  while (TFvtxHitMap::const_pointer hit_ptr = hit_iter.next())
    {
      node->q_total += hit_ptr->get()->get_q();
    }

  // Measurement data
  node->meas = (coord_ptr->get()->get_w_absolute());
  node->sigma = coord_ptr->get()->get_error() * _fvtx_hit_sigma_factor;
  node->p_det = convert(coord_ptr->get()->get_coord_midpoint());
  node->p_strip_begin = convert(coord_ptr->get()->get_coord_begin());
  node->p_strip_end = convert(coord_ptr->get()->get_coord_end());

  node->half_angle = get_half_angle(coord_ptr->get()->get_arm(),
      coord_ptr->get()->get_cage(), coord_ptr->get()->get_station(),
      coord_ptr->get()->get_sector(), coord_ptr->get()->get_column(),
      coord_ptr->get()->get_peak_strip());

  // implicitly defined the sign for w
  const double cos_phi = -sin(node->half_angle);
  const double sin_phi = cos(node->half_angle);
  const double z_det = node->p_det.Z();

  // Derivatives for internal fits

  node->dwdx = (cos_phi);
//  node->dwdtx = (cos_phi * (z_det - _track->z0_fit));
  node->dwdy = (sin_phi);
//  node->dwdty = (sin_phi * (z_det - _track->z0_fit));
  node->fill_slope_derivatives(z_det, _track->z0_fit);

//  // Kalman fit
//  // find best matching track parameters
//  // DONE: 1. should use PHTrackIntegratorKF to extrapolate track; 2. should use best MuTr-FVTX fit
//  const TMutTrkPar trk_par(
//      *min_element(trk_par_vect.begin(), trk_par_vect.end(),
//          closest_z_ftor(z_det)));
//  const double z_fit = trk_par.get_z(); // z of Kalman fit point
//
//  const double x_not_extrapo = trk_par.get_x();
//  const double y_not_extrapo = trk_par.get_y();
//
//  const double tx = (trk_par.get_px()) / (trk_par.get_pz());
//  const double ty = (trk_par.get_py()) / (trk_par.get_pz());
//  const double x_fit = x_not_extrapo + tx * (z_det - z_fit);
//  const double y_fit = y_not_extrapo + ty * (z_det - z_fit);
//
//  node->fit_kalman = node->get_measurement(x_fit, y_fit);
//  node->residu_kalman = node->meas - node->fit_kalman;
//  node->p_kalman = TVector3(x_fit, y_fit, z_det);

//  TMutTrkPar extrap_trk_par;
//  get_kalman_fit(node->p_det.z(), extrap_trk_par);
//  node->fill_kalman_fit(extrap_trk_par.get_x(), extrap_trk_par.get_y());

  // Kalman fit
  fill_node_kalman_fit(node);

//  node->wrt_z = (cos_phi
//      * (node->momentum_kalman.X() / node->momentum_kalman.Z())
//      + sin_phi * (node->momentum_kalman.Y() / node->momentum_kalman.Z()));
//  node->wrt_phi = (sin_phi * node->p_kalman.X() - cos_phi * node->p_kalman.Y());

}

//____________________________________________________________________
//! add a TMutCoord to internal track object (TFvtxMPTrack * _track)
void
FvtxGlobalAlign::process_mutr_coord( //
    TFvtxTrkMap::pointer /*trk_ptr*/, //
    TMutTrkMap::const_pointer /*trk_mutr_ptr*/, //
    TMutCoordMap::pointer coord_ptr //
    )
{

  TFvtxMPNode_MuTr * node = dynamic_cast<TFvtxMPNode_MuTr *>(_track->add_node(
      TFvtxMPNode::MuTr));
  assert(node);

  if (Verbosity() >= FVTXOO::ALOT)
    cout << "FvtxGlobalAlign::process_mutr_coord - process coord @ z = "
        << coord_ptr->get()->get_coord_midpoint().getZ() << endl;

  const int w_sign = get_w_sign_cor_mutr(coord_ptr->get()->get_arm(),
      coord_ptr->get()->get_station(), coord_ptr->get()->get_octant(),
      coord_ptr->get()->get_half_octant(), coord_ptr->get()->get_gap(),
      coord_ptr->get()->get_cathode(), coord_ptr->get()->get_peak_strip());

  node->arm = coord_ptr->get()->get_arm();
  node->station = coord_ptr->get()->get_station();
  node->octant = coord_ptr->get()->get_octant();
  node->half_octant = coord_ptr->get()->get_half_octant();
  node->gap = coord_ptr->get()->get_gap();
  node->cathode = coord_ptr->get()->get_cathode();
  node->strip = coord_ptr->get()->get_peak_strip();

  node->meas = w_sign * coord_ptr->get()->get_w_absolute();
  node->sigma = max((double) (coord_ptr->get()->get_error()),
      _mutr_hit_sigma_min);

  node->p_det = convert(coord_ptr->get()->get_coord_midpoint());
  node->p_strip_begin = convert(coord_ptr->get()->get_coord_begin());
  node->p_strip_end = convert(coord_ptr->get()->get_coord_end());

  // store projection matrix from state vector to measurement
  double angle(
      get_half_angle_mutr(coord_ptr->get()->get_arm(),
          coord_ptr->get()->get_station(), coord_ptr->get()->get_octant(),
          coord_ptr->get()->get_half_octant(), coord_ptr->get()->get_gap(),
          coord_ptr->get()->get_cathode(), coord_ptr->get()->get_peak_strip()));

  node->dwdx = -sin(angle);
  node->dwdy = cos(angle);
  node->fill_slope_derivatives(node->p_det.z(), _track->z0_fit, _track->z_ref);

  // Kalman fit
  fill_node_kalman_fit(node);

//  node->wrt_z = (node->dwdx
//      * (node->momentum_kalman.X() / node->momentum_kalman.Z())
//      + node->dwdy * (node->momentum_kalman.Y() / node->momentum_kalman.Z()));
//  node->wrt_phi = (sin_phi * node->p_kalman.X() - cos_phi * node->p_kalman.Y());
}

//____________________________________________________________________
//! add a TFvtxSvxCluster to internal track object (TFvtxMPTrack * _track)
void
FvtxGlobalAlign::process_vtx_cluster(TFvtxTrkMap::pointer trk_ptr, //
    TFvtxSvxClusterMap::const_pointer trk_svx_ptr, //
    bool switch_r_phi //
    )
{
  const SvxCluster *clus = trk_svx_ptr->get()->get_cluster();
  assert(clus);

  TFvtxMPNode_VTX * node = dynamic_cast<TFvtxMPNode_VTX *>(_track->add_node(
      TFvtxMPNode::VTX));
  assert(node);

  node->switch_r_phi = switch_r_phi;
  node->svxSection = (short) clus->get_svxSection();
  node->layer = (short) clus->get_layer();
  node->ladder = (short) clus->get_ladder();
  node->sensor = (short) clus->get_sensor();

  // Measurement data
  const double x_global = clus->get_xyz_global(0);
  const double y_global = clus->get_xyz_global(1);
  const double z_global = clus->get_xyz_global(2);

  node->p_det.SetXYZ(x_global, y_global, z_global);

  const double phi = node->p_det.Phi();

  const double angle_sensitive = switch_r_phi ? phi : phi - M_PI / 2;
//  double sigma = _svx_hit_sigma;
//      switch_r_phi ?
//          trk_svx_ptr->get()->get_r_error() :
//          trk_svx_ptr->get()->get_phi_error();
//  sigma = max(sigma, 20e-4); // set min sigma = 20um
  double sigma = switch_r_phi ? _svx_hit_sigma_r : _svx_hit_sigma_phi;

  node->dwdx = cos(angle_sensitive);
  node->dwdy = sin(angle_sensitive);
  node->fill_slope_derivatives(z_global, _track->z0_fit);

  node->meas = node->get_measurement(x_global, y_global);
  node->sigma = sigma;

  // Kalman fit
  fill_node_kalman_fit(node);

}

//____________________________________________________________________
void
FvtxGlobalAlign::process_muid_road(TMuiRoadMapO::const_key_iterator mui_iter,
    TMutTrkMap::const_pointer trk_ptr)
{

  TMuiRoadMapO::const_pointer best_mui_ptr = NULL;
  double min_DG0 = 9999;
  double tag_DDG0 = 9999;

  if (Verbosity() >= FVTXOO::ALOT)
    {
      cout
          << "FvtxGlobalAlign::process_trk(TMutTrkMap::pointer) - Info - checking "
          << mui_iter.count() << " MuID roads" << endl;
    }
  while (TMuiRoadMapO::const_pointer mui_ptr = mui_iter.next())
    {
      // ref to Tools::DG0, Tools::DDG0,

      const TMutTrkPar& track_par(trk_ptr->get()->get_trk_par_list()->back());
      const PHPoint gap0_point = mui_ptr->get()->get_gap0_point();

      const double x_mut = track_par.get_x();
      const double y_mut = track_par.get_y();
      const double z_mut = track_par.get_z();

      const double dxdz_mut = track_par.get_px() / track_par.get_pz();
      const double dydz_mut = track_par.get_py() / track_par.get_pz();

      // muid point at gap 0
      const double x_mui = gap0_point.getX();
      const double y_mui = gap0_point.getY();
      const double z_mui = gap0_point.getZ();

      const double DG0 = sqrt(
          MUTOO::SQUARE(x_mui - x_mut - dxdz_mut * (z_mui - z_mut))
              + MUTOO::SQUARE(y_mui - y_mut - dydz_mut * (z_mui - z_mut)));

      if (DG0 < min_DG0)
        {
          best_mui_ptr = mui_ptr;
          min_DG0 = DG0;

          // ref: MWGFvtxReco::do_muons
          const TMutFitPar* road_par = mui_ptr->get()->get_const_fitpar();
          if (road_par)
            {

              PHVector v_mutr(track_par.get_px(), track_par.get_py(),
                  track_par.get_pz());
              PHVector v_mui(road_par->get_dxdz(), road_par->get_dydz(), 1);
              if (track_par.get_pz() < 0)
                v_mui = v_mui * (-1);

              tag_DDG0 = v_mutr.angle(v_mui) * MUTOO::RAD_TO_DEG;

            }
        }
    }

  if (best_mui_ptr)
    {

      if (Verbosity() >= FVTXOO::ALOT)
        {
          cout
              << "FvtxGlobalAlign::process_trk(TMutTrkMap::pointer) - Info - matching MuID road: "
              << "get_depth = " << best_mui_ptr->get()->get_depth()
              << ", get_pass2_depth = "
              << best_mui_ptr->get()->get_pass2_depth() << ", get_gapbit = "
              << best_mui_ptr->get()->get_gapbit() << endl;
        }

      _track->muid_lastgap = best_mui_ptr->get()->get_depth();
      _track->dg0 = min_DG0;
      _track->ddg0 = tag_DDG0;

      if (get_flag(USE_MUID_HITS))
        {
          TMuiClusterMapO::key_iterator clus_iter(
              best_mui_ptr->get()->get_associated<TMuiClusterO>());
          while (TMuiClusterMapO::pointer clus_ptr = clus_iter.next())
            {

              process_muid_clus(NULL, trk_ptr, clus_ptr);

            }
        }

    }
}

//____________________________________________________________________
//! add a TMutCoord to internal track object (TFvtxMPTrack * _track)
void
FvtxGlobalAlign::process_muid_clus( //
    TFvtxTrkMap::pointer trk_ptr, //
    TMutTrkMap::const_pointer trk_mutr_ptr, //
    TMuiClusterMapO::pointer coord_ptr //
    )
{

  TFvtxMPNode_MuID * node = dynamic_cast<TFvtxMPNode_MuID *>(_track->add_node(
      TFvtxMPNode::MuID));
  assert(node);

  PHPoint midpoint = (coord_ptr->get()->get_coord_begin()
      + coord_ptr->get()->get_coord_end()) * .5;

  if (Verbosity() >= FVTXOO::ALOT)
    cout << "FvtxGlobalAlign::process_muid_clus - process coord @ z = "
        << midpoint.getZ() << endl;

  node->arm = coord_ptr->get()->get_arm();
  node->plane = coord_ptr->get()->get_plane();
  node->panel = coord_ptr->get()->get_panel();
  node->orientation = coord_ptr->get()->get_orientation();

  node->meas = coord_ptr->get()->get_w_absolute();
  node->sigma = coord_ptr->get()->get_error() * _muid_hist_sigma_scale_factor;

  node->p_det = convert(midpoint);
  node->p_strip_begin = convert(coord_ptr->get()->get_coord_begin());
  node->p_strip_end = convert(coord_ptr->get()->get_coord_end());

  // store projection matrix from state vector to measurement
  double angle(TMuiGeo::get_panel_angle(coord_ptr->get()->get_location()));

  node->dwdx = -sin(angle);
  node->dwdy = cos(angle);
  node->fill_slope_derivatives(node->p_det.z(), _track->z0_fit, _track->z_ref);

  // Kalman fit
  fill_node_kalman_fit(node);
}

//____________________________________________________________________
//! Fill vertex DCA from MuTr track
void
FvtxGlobalAlign::fill_mutr_disp_vtx()
{

  _integrator.initialize(_track->trk_par_kalman_mutr);
  TMutTrkPar extrap_trk_par;
  get_kalman_fit(_track->vtx_point.Z(), extrap_trk_par);

  const double phi_pt = atan2( extrap_trk_par.get_py(),extrap_trk_par.get_px() );
  const double phi_lateral = phi_pt - TMath::Pi()/2;
  const double dx = extrap_trk_par.get_x() - _track->vtx_point.x();
  const double dy = extrap_trk_par.get_y() - _track->vtx_point.y();

  _track->mutr_disp_pt_vtx = dx * cos(phi_pt) + dy * sin(phi_pt);
  _track->mutr_disp_lateral_vtx = dx * cos(phi_lateral) + dy * sin(phi_lateral);
}

//____________________________________________________________________
void
FvtxGlobalAlign::process_after_filling()
{

  //-----------------------------------------
  // add constraints and vertex fit
  //-----------------------------------------

  try
    {
      TMutTrkPar extrap_trk_par;
      const TMutTrkPar * trk_par_kalman = &(_track->trk_par_kalman);
      const double z_ref = _track->z_ref;
      const bool process_mutr = _track->get_n_nodes(TFvtxMPNode::MuTr) > 0;

      if (process_mutr)
        {

          if (Verbosity() >= FVTXOO::ALOT)
            {
              cout
                  << "FvtxGlobalAlign::process_after_filling() - Info - Process MuTr projections"
                  << endl;
            }

          _integrator.initialize(*trk_par_kalman);
          get_kalman_fit(z_ref, extrap_trk_par);
          _track->trk_par_kalman_zref = extrap_trk_par;

          _track->z_ref_point_kalman.SetXYZ(extrap_trk_par.get_x(),
              extrap_trk_par.get_y(), z_ref);

          _integrator.initialize(_track->trk_par_kalman_mutr);
          get_kalman_fit(z_ref, extrap_trk_par);
          _track->trk_par_kalman_mutr_zref = extrap_trk_par;

          fill_mutr_disp_vtx();

        } //      if (process_mutr)

      _integrator.initialize(*trk_par_kalman);
      const double vtx_z = _track->vtx_point.z();
      if (abs(vtx_z) < 60)
        {
          if (Verbosity() >= FVTXOO::ALOT)
            {
              cout
                  << "FvtxGlobalAlign::process_after_filling() - Info - Process vertex projections to z = "
                  << vtx_z << " cm" << endl;
            }

          get_kalman_fit(vtx_z, extrap_trk_par); // this will not be calculated through Fortran integrator again in process_trk_constraint
          _track->vtx_point_kalman.SetXYZ(extrap_trk_par.get_x(),
              extrap_trk_par.get_y(), vtx_z);
        }

      process_trk_constraint();
    }
  catch (std::exception &e)
    {

      cout
          << "FvtxGlobalAlign::process_trk - Error - failed when filling track constraint nodes : "
          << e.what() << endl;

      throw;
    }

  //-----------------------------------------
  // Track process after filling all the hits
  //-----------------------------------------

//  if (get_flag(USE_MUTR_HITS) and _track->get_n_nodes(TFvtxMPNode::MuTr) > 0)
//    process_fvtx_mutr_constraint();

  // fit track
  _track->interanl_fit();
  _track->interanl_fit_no_VTX_DCA_Constraint();

//  if (_track->get_n_nodes(TFvtxMPNode::FVTX) > 0 and get_flag(USE_MUTR_HITS)
//      and _track->get_n_nodes(TFvtxMPNode::MuTr) > 0)
//    _track->interanl_fit_MuTr_Alone();
}

//____________________________________________________________________
void
FvtxGlobalAlign::process_trk_constraint()
{
  assert(_track);

  if (_track->get_n_nodes(TFvtxMPNode::Constraint) > 0)
    {
      // clean up old constrains

      _track->remove_nodes(TFvtxMPNode::Constraint);

    }

  const bool process_fvtx = _track->get_n_nodes(TFvtxMPNode::FVTX) > 0;
  const double phi = _track->phi_acpt_cent; // roughly estimated azimuthal direction of the track
  const double angle = phi - M_PI / 2; // roughly estimated pt x z, i.e. strip direction
  const double vtx_x = _track->vtx_point.x();
  const double vtx_y = _track->vtx_point.y();
  const double vtx_z = _track->vtx_point.z();

//  TVector3 x0_kalman(trk_ptr->get()->get_trk_par_vtx()->get_x(),
//      trk_ptr->get()->get_trk_par_vtx()->get_y(),
//      trk_ptr->get()->get_trk_par_vtx()->get_z());
//  TVector3 tx_kalman(trk_ptr->get()->get_trk_par_vtx()->get_px(),
//      trk_ptr->get()->get_trk_par_vtx()->get_py(),
//      trk_ptr->get()->get_trk_par_vtx()->get_pz());
//  TVector3 vertex_proj_kalman = x0_kalman + tx_kalman * (vtx_z - x0_kalman.z());

  // vertex lateral constraints - sensitive direction
  if (get_flag(USE_SVTX_CONSTRAINT))
    {

      if (Verbosity() >= FVTXOO::ALOT)
        cout
            << "FvtxGlobalAlign::process_trk_constraint - process DCA_CONTRAINT_VTX @ z = "
            << _track->vtx_point.Z() << endl;

      TFvtxMPNode_Constraint * node =
          dynamic_cast<TFvtxMPNode_Constraint *>(_track->add_node(
              TFvtxMPNode::Constraint));
      assert(node);

      node->constraint_type = TFvtxMPNode_Constraint::DCA_CONTRAINT_VTX;

      node->dwdx = cos(phi); //dwdx;
      node->dwdtx = cos(phi) * (vtx_z - _track->z0_fit); //dwdtx;
      node->dwdy = sin(phi); //dwdy;
      node->dwdty = sin(phi) * (vtx_z - _track->z0_fit); //dwdty;

      node->meas = node->get_measurement(vtx_x, vtx_y) - _vtx_dca_shift;
      node->sigma = _vtx_dca_constraint;
      node->p_det = _track->vtx_point;

      // Kalman fit
      fill_node_kalman_fit(node);
    }

  // vertex lateral constraints - vertex
  if (get_flag(TRACK_LATCON_FIT)
//      && process_fvtx
//      && (_track->get_n_nodes(TFvtxMPNode::VTX) == 0 || _svx_hit_sigma >= .1)
      )
    {

      if (Verbosity() >= FVTXOO::ALOT)
        cout
            << "FvtxGlobalAlign::process_trk_constraint - process LAT_CONTRAINT_VTX @ z = "
            << _track->vtx_point.Z() << endl;

      TFvtxMPNode_Constraint * node =
          dynamic_cast<TFvtxMPNode_Constraint *>(_track->add_node(
              TFvtxMPNode::Constraint));
      assert(node);
      node->constraint_type = TFvtxMPNode_Constraint::LAT_CONTRAINT_VTX;

      double z = 0;

      if (get_flag(USE_SVTX_CONSTRAINT))
        {
//          // constraint at vertex
//          node->dwdx = cos(angle); //dwdx;
//          node->dwdtx = cos(angle) * (vtx_z - _track->z0_fit); //dwdtx;
//          node->dwdy = sin(angle); //dwdy;
//          node->dwdty = sin(angle) * (vtx_z - _track->z0_fit); //dwdty;
//
//          node->meas = node->get_measurement(vtx_x, vtx_y);
//          node->sigma = _vertex_lateral_constraint;
//          node->p_det = _track->vtx_point;

          z = vtx_z;

        }
      else
        {
//          // constraint at z0_fit
//
//          node->dwdx = cos(angle); //dwdx;
//          node->dwdtx = 0; //dwdtx;
//          node->dwdy = sin(angle); //dwdy;
//          node->dwdty = 0; //dwdty;
//
//          node->meas = node->get_measurement(vtx_x, vtx_y);
//          node->sigma = _vertex_lateral_constraint;
//          node->p_det = TVector3(vtx_x, vtx_y, _track->z0_fit);

          z = _track->fvtx_min_z();

        }

      // constraint at vertex
      node->dwdx = cos(angle); //dwdx;
      node->dwdtx = cos(angle) * (z - _track->z0_fit); //dwdtx;
      node->dwdy = sin(angle); //dwdy;
      node->dwdty = sin(angle) * (z - _track->z0_fit); //dwdty;

      node->meas = node->get_measurement(vtx_x, vtx_y);
      node->sigma = _vertex_lateral_constraint;
      node->p_det.SetXYZ(vtx_x, vtx_y, z);

      // Kalman fit
      fill_node_kalman_fit(node);

    }

  // track lateral constraints at station 3
  if (get_flag(TRACK_LATCON_FIT) && process_fvtx
  // always use lateral constraint regardless of MuTr hits due to the large multiple scattering in absorber + magnets
//      && (_track->get_n_nodes(TFvtxMPNode::MuTr) < 6 or _mutr_hit_sigma_min >= 1)
      )
    {

      const TFvtxMPNode_FVTX * node_max = _track->fvtx_node_max_z();

      if (node_max)
        {

          const double max_z = node_max->p_det.Z();

          if (Verbosity() >= FVTXOO::ALOT)
            cout
                << "FvtxGlobalAlign::process_trk_constraint - process LAT_CONTRAINT_STAION3 @ z = "
                << max_z << endl;
          if (abs(max_z) > 40)
            {
              cout
                  << "FvtxGlobalAlign::process_trk_constraint - Error - wrong max z = "
                  << max_z << endl;
              _track->Print();
            }

          TFvtxMPNode_Constraint * node =
              dynamic_cast<TFvtxMPNode_Constraint *>(_track->add_node(
                  TFvtxMPNode::Constraint));
          assert(node);

          node->constraint_type = TFvtxMPNode_Constraint::LAT_CONTRAINT_STAION3;

          node->dwdx = cos(angle); //dwdx;
          node->dwdtx = cos(angle) * (max_z - _track->z0_fit); //dwdtx;
          node->dwdy = sin(angle); //dwdy;
          node->dwdty = sin(angle) * (max_z - _track->z0_fit); //dwdty;

          node->meas = node->get_measurement(vtx_x, vtx_y);
          ; // constraint it to be at the center of phi acceptance window
          node->sigma = _vertex_lateral_constraint; // reuse this parameter
          node->p_det = TVector3(vtx_x, vtx_y, max_z);

          // Kalman fit
          fill_node_kalman_fit(node);

        }
      else
        {
          cout
              << "FvtxGlobalAlign::process_trk_constraint - Error - no FVTX cluster for this track"
              << endl;
        }

    }

  //multiple scattering constraint
  if (get_flag(USE_MUTR_HITS))
    assert(is_TForwardMPTrack());
  if (is_TForwardMPTrack())
    {
      const double average_pz = (_track->trk_par_kalman_mutr_zref.get_pz()
          + _track->trk_par_kalman_zref.get_pz()) / 2;

      TVector3 momentum_diff_kalman(
          _track->trk_par_kalman_mutr_zref.get_px()
              / _track->trk_par_kalman_mutr_zref.get_pz()
              - _track->trk_par_kalman_zref.get_px()
                  / _track->trk_par_kalman_zref.get_pz(),
          _track->trk_par_kalman_mutr_zref.get_py()
              / _track->trk_par_kalman_mutr_zref.get_pz()
              - _track->trk_par_kalman_zref.get_py()
                  / _track->trk_par_kalman_zref.get_pz(), 1);
      momentum_diff_kalman *= average_pz;

        {
          TFvtxMPNode_Constraint * node =
              dynamic_cast<TFvtxMPNode_Constraint *>(_track->add_node(
                  TFvtxMPNode::Constraint));
          assert(node);

          node->constraint_type =
              TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_DR_PT;

          node->dwdx = cos(phi); //dwdx;
          node->dwdtx = cos(phi) * (_track->z_ref - _track->z0_fit); //dwdtx;
          node->dwdy = sin(phi); //dwdy;
          node->dwdty = sin(phi) * (_track->z_ref - _track->z0_fit); //dwdty;

          node->meas = node->get_measurement(0, 0);
          node->sigma = _sigma_dr_pt; // reuse this parameter
          node->p_det = TVector3(0, 0, _track->z_ref);

          // Kalman fit
          node->fill_kalman_fit(
              _track->trk_par_kalman_mutr_zref.get_x()
                  - _track->trk_par_kalman_zref.get_x(),
              _track->trk_par_kalman_mutr_zref.get_y()
                  - _track->trk_par_kalman_zref.get_y());

          node->momentum_kalman = momentum_diff_kalman;
        }
        {
          TFvtxMPNode_Constraint * node =
              dynamic_cast<TFvtxMPNode_Constraint *>(_track->add_node(
                  TFvtxMPNode::Constraint));
          assert(node);

          node->constraint_type =
              TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_DR_LATERAL;

          node->dwdx = cos(angle); //dwdx;
          node->dwdtx = cos(angle) * (_track->z_ref - _track->z0_fit); //dwdtx;
          node->dwdy = sin(angle); //dwdy;
          node->dwdty = sin(angle) * (_track->z_ref - _track->z0_fit); //dwdty;

          node->meas = node->get_measurement(0, 0);
          node->sigma = _sigma_dr_lateral; // reuse this parameter
          node->p_det = TVector3(0, 0, _track->z_ref);

          // Kalman fit
          node->fill_kalman_fit(
              _track->trk_par_kalman_mutr_zref.get_x()
                  - _track->trk_par_kalman_zref.get_x(),
              _track->trk_par_kalman_mutr_zref.get_y()
                  - _track->trk_par_kalman_zref.get_y());

          node->momentum_kalman = momentum_diff_kalman;
        }
        {
          TFvtxMPNode_Constraint * node =
              dynamic_cast<TFvtxMPNode_Constraint *>(_track->add_node(
                  TFvtxMPNode::Constraint));
          assert(node);

          node->constraint_type =
              TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_DTHETA_PT;

          node->dwdx = 0; //dwdx;
          node->dwdtx = cos(phi); //dwdtx;
          node->dwdy = 0; //dwdy;
          node->dwdty = sin(phi); //dwdty;

          node->meas = node->get_measurement_angle(0, 0);
          node->sigma = _sigma_dtheta_pt; // reuse this parameter
          node->p_det = TVector3(0, 0, _track->z_ref);

          // Kalman fit
          node->fill_kalman_fit_angle(
              momentum_diff_kalman.X() / momentum_diff_kalman.Z(),
              momentum_diff_kalman.Y() / momentum_diff_kalman.Z());

          node->momentum_kalman = momentum_diff_kalman;
        }
        {
          TFvtxMPNode_Constraint * node =
              dynamic_cast<TFvtxMPNode_Constraint *>(_track->add_node(
                  TFvtxMPNode::Constraint));
          assert(node);

          node->constraint_type =
              TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_DTHETA_LATERAL;

          node->dwdx = 0; //dwdx;
          node->dwdtx = cos(angle); //dwdtx;
          node->dwdy = 0; //dwdy;
          node->dwdty = sin(angle); //dwdty;

          node->meas = node->get_measurement_angle(0, 0);
          node->sigma = _sigma_dtheta_lateral; // reuse this parameter
          node->p_det = TVector3(0, 0, _track->z_ref);

          // Kalman fit
          node->fill_kalman_fit_angle(
              momentum_diff_kalman.X() / momentum_diff_kalman.Z(),
              momentum_diff_kalman.Y() / momentum_diff_kalman.Z());

          node->momentum_kalman = momentum_diff_kalman;
        }

    } //   if (is_TForwardMPTrack())

}

//____________________________________________________________________
//! add FVTX - MuTr alignment constraints to track fittings,
void
FvtxGlobalAlign::process_fvtx_mutr_constraint()
{
  cout
      << "FvtxGlobalAlign::process_trk_constraint - WARNING - this function is obsolete" << endl;

  const double phi = _track->phi_acpt_cent; // roughly estimated azimuthal direction of the track
  const double angle = phi - M_PI / 2; // roughly estimated pt x z, i.e. strip direction
  const double vtx_x = _track->vtx_point.x();
  const double vtx_y = _track->vtx_point.y();
//  const double vtx_z = _track->vtx_point.z();

  const TFvtxMPNode_FVTX * node_max = _track->fvtx_node_max_z();

  if (node_max)
    {

      const double max_z = node_max->p_det.Z();

      if (Verbosity() >= FVTXOO::ALOT)
        cout
            << "FvtxGlobalAlign::process_trk_constraint - process LAT_CONTRAINT_STAION3 @ z = "
            << max_z << endl;
      if (abs(max_z) > 40)
        {
          cout
              << "FvtxGlobalAlign::process_trk_constraint - Error - wrong max z = "
              << max_z << endl;
          _track->Print();
        }

      TFvtxMPNode_Constraint * node =
          dynamic_cast<TFvtxMPNode_Constraint *>(_track->add_node(
              TFvtxMPNode::Constraint));
      assert(node);

      node->constraint_type = TFvtxMPNode_Constraint::LAT_CONTRAINT_STAION3;

      node->dwdx = cos(angle); //dwdx;
      node->dwdtx = cos(angle) * (max_z - _track->z0_fit); //dwdtx;
      node->dwdy = sin(angle); //dwdy;
      node->dwdty = sin(angle) * (max_z - _track->z0_fit); //dwdty;

      node->meas = node->get_measurement(vtx_x, vtx_y);
      ; // constraint it to be at the center of phi acceptance window
      node->sigma = _vertex_lateral_constraint; // reuse this parameter
      node->p_det = node_max->p_det;

      // Kalman fit
      fill_node_kalman_fit(node);

    }
  else
    {
      cout
          << "FvtxGlobalAlign::process_trk_constraint - Error - no FVTX cluster for this track"
          << endl;
    }

}

//____________________________________________________________________
void
FvtxGlobalAlign::fill_node_kalman_fit(TFvtxMPNode * node)
{
  assert(node);

  TMutTrkPar extrap_trk_par;
  get_kalman_fit(node->p_det.z(), extrap_trk_par);
  node->fill_kalman_fit(extrap_trk_par.get_x(), extrap_trk_par.get_y());
  node->momentum_kalman.SetXYZ(extrap_trk_par.get_px(), extrap_trk_par.get_py(),
      extrap_trk_par.get_pz());

}

//____________________________________________________________________
//! prepare track extrapolator
const TMutTrkPar *
FvtxGlobalAlign::get_kalman_fit(TFvtxTrkMap::pointer trk_ptr)
{
  const TMutTrkPar * trk_par = NULL;

  const TMutTrkPar* fvtxmutr_trk_par = trk_ptr->get()->get_trk_par_mutr();
  double p_tot = 0;
  if (fvtxmutr_trk_par)
    p_tot = fvtxmutr_trk_par->get_ptot();
  if (p_tot > 0)
    {
      // use joint MuTr - FVTX fit

      trk_par = trk_ptr->get()->get_trk_par_mutr();
    }
  else
    {
      // use joint FVTX standalone fit

//      const std::vector<TMutTrkPar> &trk_par_vect(
//          *trk_ptr->get()->get_trk_par_list());

//      trk_par = &(*min_element(trk_par_vect.begin(), trk_par_vect.end(),
//          closest_z_ftor(z_det)));
//      trk_par = &(*trk_par_vect.begin());
      trk_par = trk_ptr->get()->get_trk_par_vtx();

    }

  assert(trk_par);

  if (Verbosity() >= FVTXOO::ALOT)
    {
      cout
          << "FvtxGlobalAlign::get_kalman_fit - Info - Track extrapolation from"
          << endl;
      trk_par->print();
    }

  if (trk_par->get_ptot() == 0)
    {

      stringstream out;
      out
          << "FvtxGlobalAlign::get_kalman_fit - Error - inital track parameter with p_tot = "
          << trk_par->get_ptot();

      throw runtime_error(out.str());

    }

  return trk_par;
}

//____________________________________________________________________
//! Extrapolate the system to specified z.
void
FvtxGlobalAlign::TrackIntegratorKF::extrapolate(double z_det,
    bool start_from_init, bool verbose)
{

  if (start_from_init)
    {
      PHTrackIntegratorKF::initialize(_trk_par_init);
    }

  PHTrackIntegratorKF::extrapolate(z_det);
  if (PHTrackIntegratorKF::get_error())
    {

      if (start_from_init)
        {
          stringstream out;
          out
              << "FvtxGlobalAlign::get_kalman_fit - Error - extrapolation failed ("
              << PHTrackIntegratorKF::get_z() << "->" << z_det << ") @ p = "
              << _trk_par_init.get_ptot();

          throw runtime_error(out.str());
        }
      else
        {
          if (verbose)
            {

              cout
                  << "FvtxGlobalAlign::get_kalman_fit - WARNING - extrapolation failed ("
                  << PHTrackIntegratorKF::get_z() << "->" << z_det << "). "
                  << endl;

              cout
                  << "FvtxGlobalAlign::get_kalman_fit - Print init track parameter : "
                  << endl;
              _trk_par_init.print();
              cout
                  << "FvtxGlobalAlign::get_kalman_fit - Print last track parameter : "
                  << endl;
              _trk_par_extrap.print();

              cout
                  << "FvtxGlobalAlign::get_kalman_fit - WARNING - Try again from the starting point"
                  << endl;
            }

          extrapolate(z_det, true);
        }

    }
  else
    {

      PHTrackIntegratorKF::finish(_trk_par_extrap);

    }

  if (_trk_par_extrap.get_z() != z_det)
    {

      if (start_from_init)
        {
          stringstream out;
          out
              << "FvtxGlobalAlign::get_kalman_fit - Error - extrapolation failed ("
              << PHTrackIntegratorKF::get_z() << "->" << z_det << ") at "
              << _trk_par_extrap.get_z() << ", p_init = "
              << _trk_par_init.get_ptot();

          throw runtime_error(out.str());
        }
      else
        {
          if (verbose)
            {
              cout
                  << "FvtxGlobalAlign::get_kalman_fit - WARNING - extrapolation failed ("
                  << PHTrackIntegratorKF::get_z() << "->" << z_det << "). "
                  << ") at " << _trk_par_extrap.get_z()
                  << ". Try again from the starting point" << endl;
            }
          extrapolate(z_det, true);
        }

    }
}

//____________________________________________________________________
//! get kalman fit at specific z
void
FvtxGlobalAlign::get_kalman_fit(const double z_det, TMutTrkPar & extrap_trk_par)
{
  _integrator.extrapolate(z_det);

  _integrator.finish(extrap_trk_par);

  if (Verbosity() >= FVTXOO::ALOT)
    {
      cout << "FvtxGlobalAlign::get_kalman_fit - Info - Track extrapolation to"
          << endl;
      extrap_trk_par.print();
    }
}

//____________________________________________________________________
bool
FvtxGlobalAlign::export_misalignment(const char* filename_align)
{
  MUTOO::TRACE(
      string("FvtxGlobalAlign::export_misalignment - ") + filename_align);

  // make_backup( filename_align );
  ofstream out(filename_align, ios::out);
  if (!out)
    {
      cout << "Align::DumpToFile - ERROR: cannot write to file \""
          << filename_align << "\".\n";
      return false;
    }

  try
    {
      if (get_flag(ALIGN_FVTX))
        {
          export_fvtx_misalignment_to_tree(out);
        }
      if (get_flag(ALIGN_MUTR))
        {
          export_mutr_parameters_to_tree(out);
        }
      if (get_flag(ALIGN_MUID))
        {
          export_muid_parameters_to_tree(out);
        }
    }
  catch (exception &e)
    {

      cout << "FvtxGlobalAlign::export_misalignment - Error - " << e.what()
          << endl;

      throw;
    }

  export_misalignment_to_text();

//  print_fixed_parameters(out);

  MUTOO::PRINT(out, "CONFIGURATION - FVTX");
  out << "_align_fvtx_station :"
      << ((get_flag(ALIGN_FVTX_STATION)) ? "true" : "false") << endl;
  out << "_align_fvtx_wedge   :"
      << ((get_flag(ALIGN_FVTX_WEDGE)) ? "true" : "false") << endl;
  out << "_align_w      :" << ((get_flag(ALIGN_W)) ? "true" : "false") << endl;
  out << "_align_z      :" << ((get_flag(ALIGN_Z)) ? "true" : "false") << endl;
  out << "_align_phi    :" << ((get_flag(ALIGN_PHI)) ? "true" : "false")
      << endl;
  out << "_align_psix   :" << ((get_flag(ALIGN_PSIX)) ? "true" : "false")
      << endl;
  out << "_align_psiy   :" << ((get_flag(ALIGN_PSIY)) ? "true" : "false")
      << endl;
  out << "_iterate      :" << ((get_flag(ITERATE)) ? "true" : "false") << endl;
  out << "_scratch_filename :" << _scratch_filename << endl;
  out << "_n_std_dev    :" << _n_std_dev << endl;
  MUTOO::PRINT(out, "**");

  MUTOO::PRINT(out, "CONFIGURATION - Mu Arm");
  out << "_align_muid     :" << ((get_flag(ALIGN_MUID)) ? "true" : "false")
      << endl;
  out << "_align_mutr     :" << ((get_flag(ALIGN_MUTR)) ? "true" : "false")
      << endl;
  out << "_align_w      :" << ((get_flag(ALIGN_W_MU_ARM)) ? "true" : "false")
      << endl;
  out << "_align_z      :" << ((get_flag(ALIGN_Z_MU_ARM)) ? "true" : "false")
      << endl;
  out << "_align_phi    :" << ((get_flag(ALIGN_PHI_MU_ARM)) ? "true" : "false")
      << endl;
  out << "_constraint   :"
      << ((get_flag(USE_CONSTRAINTS_MU_ARM)) ? "true" : "false") << endl;
  out << "_scratch_filename :" << _scratch_filename << endl;
  out << "_n_std_dev    :" << _n_std_dev << endl;
  MUTOO::PRINT(out, "**");

  out.close();

  return true;

}

//____________________________________________________________________
bool
FvtxGlobalAlign::accept_evt()
{

  if (abs(_bbcz) > _vertex_acceptance)
    {
      if (Verbosity() >= 1)
        cout
            << "FvtxGlobalAlign::accept_evt - Rejected - Improper BBC vertex = "
            << _bbcz << " cm" << endl;
      return false;
    }

  if (get_flag(USE_SVTX_CONSTRAINT))
    {
      // use VTX for vertex

      if (isnan(_vtxxp) or isnan(_vtxyp) or isnan(_vtxzp))
        {

          if (Verbosity() >= 1)
            cout << "FvtxGlobalAlign::accept_evt - Rejected - "
                << "Invalid VTX vertex (nan)" << endl;

          return false;
        }

      if (abs(_vtxzp) > _vertex_acceptance)
        {
          if (Verbosity() >= 1)
            cout
                << "FvtxGlobalAlign::accept_evt - Rejected - Improper VTX vertex z = "
                << _vtxzp << " cm" << endl;
          return false;
        }

      if (abs(_vtxzp - _bbcz) > 6)
        {
          if (Verbosity() >= 1)
            cout << "FvtxGlobalAlign::accept_evt - Rejected - "
                << "Large diff between VTX and BBC vertex z = "
                << _vtxzp - _bbcz << " cm" << endl;
          return false;
        }

    } //   if (get_flag(USE_SVTX_CONSTRAINT))

  return true;
}

//____________________________________________________________________
int
FvtxGlobalAlign::evaluate_event(void)
{
//  const bool good_mutr = get_flag(USE_MUTR_HITS) && _n_good_muon_trk > 0;

  if (_n_good_trk > 0)
    {
      _n_events_acpt++;

      return EVENT_OK;
    }
  else
    {
      return DISCARDEVENT;
    }
}

//______________________________________________________
bool
FvtxGlobalAlign::accept_trk()
{

  assert(_track);

  _track->reject_track = 9999;

  if (_track->chi_square_fvtx / _track->ndf_fvtx > 40)
    {

      if (Verbosity() >= 1)
        cout << "FvtxGlobalAlign::accept_trk - Rejected - chi_square cut = "
            << _track->chi_square_fvtx / _track->ndf_fvtx << endl;

      _track->reject_track = 101; // ID for this reason

      return false;

    }

//  //fit-residu consistency
//  double sum_residu = _track->fvtx_sum_residual();
//  if (fabs(sum_residu) > 0.1e-3)
//    {
//
//      if (Verbosity() >= 1)
//        cout
//            << "FvtxGlobalAlign::accept_trk - Rejected - too large sum residu = "
//            << sum_residu * 1e4 << " um " << endl;
//
//      _track->reject_track = 102; // ID for this reason
//
//      return false;
//
//    }

//  // make sure hit are consistent with #-of-hits constrains
  const bool allow_missing_st0_or_3 = //
      (get_flag(USE_VTX_HITS) and _track->get_n_nodes(TFvtxMPNode::VTX) > 0) //
          or (get_flag(USE_MUTR_HITS)
              and _track->get_n_nodes(TFvtxMPNode::MuTr) > 0) //
              or get_flag(USE_SVTX_CONSTRAINT)
          ;
  if (!_track->fvtx_one_hit_per_station(get_flag(DOUBLE_HIT_PER_STA),
      allow_missing_st0_or_3, Verbosity() > FVTXOO::NONE))
    {
      if (Verbosity() >= 1)
        {
          cout
              << "FvtxGlobalAlign::accept_trk - Rejected - Improper hits pattern"
              << endl;
        }

      _track->reject_track = 103; // ID for this reason

      return false;
    }

  // cluster hit size
  if (!_track->fvtx_cluster_cut(3))
    {
      if (Verbosity() >= 1)
        cout
            << "FvtxGlobalAlign::accept_trk - Rejected - Improper cluster hit size"
            << endl;

      _track->reject_track = 104; // ID for this reason

      return false;
    }

  // vertex
  if (get_flag(USE_SVTX_CONSTRAINT))
    {
      static bool once = true;
      if (once)
        {

          cout
              << "FvtxGlobalAlign::accept_trk - Info - will NOT use FVTX track vertex cut when USE_SVTX_CONSTRAINT = true"
              << endl;

          once = false;
        }
    }
  if (abs(_track->z0reco_fvtx) > _vertex_acceptance
      && !get_flag(USE_SVTX_CONSTRAINT))
    {
      if (Verbosity() >= 1)
        cout
            << "FvtxGlobalAlign::accept_trk - Rejected - Improper FVTX vertex = "
            << _track->z0reco_fvtx << " cm" << endl;

      _track->reject_track = 201; // ID for this reason

      return false;
    }

  if (get_flag(USE_SVTX_CONSTRAINT))
    {
      // use VTX for vertex

      if (abs(_track->vtxzp - _track->z0reco_fvtx) > 4)
        {
          if (Verbosity() >= 1)
            cout << "FvtxGlobalAlign::accept_trk - Rejected - "
                << "Large diff between VTX and FVTX vertex z = "
                << _track->vtxzp - _track->z0reco_fvtx << " cm" << endl;

          _track->reject_track = 202; // ID for this reason

          return false;
        }

    }
  else if (_track->get_n_nodes(TFvtxMPNode::MuTr) == 0)
    {
      // use BBC for vertex only

      if (abs(_track->bbcz - _track->z0reco_fvtx) > 5.41156e-01 + 2.03284e+00)
        {
          if (Verbosity() >= 1)
            cout << "FvtxGlobalAlign::accept_trk - Rejected - "
                << "Large diff between BBC and FVTX vertex = "
                << _track->bbcz - _track->z0reco_fvtx << " cm" << endl;

          _track->reject_track = 203; // ID for this reason

          return false;
        }

    } // //   if (get_flag(USE_SVTX_CONSTRAINT))

  // Inspect each wedge and see whether they are enabled
  bool wedge_enabled = true;
  for (unsigned int i = 0; i < _track->get_n_nodes(TFvtxMPNode::FVTX); i++)
    {
      const TFvtxMPNode_FVTX * node =
          dynamic_cast<const TFvtxMPNode_FVTX *>(_track->get_node(
              TFvtxMPNode::FVTX, i));
      assert(node);

      wedge_enabled = get_wedge_status(node->arm, node->cage, node->station,
          node->sector, node->column) == DET_ENABLE;

      if (!wedge_enabled)
        break;
    }
  if (!wedge_enabled)
    {
      if (Verbosity() >= 1)
        cout
            << "FvtxGlobalAlign::accept_trk - Rejected - At least one wedge disabled for the analysis"
            << endl;

      _track->reject_track = 301; // ID for this reason

      return false;
    }

  if (_track->phi_acpt_width < -.015 && get_flag(USE_PHI_ACPT_CUT))
    {

      if (Verbosity() >= 1)
        cout << "FvtxGlobalAlign::accept_trk - Rejected - "
            << "Invalid phi acceptance for track originated from beam" << endl;

      _track->reject_track = 302; // ID for this reason

      return false;

    }

  // VTX hit
  if (!_track->vtx_one_hit_per_layer() && get_flag(USE_VTX_HITS))
    {
      if (Verbosity() >= 1)
        {
          cout
              << "FvtxGlobalAlign::accept_trk - Rejected - Improper hits pattern on VTX"
              << endl;
        }

      _track->reject_track = 1101; // ID for this reason

      return false;
    }

  // Event
  if (_track->reject_event)
    {

      if (Verbosity() >= 1)
        cout << "FvtxGlobalAlign::accept_trk - Rejected - "
            << "The event for this track is marked as rejected" << endl;

      _track->reject_track = 401; // ID for this reason

      return false;

    }

  if (!_track->IsValid())
    {
      if (Verbosity() >= 1)
        {
          cout << "FvtxGlobalAlign::accept_trk_mutr - Rejected - invalid data"
              << endl;
        }

      _track->reject_track = 3201; // ID for this reason

      return false;
    }

  _track->reject_track = 0;
  if (Verbosity() >= 1)
    cout << "FvtxGlobalAlign::accept_trk - Accepted track" << endl;
  return true;

}

//______________________________________________________
bool
FvtxGlobalAlign::accept_trk_mutr()
{

  assert(_track);

  _track->reject_track = 9999;

  if (_track->muid_lastgap < 4)
    {

      if (Verbosity() >= 1)
        cout
            << "FvtxGlobalAlign::accept_trk_mutr - Rejected - muid_lastgap cut = "
            << _track->muid_lastgap << endl;

      _track->reject_track = 1101; // ID for this reason

      return false;

    }

  // num of hit in MuTr
  if (_track->get_n_nodes(TFvtxMPNode::MuTr) <= 13)
    {

      if (Verbosity() >= 1)
        cout
            << "FvtxGlobalAlign::accept_trk_mutr - Rejected - number MuTr hit too low"
            << _track->get_n_nodes(TFvtxMPNode::MuTr) << endl;

      _track->reject_track = 1102; // ID for this reason

      return false;

    }

  // num of hit in MuID
  if (_track->get_n_nodes(TFvtxMPNode::MuID) <= 7 && get_flag(USE_MUID_HITS))
    {

      if (Verbosity() >= 1)
        cout
            << "FvtxGlobalAlign::accept_trk_mutr - Rejected - number MuID hit too low"
            << _track->get_n_nodes(TFvtxMPNode::MuID) << endl;

      _track->reject_track = 1103; // ID for this reason

      return false;

    }

  const double pz = _track->trk_par_kalman_mutr.get_pz();
  if (abs(pz) > _pz_max || abs(pz) < _pz_min)
    {

      if (Verbosity() >= 1)
        {
          cout << "FvtxGlobalAlign::accept_trk_mutr - Rejected - pz = " << pz
              << " from trk_par_kalman_mutr:" << endl;
          _track->trk_par_kalman_mutr.print();
        }

      _track->reject_track = 1201; // ID for this reason

      return false;

    }

  if (_track->get_n_nodes(TFvtxMPNode::MuTr) > 0
      && _track->get_n_nodes(TFvtxMPNode::FVTX) > 0)
    {
      // track matching

      const double p_zref = _track->trk_par_kalman_zref.get_momentum().length();
      const double p_mutr_zref =
          _track->trk_par_kalman_mutr_zref.get_momentum().length();

      if (abs(p_zref - p_mutr_zref) / p_zref > 0.1)
        {

          if (Verbosity() >= 1)
            {
              cout
                  << "FvtxGlobalAlign::accept_trk_mutr - Rejected - diff p @ zref = "
                  << p_zref << " - " << p_mutr_zref << " = "
                  << abs(p_zref - p_mutr_zref) / p_zref << " * p_zref" << endl;

            }

          _track->reject_track = 2201; // ID for this reason

          return false;

        }
    }

  if (!_track->IsValid())
    {
      if (Verbosity() >= 1)
        {
          cout << "FvtxGlobalAlign::accept_trk_mutr - Rejected - invalid data"
              << endl;
        }

      _track->reject_track = 3201; // ID for this reason

      return false;
    }

  _track->reject_track = 0;
  if (Verbosity() >= 1)
    cout << "FvtxGlobalAlign::accept_trk - Accepted track" << endl;
  return true;

}

//______________________________________________________
bool
FvtxGlobalAlign::is_TForwardMPTrack()
{
  if (!_track)
    return false;
  string name(_track->ClassName());

  return name == "TForwardMPTrack";
}

//______________________________________________________
void
FvtxGlobalAlign::BeamPosXY::load_data(string data_file, int verbose)
{
  cout << "FvtxGlobalAlign::BeamPosXY::load_data - Info - loading data file"
      << data_file << endl;

  fstream fdata(data_file.c_str());
  if (!fdata.is_open())
    {
      cout
          << "FvtxGlobalAlign::BeamPosXY::load_data - Error - cannot open data file"
          << data_file << endl;
      return;
    }

  while (!fdata.eof())
    {
      int run;
      double x, y, ex, ey;

      fdata >> run >> x >> y >> ex >> ey;
      if (ex <= 0)
        ex = 1e-6;
      if (ey <= 0)
        ey = 1e-6;

      const double wx = 1 / ex / ex;
      const double wy = 1 / ey / ey;

      if (position.count(run))
        {
          const double oldx = position[run].first;
          const double oldy = position[run].second;
          const double oldwx = weight[run].first;
          const double oldwy = weight[run].second;

          position[run] = pos((x * wx + oldx * oldwx) / (wx + oldwx),
              (y * wy + oldy * oldwy) / (wy + oldwy));
          weight[run] = pos(wx + oldwx, wy + oldwy);
        }
      else
        {
          position[run] = pos(x, y);
          weight[run] = pos(wx, wy);
        }

    }
  fdata.close();

  for (record::const_iterator iter = position.begin(); iter != position.end();
      ++iter)
    {
      const int run = iter->first;
      pos xy = iter->second;
      pos xy_weight = weight[run];

      if (verbose)
        cout
            << "FvtxGlobalAlign::BeamPosXY::load_data - Info - beam positon for run "
            << run << " = [" << xy.first << " , " << xy.second << "] +/- "
            << " [" << 1 / sqrt(xy_weight.first) << " , "
            << 1 / sqrt(xy_weight.second) << "] cm" << endl;
    }
}

//________________________________________
bool
FvtxGlobalAlign::BeamPosXY::have_run(int run)
{
  return position.count(run) > 0;
}

//________________________________________
double
FvtxGlobalAlign::BeamPosXY::get_x(int run)
{
  return position[run].first;
}

//________________________________________
double
FvtxGlobalAlign::BeamPosXY::get_y(int run)
{
  return position[run].second;
}

