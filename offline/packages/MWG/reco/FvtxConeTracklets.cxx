// $Id: FvtxConeTracklets.cxx,v 1.8 2013/02/25 04:34:38 jinhuang Exp $
/*!
 \file		FvtxConeTracklets.cxx
 \ingroup supermodules
 \brief	 Make coordinate pairs projecting to the vertex and count them
 around each muon track
 \author	Cesar Luiz da Silva, Jin Huang
 \version $Revision: 1.8 $
 \date		$Date: 2013/02/25 04:34:38 $
 */

#include <cmath>
#include <iostream>
#include <boost/algorithm/string.hpp>

#include <TVector3.h>
#include <TMath.h>
#include <TNtuple.h>

#include <Fun4AllReturnCodes.h>
#include <PHIODataNode.h>
#include <PHTFileServer.h>
#include <getClass.h>

#include <VtxOut.h>
#include <BbcOut.h>
#include <FVTXOO.h>
#include <TFvtxCompactTrkMap.h>
#include <FvtxGeom.h>

#include <Tools.h>
#include <PHMuoTracksOut.h>
#include <MWG.h>
#include <MWGVersion.h>

#include "FvtxConeTracklets.h"

using namespace std;

//______________________________________________________
namespace small_tool
{
  //! convert PHPoint to TVector3
  const TVector3
  convert(const PHPoint & p)
  {
    return TVector3(p.getX(), p.getY(), p.getZ());
  }

  //! true for 1, 2, 10, 20, 100, 200, etc
  bool
  is_special_num(const int event_num)
  {
    const double significand = event_num / pow(10, (int) (log10(event_num)));

    if (fmod(significand, 1) == 0 && significand <= 2)
      return true;
    else
      return false;
  }

}
using namespace small_tool;

//______________________________________________________
FvtxConeTracklets::FvtxConeTracklets(string evalfile) : //
    SubsysReco("FVTXCONETRACKLETS"), //
    _save_tracklet(false), //
    _max_cone(1.), _min_cone(.01), //
    phi_acpt_width_cut(-0.02), w_acpt_cut(0.05), //
    vertex_cut_min(2), _vertex_cut(2), //
    _vertex_name("AUTO"), _n_bad_vertex(0), //
    _muon_event_only(true), _is_new_nDST(true), _muo_local(NULL), //
    _eventCount(0), //
    make_eval(false), eval_name(evalfile), ntup(NULL)
{
}

//______________________________________________________
FvtxConeTracklets::~FvtxConeTracklets()
{

  if (_muo_local)
    {
      delete _muo_local;
      _muo_local = NULL;
    }

}

//______________________________________________________
int
FvtxConeTracklets::Init(PHCompositeNode *top_node)
{
  FVTXOO::PRINT(cout, "FvtxConeTracklets::Init");

  if (_save_tracklet)
    {
      cout
          << "FvtxConeTracklets::Init - WARNING - saving tracklets to TFvtxCompactTrkMap, "
          << "and replace its original content if there is any. "
          << "This option is for debug only. " << endl;
    }

  if (make_eval)
    {
      enum
      {
        AUTO_SAVE = 16000
      };
      PHTFileServer::get().open(eval_name.c_str(), "RECREATE");
      ntup = new TNtuple("ntup", "evaluation ntuple",
          "ievt:vtxZ:Z:dw:dphi:accept");
      ntup->SetAutoSave(AUTO_SAVE);
    }

  cout << ">>> Tracklet finding parameters" << endl;

  if (_muon_event_only)
    cout << "Only process event which have at least one MuTr track out" << endl;
  else
    cout << "Process all events regardless MuTr track out" << endl;

  cout << "phi_acpt_width_cut = \t" << phi_acpt_width_cut << " rad" << endl;
  cout << "w_acpt_cut = \t" << w_acpt_cut << " cm" << endl;
  cout << "vertex_cut_min = \t" << vertex_cut_min << " cm" << endl;
  cout << "_vertex_name = \t" << get_vertex_name() << endl;

  cout
      << "WARNING: forcing veterx x,y=0 at this moment until more stablized vertex finder is used."
      << endl;

  cout << ">>> Tracklet storage parameters" << endl;

  if (_save_tracklet)
    cout << "Save tracklet to TFvtxCompactTrkMap" << endl;
  else
    cout << "Leave TFvtxCompactTrkMap as it is" << endl;

  cout << "Cone range = \t" << _min_cone << " to " << _max_cone << " rad"
      << endl;

  if (make_eval)
    cout << "Save evaluation to \t" << eval_name << endl;
  else
    cout << "Do not save evaluation file" << endl;

  FVTXOO::PRINT(cout, "**");

  // allocate local nDST object
  if (!_muo_local)
    {
      _muo_local = MWG::newPHMuoTracksFvtx();
      assert(_muo_local);
      assert(MWGVersion::get(_muo_local->ClassName())>=15);
    }

  return EVENT_OK;
}

//______________________________________________________
int
FvtxConeTracklets::InitRun(PHCompositeNode *top_node)
{
  _eventCount = 0;

  _fvtx_phi_cent_list.clear();
  _fvtx_phi_cent_list.resize(FVTXOO::MAX_ARM);

  for (int arm = 0; arm < FVTXOO::MAX_ARM; arm++)
    {
      FvtxArm * fvtx_arm = FvtxGeom::get_arm(arm);
      assert(fvtx_arm);

      for (int cage = 0; cage < FVTXOO::MAX_CAGE; cage++)
        {
          FvtxStation * fvtx_sta = fvtx_arm->get_cage(cage)->get_station(0);
          assert(fvtx_sta);

          for (int sec = 0; sec < FVTXOO::MAX_SECTOR; sec++)
            {
              FvtxSector * fvtx_sec = fvtx_sta->get_sector(sec);

              const int wedge = cage * FVTXOO::MAX_SECTOR + sec;
              const double phi_center = fvtx_sec->get_phi();

              _fvtx_phi_cent_list[arm].push_back(make_pair(wedge, phi_center));
            }
        }
    }

  return 0;
}

//______________________________________________________
int
FvtxConeTracklets::process_event(PHCompositeNode *top_node)
{
  _eventCount++;

  // VTX
  /*VtxOut**/vtx = findNode::getClass<VtxOut>(top_node, "VtxOut");
  if (!vtx)
    {
      cout
          << "FvtxConeTracklets::process_event - Error - VtxOut not in Node Tree"
          << endl;
      return ABORTRUN;
    }

  // new framework MWG tracks
  /*PHMuoTracksOut**/_muo = findNode::getClass<PHMuoTracksOut>(top_node,
      "PHMuoTracksOO");
  if (!_muo)
    {
      cout
          << "FvtxConeTracklets::process_event - Error - PHMuoTracksOO (new framework) not in Node Tree"
          << endl;
      return ABORTRUN;
    }
  else
    {
      // MWG version check

      static bool once = true;
      if (once)
        {
          once = false;

          const unsigned int muo_version = MWGVersion::get(_muo->ClassName());
          if (muo_version < 15)
            {
              cout
                  << "FvtxConeTracklets::process_event - WARNING - older version (v"
                  << muo_version
                  << ") of PHMuoTracksOut is used in nanoDST (e.g. run12 1st production)."
                  << " I will use back compatible mode." << endl;

              _is_new_nDST = false;
            }
          else
            {
              cout << "FvtxConeTracklets::process_event - INFO - new version (v"
                  << muo_version
                  << ") of PHMuoTracksOut is used in nanoDST. Appending cone observables to it."
                  << endl;

              _is_new_nDST = true;
            }
        }
    }
  if (_eventCount < 2)
    _muo->ShutUp();

  /*TFvtxCoordMap**/_coord_map = findNode::getClass<TFvtxCoordMap>(top_node,
      "TFvtxCoordMap");
  if (!_coord_map)
    {
      cout
          << "FvtxConeTracklets::process_event - Error - TFvtxCoordMap not in Node Tree"
          << endl;
      return ABORTRUN;
    }

  _trk_map = (TFvtxTrkMap*) NULL;
  try
    {
      _trk_map = TMutNode<TFvtxTrkMap>::find_node(top_node, "TFvtxTrkMap");
    }
  catch (runtime_error & e)
    {
      static bool once = true;

      if (once)
        cout
            << "FvtxConeTracklets::process_event - WARNING - TFvtxTrkMap not in Node Tree. Will not use FVTX track info. Error message:"
            << e.what() << endl;
      once = false;
    }

  _svx_map = (TFvtxSvxClusterMap*) NULL;
  try
    {
      /*TFvtxSvxClusterMap**/_svx_map = TMutNode<TFvtxSvxClusterMap>::find_node(
          top_node, "TFvtxSvxClusterMap");
    }
  catch (runtime_error & e)
    {
      static bool once = true;

      if (once)
        {
          cout
              << "FvtxConeTracklets::process_event - WARNING - TFvtxSvxClusterMap not in Node Tree. Will not use vtx info. Error message:"
              << e.what() << endl;
          once = false;
        }
    }

  _ctrk_map = (TFvtxCompactTrkMap*) NULL;
  if (_save_tracklet)
    {
      /*TFvtxCompactTrkMap**/_ctrk_map = findNode::getClass<TFvtxCompactTrkMap>(
          top_node, "TFvtxCompactTrkMap");
      if (!_ctrk_map)
        {
          // make the node
          cout
              << "FvtxConeTracklets::process_event - INFO - Adding TFvtxCompactTrkMap"
              << endl;

          // FVTX working space _fvtx_node
          PHNodeIterator nodeItr(top_node);
          PHCompositeNode* _fvtx_node =
              static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode",
                  "FVTXOO"));
          if (!_fvtx_node)
            {
              _fvtx_node = new PHCompositeNode("FVTXOO");
              top_node->addNode(_fvtx_node);

            }

          _ctrk_map = TMutNode<TFvtxCompactTrkMap>::new_node(_fvtx_node,
              "TFvtxCompactTrkMap");
          assert(_ctrk_map);

          // PHCompositeNode *dst_node;
          PHCompositeNode* dst_node = 0;
            {
              PHNodeIterator nodeItr(top_node);
              dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst(
                  "PHCompositeNode", "DST"));
              if (!dst_node)
                {
                  dst_node = new PHCompositeNode("DST");
                  top_node->addNode(dst_node);
                }
            }

          _ctrk_map->make_persistant(dst_node, "TFvtxCompactTrk");
        } //if (!_ctrk_map)
    } //if (_save_tracklet)

  assert(_muo);
  assert(vtx);
  assert(_coord_map);

  // check if it is necessary to process this event.
  const unsigned int n_mu = _muo->get_npart();
  if (n_mu == 0 && _muon_event_only)
    {
      if (Verbosity() >= 1)
        {
          cout
              << "FvtxConeTracklets::process_event - INFO - no muon track found, "
              << "do not process cone isolation cut for event " << _eventCount
              << endl;
        }
      return DISCARDEVENT;
    }

  // cleanup
  _vtx_layer_list.clear();
  _tracklets.clear();
  if (_save_tracklet)
    _ctrk_map->clear();

  // init
  const int ret_vertex = process_vertex();
  if (ret_vertex != EVENT_OK)
    return ret_vertex;

  if (!_is_new_nDST)
    {
      // sync _muo_local's size to nanoDST
      _muo_local->Reset();

      for (unsigned int i = 0; i < _muo->get_npart(); i++)
        {
          _muo_local->AddPHParticle(i);
        }
      _muo_local->set_npart(_muo->get_npart());

      if (Verbosity() >= 1)
        cout << "FvtxConeTracklets::process_event - INFO - "
            << "Prepared local copy of nDST object with "
            << _muo_local->get_npart() << " tracks" << endl;
    }

  // tracklet finding
  if (_svx_map)
    import_vtx_clusters();
  if (_trk_map)
    import_fvtx_trk();
  if (_svx_map)
    make_tracklets_from_vtx();
  make_tracklets_from_fvtx();

// save info
  save_tracklets();
  process_clusters();

  return EVENT_OK;
}

//______________________________________________________
int
FvtxConeTracklets::End(PHCompositeNode* top_node)
{
  if (make_eval)
    {
      PHTFileServer::get().write(eval_name.c_str());
      //      ntup->Write();
      //      ntupcone->Write();
      //      file->Close();
      //      delete ntup;
      //      delete ntupcone;
    }

  // clean up
  if (_muo_local)
    {
      delete _muo_local;
      _muo_local = NULL;
    }

  return 0;
}

//______________________________________________________
void
FvtxConeTracklets::set_is_sim(bool is_sim)
{
  cout << "FvtxConeTracklets::set_is_sim - WARNING - "
      << "this option is obsolete. Please use set_vertex_name(\"BBC\" \"FVTX\" or \"SIM\") instead."
      << endl;

  if (is_sim)
    set_vertex_name("SIM");
  else
    set_vertex_name("BBC");
}

//______________________________________________________
int
FvtxConeTracklets::process_vertex()
{
  // get the default vertex name
  string vertex_name = get_vertex_name();

  // check whether to use automatic vertex choice
  string vertex_name_tmp = vertex_name;
  boost::algorithm::to_upper(vertex_name_tmp);
  if (vertex_name_tmp == string("AUTO"))
    {
      //automatic vertex choice

      const unsigned int n_mu = _muo->get_npart();

      const PHPoint v_fvtx = vtx->get_Vertex("FVTX");
      const bool v_fvtx_bad = isnan(v_fvtx.getX()) || isnan(v_fvtx.getY())
          || isnan(v_fvtx.getZ()) || abs(v_fvtx.getZ()) > 100;

      const PHPoint v_fvtx_sec = vtx->get_Vertex("FVTX_SECOND");
      const bool v_fvtx_sec_bad = isnan(v_fvtx_sec.getX())
          || isnan(v_fvtx_sec.getY()) || isnan(v_fvtx_sec.getZ())
          || abs(v_fvtx_sec.getZ()) > 100;

      if (v_fvtx_bad && v_fvtx_sec_bad)
        {
          // all FVTX vertex is bad, use BBC then
          vertex_name = "BBC";
        }
      else
        {
          //first priority is FVTX vertexes

          if (!v_fvtx_bad && v_fvtx_sec_bad)
            {
              //there is no 2nd vertex, use first one anyway

              vertex_name = "FVTX";
            }
          else if (v_fvtx_bad && !v_fvtx_sec_bad)
            {
              //there is no 1st vertex, weird. Anyway use 2nd one anyway

              cout
                  << "FvtxConeTracklets::process_vertex - WARNING - Vertex FVTX is missing while FVTX_SECOND presents."
                  << "This is weird, but I will proceed by using FVTX_SECOND anyway.";

              vertex_name = "FVTX_SECOND";
            }
          else
            {
              assert(!v_fvtx_bad);
              assert(!v_fvtx_sec_bad);

              //both vertex is good, choose one
              if (n_mu == 0)
                {
                  vertex_name = "FVTX";
                }
              else
                {
                  // use the vertex with minimal sum DCA

                  double dca_v_fvtx = 0;
                  double dca_v_fvtx_sec = 0;

                  for (unsigned int imu = 0; imu < n_mu; imu++)
                    {
                      PHPoint v;
                      PHVector p;

                      if (_muo->get_fvtx_dr(imu) >= 0)
                        {
                          // have FVTX matching

                          v.setX(_muo->get_fvtxmutr_vtx(imu, 0));
                          v.setY(_muo->get_fvtxmutr_vtx(imu, 1));
                          v.setZ(_muo->get_fvtxmutr_vtx(imu, 2));

                          p.setX(_muo->get_fvtxmutr_p(imu, 0));
                          p.setY(_muo->get_fvtxmutr_p(imu, 1));
                          p.setZ(_muo->get_fvtxmutr_p(imu, 2));

                          assert(v.getX()!=-100);
                        }
                      else
                        {
                          v.setX(_muo->get_xpos(0, imu));
                          v.setY(_muo->get_ypos(0, imu));
                          v.setZ(_muo->get_zpos(0, imu));

                          p.setX(_muo->get_px(0, imu));
                          p.setY(_muo->get_py(0, imu));
                          p.setZ(_muo->get_pz(0, imu));
                        }

                      const PHLine trk(v, p);

                      const PHPoint ca_fvtx =
                          PHGeometry::closestApproachLinePoint(trk, v_fvtx);
                      const PHLine dca_line_fvtx(v_fvtx, ca_fvtx);
                      dca_v_fvtx += dca_line_fvtx.length();

                      const PHPoint ca_fvtx_sec =
                          PHGeometry::closestApproachLinePoint(trk, v_fvtx_sec);
                      const PHLine dca_line_fvtx_sec(v_fvtx_sec, ca_fvtx_sec);
                      dca_v_fvtx_sec += dca_line_fvtx_sec.length();
                    }

                  if (dca_v_fvtx_sec < dca_v_fvtx)
                    {
                      vertex_name = "FVTX_SECOND";
                    }
                  else
                    {
                      vertex_name = "FVTX";
                    } // if (dca2_v_fvtx_sec < dca2_v_fvtx) else

                } //  if (n_mu == 0) else
            } // if (!v_fvtx_bad && v_fvtx_sec_bad) else
        } // if (v_fvtx_bad && v_fvtx_sec_bad) else

      // print result
      static bool once = true;
      if (once || Verbosity() >= 1)
        {
          cout
              << "FvtxConeTracklets::process_vertex - INFO - use automatic vertex choice. "
              << "This event used vertex " << vertex_name << " = ["
              << vtx->get_Vertex(vertex_name.c_str()).getX() << ", "
              << vtx->get_Vertex(vertex_name.c_str()).getY() << ", "
              << vtx->get_Vertex(vertex_name.c_str()).getZ() << "] cm." << endl;

          once = false;
        }

    } // if (vertex_name_tmp == string("AUTO"))

  // Go on to load vertex
  _vertex = vtx->get_Vertex(vertex_name.c_str());
  _vertex_cut = TMath::Max(vtx->get_ZVertexError(get_vertex_name().c_str()) * 2,
      (float) vertex_cut_min);

  const bool bad_vertex = isnan(_vertex.getX()) || isnan(_vertex.getY())
      || isnan(_vertex.getZ()) || isnan(_vertex_cut)
      || abs(_vertex.getZ()) > 100;

  if (bad_vertex)
    {
      _n_bad_vertex++;

      if (is_special_num(_n_bad_vertex) || Verbosity() >= 1)
        {
          cout
              << "FvtxConeTracklets::process_vertex - WARNING - Received an invalid vertex from "
              << vertex_name << ": (";
          cout << _vertex.getX() << ", ";
          cout << _vertex.getY() << ", ";
          cout << _vertex.getZ() << "), ";
          cout << _n_bad_vertex << " events discarted for this reason." << endl;
        }

      return DISCARDEVENT;
    } //   if (bad_vertex)

  return EVENT_OK;
}

//______________________________________________________
void
FvtxConeTracklets::make_tracklets_from_fvtx()
{

  assert(_coord_map);

  if (Verbosity() >= 2)
    cout << "FvtxConeTracklets::make_tracklets_from_fvtx - process  "
        << _coord_map->range().count() << " FVTX clusters" << endl;

  for (int iarm = 0; iarm < FVTXOO::MAX_ARM; iarm++)
    for (int istation1 = 0; istation1 < FVTXOO::MAX_STATION; istation1++)
      for (int icage1 = 0; icage1 < FVTXOO::MAX_CAGE; icage1++)
        for (int isector1 = 0; isector1 < FVTXOO::MAX_SECTOR; isector1++)
          {
            TFvtxCoordMap::iterator coord_iter1 = _coord_map->get(iarm, icage1,
                istation1, isector1);
            while (TFvtxCoordMap::pointer coord_ptr1 = coord_iter1.next())
              {
                // first coord

                long int status1 = coord_ptr1->get()->get_status();
                if (status1 & (fvtx_cluster_status_bit))
                  continue; // alread used in tracklets

                // new tracklet
                _trk.Reset();

                _trk.event_num = _eventCount;
                _trk.vtx_point = convert(_vertex);

                // give a guess of momentum vector
                const PHPoint & vec = coord_ptr1->get()->get_coord_midpoint()
                    - _vertex;
                _trk.trk_par_kalman.set_px(vec.getX());
                _trk.trk_par_kalman.set_py(vec.getY());
                _trk.trk_par_kalman.set_pz(vec.getZ());

                add_cluster(coord_ptr1);

                const int wedge1 = icage1 * FVTXOO::MAX_SECTOR + isector1;
                add_fvtx_clusters(/*const int */iarm, /*const int */istation1, /*const int */
                wedge1);

                if (_trk.get_n_nodes(TFvtxMPNode::FVTX) > 1)
                  {
                    // found one tracklet, then use fast pseudo-fit

                    status1 |= (fvtx_cluster_status_bit);
                    coord_ptr1->get()->set_status(status1);

                    store_tracklet();

                  } //                 if (trk.get_n_nodes(TFvtxMPNode::FVTX) > 1)

              } //             while (TFvtxCoordMap::pointer coord_ptr1 = coord_iter1.next())
          }
}

//______________________________________________________
void
FvtxConeTracklets::add_fvtx_clusters(const int iarm, const int istation1,
    const int wedge1)
{

  // list of wedge for searching, start from the central one
  vector<int> wedge_list;
  wedge_list.push_back(wedge1);
  wedge_list.push_back(wedge1 - 1);
  wedge_list.push_back(wedge1 + 1);

  for (int istation2 = istation1 + 1; istation2 < FVTXOO::MAX_STATION;
      istation2++)
    for (vector<int>::iterator it = wedge_list.begin(); it != wedge_list.end();
        it++)
      {
        const int iwedge = (*it);

        // 2nd or more coord
        int wedge2 = iwedge;
        if (iwedge == -1)
          wedge2 = 2 * FVTXOO::MAX_SECTOR - 1;
        if (iwedge == (2 * FVTXOO::MAX_SECTOR - 1))
          wedge2 = 0;

        int icage2 = wedge2 / FVTXOO::MAX_SECTOR;
        int isector2 = wedge2 % FVTXOO::MAX_SECTOR;
        TFvtxCoordMap::iterator coord_iter2 = _coord_map->get(iarm, icage2,
            istation2, isector2);
        while (TFvtxCoordMap::pointer coord_ptr2 = coord_iter2.next())
          {
            if (coord_ptr2->get()->get_status() & (fvtx_cluster_status_bit))
              continue; // alread used in tracklets

            bool pass = true;
            double diff = -12324;

            if (_trk.get_n_nodes(TFvtxMPNode::FVTX)
                + _trk.get_n_nodes(TFvtxMPNode::VTX) > 1)
              {
                //3rd or more coord
                const double w_det = abs(coord_ptr2->get()->get_w_absolute());

                const double w_fit = _trk.trk_par_wz_fit.get_w_abs_fit(
                    coord_ptr2->get()->get_coord_midpoint().getZ());

                diff = w_det - w_fit;

                if (abs(diff) > w_acpt_cut)
                  pass = false;
              }

            if (pass)
              pass = try_cluster(coord_ptr2);

            if (make_eval)
              {

                const float phi_next = atan2(
                    coord_ptr2->get()->get_coord_midpoint().getY(),
                    coord_ptr2->get()->get_coord_midpoint().getX());

                _trk.calculate_phi_aceptance(Verbosity() - 1, true);
                const float dphi = fmod(
                    phi_next - _trk.phi_acpt_cent + TMath::Pi() * 3,
                    TMath::Pi() * 2) - TMath::Pi();

                ntup->Fill(_eventCount, //          ievt
                    _trk.vtx_point.z(), //vtxZ
                    coord_ptr2->get()->get_coord_midpoint().getZ(), //Z
                    diff, //dw
                    dphi, //phi_acpt
                    (float) pass //accept

                    );

              }
          } //                   for (int iwedge = wedge1 - 1; iwedge <= wedge1 + 1; iwedge++)

      }
}

//! TFvtxMPTrack -> tracklets
void
FvtxConeTracklets::store_tracklet()
{

  _trk.calculate_phi_aceptance(Verbosity() - 1, true);
  _trk.StraightLineFitInWZ_Sili();

  bool pass = true;

  if (_trk.phi_acpt_width < phi_acpt_width_cut)
    {
      pass = false;
    }
  else if (abs(_trk.z0reco_fvtx - _trk.vtx_point.z()) > _vertex_cut)
    {
      pass = false;
    }

  if (!pass)
    return;

  const double theta = atan(_trk.trk_par_wz_fit.get_tw());
  const double eta = -log(tan(0.5 * fmod(theta + TMath::Pi(), TMath::Pi())));

  _tracklets.push_back(make_pair(eta, _trk.phi_acpt_cent));

  if (_save_tracklet)
    {
      // overwrite compact track objects,  this is for debug only

      int iarm = 0;
      if (_trk.get_n_nodes(TFvtxMPNode::FVTX) > 0)
        {
          TFvtxMPNode_FVTX * node =
              dynamic_cast<TFvtxMPNode_FVTX *>(_trk.get_node(TFvtxMPNode::FVTX,
                  _trk.get_n_nodes(TFvtxMPNode::FVTX) - 1));
          assert(node);
          iarm = node->arm;
        }

      TFvtxCompactTrkMap::iterator ctrk_iter = _ctrk_map->insert_new(iarm);
      TFvtxCompactTrkMap::pointer ctrk_ptr = ctrk_iter.current();
      TFvtxCompactTrkMap::value_imp_type * fvtx_trk =
          (TFvtxCompactTrkMap::value_imp_type *) ctrk_ptr->get();
      assert(fvtx_trk);

      fvtx_trk->set_track_vtx(
          PHPoint(_trk.vtx_point.x(), _trk.vtx_point.y(), _trk.z0reco_fvtx));

      fvtx_trk->set_fvtx_phi(_trk.phi_acpt_cent);
      fvtx_trk->set_fvtx_theta(theta);
      fvtx_trk->set_chi2_ndf( // special coding to save # of clusters
          _trk.get_n_nodes(TFvtxMPNode::FVTX) + //
              _trk.get_n_nodes(TFvtxMPNode::VTX) * 10 //
                  );
    }

  if (Verbosity() >= 2)
    {
      cout << "FvtxConeTracklets::store_tracklet - Found one tracklet" << endl;

      _trk.Print();

      cout << "Result : eta = " << eta << ", phi = " << _trk.phi_acpt_cent
          << endl;
    } //                    if (Verbosity() >= 2)

}

//______________________________________________________
void
FvtxConeTracklets::import_fvtx_trk()
{

  assert(_trk_map);

  TFvtxTrkMap::iterator trk_iter(_trk_map->range());

  while (TFvtxTrkMap::pointer trk_ptr = trk_iter.next())
    {

      if (Verbosity() >= 2)
        cout << "FvtxConeTracklets::import_fvtx_trk - process track in arm "
            << trk_ptr->get()->get_arm() << endl;

      _trk.Reset();
      _trk.vtx_point = convert(_vertex);

      // Add FVTX cluster
      TFvtxCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<
          TFvtxCoord>();

      if (Verbosity() >= 2)
        cout << "FvtxConeTracklets::import_fvtx_trk - Info - " << "Import "
            << coord_iter.count() << "FVTX clusters" << endl;

      if (coord_iter.count() == 0)
        {

          static bool once = true;

          if (once)
            {
              cout << "FvtxConeTracklets::import_fvtx_trk - WARNING - "
                  << "FVTX track present but not associated to any cluster. "
                  << "This probably mean track and coordinate are not saved at same time. Ignore FVTX tracks to avoid over counting";
              once = false;
            }
        } // if (coord_iter.count() == 0)
      else
        {
          while (TFvtxCoordMap::pointer coord_ptr = coord_iter.next())
            {
              unsigned long status = coord_ptr->get()->get_status();
              status |= (fvtx_cluster_status_bit);
              coord_ptr->get()->set_status(status);

              add_cluster(coord_ptr);
            }
        } // if (coord_iter.count() == 0) else

      // Add VTX cluster
      TFvtxSvxClusterMap::key_iterator trk_svx_iter =
          trk_ptr->get()->get_associated<TFvtxSvxCluster>();

      if (Verbosity() >= 2)
        cout << "FvtxConeTracklets::import_fvtx_trk - Info - " << "Import "
            << trk_svx_iter.count() << "VTX clusters" << endl;

      while (TFvtxSvxClusterMap::pointer trk_svx_ptr = trk_svx_iter.next())
        {

          const SvxCluster *clus = trk_svx_ptr->get()->get_cluster();
          const int hitID = clus->get_hitID();
          const int layer = clus->get_layer();

          assert(layer >= 0);

          if (layer < n_vtx_layers)
            {

              vtx_cluster_list & list = _vtx_layer_list[layer];

              bool found = false;

              for (vtx_cluster_list::iterator it = list.begin();
                  it != list.end(); it++)
                {
                  if ((*it).second->get_hitID() == hitID)
                    {
                      (*it).first = true;
                      found = true;
                      break;

                    }
                }

              if (!found && _svx_map)
                cout << "FvtxConeTracklets::import_fvtx_trk - Error - "
                    << "can not find the vtx cluster, which belongs to an FVTX track, in book keeping list"
                    << endl;
            } //           if (layer < n_vtx_layers)

          add_cluster(clus);
        }

      // Build track

      assert(
          _trk.get_n_nodes(TFvtxMPNode::FVTX) + _trk.get_n_nodes(TFvtxMPNode::VTX) > 1);
      _trk.trk_par_kalman = (*trk_ptr->get()->get_trk_par_vtx());
      store_tracklet();

    } //  while (TFvtxTrkMap::pointer trk_ptr = trk_iter.next())

}

//______________________________________________________
void
FvtxConeTracklets::make_tracklets_from_vtx()
{

  //-------------------------------------
  // tracklet searching
  //-------------------------------------

  for (int layer = 0; layer < n_vtx_layers; layer++)
    {
      vtx_cluster_list & list = _vtx_layer_list[layer];

      if (Verbosity() >= 2)
        cout << "FvtxConeTracklets::make_tracklets_from_vtx - process layer "
            << layer << " with " << list.size() << " clusters" << endl;

      for (vtx_cluster_list::iterator it = list.begin(); it != list.end(); it++)
        {

          if ((*it).first)
            continue; // already used

          const SvxCluster * vtx_clus1 = (*it).second;
          assert(vtx_clus1);

          // new tracklet
          _trk.Reset();

          _trk.vtx_point = convert(_vertex);

          // give a guess of momentum vector
          const PHPoint & vec = PHPoint( //
              vtx_clus1->get_xyz_global(0), //
              vtx_clus1->get_xyz_global(1), //
              vtx_clus1->get_xyz_global(2) //
                  ) - _vertex;
          _trk.trk_par_kalman.set_px(vec.getX());
          _trk.trk_par_kalman.set_py(vec.getY());
          _trk.trk_par_kalman.set_pz(vec.getZ());

          TFvtxMPNode_VTX * node1 = dynamic_cast<TFvtxMPNode_VTX *>(add_cluster(
              vtx_clus1));
          assert(node1);
          const double vtx_phi1 = atan2(vec.getY(), vec.getX());
          const double vtx_r1 = sqrt(
              vec.getY() * vec.getY() + vec.getX() * vec.getX());
          const double vtx_dz1 = vec.getZ();

          // add next vtx hit
          for (int layer_next = layer + 1; layer_next < n_vtx_layers;
              layer_next++)
            {
              vtx_cluster_list & list_next = _vtx_layer_list[layer_next];

              for (vtx_cluster_list::iterator it_next = list_next.begin();
                  it_next != list_next.end(); it_next++)
                {

                  if ((*it_next).first)
                    continue; // already used

                  const SvxCluster * vtx_clus_next = (*it_next).second;

                  const PHPoint & vec_next = PHPoint( //
                      vtx_clus_next->get_xyz_global(0), //
                      vtx_clus_next->get_xyz_global(1), //
                      vtx_clus_next->get_xyz_global(2) //
                          ) - _vertex;

                  // phi check
                  const double vtx_phi_next = atan2(vec_next.getY(),
                      vec_next.getX());
                  const double dphi = fmod(
                      vtx_phi1 - vtx_phi_next + TMath::Pi() * 3,
                      TMath::Pi() * 2) - TMath::Pi();

                  if (abs(dphi) > abs(phi_acpt_width_cut) / 2.)
                    continue; // large difference in phi

                  // z check
                  const double vtx_r_next = sqrt(
                      vec_next.getY() * vec_next.getY()
                          + vec_next.getX() * vec_next.getX());
                  const double vtx_dz_next = vec_next.getZ();

                  if (vtx_r1 == vtx_r_next)
                    continue; // save radius
                  const double dz_vertex = (vtx_r1 * vtx_dz_next
                      - vtx_r_next * vtx_dz1) / (vtx_r1 - vtx_r_next);

                  if (abs(dz_vertex) > _vertex_cut)
                    continue; // fail vertex cut

                  add_cluster(vtx_clus_next);
                  (*it_next).first = true;

                } //   for (cluster_list::iterator it_next = list_next.begin(); it_next!= list_next.end(); it_next++)

            } //  if (layer <n_vtx_layers - 1)

          if (_trk.get_n_nodes(TFvtxMPNode::VTX) > 1)
            {
              _trk.StraightLineFitInWZ_Sili();
            }

          // add more FVTX hits if possible
          const int iarm = (vtx_dz1 > 0) ? 1 : 0;
          const fvtx_phi_cent_list_arm & phi_list = _fvtx_phi_cent_list[iarm];
          const int istation1 = 0;
          for (fvtx_phi_cent_list_arm::const_iterator phi_it = phi_list.begin();
              phi_it != phi_list.end(); phi_it++)
            {
              const fvtx_phi_cent_record & rec = (*phi_it);

              const double dphi = fmod(vtx_phi1 - rec.second + TMath::Pi() * 3,
                  TMath::Pi() * 2) - TMath::Pi();

              if (abs(dphi) <= TMath::Pi() / FVTXOO::MAX_SECTOR / 2)
                {
                  const int wedge1 = rec.first;

                  add_fvtx_clusters(iarm, istation1, wedge1);

                  break;
                }
            }

          // check if a track is found
          if (_trk.get_n_nodes(TFvtxMPNode::FVTX)
              + _trk.get_n_nodes(TFvtxMPNode::VTX) > 1)
            {
              // found one tracklet, then use fast pseudo-fit
              (*it).first = true;

              store_tracklet();
            } //                 if (trk.get_n_nodes(TFvtxMPNode::FVTX) > 1)

        } //      for (vtx_cluster_list::iterator it = list.begin(); it != list.end(); it++)

    } //   for (int layer = 0; layer < n_vtx_layers; layer++)

}

//______________________________________________________
bool
FvtxConeTracklets::try_cluster(TFvtxCoordMap::pointer &coord)
{
  assert(
      _trk.get_n_nodes(TFvtxMPNode::FVTX) + _trk.get_n_nodes(TFvtxMPNode::VTX) >= 1);

  add_cluster(coord);
  _trk.calculate_phi_aceptance(Verbosity() - 1, true);
  _trk.StraightLineFitInWZ_Sili();

  bool pass = true;

  if (_trk.phi_acpt_width < phi_acpt_width_cut)
    {
      pass = false;
    }
  else if (abs(_trk.z0reco_fvtx - _trk.vtx_point.z()) > _vertex_cut)
    {
      pass = false;
    }

  if (pass)
    {

      // tag clusters as used in tracklet
      long int status = coord->get()->get_status();
      status |= (fvtx_cluster_status_bit);
      coord->get()->set_status(status);

    }
  else
    {
      // failed. pop the last point

      _trk.remove_last_node_fvtx();
      if (_trk.get_n_nodes(TFvtxMPNode::FVTX)
          + _trk.get_n_nodes(TFvtxMPNode::VTX) > 1)
        _trk.StraightLineFitInWZ_Sili();
    }

  return pass;
}

//______________________________________________________
TFvtxMPNode *
FvtxConeTracklets::add_cluster(TFvtxCoordMap::pointer &coord)
{

  TFvtxMPNode_FVTX * node = dynamic_cast<TFvtxMPNode_FVTX *>(_trk.add_node(
      TFvtxMPNode::FVTX));
  assert(node);

  // Hit ID
  node->arm = coord->get()->get_arm();
  node->cage = coord->get()->get_cage();
  node->station = coord->get()->get_station();
  node->sector = coord->get()->get_sector();
  node->column = coord->get()->get_column();
  node->strip = coord->get()->get_peak_strip();

  // Measurement data
  node->meas = (coord->get()->get_w_absolute());
  node->sigma = coord->get()->get_error();
  node->p_det = convert(coord->get()->get_coord_midpoint());
  node->p_strip_begin = convert(coord->get()->get_coord_begin());
  node->p_strip_end = convert(coord->get()->get_coord_end());

  return node;
}

//______________________________________________________
TFvtxMPNode *
FvtxConeTracklets::add_cluster(const SvxCluster * clus)
{

  TFvtxMPNode_VTX * node = dynamic_cast<TFvtxMPNode_VTX *>(_trk.add_node(
      TFvtxMPNode::VTX));
  assert(node);

  node->switch_r_phi = true;
  node->svxSection = (short) clus->get_svxSection();
  node->layer = (short) clus->get_layer();
  node->ladder = (short) clus->get_ladder();
  node->sensor = (short) clus->get_sensor();

  // Measurement data
  const double x_global = clus->get_xyz_global(0);
  const double y_global = clus->get_xyz_global(1);
  const double z_global = clus->get_xyz_global(2);

  node->p_det.SetXYZ(x_global, y_global, z_global);

  const double r = node->p_det.Perp();

  double sigma = .05;

  node->meas = r;
  node->sigma = sigma;

  return node;
}

//______________________________________________________
//! dR -> bin #
size_t
FvtxConeTracklets::get_bin(double delta)
{

  const double log_bin_size = (log(_max_cone) - log(_min_cone)) / (n_bin - 1);
  int bin = floor((log(delta) - log(_min_cone)) / log_bin_size) + 1;
  if (bin < 0)
    bin = 0;

  return (size_t) bin;
}

//______________________________________________________
//! min cut for bin
double
FvtxConeTracklets::get_bin_floor(size_t bin)
{
  if (bin == 0)
    return 0;
  else
    return exp(
        (log(_max_cone) - log(_min_cone)) * (bin - 1) / (n_bin - 1)
            + log(_min_cone));
}

//______________________________________________________
//! tracklets -> PHMuoTracksOut
void
FvtxConeTracklets::save_tracklets()
{

  PHMuoTracksOut * _muo_dest = _is_new_nDST ? _muo : _muo_local;
  assert(_muo_dest);
  assert(_muo_dest -> get_npart() == _muo->get_npart());

  // Loop over single muons to fill container
  for (size_t imu = 0; imu < _muo->get_npart(); imu++)
    {
      // save total count. At this moment, all tracks are assumed from save vertex therefore have same of traclet count
      _muo_dest->set_nfvtx_tracklets(imu, _tracklets.size());

      bool match_fvtx = false;
      PHVector dir = PHVector(_muo->get_px(0, imu), _muo->get_py(0, imu),
          _muo->get_pz(0, imu));

      PHPoint base = PHPoint(_muo->get_xpos(0, imu), _muo->get_ypos(0, imu),
          _muo->get_zpos(0, imu));

      if (_muo->get_fvtx_dphi(imu) > -10)
        {
          dir = PHVector(_muo->get_fvtxmutr_p(imu, 0),
              _muo->get_fvtxmutr_p(imu, 1), _muo->get_fvtxmutr_p(imu, 2));

          base = PHPoint(_muo->get_fvtxmutr_vtx(imu, 0),
              _muo->get_fvtxmutr_vtx(imu, 1), _muo->get_fvtxmutr_vtx(imu, 2));
          match_fvtx = true;
        }

      float theta = acos(dir.getZ() / dir.length());
      float phi = atan2(dir.getY(), dir.getX());
      float eta = -log(tan(theta / 2));
      for (vector<tracklet_t>::iterator tracklets_iter = _tracklets.begin();
          tracklets_iter != _tracklets.end(); tracklets_iter++)
        {
          const double eta_tracklet = (*tracklets_iter).first;
          const double phi_tracklet = (*tracklets_iter).second;

          // calcuate angle between tracks

          const double deta = eta - eta_tracklet;
          const double dphi = fmod(phi - phi_tracklet + 3 * TMath::Pi(),
              2 * TMath::Pi()) - TMath::Pi();
          const double delta = sqrt(deta * deta + dphi * dphi);

          size_t bin = get_bin(delta);
          if (bin < n_bin)
            {
              size_t ntracklets = _muo_dest->get_nfvtx_tracklets_conerange(imu,
                  bin) + 1;
              _muo_dest->set_nfvtx_tracklets_conerange(imu, bin, ntracklets);
            }

          if (Verbosity() >= 2)
            {
              cout
                  << "FvtxConeTracklets::make_tracklets - matching tracklet to mutr"
                  << endl;
              cout << "\t momentum = ";
              dir.print();
              cout << "\t match_fvtx = " << match_fvtx << endl;
              cout << "\t eta = " << eta_tracklet << endl;
              cout << "\t phi = " << phi_tracklet << endl;
              cout << "\t eta_tracklet = " << eta_tracklet << endl;
              cout << "\t phi_tracklet = " << phi_tracklet << endl;
              cout << "\t deta = " << deta << endl;
              cout << "\t dphi = " << dphi << endl;
              cout << "\t delta = " << delta << endl;
              cout << "\t bin = " << bin << endl;
            }
        }
    }
}

//______________________________________________________
//! TFvtxSvxClusterMap  -> layer_list
void
FvtxConeTracklets::import_vtx_clusters()
{
  // loop vtx clusters
  assert(_svx_map);

  assert(_vtx_layer_list.empty());

  _vtx_layer_list.resize(n_vtx_layers);
  if (Verbosity() >= 2)
    cout << "FvtxConeTracklets::import_vtx_clusters - Importing ";
  TFvtxSvxClusterMap::const_iterator clus_iter = _svx_map->range();
  while (TFvtxSvxClusterMap::const_pointer clus_ptr = clus_iter.next())
    {
      const SvxCluster *clus = clus_ptr->get()->get_cluster();
      assert(clus);

      if (Verbosity() >= 2)
        {
          cout << "\t" << clus->get_hitID() << " [";
          cout << clus->get_svxSection() << ", ";
          cout << clus->get_layer() << ", ";
          cout << clus->get_ladder() << ", ";
          cout << clus->get_sensor() << "]" << endl;
        }

      if (clus->get_layer() < n_vtx_layers)
        {
          (_vtx_layer_list[clus->get_layer()]).push_back(
              make_pair(false, (const SvxCluster *) clus));
        }
    }

  if (Verbosity() >= 2)
    cout << "FvtxConeTracklets::import_vtx_clusters - Finish import of "
        << _vtx_layer_list[0].size() << "/" << _vtx_layer_list[1].size()
        << " clusters for layer 0/1" << endl;
}

//______________________________________________________
//! TFvtxCoordMap & TFvtxSvxClusterMap -> PHMuoTracksOut
void
FvtxConeTracklets::process_clusters()
{

  PHMuoTracksOut * _muo_dest = _is_new_nDST ? _muo : _muo_local;
  assert(_muo_dest);
  assert(_muo_dest -> get_npart() == _muo->get_npart());

  // loop muon tracks
  const unsigned int n_mu = _muo->get_npart();

  //! most compact descript of track, pair of eta and phi
  std::vector<tracklet_t> mu_trk;

  //! projection of track to the vertex x-y plane
  std::vector<PHPoint> mu_trk_vtx;

  // Loop over single muons to fill container
  for (size_t imu = 0; imu < n_mu; imu++)
    {
      PHPoint v, v_proj;
      PHVector p;

      if (_muo->get_fvtx_dr(imu) >= 0)
        {
          // have FVTX matching

          v.setX(_muo->get_fvtxmutr_vtx(imu, 0));
          v.setY(_muo->get_fvtxmutr_vtx(imu, 1));
          v.setZ(_muo->get_fvtxmutr_vtx(imu, 2));

          p.setX(_muo->get_fvtxmutr_p(imu, 0));
          p.setY(_muo->get_fvtxmutr_p(imu, 1));
          p.setZ(_muo->get_fvtxmutr_p(imu, 2));

          assert(v.getX()!=-100);
        }
      else
        {
          v.setX(_muo->get_xpos(0, imu));
          v.setY(_muo->get_ypos(0, imu));
          v.setZ(_muo->get_zpos(0, imu));

          p.setX(_muo->get_px(0, imu));
          p.setY(_muo->get_py(0, imu));
          p.setZ(_muo->get_pz(0, imu));
        }

      const double theta = acos(p.getZ() / p.length());
      const double phi = atan2(p.getY(), p.getX());
      const double eta = -log(tan(theta / 2));

      mu_trk.push_back(
          make_pair<tracklet_t::first_type, tracklet_t::second_type>(eta, phi));

      const PHLine trk(v, p);
      const PHPlane vertex_plane(_vertex, PHVector(0, 0, 1));
      PHGeometry::intersectionLinePlane(trk, vertex_plane, v_proj);

      mu_trk_vtx.push_back(v_proj);

      if (Verbosity() >= 1)
        {
          cout
              << "FvtxConeTracklets::process_clusters - Info - Loading muon track #"
              << imu << ":" << endl;
          cout << "\t v = ";
          v.print();
          cout << "\t p = ";
          p.print();
          cout << "\t vertex = ";
          _vertex.print();
          cout << "\t v_proj = ";
          v_proj.print();
        }
    }
  assert(mu_trk.size() == n_mu);

  assert(_coord_map);

  // loop fvtx clusters
  TFvtxCoordMap::const_iterator coord_iter = _coord_map->range();
  while (TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next())
    {
      const PHPoint coord = coord_ptr->get()->get_coord_midpoint();
      for (unsigned int imu = 0; imu < n_mu; imu++)
        {
          const PHPoint & vertex_proj = mu_trk_vtx[imu];

          PHVector dir = coord - vertex_proj;

          const double theta = acos(dir.getZ() / dir.length());
          const double phi = atan2(dir.getY(), dir.getX());
          const double eta = -log(tan(theta / 2));

          const double eta_mu = (mu_trk[imu]).first;
          const double phi_mu = (mu_trk[imu]).second;

          // calcuate angle between tracks

          const double deta = eta - eta_mu;
          const double dphi = fmod(phi - phi_mu + 3 * TMath::Pi(),
              2 * TMath::Pi()) - TMath::Pi();
          const double delta = sqrt(deta * deta + dphi * dphi);

          size_t bin = get_bin(delta);
          if (bin < n_bin)
            {
              const size_t nclusters = _muo_dest->get_nfvtx_clusters_conerange(
                  imu, bin) + 1;
              _muo_dest->set_nfvtx_clusters_conerange(imu, bin, nclusters);
            } //          if (bin < n_bin)

          if (Verbosity() >= 2)
            {
              cout
                  << "FvtxConeTracklets::process_clusters - Found one FVTX cluster:"
                  << endl;

              cout << "\t eta_mu = \t" << eta_mu << endl;
              cout << "\t phi_mu = \t" << phi_mu << endl;
              cout << "\t eta = \t" << eta << endl;
              cout << "\t phi = \t" << phi << endl;
              cout << "\t deta = \t" << deta << endl;
              cout << "\t dphi = \t" << dphi << endl;
              cout << "\t delta = \t" << delta << endl;
              cout << "\t bin = \t" << bin << endl;
              cout << "\t bin count = \t"
                  << _muo_dest->get_nfvtx_clusters_conerange(imu, bin) << endl;

            }

        } //      for (unsigned int imu = 0; imu < n_mu; imu++)
    } //   while (TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next())

  // loop vtx clusters
  if (_svx_map)
    {
      TFvtxSvxClusterMap::const_iterator clus_iter = _svx_map->range();
      while (TFvtxSvxClusterMap::const_pointer clus_ptr = clus_iter.next())
        {
          const SvxCluster *clus = clus_ptr->get()->get_cluster();
          assert(clus);

          const PHPoint coord( //
              clus->get_xyz_global(0), //
              clus->get_xyz_global(1), //
              clus->get_xyz_global(2) //
                  );
          for (unsigned int imu = 0; imu < n_mu; imu++)
            {

              const PHPoint & vertex_proj = mu_trk_vtx[imu];

              PHVector dir = coord - vertex_proj;

              const double theta = acos(dir.getZ() / dir.length());
              const double phi = atan2(dir.getY(), dir.getX());
              const double eta = -log(tan(theta / 2));

              const double eta_mu = (mu_trk[imu]).first;
              const double phi_mu = (mu_trk[imu]).second;

              // calcuate angle between tracks

              const double deta = eta - eta_mu;
              const double dphi = fmod(phi - phi_mu + 3 * TMath::Pi(),
                  2 * TMath::Pi()) - TMath::Pi();
              const double delta = sqrt(deta * deta + dphi * dphi);

              size_t bin = get_bin(delta);
              if (bin < n_bin)
                {
                  const size_t nclusters =
                      _muo_dest->get_nfvtx_clusters_conerange(imu, bin) + 1;
                  _muo_dest->set_nfvtx_clusters_conerange(imu, bin, nclusters);
                } //          if (bin < n_bin)

              if (Verbosity() >= 2)
                {
                  cout
                      << "FvtxConeTracklets::process_clusters - Found one VTX cluster:"
                      << endl;

                  cout << "\t eta_mu = \t" << eta_mu << endl;
                  cout << "\t phi_mu = \t" << phi_mu << endl;
                  cout << "\t eta = \t" << eta << endl;
                  cout << "\t phi = \t" << phi << endl;
                  cout << "\t deta = \t" << deta << endl;
                  cout << "\t dphi = \t" << dphi << endl;
                  cout << "\t delta = \t" << delta << endl;
                  cout << "\t bin = \t" << bin << endl;
                  cout << "\t bin count = \t"
                      << _muo_dest->get_nfvtx_clusters_conerange(imu, bin)
                      << endl;

                }

            } //      for (unsigned int imu = 0; imu < n_mu; imu++)
        } //   while (TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next())
    }
  else
    {

      if (Verbosity() >= 2)
        {
          cout << "FvtxConeTracklets::process_clusters - Missing VTX clusters"
              << endl;
        }
    }
}
