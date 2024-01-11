// $Id: FvtxVertexTest.cxx,v 1.3 2014/01/24 11:23:34 bbannier Exp $                                                                                             

/*!
 * \file FvtxVertexTest.cxx
 * \brief shift VTX vertex with an given correction for FVTX testing purpose.
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.3 $
 * \date $Date: 2014/01/24 11:23:34 $
 */

#include <FVTXOO.h>
#include <VtxOut.h>
#include <RunHeader.h>
#include <PHCylPoint.h>
#include <PHGeometry.h>
#include <PHException.h>
#include <recoConsts.h>
#include <TMutNode.h>

#include <Fun4AllReturnCodes.h>

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <cassert>
#include <cmath>

#include "FvtxVertexTest.h"

using namespace std;

FvtxVertexTest::FvtxVertexTest(const char* name) :
    MuonSubsysReco(name), //
    _vtxout_node(NULL), //
    _run_header(NULL), //
    _run_num(0), //
    _vtxxp(-9999), //
    _vtxyp(-9999), //
    _vtxzp(-9999), //
    _vtxxp_err(-9999), //
    _vtxyp_err(-9999), //
    _vtxzp_err(-9999), //
    _vertex_name("SVX_PRECISE"),
    _VTX_Avg_X(0), //
    _VTX_Avg_Y(0), //
    _VTX_Shift_X(0), //
    _VTX_Shift_Y(0) //
{
}

FvtxVertexTest::~FvtxVertexTest()
{
}

//____________________________________________________________________
int
FvtxVertexTest::Init(PHCompositeNode *top_node)
{

  // call base class initialization
  // this is needed to get the module row (in list of registered modules) set properly
  MuonSubsysReco::Init(top_node);

  MUTOO::PRINT(cout, "FvtxVertexTest::Init with top_node");

  cout
      << "WARNING : you are loading FvtxVertexTest module, which shift VTX vertex with an given correction for testing purpose."
      << endl;
  cout << "          This is for testing FVTX performance only" << endl;
  cout
      << "          Please do NOT use this module for production or any physics analysis"
      << endl;

  MUTOO::PRINT(cout, "**");

  return EVENT_OK;
}

//____________________________________________________________________
int
FvtxVertexTest::InitRun(PHCompositeNode *top_node)
{

  // call base class initialization
  // this is needed to get the module row (in list of registered modules) set properly
  MuonSubsysReco::InitRun(top_node);

  MUTOO::PRINT(cout, "FvtxVertexTest::InitRun with top_node");
  // Vertex module
  try
    {
      _vtxout_node = TMutNode<VtxOut>::find_io_node(top_node, "VtxOut");
    }
  catch (const std::exception& e)
    {
      FVTXOO::TRACE(e.what());
    }

  // Get run number
  try
    {
      _run_header = TMutNode<RunHeader>::find_io_node(top_node, "RunHeader");
    }
  catch (...)
    {
      _run_header = NULL;
    }

  if (_run_header)
    _run_num = _run_header->get_RunNumber();
  else
    {
      recoConsts *rc = recoConsts::instance();

      _run_num = rc->get_IntFlag("RUNNUMBER");
    }

  // Get offsets
  if (!_beam_pos_xy.have_run(_run_num))
    {
      cout
          << "FvtxVertexTest::InitRun - Error - Do not have average beam XY for run "
          << _run_num << ". Use 0 cm instead." << endl;

      _VTX_Avg_X = 0;
      _VTX_Avg_Y = 0;
    }
  else
    {
      _VTX_Avg_X = _beam_pos_xy.get_x(_run_num);
      _VTX_Avg_Y = _beam_pos_xy.get_y(_run_num);
    }

  // Print summary
  cout
      << "FvtxVertexTest::InitRun - Info - Set new vertex position for run "
      << _run_num << endl;
  cout << "                                 Average X = " << _VTX_Avg_X << "cm"
      << endl;
  cout << "                                 Average Y = " << _VTX_Avg_Y << "cm"
      << endl;
  cout << "                                 Shift in X = " << _VTX_Shift_X
      << "cm" << endl;
  cout << "                                 Shift in Y = " << _VTX_Shift_Y
      << "cm" << endl;
  cout << "                                 Input Vertex = " << _vertex_name <<"_TEST"
      << endl;
  cout << "                                 Output Vertex = " << _vertex_name <<"_AVE_XY and "<< _vertex_name <<"_TEST_AVE_XY  "<< endl;

  MUTOO::PRINT(cout, "**");

  return EVENT_OK;
}

//______________________________________________________
int
FvtxVertexTest::process_event(PHCompositeNode *top_node)
{
  MuonSubsysReco::process_event(top_node);

  if (!_vtxout_node)
    {
      cout << PHWHERE << " VtxOut not in Node Tree" << endl;
      return ABORTRUN;
    }

  _vtxxp = _vtxout_node->get_Vertex(_vertex_name.c_str()).getX();
  _vtxyp = _vtxout_node->get_Vertex(_vertex_name.c_str()).getY();
  _vtxzp = _vtxout_node->get_Vertex(_vertex_name.c_str()).getZ();

  _vtxxp_err = _vtxout_node->get_VertexError(_vertex_name.c_str()).getX();
  _vtxyp_err = _vtxout_node->get_VertexError(_vertex_name.c_str()).getY();
  _vtxzp_err = _vtxout_node->get_VertexError(_vertex_name.c_str()).getZ();

  if (isnan(_vtxxp))
    {
      _vtxxp = -99999;
    }
  if (isnan(_vtxyp))
    {
      _vtxyp = -99999;
    }
  if (isnan(_vtxzp))
    {
      _vtxzp = -99999;
    }
  if (isnan(_vtxxp_err))
    {
      _vtxxp_err = 99999;
    }
  if (isnan(_vtxyp_err))
    {
      _vtxyp_err = 99999;
    }
  if (isnan(_vtxzp_err))
    {
      _vtxzp_err = 99999;
    }

  float vertex_cor[100] = {0};
  float vertex_cor_ave_xy[100] = {0};
  float vertex_cor_error[100] = {0};

  vertex_cor[0] = _vtxxp + _VTX_Shift_X;
  vertex_cor[1] = _vtxyp + _VTX_Shift_Y;
  vertex_cor[2] = _vtxzp;

  vertex_cor_ave_xy[0] = _VTX_Avg_X;
  vertex_cor_ave_xy[1] = _VTX_Avg_Y;
  vertex_cor_ave_xy[2] = _vtxzp;

  vertex_cor_error[0] = _vtxxp_err;
  vertex_cor_error[1] = _vtxyp_err;
  vertex_cor_error[2] = _vtxzp_err;

  string v_test = _vertex_name + "_TEST";
  string v_test_ave = _vertex_name + "_TEST_AVE_XY";

  _vtxout_node->AddVtx(v_test.c_str(), //
      vertex_cor, vertex_cor_error, -11);
  _vtxout_node->AddVtx(v_test_ave.c_str(), //
      vertex_cor_ave_xy, vertex_cor_error, -12);

  return EVENT_OK;
}

//______________________________________________________
void
FvtxVertexTest::load_beam_xy_shift(string data_file)
{
  cout << "FvtxVertexTest::load_beam_xy_shift - Info - loading data file "
      << data_file << endl;

  fstream f;
  try
    {
      f.open(data_file.c_str(), ios_base::in);
    }
  catch (...)
    {
      cout << "FvtxVertexTest::load_beam_xy_shift - Error - cannot open "
          << data_file << endl;
      return;
    }

  if (!f.is_open())

    {
      cout << "FvtxVertexTest::load_beam_xy_shift - Error - cannot open "
          << data_file << endl;

      return;
    }

  try
    {
      string line;
      bool loadx = true;
      while (!f.eof())
        {
          getline(f, line);

          if (line.at(0) == '#')
            {
              continue;
            }

          stringstream sline(line);
          if (loadx)
            {
              sline >> _VTX_Shift_X;
              loadx = false;
            }
          else
            {
              sline >> _VTX_Shift_Y;
              break;
            }

        }
    }
  catch (...)
    {
      cout << "FvtxVertexTest::load_beam_xy_shift - Error - cannot parse "
          << data_file << endl;
      return;
    }

  f.close();

  cout
      << "FvtxVertexTest::load_beam_xy_shift - Info - load beam shift [x y] = ["
      << _VTX_Shift_X << ", " << _VTX_Shift_Y << "] cm" << endl;
}

//______________________________________________________
void
FvtxVertexTest::BeamPosXY::load_data(string data_file, int verbose)
{
  cout << "FvtxVertexTest::BeamPosXY::load_data - Info - loading data file "
      << data_file << endl;

  fstream fdata;
  try
    {
      fdata.open(data_file.c_str(), ios_base::in);
    }
  catch (const std::exception& e)
    {
      cout << "FvtxVertexTest::BeamPosXY::load_data - Error - cannot open "
          << data_file << endl;
      FVTXOO::TRACE(e.what());
      return;
    }

  if (!fdata.is_open())
    {
      cout
          << "FvtxVertexTest::BeamPosXY::load_data - Error - cannot open data file "
          << data_file << endl;
      return;
    }

  while (!fdata.eof())
    {
      int run =  -9999;
      double x = -9999, y = -9999, ex = -9999, ey = -9999;

      fdata >> run >> x >> y >> ex >> ey;
      if (verbose>=2)
        cout
            << "FvtxVertexTest::BeamPosXY::load_data - Info - Single record for beam positon of run "
            << run << " = [" << x << " , " << y << "] +/- "
            << " [" << ex << " , "
            << ey << "] cm" << endl;

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
      iter++)
    {
      const int run = iter->first;
      pos xy = iter->second;
      pos xy_weight = weight[run];

      if (verbose)
        cout
            << "FvtxVertexTest::BeamPosXY::load_data - Info - beam positon for run "
            << run << " = [" << xy.first << " , " << xy.second << "] +/- "
            << " [" << 1 / sqrt(xy_weight.first) << " , "
            << 1 / sqrt(xy_weight.second) << "] cm" << endl;
    }
}

//________________________________________
bool
FvtxVertexTest::BeamPosXY::have_run(int run)
{
  return position.count(run) > 0;
}

//________________________________________
double
FvtxVertexTest::BeamPosXY::get_x(int run)
{
  return position[run].first;
}

//________________________________________
double
FvtxVertexTest::BeamPosXY::get_y(int run)
{
  return position[run].second;
}

