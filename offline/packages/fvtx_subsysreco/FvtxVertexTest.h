// $Id: FvtxVertexTest.h,v 1.2 2013/03/21 04:00:53 jinhuang Exp $                                                                                             

/*!
 * \file FvtxVertexTest.h
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.2 $
 * \date $Date: 2013/03/21 04:00:53 $
 */

#ifndef FVTXVERTEXTEST_H_
#define FVTXVERTEXTEST_H_

#include "MuonSubsysReco.h"
#include <string>

class RunHeader;
class VtxOut;

/*!
 * \brief shift VTX vertex with an given correction for FVTX testing purpose.
 *
 * More detail at https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php?title=FVTX/Vertex_testing_module
 */
class FvtxVertexTest : public MuonSubsysReco
{
public:
  FvtxVertexTest(const char* name = "FvtxVertexTest");
  virtual
  ~FvtxVertexTest();

  //! module initialization
  int
  Init(PHCompositeNode *top_node);

  //! run initialization
  int
  InitRun(PHCompositeNode *topNode);

  //! event processing
  int
  process_event(PHCompositeNode *topNode);

private:

  //! vertex node
  VtxOut* _vtxout_node;

  //! header pointers
  RunHeader* _run_header;

  int _run_num;

  double _vtxxp; //! vtx preceise vertex
  double _vtxyp; //! vtx preceise vertex
  double _vtxzp; //! vtx preceise vertex

  double _vtxxp_err; //! Error for vtx preceise vertex
  double _vtxyp_err; //! Error for vtx preceise vertex
  double _vtxzp_err; //! Error for vtx preceise vertex

  // --------------------------------------------------------------------------
  //!@name beam position inputs
//@{

public:

  std::string get_vertex_name()
  {
    return _vertex_name;
  }
  void set_vertex_name(std::string n)
  {
    _vertex_name = n;
  }

  void
  load_default_data()
  {
    load_beam_xy_shift(
        "/afs/rhic.bnl.gov/phenix/users/jinhuang/public/FVTX/run12_pp_510/BeamPos.Shift.dat");
//    load_beam_xy_data("/afs/rhic.bnl.gov/phenix/users/jinhuang/public/FVTX/run12_pp_510/BeamPos.GoldenDimuon.dat");
    load_beam_xy_data(
        "/afs/rhic.bnl.gov/phenix/users/jinhuang/public/FVTX/run12_pp_510/BeamPos.GoldenDimuon.Smooth_Shifted.dat");
  }

  //! call this function to load beam position data
  //! in the order of file >> run >> x >> y >> ex >> ey;
  void
  load_beam_xy_data(std::string data_file = "BeamPos.dat")
  {
    _beam_pos_xy.load_data(data_file, Verbosity());
  }

  void
  load_beam_xy_shift(std::string data_file = "BeamPos.dat");

  //! get precise beam X-Y position from off-line fit of VTX vertex
  class BeamPosXY
  {
  public:
    //! load data files
    void
    load_data(std::string data_file = "BeamPos.dat", int verbose = 0);

    //! have this run in data file?
    bool
    have_run(int run);

    //! get beam position in x
    double
    get_x(int run);

    //! get beam position in y
    double
    get_y(int run);

  protected:
    //! position in x y
    typedef std::pair<double, double> pos;
    typedef std::map<int, pos> record;

    //! map to run -> pos
    record position;
    //! map to run -> weight = 1/error^2
    record weight;

  };

private:

  std::string _vertex_name;

  BeamPosXY _beam_pos_xy;

  double _VTX_Avg_X;
  double _VTX_Avg_Y;

  double _VTX_Shift_X;
  double _VTX_Shift_Y;

  //@}

};

#endif /* FVTXVERTEXTEST_H_ */
