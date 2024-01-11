// $Id: RxnpAnaDst.h,v 1.8 2007/11/30 20:16:35 hpereira Exp $

#ifndef __RXNPANADST_H__
#define __RXNPANADST_H__

/*!
  \file    RxnpAnaDst.h
  \ingroup supermodules 
  \brief   Rxnp reconstruction event loop
  \author  Chun Zhang
  \version $Revision: 1.8 $
  \date    $Date: 2007/11/30 20:16:35 $
*/

#include <string>
#include <MuonSubsysReco.h>

// Forward declerations
//
//ROOT
//
class TTree;
class TFile;

// PHENIX
class PHCompositeNode;

#ifndef __CINT__
#include <PHTimeServer.h>
#include <mRxnpCalXang.h>
#endif

/*!
  \class   RxnpAnaDst
  \ingroup supermodules 
  \brief   Rxnp reconstruction event loop
*/

class RxnpAnaDst: public MuonSubsysReco
{
 public:

  //! constructor
  RxnpAnaDst(std::string outfile="rxnpAnaDst.root");

  //! destructor
  virtual ~RxnpAnaDst()
  {}
  
  //! full initialization
  int Init(PHCompositeNode *topNode);
  
  //! full initialization
  int InitRun(PHCompositeNode *topNode);
  
  //! print parameters defined in this module
  // void print_parameters() const;                   
  
  //! event processing method
  int process_event(PHCompositeNode *topNode);
  
  //! end of run method
  int End(PHCompositeNode *topNode);

  //
  void set_iter(int iter) { _iter = iter;}

  //! set output file name
  //
  void setOutput(std::string outfile) 
  {_outfile = outfile;}
  
 protected:

  //! create all mutoo nodes
  int CreateNodeTree(PHCompositeNode *topNode);

  //! init TTree
  //
  void initTTree();
  //! init global tree
  //
  void initGlobalTree();
  //! fill the tree
  //
  void fill_tree(PHCompositeNode *node);
  //! fill the tree
  //
  void fill_GlobalTree(PHCompositeNode *node);

  // Node tree data members
  
  //! RxnpNode
  PHCompositeNode *rxnp_node;
  
  //! dst io node
  PHCompositeNode *dst_node;

  // output root filename
  std::string _outfile;

  #ifndef __CINT__
  
  //! flattening and raw reaction calculation
  mRxnpCalXang _mRxnpCalXang_mod;
  
  //! supermodule timer
  PHTimeServer::timer _timer;
  
  #endif
  
  // TFile
  TFile* _anaOut;

  // output TTree
  TTree* _anaTree;

  // data fields for TTree
  unsigned int _run;
  unsigned int _evt;

  unsigned short _arm[48];
  unsigned short _ring[48];
  unsigned short _scint[48];
  unsigned short _chanid[48];
  unsigned short _high_pre[48];
  unsigned short _high_post[48];
  unsigned short _low_pre[48];
  unsigned short _low_post[48];
  unsigned short _tdc[48];
  unsigned short _amu_pre[48];
  unsigned short _amu_post[48];
  unsigned short _amu_tdc[48];

  double _phi[48];
  double _theta[48];
  double _high_e[48];
  double _low_e[48];
  double _tof_c[48];

  double _xang_1H[9];
  double _xang_1L[9];
  double _xang_2H[9];
  double _xang_2L[9];
  double _xang_3H[9];
  double _xang_3L[9];
  double _xang_4H[9];
  double _xang_4L[9];
  double _charge[3];
  double _time[3];
  double _nhit[3];
  double _zvertex;

  double _bbcZvertex;
  double _bbcTimeZero;
  int    _bbcNpmt[2];
  double _bbcCharge[2];
  double _zdcZvertex;
  double _zdcTimeZero;
  double _zdcEnergy[2];
  double _smdEnergy[2];
  double _smdXpos[2];
  double _smdYpos[2];
  double _bbcAdc[128];
  double _bbcTdc0[128];
  short  _bbcTdc1[128];

  // time of iteration in flattening
  int _iter;

  // add global BBC and ZDC info
  //
  TTree* _global;
  
  // bit 0 BBCLL1, 1 BBCLL1 wide; 2 ZDCLL1; 3 ZDCLL1 wide; 4 BBCLL1&ZDC wide
  unsigned int _trig_raw;
  unsigned int _trig_scaled;
  unsigned int _trig_live;
  // BBC south fired tube;
  unsigned int _nbbc_tube_S;
  // BBC charge sum south;
  double _bbc_q_S;
  // BBC north fired tube;
  unsigned int _nbbc_tube_N;
  // BBC charge sum north;
  double _bbc_q_N;
  // ZDC south e;
  double _zdc_e_S;
  // ZDC south e;
  double _zdc_e_N;
  
  // zvtx
  //
  // bbc
  double _zvtx;
  // zdc
  double _zdc_zvtx;

};

#endif /* __RXNPANADST_H__ */







