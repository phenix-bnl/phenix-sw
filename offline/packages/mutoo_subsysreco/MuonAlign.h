// $Id: MuonAlign.h,v 1.6 2007/12/10 12:59:48 hpereira Exp $
#ifndef __MUONALIGN_H__
#define __MUONALIGN_H__

#include "SubsysReco.h"

/*!
\file    MuonAlign.h
\ingroup supermodules 
\brief   muon alignment module [old]
\author  S. Kelly
\version $Revision: 1.6 $
\date    $Date: 2007/12/10 12:59:48 $
*/

// Forward declerations
//

class PHCompositeNode;
class TNtuple;

#ifndef __CINT__
#include<bitset>
#include<TMutTrkMap.h>
#include<boost/array.hpp>
#include<PHTimeServer.h>
#endif

//! muon alignment module [old]
class MuonAlign: public SubsysReco
{
  public:
  
  //! constructor
  MuonAlign( const char* name = "MUONALIGN", const char* file = "muon_align_ntuples.root");
  
  //! destructor
  virtual ~MuonAlign() 
  {}
  
  //! run init
  int InitRun(PHCompositeNode *topNode);
  
  //! event method
  int process_event(PHCompositeNode *topNode);
  
  //! end of process
  int End(PHCompositeNode *topNode);
  
  //! root output filename
  void set_filename( const char* file )
  { if( file ) _filename = file; }
  
  protected:
  
  //! write alignment ntuples
  void write_align_ntuple(PHCompositeNode* top_node);
  
  //! root output filename
  std::string _filename;
  
  // //! alignment ntuple (v1)
  //TNtuple* _align_vars1;
  
  //! alignment ntuple (v2)
  TNtuple* _align_vars2;
  
  #ifndef __CINT__
  //! alignment variable
  boost::array<float,14> _comp_var;
  
  //! module timer
  PHTimeServer::timer _timer;
  
  #endif
  
  UShort_t _nvtx_o;
  UShort_t _nvtx_n;
};

#endif /* __MUIDEFFIC_H__ */
