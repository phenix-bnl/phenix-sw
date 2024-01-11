#ifndef __FVTXALIGN_H__
#define __FVTXALIGN_H__

/*!
  \file FvtxAlign.cxx 
  \ingroup supermodules
  \brief fvtx align module
  \author Zhengyun You
  \version $Revision: 1.1 $
  \date $Date: 2011/01/05 20:18:54 $
*/

#include "SubsysReco.h"

#ifndef __CINT__
#include<bitset>
#include<boost/array.hpp>
#include<PHTimeServer.h>
#endif

class FvtxAlign: public SubsysReco
{
  public:

  //! construct
  FvtxAlign( const char* name = "FVTXALIGN", const char* file = "fvtx_align_ntuples.root");

  //! destructor
  virtual ~FvtxAlign()
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

  #ifndef __CINT__
  //! alignment variable
  boost::array<float,14> _comp_var;
  
  //! module timer
  PHTimeServer::timer _timer;
  
  #endif

};

#endif /* __FVTXALIGN_H__ */


