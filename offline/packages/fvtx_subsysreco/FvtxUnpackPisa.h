// $Id: FvtxUnpackPisa.h,v 1.6 2009/01/20 15:38:38 winter Exp $
#ifndef __FvtxUnpackPisa_H__
#define __FvtxUnpackPisa_H__

/*!
  \file FvtxUnpackPisa.h	
  \ingroup supermodules
  \brief fvtx pisa to dst supermodule
  \author Hugo Pereira
  \version $Revision: 1.6 $
  \date $Date: 2009/01/20 15:38:38 $
*/

#include <MuonSubsysReco.h>
#include <TDataType.h>
#include <string>

#ifndef __CINT__
#include <boost/array.hpp>
#include <PHTimeServer.h>
#include <mFvtxSlowSim.h>
#include <mFvtxResponse.h>
#endif

// Forward declerations
class PHCompositeNode;

//! fvtx pisa to dst supermodule
/*! 
  fvtx pisa to dst supermodule
  reads pisa root files; fills forward vertex mc hit/track maps consequently;
  possibly runs the response, to build mutr/muid hit maps, writes to a simulated DST
  and possibly to a simulated PRDF. 
*/
class FvtxUnpackPisa: public MuonSubsysReco
{
  public:

  //! constructor
  FvtxUnpackPisa();

  //! destructor
  virtual ~FvtxUnpackPisa( void )
  {}
  
  //! run initialization
  int InitRun(PHCompositeNode *topNode);
  
  //! event method
  int process_event(PHCompositeNode *topNode);
  
  //! end method
  int End(PHCompositeNode *topNode);

  //! returns true if response is to be run and written to DST                                                                    
  bool do_response() const
  { return _run_response;}

  //! sets the do_response flag to tell if response is to be run and written to DST                                               
  void set_do_response( bool value )
  { _run_response = value ;}

  /// Sets the verbosity of this module (0 by default=quiet).
  virtual void Verbosity(const int ival) {verbosity = ival;}
   
  protected:

  //! create all needed nodes
  int create_node_tree(PHCompositeNode *topNode);

  //! evaluation tree
  void book_evaluation_tree( void );
  void book_evaluationRes_tree( void );
  
  //! evaluation tree
  void fill_evaluation_tree( PHCompositeNode* top_node );
  void fill_evaluationRes_tree( PHCompositeNode* top_node );
  
  //! if true, runs response on top of slowsim, write hitmaps to the DST                         
  bool _run_response;

  //! fvtx working node
  PHCompositeNode* _fvtx_node;
  
  //! mutoo working node
  PHCompositeNode* _mutoo_node;
  
  //! dst node
  PHCompositeNode* _dst_node;

  #ifndef __CINT__

  //!@name reconstruction modules
  //@{
    
  //! fvtx slow simulator module
  mFvtxSlowSim _mFvtxSlowSim_mod;

  //! fvtx response module
  mFvtxResponse _mFvtxResponse_mod;
  
  //@}
  
  //! module timer
  PHTimeServer::timer _timer;		

  #endif
  
};

#endif







