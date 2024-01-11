#ifndef __FVTXPACKPRDF_H__
#define __FVTXPACKPRDF_H__

#include "SubsysReco.h"

/*!
  \file    FvtxPackPRDF.h
  \ingroup supermodules 
  \brief   reads fvtx muid hit maps from fvtxoo nodes, packs them to a PRDF
  \author  Zhengyun You
  \version $Revision: 1.1 $
  \date    $Date: 2011/04/12 18:24:20 $
*/

// Forward declerations
//

class PHCompositeNode;

#ifndef __CINT__
#include <mFvtxPackPRDF.h>
#include <PHTimeServer.h>
#endif

/*!
  \class   FvtxPackPRDF
  \ingroup supermodules 
  \brief   creates mutr/muid hits from PRDF data, possibly write them under DST node
*/
class FvtxPackPRDF: public SubsysReco
{
 public:

  //! constructor
  FvtxPackPRDF( const char* name = "FVTXPACKPRDF" );
 
  //! run initialization
  int InitRun(PHCompositeNode *topNode);
  
  //! event method
  int process_event(PHCompositeNode *topNode);
      
  //! finish processing
  int End(PHCompositeNode *topNode);

 protected:  

  //! creates checks all needed nodes
  int CreateNodeTree(PHCompositeNode *topNode);
  
  // Node tree data members
  //
  PHCompositeNode * fvtxoo_node;   //!< fvtxoo node
  
  // FVTXOO module data members
  #ifndef __CINT__
  mFvtxPackPRDF mFvtxPackPRDF_mod;
  PHTimeServer::timer _timer;
  #endif
  
  // switches to be able to exclude FVTX parts
  bool _do_fvtx; //!< if true, fvtx packing is done. Controlled by recoConsts
  
};

#endif /* __FvtxPackPRDF_H__ */







