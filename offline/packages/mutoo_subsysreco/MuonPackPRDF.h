#ifndef __MUONPACKPRDF_H__
#define __MUONPACKPRDF_H__

#include "SubsysReco.h"

/*!
  \file    MuonPackPRDF.h
  \ingroup supermodules 
  \brief   reads mutr/muid hit maps from mutoo/muioo nodes, packs them to a PRDF
  \author  Hugo Pereira
  \version $Revision: 1.4 $
  \date    $Date: 2010/10/10 13:47:28 $
*/

// Forward declerations
//

class PHCompositeNode;

// MUTOO/MUIOO
#ifndef __CINT__
#include <mMutPackPRDF.h>
#include <mMuiPackPRDF.h>
#include <mRpcPackPRDF.h>
#include <PHTimeServer.h>
#endif

/*!
  \class   MuonPackPRDF
  \ingroup supermodules 
  \brief   creates mutr/muid hits from PRDF data, possibly write them under DST node
*/
class MuonPackPRDF: public SubsysReco
{
 public:

  //! constructor
  MuonPackPRDF( const char* name = "MUONPACKPRDF" );
 
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
  PHCompositeNode * mutoo_node;   //!< mutoo node
  PHCompositeNode * muioo_node;   //!< muioo node
  
  // MUTOO module data members
  #ifndef __CINT__
  mMuiPackPRDF mMuiPackPRDF_mod;
  mMutPackPRDF mMutPackPRDF_mod;
  mRpcPackPRDF mRpcPackPRDF_mod;
  PHTimeServer::timer _timer;
  #endif
  
  // switches to be able to exclude MUID or MUTR parts
  bool _do_mui; //!< if true, muid packing is done. Controlled by recoConsts
  bool _do_mut; //!< if true, mutr packing is done. Controlled by recoConsts
  
};

#endif /* __MuonPackPRDF_H__ */







