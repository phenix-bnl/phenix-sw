// $Id: RxnpReco.h,v 1.6 2007/11/30 20:16:35 hpereira Exp $

#ifndef __RXNPRECO_H__
#define __RXNPRECO_H__

/*!
  \file    RxnpReco.h
  \ingroup supermodules 
  \brief   Rxnp reconstruction event loop
  \author  Chun Zhang
  \version $Revision: 1.6 $
  \date    $Date: 2007/11/30 20:16:35 $
*/

#include <string>
#include "MuonSubsysReco.h"

// Forward declerations
//
//ROOT
//

// PHENIX
class PHCompositeNode;

#ifndef __CINT__
#include <PHTimeServer.h>
#include <mRxnpUnpackPRDF.h>
#include <mRxnpCalXang.h>
#endif

/*!
  \class   RxnpReco
  \ingroup supermodules 
  \brief   Rxnp reconstruction event loop
*/
class RxnpReco: public MuonSubsysReco
{
 public:

  //! constructor
  RxnpReco( const char* name = "RxnpReco" );

  //! destructor
  virtual ~RxnpReco()
  {}
   
  //! full initialization
  int InitRun(PHCompositeNode *);
  
  //! event processing method
  int process_event(PHCompositeNode *);
  
  //! end of run method
  int End(PHCompositeNode *);

  //
  void set_iter(int iter) { _iter = iter;}

  // 
  void set_use_db(int yes) { _use_db = yes;}

 protected:

  //! create all mutoo nodes
  int CreateNodeTree(PHCompositeNode *);

  //! Fill the energy weighted phi distributions
  //
  //void fill_phi_dists(PHCompositeNode *);
  //! calculat reaction plane angles
  //
  //  void cal_Rx_ang(PHCompositeNode *);
  // Node tree data members
  //! Rxnp working node
  PHCompositeNode *rxnp_node;
  
  //! dst io node
  PHCompositeNode *dst_node;
  
  #ifndef __CINT__
  
  //! unpack and calib module
  mRxnpUnpackPRDF _mRxnpUnpackPRDF_mod;
  
  //! module for raw reaction plane angle calculation
  mRxnpCalXang _mRxnpCalXang_mod;

  //! supermodule timer
  PHTimeServer::timer _timer;
  
  #endif
  
  // time of iteration in flattening
  int _iter;
  // use db flag
  int _use_db;

};

#endif /* __RXNPRECO_H__ */







