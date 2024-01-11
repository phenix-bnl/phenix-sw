// $Id: mRxnpUnpackPRDF.h,v 1.6 2007/11/30 19:49:57 hpereira Exp $
#ifndef __mRxnpUnpackPRDF_h__
#define __mRxnpUnpackPRDF_h__

/*!
  \file    mRxnpUnpackPRDF.h
  \brief   unpack raw data hits. Generates interface class TRxnpRawScint_v1.
  \author  Chun Zhang
  \version $Revision: 1.6 $
  \date    $Date: 2007/11/30 19:49:57 $
*/

#include <phool.h>
#include <PHTimeServer.h>
#include "mRxnpUnpackPRDFPar.h"

class PHCompositeNode;
class Event;
class TRxnpRawScintMap;
class TRxnpScintMap;

//@{
/*! \ingroup modules */
//! rxnp raw prdf data unpacker.
/*! 
    rxnp raw data unpacker. Generates interface class TRxnpRawScint_v1 for each scintilator.
*/

class mRxnpUnpackPRDF
{
 public:
  //! constructor
  //
  mRxnpUnpackPRDF();
  //! destructor
  //
  virtual ~mRxnpUnpackPRDF() {;}
  //! event method
  //
  virtual PHBoolean event(PHCompositeNode*);
    
 private:
  //! get local pointers to needed nodes/maps
  //
  void set_interface_ptrs(PHCompositeNode* top_node);
  //! unpack module
  //
  void unpack_prdf();
  //! calibrate the scintilator
  //
  void calibrate(int ibbc=0);

  /* private data member */

  //! module timer
  //
  PHTimeServer::timer _timer;

  // Event pointer
  //
  Event* _evt;
  //! rawscint map
  //
  TRxnpRawScintMap* _raw_map;
  //! calibrated scint map
  //
  TRxnpScintMap* _scint_map;
  //! runtime parameter
  //
  mRxnpUnpackPRDFPar* _mod_par;

};

//@}

#endif
