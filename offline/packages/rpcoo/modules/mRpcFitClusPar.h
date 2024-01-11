// $Id: mRpcFitClusPar.h,v 1.3 2014/01/26 18:01:24 bbannier Exp $
#ifndef __mRpcFitClusPar_h__
#define __mRpcFitClusPar_h__

/*!
  \file    mRpcFitClusPar.h
  \brief   Runtime parameter object for mRpcFitClus analysis module
  \author  H. Pereira Da Costa
  \version $Revision: 1.3 $
  \date    $Date: 2014/01/26 18:01:24 $
*/

#include <PHObject.h>
#include <PHException.h>
#include <iostream>

#include "TRpcParBase.h"
#include "RPCOO.h"

/*! @ingroup modules */
//! Runtime parameter object for mRpcFitClus analysis module
class mRpcFitClusPar : public TRpcParBase
{
  
 public: 

  /*! default constructor */
  mRpcFitClusPar() 
  {} 
  
  /*! destructor */
  ~mRpcFitClusPar() {}

  //! PHOOL inteface requirement
  void identify(std::ostream& os = std::cout) const { os << "mRpcFitClusPar"; }

  //! printing (insert values of all parameters here
  void print(std::ostream& out = std::cout) const {
    RPCOO::PRINT(out, "mRpcFitClusPar");
    RPCOO::PRINT(out, "**");
  }
};

#endif
