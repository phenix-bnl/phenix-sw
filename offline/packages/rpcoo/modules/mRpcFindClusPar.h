// $Id: mRpcFindClusPar.h,v 1.3 2014/01/26 18:01:24 bbannier Exp $
#ifndef __mRpcFindClusPar_h__
#define __mRpcFindClusPar_h__

/*!
  \file    mRpcFindClusPar.h
  \brief   Runtime parameter object for mRpcFindClus analysis module
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
//! Runtime parameter object for mRpcFindClus analysis module
class mRpcFindClusPar : public TRpcParBase
{
  
 public: 

  /*! default constructor */
  mRpcFindClusPar() 
  {} 
  
  /*! destructor */
  ~mRpcFindClusPar() {}

  //! PHOOL inteface requirement
  void identify(std::ostream& os = std::cout) const 
  { os << "mRpcFindClusPar";}

  //! printing (insert values of all parameters here
  void print(std::ostream& out = std::cout) const {
    RPCOO::PRINT(out, "mRpcFindClusPar");
    RPCOO::PRINT(out, "**");
  }
};

#endif
