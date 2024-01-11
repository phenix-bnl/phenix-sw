// $Id: mRpcEmbedPar.h,v 1.2 2008/08/28 00:53:56 kempel Exp $
#ifndef __mRpcEmbedPar_h__
#define __mRpcEmbedPar_h__

/*!
  \file    mRpcEmbedPar.h
  \brief   Runtime parameter object for mRpcEmbed analysis module
  \author  H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2008/08/28 00:53:56 $
*/

#include<PHObject.h>
#include<TRpcParBase.h>

/*! \ingroup modules */
//! Runtime parameter object for mRpcEmbed analysis module
class mRpcEmbedPar : public TRpcParBase
{
  
 public: 

  //! default constructor
  mRpcEmbedPar(){;}
  
  //! destructor
  virtual ~mRpcEmbedPar(){;}
  
  //! PHOOL inteface requirement
  void identify( std::ostream& os = std::cout) const 
  { os << "mRpcEmbedPar";}

 private:  

};

#endif 







