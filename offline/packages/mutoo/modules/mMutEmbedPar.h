// $Id: mMutEmbedPar.h,v 1.4 2007/01/30 11:13:58 hpereira Exp $
#ifndef __mMutEmbedPar_h__
#define __mMutEmbedPar_h__

/*!
  \file    mMutEmbedPar.h
  \brief   Runtime parameter object for mMutEmbed analysis module
  \author  S. Kelly
  \version $Revision: 1.4 $
  \date    $Date: 2007/01/30 11:13:58 $
*/

#include<PHObject.h>
#include<TMutParBase.h>

//! Runtime parameter object for mMutEmbed analysis module
class mMutEmbedPar : public TMutParBase
{
  
 public: 

  //! default constructor
  mMutEmbedPar(){;}
  
  //! destructor
  virtual ~mMutEmbedPar(){;}
  
  //! PHOOL inteface requirement
  void identify( std::ostream& os = std::cout) const 
  { os << "mMutEmbedPar";}

 private:  

};

#endif
