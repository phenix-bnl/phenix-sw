// $Id: mMuiEmbedPar.h,v 1.1 2007/01/30 11:15:03 hpereira Exp $
#ifndef __mMuiEmbedPar_h__
#define __mMuiEmbedPar_h__

/*!
  \file    mMuiEmbedPar.h
  \brief   Runtime parameter object for mMuiEmbed analysis module
  \author  S. Kelly
  \version $Revision: 1.1 $
  \date    $Date: 2007/01/30 11:15:03 $
*/

#include<PHObject.h>
#include<TMuiParBase.h>

//! Runtime parameter object for mMuiEmbed analysis module
class mMuiEmbedPar : public TMuiParBase
{
  
 public: 

  //! default constructor
  mMuiEmbedPar(){;}
  
  //! destructor
  virtual ~mMuiEmbedPar(){;}
  
  //! PHOOL inteface requirement
  void identify( std::ostream& os = std::cout) const 
  { os << "mMuiEmbedPar";}

 private:  

};

#endif
