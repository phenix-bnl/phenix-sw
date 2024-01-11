// $Id: mFvtxEmbedPar.h,v 1.2 2013/03/18 00:07:10 jinhuang Exp $
#ifndef __mFvtxEmbedPar_h__
#define __mFvtxEmbedPar_h__

/*!
  \file    mFvtxEmbedPar.h
  \brief   Runtime parameter object for mFvtxEmbed analysis module
  \author  H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2013/03/18 00:07:10 $
*/

#include<PHObject.h>
#include<TFvtxParBase.h>

/*! \ingroup modules */
//! Runtime parameter object for mFvtxEmbed analysis module
class mFvtxEmbedPar : public TFvtxParBase
{
  
 public: 

  //! default constructor
  mFvtxEmbedPar(){;}
  
  //! destructor
  virtual ~mFvtxEmbedPar(){;}
  
  //! PHOOL inteface requirement
  void identify( std::ostream& os = std::cout) const 
  { os << "mFvtxEmbedPar";}

 private:  

	ClassDef(mFvtxEmbedPar, 1);
};

#endif 







