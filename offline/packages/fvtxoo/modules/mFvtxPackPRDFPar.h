// $Id: mFvtxPackPRDFPar.h,v 1.2 2013/03/18 00:07:12 jinhuang Exp $

/*!
   \file mFvtxPackPRDFPar.h 
   \brief Parameters for Fvtx Pack PRDF
   \author Zhengyun You
   \version $Revision: 1.2 $
   \date $Date: 2013/03/18 00:07:12 $
*/

#ifndef __mFvtxPackPRDFPAR_HH__
#define __mFvtxPackPRDFPAR_HH__

#include<PHObject.h>
#include<TFvtxParBase.h>

//!Runtime parameter object for mFvtxUnpack analysis module
/*! 
*/
class mFvtxPackPRDFPar : public TFvtxParBase
{

 public: 

  /*! default constructor */
  mFvtxPackPRDFPar() 
  {;}
  
  /*! destructor */
  ~mFvtxPackPRDFPar(){;}

 private:  

  ClassDef(mFvtxPackPRDFPar, 1);
};

#endif /* __mFvtxPackPRDF_HH__ */







