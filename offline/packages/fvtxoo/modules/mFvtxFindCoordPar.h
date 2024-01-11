#ifndef __MFVTXFINDCOORdPAR_HH__
#define __MFVTXFINDCOORDPAR_HH__
// $Id: mFvtxFindCoordPar.h,v 1.6 2014/01/26 16:47:16 bbannier Exp $  

/*!
  \file mFvtxFindCoordPar.h
  \brief parameters for mFvtxFindCoord module
  \author M. Brooks
  \version $Revision: 1.6 $
  \date $Date: 2014/01/26 16:47:16 $
*/

#include<PHObject.h>
#include<FVTXOO.h>
#include<TFvtxParBase.h>

//! parameters for mFvtxFindCoord module
class mFvtxFindCoordPar : public TFvtxParBase
{
  
 public:
  
  // default constructor
  
  mFvtxFindCoordPar() :
    _use_charge_weighted_fit(true)
    {;}


  //destructor
  ~mFvtxFindCoordPar() 
  {}

  // Use charge weighting when calculating the cluster centroid or not:
  bool get_use_charge_weighted_fit() const
  { return _use_charge_weighted_fit; }

  void set_use_charge_weighted_fit(bool value)
  { _use_charge_weighted_fit = value; }

  // print method
  void print(std::ostream &out = std::cout) const {
    FVTXOO::PRINT(out, "mFvtxFindCoordPar");
    out << "_verbosity: " << get_verbosity() << std::endl;
    FVTXOO::PRINT(out, "***");
  }

 private:

  bool _use_charge_weighted_fit;

  ClassDef(mFvtxFindCoordPar,1);
};

#endif /* __MFVTXFIND/COORDAR_HH__ */
