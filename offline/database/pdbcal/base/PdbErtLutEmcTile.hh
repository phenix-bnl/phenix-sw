//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Declaration of class PdbErtLutEmcTile
//
//  Purpose: User defined storage class
//
//  Description:
//
//  Author: A. D. Frawley
//-----------------------------------------------------------------------------
#ifndef __PDBERTLUTEMCTILE_HH__
#define __PDBERTLUTEMCTILE_HH__

#include "PdbCalChan.hh"

class PdbErtLutEmcTile : public PdbCalChan {
 public:
  PdbErtLutEmcTile();
  virtual ~PdbErtLutEmcTile(){}
  
  void setAssociatedRichTile(const int number, const int richtile) 
  { 
    if(number<20)
      AssociatedRichTile[number]=richtile;
  }
  
  void setAssociatedMaxMom(const int number, const float maxmom) 
  {
    if(number<20) 
      AssociatedMaxMom[number]=maxmom;
  }
  
  
  void setNumberAssociated(const int nentries) 
  {
    // Note: constructor sets NumberAssociated to zero at initialization
    if(nentries<=20)
      NumberAssociated=nentries;
    else
      NumberAssociated=20;
  }
  
  int getAssociatedRichTile(const int number) 
  {
    if(number<20)
      return AssociatedRichTile[number];
    else
      return -1;
  }
  
  float getAssociatedMaxMom(const int number) 
  {
    if(number<20)
      return AssociatedMaxMom[number];
    else
      return -1.0;
  }
  
  int getNumberAssociated() 
  {
    return NumberAssociated;
  }
  
  virtual void print() const;
  
private:

  int AssociatedRichTile[20];
  float AssociatedMaxMom[20];
  int NumberAssociated;

  ClassDef(PdbErtLutEmcTile,1);

};

#endif /* __PDBERTLUTEMCTILE_HH__ */
