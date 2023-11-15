//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbErtLutEmcTile
//
//  Author: A. D. Frawley
//-----------------------------------------------------------------------------
#include "PdbErtLutEmcTile.hh"

#include <iostream>

PdbErtLutEmcTile::PdbErtLutEmcTile()
{
  NumberAssociated=0;
  for(int i=0;i<20;i++)
    {
      AssociatedRichTile[i] = -1;
      AssociatedMaxMom[i] = -1.0;
    }
}

void
PdbErtLutEmcTile::print() const
{
  std::cout << std::endl;
  std::cout << "Number of associated Rich tiles : " << NumberAssociated << std::endl;
  for(int i=0;i<NumberAssociated;i++)
    {
      std::cout << "Associated Rich Tile number : " << AssociatedRichTile[i] 
		<< " Maximum electron momentum for this association : " << AssociatedMaxMom[i]
		<< std::endl;
    }
}
