// $Id: EmcPISAPara.cc,v 1.2 2007/11/13 22:27:49 hpereira Exp $

/*!
  \file  EmcPISAPara.cc
  \brief container for EMC pisa parameters
  \author  T. K. Ghosh
  \version $Revision: 1.2 $
  \date    $Date: 2007/11/13 22:27:49 $
*/

#include "EmcPISAPara.h"

ClassImp(EmcPISAPara)

using namespace std;

vector<EmcPISAPara> EmcPISAPara::_hits;

//______________________________________________
EmcPISAPara::EmcPISAPara( void )
{
  for(int iRow=0; iRow<EMC_PAR_ROWS; iRow++) 
  { udetpar[iRow] = 0; }
}

//______________________________________________
EmcPISAPara::EmcPISAPara(const Float_t upar[])
{
  for(int iRow=0; iRow<EMC_PAR_ROWS; iRow++) 
  { udetpar[iRow] = upar[iRow]; }
}

//______________________________________________
void EmcPISAPara::GetEmcPar(Float_t upar[])
{
  for(int iRow=0; iRow<EMC_PAR_ROWS; iRow++) 
  { upar[iRow] = udetpar[iRow]; }
}

