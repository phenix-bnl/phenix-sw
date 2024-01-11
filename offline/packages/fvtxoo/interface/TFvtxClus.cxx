// $Id: TFvtxClus.cxx,v 1.2 2011/12/01 04:16:20 slash Exp $

/*!
   \file    TFvtxClus.cxx
   \brief   cluster interface object
   \author  H.Pereira
   \version $Revision: 1.2 $
   \date    $Date: 2011/12/01 04:16:20 $
*/

#include "TFvtxClus.h"
#include "TFvtxHitMap.h"
ClassImp(TFvtxClus)
 
//________________________________________
unsigned short  TFvtxClus::get_n_strip() const
{
	return get_associated<TFvtxHit>().count();
}
