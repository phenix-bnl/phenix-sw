// $Id: TRpcClus.cxx,v 1.2 2008/08/28 00:50:17 kempel Exp $

/*!
   \file    TRpcClus.cxx
   \brief   Interface Object Class : TRpcClus
   \author  H.Pereira
   \version $Revision: 1.2 $
   \date    $Date: 2008/08/28 00:50:17 $
*/

#include "TRpcClus.h"
#include "TRpcHitMap.h"
ClassImp(TRpcClus)

//____________________________________________
UShort_t TRpcClus::get_n_hits( void ) const
{ return get_associated<TRpcHit>().count(); }
