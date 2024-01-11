// $Id: TMutClus.cxx,v 1.11 2011/12/29 20:19:29 slash Exp $

/*!
   \file    TMutClus.cxx
   \brief   Interface Object Class : TMutClus
   \author  H.Pereira
   \version $Revision: 1.11 $
   \date    $Date: 2011/12/29 20:19:29 $
*/

#include "TMutClus.hh"
#include "TMutHitMap.h"
ClassImp(TMutClus)

//
PHClassId::id_type TMutClus::_class_id( 0 );
 
//________________________________________
UShort_t  TMutClus::get_n_strip() const
{
	return get_associated<TMutHit>().count();
}
