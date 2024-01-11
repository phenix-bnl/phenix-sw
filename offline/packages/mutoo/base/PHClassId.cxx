
// $Id: PHClassId.cxx,v 1.1 2007/05/28 14:32:08 hpereira Exp $

/*!
   \file PHClassId.hh
   \brief  map class names into unique ID
   \author H.Pereira
   \version $Revision: 1.1 $
   \date $Date: 2007/05/28 14:32:08 $
*/

#include "PHClassId.hh"

// static variables
PHClassId::IdMap PHClassId::_map;
PHClassId::id_type PHClassId::_id = 0;
