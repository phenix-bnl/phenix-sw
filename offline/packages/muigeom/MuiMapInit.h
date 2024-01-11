// $Id: MuiMapInit.h,v 1.1 2007/04/05 11:08:27 hpereira Exp $

#ifndef MuiMapInit_h
#define MuiMapInit_h

/*!
  \file MuiMapInit.h
  \brief muid L1 and L2 map initialisations
  \author Jason Newby
  \version $Revision: 1.1 $
  \date $Date: 2007/04/05 11:08:27 $
*/

#include "hash_vector.hh"

//! map channel id to logical number
void MuiMapInit(hashVector <TMuiChannelId, int> &map);

#endif
