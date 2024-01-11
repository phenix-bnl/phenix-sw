//-------------------------------------------------------------------------
// Author: Federica Ceretto/Messer 
//
// Date : 06/27/00
//
// Purpose: Variable definitions
// Here are a number of the variables we use in the drift chamber
// code. Those variables (like the number of planes and number of Arms) 
// should not need any tuning therefore the "hard-wiring" made here.
//
//-------------------------------------------------------------------------
#ifndef __DCHDGOPAR_H__
#define __DCHDGOPAR_H__

#include "phool.h"

#define west 0  //  Ummm---These are wrong.
#define east 1  //  Is this on purpose?  TKH 11-25-2001
#define north 0
#define south 1


#define numberOfArms  2
#define numberOfWireTypes 2
#define numberOfSections 2
#define numberOfSides  2
#define numberOfCells  80
#define numberOfPlanes 40
#define numberOfKeyStones 20
#define numberOfPairs 2
#define numberOfChannels 80
#define numberOfNibbles 48
#define numberOfWordsForChannel 12
#define halfNumberOfPlanes 20
#define numberOfXPlanes 12
#define numberOfUorVPlanes 4
#define numberOfModules 6
#define referenceRadius  220.0

const short X1Wire = 0;
const short UV1Wire = 1;
const short X2Wire = 2;
const short UV2Wire = 3;
const short wireType[40] =
{X1Wire,X1Wire,X1Wire,X1Wire,X1Wire,X1Wire,
 X1Wire,X1Wire,X1Wire,X1Wire,X1Wire,X1Wire,
 UV1Wire,UV1Wire,UV1Wire,UV1Wire,
 UV1Wire,UV1Wire,UV1Wire,UV1Wire,
 X2Wire,X2Wire,X2Wire,X2Wire,X2Wire,X2Wire,
 X2Wire,X2Wire,X2Wire,X2Wire,X2Wire,X2Wire,
 UV2Wire,UV2Wire,UV2Wire,UV2Wire,
 UV2Wire,UV2Wire,UV2Wire,UV2Wire};



#endif /* DchDgoPar.h */


