// $Id: rcp.C,v 1.1 2009/07/04 18:32:23 hpereira Exp $

#include <iostream>
#include <fstream>
#include <string>
#include "rcp.h"

//////////////////////////////////////////////////////////////////
/*!
   \file    rcp.C
   \brief   nanodst runtime parameter manager
   \version $Revision: 1.1 $
   \date    $Date: 2009/07/04 18:32:23 $
*/
//////////////////////////////////////////////////////////////////

using namespace std;

//________________________________________________________
bool RCP::file_ok(const char* filename)
{
    bool fileok = true;
    ifstream from(filename);
    if (!from) fileok=false;
    from.close();
    return fileok;
}
