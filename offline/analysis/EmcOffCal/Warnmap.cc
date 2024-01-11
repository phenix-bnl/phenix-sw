#include "Warnmap.h"


#include <cstdlib>
#include <iostream>
#include <fstream>

using namespace std;
using namespace EmcAnaCommon;

Warnmap::Warnmap()
{
   ResetMap();
}

void Warnmap::ResetMap()
{
   memset(m_warnmap, 0, sizeof(m_warnmap));
}

void Warnmap::ReadMap(const Char_t* fname)
{
     ifstream if_status(fname);
     if (if_status.is_open()) {
        cout << "Warnmap::ReadMap():" << endl
             << "  Open '" << fname << "'." << endl;
     } else {
        cout << "Warnmap::ReadMap():" << endl
             << "  File '" << fname << "' does not exist.  Abort." << endl;
        exit(0);
     }

     Int_t armsect, ypos, zpos, status;
     while (if_status >> armsect >> ypos >> zpos >> status) {
	  if (IsValidYZ(armsect, ypos, zpos)) {
	       m_warnmap[armsect][ypos][zpos] = status;
	  }
     }
     if_status.close();
     return;
}

Double_t Warnmap::GetGoodFraction(Int_t as, Int_t bl_include_edge)
{
   Int_t ntwr_tot = 0;
   Int_t ntwr_good = 0;
   for (Int_t y = 0; y < N_YPOS_PBGL; y++) {
   for (Int_t z = 0; z < N_ZPOS_PBGL; z++) {
   if (IsValidYZ(as, y, z)) {
      if (! bl_include_edge && IsEdgePos(as, y, z)) continue;
      ntwr_tot++;
      if (! IsBad(as, y, z)) ntwr_good++;
   }
   }
   }
   return (Double_t)ntwr_good/ntwr_tot;
}
