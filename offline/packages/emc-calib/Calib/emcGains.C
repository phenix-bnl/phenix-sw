// $Id: emcGains.C,v 1.4 2002/01/08 16:48:09 aphecetc Exp $

#include "emcTracedFEM.h"
#include "emcGains.h"
#include "emcTracedValue.h"
#include <iostream>
#include <cassert>
#include <ctime>
#include <cstdio>
#include <cstring>

//_____________________________________________________________________________
void emcGains::writeDataToFile(FILE* fp, const PHTimeStamp& tStart)
{
  time_t tics = tStart.getTics(); 
  char timeString[25]; 
  timeString[24] = '\0'; 
  strncpy(timeString, ctime(&tics), 24);   
  fprintf(fp,"%s\n",timeString) ;

  emcTracedFEM* gainFEM ;

  int i ;

  for ( i = 0 ; i < GetNumberOfFEMs() ; i++ ) {
    gainFEM = dynamic_cast<emcTracedFEM*>(GetFEM(i)) ;
    assert(gainFEM!=0);
    gainFEM->writeDataToFile(fp) ;
    fprintf(fp,"\n") ;
  }
}
