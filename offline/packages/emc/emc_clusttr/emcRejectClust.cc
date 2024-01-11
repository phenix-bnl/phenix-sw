//************************************************************
// EMC Reading Rejects.list
//************************************************************
#include <cstdlib>
#include <stdio.h>
#include <unistd.h>
#include <cstring>
#include <iostream.h>
#include <fstream.h>
#include "emcRejectList.hh"
#include "Clust.hh"
#include "emcRejectClust.hh"

int emcRejectClust(Clust& clt){
  if( clt.arm != 0 && clt.arm != 1 )
    return 0;
  int isect = 0;
  if( clt.arm == 1 ){
    if( clt.sector ==0 || clt.sector == 1 )
      isect = clt.sector + 6;
    else
      isect = clt.sector + 2;
  } else {
    isect = clt.sector;
  }
  int iz = clt.ind[0];
  int iy = clt.ind[1];
  //
  int status = 2056;
  if( iz < 0 || iz > 96 || iy < 0 || iy > 48 ||
      isect < 0 || isect > 8 ){
    cout<<" emcRejectClust:: return error code. iz,iy,isect are out of range."<<endl;
  } else {
    status = frejtwr[isect][iz][iy];
  }
  return status;
}

