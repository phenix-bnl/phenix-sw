#include "MutrgPar.hh"
#include "TMutGeo.h"

bool MutrgPar::IS_READ_NSTRIP_DB=true;

///////////////////////////////////////////////////////

int MutrgPar::NSTRIP_IN_OCTANT(int arm,int station,int octant){
  int n=0;
  for(int ihoct=0; ihoct<NHALFOCTANT; ihoct++){
    n+=NSTRIP_IN_HALFOCTANT(arm,station,octant,ihoct);
  }

  return n;
}

////////////////////////////////////////////////////////

int MutrgPar::NSTRIP_IN_OCTANT(int arm,int station,int octant,
			       int gap,int cathode){
  int n=0;
  for(int ihoct=0; ihoct<NHALFOCTANT; ihoct++){
    n+=NSTRIP_IN_HALFOCTANT(arm,station,octant,ihoct,gap,cathode);
  }

  return n;
}

////////////////////////////////////////////////////////

int MutrgPar::NSTRIP_IN_HALFOCTANT(int arm,int station,
				   int octant,int halfoctant){
  int gap=0;
  int cathode=MutrgPar::INSTALL_CATHODE[station];
  return NSTRIP_IN_HALFOCTANT(arm,station,octant,halfoctant,gap,cathode);
}

///////////////////////////////////////////////////////

int MutrgPar::NSTRIP_IN_HALFOCTANT(int arm,int station,int octant,
				   int halfoctant,int gap,int cathode){
  if(MutrgPar::IS_READ_NSTRIP_DB){
    return NSTRIP_IN_HALFOCTANT_DB(arm,station,octant,halfoctant,gap,cathode);
  }
  else{
    return NSTRIP_IN_HALFOCTANT_HC(arm,station,octant,halfoctant,gap,cathode);
  }
}

//////////////////////////////////////////////////////////

int MutrgPar::NSTRIP_IN_HALFOCTANT_DB(int arm,int station,int octant,
				      int halfoctant,int gap,int cathode){
  static bool flag_fill=false;
  static int nstrip[NARM][NSTATION][NOCTANT][NHALFOCTANT][MAX_NGAP][NCATHODE];

  if(!flag_fill){
    printf("MutrgPar : Getting # of strips from database.\n");
    for(int iarm=0; iarm<NARM; iarm++){
      for(int ist=0; ist<NSTATION; ist++){
	for(int ioct=0; ioct<NOCTANT; ioct++){
	  for(int ihoct=0; ihoct<NHALFOCTANT; ihoct++){
	    for(int igap=0; igap<NGAP[ist]; igap++){
	      for(int icath=0; icath<NCATHODE; icath++){
		int ipl=(icath==0 ? 0 : 2);
		nstrip[iarm][ist][ioct][ihoct][igap][icath]=
		  TMutGeo::get_n_strip(iarm,ist,ioct,ihoct,igap,ipl);
	      }
	    }
	  }
	}
      }
    }

    flag_fill=true;
  }

  return nstrip[arm][station][octant][halfoctant][gap][cathode];
}

///////////////////////////////////////////////////////

int MutrgPar::NSTRIP_IN_HALFOCTANT_HC(int arm,int station,int octant,
				      int halfoctant,int gap,int cathode){
  const int NSTRIP[NARM][NSTATION]={{96,160,240},{96,192,320}};

  if(arm==0){
    if(station==0){
      int nstp_hoct0=-9999;
      if(octant%2==0 && cathode%2==0){nstp_hoct0=50;} // Oct 0,2,4,6 Cath 0
      if(octant%2==0 && cathode%2==1){nstp_hoct0=51;} // Oct 0,2,4,6 Cath 1
      if(octant%2==1 && cathode%2==0){nstp_hoct0=45;} // Oct 1,3,5,7 Cath 0
      if(octant%2==1 && cathode%2==1){nstp_hoct0=46;} // Oct 1,3,5,7 Cath 1

      if(nstp_hoct0<0){return -9999;}
      else if(halfoctant==0){return nstp_hoct0;}
      else {return NSTRIP[arm][station]-nstp_hoct0;}
    }
    else{return NSTRIP[arm][station]/2;}
  }
  else if(arm==1){
    if(station==0){
      int nstp_hoct0=-9999;
      if(octant%2==0 && cathode%2==0){nstp_hoct0=46;} // Oct 0,2,4,6 Cath 0
      if(octant%2==0 && cathode%2==1){nstp_hoct0=45;} // Oct 0,2,4,6 Cath 1
      if(octant%2==1 && cathode%2==0){nstp_hoct0=51;} // Oct 1,3,5,7 Cath 0
      if(octant%2==1 && cathode%2==1){nstp_hoct0=50;} // Oct 1,3,5,7 Cath 1

      if(nstp_hoct0<0){return -9999;}
      else if(halfoctant==0){return nstp_hoct0;}
      else {return NSTRIP[arm][station]-nstp_hoct0;}
    }
    else{return NSTRIP[arm][station]/2;}
  }

  return -1;
}

//////////////////////////////////////////////////////////
