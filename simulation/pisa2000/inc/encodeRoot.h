#ifndef _ENCODEROOT_
#define _ENCODEROOT_

#include "PISAEvent.h"

void encodeRootEvntKin(int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntPri(int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntBbc(int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntZdc(int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntMvd(int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntDch(int arm, int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntCrk(int *iqf, int kentries, int i[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntCtr(int *iqf, int kentries, int i[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntPad(int sector, int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntTec(int plane, int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntTof(int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntEmc(int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntMut(int kentries, int i[], float f[], int icode, int tracks[],
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntMui(int kentries, int i[], float f[], int icode, int tracks[],
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntSvx(int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntNtc(int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntRxn(int icode, int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntHbd(int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntTpc(int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntFcl(int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntAer(int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntMuPC(int arm, int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntRLT(int irpc, int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntNCC(int arm, int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntMPCEXABS(int arm, int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntMPCFPLT(int arm, int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntMPCEXEntry(int arm, int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntMpc(int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

void encodeRootEvntTfw(int icode, int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent);

#endif





