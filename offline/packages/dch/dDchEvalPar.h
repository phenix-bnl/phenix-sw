#ifndef __DDCHEVALPAR_H__
#define __DDCHEVALPAR_H__

typedef struct {
   short verbose;
   short coordinates;
   short main;
   short effiMainCut;
   float effiDphiCut;
   float effiDalphaCut;
   float effiDzedCut;
   short ghostMainCut;
   float ghostDphiCut;
   float ghostDalphaCut;
   float ghostDzedCut;
} DDCHEVALPAR_ST;
#endif /*__DDCHEVALPAR_H__*/
