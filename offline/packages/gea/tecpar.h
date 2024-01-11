#ifndef __TECPAR_H__
#define __TECPAR_H__


typedef struct {
   float r0;
   float angle;
   float thRad;
   float thXe;
   short nArms;
   short nSect;
   short iArmor;
   short NTEC;
   short lTec[6];
   int iDateTEC;
} TECPAR_ST;
#endif /*__TECPAR_H__*/
