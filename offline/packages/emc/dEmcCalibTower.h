#ifndef __DEMCCALIBTOWER_H__
#define __DEMCCALIBTOWER_H__


typedef struct {
  short id;
  int hwkey;
  int swkey;
  short type;
  short arm;
  short sector;
  short ind[2];
  float ecal;
  float tof;
  int deadmap;
  int warnmap; // MV 2001/12/04
  float adc; // MV 2001/09/26
  float tac; // MV 2001/09/26
} DEMCCALIBTOWER_ST;

typedef DEMCCALIBTOWER_ST dEmcCalibTower;
#endif /*__DEMCCALIBTOWER_H__*/
