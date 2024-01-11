#ifndef __MNEWDCHEFFCALIBRATOR_H__
#define __MNEWDCHEFFCALIBRATOR_H__

#include "phool.h" // for PHBoolean
#include <cstdio>

class DchTrack;
class dDchHitWrapper;
class PHCompositeNode;
class TNtuple;

class mNewDchEfficiencyCalibrator
{
public:
  mNewDchEfficiencyCalibrator();
  virtual ~mNewDchEfficiencyCalibrator(){}
  PHBoolean event(PHCompositeNode *);

  int calculate();
  int saveToFile();
private:

  TNtuple *Efficiency;

int run,m;
float tot_eff;
double par[4];
int dist_cut;
char   buff[256];
int    firings[2][2][2][80]; //arm,side,plane x1/x2,cell
int    tr_id[2][2][2][80];
short  s_sign[2][2][2][80];
short  last_wire[2][2][2][80];
short  first_wire[2][2][2][80];
int    eff[2][2][40][80];
int    tot[2][2][40][80];
float  dist_h[2][2][40][80];
int    c_hit[40];
int    dcarm,dcside,dccell;
int    pl1;
int    x_flag,first_time;
int planeOfHit;
float array[30];
  int total,index;
  int EventRunNumber;
FILE  *a;

  PHCompositeNode *topNode;
  DchTrack *trackTable;
  dDchHitWrapper *hitLineTable;
};
#endif /*__MNEWDCHEFFCALIBRATOR_H__*/



