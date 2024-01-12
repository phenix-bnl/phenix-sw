// the base class SubsysReco provides the stl string variable "ThisName"
// and an int variable verbosity which can be set via
// Verbosity(const int ival);
// It can be used to e.g. configure printouts from the Analysis Module

#ifndef __EMCT0RECALRECO_H__
#define __EMCT0RECALRECO_H__

#include <SubsysReco.h>
#include <PdbEmcT0Sector.hh>
#include <PdbEmcT0Tower.hh>

#include <string>

class PHCompositeNode;

class EmcT0RecalReco: public SubsysReco
{
 public:
  EmcT0RecalReco(const std::string &name = "EMCT0RECAL");
  virtual ~EmcT0RecalReco() {}

  int InitRun(PHCompositeNode *topNode);  // Initializations which need the run number
  int process_event(PHCompositeNode *topNode); // your analysis code goes here


 protected:
  PdbEmcT0Sector TheSector;
  PdbEmcT0Tower TheTower;
  PdbEmcT0Sector TheSectorVd;
  int isvalid;
  int arm;
  int sector;
  int iy, iz;

  int nevt;
  int run;
  float get_correction( int arm, int sec, int ind_y, int ind_z, float ecent);

};

#endif /* __EMCT0RECALRECO_H__ */
