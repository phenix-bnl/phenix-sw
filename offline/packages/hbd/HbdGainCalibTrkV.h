#ifndef __HBDGAINCALIBTRKV_H__
#define __HBDGAINCALIBTRKV_H__

#include <SubsysReco.h>
#include <string>
#include <iostream>

class Event;
class PHCompositeNode;
class PHTimeStamp;
class PHCentralTrack;
class HbdBlobList;
class TH1F;
class TF1;

const int Narms   =2;
const int Nsides  =2;
const int Nsect   =6;

class HbdGainCalibTrkV: public SubsysReco
{

 public:

  HbdGainCalibTrkV(const std::string &Name = "HbdGainCalibTrkV");
  virtual ~HbdGainCalibTrkV();
  
  //
  //  Everybody needs a few of these...
  //
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  void identify(std::ostream& = std::cout) const;
//  int process_event(Event *evt, PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);  // This one analyzes and commits (if flag says to)
  
  //
  //  Database interface routines
  //
  void CommitToPdbCal(const int value) {committopdbcal=value;}   //Set committing flag...
  int  CommitedToPdbCalOK() const   {return commitsuccess;}   //Inform server of commit status...
  int  VerificationOK() const {return verificationstatus;}   //Inform server of verify status...;
  
  
private:
  int Verify();
  int Calibrate();
  int CommitConstantsToDatabase();
  int committopdbcal;
  int commitsuccess;
  int verificationstatus;
  PHTimeStamp *BeginTime; 
  PHTimeStamp *EndTime; 
  
  int evtcounter;
  HbdBlobList *BlobList;
  TH1F *h_mod_gain[2][2][6];
  TF1  *fexpo;
  
  int alldone;
  float charge, blobx, bloby, blobz;
  float hbd_dphi, hbd_dz;
  float hbd_px, hbd_py, hbd_pz;
  int sector, arm, side, size, Number_Of_Tracks, AssociationFlag;
  double Gain_mod[24];
  double Gain_mod_err[24];
 };

#endif /*__HBDGAINCALIBTRKV_H__ */
