#ifndef __HBDDCMRAW_HH_
#define __HBDDCMRAW_HH_

/*
#define NFEM            1
#define NMOD_PER_FEM    2
#define NALLCHAN       NFEM*NMOD_PER_FEM*NCHAN_PER_MOD
*/

#define NCHAN_PER_MOD   48
#define MAXSAMPLE       12

class PHTimeStamp;
class Event;
class HbdCellList;
class HbdMiniCellList;
class Packet;
class hbdDetectorGeo;

class HbdDcmRaw 
{
 public:
  HbdDcmRaw();
  virtual ~HbdDcmRaw(){}
  int Init();

  hbdDetectorGeo *hbdgeo;
  // Set the values in the HbdDcmRaw ...
  int GetEvent(Event *evt, HbdCellList *celllist);
  int GetEventToMiniCell(Event *evt, HbdMiniCellList *minicelllist);

  void TakeNthSampleIntoCell(int nsample=8){ Nthsample=nsample;}


  void SetNFEM(int nfem){ NFEM=nfem; NALLCHAN=NFEM*NMOD*NCHAN_PER_MOD;}

  void SetNModPerFEM(int nmod){ NMOD=nmod; NALLCHAN=NFEM*NMOD*NCHAN_PER_MOD;}

 // If Physics events, don't take channels that have proper samples
  void SetPhysicsDecode(int flag){ bPhysics = flag;}

  void SetChargeThreshold(float threshold){ ChargeThreshold=threshold;}


  //
  // PadId starts from "0"
  void ChanToPadId(int fem_seq, int modul, int chan, int& PadId, int runno=203976);

  void PadIdToPos(int PadId, int& sec, int& col, int& row);


  int GetADC(int padid, int samp, int& clk)
      { clk = 0; return AdcVal[padid][samp]; }

  int GetSumADC(int padid, int samp, int& clk)
      { clk = 0; return AdcSumVal[padid][samp]; }

  float GetCorADC(int padid, int samp)
      { return AdcCorVal[padid][samp]; }

  int GetBeamClock(int fem_seq,int module)
      { return beamclock[fem_seq][module]; }

  int GetPhysMod(int fem_seq,int module)
      { return physmod[fem_seq][module]; }

  int GetLvl1Trig(int fem_seq,int module)
      { return lvl1trig[fem_seq][module]; }

  int GetNTotalSample(int fem_seq)
      { return NsampleFromData[fem_seq]; }

  int fetchCalibDB(const int Runno);
//  int fetchCalibDB(PHTimeStamp Prun);
  int updateCalibDB(PHTimeStamp& tstart, PHTimeStamp& tend);

  void readCalibFromFile(const char *calibfile="CalibPar.txt");

  int SetPacketManually(int* inpacket, int fem_seq, int nwords);

  int StoreToHbdCell(HbdCellList *celllist);

  int StoreToHbdMiniCell(HbdMiniCellList *minicelllist);

  int CheckAndPreserveEvent(Event *evt);

  void SetDisablePadId(int *disablelist, int ndisable)
      { for(int i=0;i<ndisable;i++) Alive[disablelist[i]]=0; }

  void Verbosity(const int ival) {verbosity = ival;}

 private:
  int   AdcVal[2304][MAXSAMPLE];
  int   AdcSumVal[2304][3];
  float AdcCorVal[2304][MAXSAMPLE];

  int bPhysics;

  float ChargeThreshold;

  float ped[2304];
  float pep[2304];
  float pedw[2304];
  float pepw[2304];

  int PadId[2304];
  int SeqSec[2304];

  int Alive[2304];

  int fem_id[12];
  int beamclock[12][4];
  int physmod[12][4];
  int lvl1trig[12][4];
  int NsampleFromData[12];

  int Nthsample;
  int NSAMPLE;
  int NFEM;
  int NMOD;
  int NALLCHAN;

  int verbosity;

  int first_event;

  Packet *p;

};

#endif /* __HBDDCMRAW_HH_ */
