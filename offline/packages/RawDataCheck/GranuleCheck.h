#ifndef __GRANULECHECK_H__
#define __GRANULECHECK_H__

#include <iostream>
#include <string>
#include <vector>

class TH1;
class Event;
class Packet;
class RawDataCheck;

struct packetstruct
{
  unsigned int PacketId;
  int BadLength;
  int DcmCheckSumError;
  int FEMParityError;
  int FEMClockCounterError;
  int LastClock;
  int FEMGL1ClockError;
  int FEMEvtSeqError;
  unsigned int PacketCounter;
  unsigned int GoodPackets;
  unsigned int SumBytes;
  int DcmFEMParityError;
  int GlinkError;
  int SubsystemError;
};


class GranuleCheck
{
 public:
  GranuleCheck(const std::string &name = "NONE");
  virtual ~GranuleCheck() {}

  int Init(RawDataCheck *raw);
  virtual int Init() {return 0;}
  virtual int process_event(Event *e);
  virtual int Reset();
  virtual int ResetEvent() {return 0;}
  virtual int PacketErrorReset();
  virtual int PacketResetAll();
  virtual int BeginRun(const int runno);
  const char *Name() const {return ThisName.c_str();}

  virtual int DcmCheckSumCheck(); // check the dcm checksum (transmission errors)
  virtual int DcmFEMParityErrorCheck(); // check dcm bit for good fem parity
  virtual int FEMParityErrorCheck(); // Check Fem parity
  virtual void SetBeamClock();  // set the BeamClock
  virtual int FEMClockCounterCheck(); // Check if FEM clock counters are consistent in subsystem
  virtual int FEMGL1ClockCounterCheck(); // check if FEM and GL1 agree
  virtual int LocalEventNumberOffset() {return -1;} // offset between fem event cnter and event number
  virtual unsigned int LocalEventCounter();
  virtual int FEMEventSequenceCheck(); // check if event numbers agree
  virtual unsigned int GlobalEventCounter(); // return GL1 event counter for this granule
  virtual int GranuleGL1SynchCheck(); // test if granule is in sync with GL1
  virtual int GlinkCheck();
  virtual int SubsystemCheck(Event *evt, const int iwhat);// subsystem specific checks
  virtual int MissingPacketCheck(Event *evt);// check for missing packet
  virtual void identify(std::ostream& out = std::cout) const;
  virtual int CreateHisto();
  virtual int FillHisto();
  virtual int Verbosity() const {return verbosity;}
  virtual void Verbosity(const int i) {verbosity = i;}
  virtual int DoEveryEvent(Event *evt) {return 0;}
  virtual int BadBeamClock() {return 0;}
  virtual int AddPacketsToAllPacketList();
  virtual int EndRun(const int runnumber) {return 0;}

 protected:
  std::string ThisName;
  virtual int InitPacketIdHisto();
  int verbosity;
  unsigned int GranuleNumber;
  unsigned int BeamClock;
  int hitformat;
  int packets_found;
  int gl1syncerror;
  unsigned int nfill; 
  int totalgl1syncerror;
  int totalsubsystemerror;
  int packetLength;
  int rawdat[2000];
  int num_all_packets;
  int gl1syncok;
  Packet *p;
  RawDataCheck *rawdatacheck;
  TH1 *ErrorCounterHisto; // store the error counter
  TH1 *packetHisto; // store the packet id
  TH1 *variableHisto; // store whatever vars we need
  std::vector<packetstruct> pkt;
  std::vector<packetstruct>::iterator packetiter;
};

#endif /* __GRANULECHECK_H__ */

