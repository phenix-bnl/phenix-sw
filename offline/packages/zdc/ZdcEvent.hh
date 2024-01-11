#ifndef __PHENIX_ZDCEVENT_HH
#define __PHENIX_ZDCEVENT_HH

#include <phool.h>
#include <ZdcCalib.hh>
#include <map>

class PHCompositeNode;
class Event;
class ZdcRaw;
class ZdcOut;
class SmdOut;

static const int MAX_ZDC_CHANNELS = 40;

class ZdcEvent
{
public:
  ZdcEvent();
  virtual ~ZdcEvent();

  void Clear ();

  PHBoolean setRawData (PHCompositeNode *topNode);
  PHBoolean setRawData (Event *event);

  int calculate (void);
  int CalcArmTime(int arm, int nch, float& ArmTime, float& ArmTimeError);
  PHBoolean PutEndProduct( PHCompositeNode *topNode );

  // Set default calibration data to ZdcEvent.
  void setCalibDataAll();

  // Fetch Calib. data from DB.
  void setCalibDataAll(const PHTimeStamp & time);

  // Copy exist calib. data into ZdcEvent.
  void setCalibDataAll(ZdcCalib * ExistCalib, const PHTimeStamp &time);
  void setCalibDataAll(const ZdcCalib& ExistCalib, const PHTimeStamp &time);

  ZdcCalib *getCalib () { return &calib; }

  PHBoolean setEvent (PHCompositeNode *) { return True; }

  int   getEventNumber () const { return (EventNumber); }

  float getCharge (int PmtIndx) const { return (Charge[PmtIndx]); }
  float getTime0 (int PmtIndx) const { return (Time0[PmtIndx]); }
  float getTime1 (int PmtIndx) const { return (Time1[PmtIndx]); }
  float getHitTime (int side) const 
  {
    if (!((side==Zdc::South)||(side==Zdc::North)))
      {
	return INVALID_FLOAT;
      }
    return Timing[side]; 
  }
  float getEnergy (int side) const 
  { 
    if (!((side==Zdc::South)||(side==Zdc::North)))
      {
	return INVALID_FLOAT;
      }
    return Energy[side]; 
  }
  float getZvertex () const { return Zvertex; }
  float getZvertexError () const { return ZvertexError; }
  float getTimeZero () const { return TimeZero; }
  float getTimeZeroError () const { return TimeZeroError; }

  double CalcSmdPosY(const int arm);
  double CalcSmdPosX(const int arm);
  double CalcSmdEnergy(const int arm);

  float getSmdPosX(const int arm) { return smdposx[arm]; }
  float getSmdPosY(const int arm) { return smdposy[arm]; }
  float getSmdEnergy(const int arm) { return smdenergy[arm]; }

  void setEventNumber (int givenEventNumber) { EventNumber = givenEventNumber; }

private:
  int EventNumber;

  ZdcRaw *zdcraw; // Raw Input Object (TDCs,ADCs)
  ZdcRaw *zdcrawevt; // copy of zdcraw in case it needs deleting
  ZdcOut *zdcout; // Output Object (times,vertices,energies)
  SmdOut *smdout; // SMD Output Object

  // Corrected data
  float Charge[MAX_ZDC_CHANNELS];
  float Time0[MAX_ZDC_CHANNELS];
  float Time1[MAX_ZDC_CHANNELS];
  float Time0Err[MAX_ZDC_CHANNELS];	// PMT Time Resolution
  float Time1Err[MAX_ZDC_CHANNELS];

  // end-products
  float Timing[2];	// Arm Hit Time
  float TimingError[2];	// Arm Hit Time Error
  float Energy[2];
  float Zvertex;
  float ZvertexError;
  float TimeZero;
  float TimeZeroError;

  float smdposx[2];
  float smdposy[2];
  float smdrmsx[2];
  float smdrmsy[2];
  float smdenergy[2];

  // calibration parameters
  ZdcCalib calib;

  // Zdc Map
  int *zdc_pmt_map;
  unsigned int zdc_pmt_map_size;
};
#endif

