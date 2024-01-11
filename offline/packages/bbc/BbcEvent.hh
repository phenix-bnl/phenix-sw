#ifndef __BBCEVENT_HH__
#define __BBCEVENT_HH__

#include "phool.h"
#include "Bbc.hh"
#include "BbcCalib.hh"

class PHCompositeNode;
class Event;

class BbcEvent
{
public:
  BbcEvent (void);
  ~BbcEvent (void)
  {
  }

  PHBoolean SetUcal (PHCompositeNode *)
  {
    return True;
  }
  PHBoolean GhitRaw (PHCompositeNode *)
  {
    return True;
  }
  PHBoolean RawToFem (PHCompositeNode *)
  {
    return True;
  }
  PHBoolean FemToDcm (PHCompositeNode *)
  {
    return True;
  }

  // Convert DCM table to BbcRaw table.
  PHBoolean DcmToRaw (PHCompositeNode *);

  // Storing Bbcout table to DST node. 	
  PHBoolean DstStore (PHCompositeNode *);
  
  // Direct conversion of PRDF to BbcRaw table.
  PHBoolean setRawData (PHCompositeNode *);

  // Special function to set Raw data for Online software.
  PHBoolean setRawData (Event *);	

  // Convert online-Event class to BbcRaw data directly.

  // Method to set fake data to testing calibration method.   
  void setFakeEvent ();		

  // Method to set fake pedestal to testing calibration method.
  void setFakePedestal ();	

  // sigma is the cut value of Tdc0 to calculate ArmHitTime.
  int calculate ();		

  // Set default calibration data to BbcEvent.
  void setCalibDataAll ();	

  // Fetch calibration data from database
  void setCalibDataAll (const BbcTime_t & time);	

  // copy exist calibration data into BbcEvent. 
  void setCalibDataAll (BbcCalib * ExistCalib);	

  BbcCalib *getCalib ()
  {
    return &calib;
  }

  int getEventNumber (void) const
  {
    return (EventNumber);
  }
  int getAdc (int PmtIndx) const
  {
    return (Adc[PmtIndx]);
  }
  int getTdc0 (int PmtIndx) const
  {
    return (Tdc0[PmtIndx]);
  }
  int getTdc1 (int PmtIndx) const
  {
    return (Tdc1[PmtIndx]);
  }

  int getCutVal ()
  {
    return (calib.getCutVal ());
  }

  BbcTime_t getEventTime (void) const
  {
    return (0);
  }

  float getTrueAdc (int PmtIndx) const
  {
    return (TrueAdc[PmtIndx]);
  }
  float getCharge (int PmtIndx) const
  {
    return (Charge[PmtIndx]);
  }
  float getHitTime0 (int PmtIndx) const
  {
    return (HitTime0[PmtIndx]);
  }
  float getHitTime1 (int PmtIndx) const
  {
    return (HitTime1[PmtIndx]);
  }
  int isHit (int PmtIndx) const
  {
    return (iHit[PmtIndx]);
  }
 
  int isArmHitPmt (int PmtIndx) const
  {
    return (armHitPmt[PmtIndx]);
  }

  int getnHitPmt (Bbc::ArmType arm) const
  {
    return (nHitPmt[arm]);
  }
  float getChargeSum (Bbc::ArmType arm) const
  {
    return (ChargeSum[arm]);
  }
  float getArmHitTime (Bbc::ArmType arm) const
  {
    return (ArmHitTime[arm]);
  }
  float getArmHitTimeError (Bbc::ArmType arm) const
  {
    return (ArmHitTimeError[arm]);
  }
  float getZVertex (void) const
  {
    return (ZVertex);
  }
  float getZVertexError (void) const
  {
    return (ZVertexError);
  }
  float getTimeZero (void) const
  {
    return (TimeZero);
  }
  float getTimeZeroError (void) const
  {
    return (TimeZeroError);
  }

  void setEventNumber (int givenEventNumber)
  {
    EventNumber = givenEventNumber;
  }
  void setAdc (int givenAdc, int PmtIndx)
  {
    Adc[PmtIndx] = givenAdc;
  }
  void setTdc0 (int givenTdc0, int PmtIndx)
  {
    Tdc0[PmtIndx] = givenTdc0;
  }
  void setTdc1 (int givenTdc1, int PmtIndx)
  {
    Tdc1[PmtIndx] = givenTdc1;
  }

  int printAdcTdc (void) const;
  int printPmtChTime (void) const;

  float TimeLagOfTransitTime (int PmtIndx, float ZVertex2) const;

  void Clear ();

private:

  //raw data
  int EventNumber;
  int Adc[BBC_N_PMT];
  int Tdc0[BBC_N_PMT];
  int Tdc1[BBC_N_PMT];

  // converted (corrected) data
  int iHit[BBC_N_PMT];
  int armHitPmt[BBC_N_PMT];

  // Adc value w/ pedestal subtracted
  float TrueAdc[BBC_N_PMT];		

  float Charge[BBC_N_PMT];
  float HitTime0[BBC_N_PMT];
  float HitTime1[BBC_N_PMT];

  // End product data
  int nHitPmt[2];
  float ChargeSum[2];
  float ArmHitTime[2];
  float ArmHitTimeError[2];
  float ZVertex;
  float ZVertexError;
  float TimeZero;
  float TimeZeroError;

  // etc
  int calcEndProduct (void);
  int calcArmProduct (Bbc::ArmType arm);
  int calcPmtProduct (int PmtIndx);

  // must exec after calcArmProduct
  int calcFlag ();	

  BbcCalib calib;
};

#endif /* __BBCEVENT_HH__ */
