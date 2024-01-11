
#ifndef __TFvtxTrigEmulInfo_v1_HH_
#define __TFvtxTrigEmulInfo_v1_HH_

/*!
  \file    TFvtxTrigEmulInfo_v1.h
  \brief   PHObject holding FVTX trigger emulator output information (v1)
  \author  D. McGlinchey
  \version $Revision: 1.2 $
  \date    $Date: 2016/04/26 19:59:27 $
*/

#include <TFvtxTrigEmulInfo.h>
#include <FVTXOO.h>

class TFvtxTrigEmulInfo_v1 : public TFvtxTrigEmulInfo
{
public:
  TFvtxTrigEmulInfo_v1(TFvtxTrigEmulInfo* info = NULL);
  virtual ~TFvtxTrigEmulInfo_v1() {}

  void identify(std::ostream &os = std::cout) const {
    os << "Identify yourself: TFvtxTrigEmulInfo_v1 object" << std::endl;
  }

  /// Clear Event
  void Reset();

  //! @name Setters
  //@{

  void set_trig(bool trig);

  void set_trigArm(bool trig,
                   int arm);

  void set_trigCage(bool trig,
                    int arm,
                    int cage);

  void set_trigFEM(bool trig,
                   int arm,
                   int cage,
                   int fem);

  void set_trigSector(bool trig,
                      int arm,
                      int cage,
                      int sector);

  void set_trigStation(bool trig,
                       int arm,
                       int cage,
                       int sector,
                       int station);

  //@}

  //! @name Get trigger decisions
  //@{

  bool did_trigFire();

  bool did_trigFireArm(int arm);

  bool did_trigFireCage(int arm,
                        int cage);

  bool did_trigFireFEM(int arm,
                       int cage,
                       int fem);

  bool did_trigFireSector(int arm,
                          int cage,
                          int sector);

  bool did_trigFireStation(int arm,
                           int cage,
                           int sector,
                           int station);

  //@}


private:

  //--- Variables
  bool m_trigStation[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE][FVTXOO::MAX_SECTOR][FVTXOO::MAX_STATION];
  bool m_trigSector[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE][FVTXOO::MAX_SECTOR];
  bool m_trigFEM[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE][FVTXOO::MAX_SECTOR/2];
  bool m_trigCage[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE];
  bool m_trigArm[FVTXOO::MAX_ARM];
  bool m_trig;


  ClassDef(TFvtxTrigEmulInfo_v1, 1)

};
#endif
