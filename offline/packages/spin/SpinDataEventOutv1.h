#ifndef __SPINDATAEVENTOUTV1_H__
#define __SPINDATAEVENTOUTV1_H__

#include <iostream>
#include "SpinEventConstants.h"
#include "SpinDataEventOut.h"

class SpinDataEventOutv1 : public SpinDataEventOut 
{

  public:
             SpinDataEventOutv1();
    virtual ~SpinDataEventOutv1();

    void identify(std::ostream& os = std::cout) const
    {
      os << "identify yourself: I am a SpinDataEventOutv1 object" << std::endl;
      return;
    }

    int  isValid() const { return 1; }
    void Reset();

    // Set methods
    void SetEventSequence(int  eventsequence) { EventSequence = eventsequence; }

    void SetGL1CrossingID(int  gl1crossingid) { GL1CrossingID = gl1crossingid; }

    void SetSpinDirectionBlueFromV124(             short upordown) { SpinDirectionBlueFromV124              = upordown; }
    void SetSpinDirectionYellowFromV124(           short upordown) { SpinDirectionYellowFromV124            = upordown; }

    void SetSpinDirectionBlueFromCDEVFillPattern(  short upordown) { SpinDirectionBlueFromCDEVFillPattern   = upordown; }
    void SetSpinDirectionYellowFromCDEVFillPattern(short upordown) { SpinDirectionYellowFromCDEVFillPattern = upordown; }

    void SetGL1PEventNumber(   int boardid,               int  gl1peventnumber      ) { GL1PEventNumber[boardid]              = gl1peventnumber; }
    void SetGL1PCrossingID(    int boardid,               int  gl1pcrossingid       ) { GL1PCrossingID[boardid]               = gl1pcrossingid;  }
    void SetGL1PScalerCount(   int boardid, int scalerid, int  gl1pscalercount      ) { GL1PScalerCount[boardid][scalerid]    = gl1pscalercount; }

    void SetGL1PSumEventNumber(                           int  gl1psumeventnumber   ) { GL1PSumEventNumber                    = gl1psumeventnumber;   }
    void SetGL1PSumCrossingID(                            int  gl1psumcrossingid    ) { GL1PSumCrossingID                     = gl1psumcrossingid;    }
    void SetGL1PSumScalerCount(int boardid, int scalerid, long gl1psumscalercount   ) { GL1PSumScalerCount[boardid][scalerid] = gl1psumscalercount;   }
    void SetGL1PSumCrossingCount(                         long gl1psumcrossingcount ) { GL1PSumCrossingCount                  = gl1psumcrossingcount; }

    void SetSpinGL1CrossingID(                            int  spingl1crossingid    ) { SpinGL1CrossingID                     = spingl1crossingid;     }
    void SetSpinGL1PCrossingID(int boardid,               int  spingl1pcrossingid   ) { SpinGL1PCrossingID[boardid]           = spingl1pcrossingid;    }
    void SetSpinGL1PSumCrossingID(                        int  spingl1psumcrossingid) { SpinGL1PSumCrossingID                 = spingl1psumcrossingid; }


    // Get methods
    int   GetEventSequence() const { return EventSequence; }

    int   GetGL1CrossingID() const { return GL1CrossingID; }

    short GetSpinDirectionBlueFromV124()              const { return SpinDirectionBlueFromV124;   }
    short GetSpinDirectionYellowFromV124()            const { return SpinDirectionYellowFromV124; }

    short GetSpinDirectionBlueFromCDEVFillPattern()   const { return SpinDirectionBlueFromCDEVFillPattern;   }
    short GetSpinDirectionYellowFromCDEVFillPattern() const { return SpinDirectionYellowFromCDEVFillPattern; }


    int GetGL1PEventNumber(   int boardid              ) const { return GL1PEventNumber[boardid];              }
    int GetGL1PCrossingID(    int boardid              ) const { return GL1PCrossingID[boardid];               }
    int GetGL1PScalerCount(   int boardid, int scalerid) const { return GL1PScalerCount[boardid][scalerid];    }

    int  GetGL1PSumEventNumber()                          const { return GL1PSumEventNumber;                    }
    int  GetGL1PSumCrossingID()                           const { return GL1PSumCrossingID;                     }
    long GetGL1PSumScalerCount(int boardid, int scalerid) const { return GL1PSumScalerCount[boardid][scalerid]; }
    long GetGL1PSumCrossingCount()                        const { return GL1PSumCrossingCount;                  }

    int GetSpinGL1CrossingID()                      const { return SpinGL1CrossingID;              }
    int GetSpinGL1PCrossingID(int boardid)          const { return SpinGL1PCrossingID[boardid];    }
    int GetSpinGL1PSumCrossingID()                  const { return SpinGL1PSumCrossingID;          }


  protected:

    // from Event class
    int EventSequence;

    // raw numbers from GL1 packet
    int GL1CrossingID;

    // spin direction from V124 stored in GL1 packet,  up = +1, down = -1, Unpolarized=0
    short SpinDirectionBlueFromV124;
    short SpinDirectionYellowFromV124;

    // spin direction from intended fill pattern from CDEV information,  up = +1, down = -1, Unpolarized=0
    short SpinDirectionBlueFromCDEVFillPattern;
    short SpinDirectionYellowFromCDEVFillPattern;

    // raw numbers from GL1P1 and GL1P2 packets
    int GL1PEventNumber[nGL1PBoard];
    int GL1PCrossingID[nGL1PBoard];
    int GL1PScalerCount[nGL1PBoard][nGL1PScaler];

    // raw numbers from GL1Psum packet
    int  GL1PSumEventNumber;
    int  GL1PSumCrossingID;
    long GL1PSumScalerCount[nGL1PBoard][nGL1PScaler];
    long GL1PSumCrossingCount;

    // numbers calibrated
    int SpinGL1CrossingID;                // Calibrated as same as RHIC bunch id of Blue RING
    int SpinGL1PCrossingID[nGL1PBoard];   // for each beam crossing. That is, spin crossing id = 0
    int SpinGL1PSumCrossingID;            // means 0th blue bunch collides at PHENIX IR.



  ClassDef(SpinDataEventOutv1,1)    
};

#endif
