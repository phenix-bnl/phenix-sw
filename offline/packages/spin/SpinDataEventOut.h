#ifndef __SPINDATAEVENTOUT_H__
#define __SPINDATAEVENTOUT_H__

#include <iostream>
#include "PHObject.h"
#include "SpinEventConstants.h"

class SpinDataEventOut : public PHObject //++CINT
{
  
  public:
    virtual ~SpinDataEventOut() {}
  virtual void identify(std::ostream& os = std::cout) const 
    {
      os << "virtual SpinDataEventOut object" << std::endl;
      return;
    }
    
    virtual int isValid() const 
    {
      std::cout << "SpinDataEventOut: isValid() not implemented" << std::endl;
      return 0;
    }
 
    virtual void Reset() {}
 
    virtual void SetEventSequence(int geventsequence) {}

    virtual void SetGL1CrossingID(int gl1crossingid) {}

    virtual void SetGL1PEventNumber(int boardid, int gl1peventnumber) {}
    virtual void SetGL1PCrossingID(int boardid, int gl1pcrossingid) {}
    virtual void SetGL1PScalerCount(int boardid, int scalerid, int gl1pscalercount) {}

    virtual void SetGL1PSumEventNumber(int gl1psumeventnumber) {}
    virtual void SetGL1PSumCrossingID(int gl1psumcrossingid) {}
    virtual void SetGL1PSumScalerCount(int boardid, int scalerid, long gl1psumscalercount) {}
    virtual void SetGL1PSumCrossingCount(long gl1psumcrossingcount ) {}


    virtual void SetSpinGL1CrossingID(int gl1crossingid) {}
    virtual void SetSpinGL1PCrossingID(int boardid, int spincrossingid) {}
    virtual void SetSpinGL1PSumCrossingID(int spincrossingid) {}

    virtual void SetSpinDirectionBlueFromV124(short upordown) {}
    virtual void SetSpinDirectionYellowFromV124(short upordown) {}

    virtual void SetSpinDirectionBlueFromCDEVFillPattern(short upordown) {}
    virtual void SetSpinDirectionYellowFromCDEVFillPattern(short upordown) {}


    virtual int   GetEventSequence()                               const { return -99999; }

    virtual int   GetGL1CrossingID()                               const { return -99999; }

    virtual int   GetGL1PEventNumber(int boardid)                  const { return -99999; }
    virtual int   GetGL1PCrossingID(int boardid)                   const { return -99999; }
    virtual int   GetGL1PScalerCount(int boardid, int scalerid)    const { return -99999; }

    virtual int   GetGL1PSumEventNumber()                          const { return -99999; }
    virtual int   GetGL1PSumCrossingID()                           const { return -99999; }
    virtual long  GetGL1PSumScalerCount(int boardid, int scalerid) const { return -99999; }
    virtual long  GetGL1PSumCrossingCount()                        const { return -99999; }

                  
    virtual int   GetSpinGL1CrossingID()                           const { return -99999; }
    virtual int   GetSpinGL1PCrossingID(int boardid)               const { return -99999; }
    virtual int   GetSpinGL1PSumCrossingID()                       const { return -99999; }
  
    virtual short GetSpinDirectionBlueFromV124()                   const { return -999; }
    virtual short GetSpinDirectionYellowFromV124()                 const { return -999; }

    virtual short GetSpinDirectionBlueFromCDEVFillPattern()        const { return -999; }
    virtual short GetSpinDirectionYellowFromCDEVFillPattern()      const { return -999; }


  ClassDef(SpinDataEventOut,1)    
};

#endif
