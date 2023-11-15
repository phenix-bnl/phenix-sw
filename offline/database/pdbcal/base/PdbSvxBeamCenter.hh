//  Declaration of class PdbSvxBeamCenter
//  Purpose: Store the average beam position for the SVX reconstruction 
//  Author : Takashi Hachiya
//  Date   : Dec/20/2011
//-----------------------------------------------------------------------------
#ifndef __PDBSVXBEAMCENTER_HH__
#define __PDBSVXBEAMCENTER_HH__

#include "PdbCalChan.hh"

class PdbSvxBeamCenter : public PdbCalChan {
  public:
    PdbSvxBeamCenter();
    virtual ~PdbSvxBeamCenter(){}
    
    void setUsedRunNumber(const int run )              { m_usedRun=run; }
    void setBeamCenter(const double x, const double y) { m_beamCenter[0]=x; m_beamCenter[1]=y; }
    void setGeometryVersion(const int version )        { m_geomVersion = version; }
    
    int    getUsedRunNumber()           { return m_usedRun; }
    double getBeamCenter(const int idx) { return (0<=idx&&idx<2) ? m_beamCenter[idx] : -9999.; }
    int    getGeometryVersion()         { return m_geomVersion; }
    
    virtual void print() const;
    
  private:
    int    m_usedRun;
    double m_beamCenter[2];
    int    m_geomVersion;

    ClassDef(PdbSvxBeamCenter,1);
};

#endif /* __PDBSVXBEAMCENTER_HH */
