//  Declaration of class PdbSvxCoordinateOffset
//  Purpose: Store the coordinate system offset the VTX 
//  Author : Takashi Hachiya
//  Date   : Dec/21/2011
//-----------------------------------------------------------------------------
#ifndef __PDBSVXCOORDINATEOFFSET_HH__
#define __PDBSVXCOORDINATEOFFSET_HH__

#include "PdbCalChan.hh"

class PdbSvxCoordinateOffset : public PdbCalChan {
  public:
    PdbSvxCoordinateOffset();
    virtual ~PdbSvxCoordinateOffset(){}
    
    void setUsedRunNumber(const int run )              { m_usedRun=run; }
    void setOffsetVtxEastToWest(const double x, const double y, const double z) { m_offsetVtxEtoW[0]=x; m_offsetVtxEtoW[1]=y; m_offsetVtxEtoW[2]=z; }
    void setOffsetVtxToCnt(     const double x, const double y, const double z) { m_offsetVtxtoCnt[0]=x; m_offsetVtxtoCnt[1]=y; m_offsetVtxtoCnt[2]=z; }
    void setOffsetCntEastToWest(const double x, const double y, const double z) { m_offsetCntEtoW[0]=x; m_offsetCntEtoW[1]=y; m_offsetCntEtoW[2]=z; }
    void setGeometryVersion(const int version )        { m_geomVersion = version; }
    
    int    getUsedRunNumber()           { return m_usedRun; }
    double getOffsetVtxEastToWest(const int idx) { return (0<=idx&&idx<3) ? m_offsetVtxEtoW[idx]  : -9999.0; }
    double getOffsetVtxToCnt(     const int idx) { return (0<=idx&&idx<3) ? m_offsetVtxtoCnt[idx] : -9999.0; }
    double getOffsetCntEastToWest(const int idx) { return (0<=idx&&idx<3) ? m_offsetCntEtoW[idx]  : -9999.0; }
    int    getGeometryVersion()         { return m_geomVersion; }
    
    virtual void print() const;
    
  private:
    int    m_usedRun;
    double m_offsetVtxEtoW[3];
    double m_offsetVtxtoCnt[3];
    double m_offsetCntEtoW[3];
    int    m_geomVersion;

    ClassDef(PdbSvxCoordinateOffset,1);
};

#endif /* __PDBSVXCOORDINATEOFFSET_HH */
