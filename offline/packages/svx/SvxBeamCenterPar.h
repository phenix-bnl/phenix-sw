/** \file SvxBeamCenterpar.h
 * Contains the class for the beam center positon for SVX reconstruction.
 * Author : Takashi Hachiya
 * Data   :  Dec/20/2011
 * This class treats the beam center position run by run (or fill by fill).
 * This class includes the DB Interface using offline/database/PdbSvxBeamCenter.
 */

#ifndef __SVXBEAMCENTERPAR_HH_
#define __SVXBEAMCENTERPAR_HH_

#include <PdbSvxBeamCenter.hh>

class PHTimeStamp;

class SvxBeamCenterPar {
  public:

    SvxBeamCenterPar();
    virtual ~SvxBeamCenterPar();

    void Verbosity(const int level) { m_verbosity = level; }

    void setUsedRunNumber(const int run)              { m_beamcenter.setUsedRunNumber(run);}
    void setBeamCenter(const double x, const double y){ m_beamcenter.setBeamCenter(x, y);}
    void setGeometryVersion(const int version)        { m_beamcenter.setGeometryVersion(version);}
    void setParameters(const double x, const double y, const int run=0, const int version=0){
           setBeamCenter(x, y);
           setUsedRunNumber(run);
           setGeometryVersion(version);
       }

    int    getUsedRunNumber()          { return m_beamcenter.getUsedRunNumber();}
    double getBeamCenter(const int idx){ return m_beamcenter.getBeamCenter(idx);}
    int    getGeometryVersion()        { return m_beamcenter.getGeometryVersion();}

    // Database I/F functions
    bool fetchFromDB(const int run);
    bool fetchFromDB(const PHTimeStamp* Tsearch);
    bool updateToDB(const int beginRun, const int endRun, const char *desc);
    bool updateToDB(const PHTimeStamp* Tbegin, const PHTimeStamp* Tend, const char *desc);

    void print() const;

  private:
    int m_verbosity;

    PdbSvxBeamCenter m_beamcenter;
/*
    int    m_usedRun;
    double m_beamCenter[2];
    int    m_geomVersion;
*/
};

#endif

