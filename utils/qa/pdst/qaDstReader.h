/*
 * qaDstReader.h
 * $Id: qaDstReader.h,v 1.25 2005/12/13 09:21:43 hpereira Exp $
 *
 * Ed Jackson <ejackson@iastate.edu>
 *
 * This class serves to encapsulate data input so most changes required as
 * a result of future DST changes will be localized here.  However, because
 * the prototypes of data access functions in the wrapper classes (being
 * phased out) don't match newer data access classes, those calls will
 * need to be updated as DST libraries are updated.  For example:
 *    zen = zdcout->get_Energy(0);      //zdcout is type ZdcOut* (new)
 *    zen = dzdcout->get_Energy(0,0);   //dzdcout is type dZdcOutWrapper* (old)
 */

#ifndef _QADSTREADER_H
#define _QADSTREADER_H


// PHOOL headers
#include <PHNodeIterator.h>

// Phenix headers
#include <RunHeader.h>
#include <FclCompactRawv1.h>
#include <ZdcOut.h>
#include <BbcOutv1.h>
#include <NtcOutv2.h>
#include <NtcpOutv1.h>
#include <MvdVertexOut.h>
#include <MvddNdEtaOut.h>
#include <DchHitLineTablev2.hh>
#include <DchTrack.h>
#include <dPadClusterWrapper.h>
#include <dTecCalibWrapper.h>
#include <dCglTrackWrapper.h>
#include <CglTrackv2.h>
#include <dCglParticleWrapper.h>
#include <dTofReconstructedWrapper.h>
#include <PHTrackOut.h>
#include <dCrkHitWrapper.h>
#include <TecOutV2.hh>

#include <TriggerHelper.h>
#include <TzrReturncodes.h>
#include <TzrOut.h>
#include <TzrOutv1.h>
#include <TzrRaw.h>

#include <TrigLvl1.h>
#include <ErtOut.h>
#include <PHCentralTrack.h>

// forward declaration
class dEmcClusterLocalExtWrapper;
class emcClusterContainer;
class emcTowerContainer;

//-----------------------------------------------------------------------
// class definition

class qaDstReader
{
  public:
    // We rely on the default constructor and destructor
  qaDstReader();
    // update finds output nodes for an event.  Returns zero if successful,
    // non-zero if errors are encountered.
    int update(PHNodeIterator *ni, TString *optstr = NULL );

    RunHeader* getRunHeader() {return _runheader;}

    FclCompactRawv1* getFclNorth() {return _fclrawnorth;}
    FclCompactRawv1* getFclSouth() {return _fclrawsouth;}
    
    ZdcOut* getZdcOut() {return _zdcout;}

    BbcOut* getBbcOut() {return _bbcout;}

    NtcOut*  getNtcOut() {return _ntcout;}
    NtcpOut* getNtcpOut() {return _ntcpout;}
    
		TzrOut* getTzrOut() {return _tzrout;}

		MvdVertexOut* getMvdVertexOut() {return _mvdvtx;}
		MvddNdEtaOut* getMvddNdEtaOut() {return _mvddnde;}
    
    DchHitLineTablev2* getDchHitLineTable() {return _dchhitlinetable;}
    DchTrack* getDchTrack() {return _dchtrack;}
    
    dPadClusterWrapper* getPc1Cluster() {return _pc1cluster;}
    dPadClusterWrapper* getPc2Cluster() {return _pc2cluster;}
    dPadClusterWrapper* getPc3Cluster() {return _pc3cluster;}
    

    TecOutV2* getTecOut() {return _tecout;}
    
    CglTrack* getCglTrack() {return _cgltrack;}
    dCglParticleWrapper* getCglParticle() {return _cglparticle;}
    
    dTofReconstructedWrapper* getTofReconstructed() {return _tofreconstructed;}
    
    PHTrackOut* getPHTrack() {return _phtrack;}
    
    dEmcClusterLocalExtWrapper* getEmcClusterLocalExt()
      {return _emcclusterlocalext;}

    emcClusterContainer* getEmcClusterContainer()
      { return _emcclustercontainer; }

    emcTowerContainer* getEmcTowerContainer()
      {return _emctowercontainer; }

    dCrkHitWrapper* getCrkHit()
      {return _crkhit;}
						
    void updateTriggerHelper(TriggerHelper *in) {_triggerhelper = in;}
    TriggerHelper *getTriggerHelper() {return _triggerhelper;}

    Int_t getMutrackFlag() {return mutrack_wrapper;}

    TrigLvl1 *getTrigLvl1() {return _triglvl1;}
    ErtOut *getErtOut() {return _ertout;}
    PHCentralTrack *getPHCentralTrack() {return _phcentraltrack;}
  private:
    RunHeader* _runheader;
    FclCompactRawv1* _fclrawnorth;
    FclCompactRawv1* _fclrawsouth;
    ZdcOut* _zdcout;
    BbcOut* _bbcout;
    NtcOut* _ntcout;
    NtcpOut* _ntcpout;
		TzrOut* _tzrout;
		MvdVertexOut* _mvdvtx;
		MvddNdEtaOut* _mvddnde;
    DchHitLineTablev2* _dchhitlinetable;
    DchTrack* _dchtrack;
    dPadClusterWrapper* _pc1cluster;
    dPadClusterWrapper* _pc2cluster;
    dPadClusterWrapper* _pc3cluster;
    TecOutV2* _tecout;
    CglTrack* _cgltrack;
    dCglParticleWrapper* _cglparticle;
    dTofReconstructedWrapper* _tofreconstructed;
    PHTrackOut* _phtrack;
    dEmcClusterLocalExtWrapper* _emcclusterlocalext;
    emcClusterContainer* _emcclustercontainer;
    emcTowerContainer* _emctowercontainer;
    dCrkHitWrapper* _crkhit;
		dMuiRoadsWrapper* _muiroads;
    dMutCathodeClustersWrapper* _mutcathodeclusters;
    dMutCalibCathodesOut* _mutcalibcathodesout;
    dMuoTracksWrapper* _muotracks;
    dMuoTracksOut* _muotracksout;
			
    TriggerHelper * _triggerhelper;

    TrigLvl1 *_triglvl1;
    ErtOut *_ertout;
    PHCentralTrack *_phcentraltrack;
    
    Int_t mutrack_wrapper;
};  //class qaDstReader

#endif  /* _QADSTREADER_H */
// EOF


