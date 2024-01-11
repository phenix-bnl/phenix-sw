// ===============
// FILE: SvxReco.h
// ===============
#ifndef __SVXRECO_H__
#define __SVXRECO_H__

#include <SubsysReco.h>

#include <phool.h>

#include "SvxParameters.h"
#include "svxAddress.hh"

#include <PHTimeServer.h>

#include <iostream>

class PHCompositeNode      ;
class SvxRawhitList        ;
class SvxClusterList       ;
class SvxRawhitClusterList ;
class SvxSegmentList       ;
class SvxEventInfo         ;
class SvxSensor            ;

class SvxGhitList          ;
class SvxGhitRawhitList    ;
class SvxGhitClusterList   ;
class SvxDeadMap           ;
class SvxClusterContainer  ;

class SvxStripThreshold  ;


/**
 * @brief  A SubsysReco module to process Svx data.
 * @date   Created by Sasha Lebedev in December 2009
 * @date   Move EW offset to svxDetectorGeo by Takashi Hachiya in December 2011
 * @date   Move the initialization of the SvxStripThreshold in SvxParManager by Takashi Hachiya in April 2013
 * @date   Remove old functions dealing with SvxStripTreshold by D. McGlinchey on 26 Jan 2016
 * @date   Format, clean up, and add comments by D. McGlinchey on 2 May 2016
 */
class SvxReco : public SubsysReco
{

public:

    SvxReco(const std::string &name = "SVXRECO");
    virtual ~SvxReco();

    int Init(PHCompositeNode *topNode);
    int InitRun(PHCompositeNode *topNode);
    int process_event(PHCompositeNode *topNode);
    int End(PHCompositeNode *topNode);

    virtual void Verbosity(const int ival);


    // Analysis methods
    int fillClusterList(svxAddress& SvxAddressObject); ///< Fill SVX clusters, using raw hits

    void set_StripixelZeroSup(int val);         ///< Just an interface.  See SvxSensor::set_adcthre_zs()
    void set_StripixelAdcSumThreshold(int val); ///< Just an interface.  See SvxSensor::set_adcthre_rawhit_sum()


    void set_ThisIsSimulation() {ThisIsSimulation = true;}
    void setEnableGoodADCRange(bool yn) { m_enableGoodAdcRange = yn;}


protected:
    SvxStripThreshold* _StripThresholds;

    // Data nodes
    SvxGhitList          *d_ghit;            ///< GEANT hits
    SvxRawhitList        *d_rawhit;          ///< SVX raw hits
    SvxGhitRawhitList    *d_ghit2rawhit;     ///< SVX ghit<->rawhit relater
    SvxClusterList       *d_cluster;         ///< SVX clusters
    SvxClusterList       *d_fake_cluster;    ///< SVX clusters
    SvxRawhitClusterList *d_rawhit2cluster;  ///< SVX rawhit<->cluster relater
    SvxGhitClusterList   *d_ghit2cluster;    ///< SVX ghit<->cluster relater
    SvxSegmentList       *d_segment;         ///< SVX segments
    SvxEventInfo         *d_eventinfo;       ///< Svx event info
    SvxClusterContainer  *d_container;       ///< SvxClusterContainer object

    // SVX geometry definitions
    // """"""""""""""""""""""""

    enum {NSVXSECTIONS = 1};                 ///< Number of SVX sections (only barrel now)
    // Svx section indexes: 0 - barrel, 1 - north, 2 - south
    enum {SVXBARRELINDEX, SVXNORTHINDEX, SVXSOUTHINDEX};
    unsigned int svxSecNlayers[NSVXSECTIONS]; ///< Number of layers/section:
    ///< 0-Barrel 1-North, 2-South
    // SvxSensor objects: Barrel
    unsigned int nBarLadder[SVXLADDERNUMBER]; ///< ladders/layer
    unsigned int nBarSensor[SVXSENSORNUMBER]; ///< sensor/ladder
    static const unsigned int barSenType[];   ///< sensor type
    /// Pointers to barrel sensor objects
    SvxSensor *(*barSensor)[SVXLADDERNUMBER][SVXSENSORNUMBER];


    /// Utility functions to find or create the svx Nodes.
    int CreateNodeTree(PHCompositeNode *topNode);
    void GetNodes(PHCompositeNode *topNode);

    // Counters
    int nSvxRawhits;        ///< Number of SvxRawhit objects filled
    int nSvxClusters;       ///< Number of SvxCluster objects filled
    int nSvxSegments;       ///< Number of SvxSegment objects filled

    // Parameter for charge sharing btw x-u.
    int   m_stripixel_adcthre_zs;         ///< see SvxStrip11v1::m_ADCThresholdZS
    int   m_stripixel_adcthre_rawhit;     ///< see SvxStrip11v1::m_adc_min_rawhit
    int   m_stripixel_adcthre_rawhit_sum; ///< see SvxStrip11v1::m_adc_min_rawhit_sum


    bool ThisIsSimulation; ///< Simulation flag [True: simulations, else False]

    int  EventNumber; ///< Event number counter

    bool m_enableGoodAdcRange; ///< ADC cut [True: apply ADC threshold cuts]

    PHTimeServer::timer _timer; ///< Timer
};
#endif


