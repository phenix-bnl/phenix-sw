// ====================
// FILE: SvxStrip11v1.h
// ====================
#ifndef __SVXSTRIP11V1_HH_
#define __SVXSTRIP11V1_HH_
#include <iostream>
#include <vector>

#include "SvxStripixel.h"
#include "SvxCluster.h"
#include "SvxClusterList.h"
#include "SvxRawhitCluster.h"
#include "SvxRawhitClusterList.h"
#include "SvxGhitCluster.h"
#include "SvxGhitClusterList.h"

class svxAddress;

/**
 * @brief  A fully implemented class for a stripixel sensor.
 *
 * This is a class for SvxStrip11 sensor (stripixel of conventional layout).
 * The method of clustering stripixel rawhits is implemented here.
 *
 * @date  Created by V. L. Rykov on 18-Mar-2004
 */
class SvxStrip11v1 : public SvxStripixel<11>
{
 public:
// Constructor(s) & Destructor
// """""""""""""""""""""""""""
  SvxStrip11v1(const int sc=-1, const int lr=-1,
	       const int ld=-1, const int sn=-1);
  virtual ~SvxStrip11v1();

  // Standard functions of all inheritors of PHObject classes...
  // """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: SvxStrip11v1 object: isValid() = "
       << isValid() << std::endl;
  }

  virtual void set_sNOISE(float val) { m_sNOISE = val; }
  virtual void set_adcthre_zs(int val) { m_ADCThresholdZS = val; }
  virtual void set_adcthre_rawhit    (int val) { m_adc_min_rawhit     = val; }
  virtual void set_adcthre_rawhit_sum(int val) { m_adc_min_rawhit_sum = val; }

  /// Add noise for stripixel by 2008/06/01 M.Togawa
  // - added argument of g2raw 2011.02.25 T.Hachiya
  int AddNoise(const int rhit_first, const int rhit_last, SvxRawhitList* rawlist, 
               SvxGhitRawhitList* g2raw, int g2rFirst) const;

  /// Cluster finders
  virtual int findClusters(SvxRawhitList*        rawlist,
                           SvxClusterList*       clslist,
                           SvxRawhitClusterList* raw2cls = NULL,
                           SvxGhitRawhitList*    ght2raw = NULL,
                           SvxGhitClusterList*   ght2cls = NULL,
                           svxAddress* address = NULL);

  virtual void add_rawhit(SvxRawhit *rawhit);

 protected:
  typedef std::vector<SvxRawhit*> RawhitGroup_t;
  typedef std::vector<RawhitGroup_t> RawhitGroupList_t;
  /**
   * @brief  Make a group of raw hits from SvxRawhitList
   *
   * Called by findClusters() function.
   * @param[in]  rawlist   a SvxRawhitList.
   * @param[out] grp_list a RawhitGroupList_t.
   */
  //  int  makeGrouphits(SvxRawhitList* rawlist, RawhitGroupList_t** grp_list);
  int  makeGrouphits(RawhitGroupList_t** grp_list);

  /**
   * @brief  Collect raw hits around the 4th arg "rhit".
   *
   * Called by makeGrouphits() function or by itself recursively.
   * @param[in]  section   a section to be searched.
   * @param[in]  readout   a readout to be searched.
   * @param[out] group     a group to which raw hits are added.
   * @param[in]  rhit      a starting point.  if null, the 0th hit in senSec[][] is used.
   * @return     true if at least one raw hit is added to the group.
   */
  bool CollectNeighborRawhits(int section, int readout, RawhitGroup_t& group, SvxRawhit* rhit=0);
  /**
   * @brief  Make a mosaic pattern of fired tiles.
   *
   * Called from findClusters() function.
   * CAUTION: This mosaic builder is strictly for the "conventional" stripixel sensor:
   *   1) nReadout = 2
   *   2) 2 pairs of IDENTICAL sections: secTYPE[i][0] == secTYPE[i][1]
   *   3) xSlope[i][0] =    xSlope[i][1] = 1
   *   4) zSlope[i][0] = 0; zSlope[i][1] = 1
   */
  int makeMosaic(RawhitGroupList_t**   grp_list,
                         SvxClusterList*       clslist,
                         SvxRawhitClusterList* raw2cls = NULL,
                         SvxGhitRawhitList*    ght2raw = NULL,
                         SvxGhitClusterList*   ght2cls = NULL,
                         svxAddress* address = NULL);

  /// Flag for the sensor configuration applicability (check by makeMosaic)
  static bool config_checked;     //! set true if configuration is correct
  /// Instumented channel cut-off_set flag
  static bool cut_off_set_flag;  //! true if chanCutOff[][] has been set

 private:
  float m_sNOISE; ///< the Gaussian width of rawhit noise in ADC unit
  int m_ADCThresholdZS; ///< noises with ADC < this value are not stored (zero suppressed) in SvxRawhitList
  int m_adc_min_rawhit; ///< rawhits with ADC >= this value are used in clustering
  int m_adc_min_rawhit_sum; ///< rawhit groups (x & u separately) with summed ADC >= this value are used in clustering

  // ---
  ClassDef(SvxStrip11v1,1)
};

#endif
