#ifndef __SVXSTRIP11V2_H__
#define __SVXSTRIP11V2_H__

// ====================
/// \file SvxStrip11v2.h
/// \brief Class implementation for the stripixel sensor description
/// and clustering
/// \author M. McCumber (under the svx documentation effort)
///
/// Created by V. L. Rykov on 18-Mar-2004
/// v2 Created by Alex Shaver in August 2011 <alexshaver@gmail.com>
/// Documentation/Clean-up by M.McCumber 15-Feb-2013
// ====================

// standard includes
#include <iostream>
#include <vector>

// svx includes
#include "SvxStripixel.h"
#include "SvxCluster.h"
#include "SvxClusterList.h"
#include "SvxRawhitCluster.h"
#include "SvxRawhitClusterList.h"
#include "SvxGhitCluster.h"
#include "SvxGhitClusterList.h"

// forward declarations
class svxAddress;

/// \class SvxStrip11v2
/// \brief Stripixel sensor class implementation includes clustering algorithm
///
/// This is a class for SvxStrip11 sensor (stripixel of conventional layout).
/// The method of clustering stripixel rawhits is implemented here.
///
class SvxStrip11v2 : public SvxStripixel<11> {
 public:

  SvxStrip11v2(const int sc=-1, 
	       const int lr=-1,
	       const int ld=-1, 
	       const int sn=-1);
  virtual ~SvxStrip11v2() {};

  /// print object details via PHObject method call
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: SvxStrip11v2 object: isValid() = "
       << isValid() << std::endl;
  }

  /// stripixel cluster creation
  virtual int findClusters(SvxRawhitList*        rawlist,
                           SvxClusterList*       clslist,
                           SvxRawhitClusterList* raw2cls = NULL,
                           SvxGhitRawhitList*    ght2raw = NULL,
                           SvxGhitClusterList*   ght2cls = NULL,
                           svxAddress*           address = NULL);

  /// add noise to stripixel sensor (for simulation purposes)
  /// \todo bad form noise should be added in SvxSimulator not in the
  /// the reconstruction chain
  int AddNoise(const int rhit_first, 
	       const int rhit_last, 
	       SvxRawhitList* rawlist, 
               SvxGhitRawhitList* g2raw, int g2rFirst) const;

  /// set Gaussian width of noise hit (ADC units)
  virtual void set_sNOISE(float val) {m_sNOISE = val;}
  /// set suppression threshold for noise hits
  virtual void set_adcthre_zs (int val) {m_ADCThresholdZS = val;}

  /// set the minimum ADC value for single channel to be clustered
  virtual void set_adcthre_rawhit (int val) {m_adc_min_rawhit = val;}
  /// set the minimum ADC value for grouped channels to be clustered
  virtual void set_adcthre_rawhit_sum(int val) {m_adc_min_rawhit_sum = val;}

  /// insert a rawhit into the sensor
  virtual void add_rawhit(SvxRawhit *rawhit);

  /// print out settings
  virtual void printPar() const {
    SvxStripixel<11>::printPar();
    std::cout << "Noise          : " << m_sNOISE<<std::endl; 
    std::cout << "AdcThresholdZS : " << m_ADCThresholdZS<<std::endl;
    std::cout << "AdcMin         : " << m_adc_min_rawhit<<std::endl;
    std::cout << "AdcMinSum      : " << m_adc_min_rawhit_sum<<std::endl;
  }

  virtual void test_CollectNeighborRawhits();
  virtual void test_makeGrouphits();
  virtual void test_findCluster(svxAddress* address = NULL, int sec=0, double xpar=0.5, double zpar=0.5);
  virtual void test_makeMosaic(svxAddress* address = NULL);
  virtual void test_makeMosaicAll(svxAddress* address = NULL, int sec=0, int xline=0, double xpar=0.5, double zpar=0.5);

 protected:

  /// define useful structures for grouping neighboring X and U hits
  /// prior to calculating the crossing position
  /// \todo this has outgrown this way of storing clusters under construction
  /// we could add a new class to handle this information
  typedef std::vector<SvxRawhit*> RawhitGroup_t;
  typedef std::vector<RawhitGroup_t> RawhitGroupList_t;
  
  /// GroupData structure (used to build the cluster properties) 
  /// [0]=group index; [1]=ch_mean; [2]=adc_sum; [3]=nhit;
  /// [4]=ch_high;     [5]=ch_low;  [6]=edge_flag;
  typedef std::vector<float> GroupData_t;
  typedef std::vector<GroupData_t> GroupDataList_t;

  // dictionaries for matching (what-to-what?)
  typedef std::vector<short> dict_t;
  typedef std::vector<dict_t> MatchList_t;
  typedef std::vector<dict_t> AmbiguousList_t;
  typedef std::vector<dict_t> SectionEdgeList_t;

  /// makes a group of raw hits from SvxRawhitList
  int  makeGrouphits(RawhitGroupList_t** GroupList, 
                     GroupDataList_t** GroupDataList);

  /// collect raw hits around the passed SvxRawHit
  bool CollectNeighborRawhits(int section, 
                              int readout, 
                              RawhitGroup_t& group, 
                              std::vector<int>& group_wrapup, 
                              SvxRawhit* rhit=0,
                              int edgeflag=0);

  /// make a mosaic pattern of fired tiles
  int makeMosaic(RawhitGroupList_t**   GroupList,
                 GroupDataList_t** GroupDataList,
                 SvxClusterList*       clslist,
                 SvxRawhitClusterList* raw2cls = NULL,
                 SvxGhitRawhitList*    ght2raw = NULL,
                 SvxGhitClusterList*   ght2cls = NULL,
                 svxAddress* address = NULL);

  /// shift the channel after averaging if the averaged channel is out of range
  float shiftChannel(int section, int readout, float channel);



  // test functions
  virtual bool testSingle_CollectNeighborRawhits(int iro, int rawSize, int* rawhitAry, std::vector< std::vector<int> >& vAnswer);
  virtual bool testSingle_makeGrouphits(int iro, int rawSize, int* rawhitAry, int resultSize, float* resultAry);

  /// instumented channel cut-off_set flag (true if cut off structure set)
  static bool cut_off_set_flag; //! omit for ROOT IO, why?

 private:

  float m_sNOISE; ///< the Gaussian width of hit noise (ADC units)
  int m_ADCThresholdZS; ///< threshold for noise hits (ADC units)

  int m_adc_min_rawhit; ///< rawhit ADC minimum for clustering
  int m_adc_min_rawhit_sum; ///< grouped rawhit ADC minimum for cluster

  ClassDef(SvxStrip11v2,1)
};

#endif // __SVXSTRIP11V2_H__
