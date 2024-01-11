// ===============
// FILE: SvxDecode.h
// ===============
#ifndef SVXDECODE_H__
#define SVXDECODE_H__

#include "SvxParameters.h"

#include <SubsysReco.h>

#include <phool.h>
#include <PHTimeServer.h>


#include <iostream>

class PHCompositeNode;
class SvxRawhitList;
class SvxEventInfo;
class SvxPacketList;

/**
 * @brief  A SubsysReco module to read PRDF files and create raw hits.
 * @date  Created by Sasha Lebedev in June 2010
 *
 * 2011.01.14 T.Hachiya: split the process_event and add the functions 
 *                       (fillRawhitFromSimPacket(), fillRawhitFromPRDF(), isRealData, setRealData)
 *                       and data member (m_isRealData)
 */
class SvxDecode : public SubsysReco
{

 public:

  SvxDecode(const std::string &name = "SVXDECODE");
  virtual ~SvxDecode() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);


  //////////////////
  void setIsDummyData(const bool isdummy=true) { m_isDummyData=isdummy;} // added by TH. 2011.01.14
  bool isDummyData() const { return m_isDummyData;}                // added by TH. 2011.01.14

  /// this is temporary
  void setAdcOffset(const int offset) { m_adc_offset = offset; }
  int  getAdcOffset() const { return m_adc_offset; }
  void setAdcCutoff(const int cutoff) { m_adc_cutoff = cutoff; }
  int  getAdcCutoff() const { return m_adc_cutoff; }
  /// this is temporary

  void includePixel(const bool on)    { m_includePixel = on;};
  void includeStripixel(const bool on){ m_includeStripixel = on;};
  void setKeepHotDeadHits(const bool keep) { m_keepHotDeadHits = keep; }
  void setCheckPacketError(const bool flag) { m_checkPacketError = flag; }
                                                                 

 protected:
  int fillRawhitFromSimPacket(PHCompositeNode *topNode); // added by TH. 2011.01.14
  int fillRawhitFromPRDF(PHCompositeNode *topNode);      // added by TH. 2011.01.14
  int fillRawhitFromPRDF_stripixel(PHCompositeNode *topNode); // added by Yi Gu 01/31/2011
  int CreateNodeTree(PHCompositeNode *topNode);

  SvxRawhitList        *d_rawhit;         // SVX raw hits
  SvxEventInfo         *d_eventinfo;      // SVX EventInfo
  SvxPacketList        *d_event;          // SVX packets

  int nSvxRawhits;        ///< Number of SvxRawhit objects filled
  unsigned int EventNumber;
  
 private:
  bool m_isDummyData; // true: read from Event(PRDF), false: read from Simdata 
                      // This flag is used in process_event. added by TH. 2011.01.14
  
  int m_adc_offset; // temporary, this is pedestal instead
  int m_adc_cutoff; // temporary, cutoff value for strip adc

  bool m_includePixel;    // temporary added to include/exclude pixel in the process_event
  bool m_includeStripixel;// temporary added to include/exclude stripixel in the process_event
  bool m_keepHotDeadHits; //true: keeps rawhits in hot dead channels, adds the associated flag. false: does not add hot/dead rawhits to rawhit list.

  bool m_checkPacketError; // true: checking packeterror 

  PHTimeServer::timer _timer;   ///< Timer
};
#endif 
