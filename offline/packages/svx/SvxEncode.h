// ===============
// FILE: SvxEncode.h
// ===============
#ifndef __SVXENCODE_H__
#define __SVXENCODE_H__

#include "SvxParameters.h"
#include "svxAddress.hh"

#include <SubsysReco.h>

#include <phool.h>


#include <iostream> 
class PHCompositeNode      ;
class PHRawDataNode        ;
class SvxRawhitList        ;
class SvxPacketList        ;
class Svx_stripixelPacketList ;


/**
 * @brief  A SubsysReco module to convert Raw Hits into PRDF format.
 * @date  Created by Sasha Lebedev in June 2010
 */
class SvxEncode : public SubsysReco
{

 public:

  SvxEncode(const std::string &name = "SVXENCODE");
  virtual ~SvxEncode() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected: 
  SvxRawhitList        *d_rawhit;         // SVX raw hits
  SvxPacketList        *d_event;          // SVX packets
  Svx_stripixelPacketList  * d_event_stripixel;  //SVX strip packets

  int CreateNodeTree(PHCompositeNode *topNode);

  int nSvxRawhits;        ///< Number of SvxRawhit objects filled
  unsigned int EventNumber;

  // hardware/software map
  //svxAddress SvxAddressObject;

 private:
  int convertHitToPRDFPixel(svxAddress& SvxAddressObject);
  int convertHitToPRDFStripixel(svxAddress& SvxAddressObject); 
  int copyToSIMPRDF(PHCompositeNode *topNode);
  int copyToSIMPRDF_stripixel(PHCompositeNode *topNode);

  bool comparePRDFWithRawHit(PHCompositeNode *topNode);

 private:
  PHRawDataNode *m_rawDataArray[60];
  PHRawDataNode *m_rawDataArray_stripixel[40];
};
#endif 


