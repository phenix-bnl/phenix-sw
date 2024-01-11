#ifndef __SvxConvertStripRawHitList_H__
#define __SvxConvertStripRawHitList_H__


#include "SubsysReco.h"

class PHCompositeNode;
class SvxRawhitList;
class SvxRawhit;
class svxAddress;
class SvxStripRawHitList;

/**
 * Convert SvxStripRawHitList to SvxRawHits and add them to the
 * SvxRawHitList node
 *
 * This class is used to add the VTX strip hit information from
 * SvxStripRawHitList to the SvxRawHitList node.
 *
 * @author Takashi Hachiya
 * @email  hachiya@rcf.rhic.bnl.gov
 * @date 22 May 2017
 */
class SvxConvertStripRawHitList: public SubsysReco
{

public:

  SvxConvertStripRawHitList();
  virtual ~SvxConvertStripRawHitList() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  void Verbosity(const int ival){ verbosity=ival; }

  void setAdcOffset(const int offset){ d_offset = offset; }

  void setTestMode(bool set) { d_testmode = set; }

protected:

  //--nodes
  SvxRawhitList *d_rawhitlist;
  SvxStripRawHitList *d_striprawhitlist;

  svxAddress *d_address;

  int         d_offset;

  bool        d_testmode;


  //--functions
  int CreateNodes(PHCompositeNode *topNode);
  int GetNodes(PHCompositeNode *topNode);

  bool compareRawhitList(SvxRawhitList* newlist, SvxRawhitList* orglist);
  bool compareRawhit(SvxRawhit* newhit, SvxRawhit* orghit);


  //--variables
  int verbosity;
};



#endif //__SvxConvertStripRawHitList_H__
