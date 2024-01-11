#ifndef __SvxConvertPixelRawHitList_H__
#define __SvxConvertPixelRawHitList_H__


#include "SubsysReco.h"

class PHCompositeNode;
class SvxRawhitList;
class svxAddress;
class SvxPixelRawHitList;

/**
 * Convert SvxPixelRawHitList to SvxRawHits and add them to the
 * SvxRawHitList node
 *
 * This class was designed to be used in the VTXP event re-alignment
 * effort. It adds the realigned VTX pixel hit information from
 * SvxPixelRawHitList to the SvxRawHitList node.
 *
 * @author Darren McGlinchey
 * @email darren.mcglinchey@colorado.edu
 * @date 9 Sep 2016
 */
class SvxConvertPixelRawHitList: public SubsysReco
{

public:

  SvxConvertPixelRawHitList();
  virtual ~SvxConvertPixelRawHitList() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  void Verbosity(const int ival){ verbosity=ival; }

protected:

  //--nodes
  SvxRawhitList *d_rawhitlist;
  SvxPixelRawHitList *d_pixelrawhitlist;

  svxAddress *d_address;


  //--functions
  int CreateNodes(PHCompositeNode *topNode);
  int GetNodes(PHCompositeNode *topNode);


  //--variables
  int verbosity;
};



#endif //__SvxConvertPixelRawHitList_H__
