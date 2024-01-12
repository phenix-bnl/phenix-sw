#ifndef __MISALIGNSEARCHTREE_H__
#define __MISALIGNSEARCHTREE_H__

#include <SubsysReco.h>
#include <Rtypes.h>

class PHCompositeNode;
class RunHeader;
class BbcOut;
class EventHeader;
class VtxOut;
class SvxRawhitList;
class TrigLvl1;

class TTree;
class TFile;

class MisalignSearchTree: public SubsysReco
{

public:

  MisalignSearchTree() ;
  virtual ~MisalignSearchTree() {}

  int  Init(PHCompositeNode *topNode);
  int  process_event(PHCompositeNode *topNode);
  int  End(PHCompositeNode *topNode);

  int  InitRun(PHCompositeNode *topNode);
  void Print(const std::string &what) const {}
	void Set_outname(const std::string &name) {outname = name;};

private:

  //
  // Run nodes
  //
  RunHeader*       d_runheader;
  //
  // Data nodes for CM and PHENIX
  EventHeader*     d_eventhead;
  //
  // Data node for VTX
  SvxRawhitList*   d_svxraw;
  //
  // Data node for BBC
  BbcOut*          d_bbc;
  //
  // Data node for Trigger
  TrigLvl1*        d_trigl1;

  std::string outname;
	TFile* outfile;
	
  //
  // variables for the tree
  //

	Int_t event;
	UInt_t trigword;
	Float_t bbcq;
	UChar_t hits[30][16];

	TTree* tree;

	void reset_variables();
  int  GetNodes(PHCompositeNode *topNode);
};

#endif
