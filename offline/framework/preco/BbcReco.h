#ifndef __BBCRECO_H__
#define __BBCRECO_H__

// $Id: BbcReco.h,v 1.7 2008/11/25 16:25:41 hpereira Exp $

#include <SubsysReco.h>

class BbcOut;
class PHCompositeNode;
class mBbcUnpackModule;
class mBbcRawOutModule;

class BbcReco: public SubsysReco
{
 public:
  
  //! named constructor
  BbcReco(const char *name = "BBC");
  
  //! destructor
  virtual ~BbcReco();

  //! full init
  int Init(PHCompositeNode *topNode);
  
  //! run-wise init
  int InitRun(PHCompositeNode *topNode);
  
  //! event method
  int process_event(PHCompositeNode *topNode);
  
  //! reset
  int ResetEvent(PHCompositeNode *topNode);

  //! set vertex z error
  void setBbcVtxError( const double& value )
  { BbcVtxError = value; }
  
 protected:
  
  int CreateNodeTree(PHCompositeNode *topNode);
  
  //! unpacker module
  mBbcUnpackModule *mBbcUnpack;
  
  //! 'reconstruction' module
  mBbcRawOutModule *mBbcRawOut;
  
  //! Bbc out node
  BbcOut* bbcout;
    
  //! vertex z error
  double BbcVtxError;
    
};

#endif /* __BBCRECO_H__ */
