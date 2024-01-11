#ifndef __HBDSIMRECO_H__
#define __HBDSIMRECO_H__

#include <SubsysReco.h>

class hbdghitWrapper;
class hbdDetectorGeo;

#include <HbdFinalSimulator.h>
#include <HbdWisClusterizer.h>

// forward declaration
class PHCompositeNode;

class HbdSimreco: public SubsysReco
{
 public:
  
  //! constructor
  HbdSimreco(const char *name = "HBD");
  
  //! destructor
  virtual ~HbdSimreco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

 protected:
  
  //!@name modules
  /*! 
  direct reference of the objects is used in place
  of pointer so that they are created in parent object constructor
  and deleted in parent object destructor
  */
  //@{
  HbdFinalSimulator HbdSlowSim;
  HbdWisClusterizer HbdCluster;
  //@}
  
  //! hbd geant hits. 
  /*! A pointer is used because it gets written to the node tree */
  hbdghitWrapper *hbdghit;

  //! hbd geometry 
  /*! A pointer is used because it gets written to the node tree */
  hbdDetectorGeo *d_hbdgeo;

};

#endif /* __HBDSIMRECO_H__ */
