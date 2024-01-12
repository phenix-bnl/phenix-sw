
#ifndef __ACCSIMRECO_H__
#define __ACCSIMRECO_H__

#include <SubsysReco.h>
#include <string>

#include <phool.h>
#include <AccCalib.h>
#include <AccEvent.h>

class AccRaw;
class AccHit;
class AerGeaHits;
class AccGeometry;

class PHCompositeNode;

class AccSimreco: public SubsysReco
{
  public:
  
  //! constructor
  AccSimreco(const std::string &name = "ACC");
  
  //! destructor
  virtual ~AccSimreco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  
  //!@name modules
  /*! 
  direct reference of the objects is used in place
  of pointer so that they are created in parent object constructor
  and deleted in parent object destructor
  */
  //@{
  AccEvent d_acc;
  AccCalib d_calib;
  //@}

  //! aerogel geometry
  /*! A pointer is used because it gets written to the node tree */
  AccGeometry *d_geo;

  //! aerogel raw hits
  /*! A pointer is used because it gets written to the node tree */
  AccRaw      *d_raw;

  //! aerogel hits
  /*! A pointer is used because it gets written to the node tree */
  AccHit      *d_hit;

  //! aerogel geant hits
  /*! A pointer is used because it gets written to the node tree */
  AerGeaHits  *d_gea;
  
};

#endif /* __ACCSIMRECO_H__ */
