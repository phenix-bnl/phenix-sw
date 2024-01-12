#ifndef PHCGLLISTRECALRECO_H__
#define PHCGLLISTRECALRECO_H__

#include "Recalibrator.h"

#include <string>
#include <vector>

class PHCentralTrack;
class PhCglListv4_Run7a;

class PhCglListRecalReco : public Recalibrator 
{
  
 public:
  PhCglListRecalReco(const std::string &name= "PhCglListRecalReco");
  virtual ~PhCglListRecalReco() {}

  int Init(PHCompositeNode *topNode); // called during intialization
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;



 protected:

  //function that creates the node tree for the output
  bool CreateNodeTree(PHCompositeNode *topNode);
  int  combine_v4_Run7a(PHCompositeNode *topNode);

  //data nodes to write out
  PHCentralTrack        *recal_cgl;
  int phcgllist_version;

  bool phcgllist_active;

};

#endif  /* PHCGLLISTRECALRECO_H__ */
