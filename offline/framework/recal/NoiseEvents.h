#ifndef __NOISEEVENTS_H__
#define __NOISEEVENTS_H__
#include <Recalibrator.h>

#include <getClass.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <RunHeader.h>
#include <PreviousEventv1.h>

#include <phool.h>
#include <Fun4AllReturnCodes.h>
#include <RunNumberRanges.h>

typedef PHIODataNode <PHObject>       PHObjectNode_t;
class PHCompositeNode;

class NoiseEvents: public Recalibrator
{
 public:
  NoiseEvents(const std::string &name = "NoiseEvents");
  virtual ~NoiseEvents() {}

  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;

 protected:
  PreviousEvent *prevevt;

};

#endif /* __NOISEEVENTS_H__ */
