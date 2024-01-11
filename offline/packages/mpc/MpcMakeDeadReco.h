#ifndef __MPCMAKEDEADRECO_H__
#define __MPCMAKEDEADRECO_H__

//
// This reco reads in a warnmap and zero's out the energy
// of all towers in the warnmap
//
#include <SubsysReco.h>
#include <string>

class PHCompositeNode;
class MpcMap;
class MpcCalib;

class MpcMakeDeadReco: public SubsysReco
{
public:
  MpcMakeDeadReco(const std::string &name = "MPCMAKEDEADRECO");
  virtual ~MpcMakeDeadReco() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

  void SetDeadMap(const char *f) { _deadmapfname = f; }

protected:

  MpcMap   *mpcmap;
  //int _deadmap[576];
  int _deadmap[2][18][18];
  std::string _deadmapfname;
  
};

#endif /* __MPCMAKEDEADRECO_H__ */

