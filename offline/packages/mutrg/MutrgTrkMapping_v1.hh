#ifndef __MUTRGTRKMAPPING_V1__
#define __MUTRGTRKMAPPING_V1__

#include <vector>
#include <string>
#include <set>
#include <iostream>

#include "MutrgPar.hh"
#include "PHTimeStamp.h"

class MutrgTrkMapping_v1{
public:
  MutrgTrkMapping_v1(void);
  virtual ~MutrgTrkMapping_v1(void);
  int Reset(void);
  int SetMapFile(const char *filename);
  int SetMapDB(int run=-9999,
	       MutrgPar::TrkDBVersion ver=MutrgPar::TRKDV_RUN_MUTRG_RPC_0);
  int SetMapDB(PHTimeStamp ts,
	       MutrgPar::TrkDBVersion ver=MutrgPar::TRKDV_RUN_MUTRG_RPC_0);

  // strip is strip_mutr + hoct*nstrip_in_hoct0;
  int FindFromSt2(int arm,int octant,int strip_st2,
		  std::vector<unsigned short> &strips_st0,
		  std::vector<unsigned short> &strips_st1);
  int Print(std::ostream &os=std::cout);

protected:
  std::string class_name;

  // pair<strip_num_st0, strip_num_st1>
  // expect track finding starts from station=2
  std::set<std::pair<unsigned short,unsigned short> >
  mapping[MutrgPar::NARM][MutrgPar::NOCTANT][MutrgPar::MAX_NSTRIP_IN_OCTANT];
};

#endif /* __MUTRGTRKMAPPING_V1__ */
