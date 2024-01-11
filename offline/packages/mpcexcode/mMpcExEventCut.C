#include <mMpcExEventCut.h>
#include <iostream>
#include <Fun4AllReturnCodes.h>
#include <PHCompositeNode.h>
#include <TriggerHelper.h>

mMpcExEventCut::mMpcExEventCut() : SubsysReco("MMPCEXEVENTCUT") {
  _triggerNames.push_back("BBCLL1(>0 tubes) novertex");
  _triggerNames.push_back("BBCLL1(>0 tubes) narrowvtx");
  _triggerNames.push_back("BBCLL1(>0 tubes)_central_narrowvtx");
  _triggerNames.push_back("MPC_N_A");
  _triggerNames.push_back("MPC_N_B");
  _updatedTriggers = false;
}

int mMpcExEventCut::process_event(PHCompositeNode *topNode) {

  static int ncalls = 0;
  if(ncalls == 0 && !_updatedTriggers){  
    std::cout<<"*******************************************************"<<std::endl;
    std::cout<<"You are running the default MPC-EX Event Cut  "<<std::endl;
    std::cout<<"module. The following are the only triggers   "<<std::endl;
    std::cout<<"That will be analyzed."<<std::endl;
    for(unsigned int it=0; it<_triggerNames.size(); it++){
      std::cout<<"\t"<<_triggerNames[it]<<std::endl;
    }
    std::cout<<"To run with a different set of triggers call"<<std::endl;
    std::cout<<"SetTriggerNames(const std::vector<std::string>& names)"<<std::endl;
    std::cout<<"*******************************************************"<<std::endl;
  }
  ncalls++;

  TriggerHelper *myTH = new TriggerHelper(topNode);
  unsigned int n = _triggerNames.size();
  unsigned int sum = 0;
  for(unsigned int it=0; it<n; it++){
    sum += myTH->trigScaled(_triggerNames[it].c_str());
  }
  delete myTH;

  if(sum==0)
    return ABORTEVENT;
  
  return EVENT_OK;
}
