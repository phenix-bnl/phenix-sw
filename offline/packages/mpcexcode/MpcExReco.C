#include <MpcExReco.h>
#include <mMpcExCreateNodeTree.h>
#include <mMpcExLoadCalibrations.h>
#include <mMpcExEventCut.h>
#include <mMpcExApplyCalibrations.h>
#include <mMpcExShower.h>
#include <mMpcExShowerCleaner.h>

MpcExReco::MpcExReco() : SubsysRecoStack("MPCEXRECO") {
  //  ShutUp = true;
  x_push_back(new mMpcExCreateNodeTree());
  x_push_back(new mMpcExLoadCalibrations());
  _eventCut = new mMpcExEventCut();
  x_push_back(_eventCut);
  x_push_back(new mMpcExApplyCalibrations());
  mMpcExShower *shower_reco = new mMpcExShower("MPCEXSHOWER");
  shower_reco->DisableSTReco(); 
  shower_reco->DisableSouthReco(); 
  x_push_back(shower_reco);
  x_push_back(new mMpcExShowerCleaner());
}

MpcExReco::~MpcExReco(){
  while(!empty()){
    delete back();
    pop_back();
  }
}

void MpcExReco::SetTriggerNames(const std::vector<std::string>& names){
  _eventCut->SetTriggerNames(names);
}
