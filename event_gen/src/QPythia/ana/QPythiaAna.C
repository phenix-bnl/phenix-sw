#include <QPythiaAna.h>
#include <PHCompositeNode.h>
#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <PHPyJetContainer.h>
#include <PHPyJet.h>
#include <PHPythiaContainer.h>
#include <TMCParticle.h>
#include <TFile.h>
#include <TTree.h>
#include <TLorentzVector.h>
#include <TMath.h>
#include <iostream>
#include <cmath>

QPythiaAna::QPythiaAna(const std::string& jetNodeName, const std::string& outfilename) : _jetNodeName(jetNodeName), _outFileName(outfilename) {
  _outputFile = 0;
  _outputTree = 0;

  _jetpt = 0.0;
  _jeteta = 0.0;
  _jetphi = 0.0;
  _jetm = 0.0;
  _nconst = 0;
  for(int i=0; i<NMAX; i++){
    _constpt[i] = 0.0;
    _consteta[i] = 0.0;
    _constphi[i] = 0.0;
    _constm[i] = 0.0;
    _constR[i] = 0.0;
    _constjt[i] = 0.0;
    _constz[i] = 0.0;
  }

}

QPythiaAna::~QPythiaAna(){
  delete _outputFile;
}

int QPythiaAna::Init(PHCompositeNode *topNode){

  _outputFile = new TFile(_outFileName.c_str(),"RECREATE");
  _outputTree = new TTree("qjets","qjets");

  _outputTree->Branch("jetpt",&_jetpt,"jetpt/F");
  _outputTree->Branch("jeteta",&_jeteta,"jeteta/F");
  _outputTree->Branch("jetphi",&_jetphi,"jetphi/F");
  _outputTree->Branch("jetm",&_jetm,"jetm/F");
  _outputTree->Branch("nconst",&_nconst,"nconst/I");
  _outputTree->Branch("constpt",_constpt,"constpt[nconst]/F");
  _outputTree->Branch("consteta",_consteta,"consteta[nconst]/F");
  _outputTree->Branch("constphi",_constphi,"constphi[nconst]/F");
  _outputTree->Branch("constm",_constm,"constm[nconst]/F");
  _outputTree->Branch("constpid",_constpid,"constpid[nconst]/I");
  _outputTree->Branch("constR",_constR,"constR[nconst]/F");
  _outputTree->Branch("constjt",_constjt,"constjt[nconst]/F");
  _outputTree->Branch("constz",_constz,"constz[nconst]/F");

  return EVENT_OK;
}

int QPythiaAna::process_event(PHCompositeNode *topNode){

  PHPyJetContainer *jets = findNode::getClass<PHPyJetContainer>(topNode,_jetNodeName.c_str());
  if(!jets){
    std::cout<<PHWHERE<<"Could not find "<<_jetNodeName<<" jet node"<<std::endl;
    return DISCARDEVENT;
  }

  PHPythiaContainer *particles = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if(!jets){
    std::cout<<PHWHERE<<"Could not find PHPythia particle node"<<std::endl;
    return DISCARDEVENT;
  }

  unsigned int njets = jets->size();
  for(unsigned int ijet=0; ijet<njets; ijet++){
    PHPyJet *jet = jets->getJet(ijet);
    _jetpt = jet->Pt();
    _jeteta = jet->Eta();
    _jetphi = jet->Phi();
    _jetm = jet->M();

    unsigned int nconst = jet->size();
    _nconst = 0;
    for(unsigned int iconst=0; iconst<nconst; iconst++){

      int partIndex = jet->getConstituentIndex(iconst);
      TMCParticle *constituent = particles->getParticle(partIndex);
      if(!constituent){
	std::cout<<"Could not find constituent #"<<iconst<<" which is #"<<partIndex<<" out of "<<particles->size()<<std::endl;
	continue;
      }

      TLorentzVector kin(constituent->GetPx(),constituent->GetPy(),constituent->GetPz(),constituent->GetEnergy());
      _constpt[_nconst] = kin.Pt();
      _consteta[_nconst] = kin.Eta();
      _constphi[_nconst] = kin.Phi();
      _constm[_nconst] = kin.M();
      _constpid[_nconst] = constituent->GetKF();

      float deta = _jeteta - _consteta[_nconst];
      float dphi = _jetphi - _constphi[_nconst];
      if(dphi>TMath::Pi()) dphi -= 2.*TMath::Pi();
      if(dphi<-TMath::Pi()) dphi += 2.*TMath::Pi();
      _constR[_nconst] = std::sqrt(deta*deta+dphi*dphi);
      _constjt[_nconst] = _constpt[_nconst]*std::sin(_constR[_nconst]);
      _constz[_nconst] = _constpt[_nconst]*std::cos(_constR[_nconst])/_jetpt;

      _nconst++;

    }

    _outputTree->Fill();
  }

  return EVENT_OK;
}

int QPythiaAna::End(PHCompositeNode *topNode){
  _outputFile->Write();
  _outputFile->Close();

  return EVENT_OK;
}
