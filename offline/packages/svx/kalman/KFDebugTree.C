
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <McEvalSingleList.h>

// ROOT
#include <TTree.h>
#include <TFile.h>

#include <string>
#include <iostream>
using namespace std;

#include "KFDebugTree.h"

//-------------------------------------------------------------------
//
KFDebugTree::KFDebugTree(int layers, string outfilename) :
  make_tree_(true),
  nlayers(layers),
  kfmom(boost::extents[layers][3]),
  initmom(boost::extents[layers][3]),
  mcmom(boost::extents[layers][3]),
  mcpid(boost::extents[layers]),
  chisq(boost::extents[layers]),
  mcpos(boost::extents[layers][3]),
  clusterpos(boost::extents[layers][3]),
  state(boost::extents[layers][3][5]),
  covar(boost::extents[layers][3][5][5]),
  statemc(boost::extents[layers][5])
{

  // output stuff to a TTree
  if(outfilename != "") {
    outfile_ = TFile::Open(outfilename.c_str(), "recreate");
    tree_ = new TTree("kftree", "Kalman Fit Tree");
    tree_->SetAutoSave(100);

    char temp[1000];
    sprintf(temp, "kfmom[%d][3]/F", nlayers);
    tree_->Branch("kfmom", kfmom.data(), temp);

    sprintf(temp, "initmom[%d][3]/F", nlayers);
    tree_->Branch("initmom", initmom.data(), temp);

    sprintf(temp, "mcmom[%d][3]/F", nlayers);
    tree_->Branch("mcmom", mcmom.data(), temp);

    tree_->Branch("mcvtxmom", mcvtxmom, "mcvtxmom[3]/F");
    tree_->Branch("mcvtxpos", mcvtxpos, "mcvtxpos[3]/F"); 

    sprintf(temp, "mcpid[%d]/I", nlayers);
    tree_->Branch("mcpid", mcpid.data(), temp);

    tree_->Branch("nsegments", &nsegments, "nsegments/I");
    tree_->Branch("nclusters", &nclusters, "nclusters/I");

    sprintf(temp, "chisq[%d]/F", nlayers);
    tree_->Branch("chisq", chisq.data(), temp);

    tree_->Branch("initcharge", &initcharge, "initcharge/F");
    tree_->Branch("kfcharge", &kfcharge, "kfcharge/F");

    sprintf(temp, "mcpos[%d][3]/F", nlayers);
    tree_->Branch("mcpos", mcpos.data(), temp);

    sprintf(temp, "clusterpos[%d][3]/F", nlayers);
    tree_->Branch("clusterpos", clusterpos.data(), temp);

    sprintf(temp, "state[%d][3][5]/F", nlayers);
    tree_->Branch("state", state.data(), temp);

    sprintf(temp, "covar[%d][3][5][5]/F", nlayers);
    tree_->Branch("covar", covar.data(), temp);

    sprintf(temp, "statemc[%d][5]/F", nlayers);
    tree_->Branch("statemc", statemc.data(), temp);

    tree_->Branch("vtx", vtx, "vtx[3]/F");
    tree_->Branch("pca", pca, "pca[3]/F");

    tree_->Branch("dcmom", &dcmom, "dcmom/F");
    tree_->Branch("dcphi", &dcphi, "dcphi/F");
    tree_->Branch("dctheta", &dctheta, "dctheta/F");
    tree_->Branch("dcalpha", &dcalpha, "dcalpha/F");

    tree_->Branch("simdcmom", &simdcmom, "simdcmom/F");
    tree_->Branch("simdcphi", &simdcphi, "simdcphi/F");
    tree_->Branch("simdctheta", &simdctheta, "simdctheta/F");
    tree_->Branch("simdcalpha", &simdcalpha, "simdcalpha/F");

  } else {
    make_tree_ = false;
  }

  reset();
  return;
}

//-------------------------------------------------------------------
KFDebugTree::~KFDebugTree()
{
  delete outfile_;
}

//-------------------------------------------------------------------
void KFDebugTree::reset()
{
  kfcharge = -9999;
  initcharge = -9999;

  for(int i=0; i<3; i++) {
    for(int j=0; j<nlayers; j++) {
      kfmom[j][i] = -9999;
      initmom[j][i] = -9999;
      mcpos[j][i] = -9999;
      mcmom[j][i] = -9999;
      clusterpos[j][i] = -9999;
    }

    mcpid[i] = -9999;
    mcvtxmom[i] = -9999;
    mcvtxpos[i] = -9999;
    vtx[i] = -9999;
    pca[i] = -9999;
  }

  ndf = 0;
  nsegments = -9999;
  nclusters = -9999;
  for(int ilayer=0; ilayer<nlayers; ilayer++) {

    chisq[ilayer] = -9999;

    for(int i=0; i<5; i++) {
      statemc[ilayer][i] = -9999.;
      
      for(int istep=0; istep<3; istep++) {
        state[ilayer][istep][i] = -9999.;
        for(int j=0; j<5; j++) {
          covar[ilayer][istep][i][j] = -9999;
        }
      }
    }
  }

  dcmom = -9999;
  dcphi = -9999;
  dctheta = -9999;
  dcalpha = -9999;
  simdcmom = -9999;
  simdcphi = -9999;
  simdctheta = -9999;
  simdcalpha = -9999;

  return;
}

//-------------------------------------------------------------------
void KFDebugTree::Finish()
{
  if(make_tree_) {
    cout << "Writing out the tree to " << outfile_->GetName() << endl;
    outfile_->cd();
    tree_->Write();
    outfile_->Close();
  }
  return;
}

//-------------------------------------------------------------------
void KFDebugTree::Fill()
{
  if(make_tree_)
    tree_->Fill();
  return;
}


//-------------------------------------------------------------------
void KFDebugTree::fill_dc_info(const PHSnglCentralTrack *sngl_cnt_trk)
{  
  int idc = nlayers - 1;
  dcmom = sngl_cnt_trk->get_mom();
  dcphi = sngl_cnt_trk->get_phi();
  dctheta = sngl_cnt_trk->get_beta();
  dcalpha = sngl_cnt_trk->get_alpha();
  double zed = sngl_cnt_trk->get_zed();
  
  clusterpos[idc][0] = 220.*cos(dcphi);
  clusterpos[idc][1] = 220.*sin(dcphi);
  clusterpos[idc][2] = zed;
  
  unsigned int imc = _dch_reco_mc_map[sngl_cnt_trk];

  simdcmom = _mc_eval_list->get_momentumr(imc);
  simdcphi = _mc_eval_list->get_phi(imc);
  simdctheta = _mc_eval_list->get_beta(imc);
  simdcalpha = _mc_eval_list->get_alpha(imc);
  zed = _mc_eval_list->get_zed(imc);

  mcpos[idc][0] = 220.*cos(simdcphi);
  mcpos[idc][1] = 220.*sin(simdcphi);
  mcpos[idc][2] = zed;

  mcmom[idc][0] = simdcmom*sin(simdctheta)*cos(simdcphi-simdcalpha);
  mcmom[idc][1] = simdcmom*sin(simdctheta)*sin(simdcphi-simdcalpha);
  mcmom[idc][2] = simdcmom*cos(simdctheta);

  return;  
}
  
//-------------------------------------------------------------------
void KFDebugTree::fill_dc_map(McEvalSingleList* evallist, PHCentralTrack* cnt_trk_list)
{
  _dch_reco_mc_map.clear();
  _mc_eval_list = evallist;

  // loop over all truth particles      
  for(unsigned int itrue=0; itrue<evallist->get_McEvalSingleTrackN(); ++itrue) {
    //unsigned int mctrack = evallist->get_mctrackid(itrue);

    // loop over all the reconstructed particles
    for(int ireco=0; ireco<evallist->get_Nreco(itrue); ireco++) {
      unsigned int recotrack = evallist->get_recoid(itrue,ireco);
      PHSnglCentralTrack* sngl_cnt_trk = cnt_trk_list->get_track(recotrack);
      _dch_reco_mc_map[sngl_cnt_trk] = itrue;
    }
  }

  return;
}
