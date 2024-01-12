/*-------------------------
SvxFillHistoBeamrpo.C

author: Hidemitsu Asano 
code to calculate beam center and beam profile by using primary vertex distribution
----------------------------*/
#include <sstream>
#include <cmath>

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>

#include "SvxFillHistoBeampro.h"

#include <BbcOut.h>
#include <VtxOut.h>


#include <svxAddress.hh>
#include <PHPoint.h>

using namespace std;

void SvxFillHistoBeampro::initialize(TFile *outfile=NULL){
  if(outfile!=NULL) {
    cout<<"File is open"<<endl;
    outfile->cd();
    outfile->pwd();
  }


  cout<<"SvxFillHistoBeampro::initialize making histograms"<<endl;
  // create histogram
//  m_h_run  = new TH1F("h_pxl_run", "Runnumber ", 11, -1, 10);
//  m_h_evt  = new TH1F("h_pxl_evt", "Nevent filled", 2, -1, 1);
//  m_h_zvtx = new TH1F("h_pxl_zvtx", "Zvertex by BBC", 300, -150, 150);

  m_h_primx = new TH1F("h_primary_x","Primary x vertex",20000,-1.5,1.5); 
  m_h_primy = new TH1F("h_primary_y","Primary y vertex",20000,-1.5,1.5); 
//  m_h_primz = new TH1F("h_primary_z","Primary z vertex",40000,-20,20); 

//  m_h_primxy2d = new TH2F("h_primary_xy2d","Primary x and y plot",1000,-1.5,1.5,1000,-1.5,1.5);
//  m_h_primxz2d = new TH2F("h_primary_xz2d","Primary x and z plot",1000,-1.0,1.0,1000,-10.0,10.0);
//  m_h_primyz2d = new TH2F("h_primary_yz2d","Primary y and z plot",1000,-1.0,1.0,1000,-10.0,10.0);
} 

void SvxFillHistoBeampro::write(TFile *outfile=NULL){
  if(outfile!=NULL) outfile->cd();


  // create histogram
  m_h_primx->Write();
  m_h_primy->Write();
//  m_h_primz->Write();

//  m_h_primxy2d->Write();
}

void SvxFillHistoBeampro::fillInitRun(int runnumber){
}

void SvxFillHistoBeampro::fill(SvxEventContainer *cnt){
  static const float ZVTXCUT = 10.0;

  if(cnt->m_vtxout==NULL){
    return ;
  }

  if(fabs(cnt->m_bbc->get_VertexPoint())< ZVTXCUT){ // vertex selection
    PHPoint verpointsvxprim = cnt->m_vtxout->get_Vertex("SVX_PRECISE");
  //  PHPoint verpoint_west = m_vtxout->get_Vertex("SVX_PRECISEW");
  //  PHPoint verpoint_east = m_vtxout->get_Vertex("SVX_PRECISEE");
    float svxprim[3];
    svxprim[0] = verpointsvxprim.getX();
    svxprim[1] = verpointsvxprim.getY();
//    svxprim[2] = verpointsvxprim.getZ();

    m_h_primx->Fill(svxprim[0]);
    m_h_primy->Fill(svxprim[1]);
//    m_h_primz->Fill(svxprim[2]);
    
//    m_h_primxy2d->Fill(svxprim[0],svxprim[1]);
//    m_h_primxz2d->Fill(svxprim[0],svxprim[2]);
//    m_h_primyz2d->Fill(svxprim[1],svxprim[2]);

  }
}
