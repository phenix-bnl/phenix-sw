#include <sstream>

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
//#include <THmulf.h>
#include <TMath.h>

#include "SvxFillHistoPixel.h"

#include <BbcOut.h>
#include <SvxRawhitList.h>
#include <SvxRawhit.h>


#include <svxAddress.hh>

using namespace std;

void SvxFillHistoPixel::initialize(TFile *outfile=NULL){
  if(outfile!=NULL) {
    cout<<"File is open"<<endl;
    outfile->cd();
    outfile->pwd();
  }

  m_processed_run = 0; // 0-10; if exceed 10, then show error

  cout<<"SvxFillHistoPixel::initialize making histograms"<<endl;
  // create histogram
  m_h_run  = new TH1F("h_pxl_run", "Runnumber ", 11, -1, 10);
  m_h_evt  = new TH1F("h_pxl_evt", "Nevent filled", 2, -1, 1);
  m_h_zvtx = new TH1F("h_pxl_zvtx", "Zvertex by BBC", 300, -150, 150);

  for(int imod=0; imod<NMODULE; imod++){
    for(int ichip=0; ichip<NCHIP; ichip++){
      ostringstream ostr, ostitle;
      ostr<<"h_pxl_hitmap_"<<imod<<"_"<<ichip;
      ostitle<<"Pixel hitmap module:"<<imod<<" chip:"<<ichip;
      //cout<<ostr.str()<<" "<<ostitle.str()<<endl;
      
      m_h_hitmap[imod][ichip] = new TH2F(ostr.str().c_str(), ostitle.str().c_str(), 32, -0.5, 31.5, 256, -0.5, 255.5);

/*
      ostr.str(""); ostitle.str("");
      ostr<<"hm_pxl_hitmap_"<<imod<<"_"<<ichip;
      ostitle<<"thmul Pixel hitmap module:"<<imod<<" chip:"<<ichip;
      m_hm_hitmap[imod][ichip] = new THmulf(ostr.str().c_str(), ostitle.str().c_str());// module=0-59, chip:0-7
      m_hm_hitmap[imod][ichip]->AddAxis("row",   "row",       256,   -0.5, 255.5);
      m_hm_hitmap[imod][ichip]->AddAxis("col",   "column",     32,   -0.5,  31.5);
      //hm_hitmap[imod][ichip]->AddAxis("strg",  "strg",       32,   -0.5,  31.5);
      m_hm_hitmap[imod][ichip]->AddAxis("vtx",   "bbcvtx",    100, -100.0, 100.0);
*/
    }
  }
} 

void SvxFillHistoPixel::write(TFile *outfile=NULL){
  if(outfile!=NULL) outfile->cd();


  // create histogram
  m_h_run->Write();
  m_h_evt->Write();
  m_h_zvtx->Write();


  for(int imod=0; imod<NMODULE; imod++){
    for(int ichip=0; ichip<NCHIP; ichip++){
      m_h_hitmap[imod][ichip]->Write();
//      m_hm_hitmap[imod][ichip]->Write();
    }
  }
}

void SvxFillHistoPixel::fillInitRun(int runnumber){
  if(m_processed_run<11){
    m_h_run->SetBinContent(m_processed_run+1, runnumber);
    m_processed_run++; // 0-10; if exceed 10, then show error
  }
  else {
    cout<<" ERROR init run called more than 10 : runnum = "<<runnumber<<endl;
  }
}

//void SvxFillHistoPixel::fill(SvxRawhitList *rawlist, BbcOut *bbc){
void SvxFillHistoPixel::fill(SvxEventContainer *cnt){
//  static const float ZVTXCUT = 10.0;

  if(cnt->m_rawlist==NULL){
    return ;
  }

  ////////////////
  m_h_zvtx->Fill(cnt->m_bbc->get_VertexPoint());


//  if(TMath::Abs(cnt->m_bbc->get_VertexPoint())< ZVTXCUT){ // vertex selection
    ////////////////
    
    m_h_evt->Fill(0);
    
    int nrawhit = cnt->m_rawlist->get_nRawhits();
    //cout<<"    Nraw : "<<nrawhit<<endl;
    for(int iraw=0; iraw<nrawhit; iraw++){
      SvxRawhit *raw = cnt->m_rawlist->get_Rawhit(iraw);
      if(raw){
        int svxsec  = raw->get_svxSection();
        //int type    = raw->get_sensorType();
        int layer   = raw->get_layer();
        if(svxsec==0&&(layer<2)){ // VTX(noFVTX), type==1(PIXEL)
          //int ladder  = raw->get_ladder();
          //int sensor  = raw->get_sensor();
          int adc     = raw->get_adc();
          if(adc>0){
            int channel = raw->get_channel();
            int module  = raw->get_pixelModule();
            int roc     = raw->get_pixelROC();
            int row = m_addr->getPixelRocIX0(channel);
            int col = m_addr->getPixelRocIZ0(channel);
            int status = raw->get_HotDeadFlag();
            if(row>=0&&col>=0&&(0<=module&&module<NMODULE)&&(0<=roc&&roc<NCHIP)){
              if(!m_checkStatus|| status==0){
                m_h_hitmap[module][roc]->Fill(col, row); // module=0-59, chip:0-7
              }
              //m_hm_hitmap[module][roc]->Fill(1.0, row, col, cnt->m_bbc->get_VertexPoint()); // module=0-59, chip:0-7
            }
            else {
              cout<<"outof :"<<svxsec<<" "<<layer<<" ";
              cout<<module<<" "<<roc<<" "<<channel<<" "<<row<<" "<<col;
              cout<<endl;
            }
          }
        }
      }
    }
//  }
}
