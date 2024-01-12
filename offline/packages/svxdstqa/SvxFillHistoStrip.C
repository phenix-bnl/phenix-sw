#include <sstream>

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <THmulf.h>

#include "SvxFillHistoStrip.h"

#include <BbcOut.h>
#include <SvxRawhitList.h>
#include <SvxClusterList.h>
#include <SvxRawhitClusterList.h>
#include <SvxSegmentList.h>

#include <SvxRawhit.h>

#include <svxAddress.hh>

using namespace std;

void SvxFillHistoStrip::initialize(TFile *outfile=NULL){
  if(outfile!=NULL) {
    cout<<"File is open"<<endl;
    outfile->cd();
    outfile->pwd();
  }

  m_processed_run = 0; // 0-10; if exceed 10, then show error

  cout<<"SvxFillHistoStrip::initialize making histograms"<<endl;
  // create histogram
  m_h_run = new TH1F("h_stp_run", "Runnumber ", 11, -1, 10);
  m_h_evt = new TH1F("h_stp_evt", "Nevent filled", 2, -1, 1);
  m_h_zvtx = new TH1F("h_stp_zvtx", "BbcZvertex", 300, -150, 150);

  ostringstream ostr, ostitle;
  for(int imod=0; imod<NMODULE; imod++){
    for(int ircc=0; ircc<NRCC; ircc++){
      for(int ichip=0; ichip<NCHIP; ichip++){
        ostr.str(""); ostitle.str("");
        ostr<<"h_stp_adcmap_"<<imod<<"_"<<ircc<<"_"<<ichip;
        ostitle<<"Strip adcmap module:"<<imod<<" rcc:"<<ircc<<" chip:"<<ichip;
        m_h_adcmap[imod][ircc][ichip] = new TH2F(ostr.str().c_str(), ostitle.str().c_str(), 128, -0.5, 127.5, 280, -24.5, 255.5);


      }
      for(int ilr=0; ilr<2; ilr++){
        ostr.str(""); ostitle.str("");
        ostr<<"h_stp_hitmap_"<<imod<<"_"<<ircc<<"_"<<ilr;
        ostitle<<"Strip hitmap module:"<<imod<<" rcc:"<<ircc<<" iLR:"<<ilr;
        
        m_h_hitmap[imod][ircc][ilr] = new TH2F(ostr.str().c_str(), ostitle.str().c_str(), 6, -0.5, 5.5, 128, -0.5, 127.5);
      }
    }
  }

} 

void SvxFillHistoStrip::write(TFile *outfile=NULL){
  if(outfile!=NULL) outfile->cd();


  // create histogram
  m_h_run->Write();
  m_h_evt->Write();
  m_h_zvtx->Write();

  for(int imod=0; imod<NMODULE; imod++){
    for(int ircc=0; ircc<NRCC; ircc++){
      for(int ichip=0; ichip<NCHIP; ichip++){
        m_h_adcmap[imod][ircc][ichip]->Write();
      }
      for(int ilr=0; ilr<2; ilr++){
        m_h_hitmap[imod][ircc][ilr]->Write();
      }
    }
  }
}

void SvxFillHistoStrip::fillInitRun(int runnumber){
  if(m_processed_run<11){
    m_h_run->SetBinContent(m_processed_run+1, runnumber);
    m_processed_run++; // 0-10; if exceed 10, then show error
  }
  else {
    cout<<" ERROR init run called more than 10 : runnum = "<<runnumber<<endl;
  }
}

void SvxFillHistoStrip::fill(SvxEventContainer *cnt){
//  static const float ZVTXCUT      =10.;
  static const float ADCTHRESHOLD =30.;


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
        if(svxsec==0&&(layer>=2)){ // VTX(noFVTX), (STRIP)
          int adc     = raw->get_adc(); // adc already subtracted 24
          if(adc>-24){
            int ladder  = raw->get_ladder();
            int rcc     = raw->get_sensor();
            int section = raw->get_sensorSection(); // L/R = 0/1
            int readout = raw->get_sensorReadout(); // X/U = 0/1
            int channel = raw->get_channel();       // 0-343
            int chip    = m_addr->getStripRoc(section, readout, channel);
            int chipch  = m_addr->getStripRocChannel(section, readout, channel);
            int module  = m_addr->getStripModuleID(layer, ladder);
            int status  = raw->get_HotDeadFlag();       // 0-343

            if((0<=module&&module<NMODULE)&&
               ((layer==2&&0<=rcc&&rcc<(NRCC-1)) || (layer==3&&0<=rcc&&rcc<NRCC) )&&
               (0<=chip&&chip<NCHIP))
            {
              if(!m_checkStatus || status==0){
                m_h_adcmap[module][rcc][chip]->Fill(chipch, adc); // module=0-59, chip:0-7

                if(adc>ADCTHRESHOLD){
                  int ichip;
                  if(3<=chip&&chip<9)       ichip=chip-3;
                  else if(9<=chip&&chip<12) ichip=chip-6;
                  else                      ichip=chip;
                  //cout<<"chip : ichip "<<chip<<" "<<ichip<<endl;

                  m_h_hitmap[module][rcc][section]->Fill(ichip, chipch); // module=0-59, chip:0-7
                }
              }
            }
            else {
              cout<<"outof :"<<layer<<" "<<ladder<<" ";
              cout<<module<<" "<<rcc<<" "<<section<<" "<<readout<<" "<<channel<<" "<<chip<<" "<<chipch;
              cout<<endl;
            }
          }
        }
      }
    }

//  }
}
