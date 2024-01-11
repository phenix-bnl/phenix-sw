#include "HbdSimpleMonitor.h"

#include <TROOT.h>
#include <stdlib.h>
#include <iostream>
#include <stdio.h>
#include <string.h>
#include <fstream>
#include <TFile.h>

#include "HbdCell.h"
#include "HbdCellList.h"
#include "HbdBlob.h"
#include "HbdBlobList.h"
#include "HbdClusterizer.h"
#include "HbdDcmRaw.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "Event.h"
#include "PHCompositeNode.h"

HbdSimpleMonitor::HbdSimpleMonitor(char *outfilename)
{
  // geometry Info
  dcmraw = new HbdDcmRaw();
  clus = new HbdClusterizer();

  strcpy(outfile,outfilename); 

  char title[200];

  for(int i=0;i<96;i++){
     for(int j=0;j<12;j++){
        sprintf(title,"ADCCh%d_Samp%d",i,j);
        ADChist[i][j] = new TH1F(title,title,5120,-1024,4096);
        sprintf(title,"ADCCalCh%d_Samp%d",i,j);
        ADCCalhist[i][j] = new TH1F(title,title,5120,-1024,4096);
     }
  }

  nt = new TNtuple("nt","nt","evno:ch:adc0:adc1:adc2:adc3:adc4:"
                             "adc5:adc6:adc7:adc8:adc9:adc10:adc11");

  // Number of hits per pad
  PadHits = new TH1F("PadHits","PadHits",5000,0,5000);

  // Z-phi coordinate center of cone distribution.
  ConeHits = new TH2F("ConeHits","ConeHits",628,-1.57,4.71,200,-50,50);

  // Number of photo electrons per cone
  for(int i=0;i<40;i++){
     sprintf(title,"PEInConeAll%d",i);
     PEInConeAll[i] = new TH1F(title,title,200,-10,40);
     sprintf(title,"PEInConeSingle%d",i);
     PEInConeSingle[i] = new TH1F(title,title,200,-10,40);
     sprintf(title,"PEInConeMerged%d",i);
     PEInConeMerged[i] = new TH1F(title,title,200,-10,40);
  }

  // Number of hit pads per cone
  for(int i=0;i<40;i++){
     sprintf(title,"nHitsInConeAll%d",i);
     nHitsInConeAll[i] = new TH1F(title,title,200,-10,40);
     sprintf(title,"nHitsInConeSingle%d",i);
     nHitsInConeSingle[i] = new TH1F(title,title,200,-10,40);
     sprintf(title,"nHitsInConeMerged%d",i);
     nHitsInConeMerged[i] = new TH1F(title,title,200,-10,40);
  }

  date_first_flag=1;
  evtcounter=0;

  maxevt = 1000000;
  // End of initialization function
}


int HbdSimpleMonitor::process_event(Event* evt)
{
  HbdCellList *celllist = new HbdCellList();
  dcmraw->GetEvent(evt,celllist);

  HbdBlobList *bloblist = new HbdBlobList();
  clus->Clusterizer(celllist,bloblist);

  int nblob = bloblist->get_nBlobs();
  for(int i=0;i<nblob;i++){
     HbdBlob *hbdblob = bloblist->get_blob(i);
     float posx = hbdblob->get_xyzPosition(0);
     float posy = hbdblob->get_xyzPosition(1);
     float posz = hbdblob->get_xyzPosition(2);
     PadHits->Fill(posz,atan2(posy,posx));

     int ptr=0;
     int depth=hbdblob->get_size();
     nHitsInConeAll[ptr]->Fill(hbdblob->get_size());
     PEInConeAll[ptr]->Fill(hbdblob->get_charge());
     if(depth>1){
        nHitsInConeMerged[ptr]->Fill(hbdblob->get_size());
        PEInConeMerged[ptr]->Fill(hbdblob->get_charge());
     }
     else{
        nHitsInConeSingle[ptr]->Fill(hbdblob->get_size());
        PEInConeSingle[ptr]->Fill(hbdblob->get_charge());
     }
  }

  for(int i=0;i<nblob;i++) delete bloblist->get_blob(i);
  delete bloblist;

  int ncell = celllist->get_nCells();
  for(int i=0;i<ncell;i++) delete celllist->get_cell(i);
  delete celllist;
  
  evtcounter++;
  if(evtcounter>=maxevt) return 1;

  return 0;
}


int HbdSimpleMonitor::EndRun()
{
   TFile *fout = new TFile(outfile,"RECREATE");

   nt->Write();

   for(int i=0;i<96;i++){
      for(int j=0;j<12;j++){
         ADChist[i][j]->Write();
         ADCCalhist[i][j]->Write();
      }
   }

   PadHits->Write();
   ConeHits->Write();

   for(int i=0;i<40;i++){
      PEInConeAll[i]->Write();
      PEInConeSingle[i]->Write();
      PEInConeMerged[i]->Write();
      nHitsInConeAll[i]->Write();
      nHitsInConeSingle[i]->Write();
      nHitsInConeMerged[i]->Write();
   }

   fout->Write();
   fout->Close();

   return 0;
}
