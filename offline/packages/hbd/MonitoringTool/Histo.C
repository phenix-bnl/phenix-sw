#include <iostream>
#include "fileEventiterator.h"
#include "Event.h"
#include "packet.h"
#include "../HbdDcmRaw.h"
#include "../HbdCellListv1.h"
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"

#define NSAMPLES 8

using namespace std;

main(int argc,char *argv[])
{
  if(argc<3){cout<<"Usage: " << argv[0] << " (infile) (outfile) (nevents)"<< endl; exit(-1);}

  char infilename[400];  
  char outfile[400];  
  strcpy(infilename,argv[1]);
  strcpy(outfile,argv[2]);

  int nevents = 0;
  if(argc==4) nevents = atoi(argv[3]);

char title[400];

HbdCellList *celllist = new HbdCellListv1();

TH2F *ADC[2304];
for(int i=0;i<2304;i++){
   sprintf(title,"ADCch%d",i);
   ADC[i] = new TH2F(title,title,NSAMPLES,0,NSAMPLES,5120,-1024,4096);
}

TH2F *ADCsum = new TH2F("ADCsum","ADCsum",12,0,12,5120,-1024,4096);
TH1F *ErrSamp = new TH1F("ErrSamp","ErrSamp",100,-0.5,99.5);

//
// Setting up raw data decoder
//
HbdDcmRaw *dcm = new HbdDcmRaw();
dcm->SetNTotalSample(NSAMPLES);
dcm->SetNModPerFEM(4);
dcm->SetNFEM(12);
dcm->SetPhysicsDecode(1);

//
// Opening input file
//
int status;
fileEventiterator *it = new fileEventiterator(infilename, status);

int processed_nevents=0;

while(1){

   // Take the event
   Event *evt = it->getNextEvent();
   evt->identify();
   if(!evt) break;

   if(evt->getEvtType()!=1) continue;

   // Get Event and Decode
   dcm->GetEvent(evt,celllist);

   for(int i=0;i<2304;i++){

      // If the number of samples accumulated is not 12, error!
//      if(dcm->GetSampCount(i)!=12){ ErrSamp->Fill(i); continue; }

      for(int j=0;j<8;j++){
         int adc,clk;

         // i-- channel, j--sample 
         adc = dcm->GetADC(i,j,clk);

         ADC[i]->Fill(j,adc);
         ADCsum->Fill(j,adc);
      }
   }

   processed_nevents++;  

   if(nevents>0 && processed_nevents>=nevents) break;


//   dcm->showDecodedPacket(0);
//
//   char dum[10]; 
//   cout << "OK? ";   cin >> dum;
//   if(dum[0]=='n' || dum[0]=='N') break;

}

cout << "ended!" << endl;

//
// Writing Histogram out
//
TFile *fout =new TFile(outfile,"RECREATE");
for(int i=0;i<2304;i++) ADC[i]->Write();
ADCsum->Write();
ErrSamp->Write();
fout->Write();
fout->Close();

}
