RealReader(char *infilename="rc-0201011-HBD-0.prdf", char *outfile="test.root", int nevents=7000)
{
gSystem->Load("libEvent.so");
gSystem->Load("../bld/.libs/libhbd.so");

char title[100];

HbdCellList *celllist = new HbdCellListv1();
HbdBlobList *bloblist = new HbdBlobListv1();

HbdClusterizer *clus = new HbdClusterizer();
clus->Verbosity(1);

TH2F *ADC[96];
for(int i=0;i<96;i++){
   sprintf(title,"ADCch%d",i);
   ADC[i] = new TH2F(title,title,12,0,12,5120,-1024,4096);
}

TH2F *ADCsum = new TH2F("ADCsum","ADCsum",12,0,12,5120,-1024,4096);
TH1F *ErrSamp = new TH1F("ErrSamp","ErrSamp",100,-0.5,99.5);

//
// Setting up raw data decoder
//
HbdDcmRaw *dcm = new HbdDcmRaw();
dcm->SetNTotalSample(12);
dcm->SetPhysicsDecode(1);
dcm->SetChargeThreshold(2150);

int dislist[2]={19,23}; 
dcm->SetDisablePadId(dislist,2);


//
// Opening input file
//
int status;
fileEventiterator *it = new fileEventiterator(infilename, status);

int processed_nevents=0;

while(1){

   // Take the event
   Event *evt = it->getNextEvent();
   if(!evt) break;

   if(evt->getEvtType()!=1) continue;

   // Get Event and Decode
   dcm->GetEvent(evt,celllist);

   cout << "Cells: " << celllist->get_nCells() << endl;
   for(int i=0;i< celllist->get_nCells();i++){
      cout << "padid: " << celllist->get_cell(i)->get_padnum();
      cout << ", charge: " << celllist->get_cell(i)->get_charge()<< endl;
   }

   if(bloblist->get_nBlobs()!=0){
      cout << "clearing"<< endl;
      for(int i=0;i<bloblist->get_nBlobs(); i++) bloblist->RemoveBlob(i);
      bloblist->set_nBlobs(0);
   }

   cout << "Blobs: " << bloblist->get_nBlobs() << endl;
   clus->Clusterizer(celllist,bloblist);


   cout << "Blobs: " << bloblist->get_nBlobs() << endl;

   for(int i=0;i<96;i++){

      // If the number of samples accumulated is not 12, error!
      if(dcm->GetSampCount(i)!=12){ ErrSamp->Fill(i); continue; }

      for(int j=0;j<12;j++){
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
for(int i=0;i<96;i++) ADC[i]->Write();
ADCsum->Write();
ErrSamp->Write();
fout->Write();
fout->Close();

/*
TCanvas *c1 = new TCanvas("c1","c1",0,0,1600,1000);
c1->Divide(12,8);
ofstream ftxtout("testout.txt");
for(int i=0;i<96;i++){
   c1->cd(i+1);
   ADC[i]->SetAxisRange(0,4096);
   ADC[i]->Draw();
   ftxtout << "ch: " << i <<", Mean: " << ADC[i]->GetMean() << ", RMS: " << ADC[i]->GetRMS() << endl;
}
ftxtout.close();

c1->Update();
//c1->Print("good.ps");
*/

}
