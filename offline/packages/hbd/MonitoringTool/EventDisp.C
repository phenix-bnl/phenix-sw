EventDisp(char *infilename="rc-0200993-HBD-0.prdf", int nevents=70000)
{
gSystem->Load("libEvent.so");
gSystem->Load("../bld/.libs/libhbd.so");

char title[100];

HbdCellList *celllist = new HbdCellList();

TH2F *ADC[96];
for(int i=0;i<96;i++){
   sprintf(title,"ADCch%d",i);
   ADC[i] = new TH2F(title,title,12,0,12,5120,-1024,4096);
}

TH2F *ADCsum = new TH2F("ADCsum","ADCsum",12,0,12,5120,-1024,4096);
TH1F *ErrSamp = new TH1F("ErrSamp","ErrSamp",100,-0.5,99.5);

float mean[96];
float sigma[96];
int ch;
ifstream fin("fitouthbd.txt");

for(int i =0;i<96;i++){
  fin >> ch >> mean[i] >> sigma[i];
  cout <<ch<< " " << mean[i]<< " " << sigma[i]<<endl;
}

//
// Setting up raw data decoder
//
HbdDcmRaw *dcm = new HbdDcmRaw();
dcm->SetNTotalSample(12);
dcm->SetPhysicsDecode(1);

//
// Opening input file
//
int status;
fileEventiterator *it = new fileEventiterator(infilename, status);

c1 =new TCanvas();
HbdCrudeDisplay *hbdd = new HbdCrudeDisplay();

int processed_nevents=0;

while(1){

   // Take the event
   Event *evt = it->getNextEvent();
   if(!evt) break;

   if(evt->getEvtType()!=1) continue;

   // Get Event and Decode
   dcm->GetEvent(evt,celllist);

   int flag=0;
   for(int i=0;i<96;i++){
     hbdd->SetCharge(i+1,0.0);
   }

   for(int i=0;i<96;i++){

      // If the number of samples accumulated is not 12, error!
      if(dcm->GetSampCount(i)!=12){ ErrSamp->Fill(i); continue; }

//      for(int j=0;j<12;j++){
         int adc,clk;

         // i-- channel, j--sample 
         adc = dcm->GetADC(i,10,clk);

	 if(adc>mean[i]+4*sigma[i] && (i!=19 & i!=23)){
          cout << adc << " " << mean[i]+4*sigma[i]<< endl;
	  hbdd->SetCharge(i+1,(float)(adc-mean[i])/10.0);
	  flag=1;
	 }

//         ADC[i]->Fill(j,adc);
//         ADCsum->Fill(j,adc);
//      }
   }

   processed_nevents++;  

   if(flag==1){
     cout <<"Before drawing" << endl;
     hbdd->draw_hexagons(); c1->Update();
     char dum[10];
     cout << "OK"; cin >> dum;
   }

   if(nevents>0 && processed_nevents>=nevents) break;


//   dcm->showDecodedPacket(0);
//
//   char dum[10]; 
//   cout << "OK? ";   cin >> dum;
//   if(dum[0]=='n' || dum[0]=='N') break;

}

cout << "ended!" << endl;

}
