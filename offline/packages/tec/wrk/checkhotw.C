#include <iomanip.h>

//void checkhotw(const char* run="11743-0004") {
//void checkhotw(const char* run="112666-0004") {
void checkhotw(const char* run="114102-0004") {

 cout << "Hot wires for run # " << run << endl;

  char hname[30],hfile[35];
  //sprintf(hfile, "./hotw/hotwires_xxx%s.root", run);
  sprintf(hfile, "./hotw/hotwires_xx%s.root", run);

  f = new TFile(hfile);

  float threshold = 8.;

  gSystem->Load("libpreco.so");
  cout << "libraries loaded..." << endl;

  TecAddressObject *TecAddress = new TecAddressObject();
  TecAddress->setTimeStamp(PHTimeStamp(2003,2,15,0,0,0)); // run 3
  TecAddress->Fetch();

  int nhot = 0;
  for(int i=0; i<48; i++) {
    int isect = i/12;
    int iplane = (i-isect*12)/2;
    int iside = i%2;

//    sprintf(hname,"hotw4plane%d",i);
    sprintf(hname,"hotwplane%d",i);
    hh = (TProfile*)f->Get(hname);

    for(int j=1; j<491; j++) {
      float kuku = hh->GetBinContent(j);
      if(kuku>threshold) {
	PHBoolean status = TecAddress->setSoft(0,isect,iside,iplane,j-1);
	int icrate = TecAddress->getCrate();
	int islot = TecAddress->getSlot();
	int ichan = TecAddress->getChannel();
	int packetid = TecAddress->getPacketID();
        cout 
	     << setw(2) << i << " " << setw(3) << isect << " " << setw(3) << iplane << " "  
	     << setw(3) << iside << " " << setw(4) << j-1 << "  : " 
	     << setw(3) << icrate << " " << setw(3) << islot << " " 
	     << setw(3) << ichan << " " << setw(5) 
	     << packetid << " " 
             << setw(8) << kuku << "   " 
	     << (isect*12+iplane*2+iside)*1000+j-1 << endl;
        nhot++;
      }
    }

  }
  cout << "    " << nhot << " hot wires total." << endl;
  
}

