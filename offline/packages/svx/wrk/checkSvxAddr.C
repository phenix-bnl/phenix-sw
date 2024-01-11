void checkSvxAddr(){
  gSystem->Load("libfun4all.so");
  gSystem->Load("libsvx.so");

  svxAddress& svxAddrObj = svxAddress::getInstance();
  svxAddrObj.Initialize();
  svxAddrObj.DumpIntoFile("aaa.txt");


  TFile *foroot = new TFile("nt_addr.root", "recreate");

  TNtuple *nt_addr = new TNtuple("nt_addr", "svxAddr", "sec:ix:iz:ch0:ch1");

  char tmp[512];

  float ary[18];



/*
  svxAddrObj.DumpIntoFile();

  for(int irow=0; irow<256; irow++){
    for(int icol=0; icol<32; icol++){
      int chan = 32*irow + icol;
      int ix = svxAddrObj.getSensorIX0(chan);
      int iz = svxAddrObj.getSensorIZ0(chan);
      cout<<chan<<" "<<ix<<" "<<iz<<endl;
    }
  }
*/

/*
  int layer=0, ladder=0, sensor=0;
  for(int irow=0; irow<256; irow++){ // ix
    for(int icol=0; icol<128; icol++){ // iz
      int ix   = irow;
      int tmpz = icol;
      int chan = svxAddrObj.getChannelSensor0(layer, ladder, sensor, ix, tmpz);
      cout<<"ix:tmpz:chan = "<<ix<<" "<<tmpz<<" "<<chan<<endl;
    }
  }
*/

  int ix   = 0;
  for(int ilayer=0; ilayer<2; ilayer++){ // ix
    int nlad = (ilayer==0) ? 10 : 20;
    for(int iladder=0; iladder<nlad; iladder++){ // iz
      for(int isensor=0; isensor<4; isensor++){ // iz
        for(int iz=0; iz<4; iz++){ // iz
          int tmpz=32*iz;
 
          int imodule = svxAddrObj.getModuleSensor0(ilayer, iladder, isensor, ix,tmpz);
          int iroc    = svxAddrObj.getROCSensor0(ilayer, iladder, isensor, ix,tmpz);
          cout<<"layer:ladder:sensor:tmpz = "<<ilayer<<" "<<iladder<<" "<<isensor<<" "<<tmpz<<" "<<imodule<<" "<<iroc<<endl;
        }
      }
    }
  }

  float data[5];
  for(int isec=0; isec<2; isec++){
    for(int ix=0; ix<384; ix++){
      for(int iz=0; iz<30; iz++){
        int sectype = 10;
        int ch0 = svxAddrObj.getChannel0(sectype, isec, ix, iz);
        int ch1 = svxAddrObj.getChannel1(sectype, isec, ix, iz);
        int i=0;
        data[i]=isec; i++;
        data[i]=ix; i++;
        data[i]=iz; i++;
        data[i]=ch0; i++;
        data[i]=ch1; i++;
        nt_addr->Fill(data);
        cout<<isec<<" "<<ix<<" "<<iz<<" : "<<ch0<<" "<<ch1<<endl;
      }
    }
  }

  foroot->Write();
  foroot->Close();
}
