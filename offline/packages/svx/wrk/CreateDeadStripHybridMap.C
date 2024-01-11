{
    int layer;
    int ladder;
    int sensor;
    int sensorSection;
    int readout;
    int channel;
    double adcmean;
    double adcrms;
    int chanStatus;
    double thresh;
    int verbosity=0;
    int Sections[896];
    for(int i=0; i<896; i++) {Sections[i]=0;}
    int Sensors[16*5+24*6];

    int nsections = 16*5*2*2 + 24*6*2*2;
    cout << "Number of sensor 'hybrids' = " << 16*5*2*2 << " + " << 24*6*2*2 << " = " << nsections << endl;

    ifstream infile;

    //infile.open("DeadChannelMap_Stripixel_0000347129-0000.txt"); // older file with more dead hybrids
    infile.open("DCM_Stripixel_0000347129_0000.txt");

    if (!infile) {
        std::cerr << "Can't open input file " << filename << std::endl;
    }

    int count=0;

    while (infile >> layer >> ladder >> sensor >>
           sensorSection >> readout >> channel >> adcmean >> adcrms >> chanStatus >> thresh) {

      if(chanStatus!=0) { 

        int nladders=16; if(layer==1) nladders=24;
        int nsensors=5; if(layer==1) nsensors=6;
        int nsec = 2;
        int nread = 2;
        int isection = layer*16*5*2*2 + ladder*nsensors*nsec*nread + sensor*nsec*nread + sensorSection*nread + readout;
        if(isection<0 || isection>895) cout << "wrong isection = " << isection << endl;
        int is = layer*16*5 + ladder*nsensors + sensor;
        if(is<0 || is>16*5+24*6) cout << "wrong sensor = " << is << endl;

        Sections[isection]++;
        Sensors[is]++;
        count++;

        
//        if(layer==0 && ladder==13 && sensor==0 && sensorSection==0) {
//          newcount++;
//          cout << layer << " " << ladder << " " << sensor << " " << sensorSection << " " << readout << " " << channel << endl;
//        }
/*
        if (verbosity > 0) {
            std::cerr << "Read layer " << layer
                << ", ladder " << ladder
                << ", sensor " << sensor
                << ", sensor section " << sensorSection
                << ", readout " << readout
                << ", channel " << channel
                << ", status is " << chanStatus << std::endl;
*/
      }

    }

      cout << "Total number of bad channels = " << count << endl;

  std::string filenameout = "BadStripHybrids_347129.txt";
  ofstream fout(filenameout.c_str());
  if(!fout) {cout << "ERROR: Cannot open output file." << endl;}

  //int badthresh=333;
  int badthresh=256;

      int seccount=0;
      cout << "------------- layer 2 --------------------" << endl;
      for(int i=0; i<16; i++) { // ladder
      for(int j=0; j<5; j++) {  // sensor
      for(int k=0; k<2; k++) {  // sensor section
      for(int l=0; l<2; l++) {  // readout
        int itmp = i*5*2*2 + j*2*2 + k*2 + l;
        cout << Sections[itmp] << " ";
        if(Sections[itmp]>badthresh) { // total number of channels in hybrid is 128*3 = 384
          seccount++;  // count bad sections
          fout << "0 " << i << " " << j << " " << k << " " << l << endl; // write to output file
        }
      }
      }
        cout << "  ";
      }
        cout << endl;
      }

      cout << "------------- layer 3 --------------------" << endl;
      for(int i=0; i<24; i++) {
      for(int j=0; j<6; j++) {
      for(int k=0; k<2; k++) {
      for(int l=0; l<2; l++) {
        int itmp = 16*5*2*2 + i*6*2*2 + j*2*2 + k*2 + l;
        cout << Sections[itmp] << " ";
        if(Sections[itmp]>badthresh) { // total number of channels in hybrid is 128*3 = 384
          seccount++;  // count bad sections
          fout << "1 " << i << " " << j << " " << k << " " << l << endl; // write to output file
        }
      }
      }
        cout << "  ";
      }
        cout << endl;
      }

   cout << "Total number of bad hybrids is " << seccount << endl;
   cout << " Number of bad channels outside bad hybrids = " << count - seccount*384 << endl;
   fout.close();

/*
cout << "============================================================" << endl;

      int nbads=0;
      cout << "------------- layer 2 --------------------" << endl;
      for(int i=0; i<16; i++) { // ladder
      for(int j=0; j<5; j++) {  // sensor
        int itmp = i*5 + j;
        cout << Sensors[itmp] << " ";
        if(Sensors[itmp]>333) nbads++;
      }
        cout << endl;
      }

      cout << "------------- layer 3 --------------------" << endl;
      for(int i=0; i<24; i++) { // ladder
      for(int j=0; j<6; j++) {  // sensor
        int itmp = 16*5 + i*6 + j;
        cout << Sensors[itmp] << " ";
        if(Sensors[itmp]>999) nbads++;
      }
        cout << endl;
      }
      cout << "Number of bad channels in bad sensors = " << nbads << "*1536 = " << nbads*1536 << "      " << count-nbads*1536 << endl;
*/

}





