void bbcTest(Int_t maxEvents=4, Int_t PRDFverbose=4, const char *fname="pisa.root")
{
  //
  // Development version of bbcTest macro
  // See README document for more details
  //

  //
  // Set up the PHOOL initialization 
  //
  gROOT->Macro("phoolInit.C");

  //
  // Set up the PISA interface initialization 
  //
  gROOT->Macro("pisaInit.C");
  gROOT->Macro("pisaFileOpen.C");

  //
  // Set up the Bbc initialization 
  //
  gROOT->Macro("bbctestini.C");
  gROOT->Macro("bbctestpar.C");

  // gdb pause
  // int intest;
  // cout << "\n Pause " << endl;
  // cin  >> intest;
  // cout << "\n Intest =  " << intest << endl;

  PHNodeReset reset;

  //
  // BBC setup module calls
  //
  mBbcSetGeo->event(topNode);
  mBbcSetUcal->event(topNode);

  int const convPisaIDtoBBCID[128] =
{
13,18,10,16,21, 6,14,19,24, 3,
11,17,22,28, 7,15,22,25, 4,12,
23,29, 1, 8,26,31, 5,30, 2, 9,
27,32,64,59,41,34,62,37,63,58,
40,33,61,55,44,36,57,52,47,39,
60,64,49,43,35,56,51,46,38,53,
48,42,50,45,
18,13,21,16,10,24,29,14,06,28,
22,17,11, 3,25,20,15, 7,29,23,
12, 4,31,26, 8, 1,30, 5,32,27,
 9, 2,34,41,59,64,37,62,33,40,
58,63,36,44,55,61,39,47,52,57,
35,43,49,54,60,38,46,51,56,42,
48,53,50,45
}; 


  Int_t kevent = 0;  // counts number of full events processed
  //
  // Read and process PISA events
  //
  while (kevent < maxEvents) {

    //
    // NOTE: kevent is incremented by the GetOneEvent method
    //
    pisarun->GetOneEvent(pisaevent, &kevent, T);  // puts hits information into subsystem classes

    mainIter.cd();

    KinGetGEA(topNode);  // fill the fkin table
    BbcGetGEA(topNode);  // fill the ghit table (the name used by the Bbc in STAF)
    if(kevent <= PRDFverbose){
      bbcghit->Show();
      fkin->Show(); 
     cout << "\n";
    }

//  for (int i=0;i<10;i++){
//    cout << bbcghit->get_pmt(i) << endl;
//  }
//  float x[128], y[128], z[128];
//  for ( int i=0;i<128;i++){
//    x[i]=-9999.0;
//    y[i]=-9999.0;
//    z[i]=-9999.0;
//  }
//
//  for (int i=0;i<bbcghit->RowCount();i++){
//    int arm = bbcghit->get_pmt(i)/1000;
//    int pmt_np;
//    if        ( arm == 1 ) {
//      if        ( bbcghit->get_pmt(i) < arm*1000+33 ) {
//        pmt_no = bbcghit->get_pmt(i) - arm*1000 - 1;
//      } else {
//        pmt_no = bbcghit->get_pmt(i) - arm*1000 - 3;
//      }
//    } else if ( arm == 2 ) {
//      if        ( bbcghit->get_pmt(i) < arm*1000+33 ) {
//        pmt_no = bbcghit->get_pmt(i) - arm*1000 + 64 - 1;
//       } else {
//        pmt_no = bbcghit->get_pmt(i) - arm*1000 + 64 - 3;
//       }
//    }
//    int id_base;
//    id_base = convPisaIDtoBBCID[pmt_no]-1;
//    if ( arm == 2 ) id_base = id_base+63;
//
//    x[id_base] = bbcghit->get_pos(0,i);
//    y[id_base] = bbcghit->get_pos(1,i);
//    z[id_base] = bbcghit->get_pos(2,i);
//  }
//
//  for ( int i=0; i<128; i++ ) {
//    if ( x[i]>-999.0 ) { 
//       if ( i<64 )  printf("%3d %6.3f %6.3f %6.3f\n",i,   x[i],y[i],z[i]);
//       if ( i>=64 ) printf("%3d %6.3f %6.3f %6.3f\n",i-64,x[i],y[i],z[i]);
//    }
//  }
//
    if(kevent <= PRDFverbose){
    cout << "Before" << endl;
      dBbcGhitRaw->Show(); 
      dBbcRaw->Show(); 
     cout << "\n";
    }

    mBbcGhitRaw->event(topNode);

    if(kevent <= PRDFverbose){
      dBbcGhitRaw->Show(); 
      dBbcRaw->Show(); 
     cout << "\n";
    }

    cout << "Before" << endl;
    for (int ipmt=0;ipmt<128;ipmt++){
      cout << ipmt << " " << dBbcRaw->get_Adc(ipmt) << " " << dBbcRaw->get_Tdc0(ipmt) << " " << dBbcRaw->get_Tdc1(ipmt) << endl;
    }
    mBbcFEM->event(topNode);
    if(kevent <= PRDFverbose){
      dBbcFEM->Show(); 
     cout << "\n";
    }

    mBbcDCM->event(topNode);
    if(kevent <= PRDFverbose){
      dBbcDCM->Show(); 
     cout << "\n";
    }

    mBbcUnpack->event(topNode);
    if(kevent <= PRDFverbose){
     dBbcRaw->Show(); 
     cout << "\n";
    }
    cout << "After" << endl;
    for (int ipmt=0;ipmt<128;ipmt++){
//    cout << ipmt << " " << dBbcRaw->get_Adc(ipmt) << " " << dBbcRaw->get_Tdc0(ipmt) << " " << dBbcRaw->get_Tdc1(ipmt) << endl;
    }


    mBbcRawOut->event(topNode);
    if(kevent <= PRDFverbose){
      dBbcOut->Show(); 
      cout << "\n";
    }

    // Reset all data for this event
    mainIter.cd();
    if (mainIter.cd("DST")) {
      // cout << "\n In DST " << endl;
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("DCM")) {
      // cout << "\n In DCM " << endl;
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("BBC")) {
      // cout << "\n In BBC " << endl;
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("GEA")) {
      // cout << "\n In GEA " << endl;
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("EVA")) {
      // cout << "\n In EVA " << endl;
      mainIter.forEach(reset);
      mainIter.cd();
    }

    //
    // Clean up memory use at end of full event
    //
    pisarun->HitsClear();  // releases memory assigned to XxxPISAHit globals

  }  // loop over full events

  delete pisaevent;  // remove instance created with new
  delete pisarun;    // remove instance created with new

  pisaFile->Close();  // close the PISA hits file
}

