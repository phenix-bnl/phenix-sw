
#include <iostream.h>
#include <iomanip.h>

void tecresprun(Int_t maxEvents=10, const char *pisaIFile="/phenix/data06/cogilvie/run00c/PISAEvent.root", const char *prdfOFile="data.prdf", const char *parOFile="rawpar.root", const char *relOFile="rawrel.root") {

//void tecresprun(Int_t maxEvents=100, const char *pisaIFile="/phenix/data07/lebedev/pisa2000/PISA_pionplus_1080evts_sector1_6gev.root", const char *prdfOFile="data.prdf", const char *parOFile="rawpar.root", const char *relOFile="rawrel.root") {

//void tecresprun(Int_t maxEvents=7200, const char *pisaIFile="/phenix/data07/lebedev/pisa2000/PISA_05gev_pionplus_7200evts.root", const char *prdfOFile="data.prdf", const char *parOFile="rawpar.root", const char *relOFile="rawrel.root") {

//void tecresprun(Int_t maxEvents=1080, const char *pisaIFile="/phenix/data07/lebedev/pisa2000/PISA_pionplus_1080evts_sector1_6gev.root", const char *prdfOFile="data.prdf", const char *parOFile="rawpar.root", const char *relOFile="rawrel.root") {

//void tecresprun(Int_t maxEvents=500, const char *pisaIFile="/phenix/data11/rhphemds/run00/hji135_1500auau0b20b130sq01_120800.root", const char *prdfOFile="/phenix/data09/lebedev/example1/datahij10000_500evts.prdf", const char *parOFile="rawpar10000.root", const char *relOFile="/phenix/data09/lebedev/example1/rawrelhij10000_500evts.root") {

  Int_t eventNumber = 0;

// Executing initialization and parameter macros
  gROOT->Macro("tecrespini.C");

// Read map
  PHTimeStamp Tsearch(2001,8,10,0,0,0);
  TecAddress->setTimeStamp(Tsearch);
//  char* locationA;
//  locationA = "map.tec.geant0";
//  TecAddress->setCalibName(locationA);
  TecAddress->UseSimulationDatabase();
  TecAddress->Fetch();
//  TecAddress->FetchFromFile("tecmap_database_east0.txt");

  mainIter.cd();

  if (verbose>5) printf("Entering event loop.\n");
  while (kevent < maxEvents) {

// Fetch a PISA99 event
    eventNumber = kevent + 1;
    pisarun->GetOneEvent(pisaevent,&kevent,T);

    mainIter.cd();

    if (verbose>5) printf("Fetched event %d\n",eventNumber);

    KinGetGEA(topNode);
    TecGetGEA(topNode);

    fkin->Show();
    for(int k=0; k<fkin->RowCount(); k++) {
      cout << "FKIN: " 
           << setw(4) << fkin->get_true_track(k) 
           << setw(4) << fkin->get_idpart(k) 
           << setw(12) << fkin->get_ptot(k) 
           << setw(11) << fkin->get_pthet(k) 
           << setw(11) << fkin->get_pphi(k) 
           << setw(5) << fkin->get_itparent(k) 
           << endl;

    }

//    if (verbose>10) printf("Calling event modules\n");

//    if (verbose>10) printf("Calling mTecSlowSim\n");

// SET GAINS
/*
     int index1 = 1*12 + 2*2 + 0*1; 
     int index2 = 1*12 + 1*2 + 1*1; 
     int index3 = 1*12 + 0*2 + 0*1; 
     int index4 = 1*12 + 3*2 + 1*1; 
     mTecSlowSim->set_PlaneGasGain(index1,2000.);
     mTecSlowSim->set_PlaneGasGain(index2,2000.);
     mTecSlowSim->set_PlaneGasGain(index3,2000.);
     mTecSlowSim->set_PlaneGasGain(index4,2000.);
*/

     mTecSlowSim->event(topNode, TecAddress);
//      if (verbose>10) dTecFemData->Show();

    if (verbose>10) printf("Calling mTecPack\n");
// write zero-suppressed prdf (default)
//     mTecPack->set_Suppress(505);
     mTecPack->event(topNode, TecAddress);

    if (verbose>10) printf("Calling TecPutDCM\n");
    TecPutDCM(topNode);

    PHBoolean prdfStatus = prdfOut->write(prdfNode);
//    relOut->write(evaNode);

// Reset all data for this event
    mainIter.cd();
    if (mainIter.cd("DST")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("DCM")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("GEA")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("EVA")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("TEC")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }

    pisarun->HitsClear();

  }

  // Take out the garbage
//  parOut->write(parNode);
//  delete parOut;
//  delete relOut;
  delete prdfOut;
  delete TecAddress;
  pisaFile->Close();

}

