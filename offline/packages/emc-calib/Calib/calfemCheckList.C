// Macro to check emcCalFEM object features.
// Last time used : 13-FEB-2004. Here are the results:
//
// Class                          Flavor      Factory R-ASCII W-ASCII R-OBJY W-OBJY
// emcGainFEM                     Gains         ok      ok      ok     ok      ok
// emcHLRatioFEM                  HLRatios      ok      ok      ok     ok      ok
// emcLCTofFEM                    LCTofs        ok      ok      ok     ok      ok
// emcPedestalFEM (version 0)     Pedestals     ok      -       -      -       - 
// emcPedestalFEM (version 1)     Pedestals5    ok      -       -      -       -
// emcQAFEM                       QAs           ok      ok      -      -       -
// emcTacPedFEM                   TacPeds       ok      -       -      -       - 
// emcTofT0FEM (version 0)        TofT0s        ok      ok      ok     ok      ok
// emcTofT0FEM (version 1)        TofT0Bs       ok      ok      ok     ok      ok
// emcWalkTofFEM                  WalkTofFEM    ok      ok      ok     ok      ok
//
// - means not tested (if does not mean it's not supposed to work...)
// R=Read
// W=Write
//
// To use this macro you need to :
// root[0] gSystem->Load("libemcCalib.so");
// root[1] .L calfemCheckList.C
// root[2] bool write_objy = false; //or true...
// root[3] emcCalFEM* f = calfemCheckList("Gains",write_objy);
// enjoy the messages if any.
//
//_____________________________________________________________________________
emcCalFEM* error(const char* test)
{
  std::cout << "calfemCheckList failure for " << test << std::endl;
  return 0;
}

//_____________________________________________________________________________
emcCalFEM* calfemCheckList
(const char* flavor, 
 bool writeObjy=false,
 const char* directory="/home/aphecetc/CALIB/")
{
  // Check 1 : ask the factory to build an object.

  emcCalFEM* calfem = emcCalFEMFactory::Create(flavor,0);

  if ( !calfem )
    {
      return error("Factory");
    }

  // Check 2 : ask the DataManager to read from ASCII file.

  gSystem->Load("libemcOMascii.so");

  emcDataManager* dm = emcDataManager::GetInstance();
  dm->SetVerboseLevel(10);

  dm->SetSourceDir(directory);
  calfem->SetSource(emcManageable::kFile_ASCII);

  PHTimeStamp now;

  bool ok = dm->Read(*calfem,now);

  if (!ok)
    {
      return error("Read ASCII");
    }

  calfem->Print(std::cout,1);

  // Check 3 : ask the DM to write to ASCII file.

  dm->SetDestinationDir("$AFSHOME/tmp");

  calfem->SetDestination(emcManageable::kFile_ASCII);

  ok = dm->Write(*calfem);

  if (!ok)
    {
      return error("Write ASCII");
    }

  gSystem->Load("libemcOM.so");

  if ( writeObjy == true ) 
    {
      // Check 4 : ask the DM to write to Objy.

      calfem->SetDestination(emcManageable::kDB_Objy);
      PHTimeStamp start(2030,12,30,15,15,15);
      PHTimeStamp end(PHTimeStamp::PHFarFuture);
      calfem->SetValidityPeriod(start,end);
      ok = dm->Write(*calfem);

      if (!ok)
	{
	  return error("Write Objy");
	}

    }

  // Check 5 : read from Objy

  calfem->SetSource(emcManageable::kDB_Objy);
  PHTimeStamp when(2030,12,31,23,59,0);
  ok = dm->Read(*calfem,when);

  if (!ok)
    {
      return error("Read Objy");
    }

  std::cout << "All tests successfull!" << std::endl;
  return calfem;
}
