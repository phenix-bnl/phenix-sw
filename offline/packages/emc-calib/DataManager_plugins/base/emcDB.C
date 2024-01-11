#include "emcDB.h"

#include "emcDataManager.h"
#include "emcDefines.h"
#include "emcGains.h"
#include "emcHLRatios.h"
#include "emcPedestals.h"
#include "emcLCTofs.h"
#include "emcWalkTofs.h"
#include "emcTofT0s.h"
#include "emcQAs.h"
#include "emcRawDataAccessor.h"
#include "emcCalFEM.h"
#include "emcCalFEMFactory.h"
#include "fileEventiterator.h"
#include "EmcIndexer.h"
#include "emcTracedFEM.h"
#include "emcTracedValue.h"
#include "emcGainFEM.h"
#include "emcDefines.h"
#include "RunTimesFactory.h"
#include "TStopwatch.h"
#include "emcCalibrationData.h"
#include "emcRejectList.h"
#include "emcConfigurationFile.h"
#include "emcOMHelper.h"
#include "emcOMCalFEMT.h"
#include "PHTimeStamp.h"
#include <typeinfo>
#include <algorithm>
#include <iomanip>
#include <ctime>
#include <cstdlib>
#include <cstdio>
#include <cassert>
#include <stdexcept>
#include <string>
#include <list>
#include <vector>
#include <set>
#include <fstream>
#include <sstream>
#include <memory>
#include "TROOT.h"
#include "TSystem.h"

#include "PdbApplication.hh"
#include "PdbBankManagerFactory.hh"
#include "PdbBankID.hh"
#include "PdbCalBank.hh"
#include "PdbCalBankIterator.hh"
#include "RunToTime.hh"

using namespace std;

//_____________________________________________________________________________
int
isRunNumber(const std::string str)
{
  // 10 digits
  if (str.size()!=10) return -1;

  for ( size_t i = 0; i < 10; ++i )
    {
      if (!isdigit(str[i])) return -1;
    }
  return atoi(str.c_str());
}

//_____________________________________________________________________________
std::string
getTofT0Flavor(const std::string filename)
{
  std::ifstream in(filename.c_str());
  assert(in.good());
  char str[1024];

  in.getline(str,1024,'\n');

  int channel=-1;
  float dummy;

  while ( in >> channel >> dummy >> dummy >>dummy )
    {
      std::cout << channel << std::endl;
    }

  assert(channel>=144);

  if (channel>=146)
    {
      return "TofT0Bs";
    }
  else
    {
      return "TofT0s";
    }
}

//_____________________________________________________________________________
bool isInitialCalibrationDirectory(const char* dir)
{
  char* cdir = gSystem->ExpandPathName(dir);
  void* dirp = gSystem->OpenDirectory(cdir);

  const char* entry;

  bool ok = false;

  while ( (entry = gSystem->GetDirEntry(dirp)) )
    {
      string str = entry;

      if ( str.find("INICAL") < str.size() )
        {
          ok = true;
        }
    }

  gSystem->FreeDirectory(dirp);
  delete[] cdir;

  return ok;
}

//_____________________________________________________________________________
bool isRejectListDirectory(const char* dir)
{
  char* cdir = gSystem->ExpandPathName(dir);
  void* dirp = gSystem->OpenDirectory(cdir);

  const char* entry;

  bool ok = false;

  while ( (entry = gSystem->GetDirEntry(dirp)) )
    {
      string str = entry;

      if ( str == "reject.list" )
        {
          ok = true;
        }
    }

  gSystem->FreeDirectory(dirp);
  delete[] cdir;
  return ok;
}

//_____________________________________________________________________________
emcDB::emcDB(const char* dbms,
	     const char* dbname,
             bool interactive, PHTimeStamp* ts, PHTimeStamp* tsend)
{
  fDBMS = dbms;
  fDbname = dbname;
  initDBMS(fDBMS.c_str());
  fInteractive = interactive;
  fForceDate = ts;
  fForceEndDate = tsend;
  fInsertAfter.setTics(0);
  fInsertBefore.setToFarFuture();
  fStartAfter.setTics(0);
  fStartBefore.setToFarFuture();
  fEndAfter.setTics(0);
  fEndBefore.setToFarFuture();
  fDebug = false;
  fStartIntervalGiven = false;
  fEndIntervalGiven = false;
  fInsertIntervalGiven = false;
}

//_____________________________________________________________________________
emcDB::~emcDB()
{}

//_____________________________________________________________________________
void
emcDB::Abort(const string& method, const string& message)
{
  Error(method, message);
  BankManager()->getApplication()->abort();
  exit(1);
}

//_____________________________________________________________________________
int
emcDB::AbsolutePosition(const string& femName)
{
  Int_t rv = -1;

  if (femName.substr(0, 8) == "FEM.EMC.")
    {

      string sector = femName.substr(8, 2);
      int sn = EmcIndexer::EmcSectorNumber(sector.c_str());

      int sm144;

      if ( sn >= 0 && femName.substr(11, 2) == "SM")
        {

          sm144 = atoi(femName.substr(13, 2).c_str());
          rv = EmcIndexer::iSiSM144_PXSM144(sn, sm144);
        }
    }

  return rv;
}

//_____________________________________________________________________________
PdbBankManager*
emcDB::BankManager()
{
  static PdbBankManager* bankManager =
    PdbBankManagerFactory::instance().create(fDBMS.c_str());
  if (!bankManager)
    {
      std::cerr << "<FATAL> Do not know how to deal with this dbms = "
		<< fDBMS << std::endl;
      exit(1);
    }
  return bankManager;
}

//_____________________________________________________________________________
int
emcDB::Compare(PdbCalBank& bank, const string& dir,
               const string& flavour)
{
  // From bank, build an emcCalFEM object obj1
  // From dir and bank characteristics, build an emcCalFEM object obj2
  // Then compare obj1 and obj2

  initDBMS("Ascii");
  const int kCannotReadFromFile = -1;
  const int kUnknownFlavour = -2;
  const int kPluginNotFound = -3;

  const int kSame = 1;
  const int kDifferent = 0;

  // Build an emcCalFEM object from the Objy bank.

  int femAbsolutePosition = bank.getBankID().getInternalValue();

  int femPinNumber = 0;

  if ( flavour == "Pedestals5" || flavour == "Pedestals" )
    {
      // try to find pinNumber
      femPinNumber = GetPinNumber(femAbsolutePosition);
      cout << "FEM " << femAbsolutePosition << " has pinNumber="
	   << femPinNumber << endl;
    }

  emcCalFEM* calfem1 = emcCalFEMFactory::Create(flavour.c_str(),
						femAbsolutePosition);

  if (!calfem1)
    {
      return kUnknownFlavour;
    }

  // find the plugin able to read (0) the calfem1.
  calfem1->SetSource(destination());
  emcOMCalFEM* om = static_cast<emcOMCalFEM*>
    (emcOMHelper::findOM(*calfem1, 0));
  //FIXME : WARNING !!! The line above should really be a dynamic_cast
  //to be on the very safe side.
  //For whatever reason it does not work. Have no time right now to
  //look into this. But if you read this because you crash here,
  //well, maybe it's time to look again...
  
  if (!om)
    {
      return kPluginNotFound;
    }

  om->FromPdbCalBank(*calfem1, bank);

  calfem1->SetValidityPeriod(bank.getStartValTime(), bank.getEndValTime());

  // Build nows an emcCalFEM object from file.

  emcDataManager* dm = emcDataManager::GetInstance();

  string olddir = dm->GetSourceDir(); // save dir value.

  dm->SetSourceDir(dir.c_str());

  emcCalFEM* calfem2 = emcCalFEMFactory::Create(flavour.c_str(),
						femAbsolutePosition);

  calfem2->SetSource(emcManageable::kFile_ASCII);

  PHTimeStamp tdummy(0);

  int femCode = emcCalFEM::FEMCode(femAbsolutePosition, femPinNumber, 0, 0);

  bool ok = dm->Read(*calfem2, tdummy, femCode);

  dm->SetSourceDir(olddir.c_str()); // restore old dir value.

  if (!ok)
    {
      return kCannotReadFromFile;
    }

  if ( *calfem1 != *calfem2 )
    {

      emcTracedFEM* tfem = dynamic_cast<emcTracedFEM*>(calfem2);

      if ( !tfem )
	{
	  return kDifferent;
	}

      if ( EmcIndexer::isPbScFEM(femAbsolutePosition) )
        {

          // No need for tricks if they match already.
          if ( *calfem1 == *calfem2 )
            {
              return kSame;
            }

          std::auto_ptr<emcTracedFEM> test(tfem->clone());

          // Try compact only first.

          cout << "Performing Compact(0.02)" << endl;

          test->Compact(0.02);

          if ( *(test.get()) == *calfem1 )
            {
              return kSame;
            }

          // For traced-like objects, there have been some tricks
          // applied when going from File to DB.
          // Try to re-apply them here to give one more chance
          // for a match.

          cout << "Performing RemoveLastItems() and Compact(0.02)" << endl;
          tfem->RemoveLastItems();
          if ( tfem->GetNumberOfItems() < tfem->GetNumberOfChannels() )
            {
              return kDifferent;
            }
          else
            {
              tfem->Compact(0.02);
            }
        }
      else
        {
          UpdateXValue(*tfem, 0);
        }

      if ( *calfem1 != *calfem2)
        {
          return kDifferent;
        }
      else
        {
          return kSame;
        }
    }

  return kSame;
}

//_____________________________________________________________________________
emcManageable::EStorage
emcDB::destination() const
{
  if ( fDBMS == "Objy" )
    {
      return emcManageable::kDB_Objy;
    }
  else if ( fDBMS == "Pg" )
    {
      return emcManageable::kDB_Pg;
    }
  else if ( fDBMS == "Ascii" )
    {
      return emcManageable::kFile_ASCII;
    }
  else
    {
      std::cerr << "emcDB::destination : unknown DBMS=" << fDBMS
		<< std::endl;
      exit(1);
    }
}
//_____________________________________________________________________________
bool
emcDB::Dump(const string fulldbname)
{
  //
  // Basic utility to dump a given DB on screen
  //
  // fulldbname is the full database name, i.e. including dots, like in
  // calib.emc.Gains
  //

  PdbCalBankIterator* it = BankManager()->getIterator();

  if (!it)
    {
      std::cerr << "emcDB::Dump : could not get a PdbCalBankIterator!"
		<< std::endl;
      ;
      return 0;
    }

  string bankName = fulldbname;

  cout << string(80, '-') << endl;
  cout << Version() << endl;
  cout << string(80, '-') << endl;
  cout << "Dumping " << bankName << " calibration database " << endl;

  PdbBankID bankID(fBankID);

  it->init(fulldbname.c_str(), bankID);

  if ( fInsertIntervalGiven )
    {
      it->setInsertTimeLimits(fInsertAfter, fInsertBefore);
    }
  if ( fStartIntervalGiven )
    {
      it->setStartValTimeLimits(fStartAfter, fStartBefore);
    }
  if ( fEndIntervalGiven )
    {
      it->setEndValTimeLimits(fEndAfter, fEndBefore);
    }

  it->print();
  cout << string(80, '-') << endl;

  string flavour;

  if ( bankName.find("calib.emc.") < bankName.size() )
    {
      flavour = bankName.substr(strlen("calib.emc."));
    }

  PdbCalBank* bank;

  while ( (bank = it->next()) )
    {
      bank->printHeader();

      if ( fInteractive == true )
        {

          cout << "[# of channels] Show channels [n] Next bank "
	       << " [q] Quit, [c] Compare to file(s) " << endl;
          cout << "Please enter command >  ";

          string sanswer;
          cin >> sanswer;

          sanswer[0] = toupper(sanswer[0]);

          if ( sanswer == "C" )
            {
              CompareToDirectories(*bank, flavour);
            }

          if ( sanswer == "Q" )
            return Quit();
          if ( sanswer == "N" )
            continue;

          int nchannels = atoi(sanswer.c_str());

          DumpContent(*bank, nchannels);
        }

      delete bank;
    }

  return Quit();
}


//_____________________________________________________________________________
void
emcDB::CompareToDirectories(PdbCalBank& bank,
                            const string& flavour)
{
  for (size_t j = 0; j < fDirectories.size(); j++ )
    {
      int compare = Compare(bank, fDirectories[j], flavour);

      if ( compare >= 0 )
        {
          string match = "MATCHES";
          if ( compare == 0 )
            {
              match = "DOES NOT match";
            }
          cout << match << " the one in "
	       << fDirectories[j] << endl;
        }
      else
        {
          cout << "emcDB::CompareToDirectories  : "
	       << "Cannot infer equality (return code "
	       << compare << ")" << endl;
        }
    }
}

//_____________________________________________________________________________
void
emcDB::DumpContent(PdbCalBank& bank, int nchannels)
{
  if (nchannels > 0)
    {
      for ( int i = 0;
            i < static_cast<int>(bank.getLength()) &&
	      i < static_cast<int>(nchannels); i++)
        {
          bank.printEntry(i);
        }
    }
}

//_____________________________________________________________________________
PHTimeStamp
emcDB::EndOfValidity(void) const
{
  if ( fForceEndDate )
    {
      return PHTimeStamp(*fForceEndDate);
    }
  else
    {
      PHTimeStamp end;
      end.setToFarFuture();
      return end;
    }
}

//_____________________________________________________________________________
bool emcDB::Error(const string method, const string message)
{
  cerr << "<E> emcDB::" << method << endl;
  cerr << "          :" << message << endl;
  return false;
}

//_____________________________________________________________________________
int
emcDB::GetPinNumber(int absPosition)
{
  if (fPinNumbers.empty())
    {
      bool ok = ReadConfiguration(false);
      if (!ok)
        {
          cerr << EMC_ERROR_MSG
	       << " Could not read configuration, thus pin numbers are not available"
	       << "(i.e. you will not be able to properly compare pedestals)"
	       << endl;
          fPinNumbers.resize(EmcIndexer::MaxNumberOfFEMs(), 0);
        }
    }

  return fPinNumbers[absPosition];
}

//_____________________________________________________________________________
void
emcDB::initDBMS(const char* dbms)
{
  static std::set
    <std::string> alreadyLoaded;

  std::string DBMS = dbms;

  if ( alreadyLoaded.find(DBMS) != alreadyLoaded.end() )
    {
      return ;
    }

  // Be sure to load the correct libraries.
  if ( DBMS == "Objy" )
    {
      if ( !gSystem->Load("libemcOM.so") )
        {
          alreadyLoaded.insert("Objy");
        }
    }
  else if ( DBMS == "Pg" )
    {
      if ( !gSystem->Load("libemcOMpg.so") )
        {
	  gSystem->Load("libPgCalInstance.so");
	  PdbApplication* app = PdbApplication::instance();
	  app->setDBName(fDbname.c_str());
          alreadyLoaded.insert("Pg");
        }
    }
  else if ( DBMS == "Ascii" )
    {
      if (!gSystem->Load("libemcOMascii.so"))
        {
          alreadyLoaded.insert("Ascii");
        }
    }
  else 
    {
      std::cerr << "emcDB::initDBMS : Do not know this DBMS : "
		<< DBMS << std::endl;
      exit(1);
    }
}

//_____________________________________________________________________________
void emcDB::ParseFileName(const string filename, size_t& ifem, size_t& pin)
{
  //
  // Get the FEM absolute number and pin number (if available)
  // from the file name.
  //

  int iS; // Sector number (0-8)
  int iSM144; // SuperModule number (0-144), within sector

  // ifem will be Absolute FEM id (0-172)
  // Note that iS=8 (=>ifem>171) denotes reference FEM(s)

  int offset = 0;

  if ( filename.substr(0, 4) == "NONE" )
    {
      iS = 8;
      offset = 2;
    }
  else
    {
      iS = EmcIndexer::EmcSectorNumber(const_cast<char*>(filename.substr(0, 2).c_str()));
    }

  iSM144 = atoi(filename.substr(4 + offset, 2).c_str());

  ifem = EmcIndexer::iSiSM144_PXSM144(iS, iSM144);

  size_t pos = filename.find("FEM");

  if (pos < filename.size())
    {
      pin = atoi(filename.substr(pos + 3, 4).c_str());
    }
  else
    {
      pin = 0;
    }
}

//_____________________________________________________________________________
int emcDB::PedestalVersion(const string csdir)
{
  string dir = csdir;

  char* cdir = gSystem->ExpandPathName(dir.c_str());
  void* dirp = gSystem->OpenDirectory(cdir);
  const char* entry;
  string str;
  int n = 0;
  int version = 0;

  assert(dirp != 0);

  while ( (version == 0) && (entry = gSystem->GetDirEntry(dirp)) )
    {

      str = entry;

      if (str.find("HG_Post") < str.size())
        n++;
      else
        if (str.find("LG_Post") < str.size())
          n++;
        else
          if (str.find("HG_Pre") < str.size())
            n++;
          else
            if (str.find("LG_Pre") < str.size())
              n++;
            else
              if (str.find("TAC") < str.size())
                n++;
              else
                if (str.find("LG_Pre-Post") < str.size())
                  n++;
                else
                  if (str.find("HG_Pre-Post") < str.size())
                    n++;
    }

  if (n > 3)
    version = 1;

  gSystem->FreeDirectory(dirp);
  delete[] cdir;

  return version;
}

//_____________________________________________________________________________
bool
emcDB::Quit(void)
{
  BankManager()->getApplication()->commit();
  return true;
}

//_____________________________________________________________________________
void
emcDB::SetEndInterval(const PHTimeStamp& endAfter,
                      const PHTimeStamp& endBefore)
{
  fEndAfter = endAfter;
  fEndBefore = endBefore;
  fEndIntervalGiven = true;
}

//_____________________________________________________________________________
void
emcDB::SetInsertInterval(const PHTimeStamp& insertAfter,
                         const PHTimeStamp& insertBefore)
{
  fInsertAfter = insertAfter;
  fInsertBefore = insertBefore;
  fInsertIntervalGiven = true;
}

//_____________________________________________________________________________
void
emcDB::SetStartInterval(const PHTimeStamp& startAfter,
                        const PHTimeStamp& startBefore)
{
  fStartAfter = startAfter;
  fStartBefore = startBefore;
  fStartIntervalGiven = true;
}

//_____________________________________________________________________________
bool emcDB::Update(const string ctedir)
{
  //
  // Reads all the FEM calib. parameter from dir and put them into DB
  // Calib. flavor is taken from last characters in directory name.
  //

  string dir = ctedir;

  // Must be in interactive mode.
  if ( !fInteractive )
    {
      return Error("Update", "Must be in interactive mode.");
    }

  initDBMS("Ascii");

  // Check that directory does exist.

  char* cdir = gSystem->ExpandPathName(dir.c_str());
  void* dirp = gSystem->OpenDirectory(cdir);

  if ( !dirp )
    {
      cerr << "Failed to open directory: " << dir << endl;
      return false;
    }

  // Find the flavor type

  while (dir[dir.size() - 1] == '/')
    {
      dir = dir.substr(0, dir.size() - 1);
    }

  size_t pos = dir.rfind('/');

  string topdir = dir.substr(0, pos + 1);
  string flavor = dir.substr(pos + 1, dir.size() - pos - 1);

  // Dir. names and flavor names have not always the same name...

  if (flavor.find("Gains") < flavor.size())
    {
      flavor = "Gains";
    }
  else if (flavor.find("PEDESTALS") < flavor.size())
    {
      int version = PedestalVersion(ctedir);
      if (version == 0)
        {
          flavor = "Pedestals";
        }
      else if (version == 1)
        {
          flavor = "Pedestals5";
        }
      else
        {
          assert(0 == 1);
        }
    }
  else if (flavor.find("ToF") < flavor.size())
    {
      flavor = "TOF";
    }
  else if (flavor.find("HLRatio") < flavor.size())
    {
      flavor = "HLRatios";
    }
  else if (flavor.find("QA") < flavor.size())
    {
      flavor = "QAs";
    }
  else
    {
      // Other "flavors" are not emcCalFEM object and need
      // special treatment.

      bool inical = isInitialCalibrationDirectory(cdir);
      bool rejectlist = isRejectListDirectory(cdir);

      if ( !inical && !rejectlist )
        {
          cerr << "<E> Cannot infer flavor type for this directory : "
	       << dir << endl;
          return false;
        }
      else if ( inical )
        {
          if ( fForceDate )
            {
              return UpdateInitialCalibration(topdir);
            }
          else
            {
              cerr << "<E> Updating IniCal requires the forceDate option so far."
		   << endl;
              return false;
            }
        }
      else if ( rejectlist )
        {
          if ( fForceDate )
            {
              return UpdateRejectList(topdir);
            }
          else
            {
              cerr << "<E> Updating RejectList requires the forceDate "
		   << "option so far."
		   << endl;
              return false;
            }
        }
    }

  cout << string(50, '-') << endl;
  cout << "FLAVOUR = " << flavor << endl;
  cout << string(50, '-') << endl;

  if ( topdir.empty() )
    {
      topdir = "./";
    }

  emcDataManager* dm = emcDataManager::GetInstance();
  dm->SetSourceDir(topdir.c_str());

  // We then loop over files in this directory

  const char* entry;
  string str;
  size_t ifem;
  size_t pin;

  emcCalFEM* read = 0;

  const size_t NMAX = 200;
  vector<bool> kAlreadyRead(NMAX);
  size_t i;

  for ( i = 0; i < kAlreadyRead.size(); i++ )
    {
      kAlreadyRead[i] = false;
    }

  PHTimeStamp tdummy;
  string theflavor;

  while ( (entry = gSystem->GetDirEntry(dirp)) )
    {

      str = entry;

      if (str == "." || str == "..")
        continue;

      theflavor = flavor;

      ParseFileName(str, ifem, pin);

      if (flavor == "TOF")
        {
	  int runnumber = isRunNumber(str);

	  if ( runnumber > 0 )
	    {
	      UpdateTofSectorOffset(cdir,runnumber);
	      continue;
	    }

          if (str.find(".TOF_LC") < str.size())
            {
              theflavor = "LCTofs";
              read = emcCalFEMFactory::Create("LCTofs", ifem);
            }
          else if (str.find(".TOF_WALK") < str.size())
            {
              theflavor = "WalkTofs";
              read = emcCalFEMFactory::Create("WalkTofs", ifem);
            }
          else if (str.find("PED-TOWERS-TAC-DRIFT.TofT0s") < str.size())
            {
              theflavor = "TacPeds";
              read = emcCalFEMFactory::Create("TacPeds", ifem);
            }
          else if (str.find(".TofT0s") < str.size())
            {
	      std::string filename = cdir;
	      filename += "/";
	      filename += str;
              theflavor = getTofT0Flavor(filename);	    
              read = emcCalFEMFactory::Create(theflavor.c_str(),ifem);
            }
          else
            {
              cerr << "<W> Unknown file type " << entry << endl;
              continue;
            }
        }
      else
        {
          read = emcCalFEMFactory::Create(flavor.c_str(), ifem);
        }

      assert(read != 0);

      read->SetSource(emcManageable::kFile_ASCII);

      read->SetDestination(destination());

      if ( ifem < kAlreadyRead.size() && !kAlreadyRead[ifem] )
        {

          int code = emcCalFEM::FEMCode(ifem, pin, 0, 0);

          assert(ifem < NMAX);

          cout << "Reading " << theflavor << "...";

          bool ok = dm->Read(*read, tdummy, code);

          cout << "done" << endl;

          if (ok)
            {

              // 	if ( flavor == "Gains" && EmcIndexer::isPbScFEM(ifem) ) {
              // 	  // Special treatment for PbSc Gains.
              // 	  emcTracedFEM* tfem = dynamic_cast<emcTracedFEM*>(read);
              // 	  if (!tfem) {
              // 	    cerr << EMC_ERROR_MSG << " Uh Oh ! tfem is not of the expected type ?! PbSc gains not correctly handled. No writing done" << endl;
              // 	    ok = false;
              // 	  }
              // 	  else {
              // 	    tfem->RemoveLastItems();
              // 	    const float epsilon = 0.02;
              // 	    cout << " Compact level "
              // 		 << tfem->Compact(epsilon)
              // 	      // merge items consistent within epsilon
              // 		 << endl;
              // 	  }
              // 	}

            }

          if (ok)
            {
              if (flavor != "TOF")
                {
                  kAlreadyRead[ifem] = true;
                }

              // Update the start validity time if requested
              if (fForceDate)
                {
                  if ( fForceEndDate )
                    {
                      read->SetValidityPeriod(*fForceDate, *fForceEndDate);
                    }
                  else
                    {
                      read->SetValidityPeriod(*fForceDate, read->GetEndValTime());
                    }
                }

              read->Print();

              cout << "Writing...";
              ok = dm->Write(*read, tdummy);
              if (!ok)
                {
                  cerr << "<E> Could not dump FEM " << ifem << " into DB" << endl;
                  cout << "Failed" << endl;
                }
              else
                {
                  cout << "done" << endl;
                }
            } // ok
        } // ifem valid

      delete read; read = NULL;

    } // while

  return true;
}


//_____________________________________________________________________
void
emcDB::UpdateXValue(emcTracedFEM& fem, int value)
{
  // Replaces, for all channels, the X parameter by value.
  // Assumes that there only one item per channel.
  // Used by UpdatePbGlGains only.

  emcTracedValue* tv;

  for ( size_t i = 0; i < fem.GetNumberOfChannels(); i++ )
    {
      int n = 0;
      fem.FirstItem(i);
      while ( (tv = fem.NextItem()) != 0 )
        {
          tv->Set(value, tv->GetConstant(), tv->GetSlope());
          n++;
        }
      assert(n == 1);
    }
}

//_____________________________________________________________________________
bool
emcDB::UpdateInitialCalibration(const string top_directory)
{
  string browsedir = top_directory;

  browsedir += "IniCal/";

  char* cdir = gSystem->ExpandPathName(browsedir.c_str());
  void* dirp = gSystem->OpenDirectory(cdir);

  cout << "<I> emcDB::UpdateInitialCalibration : dir = "
       << cdir << endl;

  const char* entry;

  while ( (entry = gSystem->GetDirEntry(dirp)) )
    {
      string str = entry;

      cout << str << endl;

      if ( str.find("INICAL") < str.size() )
        {
          // Get sector number out of here
          size_t pos = str.find('.');
          string sectorId = str.substr(0, pos);
          int iS = EmcIndexer::EmcSectorNumber(sectorId.c_str());
          if ( iS < 0 )
            {
              cerr << "<E> emcDB::UpdateInitialCalibration : file "
		   << str << " is not of InitialCalibration type, as "
		   << " its name suggest. I skip it."
		   << endl;
            }
          else
            {
              emcCalibrationData sector(emcCalibrationData::kIniCal, iS);
              sector.SetSource(emcManageable::kFile_ASCII);
              sector.SetDestination(destination());
              emcDataManager* dm = emcDataManager::GetInstance();
              dm->SetSourceDir(top_directory.c_str());
              PHTimeStamp dummy;
              bool ok = dm->Read(sector, dummy);
              if (ok)
                {
                  assert(fForceDate != 0);
                  sector.SetValidityPeriod(*fForceDate, EndOfValidity());
                  sector.Print();
                  ok = dm->Write(sector);
                  if ( !ok )
                    {
                      cerr << "<E>  emcDB::UpdateInitialCalibration : "
			   << " Could not write sector " << iS
			   << " to Objy" << endl;
                    }
                }
              else
                {
                  cerr << "<E> emcDB::UpdateInitialCalibration : Could not "
		       << " read sector " << iS << " in "
		       << cdir << endl;
                }
            }
        }
    }

  return true;
}

//_____________________________________________________________________________
bool
emcDB::UpdateRejectList(const string top_directory)
{
  emcRejectList rl;

  emcDataManager* dm = emcDataManager::GetInstance();

  dm->SetSourceDir(top_directory.c_str());

  rl.SetSource(emcManageable::kFile_ASCII);
  rl.SetDestination(destination());

  PHTimeStamp dummy;

  bool ok = dm->Read(rl, dummy);

  if (ok)
    {
      assert(fForceDate != 0);
      rl.SetValidityPeriod(*fForceDate, EndOfValidity());
      ok = dm->Write(rl);
      if ( !ok )
        {
          cerr << "<E>  emcDB::UpdateRejectList : "
	       << " Could not write rejectlist "
	       << " to Objy" << endl;
        }
    }
  else
    {
      cerr << "<E> emcDB::UpdateRejectList : Could not "
	   << " read rejectlist in "
	   << top_directory << "/RejectList" << endl;
    }
  return true;
}

//_____________________________________________________________________________
bool
emcDB::UpdateTofSectorOffset(const std::string dir, int runnumber)
{
  // Very first thing to check is that we do have valid start and 
  //  end timestamps for this run.

  RunToTime* rt = RunToTime::instance();
  if (!rt)
    {
      std::cerr << "UpdateTofSectorOffset: could not get to RunToTime object"
		<< std::endl;
      return false;
    }

  std::auto_ptr<PHTimeStamp> start(rt->getBeginTime(runnumber));
  std::auto_ptr<PHTimeStamp> end(rt->getEndTime(runnumber));

  if ( !start.get() )
    {
      std::cerr << "UpdateTofSectorOffset: could not get start time of run="
		<< runnumber << std::endl;
      return false;
    }

  if ( *(end.get()) <= *(start.get()) )
    {
      std::cout << "End time = " << *end << " of run=" << runnumber 
		<< " is invalid. I will use start+1 second" << std::endl;
      *(end.get()) = *(start.get());
      *(end.get()) += 1;
    }

  // Ok. We got the time stamps. Let's continue.

  std::ostringstream sdir;
  
  sdir << dir << "/" << std::setfill('0') << std::setw(10)
       << runnumber;

  char* cdir = gSystem->ExpandPathName(sdir.str().c_str());
  void* dirp = gSystem->OpenDirectory(cdir);

  if ( !dirp )
    {
      cerr << "Failed to open directory: " << dir << endl;
      return false;
    }

  const char* entry;

  // be very strict on the files we consider
  std::map<const std::string, int> validentries;
  validentries["W0.TOF_OFFSET"]=1;
  validentries["W1.TOF_OFFSET"]=1;
  validentries["W2.TOF_OFFSET"]=1;
  validentries["W3.TOF_OFFSET"]=1;
  validentries["E2.TOF_OFFSET"]=1;
  validentries["E3.TOF_OFFSET"]=1;
  validentries["E0.TOF_OFFSET"]=1;
  validentries["E1.TOF_OFFSET"]=1;
			       
  while ( (entry = gSystem->GetDirEntry(dirp)) )
    {
      std::string sentry = entry;
      if ( validentries[sentry] == 1 )
	{
	  int isector = 
	    EmcIndexer::EmcSectorNumber(sentry.substr(0,2).c_str());
	  assert(isector>=0 && isector<=7);

	  emcDataManager* dm = emcDataManager::GetInstance();
	  emcCalibrationData sec(emcCalibrationData::kTofSectorOffset,isector);
	  sec.SetSource(emcManageable::kFile_ASCII);
	  bool ok = dm->Read(sec,runnumber);
	  if (!ok)
	    {
	      std::cerr << "UpdateTofSectorOffset: could not read run "
			<< runnumber << " sector " << isector
			<< " from dir " << cdir
			<< std::endl;	      
	    }
	  else
	    {
	      sec.SetValidityPeriod(*(start.get()),*(end.get()));
	      sec.SetDestination(emcManageable::kDB_Pg);
	      ok = dm->Write(sec);
	      if (!ok)
		{
		  std::cerr << "updatePbGlGains: could not upload "
			    << "for run " << runnumber
			    << " sector " << isector
			    << std::endl;
		}
	    }
	}
    }

  gSystem->FreeDirectory(dirp);

  return true;
}

//_____________________________________________________________________
bool
emcDB::UpdatePbGlGains(const string basedir)
{
  // Read PbGlGain ASCII files from disk and
  // write them into Objy.
  //
  // Disk structure is supposed to be :
  //
  // basedir/
  //   | XXXXX/
  //   |   | Gains /
  //   | XXXXY/
  //   |   | Gains/
  //   | XXXXZ/
  //   |   | Gains/
  //   |   |
  //
  // etc...

  bool kFaultTolerant = false; // exit on any error.

  PHTimeStamp tdummy(0);

  // Reads the basedir directory to find out the list
  // of run number to handle.
  void* pdir = gSystem->OpenDirectory(basedir.c_str());

  if (!pdir)
    return false;

  const char* cfilename;
  std::vector<int> runs;

  while ( ( cfilename = gSystem->GetDirEntry(pdir) ) != 0 )
    {
      std::string filename = std::string(cfilename);
      if ( filename.find("Run") < filename.size() )
        {
          runs.push_back(atoi(filename.substr(3).c_str()));
        }
    }

  // Be kind with the user and tell him how many runs
  // were found.
  cout << EMC_INFO_MSG << "Number of runs in " << basedir
       << " = " << runs.size() << endl;

  if (runs.empty())
    {
      cerr << EMC_INFO_MSG << " huh ? no run found ? Is the top directory "
	   << "correct ? " << endl;
      cerr << "I'm expecting something like " << endl;
      cerr << basedir << "/RunXXXX/Gains/..." << endl;
      cerr << basedir << "/RunXXXY/Gains/..." << endl;
      cerr << basedir << "/RunXXXZ/Gains/..." << endl;
      return false;
    }

  int maxrunnumber = 1500000;

  // Last run number put by hand.
  runs.push_back(maxrunnumber);

  // be sure run number are sorted.
  sort(runs.begin(), runs.end());

  // be sure we'll read relation run number <-> timestamp
  // from a file, not from DB (otherwise we might crash
  // if we're doing a test on a private Federation which
  // has no copy of the Run DB).
  initDBMS("Ascii");
  RunTimes* rt = RunTimesFactory::instance().create("Ascii");
  assert(rt != 0);
  rt->RunStart(1);

  // Just to ease the expert user's life, keep track
  // of "problematic" runs, i.e. those
  // whose start of validity period was not found.
  size_t irun;
  std::vector<int> pbruns;
  std::vector<int> pbruns_next;

  size_t nmax = runs.size();

  for ( irun = 0; irun < nmax - 1; irun++ )
    {
      PHTimeStamp start = rt->RunStart(runs[irun]);
      if (start.getTics() == 0)
        {
          pbruns.push_back(runs[irun]);
          pbruns_next.push_back(runs[irun + 1]);
        }
    }

  if (!pbruns.empty())
    {
      // report the problematic run numbers (with the intervals)
      std::cerr << "<E> Runs for which I did not find start date : ";
      for (irun = 0;irun < pbruns.size();irun++)
        {
          std::cerr << pbruns[irun] << "[ -" << pbruns_next[irun] << "] , ";
        }
      std::cerr << std::endl;
      std::cerr << " (" << pbruns.size() << " runs over "
		<< runs.size() << std::endl;
      std::cerr << "Correct this error before continuing\n"
	"Use emcDB --runtimes (with OO_FD_BOOT = PHENIX_FD_BOOT)\n"
	"to get an ASCII file containing run to timestamp relationship\n"
	"and complete this file for the runs mentionned above\n"
	"Then restart emcDB --updatePbGlGains\n" << std::endl;
      std::cerr << "Aborting now." << std::endl;
      // to avoid time gaps, we insure that we know all the dates we need.
      // if not, just exit.
      return false;

    }

  // Fine. Now to the real work.
  char des[80];
  char dirname[1024];
  bool ok;

  // Our beloved datamanager that will do the underlying work.
  emcDataManager* dm = emcDataManager::GetInstance();

  if (fDebug)
    {
      dm->SetVerboseLevel(10);
    }

  // Loop over all runs found in basedir.
  for ( irun = 0; irun < nmax - 1; irun++ )
    {

      sprintf(dirname, "%s/Run%d/", basedir.c_str(), runs[irun]);
      dm->SetSourceDir(dirname);
      // Let the user know where we are.
      std::cout << dirname << std::endl;

      // Loop over all PbGl fems.
      for ( int ifem = 108; ifem <= 171; ifem++)
        {

          // Create a gainFEM object, set the source
          // to file and destination to Objy.
          emcGainFEM gains(ifem);
          gains.SetSource(emcManageable::kFile_ASCII);
          gains.SetDestination(destination());

          // ask the DataManager to read this object for us.
          ok = dm->Read(gains, tdummy, ifem);

          if (ok)
            {
              // We got it from file, we now sets its validity
              // period, change its X parameter for all channels
              // (see emcTracedValue class.)
              // and write it into DB.

              PHTimeStamp start = rt->RunStart(runs[irun]);
              // end-of-validity of this set is the beginning
              // of next set, so there's no time-gap.
              PHTimeStamp end = rt->RunStart(runs[irun + 1]);

              assert(start.getTics() != 0);

              if ( runs[irun + 1] != maxrunnumber )
                {
                  gains.SetValidityPeriod(start, end);
                  sprintf(des, "RUNS[%06d-%06d[", runs[irun], runs[irun + 1]);
                }
              else
                {
                  end.setToFarFuture();
                  gains.SetValidityPeriod(start, end);
                  sprintf(des, "RUNS[%06d-[", runs[irun]);
                }

              if (fDebug)
                {
                  cout << des << " " << start << " " << end << endl;
                  cout << rt->RunStart(runs[irun]) << endl;
                }
              // Sets the description field of the gain
              // so that the description field of the pdbcalbank
              // will be meaningfull (e.g. when browsing the DB).
              gains.SetDescription(des);

              // Xmin and Xmax are the run number boundaries
              // for this calibration set.
              gains.SetXmin(runs[irun]);
              gains.SetXmax(runs[irun + 1]);

              // Short fix.
              UpdateXValue(static_cast<emcTracedFEM&>(gains), 0);

              // Let the DM do the job.
              ok = dm->Write(gains, tdummy);
              assert(ok == true);
            }
          else
            {
              std::cerr << "<E> Cannot read fem " << ifem << " for directory "
			<< dirname << std::endl;
              if (!kFaultTolerant)
                return false;
            }
        }

      if (!ok)
        {
          std::cerr << "<E> Cannot write"
		    << " into DB " << std::endl;
          if (!kFaultTolerant)
            return false;
        }
    }

  return true;
}

//_____________________________________________________________________________
void
emcDB::MakeRunTimes(int minrunnumber)
{
  initDBMS("Ascii");
  RunTimes* rt = RunTimesFactory::instance().create(fDBMS.c_str());
  if (!rt)
    {
      std::cerr << "emcDB::RunTimes: "
		<< "Could not get RunTimes object for DBMS="
		<< fDBMS
		<< std::endl;
      exit(1);
    }
  else
    {
      rt->MinRunNumber(minrunnumber);
      rt->Output();
    }
}

//_____________________________________________________________________________
bool
emcDB::ReadConfiguration(bool debug)
{
  char str[200];

  emcConfigurationFile config(fConfigurationFile.c_str());

  if (!config.IsValid())
    return false;

  config.Rewind();

  fPinNumbers.resize(EmcIndexer::MaxNumberOfFEMs(), 0);

  string s;
  string fem;
  string pinNumber;
  vector<string> split;
  int absPosition;

  while ( config.GetLine(str, 200) )
    {

      s = str;

      // skip comments
      if ( s.substr(0, 2) == "//" || s.substr(0, 1) == "#" )
        continue;

      if ( s.find("FEM.EMC") < s.size() )
        {

          Split(s, split);

          if (split.size() == 4)
            {
              fem = split[0];
              absPosition = AbsolutePosition(fem);
              pinNumber = split[3];
              if (absPosition >= 0)
                {
                  fPinNumbers[absPosition] = atoi(pinNumber.c_str());
                }
              if (debug)
                {
                  cout << fem << ":absPosition=" << absPosition
		       << ":pinNumber=" << (absPosition >= 0 ? fPinNumbers[absPosition] : -1) << endl;
                }

            }
        }

    }
  return true;
}

//_____________________________________________________________________________
void
emcDB::Split(const string& str, vector<string>& split)
{
  split.clear();
  string s = str;

  size_t i = 0;

  // Erase TABs and ,
  do
    {

      if (s[i] == '\t' || s[i] == ',')
        {
          s.erase(i, 1);
        }
      else
        {
          i++;
        }
    }
  while (i < s.size());

  // Erase multiple blanks
  i = 0;
  if (s.size() > 2)
    {
      do
        {
          if (s[i] == ' ' && s[i + 1] == ' ')
            {
              s.erase(i, 1);
            }
          else
            {
              i++;
            }
        }
      while (i < s.size() - 1);
    }

  // Now split on blanks

  vector<int> pos;

  for (i = 0;i < s.size();i++)
    {
      if (s[i] == ' ')
        pos.push_back(i);
    }

  pos.push_back(s.size());

  size_t p = 0;

  for (i = 0;i < pos.size();i++)
    {
      split.push_back(s.substr(p, pos[i] - p));
      p = pos[i] + 1;
    }
}

//_____________________________________________________________________________
string
emcDB::Version(void)
{
  //  return string("emcDB 1.25 (11 February 2003)");

  // fix emcOMCalFEM:: and emcOMCalibrationData::Write memory leak
  // fix emcOMCalFEM::getRealName memory leak.

  //  return string("emcDB 1.26 (14 February 2003)");
  // fix the compare for emcTracedFEM.

  //  return string("emcDB 1.27 (07 March 2003)");
  // fix the update to work even if source directory is not fully
  // specified (i.e. in cases like emcDB --update directory : previouly
  // it had to be speficied ./directory. Now the ./ is added by emcDB itself

  //  return string("emcDB 1.4 (26 January 2004)");
  // externalize Objy specifics (so we get plug/unplug them, as well
  // as postgres ones).

  //  return string("emcDB 1.5 (04 February 2004)");
  // revive --runtimes option.

  //  return string("emcDB 1.6 (13 February 2004)");
  // handle TofT0Bs flavour (together with old TofT0s)

  //  return string("emcDB 1.7 (23 March 2004)");
  // reconnect --updatePbGlGains

  //  return string("emcDB 2.0 (10 June 2004)");
  // make Postgres the default DBMS

  //  return string("emcDB 2.01 (01 September 2004)");
  // fixing --update for rejectlist

  // return string("emcDB 2.02 (05 October 2004)");
  // adding --update for TofSectorOffsets

  return string("emcDB 2.1 (27 January 2005)");
  // adding --dbname option
}
