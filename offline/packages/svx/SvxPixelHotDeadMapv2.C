// ===================
// FILE: SvxPixelHotDeadMapv2.C
// ===================

#include "SvxPixelHotDeadMapv2.h"
#include "SvxParameters.h"

#include "svxAddress.hh"

#include <PdbSvxPixelHotDeadChipMap.hh>
#include <PdbSvxPixelHotDeadPixelMap.hh>
#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <RunToTime.hh>

#include <phool.h>

#include <fstream>
#include <sstream>
#include <iostream>


using namespace std;

/**
 * @brief Class to manage VTX pixel hot/dead map 
 * - File or DB i/o
 * - Access status of chips or pixel channels
 * Corresponding class for stripixels is SvxHotDeadMap
 */

SvxPixelHotDeadMapv2::SvxPixelHotDeadMapv2(const int verbosityLevel) :
  verbosity(verbosityLevel),
  m_useChipMap(true),
  m_usePixelMap(true),
  reference_map_bankID_offset(0),
  blank_status(0),
  firstrun(0),
  lastrun(0)
{
  initializeMaps();
}


void SvxPixelHotDeadMapv2::setBlankStatus(const char blank)
{
  blank_status = blank;
  cout<<"blank_status = "<< static_cast<int> (blank_status) <<endl;
  for (int module=0; module<NMODULE; module++) {
    for (int ROC=0; ROC<NROC; ROC++) {
      for (int column=0; column<NCOLUMN; column++) {
        for (int row = 0; row<NROW; row++) {
          pixelmap[module][ROC][column][row]=blank_status;
        }
      }
    }
  }
}


void SvxPixelHotDeadMapv2::initializeChipMap()
{
  memset(chipmap,0,sizeof(chipmap));
}

void SvxPixelHotDeadMapv2::initializePixelMap(const int module, const int ROC)
{
  memset(pixelmap,0,sizeof(pixelmap));
}

void SvxPixelHotDeadMapv2::initializeMaps()
{
  initializeChipMap();

  // initialization of the maps
  for (int module=0; module<NMODULE; module++) {
    for (int ROC=0; ROC<NROC; ROC++) {
      for (int tile=0; tile<NTILE; tile++) {
        tilemap[module][ROC][tile]=-1;
      }
      
      initializePixelMap(module,ROC);
    }
  }
}

void SvxPixelHotDeadMapv2::Verbosity(const int level)
{
  if (level < 0 || level > 99999) {
    cerr << "Verbosity level out of range: " << level << endl;
    return;
  }

  verbosity = level;
}


bool SvxPixelHotDeadMapv2::writeChipMapToFile(const int runStart, const int runEnd, const string &filename)
{
  ofstream outfile;

  outfile.open(filename.c_str(),ios::out);
    
  if (!outfile) {
    cerr << PHWHERE << "Can't open output file " << filename << endl;
    return false;
  }

  for (int module=0; module<NMODULE; module++) {
    for (int ROC=0; ROC<NROC; ROC++) {
      int status = getChipStatus(module,ROC);
      if (status != PdbSvxPixelHotDeadChipMap::SVX_CHIP_NORMAL) {//only write abnormal chips
        if (verbosity > 0) {      
          cout << "Write:\t" << runStart << "\t" << runEnd << "\t" << module <<"\t" << ROC <<"\t" << status << endl;
        }
        outfile << runStart << "\t" << runEnd << "\t" << module <<"\t" << ROC <<"\t" << status << endl;
      }
    }
  }
  
  outfile.close();
  return true;
}



///
/// For entire modules and rocs
///
bool SvxPixelHotDeadMapv2::writePixelMapToFile(const int runStart, const int runEnd, const string &filename)
{
  ofstream outfile;

  outfile.open(filename.c_str(),ios::out);
    
  if (!outfile) {
    cerr << PHWHERE << "Can't open output file " << filename << endl;
    return false;
  }

  for (int module=0; module<NMODULE; module++) {
    for (int roc=0; roc<NROC; roc++) {
      for (int column=0; column<NCOLUMN; column++) {
        for (int row=0; row<NROW; row++) {
          int status = getPixelStatus(module,roc,column,row);
          if (status != PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_NORMAL) {//only write abnormal chips
            if (verbosity > 0) {      
              cout << "Write:\t" << runStart << "\t" << runEnd << "\t" << module <<"\t" << roc <<"\t"
                   << column << "\t" << row << "\t" << status << endl;
            }
            outfile << runStart << "\t" << runEnd << "\t" << module <<"\t" << roc <<"\t"
                    << column << "\t" << row << "\t" << status << endl;
          }
        }
      }
    }
  }

  outfile.close();
  return true;
}



///
/// For specific module and roc
///
bool SvxPixelHotDeadMapv2::writePixelMapToFile(const int runStart, const int runEnd, const int module, const int ROC, const string &filename)
{
  ofstream outfile;

  outfile.open(filename.c_str(),ios::out);
    
  if (!outfile) {
    cerr << PHWHERE << "Can't open output file " << filename << endl;
    return false;
  }

  for (int column=0; column<NCOLUMN; column++) {
    for (int row=0; row<NROW; row++) {
      int status = getPixelStatus(module,ROC,column,row);
      if (status != PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_NORMAL) {//only write abnormal chips
        if (verbosity > 0) {      
          cout << "Write:\t" << runStart << "\t" << runEnd << "\t" << module <<"\t" << ROC <<"\t" << column << "\t" << row << "\t" << status << endl;
        }
        outfile << runStart << "\t" << runEnd << "\t" << module <<"\t" << ROC <<"\t" << column << "\t" << row << "\t" << status << endl;
      }
    }
  }
  
  outfile.close();
  return true;
}

bool SvxPixelHotDeadMapv2::readChipMapFromFile(const string &filename)
{ 

  ifstream infile;

  infile.open(filename.c_str(),ios::in);

  if (!infile) {
    cout << PHWHERE << " Can't open input file " << filename << endl;
    return false;
  }

  firstrun = 0;//initialize
  lastrun = 0;//initialize
  int first = 1;
  int runTemp1;
  int runTemp2;
  int module;
  int ROC;
  int chanStatus;
  
  string line;
  while (getline(infile, line)) {
    istringstream iss_line(line.data());
    iss_line >> runTemp1 >> runTemp2 >> module >> ROC >> chanStatus;

    if (verbosity > 0)
      cout << "Read module " << module
           << ", ROC " << ROC
           << ", chanStatus " << chanStatus << endl;
      
    // Add this record to the memory-resident map 
    if (rangeCheck(module,ROC)) {

      setChipStatus(module,ROC,chanStatus);
      if (first)
	{
	  firstrun = runTemp1;
	  lastrun = runTemp2;
	  first = 0;
	}
      else
	{
	  if (firstrun != runTemp1 || lastrun != runTemp2)
	    {
	      cout << PHWHERE << "Inconsistent run numbers in " << filename << endl;
	      cout << "first run: " << firstrun << ", read: " << runTemp1 << endl;
	      cout << "end run: " << lastrun << ", read: " << runTemp2 << endl;
	      infile.close();
	      return false;
	    }
	}

      if (verbosity > 90) {
        cout << "chanStatus=" << chanStatus << endl;
        cout << "getChipStatus() = " << getChipStatus(module,ROC) << endl;
      }
    }
  }

  infile.close();

  return true;
}



///
/// For specific module and roc
///
bool SvxPixelHotDeadMapv2::readPixelMapFromFile(const int mod, const int chip, const string &filename)
{ 

  ifstream infile;

  infile.open(filename.c_str(),ios::in);

  if (!infile) {
    cerr << "Can't open input file " << filename << endl;
    return false;
  }

  firstrun = 0;//initialize
  lastrun = 0;//initialize
  int run0;
  int run1;
  int module;
  int ROC;
  int column;
  int row;
  int chanStatus;
  
  string line;
  while (getline(infile, line)) {
    istringstream iss_line(line.data());
    iss_line >> run0 >> run1 >> module >> ROC >> column >> row >> chanStatus;


    if ((mod != module)||(chip != ROC)) {
      cerr << "SvxPixelHotDeadMapv2::readPixelMapFromFile inconsistent chip of map and chip in map-file" << chip << " " << ROC << endl;
      return false;
    }

    if (verbosity > 0)
      cout << "Read module " << module
           << ", ROC " << ROC
           << ", column " << column
           << ", row " << row
           << ", chanStatus " << chanStatus << endl;
      
    // Add this record to the memory-resident map 
    if (rangeCheck(module,ROC,column,row)) {

      int status=PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_DEAD;//default is dead

      //conversion from status in the text to official flag
      if (chanStatus == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_DEAD) {//DEAD
        status = PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_DEAD;
      } else if (chanStatus == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_HOT) {//HOT
        status = PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_HOT;
      } else if (chanStatus == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_NORMAL) {//NORMAL
        status = PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_NORMAL;
      }

      setPixelStatus(module,ROC,column,row,status);
      if (firstrun == 0) {
        firstrun = run0;
        lastrun   = run1;
      }

      if (verbosity > 90) {
        cout << "chanStatus=" << chanStatus << endl;
        cout << "getPixelStatus() = " << getPixelStatus(module,ROC,column,row) << endl;
      }
    } else {
      if (verbosity > 90) {
        cerr << "out of range" << endl;
      }
    }
  }

  infile.close();

  return true;
}



///
/// For entire modules and rocs
///
bool SvxPixelHotDeadMapv2::readPixelMapFromFile(const string &filename)
{ 

  ifstream infile;

  infile.open(filename.c_str(),ios::in);

  if (!infile) {
    cerr << "Can't open input file " << filename << endl;
    return false;
  }

  firstrun = 0;//initialize
  lastrun = 0;//initialize
  int first = 1;
  int run0;
  int run1;
  int module;
  int ROC;
  int column;
  int row;
  int chanStatus;
  
  string line;
  while (getline(infile, line)) {
    istringstream iss_line(line.data());
    iss_line >> run0 >> run1 >> module >> ROC >> column >> row >> chanStatus;

    if (verbosity > 0)
      cout << "Read module " << module
           << ", ROC " << ROC
           << ", column " << column
           << ", row " << row
           << ", chanStatus " << chanStatus << endl;
      
    // Add this record to the memory-resident map 
    if (rangeCheck(module,ROC,column,row)) {

      setPixelStatus(module,ROC,column,row,chanStatus);
      if (first)
	{
	  firstrun = run0;
	  lastrun = run1;
	  first = 0;
	}
      else
	{
	  if (firstrun != run0 || lastrun != run1)
	    {
	      cout  << PHWHERE << "Inconsistent run numbers in " << filename << endl;
	      cout << "first run: " << firstrun << ", read: " << run0 << endl;
	      cout << "end run: " << lastrun << ", read: " << run1 << endl;
	      infile.close();
	      return false;
	    }
	}
      if (verbosity > 90) {
        cout << "chanStatus=" << chanStatus << endl;
        cout << "getPixelStatus() = " << getPixelStatus(module,ROC,column,row) << endl;
      }
    } else {
      if (verbosity > 90) {
        cerr << "out of range" << endl;
      }
    }
  }

  infile.close();

  return true;
}

bool SvxPixelHotDeadMapv2::readPixelMapsFromFile(int &runStart, int &runEnd, std::string ref_file, std::string diff_file)
{
  // Expected columns in file 
  int run0, run1, mod, roc, col, row, status; 
  string line;
  runStart = runEnd = 0;
  ifstream pixref(ref_file.c_str());
  ifstream pixdif(diff_file.c_str());

  // Default is NORMAL. Text files list bad pixels.
  for (mod=0; mod<NMODULE; mod++) {
    for (roc=0; roc<NROC; roc++) {
      for (col=0; col<NCOLUMN; col++) {
        for (row=0; row<NROW; row++) {
          setPixelStatus(mod,roc,col,row,PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_NORMAL);
        }
      }
    }
  }

  if (pixref)
    cout << PHWHERE << "Reading pixel layer pixel reference input file " << ref_file << endl;
  else
    cerr << PHWHERE << "Can't open pixel reference input file " << ref_file << endl;

  if (pixdif)
    cout << PHWHERE << "Reading pixel layer pixel diff input file " << diff_file << endl;
  else
    cerr << PHWHERE << "Can't open pixel diff input file " << diff_file << endl;

  if (!pixref || !pixdif) {
    return false;
  }

  // 1. Update map from ref file.
  while (getline(pixref, line)) {
    istringstream iss_line(line.data());
    iss_line >> run0 >> run1 >> mod >> roc >> col >> row >> status;
    if (rangeCheck(mod,roc,col,row)) {
      setPixelStatus(mod,roc,col,row,status);
    }
  }

  // 2. Update map from diff file.
  while (getline(pixdif, line)) {
    istringstream iss_line(line.data());
    iss_line >> run0 >> run1 >> mod >> roc >> col >> row >> status;
    if (rangeCheck(mod,roc,col,row)) {
      if (runStart == 0) {
        runStart = run0;
        runEnd   = run1;
      }
      setPixelStatus(mod,roc,col,row,status);
    }
  }
  
  return true;
}


void SvxPixelHotDeadMapv2::setPixelStatus(const int module,
                                          const int ROC,
                                          const int column,
                                          const int row,
                                          const int status)
{
  // Add this record to the memory-resident map
  if (rangeCheck(module,ROC,column,row)) {
    pixelmap[module][ROC][column][row] = (signed char)status;
  }
}



void SvxPixelHotDeadMapv2::setRawPixelStatus(const int module,
                                             const int ROC,
                                             const int column,
                                             const int row,
                                             const signed char rawStatus)
{
  // Add this record to the memory-resident map
  if (rangeCheck(module,ROC,column,row)) {
    pixelmap[module][ROC][column][row] = rawStatus;
  }
}


void SvxPixelHotDeadMapv2::setPixelStatus(const int layer,
                                          const int ladder,
                                          const int south_north,
                                          const int ROC,
                                          const int column,
                                          const int row,
                                          const int status)
{
  int module = svxAddress::getPixelModuleIDSouthNorth(layer,ladder,south_north);
  setPixelStatus(module,ROC,column,row,status);
}

void SvxPixelHotDeadMapv2::setChipStatus(const int module,
                                         const int ROC,
                                         const int status)
{
  if (rangeCheck(module,ROC)) {
    chipmap[module][ROC] = (signed char)status;
  }
}

void SvxPixelHotDeadMapv2::setRawChipStatus(const int module,
                                            const int ROC,
                                            const signed char status)
{
  if (rangeCheck(module,ROC)) {
    chipmap[module][ROC] = status;
  }
}

bool SvxPixelHotDeadMapv2::readFromDatabase(const int run, const bool read_reference) {
  bool chipStatus = readChipMapFromDatabase(run);
  bool pixelStatus = false;

  if(read_reference==true){ readReferencePixelMapFromDatabase(run); }
  pixelStatus = readPixelMapFromDatabase(run);
  if (!pixelStatus) {
    cerr << "readFroMDatabase, failed to read pixel map = " << endl;
  }

  return (chipStatus & pixelStatus);
}

bool SvxPixelHotDeadMapv2::readChipMapFromDatabase(const int run)
{
  if ((run<0)||(run>99999999)) {
    cerr << "SvxPixelHotDeadMapv2::readChipMapFromDatabase Strange run " << endl;
    cerr << "run = " << run << endl;
    return false;
  }
  bool success = true;
  PdbCalBank *svxBank = NULL;
  const char *tableName = "svxpixelhotdeadchipmap";
  PdbBankID bankID;

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (!application->startRead()) {
    application->abort();
    std::cerr << PHWHERE
              << " ERROR -> Transaction aborted. Database NOT available."
              << std::endl;
    return false;
  }
  
  bankID.setInternalValue(run);//important!!
  RunToTime *rt = RunToTime::instance();
  PHTimeStamp *Tsearch = rt->getBeginTime(run);
  
  if (Tsearch == NULL) {
    cerr << "SvxPixelHotDeadMapv2::readChipMapFromDatabase> Error Tsearch==null" << endl;
    cerr << "run = " << run << endl;
    return false;
  }
  
  const char *bankName = "PdbSvxPixelHotDeadMapChipBank";
  

  if (verbosity>90) {
    cout << "tableName = " << tableName << endl;
    cout << "bankID = " << bankID.getInternalValue() << endl;
    cout << "Tsearch: " << *Tsearch << endl;
  }

  svxBank = bankManager->fetchBank(bankName, bankID,
                                   tableName, *Tsearch);

  if (svxBank) {

    unsigned numRecords = (unsigned)svxBank->getLength();

    if (verbosity>90) {
      cout << "readChipMapFromDatabase numRecords = " << numRecords << endl;
    }
    
    for (unsigned recordNumber=0; recordNumber<numRecords; recordNumber++) {
      PdbSvxPixelHotDeadChipMap *rec =
        (PdbSvxPixelHotDeadChipMap *) & (svxBank->getEntry(recordNumber));
      importChipMap(*rec);
    }

  } else {
    cerr << PHWHERE << " ERROR -> bankManager returned null pointer."<<endl;
    success = false;
  }

  if (success) {
    application->commit();
    if (verbosity>90) {
      cout << "readChipMapFromDatabase commit()" << endl;
    }
  }
  else {
    application->abort();
    if (verbosity>90) {
      cout << "readChipMapFromDatabase abort()" << endl;
    }
  }
  
  delete svxBank;

  delete Tsearch;
  
  return success;
}



bool SvxPixelHotDeadMapv2::readReferencePixelMapFromDatabase(const int run)
{
  setReferenceOffset();
  bool status = readPixelMapFromDatabase(run);
  setDifferenceOffset();
  return status;
}


///
/// For entire modules and rocs
///
bool SvxPixelHotDeadMapv2::readPixelMapFromDatabase(const int run)
{
  if ((run<0)||(run>99999999)) {
    cerr << "SvxPixelHotDeadMapv2::readPixelMapFromDatabase Strange run " << endl;
    cerr << "run = " << run << endl;
    return false;
  }

  bool success = true;

  PdbCalBank *svxBank = NULL;
  const char *tableName = "svxpixelhotdeadpixelmap2";
  PdbBankID bankID;
  
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (!application->startRead()) {
    application->abort();
    std::cerr << PHWHERE
              << " ERROR -> Transaction aborted. Database NOT available."
              << std::endl;
    return false;
  }
  
  bankID.setInternalValue( reference_map_bankID_offset );//important!!
  RunToTime *rt = RunToTime::instance();
  PHTimeStamp *Tsearch = rt->getBeginTime(run);
  //  PHTimeStamp *Tstop = rt->getEndTime(run);
  
  if (Tsearch == NULL) {
    cerr << "SvxPixelHotDeadMapv2::readPixelMapFromDatabase> Error Tsearch==null" << endl;
    cerr << "run = " << run << endl;
    return false;
  }
  
  const char *bankName = "PdbSvxPixelHotDeadMapPixelBank";
      
  if (verbosity>90) {
    cout << "tableName = " << tableName << endl;
    cout << "bankID = " << bankID.getInternalValue() << endl;
    cout << "Tsearch: " << *Tsearch << endl;
  }
      
      
  svxBank = bankManager->fetchBank(bankName, bankID,
                                   tableName, *Tsearch);

  if (svxBank) {
    unsigned numRecords = (unsigned)svxBank->getLength();
        
    for (unsigned recordNumber=0; recordNumber<numRecords; recordNumber++) {
      PdbSvxPixelHotDeadPixelMap *rec =
        (PdbSvxPixelHotDeadPixelMap *) & (svxBank->getEntry(recordNumber));
      importPixelMap(*rec);
    }
  }
  else {
    cerr << PHWHERE << " ERROR -> bankManager returned null pointer."<<endl;
    success = false;
  }
      
  if (success) {
    application->commit();
    if (verbosity>90) {
      cout << "readPixelMapFromDatabase commit()" << endl;
    }
  }
  else {
    application->abort();
    if (verbosity>90) {
      cout << "readPixelMapFromDatabase abort()" << endl;
    }
  }
      
  delete svxBank;

  delete Tsearch;

  return success;
}


bool SvxPixelHotDeadMapv2::writeChipMapToDatabase(const int run)
{
  if ((run<0)||(run>99999999)) {
    cerr << "SvxPixelHotDeadMapv2::writeChipMapToDatabase Strange run" << endl;
    cerr << "run = " << run << endl;
    return false;
  }

  bool success = true;
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  PdbCalBank *svxBank = NULL;

  if (application->startUpdate()) {
    RunToTime *rt = RunToTime::instance();
    PHTimeStamp *Tstart = rt->getBeginTime(run);
    PHTimeStamp *Tstop  = rt->getEndTime(run);

    if (Tstart == NULL) {
      cerr << "SvxPixelHotDeadMapv2::writeChipMapToDatabase> Warning Tstart==NULL, taking current time" << endl;
      return false;
    }
    if (Tstop == NULL) {
      cerr << "SvxPixelHotDeadMapv2::writeChipMapToDatabase> Warning Tstop==NULL, taking current time" << endl;
      delete Tstart;
      return false;
    }

    ostringstream description;
    description << "svxpixel-hotdead-chipmap"
		<< setfill('0') << setw(10) << run;

    const char *bankName = "PdbSvxPixelHotDeadChipMapBank";
    const char *tableName = "svxpixelhotdeadchipmap";
      
    PdbBankID bankID;

    bankID.setInternalValue(run);

    cout << "tableName = " << tableName << endl;
    cout << "description = " << description.str() << endl;
    cout << "bankID = " << bankID.getInternalValue() << endl;
    cout << "Validity range: " << *Tstart << " - " << *Tstop << endl;

      
    //here we have correspondence between PdbSvxPixelHotDeadMap structure and bank record length
    svxBank = bankManager->createBank(bankName,
                                      bankID, description.str().c_str(),
                                      *Tstart, *Tstop, tableName);


    if (svxBank) {
      // The number of records is same as the number of data in the map array
      unsigned numRecords = (unsigned)countBadChips();
  
      svxBank->setLength(numRecords);
  
      // And now this loop writes out the data in the map

      unsigned recordNumber = 0;
  
      for (int module = 0; module < NMODULE; module++) {
        for (int ROC = 0; ROC < NROC; ROC++) {
          int status = getChipStatus(module,ROC);
          if (status != PdbSvxPixelHotDeadChipMap::SVX_CHIP_NORMAL) {
            PdbSvxPixelHotDeadChipMap *rec =
              (PdbSvxPixelHotDeadChipMap *) & (svxBank->getEntry(recordNumber));
            exportChipMap(module,ROC,*rec);
            recordNumber++;
          }
        }
      }
    }
    else {
      cerr << PHWHERE << " ERROR: can not create svxBank." << endl;
      success = false;
    }

    delete Tstart;
    delete Tstop;
  }
  else {
    cerr << PHWHERE << "ERROR: Database not writable, aborting." 
         << endl;
    success = false;
  }

  if (verbosity>90) {
    cout << "writeChipMapToDatabase success = " << success << endl;
  }

  if (success) {
    application->commit();
    if (verbosity>90) {
      cout << "writeChipMapToDatabase commit()" << endl;
    }
  }
  else {
    application->abort();
    if (verbosity>90) {
      cout << "writeChipMapToDatabase abort()" << endl;
    }
  }

  delete svxBank;

  return success;
}



bool SvxPixelHotDeadMapv2::writeReferencePixelMapToDatabase(const int runstart, const int runend)
{
  setReferenceOffset();
  bool write_status = writePixelMapToDatabase(runstart, runend);
  setDifferenceOffset();
  return write_status;
}

bool
SvxPixelHotDeadMapv2::writePixelMapToDatabase(const int runstart, const int runend)
{
  if ((runend < 0) || (runend > 99999999))
    {
      cerr << "SvxPixelHotDeadMapv2::writePixelMapToDatabase Strange run" << endl;
      cerr << "runend = " << runend << endl;
      return false;
    }

  bool success = true;
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  PdbCalBank *svxBank = NULL;

  if (application->startUpdate())
    {

      RunToTime *rt = RunToTime::instance();
      PHTimeStamp *Tstart = rt->getBeginTime(runstart);
      PHTimeStamp *Tstop  = rt->getEndTime(runend);

      if (Tstart == NULL)
	{
	  cout << PHWHERE << " ERROR, Could not look up starttime for run " << runstart << endl;
	  return false;
	}
      if (Tstop == NULL)
	{
	  cout << PHWHERE << " ERROR, Could not look up endtime for run " << runend << endl;
	  delete Tstart;
	  return false;
	}

      ostringstream description;
      description << "svxpixel-hotdead-pixelmap"
		  << setfill('0') << setw(6) << runstart
		  << "-"
		  << setfill('0') << setw(6) << runend;

      const char *tableName = "svxpixelhotdeadpixelmap2";
      const char *bankName = "PdbSvxPixelHotDeadPixelMapBank";

      PdbBankID bankID;
      bankID.setInternalValue( reference_map_bankID_offset );

      cout << "tableName = " << tableName << endl;
      cout << "description = " << description.str() << endl;
      cout << "bankID = " << bankID.getInternalValue() << endl;
      cout << "Validity range: " << *Tstart << " - " << *Tstop << endl;


      //here we have correspondence between PdbSvxPixelHotDeadMapv2 structure and bank record length
      svxBank = bankManager->createBank(bankName,
					bankID, description.str().c_str(),
					*Tstart, *Tstop, tableName);

      if (svxBank)
	{
	  // The number of records is same as the number of data in the map array
	  unsigned int numRecords = (unsigned int) countBadPixels();

	  svxBank->setLength(numRecords);

	  unsigned int recordNumber = 0;

	  for (int module = 0; module < NMODULE; module++)
	    {
	      for (int ROC = 0; ROC < NROC; ROC++)
		{
		  for (int column = 0; column < NCOLUMN; column++)
		    {
		      for (int row = 0; row < NROW; row++)
			{
			  int status = getPixelStatus(module, ROC, column, row);
			  if (status != blank_status)
			    {
			      PdbSvxPixelHotDeadPixelMap *rec =
				(PdbSvxPixelHotDeadPixelMap *) & (svxBank->getEntry(recordNumber));

			      exportPixelMap(module, ROC, column, row, *rec);
			      ++recordNumber;
			    }
			}
		    }
		}
	    } // end module loop
	  // update internal begin/end run with what we wrote to the DB
	  firstrun = runstart;
	  lastrun = runend;
	}
      else
	{
	  cerr << PHWHERE << " ERROR: can not create svxBank." << endl;
	  success = false;
	}

      delete Tstart;
      delete Tstop;
    }
  else
    {
      cerr << PHWHERE << "ERROR: Database not writable, aborting."
	   << endl;
      success = false;
    }

  if (verbosity > 90)
    {
      cout << "writePixelMapToDatabase success = " << success << endl;
    }

  if (success)
    {
      application->commit();
      if (verbosity > 90)
	{
	  cout << "writePixelMapToDatabase commit()" << endl;
	}
    }
  else
    {
      application->abort();
      if (verbosity > 90)
	{
	  cout << "writePixelMapToDatabase abort()" << endl;
	}
    }

  delete svxBank;

  return success;
}



void SvxPixelHotDeadMapv2::print() const 
{
  for (int module = 0; module < NMODULE; module++) {
    for (int ROC = 0; ROC < NROC; ROC++) {
      for (int column = 0; column < NCOLUMN; column++) {
        for (int row = 0; row < NROW; row++) {
          int status = getStatus(module,ROC,column,row);
	  
          string statusString =
            (status == PdbSvxPixelHotDeadChipMap::SVX_CHIP_DEAD) ? "chip-dead" :
            (status == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_NORMAL) ? "normal" :
            (status == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_DEAD) ? "dead" :
            (status == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_VERYCOLD) ? "very-cold" :
            (status == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_COLD) ? "cold" :
            (status == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_UNSTABLE) ? "unstable" :
            (status == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_HOT)  ? "hot" :
            (status == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_VERYHOT)  ? "very-hot" :
            (status == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_UNKNOWN) ? "unknown" :
            (status == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_OUTOFRANGE) ? "outofrange" : "not-found";
	  
          cout << "Module " << module
               << ", ROC " << ROC
               << ", column " << column
               << ", row " << row
               << ": " << statusString << endl;
        }
      }
    }
  }
}

//combined chip/pixel status
int SvxPixelHotDeadMapv2::getStatus(const int module,
                                    const int ROC,
                                    const int column,
                                    const int row) const
{
  int chipStatus = (m_useChipMap) ? getChipStatus(module,ROC) 
    : PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_NORMAL; // force NORMAL as default if useChipMap=false

  int status = PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_UNKNOWN;
  if (chipStatus != PdbSvxPixelHotDeadChipMap::SVX_CHIP_NORMAL) {
    status = chipStatus;
  } 
  else { // use chipmap and pixelmap
    if (m_usePixelMap) status = getPixelStatus(module,ROC,column,row);
    else              status = PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_NORMAL; // force NORMAL as default if usePixelMap=false
  }

  return status;
}


int SvxPixelHotDeadMapv2::getStatus(const int layer,
                                    const int ladder,
                                    const int south_north,
                                    const int ROC,
                                    const int column,
                                    const int row) const
{
  int module = svxAddress::getPixelModuleIDSouthNorth(layer,ladder,south_north);

  return (getStatus(module,ROC,column,row));
}


int SvxPixelHotDeadMapv2::getChipStatus(const int module,
                                        const int ROC) const
{                         

  if (rangeCheck(module,ROC)) {
    char cStatus = chipmap[module][ROC];
    if (verbosity>90) {
      cout << "getChipStatus (int)cStatus = " << (int)cStatus<< endl;
    }
    if (cStatus == (char)PdbSvxPixelHotDeadChipMap::SVX_CHIP_NORMAL) {
      return PdbSvxPixelHotDeadChipMap::SVX_CHIP_NORMAL;
    } else if (cStatus == (char)PdbSvxPixelHotDeadChipMap::SVX_CHIP_DEAD) {
      return PdbSvxPixelHotDeadChipMap::SVX_CHIP_DEAD;
    } else if (cStatus == (char)PdbSvxPixelHotDeadChipMap::SVX_CHIP_HOT) {
      return PdbSvxPixelHotDeadChipMap::SVX_CHIP_HOT;
    } else if (cStatus == (char)PdbSvxPixelHotDeadChipMap::SVX_CHIP_OUTOFRANGE) {
      return PdbSvxPixelHotDeadChipMap::SVX_CHIP_OUTOFRANGE;
    } else if (cStatus == (char)PdbSvxPixelHotDeadChipMap::SVX_CHIP_UNKNOWN) {
      return PdbSvxPixelHotDeadChipMap::SVX_CHIP_UNKNOWN;
    } else {
      return PdbSvxPixelHotDeadChipMap::SVX_CHIP_UNKNOWN;
    }
  } else {
    return PdbSvxPixelHotDeadChipMap::SVX_CHIP_OUTOFRANGE;
  }
} 


int SvxPixelHotDeadMapv2::getPixelStatus(const int module,
                                         const int ROC,
                                         const int column,
                                         const int row) const 
{                       

  if (rangeCheck(module,ROC,column,row)) {
    char cStatus = pixelmap[module][ROC][column][row];
    return cStatus;
  }
  else {
    return PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_OUTOFRANGE;
  }
}

bool  SvxPixelHotDeadMapv2::isPixelOkForClustering(const int module,
                                                   const int ROC,
                                                   const int column,
                                                   const int row) const
{
  if (rangeCheck(module,ROC)) {
    int status = getStatus(module,ROC,column,row);
    if ((char)status == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_NORMAL ||
        (char)status == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_COLD ||
        (char)status == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_HOT ) {
      return true;
    }
    else {
      return false;
    }
    
  }
  else {
    return false; //out of range
  }
      

}


void SvxPixelHotDeadMapv2::setTileMap()
{
  for (int module=0; module<NMODULE; module++) {
    for (int ROC=0; ROC<NROC; ROC++) {
      for (int tile=0; tile<NTILE; tile++) {
        setTileGoodFrac(module, ROC, tile);
      }
    }
  }
}

void SvxPixelHotDeadMapv2::setTileGoodFrac(const int module,
                                           const int ROC,
                                           const int tile)
{
  // Tiles are 8 columns by 64 rows = 512 pixel channels.
  int rowx[4] = {192,128,64,0};
  int colz[4] = {0,8,16,24};
  int row1 = rowx[tile/4], col1 = colz[tile%4]; // first row, col
  int row2 = row1 + 64,    col2 = col1 + 8;     // upper (excluded) bounds
  double goodfrac = 0;

  for (int row=row1; row<row2; ++row) {
    for (int col=col1; col<col2; ++col) {
      if (rangeCheck(module,ROC,col,row)) {
        //int st = getStatus(module, ROC, col, row);
        //if (st==PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_NORMAL) {
        if (isPixelOkForClustering(module, ROC, col, row)) {
          goodfrac += 1./512;
        }
      }
      else {
        cerr << PHWHERE << "Out of bounds: module, ROC, tile " 
             << module << ", " << ROC << ", " << tile << endl;
        return;
      }
    }
  }
  
  tilemap[module][ROC][tile] = goodfrac;
}

float SvxPixelHotDeadMapv2::getTileGoodFrac(const int module,
                                            const int ROC,
                                            const int tile) const
{
  if (!rangeCheck(module, ROC, tile)) {
    cerr << PHWHERE << "Out of bounds: module, ROC, tile " 
         << module << ", " << ROC << ", " << tile << endl;
    return -1.0;  
  }
  
  float f = tilemap[module][ROC][tile];

  if (f<0 || f>1)
    cout << "WARNING: good pixel fraction outside [0,1]" << endl;

  return f;
}


bool SvxPixelHotDeadMapv2::rangeCheck(const int module, const int ROC, const int column, const int row) const 
{
  if ((0<=module)&&(module<NMODULE)&&(0<=ROC)&&(ROC<NROC)
      &&(0<=column)&&(column<NCOLUMN)&&(0<=row)&&(row<NROW)) {
    return true;
  } else {
    return false;
  }
}

bool SvxPixelHotDeadMapv2::rangeCheck(const int module, const int ROC) const 
{
  if ((0<=module)&&(module<NMODULE)&&(0<=ROC)&&(ROC<NROC)) {
    return true;
  } else {
    return false;
  }
}

bool SvxPixelHotDeadMapv2::rangeCheck(const int module, const int ROC, const int tile) const 
{
  if ((0<=module) && (module<NMODULE) &&
      (0<=ROC) && (ROC<NROC) &&
      (0<=tile) && (tile<NTILE)) {
    return true;
  } else {
    return false;
  }
}

unsigned int SvxPixelHotDeadMapv2::countBadChips() const 
{
  unsigned int ncount = 0;
  for (int module=0;module<NMODULE;module++) {
    for (int ROC=0;ROC<NROC;ROC++) {
      if (chipmap[module][ROC] != PdbSvxPixelHotDeadChipMap::SVX_CHIP_NORMAL) {
        ++ncount;
      }
    }
  }
  return ncount;
}

unsigned int SvxPixelHotDeadMapv2::countBadPixels() const 
{
  unsigned int ncount = 0;
  for (int module = 0; module < NMODULE; module++) {
    for (int ROC = 0; ROC < NROC; ROC++) {
      ncount += countBadPixels(module, ROC);
    }
  }
  return ncount;
}

unsigned int SvxPixelHotDeadMapv2::countBadPixels(const int module, const int ROC) const 
{
  unsigned int ncount = 0;
  for (int column=0;column<NCOLUMN;column++) {
    for (int row=0;row<NROW;row++) {
      if (pixelmap[module][ROC][column][row] != blank_status) {
        ++ncount;
      }
    }
  }
  return ncount;
}

void SvxPixelHotDeadMapv2::importChipMap(const PdbSvxPixelHotDeadChipMap &rhs) {
  int module = rhs.getModule();
  int ROC    = rhs.getROC();
  int status = rhs.getStatus();
  if (verbosity>90) {
    cout << "importChipMap, module, ROC, status = " << module << " " << ROC << " " << status << endl;
  }

  setChipStatus(module,ROC,status);
}

void SvxPixelHotDeadMapv2::importPixelMap(const PdbSvxPixelHotDeadPixelMap &rhs) {
  int module = rhs.getModule();
  int ROC = rhs.getROC();
  int column = rhs.getColumn();
  int row = rhs.getRow();
  int status = rhs.getStatus();
  setPixelStatus(module,ROC,column,row,status);
}

void SvxPixelHotDeadMapv2::exportChipMap(const int module, const int ROC, PdbSvxPixelHotDeadChipMap &rhs) {
  rhs.setRawStatus(module,ROC,chipmap[module][ROC]);
}

void SvxPixelHotDeadMapv2::exportPixelMap(const int module, const int ROC, const int column, const int row, PdbSvxPixelHotDeadPixelMap &rhs)
{
  rhs.setRawStatus(module, ROC, column, row, pixelmap[module][ROC][column][row]);
  return;
}


void SvxPixelHotDeadMapv2::printBadPixels(const int module, const int roc) const
{
  PdbSvxPixelHotDeadPixelMap pmap;
  
  for (int column = 0; column < NCOLUMN; column++)
  {
    for (int row = 0; row < NROW; row++)
    {
      int status = (int)(pixelmap[module][roc][column][row]);
      //       int status = getPixelStatus(module,roc,column,row);
      if(status == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_NORMAL){continue;}
      pmap.setStatus(module, roc, column, row, status);
      pmap.print();
    }
  }
}

int
SvxPixelHotDeadMapv2::ComparePixelMaps(const SvxPixelHotDeadMapv2 &rhs) const
{
  int ndiff = 0;
  for (int module=0; module<NMODULE; module++) 
    {
      for (int ROC=0; ROC<NROC; ROC++) 
	{
	  for (int column=0; column<NCOLUMN; column++) 
	    {
	      for (int row = 0; row<NROW; row++) 
		{
		  if (pixelmap[module][ROC][column][row] != rhs.getPixelStatus(module,ROC,column,row))
		    {
		      ndiff++;
		    }
		}
	    }
	}
    }
  return ndiff;
}

int
SvxPixelHotDeadMapv2::CompareChipMaps(const SvxPixelHotDeadMapv2 &rhs) const
{
  int ndiff = 0;
  for (int module=0; module<NMODULE; module++) 
    {
      for (int ROC=0; ROC<NROC; ROC++) 
	{
	  if (chipmap[module][ROC] != rhs.getChipStatus(module,ROC))
	    {
	      ndiff++;
	    }
	}
    }
  return ndiff;
}
