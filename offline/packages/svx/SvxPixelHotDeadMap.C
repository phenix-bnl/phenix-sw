// ===================
// FILE: SvxPixelHotDeadMap.C
// ===================

#include <SvxPixelHotDeadMap.h>
#include <SvxParameters.h>

#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <RunToTime.hh>

#include <cassert>
#include <climits>
#include <cstdlib>
#include <fstream>
#include <sstream>
#include <iostream>

#include <svxAddress.hh>

using namespace std;

/**
 * @brief Class to manage VTX pixel hot/dead map 
 * - File or DB i/o
 * - Access status of chips or pixel channels
 * Corresponding class for stripixels is SvxHotDeadMap
 */

SvxPixelHotDeadMap::SvxPixelHotDeadMap(const int verbosityLevel) : reference_map_bankID_offset(0), blank_status(0)
{
  Verbosity(verbosityLevel);
  initializeMaps();

  m_useChipMap = true;
  m_usePixelMap = true;
}


void SvxPixelHotDeadMap::setBlankStatus(char blank)
{
  blank_status = blank;
  cout<<"blank_status = "<<(int)blank_status<<endl;
  for (int module=0; module<NMODULE; module++) {
    for (int ROC=0; ROC<NROC; ROC++) {
      for (int column=0; column<NCOLUMN; column++) {
        for (int row = 0; row<NROW; row++) {
          map[module][ROC][column][row]=blank_status;
        }
      }
    }
  }
}


void SvxPixelHotDeadMap::initializeChipMap()
{
  char c =0;
  for (int module=0; module<NMODULE; module++) {
    for (int ROC=0; ROC<NROC; ROC++) {
      chipmap[module][ROC]=c;      
    }
  }
}

void SvxPixelHotDeadMap::initializePixelMap(int module, int ROC)
{
  char c = 0;
  for (int column=0; column<NCOLUMN; column++) {
    for (int row = 0; row<NROW; row++) {
      map[module][ROC][column][row]=c;
    }
  }
}

void SvxPixelHotDeadMap::initializeMaps()
{
  char c = 0;
  // initialization of the maps
  for (int module=0; module<NMODULE; module++) {
    for (int ROC=0; ROC<NROC; ROC++) {
      chipmap[module][ROC]=c;

      for (int tile=0; tile<NTILE; tile++) {
  tilemap[module][ROC][tile]=-1;
      }
      
      for (int column=0; column<NCOLUMN; column++) {
  for (int row = 0; row<NROW; row++) {
    map[module][ROC][column][row]=c;
  }
      }
    }
  }
}

SvxPixelHotDeadMap::~SvxPixelHotDeadMap()
{
}

void SvxPixelHotDeadMap::Verbosity(const int level)
{
  if (level < 0 || level > 99) {
    cerr << "Verbosity level out of range: " << level << endl;
    return;
  }

  verbosity = level;
}


bool SvxPixelHotDeadMap::writeChipMapToFile(const int runStart, const int runEnd, string filename)
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
bool SvxPixelHotDeadMap::writePixelMapToFile(const int runStart, const int runEnd, string filename)
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
bool SvxPixelHotDeadMap::writePixelMapToFile(const int runStart, const int runEnd, const int module, const int ROC, string filename)
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

bool SvxPixelHotDeadMap::readChipMapFromFile(int &runStart, int &runEnd, string filename)
{ 

  ifstream infile;

  infile.open(filename.c_str(),ios::in);

  if (!infile) {
    cerr << "Can't open input file " << filename << endl;
    return false;
    //exit(EXIT_FAILURE);
  }

  runStart = 0;//initialize
  runEnd = 0;//initialize
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

      int status=PdbSvxPixelHotDeadChipMap::SVX_CHIP_DEAD;//default is dead

      //conversion from status in the text to official flag
      if (chanStatus == PdbSvxPixelHotDeadChipMap::SVX_CHIP_DEAD) {//DEAD
  status = PdbSvxPixelHotDeadChipMap::SVX_CHIP_DEAD;
      } else if (chanStatus == PdbSvxPixelHotDeadChipMap::SVX_CHIP_HOT) {//HOT
  status = PdbSvxPixelHotDeadChipMap::SVX_CHIP_HOT;
      } else if (chanStatus == PdbSvxPixelHotDeadChipMap::SVX_CHIP_NORMAL) {//NORMAL
  status = PdbSvxPixelHotDeadChipMap::SVX_CHIP_NORMAL;
      }

      setChipStatus(module,ROC,status);
      if (runStart==0) {
  runStart = runTemp1;
  runEnd = runTemp2;
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
bool SvxPixelHotDeadMap::readPixelMapFromFile(int &runStart, int &runEnd, const int mod, const int chip, string filename)
{ 

  ifstream infile;

  infile.open(filename.c_str(),ios::in);

  if (!infile) {
    cerr << "Can't open input file " << filename << endl;
    return false;
    //exit(EXIT_FAILURE);
  }

  runStart=0;
  runEnd = 0;
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
      cerr << "SvxPixelHotDeadMap::readPixelMapFromFile inconsistent chip of map and chip in map-file" << chip << " " << ROC << endl;
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
      if (runStart == 0) {
  runStart = run0;
  runEnd   = run1;
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
bool SvxPixelHotDeadMap::readPixelMapFromFile(int &runStart, int &runEnd, string filename)
{ 

  ifstream infile;

  infile.open(filename.c_str(),ios::in);

  if (!infile) {
    cerr << "Can't open input file " << filename << endl;
    return false;
    //exit(EXIT_FAILURE);
  }

  runStart=0;
  runEnd = 0;
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

    int status = chanStatus;
      
      
//       int status=PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_DEAD;//default is dead

//       //conversion from status in the text to official flag
//       if (chanStatus == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_DEAD) {//DEAD
//  status = PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_DEAD;
//       } else if (chanStatus == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_HOT) {//HOT
//  status = PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_HOT;
//       } else if (chanStatus == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_NORMAL) {//NORMAL
//  status = PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_NORMAL;
//       }

      setPixelStatus(module,ROC,column,row,status);
      if (runStart == 0) {
  runStart = run0;
  runEnd   = run1;
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

bool SvxPixelHotDeadMap::readPixelMapsFromFile(int &runStart, int &runEnd, std::string ref_file, std::string diff_file)
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
    //exit(EXIT_FAILURE);
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
  
  /*
  // 3. Set pixel status using badmap info.
  for (mod=0; mod<NMODULE; mod++) {
    for (roc=0; roc<NROC; roc++) {
      for (col=0; col<NCOLUMN; col++) {
  for (row=0; row<NROW; row++) {
    status = PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_NORMAL;
    if (badmap[mod][roc][col][row] != PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_UNKNOWN) {
      status = badmap[mod][roc][col][row];
    }
    setPixelStatus(mod,roc,col,row,status);
  }
      }
    }
  }
  */

  return true;
}


void SvxPixelHotDeadMap::setPixelStatus(const int module,
          const int ROC,
          const int column,
          const int row,
          const int status)
{
  // Add this record to the memory-resident map
  if (rangeCheck(module,ROC,column,row)) {
    map[module][ROC][column][row] = (signed char)status;
  }
}



void SvxPixelHotDeadMap::setRawPixelStatus(const int module,
             const int ROC,
             const int column,
             const int row,
             const signed char rawStatus)
{
  // Add this record to the memory-resident map
  if (rangeCheck(module,ROC,column,row)) {
    map[module][ROC][column][row] = rawStatus;
  }
}


void SvxPixelHotDeadMap::setPixelStatus(const int layer,
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

void SvxPixelHotDeadMap::setChipStatus(const int module,
               const int ROC,
               const int status)
{
  if (rangeCheck(module,ROC)) {
    chipmap[module][ROC] = (signed char)status;
  }
}

void SvxPixelHotDeadMap::setRawChipStatus(const int module,
            const int ROC,
            const signed char status)
{
  if (rangeCheck(module,ROC)) {
    chipmap[module][ROC] = status;
  }
}

bool SvxPixelHotDeadMap::readFromDatabase(const int run, bool read_reference) {
  bool chipStatus = readChipMapFromDatabase(run);
  bool pixelStatus = false;
  for (int module=0; module<NMODULE; module++) {
    for (int ROC=0; ROC<NROC; ROC++) {
      int chipMapStatus = getChipStatus(module,ROC);
      
      //only when chip is normal read pixel map
      if (chipMapStatus == PdbSvxPixelHotDeadChipMap::SVX_CHIP_NORMAL) {
        if(read_reference==true){ readReferencePixelMapFromDatabase(run,module,ROC); }
        bool pixelStatusTemp = readPixelMapFromDatabase(run,module,ROC);
  if (verbosity>90) {
    if (!pixelStatusTemp) {
      cerr << "readFroMDatabase, failed to read pixel map for (module, ROC) = " 
     << module << " " << ROC << endl;
    }
  }
  if ((!pixelStatus)&&(pixelStatusTemp)) {
    pixelStatus = true;
  }
      } 
      else {
  if (verbosity>90) {
    cout << "readFromDatabase> since chipMapStatus is not normal, not reading pixel map" << endl;
  }
      }
    }
  }
  
  return (chipStatus&pixelStatus);
}

bool SvxPixelHotDeadMap::readChipMapFromDatabase(const int run)
{
  if ((run<0)||(run>99999999)) {
    cerr << "SvxPixelHotDeadMap::readChipMapFromDatabase Strange run " << endl;
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
  //  PHTimeStamp *Tstop = rt->getEndTime(run);
  
  if (Tsearch == NULL) {
    cerr << "SvxPixelHotDeadMap::readChipMapFromDatabase> Error Tsearch==null" << endl;
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
  }
  else {
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
  
  if (svxBank)
    delete svxBank;

  if (Tsearch) delete Tsearch;
  
  return success;
}



bool SvxPixelHotDeadMap::readReferencePixelMapFromDatabase(const int run)
{
  setReferenceOffset();
  bool write_status = readPixelMapFromDatabase(run);
  setDifferenceOffset();
  return write_status;
}


///
/// For entire modules and rocs
///
bool SvxPixelHotDeadMap::readPixelMapFromDatabase(const int run)
{
  bool success = true;
  for (int imod=0; imod<NMODULE; imod++) {
    for (int iroc=0; iroc<NROC; iroc++) {
      if (!readPixelMapFromDatabase(run, imod, iroc)) success = false;
    }
  }

  return success;
}



bool SvxPixelHotDeadMap::readReferencePixelMapFromDatabase(const int run, const int module, const int ROC)
{
  setReferenceOffset();
  bool write_status = readPixelMapFromDatabase(run, module, ROC);
  setDifferenceOffset();
  return write_status;
}


///
/// For specific module and roc
///
bool SvxPixelHotDeadMap::readPixelMapFromDatabase(const int run, const int module, const int ROC)
{
  if ((run<0)||(run>99999999)) {
    cerr << "SvxPixelHotDeadMap::readPixelMapFromDatabase Strange run " << endl;
    cerr << "run = " << run << endl;
    return false;
  }
  if ((module<0)||(module>=NMODULE)) {
    cerr << "SvxPixelHotDeadMap::readPixelMapFromDatabase Strange module " << endl;
    cerr << "module = " << module << endl;
    return false;
  }
  if ((ROC<0)||(ROC>=NROC)) {
    cerr << "SvxPixelHotDeadMap::readPixelMapFromDatabase Strange ROC " << endl;
    cerr << "module = " << ROC << endl;
    return false;
  }
  bool success = true;
  PdbCalBank *svxBank = NULL;
  const char *tableName = "svxpixelhotdeadpixelmap";
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
  
  bankID.setInternalValue(module*NROC+ROC + reference_map_bankID_offset);//important!!
  RunToTime *rt = RunToTime::instance();
  PHTimeStamp *Tsearch = rt->getBeginTime(run);
  //  PHTimeStamp *Tstop = rt->getEndTime(run);
  
  if (Tsearch == NULL) {
    cerr << "SvxPixelHotDeadMap::readPixelMapFromDatabase> Error Tsearch==null" << endl;
    cerr << "module = " << module << endl;
    cerr << "ROC = " << ROC << endl;
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
  
  if (svxBank)
    delete svxBank;

  if (Tsearch) delete Tsearch;
  
  return success;
}


/*
  bool SvxPixelHotDeadMap::writeToDatabase(int run) {
  bool chipStatus = writeChipMapToDatabase(run);
  bool pixelStaus = false;

  for (int module=0;module<NMODULE;module++) {
  for (int ROC=0;ROC<NROC;ROC++) {
      
  }
  }

  return (chipStatus & pixelStatus);
  }
*/

bool SvxPixelHotDeadMap::writeChipMapToDatabase(const int run)
{
  if ((run<0)||(run>99999999)) {
    cerr << "SvxPixelHotDeadMap::writeChipMapToDatabase Strange run" << endl;
    cerr << "run = " << run << endl;
    return false;
  }

  bool success = true;
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  const char *tableName = "svxpixelhotdeadchipmap";

  PdbCalBank *svxBank = NULL;

  if (application->startUpdate()) {
    RunToTime *rt = RunToTime::instance();
    PHTimeStamp *Tstart = rt->getBeginTime(run);
    PHTimeStamp *Tstop  = rt->getEndTime(run);

    if (Tstart == NULL) {
      cerr << "SvxPixelHotDeadMap::writeChipMapToDatabase> Warning Tstart==NULL, taking current time" << endl;
      return false;
    }
    if (Tstop == NULL) {
      cerr << "SvxPixelHotDeadMap::writeChipMapToDatabase> Warning Tstop==NULL, taking current time" << endl;
      return false;
    }

    char description[100];
    sprintf(description,"svxpixel-hotdead-chipmap%010d",run) ;
    const char *pdescription = description;

    const char *bankName = "PdbSvxPixelHotDeadChipMapBank";
      
    PdbBankID bankID;

    bankID.setInternalValue(run);

    cout << "tableName = " << tableName << endl;
    cout << "description = " << description << endl;
    cout << "bankID = " << bankID.getInternalValue() << endl;
    cout << "Validity range: " << *Tstart << " - " << *Tstop << endl;

      
    //here we have correspondence between PdbSvxPixelHotDeadMap structure and bank record length
    svxBank = bankManager->createBank(bankName,
              bankID, pdescription,
              *Tstart, *Tstop, tableName);

    //doesnt work
    //            svxBank = bankManager->createBank(run,bankName,
    //                bankID, pdescription,
    //                tableName,0);


    //doesnt work      svxBank = bankManager->createBank(run,run,bankName,
    //                bankID, pdescription,
    //                tableName);

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

    if (Tstart) delete Tstart;
    if (Tstop ) delete Tstop;
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

  if (svxBank)
    delete svxBank;

  return success;
}



bool SvxPixelHotDeadMap::writeReferencePixelMapToDatabase(const int runstart, const int runend, const int module, const int ROC)
{
  setReferenceOffset();
  bool write_status = writePixelMapToDatabase(runstart, runend, module, ROC);
  setDifferenceOffset();
  return write_status;
}


bool SvxPixelHotDeadMap::writePixelMapToDatabase(const int runstart, const int runend, const int module, const int ROC)
{
  if ((module<0)||(module>=NMODULE)) {
    cerr << "SvxPixelHotDeadMap::writePixelMapToDatabase Strange module " << endl;
    cerr << "module = " << module << endl;
    return false;
  }
  if ((ROC<0)||(ROC>=NROC)) {
    cerr << "SvxPixelHotDeadMap::writePixelMapToDatabase Strange ROC " << endl;
    cerr << "ROC = " << ROC << endl;
    return false;
  }
  if ((runend<0)||(runend>99999999)) {
    cerr << "SvxPixelHotDeadMap::writePixelMapToDatabase Strange run" << endl;
    cerr << "runend = " << runend << endl;
    return false;
  }

  bool success = true;
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  const char *tableName = "svxpixelhotdeadpixelmap";

  PdbCalBank *svxBank = NULL;

  if (application->startUpdate()) {
    RunToTime *rt = RunToTime::instance();
    PHTimeStamp *Tstart = rt->getBeginTime(runstart);
    PHTimeStamp *Tstop  = rt->getEndTime(runend);

    if (Tstart == NULL) {
      cerr << "SvxPixelHotDeadMap::writePixelMapToDatabase> Warning Tstart==NULL, taking current time" << endl;
      Tstart = new PHTimeStamp();
    }
    if (Tstop == NULL) {
      cerr << "SvxPixelHotDeadMap::writePixelMapToDatabase> Warning Tstop==NULL, taking current time" << endl;
      Tstop = new PHTimeStamp();
    }

    char description[100];
    sprintf(description,"svxpixel-hotdead-pixelmap%06d-%06d",runstart,runend) ;
    const char *pdescription = description;

    const char *bankName = "PdbSvxPixelHotDeadPixelMapBank";
      
    PdbBankID bankID;

    bankID.setInternalValue(module*NROC+ROC+reference_map_bankID_offset);

    cout << "tableName = " << tableName << endl;
    cout << "description = " << description << endl;
    cout << "bankID = " << bankID.getInternalValue() << endl;
    cout << "Validity range: " << *Tstart << " - " << *Tstop << endl;

      
    //here we have correspondence between PdbSvxPixelHotDeadMap structure and bank record length
    svxBank = bankManager->createBank(bankName,
              bankID, pdescription,
              *Tstart, *Tstop, tableName);

    //doesnt work
    //            svxBank = bankManager->createBank(run,bankName,
    //                bankID, pdescription,
    //                tableName,0);


    //doesnt work      svxBank = bankManager->createBank(run,run,bankName,
    //                bankID, pdescription,
    //                tableName);

    if (svxBank) {
      // The number of records is same as the number of data in the map array
      unsigned numRecords = (unsigned)countBadPixels(module,ROC);
  
      svxBank->setLength(numRecords);

      unsigned recordNumber = 0;

      for (int column = 0; column < NCOLUMN; column++) {
  for (int row = 0; row < NROW; row++) {
    int status = getPixelStatus(module,ROC,column,row);
    if (status != blank_status) {
      PdbSvxPixelHotDeadPixelMap *rec =
        (PdbSvxPixelHotDeadPixelMap *) & (svxBank->getEntry(recordNumber));
      
      exportPixelMap(module,ROC,column,row,*rec);
      recordNumber++;
    }
  }
      }
    }
    else {
      cerr << PHWHERE << " ERROR: can not create svxBank." << endl;
      success = false;
    }

    if (Tstart) delete Tstart;
    if (Tstop ) delete Tstop;
  }
  else {
    cerr << PHWHERE << "ERROR: Database not writable, aborting." 
   << endl;
    success = false;
  }

  if (verbosity>90) {
    cout << "writePixelMapToDatabase success = " << success << endl;
  }

  if (success) {
    application->commit();
    if (verbosity>90) {
      cout << "writePixelMapToDatabase commit()" << endl;
    }
  }
  else {
    application->abort();
    if (verbosity>90) {
      cout << "writePixelMapToDatabase abort()" << endl;
    }
  }

  if (svxBank)
    delete svxBank;

  return success;
}


void SvxPixelHotDeadMap::print() const 
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
int SvxPixelHotDeadMap::getStatus(const int module,
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


int SvxPixelHotDeadMap::getStatus(const int layer,
          const int ladder,
          const int south_north,
          const int ROC,
          const int column,
          const int row) const
{
  int module = svxAddress::getPixelModuleIDSouthNorth(layer,ladder,south_north);

  return (getStatus(module,ROC,column,row));
}


int SvxPixelHotDeadMap::getChipStatus(const int module,
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


int SvxPixelHotDeadMap::getPixelStatus(const int module,
               const int ROC,
               const int column,
               const int row) const 
{                       

  if (rangeCheck(module,ROC,column,row)) {
    char cStatus = map[module][ROC][column][row];
    return cStatus;
  }
    else {
    return PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_OUTOFRANGE;
  }
}

bool  SvxPixelHotDeadMap::isPixelOkForClustering(const int module,
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


void SvxPixelHotDeadMap::setTileMap()
{
  for (int module=0; module<NMODULE; module++) {
    for (int ROC=0; ROC<NROC; ROC++) {
      for (int tile=0; tile<NTILE; tile++) {
  setTileGoodFrac(module, ROC, tile);
      }
    }
  }
}

void SvxPixelHotDeadMap::setTileGoodFrac(const int module,
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

float SvxPixelHotDeadMap::getTileGoodFrac(const int module,
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


bool SvxPixelHotDeadMap::rangeCheck(int module, int ROC, int column, int row) const 
{
  if ((0<=module)&&(module<NMODULE)&&(0<=ROC)&&(ROC<NROC)
      &&(0<=column)&&(column<NCOLUMN)&&(0<=row)&&(row<NROW)) {
    return true;
  } else {
    return false;
  }
}

bool SvxPixelHotDeadMap::rangeCheck(int module, int ROC) const 
{
  if ((0<=module)&&(module<NMODULE)&&(0<=ROC)&&(ROC<NROC)) {
    return true;
  } else {
    return false;
  }
}

bool SvxPixelHotDeadMap::rangeCheck(int module, int ROC, int tile) const 
{
  if ((0<=module) && (module<NMODULE) &&
      (0<=ROC) && (ROC<NROC) &&
      (0<=tile) && (tile<NTILE)) {
    return true;
  } else {
    return false;
  }
}

int SvxPixelHotDeadMap::countBadChips() 
{
  int ncount = 0;
  for (int module=0;module<NMODULE;module++) {
    for (int ROC=0;ROC<NROC;ROC++) {
      if (chipmap[module][ROC] != PdbSvxPixelHotDeadChipMap::SVX_CHIP_NORMAL) {
  ncount ++;
      }
    }
  }
  return ncount;
}

int SvxPixelHotDeadMap::countBadPixels(int module, int ROC) 
{
  int ncount = 0;
  for (int column=0;column<NCOLUMN;column++) {
    for (int row=0;row<NROW;row++) {
      if (map[module][ROC][column][row] != blank_status) {
  ncount ++;
      }
    }
  }
  return ncount;
}


void SvxPixelHotDeadMap::importChipMap(const PdbSvxPixelHotDeadChipMap &rhs) {
  int module = rhs.getModule();
  int ROC    = rhs.getROC();
  int status = rhs.getStatus();
  if (verbosity>90) {
    cout << "importChipMap, module, ROC, status = " << module << " " << ROC << " " << status << endl;
  }

  setChipStatus(module,ROC,status);
}

void SvxPixelHotDeadMap::importPixelMap(const PdbSvxPixelHotDeadPixelMap &rhs) {
  int module = rhs.getModule();
  int ROC = rhs.getROC();
  int column = rhs.getColumn();
  int row = rhs.getRow();
  int status = rhs.getStatus();
  setPixelStatus(module,ROC,column,row,status);
}

void SvxPixelHotDeadMap::exportChipMap(const int module, const int ROC, PdbSvxPixelHotDeadChipMap &rhs) {
  rhs.setRawStatus(module,ROC,chipmap[module][ROC]);
}

void SvxPixelHotDeadMap::exportPixelMap(const int module, const int ROC, const int column, const int row, PdbSvxPixelHotDeadPixelMap &rhs) {
  rhs.setRawStatus(module,ROC,column,row,map[module][ROC][column][row]);
}


void SvxPixelHotDeadMap::printBadPixels(int module, int roc) const
{
  PdbSvxPixelHotDeadPixelMap pmap;
  
  for (int column = 0; column < NCOLUMN; column++)
  {
    for (int row = 0; row < NROW; row++)
    {
      int status = (int)(map[module][roc][column][row]);
//       int status = getPixelStatus(module,roc,column,row);
      if(status == PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_NORMAL){continue;}
      pmap.setStatus(module, roc, column, row, status);
      pmap.print();
    }
  }
}

