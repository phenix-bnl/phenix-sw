// ===================
// FILE: SvxDeadMap.C
// ===================


#include "SvxDeadMap.h"
#include "SvxParameters.h"

#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbSvxDeadMap.hh>
#include <RunToTime.hh>

#include <cassert>
#include <climits>
#include <cstdlib>
#include <fstream>
#include <iostream>

// The hash functions make use of assumptions about the architecture,
// which we have to guarantee are the case before the map is usable.
// The preprocessor should suffice to guarantee that.
#if WORD_BIT < 32
#error int type must be at least 32 bits long
#endif

using namespace std;

/**
 * @brief Stripixel hot/deadmap class
 *
 * These are the data structures 
 * the hash functions use to pack the fields into ints/longs.
 */
struct channelHash {
    unsigned sensorSection:8;
    unsigned readout:8;
    unsigned channel:16;
};

/**
 * @brief  [NOT USED YET]
 */
union hashValue {
    int i;
    struct channelHash ch;
};

// Helper function for writeToDatabase() below
void writeSensorMapToBank(const deadMap_t & sensorMap,
                          const unsigned recordNumberStart,
                          const unsigned striplayer,
                          const unsigned ladder,
                          const unsigned sensor, PdbCalBank * svxBank);

//--------------------------------------------------------------------------------

SvxDeadMap::SvxDeadMap(const int verbosityLevel) :
  UseDatabase(true),
  verbosity(verbosityLevel)
{
  
  // It'd be nice if there were a way to guarantee this at compile time
  assert(sizeof(hashValue) == sizeof(int));
  
  // initialize dead readout map
  memset(deadReadoutMap,0,sizeof(deadReadoutMap));
  
  if (verbosity > 0) std::cerr << "sizeof(deadMap): " << sizeof(deadMap) << std::endl;
}

//--------------------------------------------------------------------------------

void SvxDeadMap::Verbosity(const int level)
{
    if (level < 0 || level > 99) {
        std::cerr << "Verbosity level out of range: " << level << std::endl;
        return;
    }

    verbosity = level;
}

//--------------------------------------------------------------------------------

bool SvxDeadMap::writeToFile(std::string filename)
{
  ofstream fout(filename.c_str());
  if(!fout) {cerr << PHWHERE << " ERROR: Cannot open output file: " << filename << endl; return false;}

    int count=0;

      for(int h=0; h<2; h++)  { // striplayer
      for(int i=0; i<24; i++) { // ladder
      for(int j=0; j<6; j++)  { // sensor
      for(int k=0; k<2; k++)  { // section
      for(int l=0; l<2; l++)  { // readout
      for(int m=0; m<384; m++)  { // channel
        int key = hash(k, l, m);

        deadMap_t::iterator itr = deadMap[h][i][j].find(key);
        if(itr != deadMap[h][i][j].end() ){
            fout << h << " "  << i << " " << j << " " << k << " " << l << " " << m <<" " << itr->second<< endl; // write to output file
            count++;
        }

      } } } } } }

  cout << PHWHERE << "Number of bad stripixel readouts written out = " << count << endl;
  fout.close();

  return true;;
}

//--------------------------------------------------------------------------------

bool SvxDeadMap::readReadoutsFromReadoutsFile(std::string filename)
{
  ifstream infile;
  infile.open(filename.c_str());
  if(!infile) {std::cerr << PHWHERE << "Can't open input file " << filename << std::endl; return false;}
 
  int striplayer,ladder,sensor,section,readout;
  int count=0;
  while (infile >> striplayer >> ladder >> sensor >> section >> readout) {
    count++;
    deadReadoutMap[striplayer][ladder][sensor][section][readout]=1;
  }
  cout << PHWHERE << "Number of bad stripixel readouts = " << count << endl;
  infile.close();

  return true;
}

//-------------------------------------------------------------------------------

bool SvxDeadMap::writeReadoutsToFile(std::string filename)
{
  ofstream fout(filename.c_str());
  if(!fout) {cerr << PHWHERE << " ERROR: Cannot open output file: " << filename << endl; return false;}

    int count=0;
      for(int h=0; h<2; h++)  { // striplayer
      for(int i=0; i<24; i++) { // ladder
      for(int j=0; j<6; j++)  { // sensor
      for(int k=0; k<2; k++)  { // section
      for(int l=0; l<2; l++)  { // readout
        if(deadReadoutMap[h][i][j][k][l]==1) {
          fout << h << " "  << i << " " << j << " " << k << " " << l << endl; // write to output file
          count++;
        }
      } } } } }

  cout << PHWHERE << "Number of bad stripixel readouts written out = " << count << endl;
  fout.close();

  return true;
}

//-------------------------------------------------------------------------------

int SvxDeadMap::convertDeadReadoutMap()
{
  int count=0;
  int badthreshold=256;
  for(int h=0; h<2; h++)  { // striplayer
    for(int i=0; i<24; i++) { // ladder
      for(int j=0; j<6; j++)  { // sensor
        for(int k=0; k<2; k++)  { // section
          for(int l=0; l<2; l++)  { // readout
            if(deadReadoutMap[h][i][j][k][l]>badthreshold) {
              deadReadoutMap[h][i][j][k][l]=1;
              count++;
            }
            else {
              deadReadoutMap[h][i][j][k][l]=0;
            }
          } 
        } 
      } 
    } 
  }

  return count;
}

//-------------------------------------------------------------------------------
int SvxDeadMap::generateReadoutMapFromDeadMap()
{

  for(int ilay=0; ilay<2; ilay++)  { // striplayer
    for(int ilad=0; ilad<24; ilad++) { // ladder
      for(int isen=0; isen<6; isen++)  { // sensor
        for(int isec=0; isec<2; isec++)  { // section
          for(int iro=0; iro<2; iro++)     { // readout
            for(int ich=0; ich<384; ich++)   { // channel
	      
              int key = hash(isec, iro, ich);
	      
              deadMap_t::iterator itr = deadMap[ilay][ilad][isen].find(key);
	      
              if(itr != deadMap[ilay][ilad][isen].end()){ // if key exist
                int status = itr->second;
                if(status!=0) {
                  deadReadoutMap[ilay][ilad][isen][isec][iro]++;
                }
              }
            }
          }
        }
      }
    }
  }

  return convertDeadReadoutMap();

}

//-------------------------------------------------------------------------------

bool SvxDeadMap::readFromFile(std::string filename)
{
    int striplayer;
    int ladder;
    int sensor;
    int sensorSection;
    int readout;
    int channel;
    double adcmean;
    double adcrms;
    int chanStatus;
    double thresh;

    ifstream infile;

    infile.open(filename.c_str());

    if (!infile) {
        std::cerr << PHWHERE << "Can't open input file " << filename << std::endl;
        //exit(EXIT_FAILURE);
        return false;
    }

    int count=0;
    int count2=0;

    while (infile >> striplayer >> ladder >> sensor >>
           sensorSection >> readout >> channel >> adcmean >> adcrms >> chanStatus >> thresh) {

      if(chanStatus!=0) {
        count++;

        // only add bad channels to channel map if they are not already
        // in the bad readout map
        if(deadReadoutMap[striplayer][ladder][sensor][sensorSection][readout]==1) continue;
        count2++;
 
        if (verbosity > 0) {
            std::cerr << "Read striplayer " << striplayer
                << ", ladder " << ladder
                << ", sensor " << sensor
                << ", sensor section " << sensorSection
                << ", readout " << readout
                << ", channel " << channel
                << ", status is " << chanStatus << std::endl;
        }
        if(chanStatus<-1 || chanStatus>2)
          cout << PHWHERE << " Wrong status value: " << chanStatus << endl;

        int key = hash(sensorSection, readout, channel);

        // Add this record to the memory-resident map
        deadMap[striplayer][ladder][sensor][key] = toSvxChannelStatus(chanStatus);

      }
    }

    cout << PHWHERE << " Total number of bad stripixel channels = " << count << endl;
    cout << PHWHERE << " Number of bad stripixel channels not in readout dead map = " << count2 << endl;

    infile.close();
    return true;
}

void SvxDeadMap::set_channelStatus(const int striplayer,
                                   const int ladder,
                                   const int sensor,
                                   const int sensorSection,
                                   const int readout,
                                   const int channel,
                                   SvxChannelStatus status) 
{
   int key = hash(sensorSection, readout, channel);
   // Add this record to the memory-resident map
   //deadMap[striplayer][ladder][sensor][key] = toSvxChannelStatus(status);
   deadMap[striplayer][ladder][sensor][key] = status;
}

//-------------------------------------------------------------------------------------

bool SvxDeadMap::readReadoutsFromDatabase(int run1)
{
  RunToTime *rt = RunToTime::instance();
  PHTimeStamp *T1   = rt->getBeginTime(run1);
  time_t tmp = T1->getTics();
  T1->setTics(tmp);

  bool result = readReadoutsFromDatabase(T1);
  delete T1;
  return  result;
}

//-------------------------------------------------------------------------------------

bool SvxDeadMap::readReadoutsFromDatabase(PHTimeStamp * T)
{
    bool success = true;
    PHTimeStamp Tsearch = *T;
    PdbCalBank *svxBank = NULL;

    // This is actually a readout-based deadmap! 
    // Leaving "hybrid" in the name for compatibility.
    const char *tableName = "svxhotdeadstriphybrids";
    PdbBankID bankID;

    PdbBankManager *bankManager = PdbBankManager::instance();
    PdbApplication *application = bankManager->getApplication();

    if (!application->startRead()) {
        application->abort();
        std::cerr << PHWHERE << " ERROR -> Transaction aborted. Database NOT available." << std::endl; 
        return false;
    }

    bankID.setInternalValue(0);
    cout << "tableName = " << tableName << endl;
    cout << "bankID = " << bankID.getInternalValue() << endl;
    cout << "Search TimeStamp: " << Tsearch << endl;

    svxBank = bankManager->fetchBank("PdbSvxDeadMapBank", bankID, tableName, Tsearch);

    if (svxBank) {
        int banklength = (int) svxBank->getLength();
        cout << "Banklength = " << banklength << endl;

        for (int i = 0; i < banklength; i++) {
            PdbSvxDeadMap *rec = (PdbSvxDeadMap *) &(svxBank->getEntry (i));

            short striplayer  = rec->getParameter (PdbSvxDeadMap::layer);
            short ladder = rec->getParameter (PdbSvxDeadMap::ladder);
            short sensor = rec->getParameter (PdbSvxDeadMap::sensor);
            short section = rec->getParameter (PdbSvxDeadMap::sensorSection);
            short readout = rec->getParameter (PdbSvxDeadMap::sensorReadout);
            deadReadoutMap[striplayer][ladder][sensor][section][readout]=1;

        }
    }
    else {
        cerr << PHWHERE << " ERROR -> bankManager returned null pointer." << endl;
        success = false;
    }

    if (success) { application->commit(); }
      else { application->abort(); }

    if (svxBank) delete svxBank;

    return success;
}

//-------------------------------------------------------------------------------------

bool SvxDeadMap::readFromDatabase(int runnumber)
{
  RunToTime *rt = RunToTime::instance();
  PHTimeStamp *T   = rt->getBeginTime(runnumber);
  time_t tmp = T->getTics();
  T->setTics(tmp);
  bool result = readFromDatabase(T);

  delete T;
  return  result;
}

//-------------------------------------------------------------------------------------

bool SvxDeadMap::readFromDatabase(PHTimeStamp * T)
{
    bool success = true;
    PHTimeStamp Tsearch = *T;
    PdbCalBank *svxBank = NULL;
    const char *tableName = "svx.hotdead.sim0";
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

    bankID.setInternalValue(0);
    cout << "tableName = " << tableName << endl;
    cout << "bankID = " << bankID.getInternalValue() << endl;
    cout << "Search TimeStamp: " << Tsearch << endl;

    svxBank = bankManager->fetchBank("PdbSvxDeadMapBank", bankID,
                                     tableName, Tsearch);

    if (svxBank) {
        int banklength = (int) svxBank->getLength();
        cout << "Banklength = " << banklength << endl;
        
        for (int i = 0; i < banklength; i++) {
            PdbSvxDeadMap *rec = (PdbSvxDeadMap *) &(svxBank->getEntry (i));
            
            if (verbosity > 0) {
                cout << "Read database entry: ";
                rec->print();
            }
            
            short striplayer = rec->getParameter (PdbSvxDeadMap::layer);
            short ladder = rec->getParameter (PdbSvxDeadMap::ladder);
            short sensor = rec->getParameter (PdbSvxDeadMap::sensor);
            
            int key = hash(rec->getParameter (PdbSvxDeadMap::sensorSection), 
                           rec->getParameter (PdbSvxDeadMap::sensorReadout), 
                           rec->getParameter (PdbSvxDeadMap::channel));

            // Add this record to the memory-resident map
            deadMap[striplayer][ladder][sensor][key] =
                toSvxChannelStatus((int)rec->getParameter (PdbSvxDeadMap::status));
        }
    }
    else {
        cerr << PHWHERE << " ERROR -> bankManager returned null pointer."
             << endl;
        success = false;
    }
    
    if (success) {
        application->commit();
    }
    else {
        application->abort();
    }

    if (svxBank) delete svxBank;

    return success;
}

//-----------------------------------------------------------------------------

bool SvxDeadMap::writeReadoutsToDatabase(int run1, int run2)
{
  RunToTime *rt = RunToTime::instance();
  PHTimeStamp *T1   = rt->getBeginTime(run1);
  PHTimeStamp *T2   = rt->getEndTime(run2);
  bool result = writeReadoutsToDatabase(T1, T2);

  delete T1;
  delete T2;

  return  result;
}

//------------------------------------------------------------------------------

bool SvxDeadMap::writeReadoutsToDatabase(PHTimeStamp * Tbeg, PHTimeStamp * Tend)
{
    bool success = true;
    PdbBankManager *bankManager = PdbBankManager::instance();
    PdbApplication *application = bankManager->getApplication();

    PdbCalBank *svxBank = NULL;

    if (application->startUpdate()) {
      
      // This is actually a readout-based deadmap! 
      // Leaving "hybrid" in the name for compatibility.
        const char *tableName = "svxhotdeadstriphybrids";
        const char *description = "SVX dead/hot readout map";
        PHTimeStamp Tstart = *Tbeg;
        PHTimeStamp Tstop = *Tend;

        PdbBankID bankID;

        bankID.setInternalValue(0);

        cout << "tableName = " << tableName << endl;
        cout << "description = " << description << endl; 
        cout << "bankID = " << bankID.getInternalValue() << endl; 
        cout << "Validity range: " << Tstart << " - " << Tstop << endl;

        svxBank = bankManager->createBank("PdbSvxDeadMapBank",
                                          bankID, description,
                                          Tstart, Tstop, tableName);

        if (svxBank) {

            // Figure out the number of records to store 
            unsigned numRecords = 0;
            for(int h=0; h<2; h++) {  // striplayer
            for(int i=0; i<24; i++) { // ladder
            for(int j=0; j<6; j++)  { // sensor
            for(int k=0; k<2; k++)  { // section
            for(int l=0; l<2; l++)  { // readout
              if(deadReadoutMap[h][i][j][k][l]==1) numRecords++;
            } } } } }

            cout << "SvxDeadMap::writeReadoutsToDatabase: Number of records = " << numRecords << endl;
            svxBank->setLength(numRecords);

            // And now this loop writes out the data in the deadMap maps
            unsigned recordNumber = 0;

            for(int h=0; h<2; h++) { // striplayer
            for(int i=0; i<24; i++) { // ladder
            for(int j=0; j<6; j++)  { // sensor
            for(int k=0; k<2; k++)  { // section
            for(int l=0; l<2; l++)  { // readout
            if(deadReadoutMap[h][i][j][k][l]==1) {
              PdbSvxDeadMap *rec = (PdbSvxDeadMap *) & (svxBank->getEntry(recordNumber));
              rec->setAllParameters(h, i, j, k, l, 0, 0);
              recordNumber++;
            }
            }}}}}

        }
        else { cerr << PHWHERE << " ERROR: can not create svxBank." << endl; success = false; }
    }
    else { cerr << PHWHERE << "ERROR: Database not writable, aborting." << endl; success = false; }

    if (success) { application->commit(); }
      else { application->abort(); }

    if (svxBank) delete svxBank;

    return success;
}

//---------------------------------------------------------------------------------------

bool SvxDeadMap::writeToDatabase(int run1, int run2)
{
  RunToTime *rt = RunToTime::instance();
  PHTimeStamp *T1   = rt->getBeginTime(run1);
  PHTimeStamp *T2   = rt->getEndTime(run2);
  return  writeToDatabase(T1, T2);
}

//---------------------------------------------------------------------------------------

bool SvxDeadMap::writeToDatabase(PHTimeStamp * Tbeg, PHTimeStamp * Tend)
{
    bool success = true;
    PdbBankManager *bankManager = PdbBankManager::instance();
    PdbApplication *application = bankManager->getApplication();

    PdbCalBank *svxBank = NULL;

    if (application->startUpdate()) {
        const char *tableName = "svx.hotdead.sim0";
        const char *description = "SVX dead/hot channel map";
        PHTimeStamp Tstart = *Tbeg;
        PHTimeStamp Tstop = *Tend;

        PdbBankID bankID;

        bankID.setInternalValue(0);

        cout << "tableName = " << tableName << endl;
        cout << "description = " << description << endl;
        cout << "bankID = " << bankID.getInternalValue() << endl;
        cout << "Validity range: " << Tstart << " - " << Tstop << endl;

        svxBank = bankManager->createBank("PdbSvxDeadMapBank",
                                          bankID, description,
                                          Tstart, Tstop, tableName);

        if (svxBank) {
            // Figure out the number of records to store by summing up the size()
            // of every map in the deadMap array
            unsigned numRecords = 0;

            for (size_t striplayer = 0; striplayer < MAXLAYERS; striplayer++) {
                for (size_t ladder = 0; ladder < MAXLADDERS; ladder++) {
                    for (size_t sensor = 0; sensor < MAXSENSORS; sensor++) {
                        numRecords +=
                            deadMap[striplayer][ladder][sensor].size();
                    }
                }
            }

            cout << "SvxDeadMap::writeToDatabase: Number of records = " << numRecords << endl;
            svxBank->setLength(numRecords);

            // And now this loop writes out the data in the deadMap maps
            unsigned recordNumber = 0;

            for (size_t striplayer = 0; striplayer < MAXLAYERS; striplayer++) {
                for (size_t ladder = 0; ladder < MAXLADDERS; ladder++) {
                    for (size_t sensor = 0; sensor < MAXSENSORS; sensor++) {
                        deadMap_t sensorMap =
                            deadMap[striplayer][ladder][sensor];

                        writeSensorMapToBank(sensorMap, recordNumber,
                                             striplayer, ladder, sensor,
                                             svxBank);
                        recordNumber += sensorMap.size();
                    }
                }
            }
        }
        else {
            cerr << PHWHERE << " ERROR: can not create svxBank." << endl;
            success = false;
        }
    }
    else {
        cerr << PHWHERE << "ERROR: Database not writable, aborting." 
             << endl;
        success = false;
    }

    if (success) {
        application->commit();
    }
    else {
        application->abort();
    }

    if (svxBank)
        delete svxBank;

    return success;
}

void writeSensorMapToBank(const deadMap_t & sensorMap,
                          const unsigned recordNumberStart,
                          const unsigned striplayer,
                          const unsigned ladder,
                          const unsigned sensor, PdbCalBank * svxBank)
{
    unsigned recordNumber = recordNumberStart;

    for (deadMap_t::const_iterator i = sensorMap.begin();
         i != sensorMap.end(); 
         ++i) {

        PdbSvxDeadMap *rec =
            (PdbSvxDeadMap *) & (svxBank->getEntry(recordNumber++));

        // "Un-hash" the key back into its components
        union hashValue hv;

        hv.i = i->first;

        rec->setAllParameters(striplayer, ladder, sensor,
                              hv.ch.sensorSection,
                              hv.ch.readout, hv.ch.channel, i->second);
    }
}

void SvxDeadMap::print() const 
{
  for (size_t striplayer = 0; striplayer < MAXLAYERS; striplayer++) {
    for (size_t ladder = 0; ladder < MAXLADDERS; ladder++) {
      for (size_t sensor = 0; sensor < MAXSENSORS; sensor++) {
	deadMap_t sensorMap = deadMap[striplayer][ladder][sensor];
	for (deadMap_t::const_iterator i = sensorMap.begin();
	     i != sensorMap.end(); ++i) {
	  
	  enum SvxChannelStatus status = i->second;
	  
	  string statusString =
	    (status == SVX_DEAD) ? "dead" :
	    (status == SVX_HOT) ? "hot" :
	    (status == SVX_BADRMS) ? "badrms" :
	    (status == SVX_TEST) ? "testing" : "normal";
	  
	  // "Un-hash" the key back into its components
	  union hashValue hv;
	  
	  hv.i = i->first;
	  
	  cout << "Striplayer " << striplayer
	       << ", Ladder " << ladder
	       << ", sensor " << sensor
	       << ", sensor section " << hv.ch.sensorSection
	       << ", sensor readout " << hv.ch.readout
	       << ", channel " << hv.ch.channel
	       << ": " << statusString << endl;
	}
      }
    }
  }
}

SvxChannelStatus SvxDeadMap::channelStatus(const int striplayer,
                                           const int ladder,
                                           const int sensor,
                                           const int sensorSection,
                                           const int readout,
                                           const int channel) const 
{
  if (striplayer < 0 || striplayer > 1) {
    cerr << PHWHERE << "Error: invalid stripixel layer " << striplayer 
	 << ". Must be 0 or 1 (= VTX layer 2 or 3). return 0 (normal)" << endl;
    return SVX_NORMAL;
  }
  
  if(deadReadoutMap[striplayer][ladder][sensor][sensorSection][readout]==1) 
    return SVX_DEAD;

  int key = hash(sensorSection, readout, channel);
  deadMap_t::const_iterator i = deadMap[striplayer][ladder][sensor].find(key);
  
  return (i == deadMap[striplayer][ladder][sensor].end()) ?
    SVX_NORMAL : i->second;
}

int SvxDeadMap::readoutStatus(const int striplayer,
			     const int ladder,
			     const int sensor,
			     const int sensorSection,
			     const int readout
			     ) const
{
  if (striplayer < 0 || striplayer > 1) {
    cerr << PHWHERE << "Error: invalid stripixel layer " << striplayer 
	 << ". Must be 0 or 1 (= VTX layer 2 or 3)." << endl;
    return 12345678;
  }
  return deadReadoutMap[striplayer][ladder][sensor][sensorSection][readout];
}

int SvxDeadMap::channelStatus(const int striplayer,
                              const int ladder,
                              const int sensor,
                              const int sensorSection,
                              const int readout,
                              const int channel,
                              const int dummy) const
{
  if (striplayer < 0 || striplayer > 1) {
    cerr << PHWHERE << "Error: invalid stripixel layer " << striplayer 
	 << ". Must be 0 or 1 (= VTX layer 2 or 3)." << endl;
    return 12345678;
  }
  if(deadReadoutMap[striplayer][ladder][sensor][sensorSection][readout]==1)
    return -1;
  
  int key = hash(sensorSection, readout, channel);
  deadMap_t::const_iterator i = deadMap[striplayer][ladder][sensor].find(key);
  
  SvxChannelStatus tmp = (i == deadMap[striplayer][ladder][sensor].end()) ?  SVX_NORMAL : i->second;
  switch (tmp) {
    
  case SVX_DEAD:
    return -1;
  case SVX_NORMAL:
    return 0;
  case SVX_HOT:
    return 1;
  case SVX_BADRMS:
    return 2;
    
    
  default:
    //cerr << "SvxDeadMap::channelStatus() ERROR: " << "Invalid channel status value: " << tmp << endl;
    return -999;
    break;
  }
}

//inline SvxChannelStatus SvxDeadMap::toSvxChannelStatus(const int i)
SvxChannelStatus SvxDeadMap::toSvxChannelStatus(const int i)
{
    switch (i) {
      /*
    case -1:
        return SVX_HOT;
    case 0:
        return SVX_NORMAL;
    case 1:
        return SVX_DEAD;
    case 2:
        return SVX_BADRMS;
      */
    case -1:
        return SVX_DEAD;
    case 0:
        return SVX_NORMAL;
    case 1:
        return SVX_HOT;
    case 2:
        return SVX_BADRMS;
    default:
            cerr << "SvxDeadMap::toSvxChannelStatus: "
                << "Invalid channel status value: " << i << endl;
            exit(EXIT_FAILURE);
        break;
    }
}

int SvxDeadMap::hash(const int sensorSection,
                     const int readout,
                     const int channel) const 
{
    union hashValue result;

    result.ch.sensorSection = sensorSection;
    result.ch.readout = readout;
    result.ch.channel = channel;

    return result.i;
}

bool SvxDeadMap::compareDeadMap(SvxDeadMap *map) const
{

  if(map==NULL) {cout<<"Map is NULL"<<endl; return false; }

   bool ret = true;

      for(int h=0; h<2; h++)  { // striplayer
      for(int i=0; i<24; i++) { // ladder
      for(int j=0; j<6; j++)  { // sensor
      for(int k=0; k<2; k++)  { // section
      for(int l=0; l<2; l++)  { // readout
      for(int m=0; m<384; m++)  { // channel

        SvxChannelStatus st1  = channelStatus(h, i, j, k, l, m);
        SvxChannelStatus st2  = map->channelStatus(h, i, j, k, l, m);

        if(st1!=st2){
          ret = false;
  
          cout<<"MisMatch : "<<h<<" "<<i<<" "<<j<<" "<<k<<" "<<l<<" "<<m<<" : "<<st1<<" "<<st2<<endl;
        }

      } } } } } }

  return ret;;
}

bool SvxDeadMap::compareReadoutMap(SvxDeadMap *map) const
{

  if(map==NULL) {cout<<"Map is NULL"<<endl; return false; }

   bool ret = true;

      for(int h=0; h<2; h++)  { // striplayer
      for(int i=0; i<24; i++) { // ladder
      for(int j=0; j<6; j++)  { // sensor
      for(int k=0; k<2; k++)  { // section
      for(int l=0; l<2; l++)  { // readout

        int st1  = readoutStatus(h, i, j, k, l);
        int st2  = map->readoutStatus(h, i, j, k, l);

        if(st1!=st2){
          ret = false;
  
          cout<<"MisMatch : "<<h<<" "<<i<<" "<<j<<" "<<k<<" "<<l<<" : "<<st1<<" "<<st2<<endl;
        }

      } } } } }

  return ret;;
}
