// $Id: TMuiAddressTable.cc,v 1.8 2009/08/22 05:15:24 pinkenbu Exp $
#include <fstream>
#include <sstream>
#include <iostream>

#include "TMuiAddressTable.hh"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"
#include "PdbMuiChannelMap.hh"

using namespace std;

//___________________________________________________________
TMuiAddressTable* TMuiAddressTable::Table()
{
  static TMuiAddressTable singleton;
  return &singleton;
}

//___________________________________________________________
TMuiAddressTable::TMuiAddressTable(): 
  PHAddressObject(),
  fInit( false )
{ index = new PdbIndex(0, TMuiReadoutID::kChannelsTotal, 0, "GLOBAL"); }

//___________________________________________________________
TMuiAddressTable::~TMuiAddressTable()
{ 
  delete index;
  Clear(); 
}


//___________________________________________________________
void TMuiAddressTable::Clear()
{
  
  // delete all items in vector
  for( vector<PdbMuiChannelMap*>::iterator iter = fFEMConfig.begin(); iter != fFEMConfig.end(); iter++ )
  { delete *iter; }
  
  // clear vector
  fFEMConfig.clear();
  fInit = false;
  
}

//___________________________________________________________
short TMuiAddressTable::Arm(const unsigned long& module_id) const
{
  short arm = -1;

  switch (module_id) {
  case 0:
  case 0x0010:
    arm = kSOUTH;
    break;
  case 0x1000:
  case 0x1010:
    arm = kNORTH;
    break;
  default:
    cout 
      << "TMuiAddressTable::Arm-E1  invalid module ID = "
      << module_id << endl;
    break;
  }

  return arm;
}

//___________________________________________________________
short TMuiAddressTable::Orient(const unsigned long& module_id) const
{
  short orient = kHORIZ;

  switch (module_id) {
  case 0:
  case 0x1000:
    orient = kHORIZ;
    break;
  case 0x0010:
  case 0x1010:
    orient = kVERT;
    break;
  default:
    cout 
      << "TMuiAddressTable::Orient-E1  invalid module ID = "
      << module_id << endl;
    break;
  }

  return orient;
}

//___________________________________________________________
TMuiChannelId TMuiAddressTable::SoftwareAddress(const TMuiReadoutID& ident) const
{

  short arm = -1, gap = -1, panel = -1, orient = -1, twopack = -1;

  unsigned long moduleID = ident.ModuleID();
  short roc              = ident.ROC();
  short word             = ident.Word();
  short channel          = ident.Channel();

  if (fFEMConfig.size() != (unsigned int)TMuiReadoutID::kWordsTotal) {
    
    // The FEM config table should have one entry for each word in the FEM.
    cout 
      << "TMuiAddressTable::SoftwareAddress-E2 "
      << " config table size = " << fFEMConfig.size()
      << " != expected size of "
      << TMuiReadoutID::kWordsTotal << "!" << endl;
    return TMuiChannelId();
  }

  bool found(true);
  for(vector<PdbMuiChannelMap*>::const_iterator it = fFEMConfig.begin(); it != fFEMConfig.end(); it++)
    if ((*it)->HasHardwareIndex(moduleID, roc, word, channel)) {

      twopack = -1;
      (*it)->SoftwareIndex(channel, arm, gap, panel, orient, twopack);
      if (twopack >= 0) found = true;

    }

  if( !found ) {
    cout 
      << "TMuiAddressTable::SoftwareAddress-E5  did not find hardware "
      << ident << endl;
    return TMuiChannelId();
  } else
    return orient == kHORIZ ? 
      TMuiChannelId(arm, gap, panel, kHORIZ, twopack):
      TMuiChannelId(arm, gap, panel, kVERT,  twopack);
}

//___________________________________________________________
TMuiReadoutID TMuiAddressTable::HardwareAddress(
						const unsigned long& module_id, 
						const short& word_index) const
{

  if (fFEMConfig.size() != (unsigned int)TMuiReadoutID::kWordsTotal) 
  {
    // The FEM config table should have one entry for each word in the FEM.
    cout 
      << "TMuiAddressTable::HardwareAddress-E102  module " << module_id
      << " config table size = " << fFEMConfig.size()
      << " != expected size of "
      << TMuiReadoutID::kWordsTotal << "!" << endl;
    return TMuiReadoutID();
  }

  bool found( false );
  short roc = -1, word = -1, channel = -1;

  for (vector<PdbMuiChannelMap*>::const_iterator it = fFEMConfig.begin(); it != fFEMConfig.end(); it++)
    if ((*it)->HasHardwareIndex(module_id, word_index)) {

      found = true;
      unsigned int temp_modid = 0; 
      short temp_wordindex = -1 ;
			
      // read module id, roc number, word index and word from fem config
      (*it)->HardwareIndex(temp_modid, roc, temp_wordindex, word);

    }

  if(found) return TMuiReadoutID(module_id, roc, word, channel, word_index);
  else {
    cout << "TMuiAddressTable::HardwareAddress-I111 - not found" << endl;
    return TMuiReadoutID();
  }

}

//___________________________________________________________
TMuiReadoutID TMuiAddressTable::HardwareAddress(const TMuiChannelId& ident) const
{

  if (fFEMConfig.size() != (unsigned int)TMuiReadoutID::kWordsTotal) {
		
    // The FEM config table should have one entry for each word in the FEM.
    cout << "TMuiAddressTable::HardwareAddress-E1  ";
    if (ident.Orient() == kHORIZ) cout << "A" << ident.Arm() << "H";
    else cout << "A" << ident.Arm() << "V";

    cout 
      << " FEM config table size = " << fFEMConfig.size()
      << " != expected size of "
      << TMuiReadoutID::kWordsTotal << "!" << endl;
    return TMuiReadoutID();
  }

  bool found( false );
	
  short roc = -1, word = -1, channel = -1, word_index = -1;
  unsigned int moduleID = 0; 
  TMuiReadoutID idHard;

  for (vector<PdbMuiChannelMap*>::const_iterator it = fFEMConfig.begin(); it != fFEMConfig.end(); it++)
    if ((*it)->HasSoftwareIndex(ident.Arm(), ident.Plane(), ident.Panel(), ident.Orient(), ident.TwoPack())) 
      {
	channel = -1;
	(*it)->HardwareIndex(ident.TwoPack(), moduleID, roc, word_index, word, channel);
	idHard.Set(moduleID, roc, word, channel, word_index);
	if (channel >= 0) found = true;
      }

  if ( found && idHard.IsValid() ) return TMuiReadoutID(moduleID,roc,word,channel,word_index);
  else return TMuiReadoutID();

}

//___________________________________________________________
void TMuiAddressTable::initialize()
{

  if( fInit ) 
  { 
    
    cout << "TMuiAddressTable::initialize - table already initialized" << endl;
    return;
    
  } else {
   
    cout << "TMuiAddressTable::initialize - initializing table from file" << endl;
    
  }
  
  PdbMuiChannelMap* config_entry;
  char linebuf[256];

  // Remove previous entries, to handle multiple calls to Init().
  // (maybe just return instead?)
  Clear();

  // File contains the
  // (arm,gap,panel,orient,twopack) <-> (fem,roc,word,channel) mapping.

  short ROC, Word, Rword, ChanLo, ChanHi;
  short Arm, Orient, Gap, Panel, TwoPackLo, TwoPackHi, LocLo, LocHi;
  unsigned long moduleID;

  ifstream instr1("mui-fem-config.dat");
  if (!instr1) {
    cout << "TMuiAddressTable::Init-E1" << "  error opening FEM config data file" << endl;
    return;
  }

  // Read the data line by line until we reach EOF.
  while (instr1.getline(linebuf,256,'\n')) 
  {

    if (instr1.gcount() > 256) {
      cout 
        << "input buffer too small!  gcount = " << instr1.gcount()
        << endl;
      return;
    }

    // parse line
    istringstream stringbuf(linebuf);
    if(
       stringbuf 
       >> Arm >> Orient >> ROC >> Word >> Rword
       >> ChanLo >> ChanHi
       >> Gap >> Panel >> TwoPackLo >> TwoPackHi >> LocLo >> LocHi
       ) {

      // set module
      if (Arm == kNORTH) moduleID  = (Orient == kHORIZ) ? 0x1000 : 0x1010;
      else moduleID  = (Orient == kHORIZ) ? 0x0000:0x0010;

      config_entry = new PdbMuiChannelMap();

      config_entry->SetSoftwareIndex(Arm, Gap, Panel, Orient);
      config_entry->SetHardwareIndex(moduleID, ROC, Rword, Word);
      config_entry->SetChannelRange(ChanLo, ChanHi);
      config_entry->SetTwoPackRange(TwoPackLo, TwoPackHi);
      config_entry->SetLocationRange(LocLo, LocHi);

      // Copy the entry into the appropriate FEM config table.
      fFEMConfig.push_back(config_entry);

    }

  }

  instr1.close();

  if (fFEMConfig.size() != (unsigned int)TMuiReadoutID::kWordsTotal) {
    // The FEM config table should have one entry for each word in the FEM.
    cout 
      << "TMuiAddressTable::Init-E1  "
      << " FEM config table size = " << fFEMConfig.size()
      << " != expected size of "
      << TMuiReadoutID::kWordsTotal << "!" << endl;
  }

  fInit = true;
  
}

//___________________________________________________________
PHBoolean TMuiAddressTable::update(
  PHTimeStamp &Tstart, PHTimeStamp &Tstop,
  const char *calibname, PdbBankID bankID,
  const char *descrip )
{
  static const int kChannelMapBankID = 12100;

  cout << "TMuiAddressTable::update-I10  initializing" << endl;

  if (committed == 1) 
  { 
    
    if (!application->startUpdate()) 
    {
      cout << "TMuiAddressTable::update-F1" << "  Aborting ... Database not writable" << endl;
      application->abort();
      return False;
    } else committed = 0;

  }
          
  bankID.setInternalValue(kChannelMapBankID);
  addressBank = bankManager->createBank( "PdbMuiChannelMapBank", bankID, descrip, Tstart, Tstop, calibname);
  addressBank->setLength(fFEMConfig.size());
	
  //  addressBank->setLength(TMuiReadoutID::kWordsTotal);
  cout << "PdbMuiChannelMapBank length set = " << addressBank->getLength() << endl;
  
  start = Tstart;
  stop  = Tstop;

  PdbMuiChannelMap* map_entry = 0;
  vector<PdbMuiChannelMap*>::iterator it;
  unsigned long count = 0;

  for (it = fFEMConfig.begin(); it != fFEMConfig.end(); it++) 
  {

    if (count >= addressBank->getLength()) 
    {
      cout 
        << "TMuiAddressTable::update-E2  address bank index "
        << count << " larger than expected maximum value "
        << addressBank->getLength() << endl;
      break;
    }

    // for now, data is copied from the fFEMConfig array to 
    // addressBank.  This step would be unnecessary if 
    // fFEMConfig only contained pointers to the entries in 
    // addressBank, or if fFEMConfig were eliminated 
    // completely. 
    // See additional comments in TMuiAddressTable::fetch . 
    map_entry  = (PdbMuiChannelMap*)& addressBank->getEntry(count++);

    if (!map_entry) 
    {
      cout 
        << "TMuiAddressTable::update-E3  bank entry " << count
        << " is missing?" << endl;
      return False;
    }

    // copy entry in fFEMConfig to addressBank
    *map_entry = **it;  
    map_entry->print();
  }
	
  commit();
  return True;
}

//___________________________________________________________
PHBoolean TMuiAddressTable::fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID bankID)
{
	
  static const int kChannelMapBankID = 12100;

  if( fInit ) 
  { 
    
    cout << "TMuiAddressTable::fetch - table already initialized" << endl;
    return False;
    
  } else {
   
    cout << "TMuiAddressTable::fetch - initializing table from timestamp " << Tsearch << endl;
    
  }

  if (committed == 1) 
  {                       
    if (!application->startRead()) 
    {
      cout 
        << "TMuiAddressTable::fetch-F1"
        << "  Aborting ... Database not readable" << endl;
      application->abort();
    } else committed = 0;
  }

  bankID.setInternalValue(kChannelMapBankID);
  addressBank = bankManager->fetchBank( "PdbMuiChannelMapBank", bankID, calibname, Tsearch);
  addressBank->print();

  start = addressBank->getStartValTime();
  stop  = addressBank->getEndValTime();
  
  // Make sure that the sizes of the config tables are correct.
  fFEMConfig.reserve(TMuiReadoutID::kWordsTotal);
  for( int i = 0; i < TMuiReadoutID::kWordsTotal; i++) 
  {
    PdbMuiChannelMap* temp = new PdbMuiChannelMap();
    fFEMConfig.push_back(temp);   
  }
  
  vector<PdbMuiChannelMap*>::iterator it = fFEMConfig.begin();
  for(unsigned int i = 0; i < addressBank->getLength(); i++) 
  {
    
    PdbMuiChannelMap* map_entry = (PdbMuiChannelMap*)& addressBank->getEntry(i);
    if (!map_entry) {
      cout << "TMuiAddressTable::fetch-E3  bank entry " << i << " is missing?" << endl;
      return False;
    }
    
    // for now, data is copied from addressBank to the 
    // fFEMConfig array.  For future use, we might consider 
    // just copying the pointers, or using addressBank 
    // directly.  Two reasons for leaving it as it is: 
    // 1.  less chance of accidentally changing database 
    // 2.  this class can use nice STL iterators, etc. 
    if( it != fFEMConfig.end() )
    {
      **it = *map_entry;   
      it++;
    } else {
      
      cout << "TMuiAddressTable::fetch-E3  inconsistent FEM table size." << endl;
      
    }
  }
  
  delete addressBank;
  
  commit();
  fInit = true;
  return True;
}

//___________________________________________________________
ostream& operator << (ostream& s, const TMuiAddressTable& t)
{
  unsigned int module;
  short arm, orient, gap, panel;
  short sequence, roc, cable;
  short first_channel, last_channel;
  short first_twopack, last_twopack;
  short first_location, last_location;

  for (vector<PdbMuiChannelMap*>::const_iterator it = t.fFEMConfig.begin(); it != t.fFEMConfig.end(); it++) {
    first_channel = -1;
    first_twopack = -1;
    (*it)->SoftwareIndex(arm, gap, panel, orient);
    (*it)->HardwareIndex(module, roc, sequence, cable);
    (*it)->ChannelRange(first_channel, last_channel);
    (*it)->TwoPackRange(first_twopack, last_twopack);
    (*it)->LocationRange(first_location, last_location);

    if ( (first_channel >= 0) && (first_twopack >= 0) ) {
      s 
	<< " A" << arm << " G" << gap << " P" << panel << " O" << orient
	<< " T" << first_twopack  << "-" << last_twopack
	<< " X" << first_location << "-" << last_location << " : "
	<< " M" << hex << module << dec
	<< " S" << sequence
	<< " R" << roc << " I" << cable
	<< " C" << first_channel  << "-" << last_channel
	<< "\n";
    }
  }

  return s;
}

