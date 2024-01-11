// $Id: TMuiHVTable.cc,v 1.5 2009/08/22 13:58:52 hpereira Exp $
/*!
  \file    TMuiHBTable.cc
  \brief   Maps addresses of MuID HV channels between "hardware" and "software" representations
  \author  A.M. Glenn (aglenn@bnl.gov)
  \version $Revision: 1.5 $
  \date    $Date: 2009/08/22 13:58:52 $
*/

#include "TMuiHVTable.hh"

#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"
#include "PdbMuiHVMap.hh"

#include <fstream>
#include <sstream>
#include <iostream>
#include <vector>

using namespace std;

// ====================================================================
// INITIALIZATIONS OF TMuiHVTable STATIC MEMBERS.
// ====================================================================

TMuiHVTable* TMuiHVTable::f_pMuiHVTable = 0L;
short TMuiHVTable::fInit = 0;

vector<PdbMuiHVMap*> TMuiHVTable::fHVConfig(0);


// ====================================================================
// IMPLEMENTATIONS OF TMuiHVTable METHODS.
// ====================================================================

// TMuiHVTable::Table()
// Points to the *single* instance of TMuiHVTable.
TMuiHVTable* TMuiHVTable::Table()
{
  if (!f_pMuiHVTable)
    f_pMuiHVTable = new TMuiHVTable;

  if (!fInit) {
    //cout << "TMuiHVTable::Table-F1  not initialized!"  << endl;
    return f_pMuiHVTable;              // Should throw an exception here!
  } else {
    return f_pMuiHVTable;
  }
}


// TMuiHVTable::Init
// Initializes the instance of TMuiHVTable (from a file or database).
void
TMuiHVTable::Init()
{
  cout<<"INITIALIZING HV MAP!!!"<<endl;
  PdbMuiHVMap* config_entry;

  char linebuf[256];

  // Make sure that the sizes of the config tables are correct.
  fHVConfig.reserve(600); //Change to constant.

  // Remove previous entries, to handle multiple calls to Init().
  // (maybe just return instead?)
  f_pMuiHVTable->Clear();

  //
  // File contains the
  // (arm,gap,panel,orient,twopack,chain) <-> (fem,roc,word,channel) mapping.
  //

  short MainFrame, Slot, SupplyChannel;
  short Arm, Orient, Chain, Gap, Panel, TwoPackLo, TwoPackHi;

  ifstream instr1("mui-hv-config.dat");
  if (!instr1) {
    cout << "TMuiHVTable::Init-E1"
   << "  error opening HV config data file" << endl;
    return;
  }

  // Read the data line by line until we reach EOF.
  while (instr1.getline(linebuf,256,'\n')) 
  {

    if (instr1.gcount() > 256) {
      cout << "input buffer too small!  gcount = " << instr1.gcount() << endl;
      return;
    }

    istringstream stringbuf(linebuf);

    if (stringbuf >> Arm >> Orient >> Chain >>
  Gap >> Panel >> TwoPackLo >> TwoPackHi >> 
  MainFrame >> Slot >> SupplyChannel) {

      config_entry = new PdbMuiHVMap();

      config_entry->SetSoftwareIndex(Arm, Gap, Panel, Orient, Chain);
      config_entry->SetHardwareIndex(MainFrame, Slot, SupplyChannel);
      config_entry->SetTwoPackRange(TwoPackLo, TwoPackHi);

      // Copy the entry into the appropriate HV config table.
      fHVConfig.push_back(config_entry);

    } else {
      // Skip any header or comment lines.
//       cout << "read comment line" << endl;
    }

  }

  instr1.close();

  if (fHVConfig.size() != 600) {
    // The HV config table should have one entry for each HV channel.
    cout << "TMuiHVTable::Init-E1  "
   << " HV config table size = " << fHVConfig.size()
   << " != expected size of "
   << 600 << "!" << endl;
    //    return;
  }

  fInit = 1;
  cout<<"HV MAP INITIALIZED!!!"<<endl;
}


// TMuiHVTable::Destroy()
// Destroys the *single* instance of TMuiHVTable.
void TMuiHVTable::Destroy()
{
  delete f_pMuiHVTable;       // Calls destructor.
  f_pMuiHVTable = 0L;
  fInit = 0;
}


// TMuiHVTable::TMuiHVTable()
// Constructor.
TMuiHVTable::TMuiHVTable()
  : PHAddressObject()
{}

// TMuiHVTable::~TMuiHVTable()
// Destructor.
TMuiHVTable::~TMuiHVTable()
{
  delete index;
  Clear();
}

// TMuiHVTable::SoftwareAddress
// Given a hardware address (fem,roc,word,channel) of a MuID
// detector channel, return the corresponding software address
// (arm,plane,panel,orient,twopack).
int
TMuiHVTable::SoftwareAddress(const short& mainframe, const short& slot, const short& channel, 
          short& arm, short& orient, short& gap, short& panel, 
          short& chain, short& twopackLo, short& twopackHi) const
{
  arm = -1;
  gap = -1;
  panel = -1;
  orient = -1;
  chain = -1;
  twopackHi = -1;
  twopackLo = -1;

  vector<PdbMuiHVMap*>::iterator it;

  if (fHVConfig.size() != 600) {
    // The HV config table should have one entry for each HV chain.
    cout << "TMuiHVTable::SoftwareAddress-E2 "
   << " config table size = " << fHVConfig.size()
   << " != expected size of "
   << 600 << "!" << endl;
    return 1;
  }

  short found_entry = 0;

  // TODO:  might be nice to use STL find or find_if here!

  for (it = fHVConfig.begin(); it != fHVConfig.end(); it++) {
    if ((*it)->HasHardwareIndex(mainframe, slot, channel)) {
      (*it)->SoftwareIndex(arm, gap, panel, orient, chain);
      (*it)->TwoPackRange(twopackLo, twopackHi);
      found_entry = 1;
    }
  }
  
  if (found_entry == 0) {
    cout << "TMuiHVTable::SoftwareAddress-E5  did not find hardware "
   <<  endl;
    return 0;
  } else {
    return  1;
  }
}


// TMuiHVTable::HardwareAddress
// Given a software address (arm,gap,panel,orient,twopack) of
// a MuID detector channel, return the corresponding hardware
// address (fem,roc,word,channel).

int 
TMuiHVTable::HardwareAddress(const short& arm, const short& orient, const short& gap,
          const short& panel, const short& chain, short& mainframe, 
          short& slot, short& channel, short& twopackLo, short& twopackHi) const
{
  twopackHi = -1;
  twopackLo = -1;
  mainframe = -1;
  slot = -1; 
  channel = -1;
  
//    cout <<"HVTable::HardwareAddress"
//         <<" arm " <<arm
//         <<" gap "<<gap
//         <<" panel "<<panel
//         <<" orient "<<orient
//         <<" chain "<<chain
//         <<" 2pack "<<twopack<<endl;

  vector<PdbMuiHVMap*>::iterator it;

  if (fHVConfig.size() != 600) {
    // The HV config table should have one entry for each HV chain.
    cout << "TMuiHVTable::HardwareAddress-E1  ";
    if (orient == kHORIZ) {
      cout << "A" << arm << "H";
    }
    else {
      cout << "A" << arm << "V";
    }
    cout << " HV config table size = " << fHVConfig.size()
   << " != expected size of "
   << 600 << "!" << endl;
    return 1;
  }

  short found_entry = 0;

  // TODO:  use STL find or find_if here?

  for (it = fHVConfig.begin(); it != fHVConfig.end(); it++) {
    if ((*it)->HasSoftwareIndex(arm, gap, panel, orient, chain)) {
      (*it)->HardwareIndex(mainframe, slot, channel);
      (*it)->TwoPackRange(twopackLo, twopackHi);
      found_entry = 1;
    }
  }

  if (found_entry == 0) {
    cout << "TMuiHVTable::HardwareAddress-E1  did not find "
   << endl;
    return 1;
  } else {
    return  0;
  }

}

int 
TMuiHVTable::HardwareAddress(const short& arm, const short& orient, const short& gap,
        const short& panel, const short& layer, const short& twopack, 
        short& chain, short& mainframe, short& slot, short& channel, 
        short& twopackLo, short& twopackHi) const
{
  chain = -1;
  twopackHi = -1;
  twopackLo = -1;
  mainframe = -1;
  slot = -1; 
  channel = -1;
  short tempArm=-1;
  short tempOrient=-1;
  short tempGap=-1;
  short tempPanel=-1;
  
//    cout <<"HVTable::HardwareAddress"
//         <<" arm " <<arm
//         <<" gap "<<gap
//         <<" panel "<<panel
//         <<" orient "<<orient
//         <<" chain "<<chain
//         <<" 2pack "<<twopack<<endl;

  vector<PdbMuiHVMap*>::iterator it;

  if (fHVConfig.size() != 600) {
    // The HV config table should have one entry for each HV chain.
    cout << "TMuiHVTable::HardwareAddress-E1  ";
    if (orient == kHORIZ) {
      cout << "A" << arm << "H";
    }
    else {
      cout << "A" << arm << "V";
    }
    cout << " HV config table size = " << fHVConfig.size()
   << " != expected size of "
   << 600 << "!" << endl;
    return 1;
  }

  short found_entry = 0;

  // TODO:  use STL find or find_if here?

  for (it = fHVConfig.begin(); it != fHVConfig.end(); it++) {
    if ((*it)->HasSoftwareIndexLayer(arm, gap, panel, orient, layer, twopack)) {
      (*it)->SoftwareIndex(tempArm, tempGap, tempPanel, tempOrient, chain);
      (*it)->HardwareIndex(mainframe, slot, channel);
      (*it)->TwoPackRange(twopackLo, twopackHi);
      found_entry = 1;
    }
  }

  if (found_entry == 0) {
    cout << "TMuiHVTable::HardwareAddress-E1  did not find "
   << endl;
    return 1;
  } else {
    return  0;
  }

}

// TMuiHVTable::IsHardwareValid()
// Check that the hardware address makes sense.
bool
TMuiHVTable::IsHardwareValid(	  const short& mainframe,
          const short& slot,
          const short& channel) const
{
  if ((mainframe<96)||(slot<0)||(channel<0)||
    (mainframe>(96+MAINFRAMES))||(slot>SLOTS)||(channel>CHANNELS))
    {
      return False;
    }
  return True;
}

bool 
TMuiHVTable::IsSoftwareValid(     const short& arm, const short& orient, 
          const short& gap, const short& panel,
          const short& chain) const
{
  if ((arm<0)||(orient<0)||(gap<0)||(panel<0)|(chain<0)||
      (arm>ARMS)||(orient>ORIENTS)||(gap>GAPS)||(panel>PANELS)||(chain>CHAINS))
    {
      return False;
    }
  return True;
}

void
TMuiHVTable::Clear()
{
  while (fHVConfig.size() > 0) {
    PdbMuiHVMap* t = fHVConfig.back();
    delete t;
    fHVConfig.pop_back();
  }
}


void
TMuiHVTable::initialize()
{
  Init();
}

PHBoolean
TMuiHVTable::fetch(PHTimeStamp &Tsearch,
      const char *calibname, PdbBankID bankID)
{
  static const int kHVMapBankID = 12200;

  if (committed == 1) {                       
    if (!application->startRead()) {
      cout << "TMuiHVTable::fetch-F1"
     << "  Aborting ... Database not readable" << endl;
      application->abort();
    } else {
      committed = 0;
    }
  }

  bankID.setInternalValue(kHVMapBankID);
  addressBank = bankManager->fetchBank("PdbMuiHVMapBank",
               bankID, calibname, Tsearch);
  if(!addressBank){
    cout <<"TMuiHVTable::fetch-F2"
   <<" Bank Not Found"<<endl;
    return False;
  }
  
  addressBank->print();

  start = addressBank->getStartValTime();
  stop  = addressBank->getEndValTime();
  
  PdbMuiHVMap* map_entry = 0;

  // Make sure that the sizes of the config tables are correct.
  fHVConfig.reserve(600);

  //if (fHVConfig.size() != addressBank->getLength()) {
  //  // TODO:  think about how to handle this (unlikely?) case!
  //  //        Resize fHVConfig?
  //  cout << "TMuiHVTable::fetch-E2 "
  //	 << " address bank size = " << addressBank->getLength()
  //	 << " != config table size of " << fHVConfig.size() << endl;
  //    return False;
  //  }

  vector<PdbMuiHVMap*>::iterator it = fHVConfig.begin();
  if(!fInit){
    for(unsigned int i = 0; i < addressBank->getLength(); i++) {
      PdbMuiHVMap* temp = new PdbMuiHVMap();
      fHVConfig.push_back(temp);
    }
   
  }
  for(unsigned int i = 0; i < addressBank->getLength(); i++) {
    // TODO:  if bad_cast exception can be caught, code can be
    //        simplified to
    // (**it) = dynamic_cast<PdbMuiHVMap&>(addressBank->getEntry(i));
    
    map_entry = (PdbMuiHVMap*)& addressBank->getEntry(i);
    if (!map_entry) {
      cout << "TMuiHVTable::fetch-E3  bank entry " << i
     << " is missing?" << endl;
      return False;
    }
    
    // TODO:  for now, data is copied from addressBank to the
    //        fHVConfig array.  For future use, we might consider
    //        just copying the pointers, or using addressBank
    //        directly.  Two reasons for leaving it as it is:
    //        1.  less chance of accidentally changing database
    //        2.  this class can use nice STL iterators, etc.
    
    **it = *map_entry;   // copy entry in addressBank to fHVConfig
    //    (*it)->print();
    it++;
  }
  delete  addressBank; 
  commit();
  fInit = 1;   // TODO:  suffice to avoid Init call before fetch?
  //  FillMatrix();
  return True;
}

PHBoolean
TMuiHVTable::update(PHTimeStamp &Tstart, PHTimeStamp &Tstop,
       const char *calibname, PdbBankID bankID,
       const char *descrip)
{
  static const int kHVMapBankID = 12200;

  cout << "TMuiHVTable::update-I10  initializing" << endl;
  //  initialize();
  //  cout << "TMuiHVTable::update-I11  initialization done" << endl;

  if (committed == 1) {                       
    if (!application->startUpdate()) {
      cout << "TMuiHVTable::update-F1"
     << "  Aborting ... Database not writable" << endl;
      application->abort();
      return False;
    } else {
      committed = 0;
    }
  }
          
  bankID.setInternalValue(kHVMapBankID);
  addressBank = bankManager->createBank("PdbMuiHVMapBank", bankID,
                descrip, Tstart, Tstop, calibname);

  addressBank->setLength(fHVConfig.size());
  cout << "PdbMuiHVMapBank length set = "
       << addressBank->getLength() << endl;
  //  addressBank->print();
  
  start = Tstart;
  stop  = Tstop;

  PdbMuiHVMap* map_entry = 0;
  vector<PdbMuiHVMap*>::iterator it;
  unsigned long count = 0;

  for (it = fHVConfig.begin(); it != fHVConfig.end(); it++) {

    if (count >= addressBank->getLength()) {
      cout << "TMuiHVTable::update-E2  address bank index "
     << count << " larger than expected maximum value "
     << addressBank->getLength() << endl;
      break;
    }

    // TODO:  for now, data is copied from the fHVConfig array to
    //        addressBank.  This step would be unnecessary if
    //        fHVConfig only contained pointers to the entries in
    //        addressBank, or if fHVConfig were eliminated
    //        completely.
    //        See additional comments in TMuiHVTable::fetch .

    map_entry  = (PdbMuiHVMap*)& addressBank->getEntry(count++);

    if (!map_entry) {
      cout << "TMuiHVTable::update-E3  bank entry " << count
     << " is missing?" << endl;
      return False;
    }

    *map_entry = **it;  // copy entry in fHVConfig to addressBank
    map_entry->print();
  }
  commit();
  return True;
}

// Print TMuiHVTable information to a stream.
ostream&
operator << (ostream& s, const TMuiHVTable& t)
{
  vector<PdbMuiHVMap*>::iterator it;
  short arm, gap, panel, orient, chain, twopackHi;
  short twopackLo, mainFrame, slot, supplyChannel;

  for (it = t.fHVConfig.begin(); it != t.fHVConfig.end(); it++) {
    arm = -1;
    orient = -1;
    gap = -1;
    panel = -1;
    chain = -1;
    mainFrame = -1;
    slot = -1;
    supplyChannel = -1;
    twopackLo = -1;
    twopackHi = -1;
    
    (*it)->SoftwareIndex(arm, gap, panel, orient, chain);
    (*it)->HardwareIndex(mainFrame, slot, supplyChannel);
    (*it)->TwoPackRange(twopackLo, twopackHi);

    if ( twopackLo >= 0 ) {
      s << " A" << arm << " G" << gap << " P" << panel << " O" << orient
  << " Chain" << chain
  << " T" << twopackLo  << "-" << twopackHi
  << " M" << mainFrame
  << " S" << slot
  << " C" << supplyChannel
  << endl;
    }
  }

  return s;
}

