// $Id: mMuiAddDBEntry.cc,v 1.7 2006/12/21 13:41:45 hpereira Exp $  
/*!
   \file mMuiAddDBEntry.cc
   \brief add a new entry in the muid database. 
   \version $Revision: 1.7 $
   \date $Date: 2006/12/21 13:41:45 $
*/

#include <iostream>
#include "MuiGeomClasses.hh"
#include "TMuiAddressTable.hh"
#include "PHIODataNode.h"
#include "PHTable.hh"
#include "mMuiAddDBEntry.hh"

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

//___________________________________________________________
mMuiAddDBEntry::mMuiAddDBEntry(): 
  _begin_time(2000,3,2,0,0,0), 
  _end_time(2000,12,31,0,0,0),
  _description( "MuID database" ),
  _time_set(false)
{ _end_time.setToFarFuture(); }

//___________________________________________________________
void mMuiAddDBEntry::SetTimeStamps(
  const PHTimeStamp& t_begin,
  const PHTimeStamp& t_end)
{
  _begin_time = t_begin;
  _end_time = t_end;
  cout << "mMuiAddDBEntry::SetTimeStamps-I1  begin " << _begin_time << "  end " << _end_time << endl;
  _time_set = true;
}

//___________________________________________________________
PHBoolean mMuiAddDBEntry::event(PHCompositeNode *root)
{
  //  an ONCS-style name needs to be defined for each bank, 
  //  such as PdbBankID("*.PBSC.E.SE3.SM5").  This isn't 
  //  critical right now, since PdbBankID doesn't use the name 
  //  at all, yet. 
  //  The "internal value" _is_ used, however; for lack of any 
  //  real conventions, I'll choose numbers in the range 
  //  12000-12999, following the DCM offset of 12 for MuID (see 
  //  $OFFLINE_MAIN/Event/inc/packetConstants.h). 

  PdbBankID bankID(42);
  //  bankID.setInternalValue(16384);

  char calibname1[] = "map.mui.mui_address";
  char calibname2[] = "geom.mui.";

  // Insert code here to navigate node hierarchy and find
  // or create specific nodes to pass to physics module...
  cout << "mMuiAddDBEntry::event-I1  phool version" << endl;

  if (!_time_set) {
    cout << "mMuiAddDBEntry::event-E2  exiting: time stamps not set; call SetTimeStamps" << endl;
    return False;
  }

  // Init() calls aren't needed; fetch() replaces them ...
  TMuiGeometry::Geom()->Init();
  TMuiAddressTable::Table()->Init();
  TMuiAddressTable::Table()->update(_begin_time, _end_time, calibname1, bankID, _description );
  TMuiGeometry::Geom()->update(_begin_time, _end_time, calibname2, bankID, _description );

  return True;
}
