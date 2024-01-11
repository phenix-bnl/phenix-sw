// $Id: TMuiReadoutID.cc,v 1.4 2006/12/20 17:04:58 hpereira Exp $ 
/*!
  \file    TMuiReadoutID.cc
  \brief   "hardware" representation of a MuID channel address.
  \author  A.M. Glenn (aglenn@bnl.gov)
  \version $Revision: 1.4 $
  \date    $Date: 2006/12/20 17:04:58 $
*/


#include <iostream>
#include "TMuiReadoutID.hh"

using namespace std;

const short TMuiReadoutID::kROCsPerFEM=20;
const short TMuiReadoutID::kWordsPerROC=6;
const short TMuiReadoutID::kChannelsPerWord=16;
const short TMuiReadoutID::kFEMsTotal=4;
const short TMuiReadoutID::kROCsTotal=TMuiReadoutID::kFEMsTotal*TMuiReadoutID::kROCsPerFEM;
const short TMuiReadoutID::kWordsTotal=TMuiReadoutID::kROCsTotal*TMuiReadoutID::kWordsPerROC;
const short TMuiReadoutID::kChannelsTotal=TMuiReadoutID::kWordsTotal*TMuiReadoutID::kChannelsPerWord;
const short TMuiReadoutID::kWordsPerFEM=TMuiReadoutID::kROCsPerFEM*TMuiReadoutID::kWordsPerROC;

// TMuiReadoutID::TMuiReadoutID()
// Constructor (FEM specified by module ID).
TMuiReadoutID::TMuiReadoutID(
  const unsigned long& module_id,
  const short& roc,
  const short& word,
  const short& channel,
  const short& word_index)
  : 
  fModuleID(0), 
  fROC(roc), 
  fWord(word), 
  fChannel(channel),
  fWordIndex(word_index)
{
  switch (module_id) {
  case 0x0000:
  case 0x0010:
  case 0x1000:
  case 0x1010:
    fModuleID = module_id;
    break;
  default:
    fModuleID = 0;
    cout << "TMuiReadoutID(ctor)-E1  invalid module ID = " << module_id << "\n";
    cout << "                        using default = " << fModuleID << endl;
    break;
  }


  if ( (fChannel < 0) || (fChannel > 15) ) 
  {
    if (fChannel != -1) cout << "TMuiReadoutID(ctor)-E2  channel id = " << fChannel  << "out of range" << endl;
    fBits = 0;
  } else fBits  = (0x01) << fChannel;
}

short TMuiReadoutID::FEM() const
{
  switch (fModuleID) {
  case 0x0000:
    return 0;
    break;
  case 0x0010:
    return 1;
    break;
  case 0x1000:
    return 2;
    break;
  case 0x1010:
    return 3;
    break;
  default:
    cout 
    << "TMuiReadoutID::FEM-E1  invalid module ID = "
    << fModuleID << endl;
    return -1;
    break;
  }
}


bool TMuiReadoutID::IsValid() const
{
  return (  
    FEM() >= 0 && FEM() < kFEMsTotal &&
    fROC >= 0 && fROC < kROCsPerFEM &&
    fWord >= 0 && fWord < kWordsPerROC &&
    fChannel >= 0 && fChannel < kChannelsPerWord &&
    fWordIndex > 0 && fWordIndex <= kWordsPerFEM );
}

void TMuiReadoutID::Set(
  const unsigned long& module_id,
  const short& roc, 
  const short& word,
  const short& channel,
  const short& word_index)
{
  switch (module_id) {
    case 0x0000:
    case 0x0010:
    case 0x1000:
    case 0x1010:
    fModuleID = module_id;
    break;
    
    default:
    fModuleID = 0;
    cout << "TMuiReadoutID::Set-E1  invalid module ID = " << module_id << "\n";
    cout << "                       using default = " << fModuleID << endl;
    break;
  }

  fROC = roc;
  fWord = word;
  fChannel = channel;
  fWordIndex = word_index;
  if ( (fChannel < 0) || (fChannel > 15) ) 
  {
    if (fChannel != -1) cout << "TMuiReadoutID::Set-E2  channel id = " << fChannel << "out of range" << endl;
    fBits = 0;
  }else fBits  = (0x01) << fChannel;

}

// Functions for hash lookup based on TMuiReadoutID objects.
// Implement these functions outside of the TMuiReadoutID class for
// flexibility.
// (See Taligent guide, "Portable Hash")

//: Returns the position of the given TMuiReadoutID object in the
//: ???? hash table (no longer used, but left in for now).
size_t ChannelHash(const TMuiReadoutID& ident)
{
  return ( ( 
    (ident.FEM()
    *TMuiReadoutID::kROCsPerFEM   + ident.ROC() )
    *TMuiReadoutID::kWordsPerROC + ident.Word() ) 
    *TMuiReadoutID::kChannelsPerWord + ident.Channel() )
    % TMuiReadoutID::kChannelsTotal;
}

// Output to a stream.
ostream& operator << (ostream& s, const TMuiReadoutID& ident)
{
  s << "Module ID " << ident.ModuleID()
    << " ROC " << ident.ROC()
    << " Word " << ident.Word()
    << " Channel " << ident.Channel()
    << " WordIndex " << ident.WordIndex();
  return s;
}
