#ifndef MUI_ADDRESSTABLE_H
#define MUI_ADDRESSTABLE_H

// $Id: TMuiAddressTable.hh,v 1.11 2009/08/22 13:58:52 hpereira Exp $
/*!
   \file TMuiAddressTable.hh
   \brief Maps addresses of MuID channels between "hardware" and "software" representations.
   \version $Revision: 1.11 $
   \date $Date: 2009/08/22 13:58:52 $
*/

#include "MuiCommon.hh"
#include "TMuiChannelId.hh"
#include "TMuiReadoutID.hh"
#include "PHAddressObject.h"

#include "phool.h"

#include <iostream>
#include <vector>

// forward declarations follow ...
class PdbMuiChannelMap;
class PdbIndex;
class PdbBankID;

//!Maps addresses of MuID channels between "hardware" and "software" representations.
/*!
 Maps addresses of MuID channels between "hardware" and
 "software" representations.
 This is a singleton class.
 
 @author <a href="mailto:pope@phy.ornl.gov">Kyle Pope </a>
 @see TMuiChannelId
 @see TMuiReadoutID
*/

class TMuiAddressTable : public PHAddressObject
{
	
  private:

  //! Default constructor (private to protect singleton).
  TMuiAddressTable();

  //! Copy constructor (private to protect singleton).
  TMuiAddressTable(const TMuiAddressTable& source);

  //! Assignment operator (private to protect singleton).
  TMuiAddressTable& operator=(const TMuiAddressTable& source);

  //! Destructor (private to protect singleton)
  /*! this is never called */
  virtual ~TMuiAddressTable();

  //! update (does nothing here)
  PHBoolean update(PHTimeStamp&, PHTimeStamp&, const char*, PdbBankID, char*)
  { return True; }
  
  
  public:

  //! Initializes the instance of TMuiAddressTable (from a file or database).
  static void Init()
  { Table()->initialize(); }

  ///! pointer to the *single* instance of TMuiAddressTable.
  static TMuiAddressTable* Table();

  //! Return the arm identifier for the given module identifier.
  short Arm(const unsigned long& module_id) const;

  //! Return the orientation (kHORIZ,kVERT) for the given module identifier.
  short Orient(const unsigned long& module_id) const;

  //! Given a hardware address, return the corresponding software address.
  TMuiChannelId SoftwareAddress(const TMuiReadoutID& ident) const;

  //! Given a software address, return the corresponding hardware address.
  TMuiReadoutID HardwareAddress(const TMuiChannelId& ident) const;

  //! Find the hardware addr. for the FEM and position in the data list.
  TMuiReadoutID HardwareAddress(const unsigned long& module_id, const short& word_index) const;

  //! Are the elements of the hardware address within the allowed range?
  bool IsHardwareValid(
    const unsigned long& module_id,  
    const short& roc,
    const short& word, const short& channel, 
    const short& word_index) const
  { return TMuiReadoutID(module_id, roc, word, channel, word_index).IsValid(); }

  //! erase the femTable
  void Clear();

  //! Initialize table before accessing database.
  virtual void initialize();

  //! Fetch address table from the database.
  virtual PHBoolean fetch(PHTimeStamp &Tsearch,
    const char *calibname, PdbBankID);

  //! Update database with current address table.
  virtual PHBoolean update(
    PHTimeStamp &Tstart,
    PHTimeStamp &Tstop,
    const char *calibname, PdbBankID,
    const char *descrip);

  //! Unused methods from base class (PHAddressObject)
  //@{

  //! detector index
  virtual PdbIndex* getDetectorIndex(int ind, PdbIndex* pdb_index)
  { return 0; }

  //! detector index
  virtual PHBoolean setDetectorIndex(int ind, PdbIndex* value)
  { return true; }
	
  //! global index
  virtual PHBoolean setGlobalIndex(PdbIndex* pdb_index)
  { return true; }
			
  virtual PHBoolean setGlobalIndex(int ind)
  { return true; }
  
  //@}

  private:

  //! The FEM configuration table (copied from database).
  std::vector<PdbMuiChannelMap*> fFEMConfig;
	
  //! true when initialized
  bool fInit;      

  //! write to stream
  friend std::ostream& operator << (std::ostream& s, const TMuiAddressTable& t);
};

#endif  /* MUI_ADDRESSTABLE_H */
