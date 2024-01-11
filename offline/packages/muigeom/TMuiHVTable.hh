
#ifndef MUI_HVTABLE_H
#define MUI_HVTABLE_H

// $Id: TMuiHVTable.hh,v 1.6 2012/09/12 07:23:33 bbannier Exp $

/*!
  \file    TMuiHVTable.hh
  \brief   Maps addresses of MuID HV channels between "hardware" and "software" representations
  \author  A.M. Glenn (aglenn@bnl.gov)
  \version $Revision: 1.6 $
  \date    $Date: 2012/09/12 07:23:33 $
*/

#include <phool.h>
#include <PHAddressObject.h>

#include <iostream>
#include <vector>

#include "MuiCommon.hh"

// forward declarations follow ...
class PdbMuiHVMap;
class PdbIndex;
class PdbBankID;

//! Maps addresses of MuID HV channels between "hardware" and "software" representations
class TMuiHVTable : public PHAddressObject
{
  private:

  //! Default constructor (private to protect singleton).
  TMuiHVTable();

  //! Copy constructor (private to protect singleton).
  TMuiHVTable(const TMuiHVTable& source);

  //! Assignment operator (private to protect singleton).
  TMuiHVTable& operator=(const TMuiHVTable& source);

  //! Destructor (private to protect singleton).
  ~TMuiHVTable();

  //! fill HV conversion matrix
  static void FillMatrix();

  //! update (does nothing here)
  PHBoolean update(PHTimeStamp&, PHTimeStamp&, const char*, PdbBankID, char*)
  { return True; }

  public:

  //! Initializes the instance of TMuiHVTable (from a file or database).
  static void Init();

  //! Points to the *single* instance of TMuiHVTable.
  static TMuiHVTable* Table();

  //! Destroys the *single* instance of TMuiHVTable.
  static void Destroy();

  //! Given a hardware address, return the corresponding software address.
  int SoftwareAddress(const short& mainframe, const short& slot, const short& channel, 
    short& arm, short& orient, short& gap, short& panel, 
    short& chain, short& twopackLo, short& twopackHi) const;

  //! Given a software address, return the corresponding hardware address.
  int HardwareAddress(const short& arm, const short& orient, const short& gap,
    const short& panel, const short& chain, short& mainframe, 
    short& slot, short& channel, short& twopackLo, short& twopackHi) const;

  //! Given a software address, return the corresponding hardware address.
  int HardwareAddress(const short& arm, const short& orient, const short& gap,
    const short& panel, const short& layer, const short& twopack, 
    short& chain, short& mainframe, short& slot, short& channel, 
    short& twopackLo, short& twopackHi) const;

  //! true if the elements of the hardware address are within the allowed range
  bool IsHardwareValid(const short& mainframe, const short& slot, const short& channel) const;

  //! true if the elements of the software address are within the allowed range
  bool IsSoftwareValid(const short& arm, const short& orient, const short& gap, const short& panel, const short& chain) const;

  //! clear table
  void Clear();

  //!@name methods from base class
  //@{
  
  //! Initialize table before accessing database.
  virtual void initialize();

  //! Fetch address table from the database.
  virtual PHBoolean fetch(
    PHTimeStamp &Tsearch,
    const char *calibname, PdbBankID);

  //! Update database with current address table.
  virtual PHBoolean update(PHTimeStamp &Tstart,
    PHTimeStamp &Tstop,
    const char *calibname, PdbBankID,
    const char *descrip);

  //@}
  
  //! Unused methods from base class (PHAddressObject)
  //@{
  
  //! detector index
  virtual PdbIndex* getDetectorIndex(int ind, PdbIndex* pdb_index)
  { return 0; }
  
  //! detector index
  virtual PHBoolean setDetectorIndex(int ind, PdbIndex* value)
  { return True; }	
  
  //! global index
  virtual PHBoolean setGlobalIndex(PdbIndex* pdb_index)
  { return True; }	
  
  //! global index
  virtual PHBoolean setGlobalIndex(int ind)
  { return True; }	
  //@}

  private:

  //! The FEM configuration table (copied from database).
  static std::vector<PdbMuiHVMap*> fHVConfig;

  //! Pointer to the instance of TMuiHVTable.
  static TMuiHVTable* f_pMuiHVTable;
  
  //! true when initialized
  static short fInit;      
  
  //! muid geometry
  enum { 
    ARMS=2,
    ORIENTS=2,
    GAPS=5,
    PANELS=6,
    MAINFRAMES=6,
    CHAINS=6,
    LAYERS=2,
    SLOTS=16,
    CHANNELS=8,
    MAXTWOPACKS=64
  };

  //! Print TMuiHVTable information to a stream.
  friend std::ostream& operator << (std::ostream& s, const TMuiHVTable& t);
};

#endif  /* MUI_HVTABLE_H */
