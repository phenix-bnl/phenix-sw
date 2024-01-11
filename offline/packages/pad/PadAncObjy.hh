#ifndef PADANCOBJY_H
#define PADANCOBJY_H

//--------------------------------------------------------------- 
//                                                                
// Created by: David Silvermyr
//                                                                
// Description: Header for PadAncObjy class
// stores info on HV, ROC calibrations and ROC positions
//                                                              
//----------------------------------------------------------------

#include "PadBasicObject.hh"
#include "PadAddressObject.hh"

#include "PdbBankID.hh"
#include "PdbCalBank.hh"

#include <cstdlib>
#include <iostream>
#include <cstdio>
#include <string>

const int MAXPCPLANES=6; // there are 3 pc layers and 2 arms 
const int MAXHVSECT=32; // 32 per pc and arm
const int MAXINSTALLROC=4320; // 45*2*8=720 per pc and arm, * MAXPCPLANES
const int MAXCALROC=5000; // don't think we'll ever have more than that

/**
   This is the PAD Ancillary Objy class. It is used to hold PAD 
   HV, ROC Cal. and Pos. calibration data. 
   Basic DB validity checks are inherited 
   from PadBasicObject <br>
   @memo PAD Calibration Class
*/
class PadAncObjy : public PadBasicObject { 
  
public:
  
  // Constructor
  PadAncObjy(); 
  // Destructor
  ~PadAncObjy(); 
  
  // member functions, fills data in our Calibration object
  // from default ASCII files
  PHBoolean FetchFromFile();
  // from ASCII file "filehv", "fileroccal" and "filerocpos"
  PHBoolean FetchFromFile(const char* filehv, const char* fileroccal, const char* filerocpos);

  // individual bank handlling routines
  // from default ASCII file
  PHBoolean FetchHVFromFile();
  // from an ASCII file "filename"
  PHBoolean FetchHVFromFile(const char* filename);
  
  // from default ASCII file
  PHBoolean FetchROCCalFromFile();
  // from an ASCII file "filename"
  PHBoolean FetchROCCalFromFile(const char* filename);

  // from default ASCII file
  PHBoolean FetchROCPosFromFile();
  // from an ASCII file "filename"
  PHBoolean FetchROCPosFromFile(const char* filename);

  // first routines to get all banks
  PHBoolean Fetch();
  // Fetch HV information from Objectivity Database
  PHBoolean FetchHVObjy();
  // Fetch ROC Cal information from Objectivity Database
  PHBoolean FetchROCCalObjy();
  // Fetch ROC Pos information from Objectivity Database
  PHBoolean FetchROCPosObjy();

//*************************************************************
  // member functions, puts data from our Calibration object
  // to default ASCII files
  PHBoolean PutToFile();
  // to ASCII file "filehv", "fileroccal" and "filerocpos"
  PHBoolean PutToFile(const char* filehv, const char* fileroccal, const char* filerocpos);
  
  // individual bank handlling routines
  // to default ASCII file
  PHBoolean PutHVToFile();
  // to an ASCII file "filename"
  PHBoolean PutHVToFile(const char* filename);
  
  // to default ASCII file
  PHBoolean PutROCCalToFile();
  // to an ASCII file "filename"
  PHBoolean PutROCCalToFile(const char* filename);

  // to default ASCII file
  PHBoolean PutROCPosToFile();
  // to an ASCII file "filename"
  PHBoolean PutROCPosToFile(const char* filename);

  // into the database banks (Objy or ASCII)
  PHBoolean Put();
  // Put hv information into Objectivity Database
  PHBoolean PutHVObjy();
  // Put ROC calibration information into Objectivity Database
  PHBoolean PutROCCalObjy();
  // Put ROC postion information into Objectivity Database
  PHBoolean PutROCPosObjy();

//*************************************************************
  
  int getNumberPCPlanes() const
  { return MAXPCPLANES; }
  int getNumberActivePCPlanes() const
  { return numberHVPlanes; }

  // routines to obtain info about the HV
  // summary
  int getNumberHVSect() const
  { return MAXHVSECT; }

  PHBoolean calcHVSummaryInfo();

  int getNumberBadHVTot() const
  { return numberBadHVTot; } 
  int getNumberOKHVTot() const
  { return numberOkHVTot; }

  PHBoolean planeActive(short iplane) const
  { return ( ((iplane>=0) && (iplane<MAXPCPLANES)) ? PlaneInfoAvailable[iplane] : False); }
  int getPC(short iplane) const
  { return ( ((iplane>=0) && (iplane<MAXPCPLANES)) ? iplane/2 : -1); }
  int getArm(short iplane) const
  { return ( ((iplane>=0) && (iplane<MAXPCPLANES)) ? iplane%2 : -1); }

  int getNumberBadHVSect(short ipc, short iarm) const
  { return ( (((ipc*2+iarm)>=0) && ((ipc*2+iarm)<MAXPCPLANES)) ? numberBadHVSect[ipc*2+iarm] : -1); }
  int getNumberOKHVSect(short ipc, short iarm) const
  { return ( (((ipc*2+iarm)>=0) && ((ipc*2+iarm)<MAXPCPLANES)) ? numberOkHVSect[ipc*2+iarm] : -1); }

  // individual info
  PHBoolean getHVStatus(short ipc, short iarm, short ihvsectid); // false not ok, true ok
  short getHVChamber(short ipc, short ihvsectid); // really sector, but want to avoid confusion with hvsector..
  short getHVSide(short ipc, short iarm, short ihvsectid);
  short getHVNwires(short ipc, short ihvsectid);
  PHBoolean getHVHardwareNotation(short ipc, short iarm, short ihvsectid, std::string &hvhwname);

  // and more routines to obtain info about the ROC calibrations
  int getNumberROCCal() const
  { return numberROCCal; }
  int getMaxNumberROCCal() const
  { return MAXCALROC; }

  // from PdbPadROCCal.hh
  int getROCnumberROCCal(short si)  const 
  { return ( ((si>=0) && (si<MAXCALROC)) ? roccalpar[si][0] : -1); }
  int getMeasindexROCCal(short si)  const 
  { return ( ((si>=0) && (si<MAXCALROC)) ? roccalpar[si][1] : -1); }
  int getTGL1TP1ROCCal(short si)    const 
  { return ( ((si>=0) && (si<MAXCALROC)) ? roccalpar[si][2] : -1); }
  int getTGL1TP2ROCCal(short si)    const 
  { return ( ((si>=0) && (si<MAXCALROC)) ? roccalpar[si][3] : -1); }
  int getTGL1TP3ROCCal(short si)    const 
  { return ( ((si>=0) && (si<MAXCALROC)) ? roccalpar[si][4] : -1); }
  int getTGL2TP1ROCCal(short si)    const 
  { return ( ((si>=0) && (si<MAXCALROC)) ? roccalpar[si][5] : -1); }
  int getTGL2TP2ROCCal(short si)    const 
  { return ( ((si>=0) && (si<MAXCALROC)) ? roccalpar[si][6] : -1); }
  int getTGL2TP3ROCCal(short si)    const 
  { return ( ((si>=0) && (si<MAXCALROC)) ? roccalpar[si][7] : -1); }
  int getTGL3TP1ROCCal(short si)    const 
  { return ( ((si>=0) && (si<MAXCALROC)) ? roccalpar[si][8] : -1); }
  int getTGL3TP2ROCCal(short si)    const 
  { return ( ((si>=0) && (si<MAXCALROC)) ? roccalpar[si][9] : -1); }
  int getTGL3TP3ROCCal(short si)    const 
  { return ( ((si>=0) && (si<MAXCALROC)) ? roccalpar[si][10] : -1); }

  // and ROC positions
  int getNumberROCPos() const
  { return numberROCPos; }
  int getMaxNumberROCPos() const
  { return MAXINSTALLROC; }

  // from PdbPadROCPos.hh
  int getPacketidROCPos(short si)  const 
  { return ( ((si>=0) && (si<MAXINSTALLROC)) ? rocpospar[si][0] : -1); }
  int getGrouprowROCPos(short si) const 
  { return ( ((si>=0) && (si<MAXINSTALLROC)) ? rocpospar[si][1] : -1); }
  int getGroupcolumnROCPos(short si) const 
  { return ( ((si>=0) && (si<MAXINSTALLROC)) ? rocpospar[si][2] : -1); }
  int getROCnumberROCPos(short si)   const 
  { return ( ((si>=0) && (si<MAXINSTALLROC)) ? rocpospar[si][3] : -1); }   

  // Print information about object
  void print();

private:
  
  // use a simple address object, to index one bad channel (single or from
  // a bad ROC) at a time. Don't want too many objects flying around..
  PadAddressObject *addressObj;
  
  // flags for whether we have the different parts in memory or not
  short HVFlag,ROCCalFlag,ROCPosFlag;

  // summary info
  int numberBadHVTot;
  int numberOkHVTot;
  int numberBadHVSect[MAXPCPLANES];
  int numberOkHVSect[MAXPCPLANES];

  PHBoolean PlaneInfoAvailable[MAXPCPLANES];
  int numberHVPlanes;

  // individual info
  bool HVOk[MAXPCPLANES][MAXHVSECT]; 
  
  // store all ROC calibrations
  int numberROCCal;
  short roccalpar[MAXCALROC][11];

  // store all ROC positions
  int numberROCPos;
  short rocpospar[MAXINSTALLROC][4];
  
}; 

#endif /* PADANCOBJY_H */ 


