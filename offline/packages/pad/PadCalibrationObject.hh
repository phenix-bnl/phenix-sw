#ifndef PADCALIBRATIONOBJECT_H
#define PADCALIBRATIONOBJECT_H

//--------------------------------------------------------------- 
//                                                                
// Created by: David Silvermyr
// Modified by Henrik Tydesjo and Rickard du Rietz
// Implementation of bad roc list changed to std::map by Henrik Tydesjo 2/6-2003
//
// Description: Header for PadCalibrationObject class
// One object holds all the info for the entire Pad Chamber subsystem
// (yes, we don't need that much calibration data)
//                                                              
//----------------------------------------------------------------

#include "PdbBankID.hh"
#include "PdbCalBank.hh"

#include <cstdlib>
#include <iostream>
#include <cstdio>
#include <map>

#include "PadBasicObject.hh"
#include "PadAddressObject.hh"

//using std::map;

const int MAXBADCH=1000;
//const int MAXBADROC=150;
// from year 2 and on.... if we want to read the ROCCal and ROCPos banks info
// in that case, need to add more routines also
// const int MAXCALROC=5000;
// const int MAXINSTALLROC=3600; // PC1 W+E, PC2 W, PC3 W+E

/**
   This is the PAD Calibration Object class. It is used to hold PAD 
   calibration data. Basic DB validity checks are inherited 
   from PadBasicObject <br>
   @memo PAD Calibration Class
*/
//
// The smallest and largest possible value of a packet ID
//
#define SMALLEST_PACKET_ID 4001
#define LARGEST_PACKET_ID  4096

class PadCalibrationObject : public PadBasicObject { 
  
public:
  
  // Constructor
  PadCalibrationObject(); 
  // Destructor
  virtual ~PadCalibrationObject(); 
  
  // member functions, fills data in our Calibration object
  // first routines to get all banks
  PHBoolean Fetch();
  // from default ASCII files
  PHBoolean FetchFromFile();
  // from ASCII file "filebadch" and "filebadroc"
  PHBoolean FetchFromFile(const char *filebadch, const char *filebadroc);
  
  // individual bank handlling routines
  // Fetch bad channel information from Objectivity Database
  PHBoolean FetchBadChObjy();
  // from default ASCII file
  PHBoolean FetchBadChFromFile();
  // from an ASCII file "filename"
  PHBoolean FetchBadChFromFile(const char *filename);
  
  // Fetch bad ROC information from Objectivity Database
  PHBoolean FetchBadROCObjy();
  // from default ASCII file
  PHBoolean FetchBadROCFromFile();
  // from an ASCII file "filename"
  PHBoolean FetchBadROCFromFile(const char *filename);
 
 
  //method which extracts the number of pad hits
  int GetNumberofPadHits(int packetid);

  // member functions, puts data from our Calibration object
  // into the database banks (Objy or ASCII)
  PHBoolean Put();
  // to default ASCII files
  PHBoolean PutToFile();
  // to ASCII file "filebadch" and "filebadroc"
  PHBoolean PutToFile(const char *filebadch, const char *filebadroc);
  
  // individual bank handlling routines
  // Put bad channel information into Objectivity Database
  PHBoolean PutBadChObjy();
  // to default ASCII file
  PHBoolean PutBadChToFile();
  // to an ASCII file "filename"
  PHBoolean PutBadChToFile(const char *filename);
  
  // Put bad ROC information into Objectivity Database
  PHBoolean PutBadROCObjy();
  // to default ASCII file
  PHBoolean PutBadROCToFile();
  // to an ASCII file "filename"
  PHBoolean PutBadROCToFile(const char *filename);
//*************************************************************
 
  // method to compare two bad roc lists
  int CompareBadRocList(PadCalibrationObject* that);
  // method to compare two bad roc lists
  int CompareHotRocList(PadCalibrationObject* that);
  // method to merge bad roc list with known (correct) roc list
  void MergeBadRocList(PadCalibrationObject* that);
  // method to add bad roc to the list of bad rocs
  int AddBadRoc(int packet, int row, int col, int type);
  int RemoveBadRoc(int packet, int row, int col);

  void ClearBadRocList();
  void PrintBadRocList(PadCalibrationObject* that);

  // routines to obtain info about the bad channels
  int getNumberBadCh();
  int getMaxNumberBadCh() { return MAXBADCH; }

  int getPacketidBadCh(int ibadch);
  int getChannelidBadCh(int ibadch);
  int getPadtypeBadCh(int ibadch);

  int getDetBadCh(int ibadch);
  int getArmBadCh(int ibadch);
  int getSideBadCh(int ibadch);
  int getSectorBadCh(int ibadch);
  int getPadzBadCh(int ibadch);
  int getPadxBadCh(int ibadch);

  // and more routines to obtain info about the bad ROCs
  int getNumberBadROC();
  //  int getMaxNumberBadROC() { return MAXBADROC; }

  int getPacketidBadROC(int ibadroc);
  int getGrowBadROC(int ibadroc);
  int getGcolBadROC(int ibadroc);
  int getROCtypeBadROC(int ibadroc);

  int getDetBadROC(int ibadroc);
  int getArmBadROC(int ibadroc);
  int getSideBadROC(int ibadroc);
  int getSectorBadROC(int ibadroc);
  int getMaxPadzBadROC(int ibadroc);
  int getMinPadzBadROC(int ibadroc);
  int getMaxPadxBadROC(int ibadroc);
  int getMinPadxBadROC(int ibadroc);

  // Print information about object
  void print();

private:
  
  // use a simple address object, to index one bad channel (single or from
  // a bad ROC) at a time. Don't want too many objects flying around..
  PadAddressObject *addressObj;
  PadCalibrationObject *PCOnew;
 
  // store bad channels, no need to store the good ones 
  int numberBadCh;
  int _pid_to_hits[LARGEST_PACKET_ID - SMALLEST_PACKET_ID + 1];
  void _get_hot_channels();
  typedef struct 
  {
    int packetid;
    int channelid;
    int padtype;
  } ch;
  ch badch[MAXBADCH]; 
  
  // store bad ROCs, no need to store the good ones  
  int numberBadROC;
  class Roc
  {
  public:
    Roc(){packetid=0;grow=0;gcol=0;roctype=0;}
    Roc(int p,int r,int c,int t){packetid=p;grow=r;gcol=c;roctype=t;}
    ~Roc(){}
    int packetid;
    int grow;
    int gcol;
    int roctype;
  };
  
  std::map<int,Roc*> *badroc;

}; 

#endif /* PADCALIBRATIONOBJECT_H */ 














