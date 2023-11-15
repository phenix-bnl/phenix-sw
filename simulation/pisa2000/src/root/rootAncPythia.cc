// $Id: rootAncPythia.cc,v 1.10 2007/10/18 11:41:04 hpereira Exp $

#include <iostream>
#include <cstdlib>
#include "rootAnc.h"
#include "PISAEventHeader.h"

using namespace std;

//________________________________________________________________
void rootAncPythia(Int_t ancflag, PISAEventHeader *EventHeader)
{

  static int icall = 0;
  static TFile *AncPythiaFile = 0;
  static TNtuple *AncPythiaNtuple = 0;

  if(ancflag == 1) 
  {
    //
    // Check that the file has been opened
    //
    if(icall == 0 || AncPythiaFile == 0){
      cerr << "\n rootAncPythia <E> bad call with ancflag = 1" << endl;
      exit(1);
    }  // safety check
    AncPythiaFile->Write();
    AncPythiaFile->Close();
    return;
  }  // check for close output file flag

  const int NTPL_PARAM = 29;
  float evt_ntpl[NTPL_PARAM];
  if(icall == 0)
  {

    // NTUPLE files
    AncPythiaFile = new TFile("ancpythia.root", "recreate", "PYTHIA PISA NTUPLE");
    AncPythiaNtuple = new TNtuple("AncPythia", "PYTHIA Event Parameters",
				  "PROC_ID:BJORK1:BJORK2:PARTSTU1:PARTSTU2:PARTSTU3:"//
				  "QSQUARE:PTRANS:"//
				  "PART1ID:PART1P1:PART1P2:PART1P3:PART1P4:"//
				  "PART2ID:PART2P1:PART2P2:PART2P3:PART2P4:"//
				  "PART3ID:PART3P1:PART3P2:PART3P3:PART3P4:"//
				  "PART4ID:PART4P1:PART4P2:PART4P3:PART4P4:"//
				  "EVENT");

  }  // initialization
  icall++;

  evt_ntpl[NTPL_PARAM - 1] = icall;

  static Int_t iWarn = 0;

  Int_t event_code = EventHeader->GetEventCode();
  if(event_code == 7 && iWarn == 0) {
    iWarn = 1;
    cout <<"\n\n  This PISA file contained Pythia events\n"  << endl;
  }

  if(event_code == 5 && iWarn == 0) {
    iWarn = 1;
    cout <<"\n\n  This PISA file contained HIJING events\n"  << endl;
    cout << "  Number of binary collisions in the first event = " << EventHeader->GetBinaryCollisions() << endl << endl;
  }

  if(iWarn != 2) 
  {
    iWarn = 2;

    int iGDate( EventHeader->GetIgdate() );
    if(iGDate > 20117) 
    {
      
      std::cout << "\n  PISA file creation date (YYYYMMDD) = 200" << iGDate << std::endl;
      int mapFile( EventHeader->GetMapFileFlag() );
      float mapFScale( EventHeader->GetMapFScale() );
      switch(mapFile) 
      {
        case 0:
        std::cout << "\n\n This PISA file used the February 1997 2D map with date " << iGDate;
        std::cout << "\n The scaling factor for the field was " << mapFScale << std::endl;
        break;
        
        case 1:
        std::cout << "\n\n This PISA file used the September 2001 2D map";
        std::cout << "\n The scaling factor for the field was " << mapFScale << std::endl;
        break;
        
        case 2:
        std::cout << "\n\n This PISA file used the November 2001 3D01 map";
        std::cout << "\n The scaling factor for the field was " << mapFScale << std::endl;
        break;
        
        case 3:
        std::cout << "\n\n This PISA file used the February 2003 3D03 map (same as 3D+0)";
        std::cout << "\n The scaling factor for the field was " << mapFScale << std::endl;
        break;
        
        case 8:
        std::cout << "\n\n This PISA file used the November 2003 3D++ map (both coils)";
        std::cout << "\n The scaling factor for the field was " << mapFScale << std::endl;
        break;

        case 9:
        std::cout << "\n\n This PISA file used the October 10, 2007 3D+- map (both coils)";
        std::cout << "\n The scaling factor for the field was " << mapFScale << std::endl;
        break;
        
        default:
        std::cout << "\n\n Unrecognized mapFile value " << mapFile;
        std::cout << "\n PISA file presumed to have been produced before January 19, 2002" << std::endl;
        break;
        
      }
      
    } else {
      cout << "\n\n PISA file was produced before January 18, 2001";
      cout << "\n There is no magnetic field map information encoded " << endl;
    }
    
    if(iGDate>20521) 
    {
      
      Int_t projectNumber = EventHeader->GetProjectNumber();
      Int_t versionNumber = EventHeader->GetVersionNumber();
      Int_t inputRunNumber = EventHeader->GetInputRunNumber();
      Int_t outputRunNumber = EventHeader->GetOutputRunNumber();

      cout << "\n Simulation Project Number = " << projectNumber;
      cout << ",  event input file ID number = " << inputRunNumber;
      cout << ",  PISA hits output file ID number = " << outputRunNumber;
      cout << ",  Version number = " << versionNumber << endl;
      
    } // check on date later than May 21, 2002
  }
  
  if(event_code != 7) return;
  
  evt_ntpl[0] = EventHeader->GetEventInt(0);
  Int_t kpart;
  for(kpart = 0; kpart<7; kpart++) { evt_ntpl[1 + kpart] = EventHeader->GetEventFloat(kpart); }

  //
  // Store intermediate particle information
  //
  for(kpart=0; kpart<4; kpart++){
    evt_ntpl[8 + kpart*5] = EventHeader->GetEventInt(1 + kpart);
    int ixyze;
    for(ixyze=0; ixyze<4; ixyze++){
      int index1 = 9 + ixyze + kpart*5;
      int index2 = 7 + ixyze + kpart*4;
      evt_ntpl[index1] = EventHeader->GetEventFloat(index2);
    }  // loop over 4 momentum
  }  // loop over 4 intermediate particles

  AncPythiaNtuple->Fill(evt_ntpl);

  return;
}
