//-------------------------------------------------------------------------
// Class: svxAddress (implementation)
// Created by: Sasha Lebedev <lebedev@iastate.edu>
//-------------------------------------------------------------------------
#include <iostream>
#include <fstream>
#include <cmath>

#include "svxAddress.hh"
#include "SvxPixel1v1.h"
#include "SvxStripixel.h"
#include "SvxStrip11v1.h"
#include "SvxPixStruct.h"

#include "SvxMap.h"

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbMapIntInt.hh>
#include <PdbCalBank.hh>

using namespace std;

int svxAddress::m_count=0;

svxAddress::svxAddress() { 
  Verbose=0;
  maptype=1; // maptype==0 is the old, hardcoded, way to fill pixel hardware/software map.
             // It is not used any more. I keep it only to check that new changes do not mess up anything.
  m_usedatabase=0;
  m_tFetchDB=NULL;
  m_initialized=false;  // initial value is -1;
  use_oldmap = false;
  /// there is no relation between use_oldmap and maptype.
  /// this is only for pixel and used before run 340200.

  m_count++;
  if(Verbose>0) {
    cout<<"Nth svxAddress : "<<m_count<<endl;
  }
  if(m_count>1) {
    cerr<<"You are using Nth="<<m_count<<" svxAddress."<<endl;
    cerr<<"Should use common svxAddress object."<<endl;
  }
}

svxAddress::~svxAddress() { 
  // don't forget to clear maps!!!
  //cout<<"svxAddress::~svxAddress"<<endl;
  
  if(Verbose>0) {
    cout<<"svxAddresss::~svxAddress : Nth="<<m_count<<" is deleted."<<endl;
  }
  m_count--;
}

void svxAddress::Initialize() {

  if(m_usedatabase) {
    if(m_tFetchDB!=NULL) {
      fetchPixelMap(m_tFetchDB);
      fetchStripixelMap(m_tFetchDB);
    }
    else {
      cout<<PHWHERE<<"No Fetch Time is set. Initialization is failed."<<endl;
      return;
    }
  }
  else {
    UseDefaultsPixels();
    UseDefaultsStripixels();
  }

  if(Verbose>0) cout<<__FUNCTION__<<" end"<<endl;
  m_initialized=true;
  
  return;
}

//--------------------------------------------------------------------------

PHBoolean svxAddress::Update(PHTimeStamp *Tbeg, PHTimeStamp *Tend) {

  PHBoolean success = True;
  PHTimeStamp Tstart = *Tbeg;
  PHTimeStamp Tstop  = *Tend;
  PdbCalBank *svxBank = 0;
  PdbBankID bankID; bankID.setInternalValue(0);
  const char *description = "Hardware/Software map for VTX pixel layers.";
  const char *tableName = "svx.address.pixel";
  //int banklength = SVXLADDERSLAYER0*2*SVXSENSORSLAYER0*SVXSENSECPIX + SVXLADDERSLAYER1*2*SVXSENSORSLAYER1*SVXSENSECPIX;
  int banklength = SVXLADDERSLAYER0*2*SVXSENSORSLAYER0 + SVXLADDERSLAYER1*2*SVXSENSORSLAYER1;
  int tmpladders[2]; tmpladders[0]=SVXLADDERSLAYER0*2; tmpladders[1]=SVXLADDERSLAYER1*2;
  int tmpsensors[2]; tmpsensors[0]=SVXSENSORSLAYER0; tmpsensors[1]=SVXSENSORSLAYER1;
  if(Verbose) {
    std::cout << "  tableName:    " << tableName << std::endl;
    std::cout << "  description:  " << description << std::endl;
    std::cout << "  bankID =      " << bankID.getInternalValue() << std::endl;
    std::cout << "  Time stamp:   " << Tstart << " - " << Tstop << std::endl;
    std::cout << "  Bank length = " << banklength << std::endl;
  }
  std::map<int,int> tmpmap;
  PdbMapIntInt* address = 0;  

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
                                                              
  if(application->startUpdate()) {
    
    svxBank = bankManager->createBank("PdbMapIntIntBank", bankID, description, Tstart, Tstop, tableName);

    if(svxBank) {

      svxBank->setLength(banklength);

      int iindex=0;
      for(int ilr=0; ilr<2; ilr++) {
        for(int ild=0; ild<tmpladders[ilr]; ild++) {
          for(int isn=0; isn<tmpsensors[ilr]; isn++) {
	    tmpmap.clear();
	    tmpmap = _svxaddress_sensor1[ilr][ild][isn];
	    if(Verbose) {
	      std::cout << "=== Layer,ladder.sensor: " 
			<< ilr << " " << ild << " " << isn << std::endl;
	      
	      std::cout << "  Map size: " << tmpmap.size() << std::endl;
	    }
	    
	    address = (PdbMapIntInt*) & (svxBank->getEntry(iindex));
	    address->set_map(&tmpmap);
	    iindex++;
          }  // sensor
        } // ladder
      } // layer
      
    }
    else {
      std::cerr << PHWHERE << " ERROR: can not create svxBank." 
		<< std::endl;
      success = False;
    }
    
  }
  else {
    std::cerr << PHWHERE << "ERROR: Database not writable, aborting." 
	      << std::endl;
    success = False;
  }
  
  cout << "success = " << success << endl;
  if(success) {
    cout << "committing..." << endl; 
    application->commit(); 
    cout << "commit done." << endl;
  } 
  else {
    application->abort();
  }
  
  cout << "deleting bank..." << endl;
 
  if(svxBank) 
    delete svxBank;
 
  cout << "returning..." << endl;
  
  return success; 
}

//------------------------------------------------------------------------------------------------

PHBoolean svxAddress::Fetch(PHTimeStamp *T) {

  PHBoolean success = True;
  PHTimeStamp TSearch = *T;
  PdbMapIntInt* address = 0;
  PdbBankID bankID; bankID.setInternalValue(0);
  PdbCalBank *svxBank = 0;
  const char *tableName = "svx.address.pixel";
  int tmpladders[2]; tmpladders[0]=SVXLADDERSLAYER0*2; tmpladders[1]=SVXLADDERSLAYER1*2;
  int tmpsensors[2]; tmpsensors[0]=SVXSENSORSLAYER0; tmpsensors[1]=SVXSENSORSLAYER1;
  if(Verbose) {
    std::cout << "Reading tableName:    " << tableName << std::endl;
    std::cout << "  Time stamp:   " << Tsearch << std::endl;
  }

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if(!application->startRead()) {
    application->abort();
    cerr << PHWHERE << " ERROR: Transaction aborted. Database NOT available." << endl;
    return False;
  }

  svxBank = bankManager->fetchBank("PdbMapIntIntBank", bankID, tableName, Tsearch);

  if(svxBank) {

    int banklength = (int)svxBank->getLength();
    //if(Verbose) {cout << "  Bank length = " << banklength << " = " << SVXLADDERSLAYER0*2*SVXSENSORSLAYER0*SVXSENSECPIX + SVXLADDERSLAYER1*2*SVXSENSORSLAYER1*SVXSENSECPIX << endl;}
    if(Verbose) {cout << "  Bank length = " << banklength << " = " << SVXLADDERSLAYER0*2*SVXSENSORSLAYER0 + SVXLADDERSLAYER1*2*SVXSENSORSLAYER1 << endl;}
    //if(banklength != SVXLADDERSLAYER0*2*SVXSENSORSLAYER0*SVXSENSECPIX + SVXLADDERSLAYER1*2*SVXSENSORSLAYER1*SVXSENSECPIX) {}
    if(banklength != SVXLADDERSLAYER0*2*SVXSENSORSLAYER0 + SVXLADDERSLAYER1*2*SVXSENSORSLAYER1) {
      cerr << PHWHERE << " ERROR: wrong bank length: " << banklength << endl;
      success = False;
    }

    if(success) {

      int iindex=0;
      for(int ilr=0; ilr<2; ilr++) {
        for(int ild=0; ild<tmpladders[ilr]; ild++) {
          for(int isn=0; isn<tmpsensors[ilr]; isn++) {
//            for(int isec=0; isec<SVXSENSECPIX; isec++) {

              address = (PdbMapIntInt*) & (svxBank->getEntry(iindex));
              address->print();
              //_svxaddress_sensec1[ilr][ild][isn][isec] = address->get_map();
              _svxaddress_sensor1[ilr][ild][isn] = address->get_map();
              cout << "Layer,ladder,sensor: " << ilr << " " << ild << " " << isn << " " << iindex << endl;
              //cout << "  Map size = " << (_svxaddress_sensec1[ilr][ild][isn][isec]).size() << endl;
              cout << "  Map size = " << (_svxaddress_sensor1[ilr][ild][isn]).size() << endl;
              iindex++;

//            } // section
          }  // sensor
        } // ladder
      } // layer

    }
  }
  else {
    cerr << PHWHERE << " ERROR: bankManager returned zero-pointer." << endl;
    success = False;
  }

  if(success) {application->commit();} else {application->abort();}

  if(svxBank) delete svxBank;

  return success;
}

PHBoolean svxAddress::fetchPixelMap(PHTimeStamp *time) { // Added by T.Hachiya 2011.04.29
  int *array;
  int size=0;

  if(use_oldmap) {
   cout<<PHWHERE<<" Fetch packetID from DB with "<<time->formatTimeString()<<endl;
   cout<<PHWHERE<<" use_oldmap is neglected."<<endl;
  }

  SvxMap map;
  if(!map.fetchPixelPacketMap(*time, &size, &array)) {
    cout<<PHWHERE<<" Failed to fetch PixelMap from DB"<<endl;
    return False;
  }

  // fill m_aryPixelPacketID
  m_aryPixelPacketID.clear();
  for(int i=0; i<size; i++) {
    m_aryPixelPacketID.push_back(array[i]);
  }
  
  // fill m_aryPixelModule
  m_aryPixelModule.clear();
  m_aryPixelModule.reserve(60);
  for(int i=0; i<size; i++) {
    //cout<<i<<" "<<array[i]<<" "<<array[i]-24001<<endl;
    m_aryPixelModule[array[i]-24001] = i;
  }

  delete [] array;

  cout<<"fetchPixelMap done. PixelPacketID and PixelModule are initialized with DB"<<endl;

  return True;
}

PHBoolean svxAddress::fetchStripixelMap(PHTimeStamp *time) { // Added by T.Hachiya 2011.04.29
  UseDefaultsStripixels(); // now temporally use this function. 
                           // should be replaced by DB access
  return True;
}

PHBoolean svxAddress::commitPixelMap(const char *description, PHTimeStamp* timeBegin, PHTimeStamp* timeEnd)
{ 
  // Added by T.Hachiya 2011.04.29
  if (!isInitialized()) {
    cout<<PHWHERE<<" Not initialized. No data can be commited."<<endl;
    return False;
  }
  
  if (m_aryPixelPacketID.size()!=60) { // further check
    cout<<PHWHERE<<" aryPixelPacketID is not correctly initialized "<<endl; //size should be 60
    return False;
  }

  int packetid[60];
  for(int i=0; i<60; i++) {
    packetid[i] = m_aryPixelPacketID[i];
  }

  SvxMap map;
  if(!map.commitPixelPacketMap(description, *timeBegin, *timeEnd, m_aryPixelPacketID.size(), packetid)) {
    cout<<"DB commiting is failed"<<endl;
    return False;
  }

  cout<<"Commit succeeded"<<endl;

  return True;
}

//-----------------------------------------------------------------------------------------

PHBoolean svxAddress::FetchFromFile() {
  std::string filename = "svxAddressMap.txt";
  return FetchFromFile(filename);
}

//-----------------------------------------------------------------------------------------

PHBoolean svxAddress::FetchFromFile(std::string filename) {

  ifstream fin(filename.c_str());
  if(!fin) {
    cout << "svxAddress::FetchFromFile() ERROR: Cannot open input file: " << filename << endl; 
    return False;
  }
  if(Verbose>0) {
    std::cout << "svxAddress::FetchFromFile(): " << filename << " input file opened." << std::endl;
  }
  
  int tmpladders[2]; tmpladders[0]=SVXLADDERSLAYER0*2; tmpladders[1]=SVXLADDERSLAYER1*2;
  int tmpsensors[2]; tmpsensors[0]=SVXSENSORSLAYER0; tmpsensors[1]=SVXSENSORSLAYER1;

  int iindex=0;
  int icount=0;
  int ix=0; int iz=0; int superchannel=0;
  int ilr2=0; int ild2=0; int isn2=0; 
  for(int ilr=0; ilr<2; ilr++) {
    for(int ild=0; ild<tmpladders[ilr]; ild++) {
      for(int isn=0; isn<tmpsensors[ilr]; isn++) {
        for(int itmp=0; itmp<128*256; itmp++) { // loop over pixels in a sensor

          //std::map<int,int>::iterator it = _svxaddress_sensec1[ilr][ild][isn][isec].begin();
//          std::map<int,int>::iterator it = _svxaddress_sensor1[ilr][ild][isn].begin();
          //while ( it != _svxaddress_sensec1[ilr][ild][isn][isec].end() ) {
//          while ( it != _svxaddress_sensor1[ilr][ild][isn].end() ) {
            fin >> ilr2 >> ild2 >> isn2 >> ix >> iz >> superchannel;
            //_svxaddress_sensec1[ilr2][ild2][isn2][isec2][channel] = ix+iz*1000;
            _svxaddress_sensor1[ilr2][ild2][isn2][superchannel] = ix+iz*1000;
//            it++;
            icount++;
//          }

          iindex++;
        } // pixels
      }  // sensor
    } // ladder
  } // layer

  fin.close();

  if(Verbose>0) {
    std::cout << "svxAddress::FetchFromFile(): Read " << icount << " records." << std::endl;
  }

  return True;
}

//-----------------------------------------------------------------------------------------

PHBoolean svxAddress::DumpIntoFile() {
  std::string filename = "svxAddressMap.txt";
  return DumpIntoFile(filename);
}

//-----------------------------------------------------------------------------------------

PHBoolean svxAddress::DumpIntoFile(std::string filename) {
  
  ofstream fout(filename.c_str());
  if(!fout) {cout << "svxAddress::DumpIntoFile() ERROR: Cannot open output file: " << filename << endl; return False;}
  if(Verbose>0) {std::cout << "svxAddress::DumpIntoFile(): " << filename << " output file opened." << std::endl;}

  int tmpladders[2]; tmpladders[0]=SVXLADDERSLAYER0*2; tmpladders[1]=SVXLADDERSLAYER1*2;
  int tmpsensors[2]; tmpsensors[0]=SVXSENSORSLAYER0; tmpsensors[1]=SVXSENSORSLAYER1;

  int iindex=0;
  int icount=0;
  for(int ilr=0; ilr<2; ilr++) {
    for(int ild=0; ild<tmpladders[ilr]; ild++) {
      for(int isn=0; isn<tmpsensors[ilr]; isn++) {
//        for(int isec=0; isec<SVXSENSECPIX; isec++) {

          int ich=0;
          //std::map<int,int>::iterator it = _svxaddress_sensec1[ilr][ild][isn][isec].begin();
          std::map<int,int>::iterator it = _svxaddress_sensor1[ilr][ild][isn].begin();
          //while ( it != _svxaddress_sensec1[ilr][ild][isn][isec].end() ) {
          while ( it != _svxaddress_sensor1[ilr][ild][isn].end() ) {
            int ix = (it->second)%1000;
            int iz = (it->second)/1000;
            //int channel = it->first; 
            int superchannel = it->first; 
            fout << ilr << " " << ild << " " << isn << " " << ix << " " << iz << " " << superchannel <<" ";
            fout << it->first << " "<<ich<<std::endl;
            it++;
            icount++;
            ich++;
          }

          iindex++;
//        } // section
      }  // sensor
    } // ladder
  } // layer

  fout.close();
  if(Verbose>0) {std::cout << "svxAddress::DumpIntoFile(): Dumped " << iindex << " sensors." << std::endl;}
  if(Verbose>0) {std::cout << "svxAddress::DumpIntoFile(): Dumped " << icount << " records." << std::endl;}
  return True;
}

//-----------------------------------------------------------------------------------------

void svxAddress::UsePixelHardware() {

// channels are counted from 0 to 256*32 within a roc

  int ilr=0;  // first pixel layer 
  for(int ild=0; ild<10; ild++) {
    for(int isn=0; isn<4; isn++) {
      for(int iz=0; iz<128; iz++) {
	for(int ix=0; ix<256; ix++) {
	  
	  // roc numbering is assumed left-to-right if looking from outside of the detector
	  // that is, south-to-north in east arm, and north-to-south in west arm
	  // channel numbering within roc is assumed to be opposire: right-to-left if looking from outside
	  // that is, south-to-north in west arm, and north-to-south in east arm
	  // channel row numbering assumed bottom-to-top in both arms
	  //
	  // iz index goes south-to-north in both arms (range: 0-128 in each sensor)
	  // ix index goes top-to-bottom in west arm, and bottom-to-top in east arm (range: 0-255)
          
	  if(ild>9) continue;
	  
	  //  int imodule = ild;          // packet or half-ladder
	  //--This is obsolete    if(isn<2) {imodule += 5;}  // TH 20110225
	  int imodule = 0;          // South:5-9(sens3:2), 15-19(sens0:1), North:0-4(sens1:0), 10-14(sens(2-3)
	  if(ild<5) { // west
	    if(isn<2)  {imodule = ild;}      // west-north(0-4)
	    else       {imodule = ild + 5;}  // west-south(5-9)
	  }
	  else {      // east
	    if(isn<2)  {imodule = ild + 10;} // east-south(15-19)
	    else       {imodule = ild + 5 ;} // east-north(10-14)
	  }
	  int iroc = (3-iz/32) + (isn%2)*4;  // east arm
	  //--This is obsolete    if(ild<=4) iroc = 7-iroc;    // west arm // TH20110225
	  int tmpiz = iz%32;             // iz within roc
	  int ichan = -1;
	  if(ild>4) { // east arm
	    //ichan = 32*256 - ix*32 - tmpiz - 1; // old pisa
	    ichan = ix*32 + tmpiz; // new local x and z
	  } else {    // west arm
	    ichan = ix*32 + tmpiz;
	  }
	  int superchan =  ichan + iroc*10000 + imodule*100000;
	  int ixz = ix+iz*1000;
	  //              if(ild==0 && isn==1 && ix==47 && iz==112) {std::cout << "LEBEDEVLEBEDEV: " << ilr << " " << ild << " " << isn << " " << ix << " " << iz << " " << superchan << " " << ixz << std::endl;}
	  //              if(ild==1 && isn==1 && ix==178 && iz==113) {std::cout << "LEBEDEVLEBEDEV: " << ilr << " " << ild << " " << isn << " " << ix << " " << iz << " " << superchan << " " << ixz << std::endl;}
	  _svxaddress_sensor1[ilr][ild][isn][superchan] = ixz;
	  
	} // ix
      } // iz
    }  // sensor
  } // ladder
  
  ilr=1;  // second pixel layer 
  for(int ild=0; ild<20; ild++) {
    for(int isn=0; isn<4; isn++) {
      for(int iz=0; iz<128; iz++) {
	for(int ix=0; ix<256; ix++) {
	  
	  //--int imodule = ild;          // packet or half-ladder
	  //-- This is obsolete  if(isn<2) {imodule += 10;}  // TH 20110225
	  //--  if(isn>=2) {imodule += 10;} 
	  //--  if(ild>9) {imodule += 10;} // east arm
	  
	  int imodule = 0;          // South:5-9(sens3:2), 15-19(sens0:1), North:0-4(sens1:0), 10-14(sens(2-3)
	  if(ild<10) { // west
	    if(isn<2)  {imodule = ild;}      // west-north(0-9) + 20
	    else       {imodule = ild + 10;} // west-south(10-19) + 20
	  }
	  else {      // east
	    if(isn<2)  {imodule = ild + 20;} // east-south(30-39) + 20
	    else       {imodule = ild + 10;} // east-north(20-29) + 20
	  }
	  imodule += 20;          // first 20 modules in layer 0
	  
	  int iroc = (3-iz/32) + (isn%2)*4;  // east arm
	  //--This is obsolete  if(ild<=9) iroc = 7-iroc;    // west arm TH20110225
	  int tmpiz = iz%32;             // iz within roc
	  int ichan = -1;
	  if(ild>9) { // east arm
	    //ichan = 32*256 - ix*32 - tmpiz - 1; // old pisa
	    ichan = ix*32 + tmpiz; // new local x and z
	  } else {    // west arm
	    ichan = ix*32 + tmpiz;
	  }
	  int superchan =  ichan + iroc*10000 + imodule*100000;
	  int ixz = ix+iz*1000;
	  _svxaddress_sensor1[ilr][ild][isn][superchan] = ixz;
	  
	} // ix
      } // iz
    }  // sensor
  } // ladder
  
  std::cout << "UsePixelhardware done." << std::endl;
  return;

}

//-----------------------------------------------------------------------------------------

void svxAddress::UseDefaults() {
  UseDefaultsPixels();
  UseDefaultsStripixels();
  return;
}

//-----------------------------------------------------------------------------------------

void svxAddress::UseDefaultsPixels() {
  if(Verbose>0) cout<<__FUNCTION__<<" "<<SVXNMODULEPIXEL<<endl;

  /// conversion map from module to packet ID
  // module ID                      -> packet ID
  //  0 -  4 (Barrel 0, WEST-NORTH) -> 24031 - 24035
  //  5 -  9 (Barrel 0, WEST-SOUTH) -> 24046 - 24050
  // 10 - 14 (Barrel 0, EAST-NORTH) -> 24001 - 24005
  // 15 - 19 (Barrel 0, EAST-SOUTH) -> 24016 - 24020
  // 20 - 29 (Barrel 1, WEST-NORTH) -> 24036 - 24045
  // 30 - 39 (Barrel 1, WEST-SOUTH) -> 24051 - 24060
  // 40 - 49 (Barrel 1, EAST-NORTH) -> 24006 - 24015
  // 50 - 59 (Barrel 1, EAST-SOUTH) -> 24021 - 24030

  int packetID[SVXNMODULEPIXEL];
  for ( int imodule=0; imodule<SVXNMODULEPIXEL; imodule++ )
    {
      if      ( imodule>= 0 && imodule< 5 ) { packetID[imodule] = imodule + 24031; }
      else if ( imodule>= 5 && imodule<10 ) { packetID[imodule] = imodule + 24041; }
      else if ( imodule>=10 && imodule<15 ) { packetID[imodule] = imodule + 23991; }
      else if ( imodule>=15 && imodule<20 ) { packetID[imodule] = imodule + 24001; }
      else if ( imodule>=20 && imodule<30 ) { packetID[imodule] = imodule + 24016; }
      else if ( imodule>=30 && imodule<40 ) { packetID[imodule] = imodule + 24021; }
      else if ( imodule>=40 && imodule<50 ) { packetID[imodule] = imodule + 23966; }
      else if ( imodule>=50 && imodule<60 ) { packetID[imodule] = imodule + 23971; }
      else                                  { packetID[imodule] = -1             ; }
    }


  /// conversion map from module to packet ID
  // packet ID     -> module ID
  // 24001 - 24005 -> 10 - 14 (Barrel 0, EAST-NORTH)
  // 24006 - 24015 -> 40 - 49 (Barrel 1, EAST-NORTH)
  // 24016 - 24020 -> 15 - 19 (Barrel 0, EAST-SOUTH)
  // 24021 - 24030 -> 50 - 59 (Barrel 1, EAST-SOUTH)
  // 24031 - 24035 ->  0 -  4 (Barrel 0, WEST-NORTH)
  // 24036 - 24045 -> 20 - 29 (Barrel 1, WEST-NORTH)
  // 24046 - 24050 ->  5 -  9 (Barrel 0, WEST-SOUTH)
  // 24051 - 24060 -> 30 - 39 (Barrel 1, WEST-SOUTH)

  int module[SVXNMODULEPIXEL];
  for ( int ipacket=24001; ipacket<=24060; ipacket++ )
    {
      if      ( ipacket>24000 && ipacket<=24005 ) { module[ipacket-24001] = ipacket - 23991; }
      else if ( ipacket>24005 && ipacket<=24015 ) { module[ipacket-24001] = ipacket - 23966; }
      else if ( ipacket>24015 && ipacket<=24020 ) { module[ipacket-24001] = ipacket - 24001; }
      else if ( ipacket>24020 && ipacket<=24030 ) { module[ipacket-24001] = ipacket - 23971; }
      else if ( ipacket>24030 && ipacket<=24035 ) { module[ipacket-24001] = ipacket - 24031; }
      else if ( ipacket>24035 && ipacket<=24045 ) { module[ipacket-24001] = ipacket - 24016; }
      else if ( ipacket>24045 && ipacket<=24050 ) { module[ipacket-24001] = ipacket - 24041; }
      else if ( ipacket>24050 && ipacket<=24060 ) { module[ipacket-24001] = ipacket - 24021; }
      else                                        { module[ipacket-24001] = -1             ; }
    }

  if ( use_oldmap ) {
    /// old map (before run340200)
    /// At the old map, B0-L2-S and B0-L4-S are swapped.
    /// This means
    /// module 7 <-> packet 24050
    /// module 9 <-> packet 24048

    cout<<"svxAddress::Initialize : OLDMAP is used"<<endl;
    packetID[7] = 24050;
    packetID[9] = 24048;
    module[47]  = 9;
    module[49]  = 7;
  }

  m_aryPixelPacketID.clear();
  m_aryPixelModule.clear();
////  m_aryPixelPacketID_old.clear();
////  m_aryPixelModule_old.clear();

  for(int i=0; i<SVXNMODULEPIXEL; i++) {
    if(Verbose>100) cout<<"moduleid :"<<i<<flush;
    if(Verbose>100) cout<<", packetid:"<<packetID[i]<<endl;
    if(Verbose>100) cout<<"packetid :"<<i+24001<<flush;
    if(Verbose>100) cout<<", moduleid:"<<module[i]<<endl;
    
    /// new map (after run340200)
    m_aryPixelPacketID.push_back(packetID[i]);
    m_aryPixelModule.push_back(module[i]);

/*
    /// old map (before run340200)
    /// At the old map, B0-L2-S and B0-L4-S are swapped.
    /// This means
    /// module 7 <-> packet 24050
    /// module 9 <-> packet 24048
    if      ( i==47 ) { m_aryPixelModule_old.push_back(9);         }
    else if ( i==49 ) { m_aryPixelModule_old.push_back(7);         }
    else 	      { m_aryPixelModule_old.push_back(module[i]); }
    if      ( i== 7 ) { m_aryPixelPacketID_old.push_back(24050);       }
    else if ( i== 9 ) { m_aryPixelPacketID_old.push_back(24048);       }
    else              { m_aryPixelPacketID_old.push_back(packetID[i]); }
*/
  }

  std::cout << "UseDefaultPixels done. Only PixelPacketID" << std::endl;
}

//-----------------------------------------------------------------------------------------
void svxAddress::UseDefaultsStripixels() {

  // Stripixel layers
  // sensor section based numbering

  if(Verbose>0) cout<<__FUNCTION__<<" "<<SVXNMODULESTRIP<<endl;

  int packet_strip[40];
  packet_strip[0]  = 24101;   /// B2-L0
  packet_strip[1]  = 24102;   /// B2-L1
  packet_strip[2]  = 24103;   /// B2-L2
  packet_strip[3]  = 24104;   /// B2-L3
  packet_strip[4]  = 24108;   /// B2-L4
  packet_strip[5]  = 24105;   /// B2-L5
  packet_strip[6]  = 24106;   /// B2-L6
  packet_strip[7]  = 24107;   /// B2-L7
  packet_strip[8]  = 24117;   /// B2-L8
  packet_strip[9]  = 24118;   /// B2-L9
  packet_strip[10] = 24119;   /// B2-L10
  packet_strip[11] = 24120;   /// B2-L11
  packet_strip[12] = 24121;   /// B2-L12
  packet_strip[13] = 24122;   /// B2-L13
  packet_strip[14] = 24123;   /// B2-L14
  packet_strip[15] = 24124;   /// B2-L15
  packet_strip[16] = 24109;   /// B3-L0
  packet_strip[17] = 24110;   /// B3-L1
  packet_strip[18] = 24111;   /// B3-L2
  packet_strip[19] = 24112;   /// B3-L3
  packet_strip[20] = 24113;   /// B3-L4
  packet_strip[21] = 24114;   /// B3-L5
  packet_strip[22] = 24115;   /// B3-L6
  packet_strip[23] = 24116;   /// B3-L7
  packet_strip[24] = 24125;   /// B3-L8
  packet_strip[25] = 24126;   /// B3-L9
  packet_strip[26] = 24127;   /// B3-L10
  packet_strip[27] = 24128;   /// B3-L11
  packet_strip[28] = 24129;   /// B3-L12
  packet_strip[29] = 24132;   /// B3-L13
  packet_strip[30] = 24130;   /// B3-L14
  packet_strip[31] = 24131;   /// B3-L15
  packet_strip[32] = 24133;   /// B3-L16
  packet_strip[33] = 24134;   /// B3-L17
  packet_strip[34] = 24135;   /// B3-L18
  packet_strip[35] = 24136;   /// B3-L19
  packet_strip[36] = 24137;   /// B3-L20
  packet_strip[37] = 24138;   /// B3-L21
  packet_strip[38] = 24139;   /// B3-L22
  packet_strip[39] = 24140;   /// B3-L23


  int module_strip[40];
  module_strip[0]  = 0;
  module_strip[1]  = 1;
  module_strip[2]  = 2;
  module_strip[3]  = 3;
  module_strip[4]  = 5;
  module_strip[5]  = 6;
  module_strip[6]  = 7;
  module_strip[7]  = 4;
  module_strip[8]  = 16;
  module_strip[9]  = 17;
  module_strip[10] = 18;
  module_strip[11] = 19;
  module_strip[12] = 20;
  module_strip[13] = 21;
  module_strip[14] = 22;
  module_strip[15] = 23;
  module_strip[16] = 8;
  module_strip[17] = 9;
  module_strip[18] = 10;
  module_strip[19] = 11;
  module_strip[20] = 12;
  module_strip[21] = 13;
  module_strip[22] = 14;
  module_strip[23] = 15;
  module_strip[24] = 24;
  module_strip[25] = 25;
  module_strip[26] = 26;
  module_strip[27] = 27;
  module_strip[28] = 28;
  module_strip[29] = 30;
  module_strip[30] = 31;
  module_strip[31] = 29;
  module_strip[32] = 32;
  module_strip[33] = 33;
  module_strip[34] = 34;
  module_strip[35] = 35;
  module_strip[36] = 36;
  module_strip[37] = 37;
  module_strip[38] = 38;
  module_strip[39] = 39;

  m_aryStripPacketID.clear();
  m_aryStripModule.clear();

  for(int i=0; i<SVXNMODULESTRIP; i++) {
    m_aryStripPacketID.push_back(packet_strip[i]);
    m_aryStripModule.push_back(module_strip[i]);
  }

  std::cout << "UseDefaultStripixels done. Both map and PacketID" << std::endl;
 
 return;

}

//-----------------------------------------------------------------------------------------
// returns X position for given channel (readout=0, pixels only)
int svxAddress::getIX0(int sectype, int channel) {
  
  int IX = -1;
  int ilayer=0;
  int iladder=0;
  int isensor=0;
  int isection=0;

// pixel section types: 2,3,1,3,1,3,2
// stripixel section types: 10,10
  if(sectype==1) {isection=2;}
  else if(sectype==2) {isection=0;}
  else if(sectype==3) {isection=1;}
  else {std::cerr << "svxAddress::getIX0() ERROR: wrong section type: " << sectype << std::endl; return -1;}

    std::map<int,int>::iterator it = _svxaddress_sensec1[ilayer][iladder][isensor][isection].begin();
    while ( it != _svxaddress_sensec1[ilayer][iladder][isensor][isection].end() ) {
      if( (it->first) == channel ) { IX = (it->second)%1000;  break; }
      it++;
    }

  return IX;
}

//-----------------------------------------------------------------------------------------
// returns X position IN THE SENSOR for given channel (readout=0, pixels only)
int svxAddress::getSensorIX0(int lr, int ld, int sn, int channel, int iroc, int imodule) {
  
  int IX = -1;
  int ilayer=lr;
  //int iladder=ld;
  //int isensor=sn;
  //int superchannel = channel + 10000*iroc + 100000*imodule;

  /*  
  std::map<int,int>::iterator it = _svxaddress_sensor1[ilayer][iladder][isensor].begin();
  while ( it != _svxaddress_sensor1[ilayer][iladder][isensor].end() ) {
    if( (it->first) == superchannel ) { IX = (it->second)%1000;  break; }
    it++;
  }
  */

  if ( ilayer==0 || ilayer==1 ) {
    IX = channel/32;
  } else {
    IX = -1;
  }
  
  return IX;
}

//-----------------------------------------------------------------------------------------
// returns Z position IN THE SENSOR for given channel (readout=0, pixels only)
int svxAddress::getSensorIZ0(int lr, int ld, int sn, int channel, int iroc, int imodule) {
  
  int IZ = -1;
  int ilayer=lr;
  //int iladder=ld;
  //int isensor=sn;
  //int superchannel = channel + 10000*iroc + 100000*imodule;

  /*
  std::map<int,int>::iterator it = _svxaddress_sensor1[ilayer][iladder][isensor].begin();
  while ( it != _svxaddress_sensor1[ilayer][iladder][isensor].end() ) {
    if( (it->first) == superchannel ) { IZ = (it->second)/1000;  break; }
    it++;
  }
  */

  if ( ilayer==0 || ilayer==1 ) {
    IZ = channel%32 + (3-iroc%4)*32;
  } else {
    IZ = -1;
  }

  return IZ;
}

//-----------------------------------------------------------------------------------------
// returns X position IN THE SENSOR for given channel (readout=0, pixels only)
int svxAddress::getSensorIX0(int channel) {
  
  int IX = -1;
  int ilayer=0;
  int iladder=0;
  int isensor=0;
    
  std::map<int,int>::iterator it = _svxaddress_sensor1[ilayer][iladder][isensor].begin();
  while ( it != _svxaddress_sensor1[ilayer][iladder][isensor].end() ) {
    if( (it->first) == channel ) { IX = (it->second)%1000;  break; }
    it++;
  }

  return IX;
}   

//-----------------------------------------------------------------------------------------
// returns Z position IN THE SENSOR for given channel (readout=0, pixels only)
int svxAddress::getSensorIZ0(int channel) {
  
  int IZ = -1;
  int ilayer=0;
  int iladder=0;
  int isensor=0;
  
  std::map<int,int>::iterator it = _svxaddress_sensor1[ilayer][iladder][isensor].begin();
  while ( it != _svxaddress_sensor1[ilayer][iladder][isensor].end() ) {
    if( (it->first) == channel ) { IZ = (it->second)/1000;  break; }
    it++;
  }
  
  return IZ;
} 

//-----------------------------------------------------------------------------------------
// returns Z position for given channel (readout=0, pixels only)
int svxAddress::getIZ0(int sectype, int channel) {
  
  int IZ = -1;
  int ilayer=0;
  int iladder=0;
  int isensor=0;
  int isection=0;

  // pixel section types: 2,3,1,3,1,3,2
  // stripixel section types: 10,10
  if(sectype==1) {isection=2;}
  else if(sectype==2) {isection=0;}
  else if(sectype==3) {isection=1;}
  else {std::cerr << "svxAddress::getIZ0() ERROR: wrong section type: " << sectype << std::endl; return -1;}
  
  std::map<int,int>::iterator it = _svxaddress_sensec1[ilayer][iladder][isensor][isection].begin();
  while ( it != _svxaddress_sensec1[ilayer][iladder][isensor][isection].end() ) {
    if( (it->first) == channel ) { IZ = (it->second)/1000;  break; }
    it++;
  }
  
  return IZ;
}

//-----------------------------------------------------------------------------------------
// returns ROC number for readout 0 SENSOR BASED, pixels only so far
/*
int svxAddress::getROCSensor0(int lr, int ld, int sn, int ix, int iz) {

  if(lr<0||4<=lr) {
    cout<<"getChannelSensor0 : out of layer range : "<< lr<<endl;
    return -1;
  }

  int senType = (lr<2) ? eTypePixel : eTypeStrip;

  int roc = -1;
  if(senType==eTypePixel) {
    roc = getPixelRocSensor(sn, iz);
  }
  else {
//    roc = getStripRoc(section, readout, ix); // section(L/R), readout(X/U), ix(0-383)
  }

  return roc;
}
*/

int svxAddress::getROCSensor0(int lr, int ld, int sn, int ix, int iz) {

  //int superchannel = -1;
  int ilayer=lr;
  //int iladder=ld;
  int isensor=sn;

  /*
  if(maptype==0) {
    return -1;
  }
  else {
    std::map<int,int>::iterator it = _svxaddress_sensor1[ilayer][iladder][isensor].begin();
    while ( it != _svxaddress_sensor1[ilayer][iladder][isensor].end() ) {
      if(((it->second)%1000 == ix) && ((it->second)/1000 == iz)) { superchannel = it->first;  break; }
      it++;
    }
    if(superchannel!=-1) { 
      roc = (superchannel%100000 - superchannel%10000)/10000;
    }
  }
  */

  if ( (isensor<0 || isensor>=4) ) { return -1; }
      
  if ( ilayer==0 || ilayer==1 ) {
    if ( iz<0 || iz>=32*4 ) { return -1; }
    return isensor%2*4 + 3 - iz/32;
    
  } else if ( ilayer==2 || ilayer==3 ) {
    if ( iz<0 || iz>=30*2 ) {
      return -1;
    } else {
      return -9999;
    }
  } else {
    cout << "svxAddress::getROCSensor0 : wrong layer input" << endl;
    return -1;
  }

}

//-----------------------------------------------------------------------------------------
// returns module number for readout 0 SENSOR BASED, pixels only so far
int svxAddress::getModuleSensor0(int lr, int ld, int sn, int ix, int iz) {
  
  int module = -1;
  //int superchannel = -1;
  int ilayer=lr;
  int iladder=ld;
  int isensor=sn;

  /*
  if(maptype==0) {
    return -1;
  }
  else {
    std::map<int,int>::iterator it = _svxaddress_sensor1[ilayer][iladder][isensor].begin();
    while ( it != _svxaddress_sensor1[ilayer][iladder][isensor].end() ) {
      if(((it->second)%1000 == ix) && ((it->second)/1000 == iz)) { superchannel = it->first;  break; }
      it++;
    }
    if(superchannel!=-1) {
      module = superchannel/100000;
      //std::cout << "svxAddress(module): " << ilayer << " " << iladder << " " << isensor << " " << ix << " " << iz << " " << superchannel << " " << module << std::endl;
    }
  }
  */

  if ( ilayer==0 || ilayer==1 ) {
    module = getPixelModuleID(ilayer, iladder, isensor);
  } else if ( ilayer==2 || ilayer==3 ) {
    module = getStripModuleID(ilayer, iladder);
  } else {
    cout << "svxAddress::getModuleSensor0 : Input layer number is wrong." << endl;
  }
  
  return module;
}

//-----------------------------------------------------------------------------------------
/*
// returns channel for readout 0 SENSOR BASED, pixels only so far
// lr : 0-1(pixel),      2-3(strip)
// ld : 0-9/0-20(pixel), 0-15/0-23(strip) ButNotUsed
// sn : 0-3(pixel),      0-4/0-5(strip) ButNotUsed
// ix : 0-257(pixel),    0-383(strip)
// iz : 0-127(pixel)     N/A(strip)
int svxAddress::getChannelSensor0(int lr, int ld, int sn, int ix, int iz) {

  if(lr<0||4<=lr) {
    cout<<"getChannelSensor0 : out of layer range : "<< lr<<endl;
    return -1;
  }

  int senType = (lr<2) ? eTypePixel : eTypeStrip;

  int channel=-1;
  if(senType==eTypePixel) {
    channel = getPixelRocChannelSensor(ix, iz);
  } else {
//    channel = getStripRocChannel(ix, iz);
  }

return channel;
}
*/

// returns channel for readout 0 SENSOR BASED
int svxAddress::getChannelSensor0(int lr, int ld, int sn, int ix, int iz, int readout) {

  int channel = -1;
  //int superchannel = -1;
  int ilayer=lr;
  //int iladder=ld;
  //int isensor=sn;

  /*
  if(maptype==0) {
    std::map<int,int>::iterator it = _svxaddress_sensor1[ilayer][iladder][isensor].begin();
    while ( it != _svxaddress_sensor1[ilayer][iladder][isensor].end() ) {
      if(((it->second)%1000 == ix) && ((it->second)/1000 == iz)) { channel = it->first;  break; }
      it++;
    }
  }
  else {
    std::map<int,int>::iterator it = _svxaddress_sensor1[ilayer][iladder][isensor].begin();
    while ( it != _svxaddress_sensor1[ilayer][iladder][isensor].end() ) {
      if(((it->second)%1000 == ix) && ((it->second)/1000 == iz)) { superchannel = it->first;  break; }
      it++;
    }
    channel = superchannel%10000;
  }
  */

  if ( ilayer==0 || ilayer==1 ) {
    channel = getPixelChannel(ix, iz);
  } else if ( ilayer==2 || ilayer==3 ) {
    channel = getStripChannel(ix, iz, readout);
  } else {
    cout << "svxAddress::getChannelSensor0() : Wrong layer input " << endl;
  }

  return channel;
}

int svxAddress::getPixelChannel(int ix, int iz) {

  return ix*32 + iz%32;

}

int svxAddress::getStripChannel(int ix, int iz, int readout) {

  if ( (ix<0 || ix>=384) && (iz<0 || iz>=60) ) {
    cout<<"svxAddress::getStripChannel : Wrong input | ix="<<ix<<" iz="<<iz<<endl; 
    return -1;
  }

  if ( readout==0 ) {
    return ix;
  } else if ( readout==1 ) {
    int channel_z = (iz<30) ? ix - iz%30 : ix - iz%30 + 29;
    if ( channel_z<0    ) { channel_z += 384; }
    if ( channel_z>=384 ) { channel_z -= 384; }
    return channel_z;
  } else {
    cout << "svxAddress::getStripChannel : Readout should be 0 or 1. " << endl;
    return -1;
  }
  
}

//-----------------------------------------------------------------------------------------
// returns channel for readout 0 SENSOR BASED, pixels only so far
int svxAddress::getChannelSensor0(int ix, int iz) {

  int channel = -1;
  int superchannel = -1;
  int ilayer=0;
  int iladder=0;
  int isensor=0;
  
  if(maptype==0) {
    std::map<int,int>::iterator it = _svxaddress_sensor1[ilayer][iladder][isensor].begin();
    while ( it != _svxaddress_sensor1[ilayer][iladder][isensor].end() ) {
      if(((it->second)%1000 == ix) && ((it->second)/1000 == iz)) { channel = it->first;  break; }
      it++;
    }
  }
  else {
    std::map<int,int>::iterator it = _svxaddress_sensor1[ilayer][iladder][isensor].begin();
    while ( it != _svxaddress_sensor1[ilayer][iladder][isensor].end() ) {
      if(((it->second)%1000 == ix) && ((it->second)/1000 == iz)) { superchannel = it->first;  break; }
      it++;
    }
    channel = superchannel%10000;
  }

return channel;
}

//-----------------------------------------------------------------------------------------
// returns channel for readout 0
int svxAddress::getChannel0(int sectype, int ix, int iz) {
  return getChannel0(sectype, 0, ix, iz);
}

int svxAddress::getChannel0(int sectype, int section, int ix, int iz) {

  int channel = -1;
  int ilayer=0;
  int iladder=0;
  int isensor=0;
  int isection=0;

// pixel section types: 2,3,1,3,1,3,2
// stripixel section types: 10,10
  if(sectype==1) {isection=2;}
  else if(sectype==2) {isection=0;}
  else if(sectype==3) {isection=1;}
  else if(sectype==10) { isection=section;}
  else {std::cerr << "svxAddress::getChannel0() ERROR: wrong section type: " << sectype << std::endl; return -1;}

  if(sectype<10) {
    std::map<int,int>::iterator it = _svxaddress_sensec1[ilayer][iladder][isensor][isection].begin();
      while ( it != _svxaddress_sensec1[ilayer][iladder][isensor][isection].end() ) {
        if(((it->second)%1000 == ix) && ((it->second)/1000 == iz)) { channel = it->first;  break; }
        it++;
      }
  }
  else if(sectype==10) {
    std::map<int,int>::iterator it = _svxaddress_sensec11[ilayer][iladder][isensor][isection].begin();
      while ( it != _svxaddress_sensec11[ilayer][iladder][isensor][isection].end() ) {
        if(((it->second)%1000 == ix) && ((it->second)/1000 == iz)) { channel = it->first%1000;  break; }
        it++;
      }
  }
  else { std::cerr << "svxAddress::getChannel0() ERROR: wrong sensor section type: " << sectype << std::endl;  return -1;} 

return channel;
}

//-----------------------------------------------------------------------------------------
// returns channel for readout 1 (stripixels only)
int svxAddress::getChannel1(int sectype, int ix, int iz) {
 return  getChannel1( sectype, 0, ix, iz);
}

int svxAddress::getChannel1(int sectype, int section, int ix, int iz) {

  int channel = -1;
  int ilayer=0;
  int iladder=0;
  int isensor=0;
  int isection=section;

// stripixel section types: 10,10
  if(sectype!=10) {std::cerr << "svxAddress::getChannel1() ERROR: wrong section type: " << sectype << std::endl; return -1;}

  std::map<int,int>::iterator it = _svxaddress_sensec11[ilayer][iladder][isensor][isection].begin();
    while ( it != _svxaddress_sensec11[ilayer][iladder][isensor][isection].end() ) {
      if(((it->second)%1000 == ix) && ((it->second)/1000 == iz)) { channel = it->first/1000;  break; }
      it++;
    }

return channel;
}

/////////////////////////////////////////////
// returns IX in ROC for Pixel
int svxAddress::getPixelRocIX0(int rocchan) { 
  return (isPixelRocChannel(rocchan)) ? (rocchan/32) : -1; 
}

/////////////////////////////////////////////
// returns IZ in ROC for Pixel
int svxAddress::getPixelRocIZ0(int rocchan) { 
  return (isPixelRocChannel(rocchan)) ? (rocchan%32) : -1; 
} 

/////////////////////////////////////////////
// returns IX in Sensor for Pixel
// T.Hachiya 2011.1.21
// int roc     : 0-7 
// int channel : 0-8191 
// exception : out of range : return -1;
int svxAddress::getPixelSensorIX0(const int roc, const int rocchan) {
  if(!isPixelRoc(roc)) {
    return -1; // out of range
  }

  int rocIX0 = getPixelRocIX0(rocchan);
  if(rocIX0==-1) {
    return -1; // out of range
  }

  int sensorIX0 = rocIX0;

  return sensorIX0;
}

/////////////////////////////////////////////
// returns IZ in Sensor for Pixel
// T.Hachiya 2011.1.21
// int roc     : 0-7 
// int channel : 0-8191 
// exception : out of range : return -1;
int svxAddress::getPixelSensorIZ0(const int roc, const int rocchan) {
  if(!isPixelRoc(roc)) {
    return -1; // out of range
  }

  int rocIZ0 = getPixelRocIZ0(rocchan);
  if(rocIZ0==-1) {
    return -1; // out of range
  }

  int subroc = roc%4; // subroc: 0-3
  int sensorIZ0 = (3-subroc)*32+rocIZ0;

  return sensorIZ0;
}


/////////////////////////////////////////////
// returns Section in Sensor for Stripixel
// T.Hachiya 2011.1.21
// int roc     : 0-11 
// exception : out of range : return -1;
int svxAddress::getStripSensorSection(int roc) {
  if(!isStripRoc(roc)) {
    return -1;
  }

  // Left : 0,1,2,9,10,11
  // Right: 3,4,5,6,7,8
  return  (3<=roc&&roc<9) ? 1 : 0;
}

/////////////////////////////////////////////
// returns Readout(X/U) in Sensor for Stripixel
// T.Hachiya 2011.1.21
// int roc     : 0-11 
// exception : out of range : return -1;
int svxAddress::getStripSensorReadout(int roc) {
  if(!isStripRoc(roc)) {
    return -1;
  }

  // X=0 : 1,2,3,7,8,9
  // U=1 : 0,4,5,6,10,11
  return ( (1<=roc&&roc<=3)||(7<=roc&&roc<=9) ) ? 0 : 1;
}

/////////////////////////////////////////////
// returns Hybrid(X/U) in Sensor for Stripixel
// T.Hachiya 2013.10.24
// int roc     : 0-11 
// exception : out of range : return -1;
int svxAddress::getStripSensorHybrid(int roc) {
  if(!isStripRoc(roc)) {
    return -1;
  }

  // Hybrid=0 : 0,1,2
  // Hybrid=0 : 3,4,5
  // Hybrid=0 : 6,7,8
  // Hybrid=0 : 9,10,11
  int hybrid = roc/3;
  return hybrid;
}

/////////////////////////////////////////////
// returns Channel(0-383) in Sensor for Stripixel
// T.Hachiya 2011.1.21
// int roc     : 0-11 
// int rocchan : 0-127 
// exception : out of range : return -1;
int svxAddress::getStripSensorChannel(int roc, int rocchan) {
  if(!isStripRoc(roc) || !isStripRocChannel(rocchan)) {
    return -1;
  }

  // Roc RocChan : channel : description
  //  0   0-127  : 383-256 :  U1L (step=2,inv=1)
  //  1   0-127  : 383-256 :  X1L (step=2,inv=1)
  //  2   0-127  : 255-128 :  X2L (step=1,inv=1)
  //  3   0-127  : 383-256 :  X1R (step=2,inv=1)
  //  4   0-127  : 383-256 :  U1R (step=2,inv=1)
  //  5   0-127  : 255-128 :  U2R (step=1,inv=1)
  //  6   0-127  :   0-127 :  U3R (step=0,inv=0)
  //  7   0-127  :   0-127 :  X3R (step=0,inv=0)
  //  8   0-127  : 128-255 :  X2R (step=1,inv=0)
  //  9   0-127  :   0-127 :  X3L (step=0,inv=0)
  // 10   0-127  :   0-127 :  U3L (step=0,inv=0)
  // 11   0-127  : 128-255 :  U2L (step=1,inv=0)

  // define step = 0:0-127, 1:128-255, 2:256-383
  // step0= 6,7,9,10 1=2,5,8,11 2=0,1,3,4
  int step=0;
  if(roc<6) {
    if((roc%3)<2) step=2; // 0,1,3,4
    else          step=1; // 2,5
  } else {
    if((roc%3)<2) step=0; // 6,7,9,10
    else          step=1; // 8,11
  }
  
  // define invert = 0: normal, 1:invert
  // 0:6,7,8,9,10,11  1:0,1,2,3,4,5
  int invert = (roc<6) ? 1 : 0;

  int subchan = (invert==0) ? rocchan : 127-rocchan;
  int channel = step*128 + subchan;

  return channel;
}

/////////////////////////////////////////////
//  added by T.Hachiya
int svxAddress::getPixelPacketID(const int module) {

  if(!m_initialized) {
    cout<<"svxAddress is not initialized yet"<<endl;
    // this guarantiees (m_aryPixelPacketID.size()==SVXNMODULEPIXEL)
    return -1;
  }

  // for pixel
  // range check
  if(!isPixelModuleID(module)) {
    cout << "Wrong module ID" << endl;
    return -1; // -1 is error code
  }

  // module ID                      -> packet ID
  //  0 -  4 (Barrel 0, WEST-NORTH) -> 24031 - 24035
  //  5 -  9 (Barrel 0, WEST-SOUTH) -> 24046 - 24050
  // 10 - 14 (Barrel 0, EAST-NORTH) -> 24001 - 24005
  // 15 - 19 (Barrel 0, EAST-SOUTH) -> 24016 - 24020
  // 20 - 29 (Barrel 1, WEST-NORTH) -> 24036 - 24045
  // 30 - 39 (Barrel 1, WEST-SOUTH) -> 24051 - 24060
  // 40 - 49 (Barrel 1, EAST-NORTH) -> 24006 - 24015
  // 50 - 59 (Barrel 1, EAST-SOUTH) -> 24021 - 24030

  //if ( use_oldmap ) { return m_aryPixelPacketID_old[module]; }
  //else              { return m_aryPixelPacketID[module]; }
   return m_aryPixelPacketID[module]; 
}

/////////////////////////////////////////////
int svxAddress::getStripPacketID(const int module) {

  if(!m_initialized) {
    cout<<"svxAddress is not initialized yet"<<endl;
    // this guarantiees (m_aryStripPacketID.size()==SVXNMODULESTRIP)
    return -1;
  }

  // for pixel
  // range check
  if(!isStripModuleID(module)) {
    return -1; // -1 is error code
  }

  return m_aryStripPacketID[module];
}

int svxAddress::getPixelModuleID(const int packetid) {

  // for pixel
  // range check
  if(packetid<24001||24060<packetid) {
    cout<<"packetID is out of range, packetID="<<packetid<<endl;
    return -1; // -1 is error code
  }

  // packet ID     -> module ID
  // 24001 - 24005 -> 10 - 14 (Barrel 0, EAST-NORTH)
  // 24006 - 24015 -> 40 - 49 (Barrel 1, EAST-NORTH)
  // 24016 - 24020 -> 15 - 19 (Barrel 0, EAST-SOUTH)
  // 24021 - 24030 -> 50 - 59 (Barrel 1, EAST-SOUTH)
  // 24031 - 24035 ->  0 -  4 (Barrel 0, WEST-NORTH)
  // 24036 - 24045 -> 20 - 29 (Barrel 1, WEST-NORTH)
  // 24046 - 24050 ->  5 -  9 (Barrel 0, WEST-SOUTH)
  // 24051 - 24060 -> 30 - 39 (Barrel 1, WEST-SOUTH)

  //if ( use_oldmap ) { return m_aryPixelModule_old[packetid-24001]; }
  //else              { return m_aryPixelModule[packetid-24001]; }
  return m_aryPixelModule[packetid-24001];

}

int svxAddress::getPixelModuleIDSouthNorth(const int layer, const int ladder, const int south_north)
{
  int module=-1;
  if (layer==0) {
    if ((0<=ladder)&&(ladder<SVXLADDERSLAYER0)) {
      if (south_north==SOUTH) {
	module = ladder+5;
      } else {
	module = ladder;
      }
    } else if ((SVXLADDERSLAYER0<=ladder)&&(ladder<SVXLADDERSLAYER0*2)) {
      if (south_north==SOUTH) {
	module = ladder+10;
      } else {
	module = ladder+5;
      }      
    }
  } else if (layer==1) {
    if ((0<=ladder)&&(ladder<SVXLADDERSLAYER1)) {
      if (south_north==SOUTH) {
	module = ladder+30;
      } else {
	module = ladder+20;
      }
    } else if ((SVXLADDERSLAYER1<=ladder)&&ladder<SVXLADDERSLAYER1*2) {
      if (south_north==SOUTH) {
	module = ladder+40;
      } else {
	module = ladder+30;
      }
    }
  }

  return module;
}

int svxAddress::getPixelSouthNorth(const int module) {
  int south_north=-1;
  if ((0<=module)&&(module<10)) {
    south_north = 1-(module/5);
  } else if ((10<=module)&&(module<20)) {
    south_north = 1-((module-10)/5);
  } else if ((20<=module)&&(module<40)) {
    south_north = 1-((module-20)/10);
  } else if ((40<=module)&&(module<60)) {
    south_north = 1-((module-40)/10);
  }
  return south_north;
}

int svxAddress::getStripModuleID(const int packetid) {

  // for pixel
  // range check
  if(packetid<24101||24140<packetid) {
    cout<<"packetID is out of range, packetID="<<packetid<<endl;
    return -1; // -1 is error code
  }

  return m_aryStripModule[packetid-24101];
}

//
// This function is used to convert moduleID to packetID.
// added by T.Hachiya 2011.01.18 
// for Pixel: packetID is 24001 - 24060, moduleID is 0-59
bool svxAddress::isPixelModuleID(const int module) {
  // range check
  return (0<=module&&module<60);
}


////////////////////////////////////////
// PacketID operation
//  convert from layer, position, side to the packetID for pixel detector
//   layer    : 0, 1,
//   position : 0-9 @ 1st layer, 0-19 @ 2nd layer
//   side     : 0:North, 1:South
//  Author: T.H. (hachiya@rcf.rhic.bnl.gov)
//  date  : 2010.11.10
int svxAddress::getPixelModuleID(int layer, int ladder, int sensor) {
  // range check
  int id=-1;
  if((layer<0) || (2<=layer)) {
    cout<<"layer is out of lange : "<<layer<<endl;
    return id;
  }
  if( (layer==0&&(ladder<0||10<=ladder)) ||
      (layer==1&&(ladder<0||20<=ladder)) )
  {
    cout<<"position is out of lange : "<<ladder<<endl;
    return id;
  }
  if(sensor<0||4<=sensor)
  {
    cout<<"sensor is out of lange : "<<sensor<<endl;
    return id;
  }

  /////////////////////////////////////

  if(layer==0) {// id:0-19
    if(ladder<5) { id = ladder + sensor/2*5; } // north: id=0-4,   south=5-9
    else         { id = ladder + (3-sensor)/2*5 + 5; } // north: id=10-14, south=15-19
  }
  else {       // id:20-59
    if(ladder<10) { id = 20 + ladder + sensor/2*10;  } // north: id=20-29, south: id=30-39
    else         { id = 20 + ladder + (3-sensor)/2*10 + 10; } // north: id=40-49, south: id=50-59
  }

  return id;
}

////////////////////////////////////////
// convert packetID to layer number
// Author: T.H. (hachiya@rcf.rhic.bnl.gov)
//  moduleID layer pos  side |moduleID layer pos  side |moduleID layer pos  side |
//       0     0    0    0   |    20     1    0    0   |    40     1   10    0   |
//       1     0    1    0   |    21     1    1    0   |    41     1   11    0   |
//       2     0    2    0   |    22     1    2    0   |    42     1   12    0   |
//       3     0    3    0   |    23     1    3    0   |    43     1   13    0   |
//       4     0    4    0   |    24     1    4    0   |    44     1   14    0   |
//       5     0    0    1   |    25     1    5    0   |    45     1   15    0   |
//       6     0    1    1   |    26     1    6    0   |    46     1   16    0   |
//       7     0    2    1   |    27     1    7    0   |    47     1   17    0   |
//       8     0    3    1   |    28     1    8    0   |    48     1   18    0   |
//       9     0    4    1   |    29     1    9    0   |    49     1   19    0   |
//      10     0    5    0   |    30     1    0    1   |    50     1   10    1   |
//      11     0    6    0   |    31     1    1    1   |    51     1   11    1   |
//      12     0    7    0   |    32     1    2    1   |    52     1   12    1   |
//      13     0    8    0   |    33     1    3    1   |    53     1   13    1   |
//      14     0    9    0   |    34     1    4    1   |    54     1   14    1   |
//      15     0    5    1   |    35     1    5    1   |    55     1   15    1   |
//      16     0    6    1   |    36     1    6    1   |    56     1   16    1   |
//      17     0    7    1   |    37     1    7    1   |    57     1   17    1   |
//      18     0    8    1   |    38     1    8    1   |    58     1   18    1   |
//      19     0    9    1   |    39     1    9    1   |    59     1   19    1   |
// date  : 2010.11.10
int svxAddress::getPixelLayer(const int module) {
  int layer=-1;
  // range check
  if(!isPixelModuleID(module)) {
    return layer;
  }

  /////////////////////////////////////
  int id = module;
  layer = (id<20) ? 0 : 1;
  return layer;

}

////////////////////////////////////////
// convert moduleID to ladder number (position number)
//  T.H. (hachiya@rcf.rhic.bnl.gov)
int svxAddress::getPixelLadder(int module) {
  int ladder=-1;
  // range check
  if(!isPixelModuleID(module)) {
    return ladder;
  }

  /////////////////////////////////////
  int id = module;
  if(id<20) {
    ladder = (id<10) ? id%5 : id%5 + 5;
  } else {
    int id1 = id-20;
    ladder = (id1<20) ? id%10 : id%10 + 10;
  }
  return ladder;
}

////////////////////////////////////////
// convert packetID to sice number
//  T.H. (hachiya@rcf.rhic.bnl.gov)
//  modufied by akimoto (2011/03/01)
int svxAddress::getPixelSide(int module) {
  // 0:right-side bus, 1:left-side bus
  int side=-1;
  // range check
  if(!isPixelModuleID(module)) {
    return side;
  }

  /////////////////////////////////////
  int id = module;
  if      ( id<10 ) { side = id/5          ; }
  else if ( id<20 ) { side = 1 - (id%10)/5 ; }
  else if ( id<40 ) { side = (id%20)/10    ; }
  else              { side = 1 - (id%20)/10; }
  
  return side;
}


////////////////////////////////////////
// check if the module is corrent
// module: 0-39
//  T.H. 2011.1.24
bool svxAddress::isStripModuleID(const int module) {
  return (0<=module&&module<40);
}

////////////////////////////////////////
// return layer w.r.t module 
// module: 0-39
// layer: 2: 0-15, 1:16-39
// exception -1
//  T.H. 2011.1.24
int svxAddress::getStripLayer(const int module) {
  if(!isStripModuleID(module)) {
    return -1;
  }
  return (0<=module&&module<16) ? 2 : 3;
}

////////////////////////////////////////
// return ladder w.r.t module 
// module: 0-39
// ladder: 0-15(0-15), 0-23(16-39)
// exception -1
//  T.H. 2011.1.24
int svxAddress::getStripLadder(const int module) {
  if(!isStripModuleID(module)) {
    return -1;
  }
  return (0<=module&&module<16) ? module : module-16;
}

////////////////////////////////////////
// return moduleID w.r.t ladder and layer 
// module: 0-39
// layer: 2-3
// ladder: 0-15(2), 0-23(3)
// exception -1
//  T.H. 2011.1.24
int svxAddress::getStripModuleID(int layer, int ladder) {
  if(layer!=2 && layer!=3) {
    return -1;
  }

  if((layer==2&&(ladder<0||16<=ladder)) ||
     (layer==3&&(ladder<0||24<=ladder)) ) {
    return -1;
  }

  return ladder+16*(layer-2);
}


////////////////////////////////////////
//  T.H. (hachiya@rcf.rhic.bnl.gov)
//  update: 2011.01.21
// packetid: 24001-24060, roc=0-7
//
//======================================
// Modified by akimoto (4/20/2011)
//
//                            WEST
//             7              0   7              0   (roc)
//                 3       2         1       0       (sensor)
//             0123456  0123456   0123456  0123456   (sensor_section)
//             0   127  0   127   0   127  0   127   (iz)
// 30-39(mod) ==================|==================  20-29(mod)
//                              |
//                              |
//  5-9(mod)  ==================|==================   0-4(mod)
//                              |
//                              |
// SOUTH -----------------------|----------------------- NORTH
//                              |
//                              |
// 15-19(mod) ==================|==================   10-14(mod)
//                              |
//                              |
// 50-59(mod) ==================|==================  40-49(mod)
//             127   0  127   0   127   0  127   0   (iz)
//             6543210  6543210   6543210  6543210   (sensor_section)
//                 0       1         2       3       (sensor)
//             0              7   0              7   (roc)
//                            EAST
//
//   sensor        : defined by local coordinate (0 - 3)
//   sensorSection : defined by local coordinate (0 - 6)
//   roc           : defined by local coordinate (0 - 7)
//   ix, iz        : defined by local coordinate (ix:0-255, iz:0-127)
//   channel       : ix*32 + iz%32               (0 - 32*256-1=8191)
//

int svxAddress::getPixelSensor(int module, int roc) {

  if(!isPixelModuleID(module)) {
    return -1;
  }

  if(roc<0||7<roc) {
    cout<<"Out of RANGE roc:"<<roc<<endl;
    return -1;
  }

  int sensor = -1;
  if     ( 0<=module && module<5) { sensor = (roc<4) ? 0 : 1;  }
  else if( 5<=module && module<10) { sensor = (roc<4) ? 2 : 3;  }
  else if(10<=module && module<15) { sensor = (roc<4) ? 2 : 3;  }
  else if(15<=module && module<20) { sensor = (roc<4) ? 0 : 1;  }

  else if(20<=module && module<30) { sensor = (roc<4) ? 0 : 1;  }
  else if(30<=module && module<40) { sensor = (roc<4) ? 2 : 3;  }
  else if(40<=module && module<50) { sensor = (roc<4) ? 2 : 3;  }
  else if(50<=module && module<60) { sensor = (roc<4) ? 0 : 1;  }
  
  return sensor;

}

////////////////////////////////////////
//  T.H. (hachiya@rcf.rhic.bnl.gov)
int svxAddress::getPixelSensorSection(int iz) {
  int section = -1;
  if      ( 0<=iz && iz< 31)  section = 0;
  else if (31<=iz && iz< 33)  section = 1;
  else if (33<=iz && iz< 63)  section = 2;
  else if (63<=iz && iz< 65)  section = 3;
  else if (65<=iz && iz< 95)  section = 4;
  else if (95<=iz && iz< 97)  section = 5;
  else if (97<=iz && iz<128)  section = 6;
  else {
    cout<<"Out of RANGE iz= "<<iz<<endl;
  }

  return section;
}

int svxAddress::getPixelSensorSection(int roc, int rocchan) {
  int iz = getPixelSensorIZ0(roc, rocchan);

  return getPixelSensorSection(iz);
}

///////////////////////////////////
// get Roc Channel for PIXEL
// ix : 0-255, iz : 0-31
// channel : 0-8191
// exception -1;
int svxAddress::getPixelRocChannel(int ix, int iz) {
  if( (ix<0||256<=ix) || (iz<0||32<=iz) ) {
    return -1;
  }

  return ix*32 + iz;
}

int
svxAddress::getPixelTile(int ix, int iz)
{
  if(ix<0 || ix>255 || iz<0 || iz>31) {
    return -1;
  }

  int tileRow = (255-ix)/64;
  int tileCol = iz/8;

  return 4*tileRow + tileCol;
}


// ix:0-255, iz=0-127, exception -1
// ix : 0-255, iz : 0-127
// channel : 0-8191
// exception -1;
int svxAddress::getPixelRocChannelSensor(int ix, int iz) {
// return channel number on a pixel sensor (covering 0 - 32757) from ix and iz
  if( (iz<0||128<=iz) ) {
    return -1;
  }
  return getPixelRocChannel(ix, iz%32);
}

// sensor:0-3, ix:0-255, iz=0-127, 
// Roc : 0-7
//exception -1
int svxAddress::getPixelRocSensor(int sensor, int iz) {
  if(sensor<0||4<=sensor) {
    cerr<<" getPixelRocSensor : out of sensor range : "<< sensor<<endl;
    return -1;
  }
  if(iz<0||128<=iz) {
    cerr<<" getPixelRocSensor : out of iz range : "<< iz<<endl;
    return -1;
  }

  // ROC     0      1     2    3     4      5     6    7
  // sensor 0|2    0|2   0|2  0|2   1|3    1|3   1|3  1|3
  // iz    96-127 64-95 32-63 0-31 96-127 64-95 32-63 0-31

  return 3 - (iz/32) + (4*(sensor%2));
}

///////////////////////////////////
// get Roc Channel for STRIP
// section: 0(L)-1(R), readout : 0(X)-1(U), channel : 0-383
// rocchan : 0-127
// exception -1;
int svxAddress::getStripRocChannel(int section, int readout, int channel) {
  if( !isStripSensorSection(section) || 
      !isStripSensorReadout(readout) || 
      !isStripSensorChannel(channel) ) {
    return -1;
  }

  ////////////////////////////////////////////
  // section readout channel  | step  inv  rocchan
  //  0 (L)   0 (X)    0-127  |   0    0     0-127 
  //  0 (L)   0 (X)  128-255  |   1    1   127-0   
  //  0 (L)   0 (X)  256-383  |   2    1   127-0   
  //  0 (L)   1 (U)    0-127  |   0    0     0-127 
  //  0 (L)   1 (U)  128-255  |   1    0     0-127 
  //  0 (L)   1 (U)  256-383  |   2    1   127-0   
  //  1 (R)   0 (X)    0-127  |   0    0     0-127 
  //  1 (R)   0 (X)  128-255  |   1    0     0-127 
  //  1 (R)   0 (X)  256-383  |   2    1   127-0   
  //  1 (R)   1 (U)    0-127  |   0    0     0-127 
  //  1 (R)   1 (U)  128-255  |   1    1   127-0   
  //  1 (R)   1 (U)  256-383  |   2    1   127-0   
  // Roc RocChan : channel : description

  int step = (channel/128);
  int inv  = 0; // inv=0: normal, inv=1: invert

  if(step==2 || (step==1&&((section^readout)==0)) ) { inv = 1; }

  int rocchan = channel%128;
  if(inv==1) rocchan = 127 - rocchan; // invert

  return rocchan;
}

///////////////////////////////////
// get Roc for STRIP
// section : 0(L)-1(R), readout : 0(X)-1(U), channel : 0-383
// rocchan : 0-127
// exception -1;
int svxAddress::getStripRoc(int section, int readout, int channel) {
  if( !isStripSensorSection(section) || 
      !isStripSensorReadout(readout) || 
      !isStripSensorChannel(channel) ) {
    return -1;
  }

  ////////////////////////////////////////////
  // section readout channel(step) | roc
  //  0 (L)   0 (X)    0-127 ( 0 ) |  9
  //  0 (L)   0 (X)  128-255 ( 1 ) |  2
  //  0 (L)   0 (X)  256-383 ( 2 ) |  1
  //  0 (L)   1 (U)    0-127 ( 0 ) | 10
  //  0 (L)   1 (U)  128-255 ( 1 ) | 11
  //  0 (L)   1 (U)  256-383 ( 2 ) |  0
  //  1 (R)   0 (X)    0-127 ( 0 ) |  7
  //  1 (R)   0 (X)  128-255 ( 1 ) |  8
  //  1 (R)   0 (X)  256-383 ( 2 ) |  3
  //  1 (R)   1 (U)    0-127 ( 0 ) |  6
  //  1 (R)   1 (U)  128-255 ( 1 ) |  5
  //  1 (R)   1 (U)  256-383 ( 2 ) |  4

  static const int rocary[2][2][3] = { // [section][readout][step]
   {{9,2,1},{10,11,0}},
   {{7,8,3},{ 6, 5,4}} 
  };

  int step = (channel/128);

  return rocary[section][readout][step];
}

bool svxAddress::isSensorType(int sensorType) {
  return (sensorType==eTypePixel||sensorType==eTypeStrip);
}

int svxAddress::getSensorSection(int sensor, int roc, int rocchan) {
  // exception -1;
  if(!isSensorType(sensor)) {
    return -1;
  }

  int section=-1;
  if(sensor==eTypePixel) {
    section = getPixelSensorSection(roc, rocchan);
  }
  else {
    section = getStripSensorSection(roc);
  }
  return section;
}

int svxAddress::getSensorReadout(int sensor, int roc) {
  // exception -1;
  if(!isSensorType(sensor)) {
    return -1;
  }

  int readout=-1;
  if(sensor==eTypePixel) {
    readout = 0;
  }
  else {
    readout = getStripSensorReadout(roc);
  }
  return readout;
}


int svxAddress::getLayer(int sensor, int module) {
  // exception -1;
  if(!isSensorType(sensor)) {
    return -1;
  }

  int layer=-1;
  if(sensor==eTypePixel) {
    layer= getPixelLayer(module);
  }
  else {
    layer= getStripLayer(module);
  }
  return layer;
}


int svxAddress::getLadder(int sensor, int module) {
  // exception -1;
  if(!isSensorType(sensor)) {
    return -1;
  }

  int ladder=-1;
  if(sensor==eTypePixel) {
    ladder= getPixelLadder(module);
  }
  else {
    ladder= getStripLadder(module);
  }
  return ladder;
}


////////////////////////////////////////
// unittest the function above.
//  T.H. (hachiya@rcf.rhic.bnl.gov)
int svxAddress::unitTest() {
  int nfail=0;
  ////////////////////////////////////////////
  // check "getSensorIX0"
  if(!check_getSensorIX0()) { nfail++;}

  ////////////////////////////////////////////
  // check "getSensorIZ0"
  if(!check_getSensorIZ0()) { nfail++;}

  ////////////////////////////////////////////
  // check "getROCSensor0"
  if(!check_getROCSensor0()) { nfail++;}

  ////////////////////////////////////////////
  // check "getModuleSensor0"
  if(!check_getModuleSensor0()) { nfail++;}

  ////////////////////////////////////////////
  // check "getChannelSensor0"
  if(!check_getChannelSensor0()) { nfail++;}

  ////////////////////////////////////////////
  // check "getPixelChannel"
  if(!check_getPixelChannel()) { nfail++;}

  ////////////////////////////////////////////
  // check "getStripChannel"
  if(!check_getStripChannel()) { nfail++;}

  ////////////////////////////////////////////
  // check "getPixelModuleID"
  if(!check_getPixelModuleID()) { nfail++;}

  ////////////////////////////////////////////
  // check "getPixelLayer"
  if(!check_getPixelLayer()) { nfail++;}

  ////////////////////////////////////////////
  // check "getPixelLadder"
  if(!check_getPixelLadder()) {nfail++;}

  ////////////////////////////////
  // check "getPixelSide"
  if(!check_getPixelSide()) {nfail++;}

  ////////////////////////////////
  // check "isPixelModuleID"
  if(!check_isPixelModuleID()) {nfail++;}

  ////////////////////////////////
  // check "getPixelPacketID"
  if(!check_getPixelPacketID()) {nfail++;}

  ////////////////////////////////
  // check "getPixelSensorSection"
  if(!check_getPixelSensorSection()) {nfail++;}

  ////////////////////////////////
  // check "getPixelRocIX0"
  if(!check_getPixelRocIX0()) {nfail++;}

  ////////////////////////////////
  // check "getPixelRocIZ0"
  if(!check_getPixelRocIZ0()) {nfail++;}

  ////////////////////////////////
  // check "getPixelSensorIX0"
  if(!check_getPixelSensorIX0()) {nfail++;}

  ////////////////////////////////
  // check "getPixelSensorIZ0"
  if(!check_getPixelSensorIZ0()) {nfail++;}

  ////////////////////////////////
  // check "getPixelSensor"
  if(!check_getPixelSensor()) {nfail++;}

  ////////////////////////////////
  // check "getStripSensorSection"
  if(!check_getStripSensorSection()) {nfail++;}

  ////////////////////////////////
  // check "getStripSensorReadout"
  if(!check_getStripSensorReadout()) {nfail++;}

  ////////////////////////////////
  // check "getStripSensorChannel"
  if(!check_getStripSensorChannel()) {nfail++;}

  ////////////////////////////////
  // check "getStripLayer"
  if(!check_getStripLayer()) {nfail++;}

  ////////////////////////////////
  // check "getStripLadder"
  if(!check_getStripLadder()) {nfail++;}

  ////////////////////////////////
  // check "getStripPacketID"
  if(!check_getStripPacketID()) {nfail++;}

  ////////////////////////////////
  // check "getStripRocChannel"
  if(!check_getStripRocChannel()) {nfail++;}

  ////////////////////////////////
  // check "getStripRoc"
  if(!check_getStripRoc()) {nfail++;}

  ////////////////////////////////
  // check "getPixelRocChannelSensor"
  if(!check_getPixelRocChannelSensor()) {nfail++;}

  ////////////////////////////////
  // check "getPixelRocSensor"
  if(!check_getPixelRocSensor()) {nfail++;}


  if(nfail==0) { cout<<"UnitTest is OK!!"<<endl;}
  else        { cout<<"UnitTest is FAILED!!"<<endl;}

  return 0;
}


bool svxAddress::unitTestInt(int val, int exp, const char *valstr, const char *expstr, const char *headstr)
{
  bool ret= (val==exp);
  
  if(Verbose>0||!ret) { cout<<headstr<<" "<<valstr<<val<<", "<<expstr<<exp<<"   "<<flush;}

  if(ret) { if(Verbose>0) {cout<<"OK"<<endl;}   ret=true;}
  else   { /*if(Verbose>0)*/{cout<<"Fail"<<endl;} ret=false;}

  return ret;
}

bool svxAddress::unitTestBool(bool val, bool exp, const char *valstr, const char *expstr, const char *headstr)
{
  bool ret=false;

  if(Verbose>0) {
    char str_val[6];
    if(val) { strcpy(str_val, "true");}
    else    { strcpy(str_val, "false");}

    char str_exp[6];
    if(exp) { strcpy(str_exp, "true");}
    else    { strcpy(str_exp, "false");}

    cout<<headstr<<"  "<<valstr<<str_val<<", "<<expstr<<str_exp<<"   "<<flush;
  }
  
  if(val==exp) { if(Verbose>0) {cout<<"OK"<<endl;}   ret=true;}
  else        { if(Verbose>0) {cout<<"Fail"<<endl;} ret=false;}
  
  return ret;
}

bool svxAddress::check_getSensorIX0()
{
  cout << "Start checking getSensorIX0" << endl;
  
  int nfail = 0;
  bool ret = false;
  
  int channel[7] = {-1, 0, 2000, 4000, 6000, 8191, 8192};
  int resultary[7] = {-1, 0, 62, 125, 187, 255, -1};
  
  int ix;
  int any = 0;
  for ( int ilayer=0; ilayer<4; ilayer++ )
    {
      for ( int i=0; i<7; i++ )
	{
	  ix = getSensorIX0(ilayer, any, any, channel[i], any, any);
	  
	  char tmp[100];
	  sprintf(tmp, "    layer=%d, channel=%d, ix=%d", ilayer, channel[i], ix);
	  if ( ilayer==0 || ilayer==1 )
	    {
	      if ( !unitTestInt(ix, resultary[i], "ix=", "exp=", tmp) ) nfail++;
	    }
	  else
	    {
	      if ( !unitTestInt(ix, -1, "ix=", "exp=", tmp) ) nfail++;
	    }
	}
    }
  
  if ( nfail==0 ) { cout << "  getSensorIX0 is OK" << endl; ret = true; }
  else            { cout << "  getSensorIX0 is Fail. Nfail=" << nfail << endl; }

  return ret;

}

bool svxAddress::check_getSensorIZ0()
{
  cout << "Start checking getSensorIZ0" << endl;

  int nfail = 0;
  bool ret = false;

  int channel[7] = {-1, 0, 2000, 4000, 6000, 8191, 8192};
  int resultary[8][7] = {
    {-1,   0,  16,   0,  16,  31, -1}, {-1,  32,  48,  32,  48,  63, -1},
    {-1,  64,  80,  64,  80,  95, -1}, {-1,  96, 112,  96, 112, 127, -1},
    {-1,   0,  16,   0,  16,  31, -1}, {-1,  32,  48,  32,  48,  63, -1},
    {-1,  64,  80,  64,  80,  95, -1}, {-1,  96, 112,  96, 112, 127, -1}
  };

  int iz;
  int any = 0;
  for (int ilayer=0; ilayer<4; ilayer++ )
    {
      for (int iroc=0; iroc<8; iroc++ )
	{
	  for ( int i=0; i<7; i++ )
	    {
	      iz = getSensorIZ0(ilayer, any, any, channel[i], iroc, any);

	      char tmp[100];
	      sprintf(tmp, "    layer=%d, channel=%d, roc=%d, iz=%d", ilayer, channel[i], iroc, iz);
	      if ( ilayer==0 || ilayer==1 )
		{
		  if ( !unitTestInt(iz, resultary[iroc][i], "iz=", "exp=", tmp) ) nfail++;
		}
	      else
		{
		if ( !unitTestInt(iz, -1, "iz=", "exp=", tmp) ) nfail++;
		}
	    }
	}
    }
  
  if ( nfail==0 ) { cout << "  getSensorIZ0 is OK" << endl; ret = true; }
  else            { cout << "  getSensorIZ0 is Fail. Nfail=" << nfail << endl; }

  return ret;

}

bool svxAddress::check_getROCSensor0()
{
  cout << "Start checking getROCSensor0" << endl;

  int nfail = 0;
  bool ret = false;
  
  int iz[6] = {-1, 0, 50, 100, 127, 128};
  int resultary[4][6] = {
    {-1, 0, 1, 3, 3, -1}, {-1, 4, 5, 7, 7, -1},
    {-1, 0, 1, 3, 3, -1}, {-1, 4, 5, 7, 7, -1},
  };

  int iroc;
  int any = 0;
  for (int ilayer=0; ilayer<4; ilayer++ )
    {
      for (int isensor=0; isensor<4; isensor++ )
	{
	  for ( int i=0; i<6; i++ )
	    {
	      iroc = getROCSensor0(ilayer, any, isensor, any, iz[i]);

	      char tmp[100];
	      sprintf(tmp, "    layer=%d, sensor=%d, iz=%d, roc=%d", ilayer, isensor, iz[i], iroc);
	      if ( ilayer==0 || ilayer==1 )
		{
		  if ( !unitTestInt(iroc, resultary[isensor][i], "roc=", "exp=", tmp) ) nfail++;
		}
	      else
		{
		  if ( !unitTestInt(iroc, -9999, "roc=", "exp=", tmp) ) nfail++;
		}
	    }
	}
    }
  
  if ( nfail==0 ) { cout << "  getROCSensor0 is OK" << endl; ret = true; }
  else            { cout << "  getROCSensor0 is Fail. Nfail=" << nfail << endl; }

  return ret;
  
}

bool svxAddress::check_getModuleSensor0()
{
  cout << "Start checking getModuleSensor0" << endl;

  int nfail = 0;
  bool ret = false;
  
  int nladder[4] = {10, 20, 16, 24};
  int resultary_l0[10][4] = {
    { 0, 0, 5, 5}, { 1, 1, 6, 6}, { 2, 2, 7, 7}, { 3, 3, 8, 8}, { 4, 4, 9, 9},
    {15,15,10,10}, {16,16,11,11}, {17,17,12,12}, {18,18,13,13}, {19,19,14,14}
  };

  int resultary_l1[20][4] = {
    {20,20,30,30}, {21,21,31,31}, {22,22,32,32}, {23,23,33,33}, {24,24,34,34},
    {25,25,35,35}, {26,26,36,36}, {27,27,37,37}, {28,28,38,38}, {29,29,39,39},
    {50,50,40,40}, {51,51,41,41}, {52,52,42,42}, {53,53,43,43}, {54,54,44,44},
    {55,55,45,45}, {56,56,46,46}, {57,57,47,47}, {58,58,48,48}, {59,59,49,49}
  };

  int resultary_l23[40] = {
     0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
    10,11,12,13,14,15,16,17,18,19,
    20,21,22,23,24,25,26,27,28,29,
    30,31,32,33,34,35,36,37,38,39
  };

  int module;
  int any = 0;
  for (int ilayer=0; ilayer<4; ilayer++ )
    {
      for (int iladder=0; iladder<nladder[ilayer]; iladder++ )
	{
	  if ( ilayer==0 || ilayer==1 )
	    {
	      for ( int isensor=0; isensor<4; isensor++ )
		{
		  char tmp[100];
		  sprintf(tmp, "    layer=%d, ladder=%d, sensor=%d", ilayer, iladder, isensor);
		  module = getModuleSensor0(ilayer, iladder, isensor, any, any);  
		  if ( ilayer==0 )
		    {
		      if ( !unitTestInt(module, resultary_l0[iladder][isensor], "module=", "exp=", tmp) ) nfail++;
		    }
		  else
		    {
		      if ( !unitTestInt(module, resultary_l1[iladder][isensor], "module=", "exp=", tmp) ) nfail++;
		    }
		}
	    }
	  else
	    {
	      char tmp[100];
	      sprintf(tmp, "    layer=%d, ladder=%d", ilayer, iladder);

	      module = getModuleSensor0(ilayer, iladder, any, any, any);
	      if ( !unitTestInt(module, resultary_l23[iladder+(ilayer-2)*16], "module=", "exp=", tmp) ) nfail++;
	    }
	}
    }
  
  if ( nfail==0 ) { cout << "  getModuleSensor0 is OK" << endl; ret = true; }
  else            { cout << "  getModuleSensor0 is Fail. Nfail=" << nfail << endl; }
  
  return ret;
  
}

bool svxAddress::check_getChannelSensor0()
{
  cout << "Start checking getChannelSensor0" << endl;
  
  int nfail = 0;
  bool ret = false;
  
  int ixz[5][2] = {{-1,-1}, {10,25}, {10,50}, {200,25}, {200,50}};

  int resultary_pixel0[5] = {-1, 345, 338, 6425, 6418};
  int resultary_pixel1[5] = {-1,  -1,  -1,   -1,   -1};
  int resultary_strip0[5] = {-1,  10,  10,  200,  200};   /// for readout==0
  int resultary_strip1[5] = {-1, 365,  19,  175,  209};   /// for readout==1

  int channel;
  int any = 0;
  for (int ilayer=0; ilayer<4; ilayer++ )
    {
      for (int i=0; i<5; i++ )
	{
	  for (int ireadout=0; ireadout<2; ireadout++ )
	    {
	      char tmp[100];
	      sprintf(tmp, "    layer=%d, ix=%d, iz=%d, ireadout=%d",
		      ilayer, ixz[i][0], ixz[i][1], ireadout);
	      channel = getChannelSensor0(ilayer, any, any, ixz[i][0], ixz[i][1], ireadout);
	      
	      if ( ilayer==0 || ilayer==1 )
		{
		  if ( ireadout==0 )
		    {
		      if ( !unitTestInt(channel, resultary_pixel0[i], "channel=", "exp=", tmp) ) nfail++;
		    }
		  else
		    {
		      if ( !unitTestInt(channel, resultary_pixel1[i], "channel=", "exp=", tmp) ) nfail++;
		    }
		}
	      else
		{
		  if ( ireadout==0 )
		    {
		      if ( !unitTestInt(channel, resultary_strip0[i], "channel=", "exp=", tmp) ) nfail++;
		    }
		  else
		    {
		      if ( !unitTestInt(channel, resultary_strip1[i], "channel=", "exp=", tmp) ) nfail++;
		    }
		}
	    }
	}
    }
  
  if ( nfail==0 ) { cout << "  getChannelSensor0 is OK" << endl; ret = true; }
  else            { cout << "  getChannelSensor0 is Fail. Nfail=" << nfail << endl; }
  
  return ret;
  
}

bool svxAddress::check_getPixelChannel()
{
  cout << "Start checking getPixelChannel" << endl;
  
  int nfail = 0;
  bool ret = false;
  
  int ixz[5][2] = {{-1,-1}, {10,25}, {10,50}, {200,25}, {200,50}};

  int resultary_pixel0[5] = {-1, 345, 338, 6425, 6418};

  int channel;
  for (int ilayer=0; ilayer<2; ilayer++ )
    {
      for (int i=0; i<5; i++ )
	{
	  char tmp[100];
	  sprintf(tmp, "    layer=%d, ix=%d, iz=%d", ilayer, ixz[i][0], ixz[i][1]);
	  channel = getPixelChannel(ixz[i][0], ixz[i][1]);
	  
	  if ( !unitTestInt(channel, resultary_pixel0[i], "channel=", "exp=", tmp) ) nfail++;
	}
    }
  
  if ( nfail==0 ) { cout << "  getPixelChannel is OK" << endl; ret = true; }
  else            { cout << "  getPixelChannel is Fail. Nfail=" << nfail << endl; }
  
  return ret;
  
}
bool svxAddress::check_getStripChannel()
{
  cout << "Start checking getStripChannel" << endl;
  
  int nfail = 0;
  bool ret = false;
  
  int ixz[5][2] = {{-1,-1}, {10,25}, {10,50}, {200,25}, {200,50}};

  int resultary_strip0[5] = {-1,  10,  10,  200,  200};   /// for readout==0
  int resultary_strip1[5] = {-1, 365,  19,  175,  209};   /// for readout==1

  int channel;
  for (int ilayer=2; ilayer<3; ilayer++ )
    {
      for (int i=0; i<5; i++ )
	{
	  for (int ireadout=0; ireadout<2; ireadout++ )
	    {
	      char tmp[100];
	      sprintf(tmp, "    ix=%d, iz=%d, ireadout=%d", ixz[i][0], ixz[i][1], ireadout);
	      channel = getStripChannel(ixz[i][0], ixz[i][1], ireadout);
	      
	      if ( ireadout==0 )
		{
		  if ( !unitTestInt(channel, resultary_strip0[i], "channel=", "exp=", tmp) ) nfail++;
		}
	      else
		{
		  if ( !unitTestInt(channel, resultary_strip1[i], "channel=", "exp=", tmp) ) nfail++;
		}
	    }
	}
    }
  
  if ( nfail==0 ) { cout << "  getStripChannel is OK" << endl; ret = true; }
  else            { cout << "  getStripChannel is Fail. Nfail=" << nfail << endl; }
  
  return ret;
  
}

bool svxAddress::check_getPixelModuleID()
{
  ////////////////////////////////////////////
  // check "getPixelModuleID"
  cout<<"Starting getPixelModuleID"<<endl;

  bool ret=false;

  int posmax[2]={10, 20};

  // layer =[0,1]
  // ladder=[0,1,2,3,4,5,6,7,8,9]
  // side  =[0,1]
  int resultary_l0[10][2] = {
                {0,5}, {1,6}, {2,7}, {3,8}, {4,9},
                {10,15}, {11,16}, {12,17}, {13,18}, {14,19},
              };

  int resultary_l1[20][2] = {
          {20,30}, {21,31}, {22,32}, {23,33}, {24,34},
          {25,35}, {26,36}, {27,37}, {28,38}, {29,39},
          {40,50}, {41,51}, {42,52}, {43,53}, {44,54},
          {45,55}, {46,56}, {47,57}, {48,58}, {49,59}
  };



  // packetID
  int nfail=0;

  int module = 0;
  for(int ilayer=0; ilayer<2; ilayer++) {
    for(int ipos=0; ipos<posmax[ilayer]; ipos++) {
      for(int iside=0; iside<2; iside++) {
        module = getPixelModuleID(ilayer, ipos, iside);

        char tmp[100];
        sprintf(tmp, "    layer=%d, ladder=%d, side=%d, moduleID=%d", ilayer, ipos, iside, module);

        if(ilayer==0) {
          if(!unitTestInt(module, resultary_l0[ipos][iside], "module=", "exp=", tmp)) nfail++;
        }
        else {
          if(!unitTestInt(module, resultary_l1[ipos][iside], "module=", "exp=", tmp)) nfail++;
        }
      }
    }
  }

  // out of range check
  int lr_ps_sd[7][3]={
    {-1,0,0},{2,0,0},
    {0,-1,0},{0,10,0},{1,20,0},
    {0,0,-1},{0,0,2}
  };

  for(int i=0; i<7; i++) {
    int layer =lr_ps_sd[i][0];
    int ladder=lr_ps_sd[i][1];
    int side  =lr_ps_sd[i][2];
    module = getPixelModuleID(layer, ladder, side);

    char tmp[100];
    sprintf(tmp, "    layer=%d, ladder=%d, side=%d, moduleID=%d", layer, ladder, side, module);

    if(!unitTestInt(module, -1, "module=", "exp=", tmp)) nfail++;
  }
  
  if(nfail==0) { cout<<"  getPixelModuleID is OK"<<endl; ret=true;}
  else        { cout<<"  getPixelModuleID is Fail. Nfail="<<nfail<<endl; }

  return ret;
}
  

bool svxAddress::check_getPixelLayer() {
  ////////////////////////////////////////////
  // check "getPixelLayer"
  bool ret=false;

  cout<<"Starting getPixelLayer"<<endl;
  int resultary[62] = {-1, 
                        0,0,0,0,0, 0,0,0,0,0,  
                        0,0,0,0,0, 0,0,0,0,0,  
                        1,1,1,1,1, 1,1,1,1,1,  1,1,1,1,1, 1,1,1,1,1,  
                        1,1,1,1,1, 1,1,1,1,1,  1,1,1,1,1, 1,1,1,1,1,  
                       -1};
  int nfail=0;
  for(int i=0; i<62; i++) {
    int pid = i-1;
    int layer = getPixelLayer(pid);

    char tmp[50]; sprintf(tmp, "    moduleID=%d", pid);
    if(!unitTestInt(layer, resultary[i], "layer=", "exp=", tmp)) nfail++;
  }
  if(nfail==0) { cout<<"  getPixelLayer is OK"<<endl; ret=true;}
  else        { cout<<"  getPixelLayer is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getPixelLadder() {
  ////////////////////////////////
  // check "getPixelLadder"
  bool ret=false;

  cout<<"Starting getPixelLadder"<<endl;
  int resultary[62] = {-1, 
                        0,1,2,3,4, 0,1,2,3,4,  
                        5,6,7,8,9, 5,6,7,8,9,  
                        0,1,2,3,4, 5,6,7,8,9,   
                        0,1,2,3,4, 5,6,7,8,9,   
                        10,11,12,13,14, 15,16,17,18,19,
                        10,11,12,13,14, 15,16,17,18,19,
                       -1};
  int nfail=0;
  for(int i=0; i<62; i++) {
    int pid = i-1;
    int ladder = getPixelLadder(pid);

    char tmp[50]; sprintf(tmp, "    moduleID=%d", pid);
    if(!unitTestInt(ladder, resultary[i], "ladder=", "exp=", tmp)) nfail++;
  }
  if(nfail==0) { cout<<"  getPixelLadder is OK"<<endl; ret=true;}
  else        { cout<<"  getPixelLadder is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getPixelSide() {
  ////////////////////////////////
  // check "getPixelSide"
  bool ret=false;

  cout<<"Starting getPixelSide"<<endl;
  int resultary[62] = {-1, 0,0,0,0,0, 1,1,1,1,1, 1,1,1,1,1, 0,0,0,0,0,
                           0,0,0,0,0,0,0,0,0,0, 1,1,1,1,1,1,1,1,1,1,
                           1,1,1,1,1,1,1,1,1,1, 0,0,0,0,0,0,0,0,0,0,
                       -1};

  int nfail=0;
  for(int i=0; i<62; i++) {
    int pid = i-1;
    int side = getPixelSide(pid);

    char tmp[50]; sprintf(tmp, "    moduleID=%d", pid);
    if(!unitTestInt(side, resultary[i], "side=", "exp=", tmp)) nfail++;
  
  }
  if(nfail==0) { cout<<"  getPixelSide is OK"<<endl; ret=true;}
  else        { cout<<"  getPixelSide is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_isPixelModuleID() {
  ////////////////////////////////
  // check "isPixelModuleID"
  bool ret=false;

  cout<<"Starting isPixelModuleID"<<endl;
  bool resultary[62] = {false, 
                        true,true,true,true,true, true,true,true,true,true,  
                        true,true,true,true,true, true,true,true,true,true,  
                        true,true,true,true,true, true,true,true,true,true,  
                        true,true,true,true,true, true,true,true,true,true,  
                        true,true,true,true,true, true,true,true,true,true,  
                        true,true,true,true,true, true,true,true,true,true,  
                        false};
  int nfail=0;
  for(int i=0; i<62; i++) {
    int pid = -1+i;
    bool isMod = isPixelModuleID(pid);

    char tmp[50]; sprintf(tmp, "    moduleID=%d", pid);
    if(!unitTestBool(isMod, resultary[i], "isMod=", "exp=", tmp)) nfail++;
  }
  if(nfail==0) { cout<<"  isPixelModuleID is OK"<<endl; ret=true;}
  else        { cout<<"  isPixelModuleID is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getPixelPacketID() {
  ////////////////////////////////
  // check "getPixelPacketID"
  bool ret=false;

  cout<<"Starting getPixelPacketID"<<endl; 
  int resultary[62] = {-1, 
                       24031,24032,24033,24034,24035, 24046,24047,24048,24049,24050,
                       24001,24002,24003,24004,24005, 24016,24017,24018,24019,24020,
                       24036,24037,24038,24039,24040, 24041,24042,24043,24044,24045,
                       24051,24052,24053,24054,24055, 24056,24057,24058,24059,24060,
                       24006,24007,24008,24009,24010, 24011,24012,24013,24014,24015,
                       24021,24022,24023,24024,24025, 24026,24027,24028,24029,24030,
                       -1};
  int nfail=0;
  for(int i=0; i<62; i++) {
    int pid = i-1;
    int packetID = getPixelPacketID(pid);

    char tmp[50]; sprintf(tmp, "    moduleID=%d", pid);
    if(!unitTestInt(packetID, resultary[i], "packetID=", "expect=", tmp)) nfail++;
  }
  if(nfail==0) { cout<<"  getPixelPacketID is OK"<<endl; ret=true;}
  else        { cout<<"  getPixelPacketID is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getPixelSensorSection() {
  ////////////////////////////////
  // check "getPixelSensorSection"
  bool ret=false;

  cout<<"Starting getPixelSensorSection"<<endl;

  int checkary[16]  = {-1, 0, 30, 31, 32, 33, 62, 63, 64, 65, 94, 95, 96, 97, 127, 128};
  int resultary[16] = {-1, 0,  0,  1,  1,  2,  2,  3,  3,  4,  4,  5,  5,  6,   6,  -1};
  int nfail=0;
  for(int i=0; i<16; i++) {
    int section = getPixelSensorSection(checkary[i]);

    char tmp[50]; sprintf(tmp, "    iz=%d", checkary[i]);
    if(!unitTestInt(section, resultary[i], "section=", "expect=", tmp)) nfail++;
  }
  if(nfail==0) { cout<<"  getPixelSensorSection is OK"<<endl; ret=true;}
  else        { cout<<"  getPixelSensorSection is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getPixelRocIX0() {
  ////////////////////////////////
  // check "getPixelRocIX0"
  bool ret=false;

  cout<<"Starting getPixelRocIX0"<<endl;

  int nfail=0;
  for(int i=0; i<8194; i++) {
    int chan = i-1;
    int ix   = getPixelRocIX0(chan);

    int exp = chan/32;
    if(i==0||i==8193) exp = -1;
    char tmp[50]; sprintf(tmp, "    getPixelRocIX0 i=%d, chan=%d", i, chan);
    if(!unitTestInt(ix, exp, "ix=", "exp=", tmp)) nfail++;
  }
  if(nfail==0) { cout<<"  getPixelRocIX0 is OK"<<endl; ret=true;}
  else        { cout<<"  getPixelRocIX0 is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getPixelRocIZ0() {
  ////////////////////////////////
  // check "getPixelRocIZ0"
  bool ret=false;

  cout<<"Starting getPixelRocIZ0"<<endl;

  int nfail=0;
  for(int i=0; i<8194; i++) {
    int chan = i-1;
    int iz   = getPixelRocIZ0(chan);

    int exp = chan%32;
    if(i==0||i==8193) exp = -1;
    char tmp[50]; sprintf(tmp, "    getPixelRocIZ0 i=%d, chan=%d", i, chan);
    if(!unitTestInt(iz, exp, "iz=", "exp=", tmp)) nfail++;
  }
  if(nfail==0) { cout<<"  getPixelRocIZ0 is OK"<<endl; ret=true;}
  else        { cout<<"  getPixelRocIZ0 is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getPixelSensorIX0() {
  ////////////////////////////////
  // check "getPixelSensorIX0"
  bool ret=false;

  cout<<"Starting getPixelSensorIX0"<<endl;

  int in_chan[4]={-1, 0, 8191, 8192};
  int in_exp[4] ={-1, 0, 255, -1};

  int nfail=0;
  for(int i=0; i<10; i++) {
    for(int j=0; j<4; j++) {
    int roc = i-1;
    int chan = in_chan[j];
    int ix   = getPixelSensorIX0(roc, chan);

    int exp = in_exp[j];;
    if(i==0||i==9) exp=-1;

    char tmp[50]; sprintf(tmp, "    getPixelSensorIX0 roc=%d, chan=%d", roc, chan);
    if(!unitTestInt(ix, exp, "ix=", "exp=", tmp)) nfail++;
    }
  }
  if(nfail==0) { cout<<"  getPixelSensorIX0 is OK"<<endl; ret=true;}
  else        { cout<<"  getPixelSensorIX0 is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getPixelSensorIZ0() {
  ////////////////////////////////
  // check "getPixelSensorIZ0"
  bool ret=false;

  cout<<"Starting getPixelSensorIZ0"<<endl;

  int in_chan[4]={-1, 0, 8191, 8192};
  int in_exp[10][4] =
                 { {-1, -1, -1, -1},
                   {-1, 96, 127, -1}, {-1, 64, 95, -1}, {-1, 32, 63, -1}, {-1, 0, 31, -1},
                   {-1, 96, 127, -1}, {-1, 64, 95, -1}, {-1, 32, 63, -1}, {-1, 0, 31, -1},
                   {-1, -1, -1, -1},
                 };

  int nfail=0;
  for(int i=0; i<10; i++) {
    for(int j=0; j<4; j++) {
    int roc = i-1;
    int chan = in_chan[j];
    int iz   = getPixelSensorIZ0(roc, chan);

    int exp = in_exp[i][j];

    char tmp[50]; sprintf(tmp, "    getPixelSensorIZ0 roc=%d, chan=%d", roc, chan);
    if(!unitTestInt(iz, exp, "iz=", "exp=", tmp)) nfail++;
    }
  }
  if(nfail==0) { cout<<"  getPixelSensorIZ0 is OK"<<endl; ret=true;}
  else        { cout<<"  getPixelSensorIZ0 is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getPixelSensor() {
  ////////////////////////////////
  // check "getPixelSensor"
  bool ret=false;

  cout<<"Starting getPixelSensor"<<endl;

  int in_exp[62][10] =
                 { {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
                   {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, //0, 1
                   {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, //2, 3
                   {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, //4
                   {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, //5,6
                   {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, //7,8
                   {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, //9
                   {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, //10,11
                   {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, //12,13
                   {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, //14
                   {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, //15,16
                   {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, //17,18
                   {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, //15

                   {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, //20,21
                   {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, //22,23
                   {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, //24,25
                   {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, //26,27
                   {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, //28,29
                   {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, //30,31
                   {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, //32,33
                   {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, //34,35
                   {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, //36,37
                   {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, //38,39
                   {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, //40,41
                   {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, //42,43
                   {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, //44,45
                   {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, //46,47
                   {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, {-1, 2, 2, 2, 2, 3, 3, 3, 3,-1}, //48,49
                   {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, //50,51
                   {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, //52,53
                   {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, //54,55
                   {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, //56,57
                   {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, {-1, 0, 0, 0, 0, 1, 1, 1, 1,-1}, //58,59
                   {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
                 };

  int nfail=0;
  for(int i=0; i<62; i++) {
    for(int j=0; j<10; j++) {
    int module = i-1;
    int roc    = j-1;
    int sensor   = getPixelSensor(module, roc);

    int exp = in_exp[i][j];

    char tmp[50]; sprintf(tmp, "    getPixelSensor module=%d, roc=%d", module, roc);
    if(!unitTestInt(sensor, exp, "sensor=", "exp=", tmp)) nfail++;
    }
  }
  if(nfail==0) { cout<<"  getPixelSensor is OK"<<endl; ret=true;}
  else        { cout<<"  getPixelSensor is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getStripSensorSection() {
  ////////////////////////////////
  // check "getStripSensorSection"

  cout<<"Starting getStripSensorSection"<<endl;

  bool ret=false;
  int nfail=0;

  int expary[14]={-1, 0,0,0, 1,1,1,1,1,1, 0,0,0, -1};

  for(int i=0; i<14; i++) {
    int roc = i-1;
    int section = getStripSensorSection(roc);

    int exp = expary[i];

    char tmp[50]; sprintf(tmp, "    getStripSensorSection roc=%d", roc);
    if(!unitTestInt(section, exp, "section=", "exp=", tmp)) nfail++;
  }
  if(nfail==0) { cout<<"  getStripSensorSection is OK"<<endl; ret=true;}
  else        { cout<<"  getStripSensorSection is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getStripSensorReadout() {
  ////////////////////////////////
  // check "getStripSensorReadout"

  cout<<"Starting getStripSensorReadout"<<endl;

  bool ret=false;
  int nfail=0;

  int expary[14]={-1, 1,0,0, 0,1,1, 1,0,0, 0,1,1, -1};

  for(int i=0; i<14; i++) {
    int roc = i-1;
    int readout = getStripSensorReadout(roc);

    int exp = expary[i];

    char tmp[50]; sprintf(tmp, "    getStripSensorReadout roc=%d", roc);
    if(!unitTestInt(readout, exp, "readout=", "exp=", tmp)) nfail++;
  }
  if(nfail==0) { cout<<"  getStripSensorReadout is OK"<<endl; ret=true;}
  else        { cout<<"  getStripSensorReadout is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getStripSensorChannel() {
  ////////////////////////////////
  // check "getStripSensorChannel"

  cout<<"Starting getStripSensorChannel"<<endl;

  bool ret=false;
  int nfail=0;

  int expstart_ary[14]={-1, 383, 383, 255, 383, 383, 255, 0, 0, 128, 0, 0, 128, -1};
  int expsign_ary[14] ={ 0,  -1,  -1,  -1,  -1,  -1,  -1, 1, 1,   1, 1, 1,   1,  0};

  for(int i=0; i<14; i++) {
    for(int j=0; j<130; j++) {
      int roc = i-1;
      int rocchan = j-1;
      int channel = getStripSensorChannel(roc, rocchan);

      int exp = expstart_ary[i]+expsign_ary[i]*rocchan;
      if(rocchan<0||128<=rocchan) exp=-1;

      char tmp[50]; sprintf(tmp, "    getStripSensorChannel roc=%d rocchan=%d", roc, rocchan);
      if(!unitTestInt(channel, exp, "channel=", "exp=", tmp)) nfail++;
    }
  }
  if(nfail==0) { cout<<"  getStripSensorChannel is OK"<<endl; ret=true;}
  else        { cout<<"  getStripSensorChannel is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getStripLayer() {
  ////////////////////////////////
  // check "getStripLayer"

  cout<<"Starting getStripLayer"<<endl;

  bool ret=false;
  int nfail=0;

  int exp_ary[42]={-1, 
                    2,2,2,2, 2,2,2,2,  2,2,2,2, 2,2,2,2,
                    3,3,3,3, 3,3,3,3, 3,3,3,3,  3,3,3,3, 3,3,3,3, 3,3,3,3,
                   -1};

  for(int i=0; i<42; i++) {
    int module = i-1;
    int layer = getStripLayer(module);

    int exp = exp_ary[i];

    char tmp[50]; sprintf(tmp, "    getStripLayer module=%d ", module);
    if(!unitTestInt(layer, exp, "layer=", "exp=", tmp)) nfail++;
  }
  if(nfail==0) { cout<<"  getStripLayer is OK"<<endl; ret=true;}
  else        { cout<<"  getStripLayer is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getStripLadder() {
  ////////////////////////////////
  // check "getStripLadder"

  cout<<"Starting getStripLadder"<<endl;

  bool ret=false;
  int nfail=0;

  int exp_ary[42]={-1, 
                    0,1,2,3, 4,5,6,7,  8,9,10,11, 12,13,14,15,
                    0,1,2,3, 4,5,6,7, 8,9,10,11,  12,13,14,15, 16,17,18,19, 20,21,22,23,
                   -1};

  for(int i=0; i<42; i++) {
    int module = i-1;
    int layer = getStripLadder(module);

    int exp = exp_ary[i];

    char tmp[50]; sprintf(tmp, "    getStripLadder module=%d ", module);
    if(!unitTestInt(layer, exp, "layer=", "exp=", tmp)) nfail++;
  }
  if(nfail==0) { cout<<"  getStripLadder is OK"<<endl; ret=true;}
  else        { cout<<"  getStripLadder is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getStripPacketID() {
  ////////////////////////////////
  // check "getStripPacketID"
  bool ret=false;

  cout<<"Starting getStripPacketID"<<endl; 
  int resultary[62] = {-1, 
                       24101,24102,24103,24104,24105, 24106,24107,24108,24109,24110,
                       24111,24112,24113,24114,24115, 24116,24117,24118,24119,24120,
                       24121,24122,24123,24124,24125, 24126,24127,24128,24129,24130,
                       24131,24132,24133,24134,24135, 24136,24137,24138,24139,24140,
                       -1};
  int nfail=0;
  for(int i=0; i<42; i++) {
    int pid = i-1;
    int packetID = getStripPacketID(pid);

    char tmp[50]; sprintf(tmp, "    moduleID=%d", pid);
    if(!unitTestInt(packetID, resultary[i], "packetID=", "expect=", tmp)) nfail++;
  }
  if(nfail==0) { cout<<"  getStripPacketID is OK"<<endl; ret=true;}
  else        { cout<<"  getStripPacketID is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getStripRocChannel() {
  ////////////////////////////////
  // check "getStripRocChannel"
  bool ret=false;

  cout<<"Starting getStripRocChannel"<<endl; 
  int start[2][4][3] = {{{-1,-1,-1}, {0,127,127},{0,  0,127}, {-1,-1,-1}}, 
                        {{-1,-1,-1}, {0,  0,127},{0,127,127}, {-1,-1,-1}}};
  int sign [2][4][3] = {{{ 0, 0, 0}, {1, -1, -1},{1, 1, -1}, { 0, 0, 0}},
                        {{ 0, 0, 0}, {1,  1, -1},{1,-1, -1}, { 0, 0, 0}}};

  int nfail=0;
  for(int i=0; i<4; i++) {
    for(int j=0; j<4; j++) {
      for(int k=0; k<386; k++) {
        int section = i-1;
        int readout = j-1;
        int channel = k-1;

        int rocchan = getStripRocChannel(section, readout, channel);

        int exp = -1;
        {
          int step    = channel/128;
          int subchan = channel%128;
          //if(1<=i&&i<3) {
          if(0<=section&&section<2) {
            if(0<=step&&step<3) exp = start[section][j][step] + sign[section][j][step] * subchan;
          }
        }
      
        char tmp[50]; sprintf(tmp, "    section=%d, readout=%d, channel=%d", section, readout, channel);
        if(!unitTestInt(rocchan, exp, "rocchan=", "expect=", tmp)) nfail++;
      }
    }
  }
  if(nfail==0) { cout<<"  getStripRocChannel is OK"<<endl; ret=true;}
  else        { cout<<"  getStripRocChannel is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getStripRoc() {
  ////////////////////////////////
  // check "getStripRoc"
  bool ret=false;

  cout<<"Starting getStripRoc"<<endl; 
  int expary[2][2][3] = {{{9,2,1},{10,11,0}},
                         {{7,8,3},{ 6,5, 4}}
                        };

  int nfail=0;
  for(int i=0; i<4; i++) {
    for(int j=0; j<4; j++) {
      for(int k=0; k<386; k++) {
        int section = i-1;
        int readout = j-1;
        int channel = k-1;

        int roc = getStripRoc(section, readout, channel);

        int exp = -1;
        int step    = (channel>=0) ? channel/128 : -1;
        {
          if(0<=section&&section<2 &&
             0<=readout&&readout<2 &&
             0<=step&&step<3)
            exp = expary[section][readout][step];
        }
      
        char tmp[50]; 
	sprintf(tmp, "    section=%d, readout=%d, channel=%d, step=%d", section, readout, channel, step);
        if (!unitTestInt(roc, exp, "roc=", "expect=", tmp))
	  nfail++;
      }
    }
  }

  if(nfail==0) { 
    cout<<"  getStripRoc is OK"<<endl; 
    ret=true;
  }
  else {
    cout << "  getStripRoc is Fail. Nfail=" << nfail << endl;
  }

  return ret;
}

bool svxAddress::check_getPixelRocChannelSensor() {
  ////////////////////////////////
  // check "getStripRoc"
  bool ret=false;

  cout<<"Starting getPixelRocChannelSensor"<<endl; 
// ix:0-255, iz=0-127, exception -1
// ix : 0-255, iz : 0-127
// channel : 0-8191
// exception -1;

  int nfail=0;
  for(int i=0; i<258; i++) {
    for(int j=0; j<130; j++) {
      int ix=i-1;
      int iz=j-1;
      int channel = getPixelRocChannelSensor(ix, iz);

      int exp = -1;
      {
        if(0<=ix&&ix<256&&0<=iz&&iz<128) 
          exp = 32*ix+(iz%32);
      }
      
      char tmp[50]; sprintf(tmp, "    ix=%d, iz=%d", ix, iz);
      if(!unitTestInt(channel, exp, "channel=", "expect=", tmp)) nfail++;
    }
  }

  if(nfail==0) { cout<<"  getPixelRocChannelSensor is OK"<<endl; ret=true;}
  else        { cout<<"  getPixelRocChannelSensor is Fail. Nfail="<<nfail<<endl; }

  return ret;
}

bool svxAddress::check_getPixelRocSensor() {
  ////////////////////////////////
  // check "getPixelRocSensor"
  bool ret=false;
  
  // sensor:0-3, ix:0-255, iz=0-127,
  // Roc : 0-7
  //exception -1
  
  // ROC     0      1     2    3     4      5     6    7
  // sensor 0|2    0|2   0|2  0|2   1|3    1|3   1|3  1|3
  // iz    96-127 64-95 32-63 0-31 96-127 64-95 32-63 0-31
  int exproc[8]={0,1,2,3,4,5,6,7};

  int nfail=0;
  for(int i=0; i<6; i++) {
    for(int j=0; j<130; j++) {
      int sensor=i-1;
      int iz=j-1;
      int roc = getPixelRocSensor(sensor, iz);

      int exp = -1;
      {
        if(0<=sensor && sensor<4 && 0<=iz && iz<128) {
          int idx = ((sensor%2)*4) + (3-(iz/32));
          exp = exproc[idx];
        }
      }
      
      char tmp[50]; sprintf(tmp, "    sensor=%d, iz=%d", sensor, iz);
      if(!unitTestInt(roc, exp, "roc=", "expect=", tmp)) nfail++;
    }
  }

  if(nfail==0) {
    cout<<"  getPixelRocSensor is OK"<<endl; 
    ret=true;
  }
  else {
    cout<<"  getPixelRocSensor is Fail. Nfail="<<nfail<<endl; 
  }
  
  return ret;
}

void svxAddress::showPixelPacketID() {
  if(!isInitialized()) {
    cout<<PHWHERE<<" svxAddress is not initialized yet."<<endl;
    return ;
  }

  cout<<"module : packetID  | packetID : module"<<endl;
  int size = m_aryPixelPacketID.size();
  for(int i=0; i<size; i++) {
    cout<<i      <<" : "<<m_aryPixelPacketID[i]<<" | ";
    cout<<i+24001<<" : "<<m_aryPixelModule[i]<<endl;
  }
}
