//--------------------------------------------------------------- 
//                                                                
// Created by Sasha Lebedev (ISU) lebedev@iastate.edu 05/19/99
//                                                                
// Description: Implementation of TEC Address Class  
//                                                                
//----------------------------------------------------------------

#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "TecAddressObject.hh" 
#include "PdbMapHS.hh"
#include <fstream>

using namespace std;

/// Constructor
TecAddressObject::TecAddressObject()
{
  Debug = 0;

  current_arm=0;
  current_sector=0;
  current_side=0;
  current_plane=0;
  current_wire=0;
  current_crate=0;
  current_slot=0;
  current_psadd=0;
  current_channel=0;
  current_index=0;
  current_packetid=0;

  int i;
  for(i=0; i<TECMAPDBMAXENT; i++) arm[i]=0;
  for(i=0; i<TECMAPDBMAXENT; i++) sector[i]=0;
  for(i=0; i<TECMAPDBMAXENT; i++) plane[i]=0;
  for(i=0; i<TECMAPDBMAXENT; i++) side[i]=0;
  for(i=0; i<TECMAPDBMAXENT; i++) fwire[i]=0;
  for(i=0; i<TECMAPDBMAXENT; i++) lwire[i]=0;
  for(i=0; i<TECMAPDBMAXENT; i++) crate[i]=0;
  for(i=0; i<TECMAPDBMAXENT; i++) slot[i]=0;
  for(i=0; i<TECMAPDBMAXENT; i++) psadd[i]=0;
  for(i=0; i<TECMAPDBMAXENT; i++) fchannel[i]=0;
  for(i=0; i<TECMAPDBMAXENT; i++) lchannel[i]=0;
  for(i=0; i<TECMAPDBMAXENT; i++) packetid[i]=0;

  iFlag=-1;

  CalibName = "map.tec.run00";
  setBankNumber(TECMAPBANK);
  setBankID(BankNumber);
  setDescription("TEC Hardware/software Map");

  Tsearch.setToSystemTime();
}

/// Destructor
TecAddressObject::~TecAddressObject() { }

// member functions

///
void TecAddressObject::UseSimulationDatabase() {
CalibName = "map.tec.geant0";
}
///
void TecAddressObject::UseRealDatabase() {
CalibName = "map.tec.run00";
}

/// Set current indices
PHBoolean TecAddressObject::setSoft(int iarm, int isector, 
                                    int iside, int iplane, int iwire) {
//----------------------------------------
// Set Software Indices
//----------------------------------------
  current_arm=iarm;
  current_sector=isector;
  current_plane=iplane;
  current_side=iside;
  current_wire=iwire;

//----------------------------------------
// and Calculate Hardware Indices
//----------------------------------------
  PHBoolean ok = FromSoftToHard();  
        if(!ok) return False;
  return True;
}

/// Set current indices
PHBoolean TecAddressObject::setHard(int ipacketid, int ichannel) {

//----------------------------------------
// Set Hardware Indices
//----------------------------------------
  PHBoolean ok1 = False;
  for(int j=0; j<TECMAPDBMAXENT; j++) {
    if((ipacketid==packetid[j]) && 
       (ichannel>=fchannel[j]) && (ichannel<=lchannel[j])) {
         current_crate = crate[j]; 
         current_slot = slot[j]; 
         current_psadd = psadd[j]; 
         current_channel = ichannel;
	 ok1 = True;
         break;
    }
  }
  if(!ok1) return False;

//----------------------------------------
// and Calculate Software Indices
//----------------------------------------
  PHBoolean ok2 = FromHardToSoft();
        if(!ok2) return False;

  return True;
}

/// Set current indices
PHBoolean TecAddressObject::setHard(int icrate, int islot, 
                                    int ipsadd, int ichannel) {
//----------------------------------------
// Set Hardware Indices
//----------------------------------------
  current_crate=icrate;
  current_slot=islot;
  current_psadd=ipsadd;
  current_channel=ichannel;

//----------------------------------------
// and Calculate Software Indices
//----------------------------------------
  PHBoolean ok = FromHardToSoft();
  	if(!ok) return False;
  return True;
}

PHBoolean TecAddressObject::FromSoftToHard() {
//---------------------------------------------------
// How to get from Software indices to Hardware ones
//---------------------------------------------------
  int iarm    = current_arm;  
  int isector = current_sector;  
  int iside   = current_side;  
  int iplane  = current_plane;  
  int iwire   = current_wire;  

  int icrate   = getCrate(iarm, isector, iside, iplane, iwire);
	if(icrate<0) return False;
  int islot    = getSlot(iarm, isector, iside, iplane, iwire);
	if(islot<0) return False;
  int ipsadd   = getPsadd(iarm, isector, iside, iplane, iwire);
	if(ipsadd<0) return False;
  int ichannel = getChannel(iarm, isector, iside, iplane, iwire);
	if(ichannel<0) return False;
  int ipacketid = getPacketID(iarm, isector, iside, iplane, iwire);
	if(ipacketid<0) return False;

  current_crate=icrate;
  current_slot=islot;
  current_psadd=ipsadd;
  current_channel=ichannel;
  current_index=current_side+current_plane*TECMAXSIDE+
                current_sector*TECMAXSIDE*TECMAXPLANE;
  current_packetid=ipacketid;

  return True;
}

PHBoolean TecAddressObject::FromHardToSoft() {
//---------------------------------------------------
// How to get from Hardware indices to Software ones
//---------------------------------------------------
  int icrate   = current_crate;
  int islot    = current_slot;
  int ipsadd   = current_psadd;
  int ichannel = current_channel;

  int iarm    = getArm(icrate, islot, ipsadd, ichannel);
	if(iarm<0) return False;
  int isector = getSector(icrate, islot, ipsadd, ichannel);
	if(isector<0) return False;
  int iside   = getSide(icrate, islot, ipsadd, ichannel);
	if(iside<0) return False;
  int iplane  = getPlane(icrate, islot, ipsadd, ichannel);
	if(iplane<0) return False;
  int iwire   = getWire(icrate, islot, ipsadd, ichannel);
	if(iwire<0) return False;
  int ipacketid   = getPacketID(icrate, islot, ichannel);
	if(ipacketid<0) return False;

  current_arm=iarm;
  current_sector=isector;
  current_side=iside;
  current_plane=iplane;
  current_wire=iwire;
  current_index=current_side+current_plane*TECMAXSIDE+
                current_sector*TECMAXSIDE*TECMAXPLANE;
  current_packetid=ipacketid;

  return True;
}

int TecAddressObject::getPacketID(int tmpcrate, int tmpslot, int tmpchannel)
{
static int kuku=0;
// Gets Packet ID from Hardware indices.
  if(iFlag!=0 && kuku==0) {
    cerr << "TecAddressObject ERROR: TAO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
    return -1;
  }
int j,nent;
int tmppid=-1;
  nent=TECMAPDBMAXENT;
  for(j=0; j<nent; j++)
  {
    if((tmpcrate==crate[j]) && (tmpslot==slot[j]) &&
       (tmpchannel>=fchannel[j]) && (tmpchannel<=lchannel[j]))
    {
      tmppid=packetid[j]; break;
    }
  }
return tmppid;
} // end getPacketID() 


int TecAddressObject::getArm(int tmpcrate, int tmpslot, int tmppsadd, int tmpchannel)
{
static int kuku=0;
// Gets Software ARM from Hardware indices.
  if(iFlag!=0 && kuku==0) {
    cerr << "TecAddressObject ERROR: TAO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
    return -1;
  }
int j,nent;
int tmparm=-1;
  nent=TECMAPDBMAXENT;
  for(j=0; j<nent; j++)
  {
    if((tmpcrate==crate[j]) && (tmpslot==slot[j]) && (tmppsadd==psadd[j]) &&
       (tmpchannel>=fchannel[j]) && (tmpchannel<=lchannel[j]))
    {
      tmparm=arm[j]; break;
    }
  }
return tmparm;
} /* end getArm() */

int TecAddressObject::getSector(int tmpcrate, int tmpslot, int tmppsadd, int tmpchannel)
{
static int kuku=0;
// Gets Software SECTOR from Hardware indices.
  if(iFlag!=0 && kuku==0) {
    cerr << "TecAddressObject ERROR: TAO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
    return -1;
  }
int j,nent;
int tmpsector=-1;
  nent=TECMAPDBMAXENT;
  for(j=0; j<nent; j++)
  {
    if((tmpcrate==crate[j]) && (tmpslot==slot[j]) && (tmppsadd==psadd[j]) &&
       (tmpchannel>=fchannel[j]) && (tmpchannel<=lchannel[j]))
    {
      tmpsector=sector[j]; break;
    }
  }
return tmpsector;
} /* end getSector() */

// Gets Software SIDE from Hardware indices.
int TecAddressObject::getSide(int tmpcrate, int tmpslot, int tmppsadd, int tmpchannel) {
static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecAddressObject ERROR: TAO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
    return -1;
  }
int j,nent;
int tmpside=-1;
  nent=TECMAPDBMAXENT;
  for(j=0; j<nent; j++)
  {
    if((tmpcrate==crate[j]) && (tmpslot==slot[j]) && (tmppsadd==psadd[j]) &&
       (tmpchannel>=fchannel[j]) && (tmpchannel<=lchannel[j]))
    {
      tmpside=side[j]; break;
    }
  }
return tmpside;
} /* end getSide() */

// Gets Software PLANE from Hardware indices.
int TecAddressObject::getPlane(int tmpcrate, int tmpslot, int tmppsadd, int tmpchannel)
{
static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecAddressObject ERROR: TAO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
    return -1;
  }
int j,nent;
int tmpplane=-1;
  nent=TECMAPDBMAXENT;
  for(j=0; j<nent; j++)
  {
    if((tmpcrate==crate[j]) && (tmpslot==slot[j]) && (tmppsadd==psadd[j]) &&
       (tmpchannel>=fchannel[j]) && (tmpchannel<=lchannel[j]))
    {
      tmpplane=plane[j]; break;
    }
  }
return tmpplane;
} /* end getPlane() */

PHBoolean TecAddressObject::HardToSoft(int tmpcrate, int tmpslot, int tmpch, int& tmpsect, int& tmpside, int& tmpplane, int& tmpwire) {

  for(int j=0; j<TECMAPDBMAXENT; j++) {
    if((tmpcrate==crate[j]) && (tmpslot==slot[j]) &&
       (tmpch>=fchannel[j]) && (tmpch<=lchannel[j])) {
      if(fwire[j]<lwire[j]) {
        tmpwire=fwire[j]+tmpch-fchannel[j]; 
        tmpsect=sector[j]; 
        tmpside=side[j]; 
        tmpplane=plane[j]; 
        return True;
      }
      else {
        tmpwire=fwire[j]-tmpch+fchannel[j]; 
        tmpsect=sector[j]; 
        tmpside=side[j]; 
        tmpplane=plane[j]; 
        return True;
      }
    }
  }

return False;

}

PHBoolean TecAddressObject::SoftToHard(int tmpsect, int tmpside, int tmpplane, int tmpwire, int& tmpcrate, int& tmpslot, int& tmpps1, int& tmpps2, int& tmpch) {
cout << "TecAddressObject:: SoftToHard not yet implemented." << endl;
return False;
}

// Gets Software WIRE from Hardware indices.
int TecAddressObject::getWire(int tmpcrate, int tmpslot, int tmppsadd, int tmpchannel)
{
static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecAddressObject ERROR: TAO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
    return -1;
  }
int j,nent;
int tmpwire=-1;
  nent=TECMAPDBMAXENT;
  for(j=0; j<nent; j++)
  {
    if((tmpcrate==crate[j]) && (tmpslot==slot[j]) && (tmppsadd==psadd[j]) &&
       (tmpchannel>=fchannel[j]) && (tmpchannel<=lchannel[j]))
    {
      if(fwire[j]<lwire[j])
      {
        tmpwire=fwire[j]+tmpchannel-fchannel[j]; break;
      }
      else
      {
        tmpwire=fwire[j]-tmpchannel+fchannel[j]; break;
      }
    }
  }
return tmpwire;
} /* end getWire() */

// Gets Packet ID from Software indices.
int TecAddressObject::getPacketID(int iarm, int isector, int iside, int iplane, int iwire)
{
static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecAddressObject ERROR: TAO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
    return -1;
  }
int j,nent;
int tmppid=-1;
  nent=TECMAPDBMAXENT;
  for(j=0; j<nent; j++)
  {
    if((iarm==arm[j]) && (isector==sector[j]) && (iside==side[j]) &&
       (iplane==plane[j]) &&
       (((iwire>=fwire[j]) && (iwire<=lwire[j])) ||
        ((iwire<=fwire[j]) && (iwire>=lwire[j])))
      )
    {
      tmppid=packetid[j]; break;
    }
  }
return tmppid;
}

// Gets Hardware CRATE from Packet ID.
int TecAddressObject::getCrate(int ipacketid) {
static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecAddressObject ERROR: TAO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
    return -1;
  }
int j,nent;
int tmpcrate=-1;
  nent=TECMAPDBMAXENT;
  for(j=0; j<nent; j++)
  {
    if(ipacketid==packetid[j]) {
      tmpcrate=crate[j]; break;
    }
  }
return tmpcrate;
}

// Gets Hardware CRATE from Software indices.
int TecAddressObject::getCrate(int iarm, int isector, int iside, int iplane, int iwire)
{
static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecAddressObject ERROR: TAO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
    return -1;
  }
int j,nent;
int tmpcrate=-1;
  nent=TECMAPDBMAXENT;
  for(j=0; j<nent; j++)
  {
    if((iarm==arm[j]) && (isector==sector[j]) && (iside==side[j]) &&
       (iplane==plane[j]) && 
       (((iwire>=fwire[j]) && (iwire<=lwire[j])) ||
        ((iwire<=fwire[j]) && (iwire>=lwire[j])))
      )
    {
      tmpcrate=crate[j]; break;
    }
  }
return tmpcrate;
}

// Gets Hardware SLOT from Packet ID.
int TecAddressObject::getSlot(int ipacketid) {
static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecAddressObject ERROR: TAO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
    return -1;
  }
int j,nent;
int tmpslot=-1;
  nent=TECMAPDBMAXENT;
  for(j=0; j<nent; j++)
  {
    if(ipacketid==packetid[j]) {
      tmpslot=slot[j]; break;
    }
  }
return tmpslot;
}

// Gets Hardware SLOT from Software indices.
int TecAddressObject::getSlot(int iarm, int isector, int iside, int iplane, int iwire)
{
static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecAddressObject ERROR: TAO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
    return -1;
  }
int j,nent;
int tmpslot=-1;
  nent=TECMAPDBMAXENT;
  for(j=0; j<nent; j++)
  {
    if((iarm==arm[j]) && (isector==sector[j]) && (iside==side[j]) &&
       (iplane==plane[j]) && 
       (((iwire>=fwire[j]) && (iwire<=lwire[j])) ||
        ((iwire<=fwire[j]) && (iwire>=lwire[j])))
      )
    {
      tmpslot=slot[j]; break;
    }
  }
return tmpslot;
}

// Gets first Hardware PS Address from Packet ID.
int TecAddressObject::getPsadd1(int ipacketid)
{
static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecAddressObject ERROR: TAO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
    return -1;
  }
int j,nent;
int tmppsadd=-1;
  nent=TECMAPDBMAXENT;
  for(j=0; j<nent; j++)
  {
    if(ipacketid==packetid[j]) {
      tmppsadd=psadd[j]; break;
    }
  }
return tmppsadd;
}

// Gets second (if any) Hardware PS Address from Packet ID.
int TecAddressObject::getPsadd2(int ipacketid)
{ 
static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecAddressObject ERROR: TAO not initialized." << endl 
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
    return -1;
  } 
int j,nent,npsadd=0;
int tmppsadd[2];
  tmppsadd[0]=-1; tmppsadd[1]=-1;
  nent=TECMAPDBMAXENT;
  for(j=0; j<nent; j++)
  { 
    if(ipacketid==packetid[j]) {
      tmppsadd[npsadd]=psadd[j]; 
      npsadd++;
      if(npsadd>2) { 
        cout << "TecAddressObject ERROR: Too many PS addresses for Packet ID = " << ipacketid << endl;
        return -1; 
      }
    }
  }

  if(npsadd==2) {
    return tmppsadd[1];
  }
  else {
    return -1;
  }
}


// Gets Hardware PS Address from Software indices.
int TecAddressObject::getPsadd(int iarm, int isector, int iside, int iplane, int iwire)
{
static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecAddressObject ERROR: TAO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
    return -1;
  }
int j,nent;
int tmppsadd=-1;
  nent=TECMAPDBMAXENT;
  for(j=0; j<nent; j++)
  {
    if((iarm==arm[j]) && (isector==sector[j]) && (iside==side[j]) &&
       (iplane==plane[j]) && 
       (((iwire>=fwire[j]) && (iwire<=lwire[j])) ||
        ((iwire<=fwire[j]) && (iwire>=lwire[j])))
      )
    {
      tmppsadd=psadd[j]; break;
    }
  }
return tmppsadd;
}

int TecAddressObject::getChannel(int iarm, int isector, int iside, int iplane, int iwire)
{
static int kuku=0;
// Gets Hardware CHANNEL from Software indices.
  if(iFlag!=0 && kuku==0) {
    cerr << "TecAddressObject ERROR: TAO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
    return -1;
  }
int j,nent;
int tmpchannel=-1;
  nent=TECMAPDBMAXENT;
  for(j=0; j<nent; j++)
  {
    if((iarm==arm[j]) && (isector==sector[j]) && (iside==side[j]) &&
       (iplane==plane[j]) && 
       (((iwire>=fwire[j]) && (iwire<=lwire[j])) ||
        ((iwire<=fwire[j]) && (iwire>=lwire[j])))
      )
    {
      if(fwire[j]<lwire[j])
      {
        tmpchannel=fchannel[j]+iwire-fwire[j]; break;
      }
      else
      {
        tmpchannel=fchannel[j]-iwire+fwire[j]; break;
      }
    }
  }
return tmpchannel;
}

/// Fetch information from default ASCII file 
PHBoolean TecAddressObject::FetchFromFile() {
  const char *filename = "tecmap_database_east0.txt";
  return TecAddressObject::FetchFromFile(filename);
}

/// Fetch information from ASCII file "filename"
PHBoolean TecAddressObject::FetchFromFile(const char* filename) 
{
  const char *tecmapdb = filename;
  ifstream file;
  file.open(tecmapdb);
  int tmp[12];
    int ibuf=0;

  if(!file)
  {
    cerr << "TecAddressObject::FetchFromFile ERROR:" << endl 
         << "  Can not open " << filename << endl
         << "  Please make a soft link to this file" << endl
         << "    ln -s /afs/rhic/phenix/PHENIX_LIB/calibration/new/" << filename << " " << filename << endl
         << "  or run " << endl
         << "    /afs/rhic/phenix/software/calibration/LuxorLinker"
         << endl;
    return False;
  }
  else
  {
    for(int ip=0; ip<TECMAPDBMAXENT; ip++)
    {
      file >> tmp[0] >> tmp[1] >> tmp[2] >> tmp[3] >> tmp[4] >> tmp[5] >> tmp[6] >> tmp[7] >> tmp[8] >> tmp[9] >> tmp[10] >> tmp[11];

      if(tmp[11] < 5000) {
        cout << "TecAddressObject::FetchFromFile ERROR: Wrong ASCII file." << endl;
        return False;
      }

      if (file.eof()) break;

      arm[ip]      = tmp[0]; 
      sector[ip]   = tmp[1];  
      side[ip]     = tmp[2];  
      plane[ip]    = tmp[3];  
      fwire[ip]    = tmp[4];  
      lwire[ip]    = tmp[5];  
      crate[ip]    = tmp[6];  
      slot[ip]     = tmp[7];  
      psadd[ip]    = tmp[8];  
      fchannel[ip] = tmp[9];  
      lchannel[ip] = tmp[10]; 
      packetid[ip] = tmp[11]; 
      ibuf++;
    } /* end ip loop */

    if(Debug>0) cout
    << "TecAddressObject::FetchFromFile: TEC Map read from " << filename << endl
    << "          Total " << ibuf << " rows."
    << endl;
 }

  iFlag=0;
  return True;
}

/// Fetch information from Objy Database
PHBoolean TecAddressObject::Fetch() {

int a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11;
int noent=0;
int i;

   PdbBankManager *bankManager = PdbBankManager::instance();
   PdbApplication *application = bankManager->getApplication();
   PdbCalBank *tecBank = 0;

   if (application->startRead()) {

      PHTimeStamp tSearch = Tsearch;
      PdbBankID bankID;
      const char *calname = CalibName;
      int bankid = BankNumber;
      bankID.setInternalValue(bankid);

      if(Debug>0) {
         cout << "TecAddressObject::Fetch: calibname = " << calname << endl;
         cout << "TecAddressObject::Fetch: bankid = " << bankid << endl;
         cout << "Search Time = "; tSearch.print(); cout << endl;
      }

      PdbMapHS* achan;

      tecBank = bankManager->fetchBank("PdbMapHSBank", bankID, calname, tSearch);
      if (tecBank) {
         if(Debug > 1) tecBank->print();
         for(i=0; i < (int)tecBank->getLength(); i++) {
         achan = (PdbMapHS*)&(tecBank->getEntry(i));
         a0 = achan->getParameter(0);
         a1 = achan->getParameter(1);
         a2 = achan->getParameter(2);
         a3 = achan->getParameter(3);
         a4 = achan->getParameter(4);
         a5 = achan->getParameter(5);
         a6 = achan->getParameter(6);
         a7 = achan->getParameter(7);
         a8 = achan->getParameter(8);
         a9 = achan->getParameter(9);
         a10 = achan->getParameter(10);
         a11 = achan->getParameter(11);
if(Debug > 4) cout << a0 << " " << a1 << " " << a2 << " " << a3 << " " << a4 << " " << a5 << " " << a6 << " " << a7 << " " << a8 << " " << a9 << " " << a10 << " " << a11 << endl;
           arm[i]=a0;
           sector[i]=a1;
           side[i]=a2;
           plane[i]=a3;
           fwire[i]=a4;
           lwire[i]=a5;
           crate[i]=a6;
           slot[i]=a7;
           psadd[i]=a8;
           fchannel[i]=a9;
           lchannel[i]=a10;
           packetid[i]=a11;
                noent++;
         } // i loop end
      }
      else {
         cerr << "TecAddressObject::Fetch() -> ERROR: " << "bankManager returned zero-pointer" << endl;
	 return False;
      }
     application->commit();

   } // startread
   else
   {
     application->abort();
     cerr << "TecAddressObject::Fetch() -> ERROR: Transaction aborted." << endl;
     return False;
   }

  iFlag=0;
  if(tecBank) delete tecBank;
  return True;
}

// Update Hardware/Software Map from current TAO in memory
PHBoolean TecAddressObject::Update(PHTimeStamp* Tbeg, PHTimeStamp* Tend) {

// Count number of non-zero entries in the TAO
  int nentries=0;
  for(int j=0; j<TECMAPDBMAXENT; j++) {
    if(packetid[j]==0) break;
    nentries++;
  }

   PdbBankManager *bankManager = PdbBankManager::instance();
   PdbApplication *application = bankManager->getApplication();
   PdbCalBank *tecBank = 0;

   if(Debug>0) cout <<
     "TecAddressObject::Update: opening FD in update mode..." << endl;

   if (application->startUpdate()) {
      PHTimeStamp tStart = *Tbeg;
      PHTimeStamp tStop  = *Tend;

      PdbBankID bankID;
      const char *descrip   = Description;

      const char *calibname = CalibName;
      int bankid = TECMAPBANK;

      bankID.setInternalValue(bankid);

      if(Debug>0) {
          cout << "TecAddressObject::Update: calibname = " << calibname << endl;
          cout << "TecAddressObject::Update: bankid = " << bankid << endl;
          cout << "Start validity = "; tStart.print(); cout << endl;
          cout << "End validity = "; tStop.print(); cout << endl;
      }

          tecBank = bankManager->createBank("PdbMapHSBank", bankID, descrip, tStart, tStop, calibname);

          size_t itmp = nentries;
          tecBank->setLength(itmp);

          if(Debug>1) tecBank->print();

	  int mumu;

              for(int i=0; i < (int)tecBank->getLength(); i++) {

                PdbMapHS* achan = (PdbMapHS*)&(tecBank->getEntry(i));
                mumu = arm[i]; achan->setParameter(0,mumu);
                mumu = sector[i]; achan->setParameter(1,mumu);
                mumu = side[i]; achan->setParameter(2,mumu);
                mumu = plane[i]; achan->setParameter(3,mumu);
                mumu = fwire[i]; achan->setParameter(4,mumu);
                mumu = lwire[i]; achan->setParameter(5,mumu);
                mumu = crate[i]; achan->setParameter(6,mumu);
                mumu = slot[i]; achan->setParameter(7,mumu);
                mumu = psadd[i]; achan->setParameter(8,mumu);
                mumu = fchannel[i]; achan->setParameter(9,mumu);
                mumu = lchannel[i]; achan->setParameter(10,mumu);
                mumu = packetid[i]; achan->setParameter(11,mumu);
              } // end i loop

      application->commit();
   }
   else {
     cerr << "TecAddressObject::Update: ERROR: failed to start application for update." << endl;
     return False;
   }

  if(tecBank) delete tecBank;
  return True;
}


