#include "emcBadModulesv1.h"

#include "emcDefines.h"
#include "emcFEMList.h"
#include "EmcIndexer.h"
#include "PbScIndexer.h"
#include "PbGlIndexer.h"
#include "emcDataManager.h"
#include "emcManageable.h"
#include "emcQAFEM.h"
#include "emcRejectList.h"

#include <fstream>
#include <cstdio>
#include <cassert>
#include <sstream>

ClassImp(emcBadModulesv1)

using namespace std;

static size_t NFEMS = 172;
static size_t NCHANNELS = NFEMS*144;

static string INFO_NO_REJECT_FILE = "<INFO> No Q&A information from physics.";
static string ERROR_CANNOT_READ_REJECT_FILE = "<ERROR> Cannot open reject file ";

//_____________________________________________________________________________
emcBadModulesv1::emcBadModulesv1()
  : fDataSource(emcDataStorageMap(emcManageable::kNone)),
    fOrigin(emcBadModules::kNone),
    fDirectory(""),
    fTimeStamp(0),
    fIsRejectListAlreadyRead(false)
{
  Allocate();
  ComputeMaps();
}

//_____________________________________________________________________________
emcBadModulesv1::emcBadModulesv1(const char* directory, 
				 emcBadModules::EInformationOrigin origin,
				 bool init,
				 const char* sectors)
  : fDataSource(emcDataStorageMap(emcManageable::kFile_ASCII)),
    fOrigin(origin),
    fDirectory(directory), 
    fTimeStamp(0),
    fIsRejectListAlreadyRead(false)
{
  Allocate();
  if (init) 
    {
      ComputeMaps(sectors);
    }
}

//_____________________________________________________________________________
emcBadModulesv1::emcBadModulesv1(const PHTimeStamp& ts,
				 emcBadModules::EInformationOrigin origin,
				 const emcDataStorageMap& source,
				 bool init,
				 const char* sectors)
  : fDataSource(source),
    fOrigin(origin),
    fDirectory(""), 
    fTimeStamp(ts),
    fIsRejectListAlreadyRead(false)
{
  Allocate();
  if (init) 
    {
      ComputeMaps(sectors);
    }
}

//_____________________________________________________________________________
emcBadModulesv1::emcBadModulesv1(const emcBadModulesv1& bm) 
  : emcBadModules()
{
  bm.copyTo(*this);
}

//_____________________________________________________________________________
emcBadModulesv1&
emcBadModulesv1::operator=(const emcBadModulesv1& bm)
{
  if ( this != &bm )
    {
      bm.copyTo(*this);
    }
  return *this;
}

//_____________________________________________________________________________
void
emcBadModulesv1::copyTo(emcBadModulesv1& to) const
{
  to.fDataSource = fDataSource;
  to.fOrigin = fOrigin;
  to.fDirectory = fDirectory;
  to.fTimeStamp = fTimeStamp;
  to.fErrorMap = fErrorMap;
  to.fWarnMap = fWarnMap;
  to.fCollectedFEMs = fCollectedFEMs;
  to.fComputedFEMs = fComputedFEMs;
  to.fErrorRaw = fErrorRaw;
  to.fWarnRaw = fWarnRaw;
}

//_____________________________________________________________________________
emcBadModulesv1::~emcBadModulesv1()
{
  
}

//_____________________________________________________________________________
void
emcBadModulesv1::Allocate(void)
{
  fErrorRaw[emcBadModules::kOnline].resize(NCHANNELS,0);
  fWarnRaw[emcBadModules::kOnline].resize(NCHANNELS,0);

  fErrorRaw[emcBadModules::kPhysics].resize(NCHANNELS,0);
  fWarnRaw[emcBadModules::kPhysics].resize(NCHANNELS,0);

  fErrorMap.resize( fErrorRaw[emcBadModules::kOnline].size() , 0 );
  fWarnMap.resize( fErrorRaw[emcBadModules::kOnline].size() , 0 ); 

  fCollectedFEMs.resize(NFEMS,false);
  fComputedFEMs.resize(NFEMS,false);
}

//_____________________________________________________________________________
void
emcBadModulesv1::Collect(size_t ifem)
{
  if ( fOrigin == emcBadModules::kPhysics || fOrigin == emcBadModules::kAll ) 
    {
      CollectPhysicsQA();
    }

  if ( fOrigin == emcBadModules::kOnline || fOrigin == emcBadModules::kAll ) 
    {
      CollectOnlineQA(ifem);
    }

  fCollectedFEMs[ifem] = true;
}

//_____________________________________________________________________________
string 
emcBadModulesv1::CollectPhysicsQA(void)
{
  if ( fIsRejectListAlreadyRead ) 
    {
      return "";
    }

  if ( fDataSource.storage("RejectList") == emcManageable::kNone ) 
    {
      return "";
    }

  emcRejectList rl;

  rl.SetSource(fDataSource.storage("RejectList"));

  emcDataManager* dm = emcDataManager::GetInstance();

  string sourcedir = dm->GetSourceDir();
  
  dm->SetSourceDir(fDirectory.c_str());

  bool ok = dm->Read(rl,fTimeStamp);

  if ( ok )
    {
      for ( size_t towerid = 0; towerid < rl.maxsize(); ++towerid ) 
	{
	  fErrorRaw[emcBadModules::kPhysics][towerid] = rl.Error(towerid);
	  fWarnRaw[emcBadModules::kPhysics][towerid] = rl.Warning(towerid);
	}
      fIsRejectListAlreadyRead=true;
      return "";
    }
  else
    {
      string msg = "Could not read Physics QA information from ";
      msg += emcManageable::GetStorageName(fDataSource.storage("QAs"));
      return msg;
    }
}

//_____________________________________________________________________________
string 
emcBadModulesv1::CollectOnlineQA(void)
{
  for ( size_t i = 0; i < NFEMS; i++ ) 
    {
      string msg = CollectOnlineQA(i);
      if ( msg != "" ) 
	{
	  cerr << msg << endl;
	}
    }  
  return "";
}

//_____________________________________________________________________________
string
emcBadModulesv1::CollectOnlineQA(size_t ifem)
{
  string msg = "";

  if ( fDataSource.storage("QAs") == emcManageable::kNone )
    {
      return "";
    }

  emcQAFEM qaFEM(ifem);
  qaFEM.SetSource(fDataSource.storage("QAs"));
  emcDataManager* dm = emcDataManager::GetInstance();
  bool ok = dm->Read(qaFEM, fTimeStamp);
  if ( !ok )
    {
      ostringstream message;
      message << "Cannot collect FEM " << ifem
	      << " from "
	      << emcManageable::GetStorageName(fDataSource.storage("QAs"))
	      << " : using defaults instead";
      emcQAFEM* def = emcQAFEM::Default(ifem);
      qaFEM = *def;
      delete def;
      msg = message.str();
    }

  // Copy information from QAFEM object to our guts.

  for ( size_t i = 0; i < qaFEM.size(); i++) 
    {
      int towerID = EmcIndexer::PXSM144iCH_iPX(ifem,i);
      unsigned int fError, fWarn;
      fError = (static_cast<unsigned int> (qaFEM.getValue(i,0)));
      fWarn  = (static_cast<unsigned int>(qaFEM.getValue(i,1)));
      if(fError&0x1 && fWarn&0x2) {
	fError&= 0xfffffffe;
	fWarn &= 0xfffffffd;

      } 
      fError = 0;
      fWarn = 0;
      fErrorRaw[emcBadModules::kOnline][towerID] = fError;
	//	  static_cast<unsigned int>(qaFEM.getValue(i,0));
      fWarnRaw[emcBadModules::kOnline][towerID]  = fWarn; 
	//	  static_cast<unsigned int>(qaFEM.getValue(i,1));
      
    }

  return msg;
}

//_____________________________________________________________________________
void
emcBadModulesv1::ComputeMaps(const char* sectors)
{
  if ( fDirectory.empty() && 
       ( fDataSource.storage("QAs") != emcManageable::kNone ||
	 fDataSource.storage("RejectList") != emcManageable::kNone) 
       )
    {
      std::cout << "emcBadModulesv1::ComputeMaps : using timestamp="
		<< fTimeStamp << std::endl;
    }

  emcFEMList fems(sectors);

  for (size_t i = 0; i < NFEMS; i++ ) 
    {
      if ( fems.hasFEM(i) )
	{
	  ComputeMaps(i);
	}
    }
}

//_____________________________________________________________________________
void 
emcBadModulesv1::ComputeMaps(size_t ifem)
{
  // Compute the neighbour error and warning flags for each tower
  // in the fem ifem.
  //
  // For amplitude bits are:
  // ---------------------
  // |   | 18| 19| 20|   |
  // ---------------------
  // | 13| 14| 15| 16| 17|
  // ---------------------  ^ y
  // | 8 | 9 | 10| 11| 12|  |
  // ---------------------  |
  // | 3 | 4 | 5 | 6 | 7 |  |
  // ---------------------  ------> z(x)
  // |   | 0 | 1 | 2 |   |
  // ---------------------
  // as viewed from the back of the central tower (which has bit 10 set
  // to 1 if it's itself a bad module); corner towers are excluded
  //
  // For ToF bits are :
  // -------------
  // | 27| 28| 29|  ^ y
  // -------------  |
  // | 24| 25| 26|  |
  // -------------  |
  // | 21| 22| 23|  ------> z(x)
  // -------------
  // as viewed from the back of the central tower (which has bit 25 set
  // to 1 if it's itself a bad module)
  //
  // So, a channel has a problem with amplitude measurements if its neighbor
  // error bit map  satisfies this mask:
  //            0x400
  // Actually, this mask is returned by the IamDeadMask() method
  // so that the amplitude for bad modules can be set to 0 at the calibration
  // stage.
  //
  // Some other useful masks.
  // The mask to look for amplitude errors or warnings in the 3x3 region
  // around a tower is:
  //          0x1ce70
  // In the 5x5 region:
  //         0x1fffff
  // To see if there are ToF problems for this tower:
  //        0x2000000

  if ( fComputedFEMs[ifem] ) return;

  // Be sure all the information we need is there.
  if ( ! fCollectedFEMs[ifem] ) 
    {
      vector<size_t> fems;
      GetListOfNeighbourFEMs(ifem,fems);
      for ( size_t i = 0; i < fems.size(); i++) 
	{
	  if ( ! fCollectedFEMs[ fems[i] ] ) 
	    {
	      Collect(fems[i]);
	    }
	}
    }

  // Mind your steps. In the loop below, only *Fast methods are allowed
  // as the non-Fast method use update() which in turn use this ComputeMaps
  // method (i.e. you would end up in infinite recursion trying to use
  // non-Fast methods here).

  for ( size_t i = 0; i < 144; i++ ) 
    {
      int towerID = EmcIndexer::PXSM144iCH_iPX(ifem,i);
      int sector,x,y;
      EmcIndexer::decodeTowerId(towerID,sector,x,y);
      
      unsigned int neighbourError = 0;
      unsigned int neighbourWarn = 0;
      unsigned int bitOffset = 0;
      
      //
      // Amplitude tests first (in a 5x5 area).
      //
      
      for ( int yoff = -2; yoff <= 2; yoff++ ) {       
	for ( int xoff = -2; xoff <= 2; xoff++ ) {
	  //=====> check if this is a corner tower
	  bool corner=
	    (xoff==-2 && yoff==-2) ||
	    (xoff==-2 && yoff== 2) ||
	    (xoff== 2 && yoff==-2) ||
	    (xoff== 2 && yoff== 2);
	  
	  if (corner) continue;
	  
	  if (!EmcIndexer::IsValid(sector, x+xoff, y+yoff)) 
	    {
	      // physical boundaries (edges)
	      neighbourError |= (1<<bitOffset);
	      neighbourWarn |= (1<<bitOffset);		
	    }
	  else 
	    {
	      int neighbourTower = 
		EmcIndexer::getTowerId(sector,x+xoff,y+yoff);
	      
	      // Errors
	      if ( ( ErrorFast(kPhysics,neighbourTower) & 
		     fMASK_Ampl_Physics ) ||
		   ( ErrorFast(kOnline,neighbourTower ) & 
		     fMASK_Ampl_Online ) ) 
		{
		  neighbourError |= ( 1 << bitOffset );
		}
	      
	      // Warnings
	      if ( ( WarningFast(kPhysics,neighbourTower) & 
		     fMASK_Ampl_Physics ) ||
		   ( WarningFast(kOnline,neighbourTower ) & 
		     fMASK_Ampl_OnlineWarn ) ) 
		{
		  neighbourWarn |= ( 1 << bitOffset );
		}
	    }  	  
	  bitOffset++;
	}
      } // end of ampl tests.
      
      //
      // timing tests then. (in a 3x3 area).
      //
      
      for ( int yoff = -1; yoff <= 1; yoff++ ) {
	for ( int xoff = -1; xoff <= 1; xoff++ ) {
	  
	  if (!EmcIndexer::IsValid(sector, x+xoff, y+yoff)) {
	    // physical boundaries (edges)
	    neighbourError |= (1<<bitOffset);
	    neighbourWarn |= (1<<bitOffset);	    
	  }
	  else {
	    
	    int neighbourTower = EmcIndexer::getTowerId(sector,x+xoff,y+yoff);
	    
	    // Errors
	    if ( ( ErrorFast(kPhysics,neighbourTower) & fMASK_TOF_Physics ) ||
		 ( ErrorFast(kOnline,neighbourTower ) & fMASK_TOF_Online ) ) 
	      {
		neighbourError |= ( 1 << bitOffset );
	      }
	    
	    // Warnings
	    if ( ( WarningFast(kPhysics,neighbourTower) & fMASK_TOF_Physics ) 
		 ||
		 ( WarningFast(kOnline,neighbourTower ) & fMASK_TOF_OnlineWarn 
		   ) ) 
	      {
		neighbourWarn |= ( 1 << bitOffset );
	      }
	  }  	  
	  bitOffset++;
	}
      } // end of timing tests.
      
      fErrorMap[towerID] = neighbourError;
      fWarnMap[towerID] = neighbourWarn;
      
    } // end of loop over towers.
  
  fComputedFEMs[ifem] = true;
}

//_____________________________________________________________________________
unsigned int 
emcBadModulesv1::Error(emcBadModules::EInformationOrigin source, 
		     int towerID)
{
  if ( !IsValid(towerID) ) return 0;
  update(towerID);

  return ErrorFast(source,towerID);
}

//_____________________________________________________________________________
unsigned int
emcBadModulesv1::ErrorFast(emcBadModules::EInformationOrigin source, 
			 int towerID) const
{
  RawMapIterator it;
  it = fErrorRaw.find(source);
  if ( it != fErrorRaw.end() ) 
    {
      return (it->second)[towerID];
    }
  else 
    {
      return 0;
    }
}

//_____________________________________________________________________________
unsigned int
emcBadModulesv1::Deadmap(int towerID)
{
  if ( !IsValid(towerID) ) return 0;
  update(towerID);
  return DeadmapFast(towerID);
}

//_____________________________________________________________________________
unsigned int
emcBadModulesv1::DeadmapFast(int towerID) const
{
  return fErrorMap[towerID];
}

//_____________________________________________________________________________
void
emcBadModulesv1::GetListOfNeighbourFEMs(size_t ifem, vector<size_t>& fems)
{
  int iS;
  int iSM;

  fems.clear();

  EmcIndexer::PXSM144_iSiSM144(ifem, iS, iSM);
  
  EmcIndexer* indexer;
  
  assert(iS>=0 && iS<8);
  
  if (iS<6) 
    {
      indexer = PbScIndexer::buildPbScIndexer();
    }
  else 
    {
      indexer = PbGlIndexer::buildPbGlIndexer();
    }
  
  int xref,yref;
  
  indexer->SMxySM(iSM,xref,yref);
  
  int x,y;
  
  for (x=xref-1;x<=xref+1;x++) {
    for (y=yref-1;y<=yref+1;y++) {
      iSM = indexer->xySMiSM(x,y);
      if (iSM>-1) {
	int sm_phenix_scope = EmcIndexer::iSiSM144_PXSM144(iS, iSM);	
	assert(sm_phenix_scope>=0);
	fems.push_back(static_cast<size_t>(sm_phenix_scope));
      }
    }
  }
}

//_____________________________________________________________________________
void
emcBadModulesv1::identify(std::ostream& os) const
{
  os << "emcBadModulesv1" << endl;
}

//_____________________________________________________________________________
int
emcBadModulesv1::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
bool 
emcBadModulesv1::IsValid(int towerID) const
{
  return ( towerID >=0 && towerID < static_cast<int>(fErrorMap.size()) );
}

//_____________________________________________________________________________
ostream& 
emcBadModulesv1::Print(int towerid, ostream& out)
{
  string head = " TOWID FEM  CH SEC  Z  Y   ERRonl  WARNonl  ERRphys  WARNphys  ERRMAP  WARNMAP";

  size_t rc = 20;
  size_t count = rc;

  if ( towerid == -1 ) 
    {
      for ( size_t i = 0; i < fErrorMap.size(); i++ ) 
	{
	  if ( count == rc )
	    {
	      out << head << endl;
	      count = 0;
	    }
	  PrintOne(i,out);
	  ++count;
	}
    }
  else
    {
      out << head << endl;
      PrintOne(towerid,out);
    }
  return out;
}

//_____________________________________________________________________________
ostream& 
emcBadModulesv1::PrintOne(int towerID, ostream& out)
{
  ostream::fmtflags oldflags = out.flags();

  int femAbsolutePosition, femChannel;
  int sector, z, y;
  
  EmcIndexer::PXPXSM144CH(towerID, femAbsolutePosition, femChannel);
  
  EmcIndexer::decodeTowerId(towerID, sector, z, y );

  // We put those into local variables, just in case
  // some data collection occurs in the methods in the r.h.s. of
  // the expressions below (because in collection some messages can
  // be output on the screen and would screw up this PrintOne method output).
  int errOnl = Error(emcBadModules::kOnline,towerID);
  int warnOnl = Warning(emcBadModules::kOnline,towerID);
  int errPhy = Error(emcBadModules::kPhysics,towerID);
  int warnPhy = Warning(emcBadModules::kPhysics,towerID);
  unsigned int dead = Deadmap(towerID);
  unsigned int warn = Warnmap(towerID);
  
  out << setw(6) << towerID << " " 
      << setw(3) << femAbsolutePosition << " "
      << setw(3) << femChannel << " "
      << setw(3) << sector << " "
      << setw(2) << z << " "
      << setw(2) << y << " ";
  
  out.setf(ostream::showbase);
  out.setf(ostream::hex,ostream::basefield);
  
  out << setw(8) << errOnl << " "
      << setw(8) << warnOnl  << " "
      << setw(8) << errPhy << " "
      << setw(8) << warnPhy << " "
      << setw(8) << dead << " "
      << setw(8) << warn << endl;
  
  out.setf(oldflags);

  return out;
}

//_____________________________________________________________________________
void
emcBadModulesv1::Reset()
{
  Allocate();
  fDataSource.clear();
  fOrigin = emcBadModules::kNone;
  fDirectory = "";
  fTimeStamp.setTics(0);
  fIsRejectListAlreadyRead=false;
}

// //_____________________________________________________________________________
// int 
// emcBadModules::TowerID(int femAbsolutePosition, int femChannel) const
// {
//   return EmcIndexer::PXSM144iCH_iPX(femAbsolutePosition,femChannel);
// }

//_____________________________________________________________________________
void
emcBadModulesv1::update(int towerID)
{
  int ifem, ichannel;
  EmcIndexer::PXPXSM144CH(towerID,ifem,ichannel);
  if ( ifem >= 0 ) 
    {
      if ( !fCollectedFEMs[ifem] || !fComputedFEMs[ifem] )
	{
	  ComputeMaps(ifem);
	}
    }
}

//_____________________________________________________________________________
unsigned int 
emcBadModulesv1::Warning(emcBadModules::EInformationOrigin source, 
		       int towerID)
{
  if ( !IsValid(towerID) ) return 0;
  update(towerID);
  return WarningFast(source,towerID);
}

//_____________________________________________________________________________
unsigned int
emcBadModulesv1::WarningFast(emcBadModules::EInformationOrigin source, 
			   int towerID) const
{
  RawMapIterator it;
  it = fWarnRaw.find(source);
  if ( it != fWarnRaw.end() ) 
    {
      return (it->second)[towerID];
    }
  else 
    {
      return 0;
    }
}

//_____________________________________________________________________________
unsigned int
emcBadModulesv1::Warnmap(int towerID)
{
  if ( !IsValid(towerID) ) return 0;
  update(towerID);
  return WarnmapFast(towerID);
}

//_____________________________________________________________________________
unsigned int
emcBadModulesv1::WarnmapFast(int towerID) const
{
  return fWarnMap[towerID];
}
