/*!
  \author Jamie Nagle
*/

#include <iostream>
#include "EmcIndexer.h"
#include "PHGeometry.h"
#include "CrkGeometryObject.hh"
#include "RunNumberRanges.h"

#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PHTimeStamp.h"
#include "PdbBankID.hh"
#include "PdbCalBank.hh"
#include "PdbErtLutEmcTile.hh"

// for database access
#include <odbc++/connection.h>
#include <odbc++/setup.h>
#include <odbc++/errorhandler.h>
#include <odbc++/drivermanager.h>
#include <odbc++/resultset.h>
#include <odbc++/resultsetmetadata.h>
#include <odbc++/preparedstatement.h>
#include <odbc++/databasemetadata.h>
#include <odbc++/types.h>

#include <cstdlib>
#include <iostream>
#include <sstream>
#include <string>

using namespace std;
using namespace odbc;

#include "ErtUtils.h"

//=====================================================================================

bool ErtUtils::is_included_in_ERT_LUT(int runnumber, int arm, int EMC_TrigTile, int RICH_TrigTile, float mom) {

  // check if index is out of range for this convention
  if(arm<0 || arm>1 || EMC_TrigTile<0 || EMC_TrigTile>=128 || RICH_TrigTile<0 || RICH_TrigTile>=128) {
    cout << "ErtUtils::is_included_in_LUT ERROR - index passed in out of range.  Check convention (arm=" 
	 << arm << " EMC_TrigTile=" << EMC_TrigTile << " RICH_TrigTile=" << RICH_TrigTile << endl;
    return false;
  }

  int a[21] = {21*0};
  get_ERT_RICH_LUT(runnumber, arm, EMC_TrigTile, a, mom);
  if(a[0]==0) return false;
  for(int i=1; i<=a[0]; i++)
    {
      if(RICH_TrigTile==a[i]) return true;
      //      if(RICH_TrigTile<a[i]) return false;
    }
  return false;
}


//=====================================================================================

int ErtUtils::get_lut_bankid(int runnumber)
{
  if (runnumber<BEGIN_OF_RUN9)
    return 2000; // old LUT CM++

  //  Cutting and past from trigger utilities code
   //  Establish connection to Postgres...

  Connection *con = 0;
  try
    {
      con = DriverManager::getConnection("daq", "phnxrc", "");
    }
  catch (SQLException& e)
    {
      cout << PHWHERE << " Exception caught during DriverManager::getConnection" ;
      cout << "Message: " << e.getMessage() ;
      if (con)
        {
          delete con;
        }
      return -1;
    }

  Statement *stmt = con->createStatement();

  ostringstream msg;
  msg.str("");
  msg << "select electronalgideast, electronalgidwest from run where runnumber=" << runnumber << " limit 1" ;

  ResultSet *rs = 0;
  try
    {
      rs = stmt->executeQuery(msg.str().c_str());
    }
  catch (SQLException& e)
    {
      cout << PHWHERE << "Exception caught"
	  << " Message: " << e.getMessage() ;
      delete stmt;
      delete con;
      return -1;
    }

  if (!rs->next())
    {
      cout << PHWHERE << "Error retrieving configuration for run."
	   << runnumber ;
      delete rs;
      delete stmt;
      delete con;
      return -1;
    }

  int idE = rs->getInt("electronalgideast");
  int idW = rs->getInt("electronalgidwest");

  delete rs;
  delete stmt;
  delete con;

  if (idE!=idW)
    return -1;
  else
    return idE;
}

//=====================================================================================

void ErtUtils::get_ERT_RICH_LUT(int runnumber, int arm, int EMC_TrigTile, int *aLUT, float mom)
{
  static int last_loaded_runnumber = 0;

  static PdbErtLutEmcTile *ertlutemctile[256];
  
  if (runnumber!=last_loaded_runnumber)
    {
      last_loaded_runnumber = runnumber;
      int bank_id = get_lut_bankid(runnumber);

      PdbBankManager *bankManager = PdbBankManager::instance();
      PdbApplication *application = bankManager->getApplication ();

      if (application->startRead ())
	{
	  PHTimeStamp tSearch = PHTimeStamp (2008,12,31, 0, 0, 0);  // does not matter
	  PdbBankID bankID;
	  bankID.setInternalValue (bank_id);
	  
	  PdbCalBank *ertlutbank = 0;
	  ertlutbank =  bankManager->fetchBank ("PdbErtLutEmcTileBank", bankID,"ertlut", tSearch);
	  if(ertlutbank)
	    {
	      int length = ertlutbank->getLength();
	      for (int i = 0; i < length; i++)
		{
		  ertlutemctile[i] = (PdbErtLutEmcTile *) & (ertlutbank->getEntry (i));
		}
	    }
	  else
	    {
	      cout << "Failed to read bank, quit!" << endl;
	      exit(-1);
	    }
	}
    }

  // actual algorithm here...

  int num = 128*(1-arm)+EMC_TrigTile; // different numbering
  if(arm<0 || arm>1 || EMC_TrigTile<0 || EMC_TrigTile>=128)
    {
      aLUT[0] = -1;
    }
  else
    {
      if (!ertlutemctile[num]) return;
      aLUT[0] = ertlutemctile[num]->getNumberAssociated();
      int nassoc_mom = 0;
      for(int i= 0; i<=aLUT[0]; i++)
	{
	  if (ertlutemctile[num]->getAssociatedMaxMom(i)<mom) continue;
	  aLUT[nassoc_mom+1] = ertlutemctile[num]->getAssociatedRichTile(i);
	  nassoc_mom++;
	}
      aLUT[0] = nassoc_mom;
    }
}

//=====================================================================================

int ErtUtils::get_EMC_TrigTile_FromsmID(int arm, int sect, int sm) {

  int local_num = (arm*4+sect)*32+sm;
  if(local_num<0 || local_num>=256) return -1;
  static const int EMC_TrigTile[256] = 
    {
      //West
      4, 5, 6, 0, 1, 2,
      7, 12, 13, 3, 8, 9,
      14, 15, 20, 10, 11, 16,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
      36, 37, 38, 32, 33, 34, 
      39, 44, 45, 35, 40, 41,
      46, 47, 52, 42, 43, 48,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
      68, 69, 70, 64, 65, 66,
      76, 77, 78, 72, 73, 74,
      84, 85, 86, 80, 81, 82,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
      100, 101, 102, 96, 97, 98,
      108, 109, 110, 104, 105, 106,
      116, 117, 118, 112, 113, 114,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
      //East
      0, 1, 2, 3, 4, 5, 6, 7, 
      8, 9, 10, 11, 12, 13, 14, 15,
      16, 17, 18, 19, 20, 21, 22, 23,
      24, 25, 26, 27, 28, 29, 30, 31,
      32, 33, 34, 35, 36, 37, 38, 39,
      40, 41, 42, 43, 44, 45, 46, 47,
      48, 49, 50, 51, 52, 53, 54, 55,
      56, 57, 58, 59, 60, 61, 62, 63,
      64, 65, 66, 68, 69, 70,
      72, 73, 74, 76, 77, 78,
      80, 81, 82, 84, 85, 86,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
      96, 97, 98, 100, 101, 102,
      104, 105, 106, 108, 109, 110,
      112, 113, 114, 116, 117, 118,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
    };
  return EMC_TrigTile[local_num];

}

//=====================================================================================

int ErtUtils::get_RICH_TrigTile_FromsmID(int arm, int sect, int sm) {

  if(arm==0) return sect*32+7-sm%8+8*(sm/8); //West
  else       return sect*32+sm; //East

}

//=====================================================================================
//=====================================================================================

int ErtUtils::get_EMC_Towerkey_FromIndex(int arm, int sector, int iy, int iz) {
  return (100000 * arm + 10000 * sector + 100 * iy + iz);
}

//=====================================================================================

bool ErtUtils::get_EMC_smID_FromTowerkey(int towerkey, int &arm, int &sector, int &smID) {

  // j.nagle - there must be a more direct way of doing this mapping between detector index values

  int moduleID = ErtUtils::get_EMC_Module_FromTowerkey(towerkey);
  if(moduleID==-1) return false;

  int emctt = ErtUtils::get_EMC_TrigTile_FromModule(moduleID);
  if(emctt==-1) return false;

  ErtUtils::get_EMC_smID_FromTrigTile(arm, sector,smID, emctt);
  
  return true;

}

//=====================================================================================

int ErtUtils::get_EMC_Module_FromTowerkey(int towerkey) {

  int iz=towerkey%100;
  towerkey/=100;
  int iy=towerkey%100;
  towerkey/=100;
  int sector=towerkey%10;
  towerkey/=10;
  int arm=towerkey;

  if (0==arm && sector<4 && iy<36 && iz<72)
            return (int) iz/2+(iy/2)*36+sector*648;

  if (1==arm && sector<2 && iy<48 && iz<96) 
            return (int) iz/2+(iy/2)*48+sector*1152+2592;

  if (1==arm && sector<4 && iy<36 && iz<72)
            return (int) iz/2+(iy/2)*36+(sector-2)*648+4896;

  return -1;
}

//=====================================================================================

int ErtUtils::get_EMC_Module_FromTowerID(int towerid) 
{

  int arm, sector, iy, iz;
  EmcIndexer::TowerLocation(towerid,arm,sector,iy,iz);

  //EMC West PbSc
  if (0==arm && sector<4 && iy<36 && iz<72)
  return (int) iz/2+(iy/2)*36+sector*648;
   
  //EMC East PbGl 
  if (1==arm && sector<2 && iy<48 && iz<96)
  return (int) iz/2+(iy/2)*48+sector*1152+2592;

  //EMC East PbSc 
  if (1==arm && sector<4 && iy<36 && iz<72)
  return (int) iz/2+(iy/2)*36+(sector-2)*648+4896;

  return -1;
}

//=====================================================================================

int ErtUtils::get_EMC_TrigTile_FromModule(int moduleid) {

  if (moduleid>=0 && moduleid<2592) 
    return (int) (35-moduleid%36)/6+((moduleid/36)/6)*6;

  if (moduleid>=2592 && moduleid<4896) { 
    moduleid-=2592;
    return (int) (moduleid%48)/6-((moduleid/48)/6)*8+164;
  }
      
  if (moduleid>=4896 && moduleid<6192) {
    moduleid-=4896;
    return (int) (moduleid%36)/6-((moduleid/36)/6)*6+102;
  }

  return -1;
}

//=====================================================================================

void ErtUtils::get_EMC_smID_FromTrigTile(int &arm, int &sector, int &smID, int emctt) { 

   if(emctt<72) {//...west arm

      if(emctt>=0  && emctt<18)        sector = 0; 
      else if (emctt>=18 && emctt<36)  sector = 1;
      else if (emctt>=36 && emctt<54)  sector = 2;
      else if (emctt>=54 && emctt<72)  sector = 3;

      arm = 0; //.. west arm 

      smID = ((emctt - 18*sector)/6 + 1)*6 - (emctt - 18*sector)%6 - 1;   

   } else if(emctt<172) { //.. east arm

     if(emctt>=72 && emctt<90)          sector = 3;
     else if(emctt>=90 && emctt<108)    sector = 2;
     else if(emctt>=108 && emctt<140)   sector = 1;
     else if(emctt>=140 && emctt<172)   sector = 0;

     arm = 1; //.. east arm 

     if(sector<2) {
        
         smID = (emctt - 140 + sector*32) + 24 - (emctt - 140 + sector*32)/8*16;

     } else if(sector <4) {

         smID = (emctt - 126 + sector*18) + 12 - (emctt - 126 + sector*18)/6*12;
        
     } else {
        cout<<"ErtUtils::get_EMC_smID_FromTrigTile ERROR::: +++  sector should not >= 4 , something is wrong +++ "<<endl;
     }

   } else {
      cout<<"ErtUtils::get_EMC_smID_FromTrigTile ERROR::: ++++ emctt = "<<emctt<<" goes beyond the range"<<endl; 
   }

}

//=====================================================================================

void ErtUtils::get_RICH_smID_FromPMT(int pmt, int &armRICH, int &sectRICH,int &smRICH) {
  // j.nagle - there must be a more direct way to do this conversion
  int crktt=  ErtUtils::get_RICH_TrigTile_FromPMT(pmt);
  ErtUtils::get_RICH_smID_FromTrigTile(armRICH, sectRICH, smRICH, crktt);
}

//=====================================================================================

int ErtUtils::get_RICH_TrigTile_FromPMT(int pmt) {

  int crktt;
  int crk_sector = pmt/1280;
  int npmt = pmt%1280;
  
  if (0==crk_sector) {
    crktt=(15-npmt%16)/4+((npmt/16)/5)*8;
  }
  else if (1==crk_sector) {
    crktt=(npmt%16)/4+((npmt/16)/5)*8+4;
  }
  else if (2==crk_sector) {
    crktt=(15-npmt%16)/4+((79-npmt/16)/5)*8+128;
  }
  else if (3==crk_sector) {
    crktt=(npmt%16)/4+((79-npmt/16)/5)*8+132;
  }
  else
    {
      cout << PHWHERE << "invalid pmt: " << pmt << endl;
      exit(1);
    }
  return(crktt);
}

//=====================================================================================

void ErtUtils::get_RICH_smID_FromTrigTile(int &arm, int &sector, int &smID, int crktt) {

  if(crktt<128) { //.. west arm
    arm = 0;
    sector = crktt/32;
    smID = 8*((2*(crktt/8)+1)-4*sector)-1 - crktt;
  } else if (crktt<256) { //.. east arm
    arm = 1;
    sector = 7 - crktt/32;
    int tmp = crktt - 104 - (3-sector)*32;
    smID = tmp - (tmp/8 - 3)*16;
  } else {
    cout<<"ErtUtils::get_RICH_smID_FromTrigTile ERROR:::  crktt = "<<crktt<<" goes beyond range "<<endl;
  }
}

//=====================================================================================

void ErtUtils::get_EMC_ArmSectSide_FromTrigTile(int &arm,int &sect,int &side,int emctt) {

  if(emctt <72) {//.. west arm
    arm = 0;
    sect = emctt/18;
    if( (emctt%6) <= 2  ){ side = 0; } // south side
    else { side = 1; }// north side
  } else {//.. east arm
    arm = 1;
    if(emctt<90) sect = 3;
    if(emctt>=90  && emctt<108) sect = 2;
    if(emctt>=108 && emctt<140) sect = 1;
    if(emctt>=140 && emctt<172) sect = 0;

    if(sect>=2) {
      if( (emctt%6) <= 2  ){ side = 0; }// south side
      else { side = 1; }// north side
    } else {
      if( ((emctt - 108)%8) <= 3 ){ side = 0; }// south side
      else { side = 1; }// north side
    }
  }
}

//=====================================================================================

void ErtUtils::get_RICH_ArmSectSide_FromTrigTile(int &arm,int &sect,int &side,int crktt) {

  if(crktt < 128) { //.. west arm
    arm = 0;
    sect = crktt/32;
  } else { //.. east arm
    arm = 1;
    sect = 7 - crktt/32;
  }

  if( (crktt%8) <= 3  ){ side = 0; }// south side
  else { side = 1; }// north side
}

//=====================================================================================

void ErtUtils::get_EMC_Module_FromTrigTile(int emctt,int *moduleids) {

  int moduleid0 = 0;
  int nrow      = 0;

  if(emctt>=0 && emctt<72)//West PbSc 
  {
    nrow=36;
    moduleid0=(5-emctt%6)*6+(emctt/6)*216;
  }
  if (emctt>=72 && emctt<108)//East PbSc
  {
    nrow=36;
    moduleid0=(17-emctt/6)*216+(emctt%6)*6+4896;
  }
  if (emctt>=108 && emctt<172)//East PbGl 
  {
    nrow=48;
    moduleid0=((emctt+4)%8)*6+(20-(emctt-4)/8)*288+2592;
  }

  for(int i=0;i<6;i++){//iz direction
    for(int j=0;j<6;j++){//iy direction
      moduleids[i+6*j]=moduleid0+i+nrow*j;
    }
  }
}

//=====================================================================================

void  ErtUtils::get_EMC_Module_Neighbors(int moduleID,int *neighbour) {

  neighbour[0]=moduleID;
  neighbour[1]=neighbour[2]=neighbour[3]=-1;

  if(moduleID>=0 && moduleID<2592){
    bool notup=((moduleID/36+1)%18);
    bool notleft=(moduleID%36);
    if(notleft){ neighbour[1]=moduleID-1; }
    if(notup && notleft){ neighbour[2]=moduleID+35; }
    if(notup){ neighbour[3]=moduleID+36; }
    return;
  }

  if(moduleID>=2592 && moduleID<4896){
    bool notup=(((moduleID-2592)/48+1)%24);
    bool notleft=((moduleID-2592)%48);
    if(notleft){ neighbour[1]=moduleID-1; }
    if(notup && notleft){ neighbour[2]=moduleID+47; }
    if(notup){ neighbour[3]=moduleID+48; }
    return;
  }

  if(moduleID>=4896 && moduleID<6192){
    bool notup=(((moduleID-4896)/36+1)%18);
    bool notleft=((moduleID-4896)%36);
    if(notleft){ neighbour[1]=moduleID-1; }
    if(notup && notleft){ neighbour[2]=moduleID+35; }
    if(notup){ neighbour[3]=moduleID+36; }
    return;
  }
}

//=====================================================================================

int ErtUtils::get_RICH_TrigTile_Neighbors(int crktt,int *crkneighbors) {

  if (crktt<0 || crktt>=256) return 0;

  int sector=(crktt%8)/4+(crktt/128)*2;
  crkneighbors[0]=crktt;
  crkneighbors[1]=crkneighbors[2]=crkneighbors[3]=-1;
  int nneighbors=0;
  int coord[2][4]={{1,-1,1,-1},{8,8,-8,-8}};
  bool nottop=(crktt<120 || crktt>135);
  bool notedgecol=(crktt-3)%8 && (crktt-4)%8;

  if (nottop) {
    crkneighbors[3]=crktt+coord[1][sector];
    nneighbors++;
  }
  if (notedgecol) {
    crkneighbors[1]=crktt+coord[0][sector];
    nneighbors++;
  }
  if (notedgecol && nottop) {
    crkneighbors[2]=crktt+coord[0][sector]+coord[1][sector];
    nneighbors++;
  }
  return nneighbors;
}

//=====================================================================================
int ErtUtils::get_RICH_pmt_fromcrossing(int emc_arm, float cross_phi, float cross_z)
{
  //.. get pmt ID associated with the track

  static bool first_time = true;
  
  static float xcrk[5120];
  static float ycrk[5120];
  static float zcrk[5120];
   
  if (first_time)
    {
      //this is the fastest way to retrieve Crk geometry.CLS
      CrkGeometryObject* d_cgo = new CrkGeometryObject;
      for(int pmtid = 0; pmtid<5120; pmtid++)
	{
	  PHPoint pmt_pos = d_cgo->GetPmtPosition(d_cgo->IdToArm(pmtid),
						  d_cgo->IdToSide(pmtid),
						  d_cgo->IdToSm(pmtid),
						  d_cgo->IdToPmt(pmtid));
	  xcrk[pmtid] = pmt_pos.getX();
	  ycrk[pmtid] = pmt_pos.getY();
	  zcrk[pmtid] = pmt_pos.getZ();
	}
      delete d_cgo;
      first_time = false;
    }

  
   if(cross_phi<-8000 || cross_z<-8000) return -1;

   int pmt_min = -1;
   float dist_min = 9999;

   PHPoint pmt_cross (265*cos(cross_phi), 265*sin(cross_phi), cross_z);

   int pmt_start = -1, pmt_end = -1;
   if(emc_arm==0) {//.. west arm
      if(cross_z<0) {
         pmt_start = 0;
         pmt_end = 1280;
      } else {
         pmt_start = 1280;
         pmt_end = 2560;
      }
   } else if(emc_arm==1) {
      if(cross_z<0) {
         pmt_start = 2560;
         pmt_end = 3840;
      } else {
         pmt_start = 3840;
         pmt_end = 5120;
      }
   }

   for(int pmtid = pmt_start; pmtid<pmt_end; pmtid++) {
     PHPoint pmt_pos(xcrk[pmtid], ycrk[pmtid], zcrk[pmtid]);
     float dist = PHGeometry::distancePointToPoint(pmt_pos, pmt_cross);
     if(dist<dist_min) {
       dist_min = dist;
       pmt_min = pmtid;
     }
   }
   
   return pmt_min;
}
