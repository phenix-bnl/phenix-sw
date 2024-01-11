#include "MpcMap.h"
#include <phool.h>
#include <RunToTime.hh>
#include <recoConsts.h>

// Database Includes
#include <PHTimeStamp.h>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankID.hh>
#include <PdbCalBank.hh>
#include <PHTimeStamp.h>
#include <PHString.h>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHIODataNode.h>
#include <PdbMpcMap.hh>
#include <PdbMpcPinMap.hh>
#include <RunHeader.h>
#include <getClass.h>

#include <ctime>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <cmath>

typedef PHIODataNode<PHObject> PHObjectNode_t;

using namespace std;

namespace {
const float tower_size = 2.20;
}

MpcMap *MpcMap::__instance = NULL;

MpcMap *MpcMap::instance(PHCompositeNode *topNode)
{
  if (__instance)
    {
      return __instance;
    }

  // instantiate new MpcMap on first call
  __instance = new MpcMap();

  if ( topNode!=0 )
    {
      __instance->AddToNodeTree(topNode);
    }
 
  return __instance;
}

MpcMap::MpcMap(PHCompositeNode *topNode, const int do_download)
{
  status = 0;
  Reset();

  if ( topNode!=0 )
    {
      this->AddToNodeTree(topNode);
    }

  if ( do_download==1 ) Download_Maps(topNode);

  __instance = this;

  edge_fraction = 0.25; //default
  SetEdges(); 
 // put SetEdges in Download_Maps

  SetModules(); 
}

int MpcMap::Download_Maps(PHCompositeNode *topNode)
{
  // check whether MPC_DATABASE is set, if so, we just load the database
  // from the directory specified by MPC_DATABASE
  const char *dbase_dir = getenv("MPC_DATABASE");
  if ( dbase_dir!=NULL )
    {
      string dbase_file(dbase_dir);
      dbase_file += "/MpcCal.map";
      status = Download_Maps(dbase_file);
    }
  else
    {
      int run_number = 0;
      recoConsts *rc = recoConsts::instance();

      // If MPC_DATABASE is not set, we get runnumber from recoConst RUNNUMBER
      // Otherwise, we get it from the RunHeader Object in the node tree.
      if ( rc->FlagExist("RUNNUMBER") )
        {
          run_number = rc->get_IntFlag("RUNNUMBER");
        }
      else if ( topNode!=0 )
        {
          RunHeader *runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
          if (runheader!=0)
            {
              run_number = runheader->get_RunNumber();
              cout << PHWHERE << " Getting RunNumber from RunHeader, " << run_number << endl;
            }
        }

      if ( run_number == 0 )	// final default
        {
          // Download most recent map from database
          cout << PHWHERE << " Warning, didn't find runnumber, downloading most recent map" << endl;
          Download_Maps( PHTimeStamp( time(NULL) ) );
        }
      else
        {
          RunToTime *runtotime = RunToTime::instance();
          PHTimeStamp *timestamp = runtotime->getBeginTime( run_number );
          cout << PHWHERE << " Downloading mpc map from run " << run_number << endl;
          if (timestamp!=0 ) 
            {
              Download_Maps( *timestamp  );
              delete timestamp;
            }
          else
          {
            cout << PHWHERE << " ERROR, This RUN does not exist in the PHTimeStamp database: "
              << run_number << endl;
            cout << "  Defaulting to most recent map. Good luck and caveat emptor..." << endl;
            Download_Maps( PHTimeStamp( time(NULL) ) );
          }
        }
    }

  edge_fraction = 0.25; //default
  SetEdges();

  return 1;
}

MpcMap::~MpcMap()
{
}


void MpcMap::SetEdges()
{
  for(int ich=0;ich<MAXCH;ich++)
    {
      is_edge[ich] = 0;
      is_dead[ich] = 0;
      is_neardead[ich] = 0;
      for(int ix=0;ix<3;ix++){
	for(int iy=0;iy<3;iy++){
	  adjacent_towers[ix][iy][ich] = 0;
	} // iy
      } // ix
    } // ich
      
  for(int ich=0;ich<MAXCH;ich++)
    {
      int gridx, gridy;
      gridx = this->getGridX(ich);
      gridy = this->getGridY(ich);
      int arm = this->getArm(ich);
      
      if(gridx < 0) {
	is_edge[ich] = -1;
	is_dead[ich] = -1;
	is_neardead[ich] = -1;
	for(int ix=0;ix<3;ix++){
	  for(int iy=0;iy<3;iy++){
	    adjacent_towers[ix][iy][ich] = -1;
	  } // iy
	} // ix
	continue;
      } // gridx<0
      
      //now determine edge type
      
      float new_gridr_tot = 0;
      int new_gridr_itr = 0;
      
      for(int ix=-1;ix<=1;ix++){
	for(int iy=-1;iy<=1;iy++){
	  if(ix == 0 && iy == 0) continue; // we are on the central tower
	  
	  int new_gridx = gridx + ix;
	  int new_gridy = gridy + iy;

	  int new_ch = this->getFeeCh(new_gridx,new_gridy,arm);

	  if(new_ch >= 0) continue;  //this means ch exists
	  // later: add !is_dead[new_ch]
	  else{
	    
	
	    new_gridr_tot+= sqrt( (float)((new_gridx-8.5)*(new_gridx-8.5)
					  +(new_gridy-8.5)*(new_gridy-8.5)) );
	    new_gridr_itr++;
	    adjacent_towers[ix+1][iy+1][ich] = 1;
	  }
	
	} //iy
      } //ix
      
      if(new_gridr_itr >0){
	float gridr = sqrt((float)((gridx-8.5)*(gridx-8.5)+(gridy-8.5)*(gridy-8.5)));
	float new_gridr_avg = new_gridr_tot/(float)new_gridr_itr;
	is_edge[ich] = (gridr > new_gridr_avg)?1:2;
      }
	
    } //ich
}

int MpcMap::isEdge(const int fee576ch) const
{
  return is_edge[fee576ch];
}

int MpcMap::isInAcceptance(const float x, const float y, const int gridx, const int gridy, const int arm) const
{ 
  int fee576ch = this->getFeeCh(gridx, gridy, arm);
  return isInAcceptance(x,y,fee576ch);
}

int MpcMap::isInAcceptance(const float x, const float y, const int fee576ch) const
{
  if(fee576ch < 0 || fee576ch >= MAXCH) return 0;
  if(is_edge[fee576ch] == 0) return 1;  //  not an edge
  
  // this can be sped up, 
  // but is not necessary b/c clustering is far more costly
  else if(is_edge[fee576ch] > 0){
    
    int arm = this->getArm(fee576ch);
    
    float tow_x = this->getX(fee576ch);
    float tow_y = this->getY(fee576ch);

    float dx = x - tow_x;
    float dy = y - tow_y;
    
    if(arm == 1) dx = -dx;  //this is b/c d(x)~-d(ix) for north mpc 
    
    for(int ix=-1;ix<=1;ix++){
      for(int iy=-1;iy<=1;iy++){
	if(ix == 0 && iy == 0) continue; // we are on the central tower
	if(adjacent_towers[ix+1][iy+1][fee576ch] == 0) continue; // adjacent tower exists
	else if(adjacent_towers[ix+1][iy+1][fee576ch] == 1) // adjacent tower does not exist
	  {
	    //use center of tower coordinate system
	    float edge_limit = tower_size*(1-edge_fraction) - tower_size/2.0; 
	      //	    float edge_limit = (1.0-edge_fraction*2.0)*(tower_size/2.0); 
	    float upper_y=9999.; float upper_x=9999.; 
	    float lower_y=-9999; float lower_x =-9999.;
	    if(iy > 0){ upper_y = 9999; lower_y = edge_limit;}
	    else if(iy <0){ upper_y = edge_limit; lower_y = -9999;}
	    
	    if(ix > 0){ upper_x = 9999; lower_x = edge_limit;}
	    else if(ix <0){ upper_x = edge_limit; lower_x = -9999;}
	    
	    
	    if(dy < upper_y && dy > lower_y && dx < upper_x && dx > lower_x) return 0; //condition for rejection 
	    
	    //	    float ratio_y = (iy != 0)? dy/((1.0-edge_fraction*2.0)*(tower_size/2.0)*(float)iy):2.0; //The 2.0>1 is necessary for only edge towers
	    //      float ratio_x = (ix != 0)? dx/((1.0-edge_fraction*2.0)*(tower_size/2.0)*(float)ix):2.0;
	    
	    //if(ratio_y > 1 && ratio_x > 1) return 0; // condition for rejection
	  }
	else return 0;
      }
    }
    //made it through everything and hit looks fine
    return 1;
  }
  cout << "Warning...problem w/ acceptance function..\n";
  return 0; //somehow 1st two if statements didn't catch it
}

void MpcMap::SetModules()
{
  for (int ix=0; ix<18; ix++)
    {
      for (int iy=0; iy<18; iy++)
        {
          int arm = 0;	// south
          if ( iy>=13 && ix<=8 ) module_number[arm][ix][iy] = 0;
          else if ( iy>=13 && ix>=9 ) module_number[arm][ix][iy] = 5;
          else if ( iy<=4 && ix<=8 ) module_number[arm][ix][iy] = 2;
          else if ( iy<=4 && ix>=9 ) module_number[arm][ix][iy] = 3;
          else if ( iy>4 && iy<13 )
            {
              if ( ix>=9 ) module_number[arm][ix][iy] = 4;
              else if ( ix<=8 ) module_number[arm][ix][iy] = 1;
            }

          arm = 1;	// north
          if ( iy>=12 && ix<=8 ) module_number[arm][ix][iy] = 6;
          else if ( iy>=12 && ix>=9 ) module_number[arm][ix][iy] = 11;
          else if ( iy<=5 && ix<=8 ) module_number[arm][ix][iy] = 8;
          else if ( iy<=5 && ix>=9 ) module_number[arm][ix][iy] = 9;
          else if ( iy>5 && iy<12 )
            {
              if ( ix>=9 ) module_number[arm][ix][iy] = 10;
              else if ( ix<=8 ) module_number[arm][ix][iy] = 7;
            }
        }
    }
}

int MpcMap::isModuleEdge(const int ifee576ch) const
{
  int arm = getArm( ifee576ch );
  int ix = getGridX( ifee576ch );
  int iy = getGridY( ifee576ch );

  if ( ix==8 || ix==9 ) return 1;
  if ( arm==0 )
    {
      if (iy==4 || iy==5 || iy==12 || iy==13) return 1;
      else return 0;
    }
  else if ( arm==1 )
    {
      if (iy==5 || iy==6 || iy==11 || iy==12) return 1;
      else return 0;
    }

  return 0;
}

int MpcMap::getPinFeeCh(const std::string pin_name)
{
  int pin_ch=-9999;

  // numbers are hardcoded from:
  // https://www.phenix.bnl.gov/WWW/offline/wikioffline/index.php/MPC_Pin
  if ( pin_name=="STL" || pin_name=="stl") pin_ch=60;
  else if ( pin_name=="SML" || pin_name=="sml") pin_ch=61;
  else if ( pin_name=="SBL" || pin_name=="sbl") pin_ch=62;
  else if ( pin_name=="STR" || pin_name=="str") pin_ch=86;
  else if ( pin_name=="SMR" || pin_name=="smr") pin_ch=85;
  else if ( pin_name=="SBR" || pin_name=="sbr") pin_ch=84;

  else if ( pin_name=="NTL" || pin_name=="ntl") pin_ch=344;
  else if ( pin_name=="NML" || pin_name=="nml") pin_ch=345;
  else if ( pin_name=="NBL" || pin_name=="nbl") pin_ch=346;
  else if ( pin_name=="NTR" || pin_name=="ntr") pin_ch=370;
  else if ( pin_name=="NMR" || pin_name=="nmr") pin_ch=368;
  else if ( pin_name=="NBR" || pin_name=="nbr") pin_ch=369;

  else {
    cout << "MpcMap::getPinFeeCh did not recognize request: " << pin_name << endl;
    cout << "Usage: <N.orth/S.outh><T.op/M.iddle/B.ottom><L.eft/R.ight>" << endl;
    cout << "Example: North Middle Right = \"NMR\" or \"nmr\"" << endl;
  }
  
  return pin_ch;
}

int MpcMap::Download_Maps(const PHTimeStamp& tstamp)
{
  Reset();

  // Download from the database
  PdbBankManager *bankManager = PdbBankManager::instance();
  if ( bankManager==0 )
    {
      cout << PHWHERE << " Failed to get bankManager" << endl;
      return -1;
    }
  
  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();
  if ( application==0 )
    {
      cout << PHWHERE << " Failed to get PdbApplication" << endl;
      return -1;
    }
  
  if ( application->startRead() )
    {
      PHTimeStamp tSearch = tstamp;
      PdbBankID bankID;
      PHString classname[2]={"PdbMpcMapBank","PdbMpcPinMapBank"};
      PHString calibname[2]={"mpcmap","mpcpinmap"};

      for (int ibank=0; ibank<2; ibank++)
	{
	  bankID.setInternalValue(0);

	  PdbCalBank *mpcBank = bankManager->fetchBank(classname[ibank].getString(),
						       bankID,
						       calibname[ibank].getString(),
						       tSearch);
	  if (mpcBank)
	    {
	      mpcBank->printHeader();
	      StartTime = mpcBank->getStartValTime();
	      EndTime = mpcBank->getEndValTime();
	      for (unsigned int ich = 0; ich < mpcBank->getLength(); ich++)
		{
		  if ( ibank==0 ) 
		    {
		      mapdata[ich] = (PdbMpcMap &)mpcBank->getEntry(ich);
		    }
		  else if ( ibank==1 )
		    {
		      pindata[ich] = (PdbMpcPinMap &)mpcBank->getEntry(ich);
		    }
		  else 
		    {
		      cout << "main()" << endl
			   << "\tError:" << endl
			   << "\tibank overlimit" << endl;
		      return 1;
		    }
		}

	      delete mpcBank;
	    }
	  else
	    {
	      cout << "main()" << endl
		   << "\tError:" << endl
		   << "\tbankManager returned zero-pointer for " << classname[ibank] << endl;
	      return 1;
	    }
	  application->commit();
	}
    }

  else
    {
      application->abort();
      cout << PHWHERE << " Transaction aborted." << endl;
      return 1;
    }

  MakeInverseMap();
  status = 0;
  return 0;
}

int MpcMap::Download_Maps(const string& dbase_textfile)
{
  Reset();

  ifstream infile( dbase_textfile.c_str() );
  char header[BUFSIZ];
  infile.getline(header,BUFSIZ);
  if ( !infile.is_open() )
    {
      cout << PHWHERE << " unable to open " << dbase_textfile << endl;
      status = -3;
      return status;
    }
  else
    {
      cout << PHWHERE << " Reading mpc map data from " << dbase_textfile << endl;
    }

  int fee576ch;
  int temp_driver;
  int temp_gridx, temp_gridy;
  int temp_pinfee576ch;
  float temp_x, temp_y, temp_z;

  while ( infile >> fee576ch >> temp_driver >> temp_gridx >> temp_gridy >> temp_pinfee576ch >> temp_x >> temp_y >> temp_z )
    {
      /*
      cout << "\t" <<  fee576ch << "\t" <<  temp_driver << "\t" 
	   <<  temp_gridx << "\t" <<  temp_gridy << "\t" 
	   << temp_pinfee576ch << "\t" 
	   <<  temp_x << "\t" <<  temp_y << "\t" <<  temp_z << endl;
      */

      // Removed negative gridx/gridy check because of pin diode channels
      // Added a negative driver check
      //if ( temp_gridx<0 || temp_gridy<0 ) continue;
      if ( temp_driver < 1 ) continue;

      mapdata[fee576ch].set(fee576ch,temp_driver,temp_gridx,temp_gridy,temp_x,temp_y,temp_z);
      pindata[fee576ch].set(fee576ch,temp_pinfee576ch);
    }

  infile.close();

  MakeInverseMap();
  status = 0;
  return status;
}

int MpcMap::StoreInDatabase(PHTimeStamp& tStart, const char *username, const char *description,const PHTimeStamp& tStop)
{
  PdbBankManager *bankManager = PdbBankManager::instance();
  if ( bankManager==0 )
    {
      cout << PHWHERE << " Failed to get bankManager" << endl;
      return -1;
    }
  PdbApplication *application = bankManager->getApplication();
  if ( application==0 )
    {
      cout << PHWHERE << " Failed to get PdbApplication" << endl;
      return -1;
    }

  cout << "Opening FD in update mode.." << endl;
  if (application->startUpdate())
    {
      //PHTimeStamp tStop = PHTimeStamp(2020, 1, 1, 0, 0, 0);	// canonical end of run time
      PdbBankID bankID;

      const char *classname[2] = {"PdbMpcMapBank","PdbMpcPinMapBank"};
      const char *calibname[2] = {"mpcmap","mpcpinmap"};

      PdbCalBank *prevBank;
      for (int ibank=0; ibank<2; ibank++)
	{
	  cout << " ibank: " << ibank << endl;
	  cout << "prevBank = bankManager->fetchBank("<<classname[ibank]<<", bankID, "<<calibname[ibank]<<", tStart);"<<endl;
	  prevBank = bankManager->fetchBank(classname[ibank], bankID, calibname[ibank], tStart);
	  cout << "got previous bank" << endl;
	  if (prevBank)
	    {
	      cout << " overlapping " << classname[ibank] << " found. Changing the EndValTime of it " << endl;
	      //tStop = prevBank->getEndValTime();
	      prevBank->setEndValTime(tStart);
	      //cout << " and setting current EndValTime to " << tStop << endl;
	      delete prevBank;
	    }

	  // Inputs for calibration bunk header informations.
	  PHString Description;
	  if ( strlen(description) == 0 )
	    {
	      char tempdescription[240];
	      cout << "Please input description for this calibration parameter:" << endl;
	      cin.getline(tempdescription, 240);
	      Description = tempdescription;
	    }
	  else
	    {
	      Description = description;
	    }

	  PHString UserName;
	  if ( strlen(username) == 0 )
	    {
	      char tempname[20];
	      cout << "Please enter your name:" << endl;
	      cin.getline(tempname, 20);
	      UserName = tempname;
	    }
	  else
	    {
	      UserName = username;
	    }

	  // why do we always use internal value 0?
	  bankID.setInternalValue(0);

	  PdbCalBank *mpcBank = bankManager->createBank(classname[ibank],
							   bankID,
							   Description.getString(),
							   tStart,
							   (PHTimeStamp&)tStop,
							   calibname[ibank]);

	  mpcBank->setLength(576);
	  mpcBank->setUserName(UserName);
	  
	  for (unsigned int ich = 0; ich < mpcBank->getLength(); ich++)
	    {
	      if ( ibank==0 )
		{
		  PdbMpcMap* entry = (PdbMpcMap*) &(mpcBank->getEntry(ich));
		  *entry = mapdata[ich];
		}
	      else if ( ibank==1 )
		{
		  PdbMpcPinMap* entry = (PdbMpcPinMap*) &(mpcBank->getEntry(ich));
		  *entry = pindata[ich];
		}

	      else 
		{
		  cout << "main()" << endl
		       << "\tError:" << endl
		       << "\tibank overlimit, exitting" << endl;
		  return 1;
		}
	    }
	  application->commit();
	} // end ibank

    }
  else
    {
      cout << PHWHERE << "failed to start dbase application for update" << endl;
      return 1;
    }

  return 1;

}

void MpcMap::MakeInverseMap()
{
  // now generate inverse maps
  for (int ifee576ch=0; ifee576ch<576; ifee576ch++)
    {
      int igridx = getGridX(ifee576ch);
      int igridy = getGridY(ifee576ch);
      if ( igridx<0 || igridy<0 ) continue;

      if ( ifee576ch<288 )
        {
          south_feech_from_gridxy[igridx][igridy] = ifee576ch;
        }
      else
        {
          north_feech_from_gridxy[igridx][igridy] = ifee576ch;
        }
    }
}

void MpcMap::Reset()
{ 
  for (int ifee576ch=0; ifee576ch<MAXCH; ifee576ch++)
    {
      mapdata[ifee576ch].Reset();
      mapdata[ifee576ch].set_fee576(ifee576ch);
      pindata[ifee576ch].set_fee576(ifee576ch);
    }

  // reset inverse map
  for (int ix=0; ix<18; ix++)
    {
      for (int iy=0; iy<18; iy++)
        {
          south_feech_from_gridxy[ix][iy] = -1;
          north_feech_from_gridxy[ix][iy] = -1;
        }
    }
}

void MpcMap::Print(Option_t*) const
{ 
  for (int ifee576ch=0; ifee576ch<MAXCH; ifee576ch++)
    {
      mapdata[ifee576ch].print();
      pindata[ifee576ch].print();
    }
}

void MpcMap::Dump_to_file(const string& outfname)
{
  string full_outfname = outfname;
  full_outfname.append(".");
  full_outfname.append(getStartTime()->formatTimeString());
  full_outfname.append("-");
  full_outfname.append(getEndTime()->formatTimeString());

  ofstream outfile(full_outfname.c_str());

  outfile << "#fee576\tdriver\tgridx\tgridy\tpinfee576\tx\ty\tz" << endl;
  for (int ifee576ch=0; ifee576ch<MAXCH; ifee576ch++)
    {
      int fee576ch = mapdata[ifee576ch].get_fee576();
      // sanity check
      if ( fee576ch != ifee576ch ) cout << PHWHERE << " ERROR " << fee576ch << " != " << ifee576ch << endl;

      int driver = mapdata[ifee576ch].get_driver();
      int gridx = mapdata[ifee576ch].get_gridx();
      int gridy = mapdata[ifee576ch].get_gridy();
      int pinfee576 = pindata[ifee576ch].get_pinfee576();
      float x = mapdata[ifee576ch].get_x();
      float y = mapdata[ifee576ch].get_y();
      float z = mapdata[ifee576ch].get_z();

      if ( driver==-9999 ) continue;

      outfile << fee576ch << "\t"
              << driver << "\t"
              << gridx << "\t"
              << gridy << "\t"
	      << pinfee576 << "\t" 
              << x << "\t"
              << y << "\t"
              << z << endl;
    }

  outfile.close();
}

void MpcMap::AddToNodeTree(PHCompositeNode *topNode)
{
  PHNodeIterator mapnode_iter(topNode);
  PHCompositeNode *mapNode = dynamic_cast<PHCompositeNode*>(mapnode_iter.findFirst("PHCompositeNode", "MpcMap"));
  if ( mapNode!= 0 )	// node already exists
    {
      cout << PHWHERE << " MpcMap Node already exists" << endl;
      return;
    }

  PHCompositeNode *mpcNode;
  PHNodeIterator iter(topNode);
  mpcNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "MPC"));
  if (!mpcNode)	// create mpcNode
    {
      mpcNode = new PHCompositeNode("MPC");
      topNode->addNode(mpcNode);
    }

  //PHObjectNode_t *MpcMapNode = new PHObjectNode_t(__instance, "MpcMap", "PHObject");
  PHObjectNode_t *MpcMapNode = new PHObjectNode_t(this, "MpcMap", "PHObject");
  mpcNode->addNode(MpcMapNode);
}

int MpcMap::IsValid(const int verbosity) const
{
  if ( verbosity>0 && status<=0 )
    {
      if ( status == -1 )
        {
          cout << PHWHERE << "Please set environment variable MPC_DATABASE" << endl;
        }
      else if ( status == -2 )
        {
          cout << PHWHERE << "Database not yet implemented" << endl;
        }
      else
        {
          cout << PHWHERE << "Unknown status " << status << endl;
        }
    }

  return status;
}

