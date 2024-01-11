#include "MpcExMapper.h"
#include "MpcExConstants.h"
#include "PHTimeStamp.h"
#include "RunToTime.hh"
#include "RunHeader.h"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbBankID.hh"
#include "PdbCalBank.hh"
#include "PdbMpcExCoordinateMap.hh"
#include "recoConsts.h"
#include "Fun4AllServer.h"
#include "PHCompositeNode.h"
#include "getClass.h"
#include <fstream>
#include <string>
#include <iostream>

using namespace MpcExConstants;

MpcExMapper* MpcExMapper::_instance = NULL;

MpcExMapper::MpcExMapper(){

  //first try the text file
  char *filename = getenv("MPCEXMAPPERFILE");

  if (  filename )
    {
      std::ifstream csvfile(filename);

      std::string header;
      std::getline(csvfile,header);
      int arm, packet, chipmap, nx, ny, layer, topbottom, chain, lx, ly, chip, quadrant, qsensor;
      float x, y, z;
      while(csvfile>>arm>>packet>>chipmap>>nx>>ny>>x>>y>>z>>layer>>topbottom>>chain>>lx>>ly>>chip>>quadrant>>qsensor)
	{
	  MinipadCoordinate *minipad = new MinipadCoordinate();
	  minipad->arm = arm;
	  minipad->packet = packet;
	  minipad->chipmap = chipmap;
	  minipad->nx = nx;
	  minipad->ny = ny;
	  minipad->x = x;
	  minipad->y = y;
	  minipad->z = z;
	  minipad->layer = layer;
	  minipad->topbottom = topbottom;
	  minipad->chain = chain;
	  minipad->lx = lx;
	  minipad->ly = ly;
	  minipad->chip = chip;
	  minipad->quadrant = quadrant;
	  minipad->qsensor = qsensor;
	  minipad->key = generate_key(arm,packet,chipmap);
	  _big_lut.insert(minipad);

	  //      std::cout<<minipad->arm<<" "<<minipad->packet<<" "<<minipad->chipmap<<" "<<minipad->nx<<" "<<minipad->ny<<" "<<minipad->x<<" "<<minipad->y<<" "<<minipad->z<<" "<<minipad->layer<<" "<<minipad->topbottom<<" "<<minipad->chain<<" "<<minipad->lx<<" "<<minipad->ly<<" "<<minipad->chip<<" "<<minipad->quadrant<<" "<<minipad->qsensor<<" "<<minipad->key<<std::endl;
	}
      csvfile.close();
    } else { //read the db instead

    //first we try to find the run number if specified in recoConst
    //or in the RunHeader of topNode.
    //if we find a valid run -- use it to retrieve the info from the db
    //otherwise we grab the most recent mapper based on the current time

    int run_number = 0;
    recoConsts *rc = recoConsts::instance();
    if(rc->FlagExist("RUNNUMBER")){
      run_number = rc->get_IntFlag("RUNNUMBER");
    } else {
      Fun4AllServer *se = Fun4AllServer::instance();
      PHCompositeNode *topNode = se->topNode();
      if(topNode != NULL){
	RunHeader *runHeader = findNode::getClass<RunHeader>(topNode,"RunHeader");
	if(runHeader != NULL){
	  run_number = runHeader->get_RunNumber();
	}
      }
    }

    PHTimeStamp *start_time = NULL;
    if(run_number == 0){
      std::cout<<PHWHERE<<" Could not find a run number -- using the most recent map"<<std::endl;
      start_time = new PHTimeStamp(time(NULL));
    } else {
      RunToTime *t = RunToTime::instance();
      start_time = t->getBeginTime(run_number);
      if(start_time == NULL){
	std::cout<<PHWHERE<<" Could not find Run "<<run_number<<" -- using the most recent map"<<std::endl;
	start_time = new PHTimeStamp(time(NULL));
      } else {
	std::cout<<PHWHERE<<" Reading for Run "<<run_number<<std::endl;
      }
    }

    //make the connection to the database
    PdbBankManager *bankManager = PdbBankManager::instance();
    if(bankManager != NULL){

      //and grab an application
      PdbApplication *app = PdbApplication::instance();
      if(app != NULL){

	//start reading the data
	if(app->startRead()){
	  PdbBankID bankID;
	  bankID.setInternalValue(0);
      
	  PdbCalBank *bank = bankManager->fetchBank("PdbMpcExCoordinateMapBank",bankID,"mpcexcoordinatemap",*start_time);

	  if(bank){
	    bank->printHeader();

	    for(unsigned int index=0; index<NMINIPADS; index++){
	      PdbMpcExCoordinateMap data = (PdbMpcExCoordinateMap&)(bank->getEntry(index));

	      MinipadCoordinate *minipad = new MinipadCoordinate();
	      minipad->arm = data.arm();
	      minipad->packet = data.packet();
	      minipad->chipmap = data.chipmap();
	      minipad->nx = data.nx();
	      minipad->ny = data.ny();
	      minipad->x = data.x();
	      minipad->y = data.y();
	      minipad->z = data.z();
	      minipad->layer = data.layer();
	      minipad->topbottom = data.topbottom();
	      minipad->chain = data.chain();
	      minipad->lx = data.lx();
	      minipad->ly = data.ly();
	      minipad->chip = data.chip();
	      minipad->quadrant = data.quadrant();
	      minipad->qsensor = data.sensor_in_quadrant();
	      minipad->key = generate_key(minipad->arm,minipad->packet,minipad->chipmap);
	      _big_lut.insert(minipad);
	    }
	    delete bank;
	  } else {
	    std::cout<<"I could not read PdbMpcExCoordinateMapBank from mpcexcoordinatemap -- bailing"<<std::endl;
	  }
	} else {
	  app->abort();
	  std::cout<<" Problem reading from the database"<<std::endl;
	}
    
	//cleanup
	app->DisconnectDB();
      }//got the application
      else {
	std::cout<<" I could not get a valid pointer to PdbApplication"<<std::endl;
      }

    } else {
      std::cout<<PHWHERE<<" I could not get a valid pointer to PdbBankManger"<<std::endl;
    }//got the bank manager

    //cleanup
    delete start_time;
  }//else read the db
}

/**
 * The key is generated as a contiguous combination of
 * arm, packet and chipmap
 * arm*NPACKETS_PER_ARM*NMINIPADS_PER_PACKET + packet*NMINIPADS_PER_PACKET + chipmap
 *
 */

unsigned int MpcExMapper::generate_key(const unsigned short arm, const unsigned short packet, const unsigned short chipmap) const 
{
  unsigned int key = (arm * NPACKETS_PER_ARM * NMINIPADS_PER_PACKET) + (packet * NMINIPADS_PER_PACKET ) + chipmap;
  return key;
}

unsigned short MpcExMapper::get_chipmap(const unsigned short chain, const unsigned short chip, const unsigned short rocbond) const {
  return (chain * NCHIPS_PER_CHAIN * NROCBONDS_PER_CHIP) + (chip * NROCBONDS_PER_CHIP) + rocbond;
}

unsigned short MpcExMapper::get_arm(const unsigned int key) const 
{
  return  key / (NPACKETS_PER_ARM*NMINIPADS_PER_PACKET);
}

unsigned short MpcExMapper::get_packet(const unsigned int key) const 
{
  return  ( key % (NPACKETS_PER_ARM*NMINIPADS_PER_PACKET) ) / NMINIPADS_PER_PACKET;
}

unsigned short MpcExMapper::get_chipmap(const unsigned int key) const 
{
  return  ( key % (NPACKETS_PER_ARM*NMINIPADS_PER_PACKET) ) % NMINIPADS_PER_PACKET;
}

unsigned short MpcExMapper::get_chip_in_packet(const unsigned int key) const 
{
  MinipadCoordinate *minipad = coordinate_from_key(key);
  if(minipad == NULL){
    std::cout<<"get_chip_in_packet: I cannot find coordinates for key: "<<key<<std::endl;
    return 0xFFFF;
  }
  return minipad->chip;
}

/**
 * rocbond, chip, and chain are not saved in the big LUTs so we have to calculate it from the chipmap:
 * chipmap = chain*NCHIPS_PER_CHAIN*NROCBONDS_PER_CHIP+chip*NROCBONDS_PER_CHIP+rocbond
 */

unsigned short MpcExMapper::get_chain(const unsigned int key) const 
{
  return  ( key % (NCHAINS_PER_PACKET*NCHIPS_PER_CHAIN*NROCBONDS_PER_CHIP) ) / (NCHIPS_PER_CHAIN*NROCBONDS_PER_CHIP);
}

unsigned short MpcExMapper::get_chip(const unsigned int key) const 
{
  return (((key%(NPACKETS_PER_ARM*NCHAINS_PER_PACKET*NCHIPS_PER_CHAIN*NROCBONDS_PER_CHIP))%(NCHAINS_PER_PACKET*NCHIPS_PER_CHAIN*NROCBONDS_PER_CHIP))%(NCHIPS_PER_CHAIN*NROCBONDS_PER_CHIP))/NROCBONDS_PER_CHIP;
}

unsigned short MpcExMapper::get_rocbond(const unsigned int key) const 
{
  return (((key%(NPACKETS_PER_ARM*NCHAINS_PER_PACKET*NCHIPS_PER_CHAIN*NROCBONDS_PER_CHIP))%(NCHAINS_PER_PACKET*NCHIPS_PER_CHAIN*NROCBONDS_PER_CHIP))%(NCHIPS_PER_CHAIN*NROCBONDS_PER_CHIP))%NROCBONDS_PER_CHIP;
}

float MpcExMapper::get_x(const unsigned int key) const 
{
  MinipadCoordinate *minipad = coordinate_from_key(key);
  if(minipad == NULL){
    std::cout<<"get_x: I cannot find coordinates for key: "<<key<<std::endl;
    return -9999.;
  }
  return minipad->x;
}

float MpcExMapper::get_y(const unsigned int key) const {
  MinipadCoordinate *minipad = coordinate_from_key(key);
  if(minipad == NULL){
    std::cout<<"get_y: I cannot find coordinates for key: "<<key<<std::endl;
    return -9999.;
  }
  return minipad->y;
}

float MpcExMapper::get_z(const unsigned int key) const {
  MinipadCoordinate *minipad = coordinate_from_key(key);
  if(minipad == NULL){
    std::cout<<"get_z: I cannot find coordinates for key: "<<key<<std::endl;
    return -9999.;
  }
  return minipad->z;
}

float MpcExMapper::get_hsx(const unsigned int key, float z_vertex) const {
  MinipadCoordinate *minipad = coordinate_from_key(key);
  if(minipad == NULL){
    std::cout<<"get_hsx: I cannot find coordinates for key: "<<key<<std::endl;
    return -9999.;
  }  
  return minipad->x/(minipad->z - z_vertex);
}

float MpcExMapper::get_hsy(const unsigned int key, float z_vertex) const {
  MinipadCoordinate *minipad = coordinate_from_key(key);
  if(minipad == NULL){
    std::cout<<"get_hsy: I cannot find coordinates for key: "<<key<<std::endl;
    return -9999.;
  }  
  return minipad->y/(minipad->z - z_vertex);
}

float MpcExMapper::get_minipad_x_width(const unsigned int key) const{
  double dx = MpcExConstants::MINIPAD_SHORT_LENGTH;
  MinipadCoordinate *minipad = coordinate_from_key(key);
  if(minipad == NULL){
    std::cout<<"get_minipad_x_width: I cannot find coordinates for key: "<<key<<std::endl;
    return -9999.;
  }
  int type = (minipad->layer)%2;
  if(type == 1)
    dx = MpcExConstants::MINIPAD_LONG_LENGTH;
  return dx;
}

float MpcExMapper::get_minipad_y_width(const unsigned int key) const{
  double dy = MpcExConstants::MINIPAD_LONG_LENGTH;
  MinipadCoordinate *minipad = coordinate_from_key(key);
  if(minipad == NULL){
    std::cout<<"get_minipad_y_width: I cannot find coordinates for key: "<<key<<std::endl;
    return -9999.;
  }
  int type = (minipad->layer)%2;
  if(type == 1)
    dy = MpcExConstants::MINIPAD_SHORT_LENGTH;
  return dy;
}

unsigned short MpcExMapper::get_layer(const unsigned int key) const 
{
  MinipadCoordinate *minipad = coordinate_from_key(key);
  if(minipad == NULL){
    std::cout<<"get_layer: I cannot find coordinates for key: "<<key<<std::endl;
    return 0xFFFF;
  }
  return minipad->layer;
}

unsigned short MpcExMapper::get_topbottom(const unsigned int key) const 
{
  MinipadCoordinate *minipad = coordinate_from_key(key);
  if(minipad == NULL){
    std::cout<<"get_topbottom: I cannot find coordinates for key: "<<key<<std::endl;
    return 0xFFFF;
  }
  return minipad->topbottom;
}

unsigned short MpcExMapper::get_quadrant(const unsigned int key) const 
{
  MinipadCoordinate *minipad = coordinate_from_key(key);
  if(minipad == NULL){
    std::cout<<"get_quadrant: I cannot find coordinates for key: "<<key<<std::endl;
    return 0xFFFF;
  }
  return minipad->quadrant;
}

unsigned short MpcExMapper::get_sensor_in_quadrant(const unsigned int key) const 
{
  MinipadCoordinate *minipad = coordinate_from_key(key);
  if(minipad == NULL){
    std::cout<<"get_sensor_in_quadrant: I cannot find coordinates for key: "<<key<<std::endl;
    return 0xFFFF;
  }
  return minipad->qsensor;
}

unsigned short MpcExMapper::get_lx(const unsigned int key) const 
{
  MinipadCoordinate *minipad = coordinate_from_key(key);
  if(minipad == NULL){
    std::cout<<"get_lx: I cannot find coordinates for key: "<<key<<std::endl;
    return 0xFFFF;
  }
  return minipad->lx;
}

unsigned short MpcExMapper::get_ly(const unsigned int key) const 
{
  MinipadCoordinate *minipad = coordinate_from_key(key);
  if(minipad == NULL){
    std::cout<<"get_ly: I cannot find coordinates for key: "<<key<<std::endl;
    return 0xFFFF;
  }
  return minipad->ly;
}

unsigned short MpcExMapper::get_nx(const unsigned int key) const 
{
  MinipadCoordinate *minipad = coordinate_from_key(key);
  if(minipad == NULL){
    std::cout<<"get_nx: I could not find the coordinates for key: "<<key<<std::endl;
    return 0xFFFF;
  }
  return minipad->nx;
}

unsigned short MpcExMapper::get_ny(const unsigned int key) const 
{
  MinipadCoordinate *minipad = coordinate_from_key(key);
  if(minipad == NULL){
    std::cout<<"get_ny: I could not find the coordinates for key: "<<key<<std::endl;
    return 0xFFFF;
  }
  return minipad->ny;
}

unsigned short MpcExMapper::get_chain(const unsigned short arm, const unsigned short layer, const unsigned short sx, const unsigned short sy) const {
  //this is for even layers in the south arm
  //the odd layers for the north arm are opposite, that is 0<-->1 and 2<-->3
  //the -1 indicate the that a sensor does not exist at that location
  static const unsigned short BADVAL = 0xFFFF;
  static const unsigned short chain_sx_sy[6][6] = {{BADVAL,1,0,0,BADVAL,BADVAL},{1,1,1,0,0,BADVAL},{1,1,BADVAL,BADVAL,0,0},{0,0,BADVAL,BADVAL,1,1},{0,0,1,1,1,BADVAL},{BADVAL,0,0,1,BADVAL,BADVAL}};

  int ch = chain_sx_sy[sx][sy];
  if(ch == -1){
    std::cout<<" Something is wrong: attempting to grab a sensor that doesn't exist at (sx,sy): ("<<sx<<","<<sy<<")"<<std::endl;
    return BADVAL;
  }

  //For the each arm, add 2 to the chain in the odd layers
  if((layer%2)==1){
    ch += 2;
  }

  //For the north arm swap 0<-->1 and 2<-->3
  if(arm==1){
    switch (ch) {
      case 0:
	ch=1;
	break;
      case 1:
	ch=0;
	break;
      case 2:
	ch=3;
	break;
      case 3:
	ch=2;
	break;
      default:
	ch = BADVAL;
	std::cout<<" Something is very wrong -- trying to swap chain values that don't exist"<<std::endl;
      }
  }

  return ch;
}

MpcExMapper::MinipadCoordinate* MpcExMapper::coordinate_from_key(const unsigned int key) const 
{
  MinipadCoordinate *guess = new MinipadCoordinate();
  guess->key = key;
  std::set<MinipadCoordinate*>::const_iterator itr = _big_lut.find(guess);
  delete guess;
  if(itr == _big_lut.end())
    return NULL;
  return *itr;
}
