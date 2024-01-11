#include <PgPostApplication.hh>
#include <PgPostBankManager.hh>
#include <RunToTimePg.hh>
#include <RunToTime.hh>
#include <PHTimeStamp.h>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankID.hh>
#include <PdbCalBank.hh>
#include <PdbMpcExCoordinateMap.hh>
#include <PdbClassMap.hh>
#include <phool.h>
#include <string>
#include <fstream>
#include <iostream>
#include <cstdlib>

static const char* classname = "PdbMpcExCoordinateMapBank";
static const char* calibname = "mpcexcoordinatemap";
static const int NMINIPADS = 49152;
PdbBankManager *bankManager = NULL;
PdbApplication *app = NULL;
bool test = false;

//! Make connections to the database, return false if either bankManager or app is NULL
bool openConnection(const char* method){
  bankManager = PdbBankManager::instance();
  if(bankManager == NULL){
    std::cout<<PHWHERE<<" mpcexdbcoordinatemap::"<<method<<" - cannot obtain PdbBankManager"<<std::endl;
    return false;
  }

  app = PdbApplication::instance();
  if(app == NULL){
    std::cout<<PHWHERE<<" mpcexdbcoordinatemap::"<<method<<" - cannot obtain PdbApplication"<<std::endl;
    return false;
  }

  return true;
}

PHTimeStamp* get_run_start_time(int run_number){
  //use the input run number for the begin time of the calibrations
  //this would be for new commits
  RunToTime *runtotime = RunToTime::instance();
  PHTimeStamp *startTime = runtotime->getBeginTime(run_number);
  return startTime;
}

/**
 * Reads information from a text file with the following format
 * <arm> <packet> <chipmap> <nx> <ny> <x> <y> <z> <layer> <topbottom> <chain> <lx> <ly> <chip> <quadrant> <sensor_in_quadrant>
 */
void commit_coordinate_data(const std::string& filename, int begin_run_number, const std::string& username, const std::string& description){

  if(openConnection("commit_coordinate_data")){
    if(app->startUpdate()){

      PdbBankID bankID;
      bankID.setInternalValue(0);

      PHTimeStamp *startTime = get_run_start_time(begin_run_number);
      PHTimeStamp endTime;
      endTime.setToFarFuture();

      //create the new bank
      PdbCalBank *bank = bankManager->createBank(classname,bankID,description.c_str(),*startTime,endTime,calibname);
      bank->setLength(NMINIPADS);
      bank->setUserName(username.c_str());

      //clean up
      delete startTime;

      //read in the intput file and fill the data object
      unsigned short arm, packet, chipmap, nx, ny, layer, topbottom, chain, lx, ly, chip, quadrant, sensor_in_quadrant;
      float x, y, z;
      std::ifstream file(filename.c_str());
      unsigned int index = 0;

      std::string header;
      std::getline(file,header);
      while(file>>arm>>packet>>chipmap>>nx>>ny>>x>>y>>z>>layer>>topbottom>>chain>>lx>>ly>>chip>>quadrant>>sensor_in_quadrant) {
	std::cout<<arm<<" "<<packet<<" "<<chipmap<<" "<<nx<<" "<<ny<<" "<<x<<" "<<y<<" "<<z<<" "<<layer<<" "<<topbottom<<" "<<chain<<" "<<lx<<" "<<ly<<" "<<chip<<" "<<quadrant<<" "<<sensor_in_quadrant<<std::endl;

	//grab the minipad entry in the table and fill it with the input from the text file
	PdbMpcExCoordinateMap *data = (PdbMpcExCoordinateMap*) &(bank->getEntry(index));
	data->set_arm(arm);
	data->set_packet(packet);
	data->set_chipmap(chipmap);
	data->set_nx(nx);
	data->set_ny(ny);
	data->set_x(x);
	data->set_y(y);
	data->set_z(z);
	data->set_layer(layer);
	data->set_topbottom(topbottom);
	data->set_chain(chain);
	data->set_lx(lx);
	data->set_ly(ly);
	data->set_chip(chip);
	data->set_quadrant(quadrant);
	data->set_sensor_in_quadrant(sensor_in_quadrant);

	index++;
      }
      file.close();

      //actually commit stuff
      if(!test) {
	app->commit(bank);
      }

    } else {
      std::cout<<PHWHERE<<" mpcexdbcoordinatemap::commit_coordinate_data - cannot update application"<<std::endl;
    }
  }

}

void dump_coordinate_data(int run_number){
  if(openConnection("dump_coordinate_data")){

    if(app->startRead()){

      PHTimeStamp *startTime = get_run_start_time(run_number);
      PdbBankID bankID;
      bankID.setInternalValue(0);
      PdbCalBank *bank = bankManager->fetchBank(classname,bankID,calibname,*startTime);
      delete startTime;

      if(bank){
	bank->printHeader();
	PHTimeStamp startTime = bank->getStartValTime();
	PHTimeStamp endTime = bank->getEndValTime();
	std::cout<<"Valid from "<<startTime<<" -- "<<endTime<<std::endl;

	int nentries = bank->getLength();
	for(int i=0; i<nentries; i++){
	  PdbMpcExCoordinateMap data = (PdbMpcExCoordinateMap&)bank->getEntry(i);
	  data.print();
	}

	delete bank;

      } else {
	std::cout<<PHWHERE<<" mpcexdbcoordinatemap::dump_coordinate_data -- no bank is valid for run "<<run_number<<std::endl;
      }

    } else {
      app->abort();
      std::cout<<PHWHERE<<" mpcexdbcoordinatemap::dump_coordinate_data - cannot update application"<<std::endl;
    }
    //clean up
    app->DisconnectDB();
  }
}

void printUsage(){
  std::cout<<"Usage:"<<std::endl;
  std::cout<<"\tmpcexdccoordinatemap [commit|test|dump|help]"<<std::endl;
  std::cout<<"\t  Followed by several other options"<<std::endl;
  std::cout<<"\t\t commit <filename> <begin runnumber> <username> <description>"<<std::endl;
  std::cout<<"\t\t test <filename> <begin runnumber> <username> <description>"<<std::endl;
  std::cout<<"\t\t dump <runnumber>"<<std::endl;
  std::cout<<"\t\thelp -- prints this message"<<std::endl;
}

int main(int argc, char** argv){

  //use the factory to create singletons 
  //used later for db access and finding times
  PgPostApplication::Register();
  PgPostBankManager::Register();
  RunToTimePg::Register();

  //make sure all is well...
  if(argc<2) {
    printUsage();
    return -1;
  }

  std::string what = argv[1];
  if(what=="help" || 
     (what != "dump" && what != "commit" && what != "test") || 
     (what=="dump" && argc!=3) || 
     (what=="commit" && argc!=6) || 
     (what=="test" && argc!=6)){
    printUsage();
    return -1;
  }

  //commit the data
  if(what=="commit" || what=="test"){
    std::string filename = argv[2];
    int begin_runnumber = atoi(argv[3]);
    std::string username = argv[4];
    std::string description = argv[5];
    test = (what == "test");
    commit_coordinate_data(filename,begin_runnumber,username,description);
  }

  //dump the data
  if(what=="dump"){
    int run_number = atoi(argv[2]);
    dump_coordinate_data(run_number);
  }

  return 0;
}
