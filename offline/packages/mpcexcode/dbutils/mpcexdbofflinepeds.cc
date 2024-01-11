#include <PgPostApplication.hh>
#include <PgPostBankManager.hh>
#include <RunToTimePg.hh>
#include <RunToTime.hh>
#include <PHTimeStamp.h>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankID.hh>
#include <PdbCalBank.hh>
#include <PdbMpcExOfflinePedestals.hh>
#include <PdbClassMap.hh>
#include <phool.h>
#include <string>
#include <fstream>
#include <iostream>
#include <cstdlib>

static const char* classname = "PdbMpcExOfflinePedestalsBank";
static const char* calibname = "mpcexofflinepedestals";
static const int NMINIPADS = 49152;
PdbBankManager *bankManager = NULL;
PdbApplication *app = NULL;
bool test = false;

//! Make connections to the database, return false if either bankManager or app is NULL
bool openConnection(const char* method){
  bankManager = PdbBankManager::instance();
  if(bankManager == NULL){
    std::cout<<PHWHERE<<" mpcexdbofflinepeds::"<<method<<" - cannot obtain PdbBankManager"<<std::endl;
    return false;
  }

  app = PdbApplication::instance();
  if(app == NULL){
    std::cout<<PHWHERE<<" mpcexdbofflinepeds::"<<method<<" - cannot obtain PdbApplication"<<std::endl;
    return false;
  }

  return true;
}

PHTimeStamp* get_run_start_time(int run_number){
  //use the input run number for the begin time of the calibrations
  //this would be for new commits
  RunToTime *runtotime = RunToTime::instance();
  PHTimeStamp *startTime = new PHTimeStamp();
  startTime->setTics(runtotime->getBeginTime(run_number)->getTics());
  return startTime;
}

/**
 * Reads information from a text file with the following format
 * <arm> <layer> <type> <senx> <seny> <ix> <iy> <high ped> <high width> <low ped> <low width>
 */
void commit_pedestal_data(const std::string& filename, int begin_run_number, int end_run_number, const std::string& username, const std::string& description){

  if(openConnection("commit_pedestal_data")){
    if(app->startUpdate()){

      PdbBankID bankID;
      bankID.setInternalValue(0);

      PHTimeStamp *startTime = get_run_start_time(begin_run_number);
      PHTimeStamp *endTime;
      //if the ending run number is <0, then we will set the end 
      //validity to 30 days from the start of the begin_run_number
      //otherwise use the start time of the given run number as the end
      if(end_run_number > 0) {
	endTime = get_run_start_time(end_run_number);
      } else {
	time_t thirty_days(30*24*3600);
	endTime = new PHTimeStamp(*startTime + thirty_days);
      }

      //create the new bank
      PdbCalBank *bank = bankManager->createBank(classname,bankID,description.c_str(),*startTime,*endTime,calibname);
      bank->setLength(NMINIPADS);
      bank->setUserName(username.c_str());

      //clean up
      delete startTime;
      delete endTime;

      //read in the intput file and fill the data object
      unsigned short arm, layer, type, senx, seny, ix, iy;
      float high_pedestal, high_width, high_chi2, low_pedestal, low_width, low_chi2;
      std::ifstream file(filename.c_str());
      unsigned int index = 0;
      while(file>>arm>>layer>>type>>senx>>seny>>ix>>iy>>high_pedestal>>high_width>>high_chi2>>low_pedestal>>low_width>>low_chi2){
	std::cout<<arm<<" "<<layer<<" "<<type<<" "<<senx<<" "<<seny<<" "<<ix<<" "<<iy<<" "<<high_pedestal<<" "<<high_width<<" "<<low_pedestal<<" "<<low_width<<std::endl;

	//grab the minipad entry in the table and fill it with the input from the text file
	PdbMpcExOfflinePedestals *data = (PdbMpcExOfflinePedestals*) &(bank->getEntry(index));
	data->set_data(arm,layer,type,senx,seny,ix,iy,high_pedestal,high_width,high_chi2,low_pedestal,low_width,low_chi2);

	index++;
      }
      file.close();

      //actually commit stuff
      if(!test) {
	app->commit(bank);
      }

    } else {
      std::cout<<PHWHERE<<" mpcexdbonlinepeds::commit_pedestal_data - cannot update application"<<std::endl;
    }
  }

}

void dump_pedestal_data(int run_number){
  if(openConnection("dump_pedestal_data")){

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
	  PdbMpcExOfflinePedestals data = (PdbMpcExOfflinePedestals&)bank->getEntry(i);
	  data.print();
	}

	delete bank;

      } else {
	std::cout<<PHWHERE<<" mpcexdbofflinepeds::dump_pedestal_data -- no bank is valid for run "<<run_number<<std::endl;
      }

    } else {
      app->abort();
      std::cout<<PHWHERE<<" mpcexdbofflinepeds::commit_pedestal_data - cannot update application"<<std::endl;
    }
    //clean up
    app->DisconnectDB();
  }
}

void printUsage(){
  std::cout<<"Usage:"<<std::endl;
  std::cout<<"\tmpcexdcofflinepeds [commit|test|dump|help]"<<std::endl;
  std::cout<<"\t  Followed by several other options"<<std::endl;
  std::cout<<"\t\t commit <filename> <begin runnumber> <end runnumber> <username> <description>"<<std::endl;
  std::cout<<"\t\t test <filename> <begin runnumber> <end runnumber> <username> <description>"<<std::endl;
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
     (what=="commit" && argc!=7) || 
     (what=="test" && argc!=7)){
    printUsage();
    return -1;
  }

  //commit the data
  if(what=="commit" || what=="test"){
    std::string filename = argv[2];
    int begin_runnumber = atoi(argv[3]);
    int end_runnumber = atoi(argv[4]);
    std::string username = argv[5];
    std::string description = argv[6];
    test = (what == "test");
    commit_pedestal_data(filename,begin_runnumber,end_runnumber,username,description);
  }

  //dump the data
  if(what=="dump"){
    int run_number = atoi(argv[2]);
    dump_pedestal_data(run_number);
  }

  return 0;
}
