#include <PgPostApplication.hh>
#include <PgPostBankManager.hh>
#include <RunToTimePg.hh>
#include <RunToTime.hh>
#include <PHTimeStamp.h>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankID.hh>
#include <PdbCalBank.hh>
#include <PdbMpcExOnlinePedestals.hh>
#include <PdbClassMap.hh>
#include <phool.h>
#include <string>
#include <fstream>
#include <iostream>
#include <cstdlib>
#include <ctime>

static const char* classname = "PdbMpcExOnlinePedestalsBank";
static const char* calibname = "mpcexonlinepedestals";
PdbBankManager *bankManager = NULL;
PdbApplication *app = NULL;
bool test = false;

//! Make connections to the database, return false if either bankManager or app is NULL
bool openConnection(const char* method){
  bankManager = PdbBankManager::instance();
  if(bankManager == NULL){
    std::cout<<PHWHERE<<" mpcexdbonlinepeds::"<<method<<" - cannot obtain PdbBankManager"<<std::endl;
    return false;
  }

  app = PdbApplication::instance();
  if(app == NULL){
    std::cout<<PHWHERE<<" mpcexdbonlinepeds::"<<method<<" - cannot obtain PdbApplication"<<std::endl;
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

void commit_data(const std::string& filename, PHTimeStamp& startTime, PHTimeStamp& endTime, const std::string& username, const std::string& description){

  if(openConnection("commit_data")){
    if(app->startUpdate()){

      PdbBankID bankID;
      bankID.setInternalValue(0);

      //create the new bank
      PdbCalBank *bank = bankManager->createBank(classname,bankID,description.c_str(),startTime,endTime,calibname);
      bank->setLength(1);
      bank->setUserName(username.c_str());

      //get the content object to fill
      PdbMpcExOnlinePedestals *data = (PdbMpcExOnlinePedestals*) &(bank->getEntry(0));

      //read in the intput file and fill the data object
      unsigned int arm, packet, channel, threshold;
      float pedestal, width, chi2;
      std::ifstream file(filename.c_str());
      int applied;
      file>>applied;
      data->set_applied_thresholds(applied);
      while(file>>std::dec>>arm>>packet>>channel>>pedestal>>width>>chi2>>std::hex>>threshold){
	//	std::cout<<arm<<" "<<packet<<" "<<channel<<" "<<pedestal<<" "<<width<<" "<<chi2<<" "<<threshold<<std::endl;
	data->set_pedestal(arm,packet,channel,pedestal);
	data->set_pedestal_width(arm,packet,channel,width);
	data->set_pedestal_chi2(arm,packet,channel,chi2);
	data->set_threshold(arm,packet,channel,threshold);
      }
      file.close();


      //testing...
      //      data->print();

      //actually commit stuff as long as it isn't a test
      if(!test){
	app->commit(bank);
      }

    } else {
      std::cout<<PHWHERE<<" mpcexdbonlinepeds::commit_pedestal_data - cannot update application"<<std::endl;
    }
  }

}

/**
 * Initially commits pedestal data by reading information from a text file with the following format
 * <applied threshold> [0 or 1] for [no yes]
 * <arm> <packet> <channel> <pedestal> <width> <chi2> <threshold>
 * <arm> <packet> <channel> <pedestal> <width> <chi2> <threshold>
 * <arm> <packet> <channel> <pedestal> <width> <chi2> <threshold>
 * ...
 * A single run number is given as the pedestal run. This sets the start validity time. The end
 * validity time is set to the far future since we don't know when the next pedestal calibration 
 * will be done. Because of this pedestals must be submitted in ascending run order to ensure
 * no pedestal data will be hidden by the stabbing query. A check ensures this will not be done.
 */
void commit_pedestal_data(const std::string& filename, int run_number, const std::string& username, const std::string& description){

  PHTimeStamp *startTime = get_run_start_time(run_number);
  //set the end time to the far future. If a new run comes along, then the
  //end validity will be set to the start validity of the new set of pedestals
  PHTimeStamp endTime;
  endTime.setToFarFuture();
  PdbBankID bankID;
  bankID.setInternalValue(0);

  //all banks are initially inserted with end validity time in the 
  //far future since I don't know when the next calibration run will be.
  //So, this requires the calibrations to be uploaded in ascending 
  //run number order to ensure that back-inserting calibrations will
  //never hide calibrations.
  if(openConnection("commit_pedestal_data")){
    if(app->startRead()){
      PdbCalBank *prev = bankManager->fetchBank(classname,bankID,calibname,*startTime);
      if(prev){
	if(prev->getStartValTime() > *startTime){
	  std::cout<<"Trying to back insert a run! These pedestal calibrations MUST BE INSERTED IN ASCENDING RUN NUMBER ORDER!"<<std::endl;
	  std::cout<<"You will have to email ngrau@augie.edu to fix this."<<std::endl;
	  std::cout<<"In the email specify the run number you are trying to insert"<<std::endl;
	  delete prev;
	  return;
	}
	delete prev;
      }
    } else {
      std::cout<<PHWHERE<<" mpcexdbonlinepeds::commit_pedestal_data - cannot update application"<<std::endl;
    }
  }

  commit_data(filename,*startTime,endTime,username,description);

  delete startTime;
}

/**
 * Recommits pedestal data by reading information from a text file with the following format
 * <applied threshold> [0 or 1] for [no yes]
 * <arm> <packet> <channel> <pedestal> <width> <chi2> <threshold>
 * <arm> <packet> <channel> <pedestal> <width> <chi2> <threshold>
 * <arm> <packet> <channel> <pedestal> <width> <chi2> <threshold>
 * ...
 * Two run numbers are given as the validity range between two successive pedestal runs. If the
 * recommit is the last pedestal run, this it is sufficient to run commit instead of recommit.
 * A check based on existing table data to make sure the run numbers given do not overlap an 
 * intermediate pedestal run and thereby hide it from a stabbing query.
 */
void recommit_pedestal_data(const std::string& filename, int start_run_number, int end_run_number, const std::string& username, const std::string& description){

  //upon recommit, I hope that we know the start and end run numbers, otherwise
  //we can just use commit to set the end time to the far future.
  PHTimeStamp *startTime = get_run_start_time(start_run_number);
  PHTimeStamp *endTime = get_run_start_time(end_run_number);
  PdbBankID bankID;
  bankID.setInternalValue(0);

  //since the structure of the table in insert time vs. validity time looks like
  // ^
  // |
  // |
  // |        |----->
  // |     |-------->
  // |  |----------->
  // |-------------->
  // and we are trying to add something in the middle with the right range like
  // ^
  // |
  // |     |--|
  // |        |----->
  // |     |-------->
  // |  |----------->
  // |-------------->
  // It makes sense to check the bank that starts with the requested end range
  // look at the immediate previous bank and confirm that those are the same
  // run numbers. I want to protect against, the following happening
  // ^
  // |
  // |  |-----|
  // |        |----->
  // |     |-------->
  // |  |----------->
  // |-------------->
  // and hiding the second calibration by spanning the 1st--3rd
  time_t shorttime(10);
  RunToTime *run2time = RunToTime::instance();
  if(openConnection("commit_pedestal_data")){
    if(app->startRead()){
      PdbCalBank *last = bankManager->fetchBank(classname,bankID,calibname,*endTime);
      if(!last) {
	std::cout<<PHWHERE<<" recommit_pedestal_data -- something's wrong with the ending run number: no bank found. Please double check the run ranges and rerun."<<std::endl;
	return;
      }
      int last_run_number = run2time->getRunNumber(last->getStartValTime());
      if(last_run_number != end_run_number){
	std::cout<<PHWHERE<<" recommit_pedestal_data -- something's wrong with the ending run number it should be "<<last_run_number<<" instead of "<<end_run_number<<". Please double check and rerun."<<std::endl;
	delete last;
	return;
      }
      PdbCalBank *prev = bankManager->fetchBank(classname,bankID,calibname,*endTime-shorttime);
      if(!prev){
	delete last;
	std::cout<<PHWHERE<<" recommit_pedestal_data -- something's wrong with the end run number: no bank previous to the end run number was found. Please double check the run numbers and rerun."<<std::endl;
	return;
      }
      int first_run_number = run2time->getRunNumber(prev->getStartValTime());
      if(first_run_number != start_run_number){
	std::cout<<PHWHERE<<" recommit_pedestal_data -- something's wrong with the run range: "<<first_run_number<<" is between "<<start_run_number<<" and "<<end_run_number<<". Please double check and rerun."<<std::endl;
	delete last;
	delete prev;
	return;
      }

      //if we get to here, all is well and we can commit
      std::cout<<PHWHERE<<" recommit_pedestal_data -- all is alright! Updating pedestal data for "<<first_run_number<<" - "<<last_run_number<<std::endl;
      delete last;
      delete prev;
    } else {
      std::cout<<PHWHERE<<" mpcexdbonlinepeds::commit_pedestal_data - cannot update application"<<std::endl;
    }
  }

  commit_data(filename,*startTime,*endTime,username,description);

  delete startTime;
  delete endTime;
}

/**
 * A simple dump of the contents of the pedestal table for a given run. This run need not be a 
 * pedestal run.
 */
void dump_pedestal_data(int run_number){
  PdbMpcExOnlinePedestals data;

  if(openConnection("dump_pedestal_data")){

    if(app->startRead()){

      //obtain the time stamp for the requested run
      //and fetch the bank from the time stamp
      PHTimeStamp *startTime = get_run_start_time(run_number);
      PdbBankID bankID;
      bankID.setInternalValue(0);
      PdbCalBank *bank = bankManager->fetchBank(classname,bankID,calibname,*startTime);
      delete startTime;

      if(bank){
	bank->printHeader();
	data = (PdbMpcExOnlinePedestals&)bank->getEntry(0);
	app->commit();
	delete bank;
      } else {
	std::cout<<PHWHERE<<" mpcexdbonlinepeds::dump_pedestal_data -- no bank is valid for run "<<run_number<<std::endl;
      }

    } else {
      app->abort();
      std::cout<<PHWHERE<<" mpcexdbonlinepeds::commit_pedestal_data - cannot update application"<<std::endl;
    }
    //clean up
    app->DisconnectDB();
  }

  data.print();

}

void printUsage(){
  std::cout<<"Usage:"<<std::endl;
  std::cout<<"\tmpcexonlinepeds [commit|recommit|dump|help]"<<std::endl;
  std::cout<<"\t  Followed by several other options"<<std::endl;
  std::cout<<"\t\t test <filename> <runnumber> <username> <description>"<<std::endl;
  std::cout<<"\t\t\t--tests the reading and uploading of the input file"<<std::endl;
  std::cout<<"\t\t commit <filename> <runnumber> <username> <description>"<<std::endl;
  std::cout<<"\t\t\t--commits pedestals reading and uploading of the input file"<<std::endl;
  std::cout<<"\t\t recommit <filename> <start runnumber> <stop runnumber> <username> <description>"<<std::endl;
  std::cout<<"\t\t\t--recommits pedestals reading and uploading of the input file valid between two successive pedestal runs"<<std::endl;
  std::cout<<"\t\t dump <runnumber>"<<std::endl;
  std::cout<<"\t\t\t-- dumps the contents of the table for a given run number"<<std::endl;
  std::cout<<"\t\thelp "<<std::endl;
  std::cout<<"\t\t\t-- prints this message"<<std::endl;
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
     (what != "dump" && what != "commit" && what != "recommit" && what!= "test") || 
     (what=="dump" && argc!=3) || 
     (what=="commit" && argc!=6) || 
     (what=="recommit" && argc!=7) || 
     (what=="test" && argc!=6)
     ){
    printUsage();
    return -1;
  }

  //commit the data
  if(what=="commit" || what=="test"){
    std::string filename = argv[2];
    int runnumber = atoi(argv[3]);
    std::string username = argv[4];
    std::string description = argv[5];
    test = (what=="test");
    commit_pedestal_data(filename,runnumber,username,description);
  }

  if(what=="recommit"){
    std::string filename = argv[2];
    int start_runnumber = atoi(argv[3]);
    int end_runnumber = atoi(argv[4]);
    std::string username = argv[5];
    std::string description = argv[6];
    recommit_pedestal_data(filename,start_runnumber,end_runnumber,username,description);
  }

  //dump the data
  if(what=="dump"){
    int run_number = atoi(argv[2]);
    dump_pedestal_data(run_number);
  }

  return 0;
}
