#include <PgPostApplication.hh>
#include <PgPostBankManager.hh>
#include <RunToTimePg.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbMpcExOnlinePedestals.hh>
#include <RunToTime.hh>
#include <iostream>
#include <ctime>

void printUsage(){
  std::cout<<"Usage:"<<std::endl;
  std::cout<<"\tgetdcmpedestalruns [all|help]"<<std::endl;
  std::cout<<"\tPrints out a list of pedestal runs where the pedestals were applied in the dcm"<<std::endl;
  std::cout<<"\tall [option] -- prints out all pedestal runs"<<std::endl;
  std::cout<<"\thelp (optional)-- prints this message"<<std::endl;
}

int main(int argc, char** argv){

  //use the factory to create singletons 
  //used later for db access and finding times
  PgPostApplication::Register();
  PgPostBankManager::Register();
  RunToTimePg::Register();

  RunToTime *run2time = RunToTime::instance();

  bool print_dcm_only = true;
  if(argc>1){
    std::string what = argv[1];
    if(what=="help" || what != "all"){
      printUsage();
      return 1;
    }
    if(what == "all"){
      print_dcm_only = false;
    }
  }

  //the table object name and table name
  static const char* classname = "PdbMpcExOnlinePedestalsBank";
  static const char* calibname = "mpcexonlinepedestals";
  //10 seconds on RCF machines
  static const time_t shorttime(10);

  //open a connection
  PdbBankManager *bankManager = PdbBankManager::instance();
  if(bankManager == NULL){
    std::cout<<PHWHERE<<"cannot obtain PdbBankManager"<<std::endl;
    return -1;
  }

  PdbApplication *app = PdbApplication::instance();
  if(app == NULL){
    std::cout<<PHWHERE<<"cannot obtain PdbApplication"<<std::endl;
    return -1;
  }

  //start reading
  if(app->startRead()){

    //the organization of the mpcexonlinepedestals is
    //  ^
    //  |         --------------->
    //  |     ------------------->
    //  |  ---------------------->
    //  |------------------------>
    // where vertical is insert time and horizontal is validity time
    // therefore we can step backwards through all banks by starting
    // at the far future, getting the latest bank, and then looking 
    // for each subsequent bank a little before the start validity of
    // the current bank.
    //

    PHTimeStamp *readTime = new PHTimeStamp();
    readTime->setToFarFuture();
    *readTime -= shorttime;
    PdbBankID bankID;
    bankID.setInternalValue(0);
    PdbCalBank *bank;
    while((bank = bankManager->fetchBank(classname,bankID,calibname,*readTime))){
      PdbMpcExOnlinePedestals data = (PdbMpcExOnlinePedestals&)bank->getEntry(0);
      if(!print_dcm_only || data.get_applied_thresholds()==1){
	int runnumber = run2time->getRunNumber(bank->getStartValTime());
	std::cout<<runnumber<<std::endl;
      }
      *readTime = bank->getStartValTime();
      *readTime -= shorttime;
      delete bank;
    }
    delete readTime;
  } else {
    app->abort();
    std::cout<<PHWHERE<<"cannot read table..."<<std::endl;
  }

  //clean up
  app->DisconnectDB();

  return 0;
}
