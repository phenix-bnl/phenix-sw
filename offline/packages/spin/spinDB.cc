#include <cstdlib>
#include <iostream>
#include <PdbCalBank.hh>
#include <TArrayI.h>
#include <fstream>
#include <PdbSpinGL1p.hh>

#include <PgPostBankManager.hh>
#include <PgPostApplication.hh>
#include <RunToTimePg.hh>

//
// Main program for the data base access.
//    Created by H.Torii on 29/Aug/2003
//

using namespace std;

void spinDB_GL1p(char* infile);

int main(int argc,char **argv){
  if( argc != 2 ){
    cout<<" Usage :: spinDB gl1p_calibration.txt "<<endl;
    exit(0);
  }
  spinDB_GL1p(argv[1]);
};

void spinDB(char* infile){
  spinDB_GL1p(infile);
};
//
// The definition of the infile by K.Tanida (01/Sep/2003)
//
// [runnum] [shift_of_bunch_crossing] [status]
//
//    Corrected crossing ID = ( Raw crossing ID + 5 - [shift_of...] ) % 120
//
//        The number of 5 - [shift_of...] is stored into the DB, then
//    Corrected crossing ID = ( Raw crossing ID + [shift_in_DB] ) % 120
//
//
//    [status] = 0 : O.K.
//             = 1 : GL1p's data is useless or unavailable
//             = 2 : Offset finding program failed
//             = 3 : ZDC's data is useless
//             = 4 : Chisquare in the finding program is too high
//
//     If [status] = 1 or 3, the data must be still useful for the spin physics.
//     Due to the limited time, we decided not to use them at this moment.
//     In case that the offset finding program filed, the [shift...] is set as -1.
//
//

//
// The definition of the infile by Y.Fukao (Aug/2004) for Run4
//
// cross_corr = (cross_DST + shift ) % 120 
//  Be creaful about the definition of the shift has changed from Run3!!!
//

void spinDB_GL1p(char* infile){
  char cline[256];
  int irun, ishift, istat;
  int iline = 0;
  TArrayI v_run;
  TArrayI v_shift;
  TArrayI v_stat;
  //
  cout<<" spinDB_GL1p :: read file : "<<infile<<endl;
  ifstream fin(infile);
  while( fin.getline(cline,256) ){
    if( cline[0] != '#' &&
	sscanf(cline,"%d %d %d",&irun,&ishift,&istat) == 3 ){
      iline++;
      //cout<<" DEBUG:: (run,shift,stat) = ("<<irun<<","<<ishift<<","<<istat<<") "<<endl;
      v_run.Set(iline);
      v_run[iline-1] = irun;
      v_shift.Set(iline);
      v_shift[iline-1] = ishift;
      v_stat.Set(iline);
      v_stat[iline-1] = istat;
    }
  }
  //cout<<" spinDB_GL1p :: read "<<v_run.GetSize()<<" lines.. "<<endl;
  cout<<" spinDB_GL1p :: read "<<iline<<" lines from the file."<<endl;
  cout<<" spinDB_GL1p :: This is PostgreSQL version.. "<<endl;
  cout<<" spinDB_GL1p :: Do you want to update them into DB ? [n]/y : "<<flush;
  cin>>cline;
  if( strncmp(cline,"y",1) == 0 ){
    PgPostBankManager::Register();
    PgPostApplication::Register();
    RunToTimePg::Register();
    RunToTime *run_time=RunToTime::instance();
    PdbBankManager *bankManager = PdbBankManager::instance();
    PdbApplication *application = bankManager->getApplication();
    PdbBankID bankID(0);
    PdbCalBank *spinbank;
    PdbSpinGL1p* pdbspingl1p = NULL;
    const char* calibname = "calib.spin.gl1p";
    if (application->startUpdate()) {
      while( iline-- ){
	cout<<"        ------>:: (run,shift,stat) = ("<<v_run[iline]<<","<<v_shift[iline]<<","<<v_stat[iline]<<") "<<endl;
	int runnum = v_run[iline];
#ifdef TEST_FDB    
	cout<<" spinDB:: -------------------------------------------- "<<endl;
	cout<<" spinDB:: !!!! This is for a private DB. "<<endl;
	cout<<" spinDB:: -------------------------------------------- "<<endl;
	PHTimeStamp start_time;
	PHTimeStamp end_time;
	start_time.set(1998,1,1,0,0,0,0);
	end_time.setToFarFuture();
	spinbank = bankManager->createBank("PdbSpinGL1pBank",bankID,"Calibration of GL1p",start_time,end_time,calibname);
#else
	PHTimeStamp *start_time=run_time->getBeginTime(runnum);
	PHTimeStamp *end_time=run_time->getEndTime(runnum);
	if(!start_time || !end_time){
	  cout << "Warning : No start/end time for run " << runnum << endl;
	  continue;
	}

	spinbank = bankManager->createBank("PdbSpinGL1pBank",bankID,"description",*start_time,*end_time,calibname);
	delete start_time;
	delete end_time;
#endif
	if (spinbank) {
	  spinbank->setUserName(getenv("USER"));
	  spinbank->setLength(1);
	  pdbspingl1p = (PdbSpinGL1p*) & (spinbank->getEntry(0));
#ifdef SPINRUN3
	  if( v_stat[iline] == 0 )
	    pdbspingl1p->set(v_run[iline],(int)(5-v_shift[iline]),1);
	  else
	    pdbspingl1p->set(v_run[iline],(int)(5-v_shift[iline]),0);
#endif
#define SPINRUN4
#ifdef SPINRUN4
	  pdbspingl1p->set(v_run[iline],(int)(v_shift[iline]),(v_stat[iline]==0?1:0));
#endif
	  std::cout<<" -> "<<spinbank->getInsertTime()<<" ("<<spinbank->getStartValTime()
		   <<","<<spinbank->getEndValTime()<<")"<<std::endl;
	  //spinbank->printHeader();
	  spinbank->print();
	  pdbspingl1p->print();
	  if( application->commit() ){
	    cout<<" spinDB_GL1p :: Succeeded to commit the DB. "<<endl;
	  } else {
	    cout<<" spinDB_GL1p :: Failed to commit the DB!!!! "<<endl;
	  }
	  //cout<<" spinDB_GL1p :: Skip committing the DB!!!! "<<endl;
	} else {
	  cout <<" spinDB_GL1p :: Error in creating database entry." << endl;
	  //exit(0);
	}
      } // End of loop

    } else {
      cout<<" spinDB_GL1p :: Failed to open DB. Check environment. "<<endl;
    }
  } else {
    cout<<" spinDB_GL1p :: Skipped .. "<<endl;
  }


};
