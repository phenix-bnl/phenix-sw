using namespace std;
#define MAXBUF 1024 

class PHTimeStamp;

void printTime(PHTimeStamp *time);

void updateDBBeamCenter(const char *filename){
  gSystem->Load("libsvx.so");
  gSystem->Load("libfun4all");
  gSystem->Load("libpdbcalBase.so");
  gSystem->Load("libPgCal.so");
  gSystem->Load("libPgCalInstance.so");
  
  cout<<endl;
  cout<<"Database setting"<<endl;
  cout<<"ODBCINI : "<<gSystem->Getenv("ODBCINI")<<endl;
  cout<<"PGHOST  : "<<gSystem->Getenv("PGHOST")<<endl;


  SvxBeamCenterPar *par = new SvxBeamCenterPar();
  par->Verbosity(99); 

  ifstream fin;
  fin.open(filename);
  if(!fin){
    cout << "can not open : " << filename << endl;
    return ;
  }

  Int_t n_ana=0;
  Int_t run[MAXBUF]={-1000};
  Int_t seg[MAXBUF]={-1000.};
  Double_t beamx_const[MAXBUF]={-1000.};
  Double_t beamy_const[MAXBUF]={-1000.};
  Double_t beamx_mean[MAXBUF]={-1000.};
  Double_t beamy_mean[MAXBUF]={-1000.};
  Double_t beamx_sigma[MAXBUF]={-1000.};
  Double_t beamy_sigma[MAXBUF]={-1000.};
  Double_t beamx_const_err[MAXBUF]={-1000.};
  Double_t beamy_const_err[MAXBUF]={-1000.};
  Double_t beamx_mean_err[MAXBUF]={-1000.};
  Double_t beamy_mean_err[MAXBUF]={-1000.};
  Double_t beamx_sigma_err[MAXBUF]={-1000.};
  Double_t beamy_sigma_err[MAXBUF]={-1000.};
  Int_t entry_x[MAXBUF];
  Int_t entry_x_err[MAXBUF];
  Int_t entry_y[MAXBUF];
  Int_t entry_y_err[MAXBUF];
  Bool_t status[MAXBUF]= {0};

  while(fin){
    fin >> run[n_ana]
        >> seg[n_ana] 
        >> beamx_const[n_ana]
        >> beamx_const_err[n_ana]
        >> beamx_mean[n_ana]
        >> beamx_mean_err[n_ana]
        >> beamx_sigma[n_ana]
        >> beamx_sigma_err[n_ana]
        >> beamy_const[n_ana]
        >> beamy_const_err[n_ana]
        >> beamy_mean[n_ana]
        >> beamy_mean_err[n_ana]
        >> beamy_sigma[n_ana]
        >> beamy_sigma_err[n_ana]
        >> entry_x[n_ana]
        >> entry_x_err[n_ana]
        >> entry_y[n_ana]
        >> entry_y_err[n_ana]
        >> status[n_ana];

    if(   (beamx_mean[n_ana] == 0 )
        ||(beamy_mean[n_ana] == 0 )
        ||(beamx_sigma[n_ana] == 0.05 )
        ||(beamx_sigma[n_ana] > 0.03 ) 
        ||(beamy_sigma[n_ana] == 0.05 )
        ||(beamy_sigma[n_ana] > 0.03 ) 
        ||(beamy_sigma[n_ana] <-0.0 ) 
        ||(fabs(beamx_sigma[n_ana]) <0.006 ) 
        ||(fabs(beamy_sigma[n_ana]) <0.006 ) 
        ||(fabs(beamx_const[n_ana]) <20 ) 
        ||(fabs(beamy_const[n_ana]) <20 )
        || status[n_ana] == 0 ){
      beamx_const[n_ana] = -1000;
      beamy_const[n_ana] = -1000;
    //  beamx_mean[n_ana] = -1000;
      beamx_mean[n_ana] = 0.0635 ;
      beamy_mean[n_ana] = -0.0552;    
      beamx_sigma[n_ana] = -1000;       
      beamy_sigma[n_ana] = -1000;       
      cout << "bad data or bad fit " << run[n_ana] << endl;
      }
    cout << n_ana << " "  << run[n_ana]   << " "  <<  beamx_mean[n_ana]  << "  " << beamx_mean_err[n_ana] <<"  " <<status[n_ana] << endl;
    par->setParameters(beamx_mean[n_ana], beamy_mean[n_ana],run[n_ana],119);
  //x ,y ,run ,geometry version
    par->print(); 
    RunToTime   *rt      = RunToTime::instance();
    PHTimeStamp *Tsearch = rt->getBeginTime(run[n_ana]);     printTime(Tsearch);
    PHTimeStamp *Tbegin = rt->getBeginTime(run[n_ana]); printTime(Tbegin);
    PHTimeStamp *Tend   = rt->getEndTime(run[n_ana]);     printTime(Tend);
    char *c_desc = "run11 Au+Au 200 GeV beam center position :Hidemitsu Asano";
    par->updateToDB(Tbegin, Tend, c_desc); 
//    PHTimeStamp *Tbegin = new PHTimeStamp(1999, 1,1,0,0,0); printTime(Tbegin);
//    PHTimeStamp *Tend   = new PHTimeStamp(2030,12,31,0,0,0); printTime(Tend);
    n_ana++;
  }//while
 
  n_ana--;
  cout << "number of inserted data " << endl;


}

void printTime(PHTimeStamp *time){
  if(time!=NULL){
    time->print();
    cout<<endl;
  } else {
    cout<<"TimeStamp is Null"<<endl;
  }
}
