#include <odbc++/connection.h>
#include <odbc++/drivermanager.h>
#include <odbc++/resultset.h>
#include <odbc++/resultsetmetadata.h>

#include "SpinDBInput.hh"

#include <cstdlib>

ClassImp(SpinDBInput);

using namespace odbc;
using namespace std;

const char* SpinDBInput::TABLE_NAME="spin";
const int SpinDBInput::ERROR_VALUE=-999;

///////////////////////////////////////////////////////

void SpinDBInput::Initialize(void){
  con=NULL;
  run_check=-1;
  qa_check=-1;

  try{con=DriverManager::getConnection(TABLE_NAME,"phnxrc","");}
  catch (SQLException& e){
    printf("Error : %s.\n",e.getMessage().c_str());
    exit(1);
  }

  if(con!=NULL){printf("Connected to %s DB.\n",TABLE_NAME);}

  return;
}

////////////////////////////////////////////////////////

void SpinDBInput::Delete(Option_t*) {
  delete con;
  con=NULL;
}

///////////////////////////////////////////////////////////

int SpinDBInput::IsConnected(void){
  if(con==NULL){
    printf("Error : No connection to the DB.\n");
    //    printf(" You should do SpinDBInput.Connection(user_name).\n");
    return(0);
  }

  return(1);
}

////////////////////////////////////////////////////////////

int SpinDBInput::CheckRunRow(int runnum,int qa_level,const char *opt){
  if(string("simple")==opt && run_check==runnum && qa_check==qa_level){return(1);}

  if(IsConnected()!=1){return(ERROR_VALUE);}

  stringstream cmd;
  cmd << "select * from spin where runnumber=" << runnum << " and qa_level=" << qa_level << ";";
  Statement *stmt=con->createStatement();
  ResultSet *rs=NULL;
  try{rs=stmt->executeQuery(cmd.str());}
  catch(SQLException &e){
    printf("Error : %s.\n",e.getMessage().c_str());
    delete rs;
    delete stmt;
    return(ERROR_VALUE);
  }

  int flag=0;
  if(rs->next()!=0){flag=1; run_check=runnum; qa_check=qa_level;}

  delete rs;
  delete stmt;
  return(flag);
}

//////////////////////////////////////////////////////////

int SpinDBInput::CreateRunRow(int runnum, int qa_level){
  if(CheckRunRow(runnum,qa_level,"simple")==1){
    printf("SpinDBInput::CreateRunRow() Error : Row for run %d with qa level %d seems to exist, check again.\n",runnum,qa_level);
    return(0);
  }

  stringstream cmd;
  cmd << "insert into spin (runnumber,qa_level) values(" << runnum << "," << qa_level << ");";
  Statement *stmt=con->createStatement();
  try{stmt->execute(cmd.str());}
  catch(SQLException &e){
    printf("Error : %s.\n",e.getMessage().c_str());
    return(ERROR_VALUE);
  }
  delete stmt;

  SpinDBContent spin_cont_temp;
  spin_cont_temp.SetRunNumber(runnum);
  spin_cont_temp.SetQALevel(qa_level);
  InitializeRunRow(spin_cont_temp);

  return(1);
}

///////////////////////////////////////////////////////////

int SpinDBInput::DeleteRunRow(int runnum, int qa_level){
  if(IsConnected()!=1){return(0);}

  if(CheckRunRow(runnum,qa_level,"simple")!=1){
    printf("SpinDBInput::DeleteRunRow() Error : Row for run %d qa level %d seems not to exist, check again.\n",runnum,qa_level);
    return(0);
  }

  stringstream cmd;
  cmd << "delete from spin where runnumber=" << runnum << " and qa_level=" << qa_level << ";";
  Statement *stmt=con->createStatement();
  try{stmt->execute(cmd.str());}
  catch(SQLException &e){
    printf("Error : %s.\n",e.getMessage().c_str());
    delete stmt;
    return(ERROR_VALUE);
  }

  run_check=-1;
  qa_check=-1;
  delete stmt;
  return(1);
}

////////////////////////////////////////////////////////////

int SpinDBInput::CheckQARunRow(int runnum){
  if(IsConnected()!=1){return(ERROR_VALUE);}

  stringstream cmd;
  cmd << "select * from spin_default where runnumber=" << runnum << ";";
  Statement *stmt=con->createStatement();
  ResultSet *rs=NULL;
  try{rs=stmt->executeQuery(cmd.str());}
  catch(SQLException &e){
    printf("Error : %s.\n",e.getMessage().c_str());
    delete rs;
    delete stmt;
    return(ERROR_VALUE);
  }

  int flag=0;
  if(rs->next()!=0){flag=1;}

  delete rs;
  delete stmt;
  return(flag);
}

//////////////////////////////////////////////////////////

int SpinDBInput::CreateQARunRow(int runnum){
  if(IsConnected()!=1){return(0);}

  stringstream cmd;
  cmd << "insert into spin_default (runnumber,default_qa_level) values(" << runnum << "," << ERROR_VALUE << ");";
  Statement *stmt=con->createStatement();
  try{stmt->execute(cmd.str());}
  catch(SQLException &e){
    printf("Error : %s.\n",e.getMessage().c_str());
    return(ERROR_VALUE);
  }
  delete stmt;

  return(1);
}


///////////////////////////////////////////////////////////

int SpinDBInput::DeleteQARunRow(int runnum){
  if(IsConnected()!=1){return(0);}

  if(CheckQARunRow(runnum)!=1){
    printf("SpinDBInput::DeleteQARunRow() Error : Row for run %d seems not to exist in spin_default, check again.\n",runnum);
    return(0);
  }

  stringstream cmd;
  cmd << "delete from spin_default where runnumber=" << runnum << ";";
  Statement *stmt=con->createStatement();
  try{stmt->execute(cmd.str());}
  catch(SQLException &e){
    printf("Error : %s.\n",e.getMessage().c_str());
    delete stmt;
    return(ERROR_VALUE);
  }

  run_check=-1;
  qa_check=-1;
  delete stmt;
  return(1);
}

/////////////////////////////////////////////////////////

int SpinDBInput::SetQADefault(int runnum,int qa_level){
  if(IsConnected()!=1){return(0);}

  if(CheckQARunRow(runnum)!=1){
    printf("Row for runnumber %d doesn't exist in spin_default! Creating it.\n",runnum);
    CreateQARunRow(runnum);
  }

  stringstream cmd;
  cmd << "update spin_default set default_qa_level=" << qa_level;
  cmd << " where runnumber=" << runnum << ";";

  Statement *stmt=con->createStatement();
  try{stmt->execute(cmd.str());}
  catch(SQLException &e){
    printf("Error : %s.\n",e.getMessage().c_str());
    delete stmt;
    return(ERROR_VALUE);
  }
  delete stmt;

  return(1);
}

///////////////////////////////////////////////////////////

int SpinDBInput::InitializeRunRow(SpinDBContent spin_cont){
  if(IsConnected()!=1){return(0);}

  if(CheckRunRow(spin_cont.GetRunNumber(),spin_cont.GetQALevel(),"simple")!=1){
    printf("SpinDBInput::UpdateDBContent() Error : Row for run %d qa level %d seems not to exist, check again.\n",
	   spin_cont.GetRunNumber(),spin_cont.GetQALevel());
    return(0);
  }

  int runnum=spin_cont.GetRunNumber();
  int qa_level=spin_cont.GetQALevel();

  int ncross=spin_cont.GetNCrossing();

  InitializeValue(runnum,qa_level,"fillnumber");
  InitializeValue(runnum,qa_level,"badrunqa");
  InitializeValue(runnum,qa_level,"crossingshift");

  InitializeArray(runnum,qa_level,"polarblue",ncross);
  InitializeArray(runnum,qa_level,"polarblueerror",ncross);
  InitializeArray(runnum,qa_level,"polarblueerrorsys",ncross);
  InitializeArray(runnum,qa_level,"polaryellow",ncross);
  InitializeArray(runnum,qa_level,"polaryellowerror",ncross);
  InitializeArray(runnum,qa_level,"polaryellowerrorsys",ncross);
  InitializeArray(runnum,qa_level,"spinpatternblue",ncross);
  InitializeArray(runnum,qa_level,"spinpatternyellow",ncross);
  InitializeArray(runnum,qa_level,"bbcvertexcut",ncross);
  InitializeArray(runnum,qa_level,"bbcwithoutcut",ncross);
  InitializeArray(runnum,qa_level,"zdcnarrow",ncross);
  InitializeArray(runnum,qa_level,"zdcwide",ncross);
  InitializeArray(runnum,qa_level,"badbunchqa",ncross);

  InitializeValue(runnum,qa_level,"transversxblue");
  InitializeValue(runnum,qa_level,"transversxblueerr");
  InitializeValue(runnum,qa_level,"transversyblue");
  InitializeValue(runnum,qa_level,"transversyblueerr");
  InitializeValue(runnum,qa_level,"transversxyellow");
  InitializeValue(runnum,qa_level,"transversxyellowerr");
  InitializeValue(runnum,qa_level,"transversyyellow");
  InitializeValue(runnum,qa_level,"transversyyellowerr");

  return(1);
}

///////////////////////////////////////////////////////////

int SpinDBInput::UpdateDBContent(SpinDBContent spin_cont){
  if(IsConnected()!=1){return(0);}

  if(CheckRunRow(spin_cont.GetRunNumber(),spin_cont.GetQALevel(),"simple")!=1){
    printf("SpinDBInput::UpdateDBContent() Error : Row for run %d qa level %d seems not to exist, check again.\n",
	   spin_cont.GetRunNumber(),spin_cont.GetQALevel());
    return(0);
  }

  int qa_level=spin_cont.GetQALevel();

  if(qa_level==ERROR_VALUE){
    printf("You did not set a qa_level.  Please do so with SpinDBContent::SetQALevel(int qa_level).  Check that the qa level you set does not exist for this run before trying again.\n");
    return(0);
  }

  int runnum=spin_cont.GetRunNumber();
  int ncross=spin_cont.GetNCrossing();
  int fillnum=spin_cont.GetFillNumber();
  int badrun=spin_cont.GetBadRunFlag();
  int xingshift=spin_cont.GetCrossingShift();
  if(fillnum!=ERROR_VALUE)   UpdateValue(runnum,qa_level,"fillnumber",fillnum);
  if(badrun!=ERROR_VALUE)    UpdateValue(runnum,qa_level,"badrunqa",badrun);
  if(xingshift!=ERROR_VALUE) UpdateValue(runnum,qa_level,"crossingshift",xingshift);

  float bpol[ncross],bpolerr[ncross],bpolsys[ncross],ypol[ncross],ypolerr[ncross],ypolsys[ncross];
  int bpat[ncross],ypat[ncross],bad_bunch[ncross];
  long long bbc_vtxcut[ncross],bbc_nocut[ncross];
  long long zdc_narrow[ncross],zdc_wide[ncross];
  for(int i=0; i<ncross; i++){
    spin_cont.GetPolarizationBlue(i,bpol[i],bpolerr[i],bpolsys[i]);
    spin_cont.GetPolarizationYellow(i,ypol[i],ypolerr[i],ypolsys[i]);
    bpat[i]=spin_cont.GetSpinPatternBlue(i);
    ypat[i]=spin_cont.GetSpinPatternYellow(i);
    bbc_vtxcut[i]=spin_cont.GetScalerBbcVertexCut(i);
    bbc_nocut[i]=spin_cont.GetScalerBbcNoCut(i);
    zdc_wide[i]=spin_cont.GetScalerZdcWide(i);
    zdc_narrow[i]=spin_cont.GetScalerZdcNarrow(i);
    bad_bunch[i]=spin_cont.GetBadBunchFlag(i);
  }

  bool cbpol=false;
  bool cbpolerr=false;
  bool cbpolsys=false;
  bool cypol=false;
  bool cypolerr=false;
  bool cypolsys=false;
  bool cbpat=false;
  bool cypat=false;
  bool cbbc_vtxcut=false;
  bool cbbc_nocut=false;
  bool czdc_wide=false;
  bool czdc_narrow=false;
  bool cbad_bunch=false;
  
  for( int i=0; i<ncross;i++){
    if(bpol[i]!=ERROR_VALUE)       cbpol=true;
    if(bpolerr[i]!=ERROR_VALUE)    cbpolerr=true;
    if(bpolsys[i]!=ERROR_VALUE)    cbpolsys=true;
    if(ypol[i]!=ERROR_VALUE)       cypol=true;
    if(ypolerr[i]!=ERROR_VALUE)    cypolerr=true;
    if(ypolsys[i]!=ERROR_VALUE)    cypolsys=true;
    if(bpat[i]!=ERROR_VALUE)       cbpat=true;
    if(ypat[i]!=ERROR_VALUE)       cypat=true;
    if(bbc_vtxcut[i]!=ERROR_VALUE) cbbc_vtxcut=true;
    if(bbc_nocut[i]!=ERROR_VALUE)  cbbc_nocut=true;
    if(zdc_wide[i]!=ERROR_VALUE)   czdc_wide=true;
    if(zdc_narrow[i]!=ERROR_VALUE) czdc_narrow=true;
    if(bad_bunch[i]!=ERROR_VALUE)  cbad_bunch=true;
  }

  if(cbpol)       UpdateArray(runnum,qa_level,"polarblue",bpol,ncross);
  if(cbpolerr)    UpdateArray(runnum,qa_level,"polarblueerror",bpolerr,ncross);
  if(cbpolsys)    UpdateArray(runnum,qa_level,"polarblueerrorsys",bpolsys,ncross);
  if(cypol)       UpdateArray(runnum,qa_level,"polaryellow",ypol,ncross);
  if(cypolerr)    UpdateArray(runnum,qa_level,"polaryellowerror",ypolerr,ncross);
  if(cypolsys)    UpdateArray(runnum,qa_level,"polaryellowerrorsys",ypolsys,ncross);
  if(cbpat)       UpdateArray(runnum,qa_level,"spinpatternblue",bpat,ncross);
  if(cypat)       UpdateArray(runnum,qa_level,"spinpatternyellow",ypat,ncross);
  if(cbbc_vtxcut) UpdateArray(runnum,qa_level,"bbcvertexcut",bbc_vtxcut,ncross);
  if(cbbc_nocut)  UpdateArray(runnum,qa_level,"bbcwithoutcut",bbc_nocut,ncross);
  if(czdc_narrow) UpdateArray(runnum,qa_level,"zdcnarrow",zdc_narrow,ncross);
  if(czdc_wide)   UpdateArray(runnum,qa_level,"zdcwide",zdc_wide,ncross);
  if(cbad_bunch)  UpdateArray(runnum,qa_level,"badbunchqa",bad_bunch,ncross);

  float tc_bx,tc_bx_err,tc_by,tc_by_err;
  float tc_yx,tc_yx_err,tc_yy,tc_yy_err;
  spin_cont.GetTransCompBlueX(tc_bx,tc_bx_err);
  spin_cont.GetTransCompBlueY(tc_by,tc_by_err);
  spin_cont.GetTransCompYellowX(tc_yx,tc_yx_err);
  spin_cont.GetTransCompYellowY(tc_yy,tc_yy_err);
  if(tc_bx!=ERROR_VALUE)     UpdateValue(runnum,qa_level,"transversxblue",tc_bx);
  if(tc_bx_err!=ERROR_VALUE) UpdateValue(runnum,qa_level,"transversxblueerr",tc_bx_err);
  if(tc_by!=ERROR_VALUE)     UpdateValue(runnum,qa_level,"transversyblue",tc_by);
  if(tc_by_err!=ERROR_VALUE) UpdateValue(runnum,qa_level,"transversyblueerr",tc_by_err);
  if(tc_yx!=ERROR_VALUE)     UpdateValue(runnum,qa_level,"transversxyellow",tc_yx);
  if(tc_yx_err!=ERROR_VALUE) UpdateValue(runnum,qa_level,"transversxyellowerr",tc_yx_err);
  if(tc_yy!=ERROR_VALUE)     UpdateValue(runnum,qa_level,"transversyyellow",tc_yy);
  if(tc_yy_err!=ERROR_VALUE) UpdateValue(runnum,qa_level,"transversyyellowerr",tc_yy_err);

  return(1);
}

/////////////////////////////////////////////////////////

int SpinDBInput::UpdateValue(int runnum,int qa_level,const char *cmd){
  if(IsConnected()!=1){return(0);}

  if(CheckRunRow(runnum,qa_level,"simple")!=1){
    printf("SpinDBInput::UpdateValue() Error : Row for run %d qa level %d seems not to exist, check again.\n",runnum,qa_level);
    return(0);
  }

  Statement *stmt=con->createStatement();
  try{stmt->execute(cmd);}
  catch(SQLException &e){
    printf("Error : %s.\n",e.getMessage().c_str());
    delete stmt;
    return(ERROR_VALUE);
  }
  delete stmt;
  printf("%s\n",cmd);

  return(1);
}

////////////////////////////////////////////////////////

template<class T>
int SpinDBInput::UpdateValueTemp(int runnum,int qa_level,const char *name,T value){
  stringstream cmd;
  cmd << "update " << TABLE_NAME << " set " << name << "=" << value;
  cmd << " where runnumber=" << runnum << " and qa_level=" << qa_level << ";";
  return(UpdateValue(runnum,qa_level,cmd.str().c_str()));
}

//////////////////////////////////////////////////////////

int SpinDBInput::UpdateValue(int runnum,int qa_level,const char *name,int value){
  return(UpdateValueTemp(runnum,qa_level,name,value));
}

//////////////////////////////////////////////////////////

int SpinDBInput::UpdateValue(int runnum,int qa_level,const char *name,float value){
  return(UpdateValueTemp(runnum,qa_level,name,value));
}

//////////////////////////////////////////////////////////

int SpinDBInput::UpdateValue(int runnum,int qa_level,const char *name,double value){
  return(UpdateValueTemp(runnum,qa_level,name,value));
}

//////////////////////////////////////////////////////////

template<class T>
int SpinDBInput::UpdateArrayTemp(int runnum,int qa_level,const char *name,T *value,int nvalue){
  stringstream cmd;
  cmd << "update " << TABLE_NAME << " set " << name << "='{";
  for(int i=0; i<nvalue; i++){
    cmd << value[i];
    if(i<nvalue-1){cmd << ",";}
  }
  cmd << "}' where runnumber=" << runnum << " and qa_level=" << qa_level << ";";
  return(UpdateValue(runnum,qa_level,cmd.str().c_str()));
}

/////////////////////////////////////////////////////////////////

int SpinDBInput::UpdateArray(int runnum,int qa_level,const char *name,int *value,int nvalue){
  return(UpdateArrayTemp(runnum,qa_level,name,value,nvalue));
}

/////////////////////////////////////////////////////////

int SpinDBInput::UpdateArray(int runnum,int qa_level,const char *name,float *value,int nvalue){
  return(UpdateArrayTemp(runnum,qa_level,name,value,nvalue));
}

/////////////////////////////////////////////////////////

int SpinDBInput::UpdateArray(int runnum,int qa_level,const char *name,double *value,int nvalue){
  return(UpdateArrayTemp(runnum,qa_level,name,value,nvalue));
}

/////////////////////////////////////////////////////////

int SpinDBInput::UpdateArray(int runnum,int qa_level,const char *name,
			     unsigned int *value,int nvalue){
  return(UpdateArrayTemp(runnum,qa_level,name,value,nvalue));
}

/////////////////////////////////////////////////////////

int SpinDBInput::UpdateArray(int runnum,int qa_level,const char *name,
			     long long *value,int nvalue){
  return(UpdateArrayTemp(runnum,qa_level,name,value,nvalue));
}

//////////////////////////////////////////////////////////

int SpinDBInput::InitializeValue(int runnum,int qa_level,const char *name){
  return(UpdateValue(runnum,qa_level,name,ERROR_VALUE));
}

//////////////////////////////////////////////////////////

int SpinDBInput::InitializeArray(int runnum,int qa_level,const char *name,int nvalue){
  int ERROR_ARRAY[nvalue];
  for(Int_t i=0;i<nvalue;i++) ERROR_ARRAY[i]=ERROR_VALUE;

  return(UpdateArray(runnum,qa_level,name,ERROR_ARRAY,nvalue));
}

////////////////////////////////////////////////////////////
