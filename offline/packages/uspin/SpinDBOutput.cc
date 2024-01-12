#include "SpinDBOutput.hh"

#include <odbc++/connection.h>
#include <odbc++/drivermanager.h>
#include <odbc++/resultset.h>
#include <odbc++/resultsetmetadata.h>

#include <cstdlib>
#include <iostream>
#include <sstream>

ClassImp(SpinDBOutput);

using namespace odbc;
using namespace std;

const int SpinDBOutput::ERROR_VALUE=-999;

//////////////////////////////////////////////////////////

void SpinDBOutput::Initialize(void){

  // default get from spin table from spin DB
  db_name="spin";
  user_name="";
  table_name="spin";

  spin_cont_store.clear();
  return;
}

//////////////////////////////////////////////////////////

void SpinDBOutput::SetDBName(const char *dbname){

  // Other DB name is existing "spin_readonly"
  // See /opt/phenix/etc/odbc.ini
  if( strcmp(dbname,"spin_readonly") == 0 )
    {
      printf(" Database name is changed from %s to %s\n",
	     db_name.c_str(),dbname);
      db_name=dbname; 
    }
  else
    {    
      printf(" Your input database name, %s, is invalid\n", dbname);
      printf(" No change, try it as default, %s\n",db_name.c_str());
    }

  return;
}

//////////////////////////////////////////////////////////

void SpinDBOutput::SetTableName(const char *tname){

  // Other table is existing "spin_oncal"
  if( !strcmp(tname,"spin_oncal") || !strcmp(tname,"spin_daq") || !strcmp(tname,"spin_default") || !strcmp(tname,"spin"))
    {
      printf(" Table_name is changed from %s to %s\n",
	     table_name.c_str(),tname);
      table_name=tname; 
    }
  else
    {    
      printf(" Your input table name, %s, is invalid\n", tname);
      printf(" No change, try it as default, %s\n",table_name.c_str());
    }

  return;
}

//////////////////////////////////////////////////////////

Connection *SpinDBOutput::ConnectDB(void){
  Connection *con=NULL;
  try{con=DriverManager::getConnection(db_name,user_name,"");}
  catch (SQLException& e){
    printf("Error : %s.\n",e.getMessage().c_str());
    return(NULL);
  }

  return(con);
}

/////////////////////////////////////////////////////////////

int SpinDBOutput::PrintDBColumn(void){
  Connection *con=ConnectDB();
  if(con==NULL){return(0);}

  stringstream cmd;
  cmd << "select * from " << table_name << ";";
  Statement *stmt=con->createStatement();
  ResultSet *rs=NULL;
  try{rs=stmt->executeQuery(cmd.str());}
  catch(SQLException &e){
    printf("Error : %s\n",e.getMessage().c_str());
    delete rs;
    delete stmt;
    delete con;
    return(ERROR_VALUE);
  }

  int ncol=rs->getMetaData()->getColumnCount();
  for(int icol=0; icol<ncol; icol++){
    int col_type=rs->getMetaData()->getColumnType(icol+1);
    string col_type_name=rs->getMetaData()->getColumnTypeName(icol+1);
    string col_name=rs->getMetaData()->getColumnName(icol+1);
    printf("%2d : %2d %7s %s\n",icol,col_type,
	   col_type_name.c_str(),col_name.c_str());
  }

  delete rs;
  delete stmt;
  delete con;
  return(1);
}

///////////////////////////////////////////////////////////

int SpinDBOutput::PrintDBRawContent(int runnum, int qa_level){
  Connection *con=ConnectDB();
  if(con==NULL){return(0);}

  if(qa_level==QA_ERROR_VALUE){
    qa_level = GetDefaultQA(runnum);
  }

  stringstream cmd;
  cmd << "select * from " << table_name << " where runnumber=" << runnum << " and qa_level=" << qa_level << ";";
  Statement *stmt=con->createStatement();
  ResultSet *rs=NULL;
  try{rs=stmt->executeQuery(cmd.str());}
  catch(SQLException &e){
    printf("Error : %s\n",e.getMessage().c_str());
    delete rs;
    delete stmt;
    delete con;
    return(ERROR_VALUE);
  }

  if(rs->next()==0){
    printf("Error : Can't find data for run %d (with qa_level %d)\n",runnum,qa_level);
    delete rs;
    delete stmt;
    delete con;
    return(0);
  }

  int ncol=rs->getMetaData()->getColumnCount();
  for(int icol=0; icol<ncol; icol++){
    string col_name=rs->getMetaData()->getColumnName(icol+1);
    string cont=rs->getString(col_name);
    printf("%2d : %s = %s\n",icol,col_name.c_str(),cont.c_str());
  }

  delete rs;
  delete stmt;
  delete con;
  return(1);
}

///////////////////////////////////////////////////////////

int SpinDBOutput::CheckRunRow(int runnum, int qa_level){
  if(spin_cont_store1.GetRunNumber()==runnum){return(1);}
  if(CheckRunRowStore(runnum)==1){return(1);}

  Connection *con=ConnectDB();
  if(con==NULL){
    printf("No Connection to the Database!\n");
    return(0);}

  if(qa_level==QA_ERROR_VALUE){
    qa_level = GetDefaultQA(runnum);
  }

  stringstream cmd;
  cmd << "select * from " << table_name << " where runnumber=" << runnum << " and qa_level=" << qa_level << ";";
  Statement *stmt=con->createStatement();
  ResultSet *rs=NULL;
  try{rs=stmt->executeQuery(cmd.str());}
  catch(SQLException& e){
    printf("Error : %s\n",e.getMessage().c_str());
    delete rs;
    delete stmt;
    delete con;
    return(ERROR_VALUE);
  }

  if(rs->next()==0){
    delete rs;
    delete stmt;
    delete con;
    return(0);
  }

  GetDBContent(spin_cont_store1,rs);

  delete rs;
  delete stmt;
  delete con;

  return(1);
}

///////////////////////////////////////////////////////////

int SpinDBOutput::CheckRunRowStore(int runnum){
  map<int,SpinDBContent>::iterator it=spin_cont_store.find(runnum);
  if(it==spin_cont_store.end()){return 0;}
  spin_cont_store1=it->second;
  return 1;
}

/////////////////////////////////////////////////////////////

int SpinDBOutput::StoreDBContent(int run1,int run2,int qa_level){
  Connection *con=ConnectDB();
  if(con==NULL){return(0);}

  stringstream cmd;

  if(qa_level==QA_ERROR_VALUE){
    cmd << "select * from " << table_name << ", spin_default where " << table_name << ".runnumber>" << run1-1;
    cmd << " and " << table_name << ".runnumber<" << run2+1;
    cmd << " and spin_default.runnumber>" << run1-1;
    cmd << " and spin_default.runnumber<" << run2+1;
    cmd << " and default_qa_level=qa_level;";
  }
  else{
    cmd << "select * from " << table_name << " where runnumber>" << run1-1;
    cmd << " and runnumber<" << run2+1 << " and qa_level=" << qa_level << ";";
  }

  Statement *stmt=con->createStatement();
  ResultSet *rs=NULL;
  try{rs=stmt->executeQuery(cmd.str());}
  catch(SQLException& e){
    printf("Error : %s\n",e.getMessage().c_str());
    delete rs;
    delete stmt;
    delete con;
    return(ERROR_VALUE);
  }

  while(1){
    SpinDBContent spin_cont;
    if(rs->next()==0){break;}
    GetDBContent(spin_cont,rs);

    int runnum=spin_cont.GetRunNumber();
    map<int,SpinDBContent>::iterator it=spin_cont_store.find(runnum);
    if(it!=spin_cont_store.end()){(it->second)=spin_cont;}
    else{spin_cont_store.insert(pair<int,SpinDBContent>(runnum,spin_cont));}
  }

  delete rs;
  delete stmt;
  delete con;

  return(1);
}

/////////////////////////////////////////////////////////////////

void SpinDBOutput::ClearDBContent(void){
  spin_cont_store.clear();
  return;
}

//////////////////////////////////////////////////////////////

int SpinDBOutput::GetDBContent(SpinDBContent &spin_cont,int runnum,int qa_level){
  if(spin_cont_store1.GetRunNumber()==runnum){
    spin_cont=spin_cont_store1;
    return 1;
  }

  map<int,SpinDBContent>::iterator it=spin_cont_store.find(runnum);
  if(it!=spin_cont_store.end()){spin_cont=it->second; return 1;}

  Connection *con=ConnectDB();
  if(con==NULL){return(0);}

  if(qa_level==QA_ERROR_VALUE){
    qa_level = GetDefaultQA(runnum);
  }

  stringstream cmd;
  cmd << "select * from " << table_name << " where runnumber=" << runnum << " with qa_level=" << qa_level << ";";
  Statement *stmt=con->createStatement();
  ResultSet *rs=NULL;
  try{rs=stmt->executeQuery(cmd.str());}
  catch(SQLException& e){
    printf("Error : %s\n",e.getMessage().c_str());
    delete rs;
    delete stmt;
    delete con;
    return(ERROR_VALUE);
  }

  if(rs->next()==0){
    printf("Error : Can't find data for run %d (and qa level %d)\n",runnum,qa_level);
    delete rs;
    delete stmt;
    delete con;
    return(0);
  }

  GetDBContent(spin_cont,rs);

  delete rs;
  delete stmt;
  delete con;

  return(1);
}

/////////////////////////////////////////////////////////////

int SpinDBOutput::GetDBContentStore(SpinDBContent &spin_cont,int runnum){
  if(spin_cont_store1.GetRunNumber()==runnum){
    spin_cont=spin_cont_store1;
    return 1;
  }

  map<int,SpinDBContent>::iterator it=spin_cont_store.find(runnum);
  if(it!=spin_cont_store.end()){spin_cont=it->second; return 1;}

  printf("Error : Can't find row for run %d.\n",runnum);

  return 0;
}

//////////////////////////////////////////////////////////////

int SpinDBOutput::GetDBContent(SpinDBContent &spin_cont,ResultSet *rs){
  int ncross=spin_cont.GetNCrossing();

  spin_cont.SetRunNumber(rs->getInt("runnumber"));
  spin_cont.SetQALevel(rs->getInt("qa_level"));
  spin_cont.SetFillNumber(rs->getInt("fillnumber"));
  spin_cont.SetBadRunFlag(rs->getInt("badrunqa"));
  spin_cont.SetCrossingShift(rs->getInt("crossingshift"));

  float bpol[ncross],bpolerr[ncross],bpolsys[ncross],ypol[ncross],ypolerr[ncross],ypolsys[ncross];
  int bpat[ncross],ypat[ncross],bad_bunch[ncross];
  long long bbc_vtxcut[ncross],bbc_nocut[ncross];
  long long zdc_wide[ncross],zdc_narrow[ncross];

  GetArray(rs,"polarblue",bpol,ncross);
  GetArray(rs,"polarblueerror",bpolerr,ncross);
  if( table_name == "spin" ){
    GetArray(rs,"polarblueerrorsys",bpolsys,ncross);
  }
  GetArray(rs,"polaryellow",ypol,ncross);
  GetArray(rs,"polaryellowerror",ypolerr,ncross);
  if( table_name == "spin" ){
    GetArray(rs,"polaryellowerrorsys",ypolsys,ncross);
  }
  GetArray(rs,"spinpatternblue",bpat,ncross);
  GetArray(rs,"spinpatternyellow",ypat,ncross);
  GetArray(rs,"bbcvertexcut",bbc_vtxcut,ncross);
  GetArray(rs,"bbcwithoutcut",bbc_nocut,ncross);
  GetArray(rs,"zdcwide",zdc_wide,ncross);
  GetArray(rs,"zdcnarrow",zdc_narrow,ncross);
  GetArray(rs,"badbunchqa",bad_bunch,ncross);

  for(int i=0; i<ncross; i++){
    spin_cont.SetPolarizationBlue(i,bpol[i],bpolerr[i],bpolsys[i]);
    spin_cont.SetPolarizationYellow(i,ypol[i],ypolerr[i],ypolsys[i]);
    spin_cont.SetSpinPatternBlue(i,bpat[i]);
    spin_cont.SetSpinPatternYellow(i,ypat[i]);
    spin_cont.SetScalerBbcVertexCut(i,bbc_vtxcut[i]);
    spin_cont.SetScalerBbcNoCut(i,bbc_nocut[i]);
    spin_cont.SetScalerZdcWide(i,zdc_wide[i]);
    spin_cont.SetScalerZdcNarrow(i,zdc_narrow[i]);
    spin_cont.SetBadBunchFlag(i,bad_bunch[i]);
  }

  spin_cont.SetTransCompBlueX(rs->getFloat("transversxblue"),
			      rs->getFloat("transversxblueerr"));
  spin_cont.SetTransCompBlueY(rs->getFloat("transversyblue"),
			      rs->getFloat("transversyblueerr"));  
  spin_cont.SetTransCompYellowX(rs->getFloat("transversxyellow"),
				rs->getFloat("transversxyellowerr"));
  spin_cont.SetTransCompYellowY(rs->getFloat("transversyyellow"),
				rs->getFloat("transversyyellowerr"));

  return(1);
}

///////////////////////////////////////////////////////////////

int SpinDBOutput::GetArray(ResultSet *rs,const char *name,vector<string> &value){
  string cvalue = "";
  try{cvalue=rs->getString(name);}
  catch(SQLException &e){
    printf("Error : %s.\n",e.getMessage().c_str());
    return(ERROR_VALUE);
  }
  
  int length = cvalue.size();
  if(length)
    {
      if(cvalue.compare(0,1,"{")!=0||cvalue.compare(length-1,1,"}")!=0)
	{
	  printf("Error : Is this array? (%s), length = %d\n",name, length);
	  return 0;
	}
    }
  else
    {
      printf("Error : Is this array? (%s), length = %d\n",name, length);
      return 0;
    }

  cvalue=cvalue.substr(1,cvalue.size()-1)+",";

  int nvalue=0;
  vector<string> value1;
  while(1){
    size_t pos=cvalue.find(string(","));
    if(pos==cvalue.npos){break;}

    value1.push_back(cvalue.substr(0,pos));
    cvalue.erase(0,pos+1);
    nvalue++;
  }

  stringstream cerror;
  cerror << SpinDBContent::GetErrorValue();
  for(int i=0; i<(int)value.size(); i++){
    if(i<(int)value1.size()){value[i]=value1[i];}
    else{value[i]=cerror.str();}
  }

  if(value1.size()!=value.size()){
    cout << "Error : Number of values are inconsistent with expected." << endl;
    cout << " (" << name << " : " << value1.size() << " != " 
	 << value.size() << ")" << endl;
    return(0);
  }

  return(1);
}

//////////////////////////////////////////////////////////////

int SpinDBOutput::GetArray(ResultSet *rs,const char *name,float *value,int nvalue){
  vector<string> svalue(nvalue,"");
  int ret=GetArray(rs,name,svalue);
  for(int i=0; i<nvalue; i++){value[i]=atof(svalue[i].c_str());}
  return(ret);
}

/////////////////////////////////////////////////////////////////

int SpinDBOutput::GetArray(ResultSet *rs,const char *name,
			   unsigned int *value,int nvalue){
  vector<string> svalue(nvalue,"");
  int ret=GetArray(rs,name,svalue);
  for(int i=0; i<nvalue; i++){value[i]=(unsigned int)atoi(svalue[i].c_str());}
  return(ret);
}

/////////////////////////////////////////////////////////////////

int SpinDBOutput::GetArray(ResultSet *rs,const char *name,int *value,int nvalue){
  vector<string> svalue(nvalue,"");
  int ret=GetArray(rs,name,svalue);
  for(int i=0; i<nvalue; i++){value[i]=atoi(svalue[i].c_str());}
  return(ret);
}

/////////////////////////////////////////////////////////////////

int SpinDBOutput::GetArray(ResultSet *rs,const char *name,
			   long long *value,int nvalue){
  vector<string> svalue(nvalue,"");
  int ret=GetArray(rs,name,svalue);
  for(int i=0; i<nvalue; i++){value[i]=0;}
  for(int i=0; i<nvalue; i++){sscanf(svalue[i].c_str(),"%lld",&value[i]);}
  return(ret);
}

/////////////////////////////////////////////////////////////////

int SpinDBOutput::GetDefaultQA(int runnum){

  Connection *con=ConnectDB();
  if(con==NULL){return(0);}

  stringstream cmd;
  cmd << "select default_qa_level from spin_default where runnumber=" << runnum << ";";
  Statement *stmt=con->createStatement();
  ResultSet *rs=NULL;
  try{rs=stmt->executeQuery(cmd.str());}
  catch(SQLException& e){
    printf("Error : %s\n",e.getMessage().c_str());
    delete rs;
    delete stmt;
    delete con;
    return(ERROR_VALUE);
  }

  if(rs->next()==0){
    printf("Error : Can't find data for run %d\n",runnum);
    delete rs;
    delete stmt;
    delete con;
    return(0);
  }

  int default_qa_level = rs->getInt("default_qa_level");

  delete rs;
  delete stmt;
  delete con;

  return(default_qa_level);

}
