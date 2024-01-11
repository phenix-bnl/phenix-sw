#include "MutrgDBIOTrkMap.hh"

#include <stdio.h>
#include <iostream>
#include <sstream>
#include <TSystem.h>

#include "PdbBankID.hh"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"

#include "PdbMutrgRpcTrk.hh"

using namespace std;

const string MutrgDBIOTrkMap::class_name="PdbMutrgRpcTrkBank";
const string MutrgDBIOTrkMap::calib_name="calib.mutrgrpc.track";

/////////////////////////////////////////////////////////////

MutrgDBIOTrkMap::MutrgDBIOTrkMap(){
  mutrg_map.clear();
  version=MutrgPar::TRKDV_UNKNOWN;
  description="";
  user_name="";
  time_start.set(2030,1,1,0,0,0);
  time_end.set(2030,1,1,0,0,0);
  time_search.set(2030,1,1,0,0,0);
}

/////////////////////////////////////////////////////////////

MutrgDBIOTrkMap::~MutrgDBIOTrkMap(){
  Reset();
}

/////////////////////////////////////////////////////////////

void MutrgDBIOTrkMap::Reset(){
  for(unsigned int i=0; i<mutrg_map.size(); i++){
    if(mutrg_map[i]){delete mutrg_map[i];}
  }
  mutrg_map.clear();
}

/////////////////////////////////////////////////////////////

int MutrgDBIOTrkMap::ReadFile(char *filename){
  const int NSTATION=PdbMutrgRpcTrk::NSTATION;
  const int NRADSEG=PdbMutrgRpcTrk::NRADSEG;
  const int NSTRIP_RPC=PdbMutrgRpcTrk::NSTRIP_RPC;
  const int NSTATION_RPC=3;

  Reset();

  FILE *fp=fopen(filename,"r");

  while(1){
    char line[1024];
    if(fgets(line,sizeof(line),fp)==0){break;}
    if(line[0]=='#'){continue;}

    stringstream ss(line);

    int arm,oct;
    ss >> arm;
    ss >> oct;

    int strip_mutrg[NSTATION];
    for(int ist=0; ist<NSTATION; ist++){ss >> strip_mutrg[ist];}
    if(ss.fail()){printf("Format is wrong\n");}

    bool flag_st[NSTATION_RPC];
    int module_rpc[NSTATION_RPC][NRADSEG];
    int strip_rpc[NSTATION_RPC][NRADSEG][NSTRIP_RPC]; // [st]
    for(int ist=0; ist<NSTATION_RPC; ist++){
      flag_st[ist]=false;
      for(int irad=0; irad<NRADSEG; irad++){
	module_rpc[ist][irad]=-1;
	for(int istp=0; istp<NSTRIP_RPC; istp++){
	  strip_rpc[ist][irad][istp]=-1;
	}
      }
    }

    for(int ist=0; ist<NSTATION_RPC; ist++){
      int st=0;
      ss >> st;
      if(ss.fail()){break;}

      if(st<0 || st>=NSTATION_RPC){
	printf("Error - MutrgDBIOTrkMap : Station number must be 0 or 2.\n");
	break;
      }

      for(int irad=0; irad<NRADSEG; irad++){
	ss >> module_rpc[st][irad];
	for(int ipos=0; ipos<NSTRIP_RPC; ipos++){
	  ss >> strip_rpc[st][irad][ipos];
	}
	//for(int ipos=0; ipos<3; ipos++){
	//ss >> strip_rpc[st][irad][ipos+1];
	//}
      }

      if(ss.fail()){printf("Format is wrong\n"); break;}
      flag_st[st]=true;
    }

    PdbMutrgRpcTrk *trk=new PdbMutrgRpcTrk();
    trk->SetArm(arm);
    trk->SetOctant(oct);
    for(int ist=0; ist<NSTATION; ist++){
      trk->SetStripMutrg(ist,strip_mutrg[ist]);}
    for(int ist=0; ist<NSTATION_RPC; ist++){
      if(!flag_st[ist]){continue;}
      for(int irad=0; irad<NRADSEG; irad++){
	trk->SetModuleRpc(ist,irad,module_rpc[ist][irad]);
	for(int ipos=0; ipos<NSTRIP_RPC; ipos++){
	  trk->SetStripRpc(ist,irad,ipos,strip_rpc[ist][irad][ipos]);
	}
      }
    }

    mutrg_map.push_back(trk);
  }

  fclose(fp);

  return 0;
}

/////////////////////////////////////////////////////////////////

void MutrgDBIOTrkMap::SetTimeStart(int year,int mon,int day,
				   int hour,int min,int sec){
  time_start.set(year,mon,day,hour,min,sec);
  return;
}

/////////////////////////////////////////////////////////////////

void MutrgDBIOTrkMap::SetTimeEnd(int year,int mon,int day,
				 int hour,int min,int sec){
  time_end.set(year,mon,day,hour,min,sec);
  return;
}

/////////////////////////////////////////////////////////////////

void MutrgDBIOTrkMap::SetTimeSearch(int year,int mon,int day,
				    int hour,int min,int sec){
  time_search.set(year,mon,day,hour,min,sec);
  return;
}

/////////////////////////////////////////////////////////////////

int MutrgDBIOTrkMap::ReadDB(){
  Reset();

  if(version==MutrgPar::TRKDV_UNKNOWN){
    printf("Error - MutrgDBIOTrkMap : Version is unknown (%d)\n",version);
    return -1;
  }

  PdbBankManager *bank_man=PdbBankManager::instance();

  PdbBankID bid(version);
  PdbCalBank *mutrg_bank=
    bank_man->fetchBank(class_name.c_str(),bid,calib_name.c_str(),time_search);

  if(!mutrg_bank){
    printf("Error - MutrgDBIOTrkMap : Couldn't get MuTRG bank\n");
    return -1;
  }

  time_insert=mutrg_bank->getInsertTime();
  time_start=mutrg_bank->getStartValTime();
  time_end=mutrg_bank->getEndValTime();
  description=mutrg_bank->getDescription().getString();
  user_name=mutrg_bank->getUserName().getString();

  for(unsigned int ich=0; ich<mutrg_bank->getLength(); ich++){
    PdbMutrgRpcTrk *trk=new PdbMutrgRpcTrk();
    *trk=static_cast<PdbMutrgRpcTrk&>(mutrg_bank->getEntry(ich));
    mutrg_map.push_back(trk);
  }

  delete mutrg_bank;

  return 0;
}

/////////////////////////////////////////////////////////////////

int MutrgDBIOTrkMap::WriteDB(){
  if(version==MutrgPar::TRKDV_UNKNOWN){
    printf("Error - MutrgDBIOTrkMap : Version is unknown (%d)\n",version);
    return -1;
  }

  if(description==""){
    printf("Error - MutrgDBIOTrkMap : Description is NULL\n");
    return -1;
  }

  if(user_name==""){
    printf("Error - MutrgDBIOTrkMap : User name is NULL\n");
    return -1;
  }

  if(time_start>=time_end){
    printf("Error - MutrgDBIOTrkMap : time_start and time_end are inconsistent\n");
    return -1;
  }

  PdbBankManager *bank_man=PdbBankManager::instance();
  if(!bank_man){
    printf("Error - MutrgDBIOTrkMap : No instance of PdbBankManager\n");
    return -1;
  }

  PdbApplication *pdb_app=bank_man->getApplication();
  if(!pdb_app->startUpdate()){
    printf("Error - MutrgDBIOTrkMap : Couldn't process pdb_app->startUpdate()\n");
    return -1;
  }

  PdbBankID bid(version);
  PdbCalBank *mutrg_bank
    =bank_man->createBank(class_name.c_str(),bid,description.c_str(),
                          time_start,time_end,calib_name.c_str());
  mutrg_bank->setUserName (user_name.c_str());

  mutrg_bank->setLength(mutrg_map.size());
  for(unsigned int ich=0; ich<mutrg_map.size(); ich++){
    PdbMutrgRpcTrk *ch=
      static_cast<PdbMutrgRpcTrk*>(&(mutrg_bank->getEntry(ich)));
    *ch=*mutrg_map[ich];
  }

  for(unsigned int ich=0; ich<mutrg_bank->getLength(); ich++){
    PdbMutrgRpcTrk *ch=
      static_cast<PdbMutrgRpcTrk*>(&(mutrg_bank->getEntry(ich)));
    ch->Write(cout,", "); cout << endl;
  }

  cout << "ODBCINI     : " << gSystem->Getenv("ODBCINI") << endl;
  cout << "User Name   : " << mutrg_bank->getUserName() << endl;
  cout << "Description : " << mutrg_bank->getDescription() << endl;
  cout << "Bank ID     : " << mutrg_bank->getBankID().getInternalValue() << endl;
  cout << "Bank ID2    : " << mutrg_bank->getBankID2().getInternalValue() << endl;
  cout << "Start Time  : " << mutrg_bank->getStartValTime() << endl;
  cout << "End Time    : " << mutrg_bank->getEndValTime() << endl;

  string commit_decision="";
  cout << "Commit to database (yes/no)?" << endl;
  cin >> commit_decision;

  if(commit_decision=="yes"){
    pdb_app->commit(mutrg_bank);
  }
  else{
    cout << "Abort" << endl;
  }


  return 0;
}

/////////////////////////////////////////////////////////////////

void MutrgDBIOTrkMap::print(ostream &os,string delim) const {
  for(unsigned int itrk=0; itrk<mutrg_map.size(); itrk++){
    if(itrk==0){mutrg_map[itrk]->WriteFormat(os); os << endl;}
    mutrg_map[itrk]->Write(os,delim);
    os << endl;
  }
  return;
}

//////////////////////////////////////////////////////////////////////
