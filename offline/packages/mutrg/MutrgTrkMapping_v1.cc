#include "MutrgTrkMapping_v1.hh"

#include <fstream>
#include <iomanip>

#include "RunToTime.hh"
#include "PdbMutrgRpcTrk.hh"
#include "MutrgDBIOTrkMap.hh"

using namespace std;

///////////////////////////////////////////////////////////

MutrgTrkMapping_v1::MutrgTrkMapping_v1(void){
  class_name="MutrgTrkMapping_v1";
  Reset();
}

///////////////////////////////////////////////////////////

MutrgTrkMapping_v1::~MutrgTrkMapping_v1(void){
  Reset();
}

/////////////////////////////////////////////////////////

int MutrgTrkMapping_v1::Reset(void){
  for(int iarm=0; iarm<MutrgPar::NARM; iarm++){
    for(int ioct=0; ioct<MutrgPar::NOCTANT; ioct++){
      for(int istrip=0; istrip<MutrgPar::MAX_NSTRIP_IN_OCTANT; istrip++){
	mapping[iarm][ioct][istrip].clear();
      }
    }
  }

  return 0;
}

///////////////////////////////////////////////////////////

int MutrgTrkMapping_v1::SetMapFile(const char *filename){
  Reset();

  ifstream fs(filename);
  if(!fs){
    printf("Error - %s : Couldn't open %s\n",class_name.c_str(),filename);
    return -1;
  }

  while(!fs.eof()){
    string line;
    getline(fs,line);
    if(line=="" || line[0]=='#'){continue;}

    int ret,arm,oct,strip0,strip1,strip2;
    ret=sscanf(line.c_str(),"%d %d %d %d %d",
	       &arm,&oct,&strip0,&strip1,&strip2);
    if(ret!=5){
      printf("Error - %s : Format is wrong.(%s)\n",
	     class_name.c_str(),filename);
      fs.close();
      return -1;
    }

    // not valid track
    if(strip0<0 || strip1<0 || strip2<0){continue;}

    pair<unsigned short,unsigned short> p;
    p.first =(unsigned short)strip0;
    p.second=(unsigned short)strip1;
    mapping[arm][oct][strip2].insert(p);
  }

  fs.close();

  printf("MutrgTrkMapping_v1::SetMapFile : %s is used for tracking.\n",
	 filename);

  return 0;
}

//////////////////////////////////////////////////////////

int MutrgTrkMapping_v1::SetMapDB(int run,MutrgPar::TrkDBVersion ver){
  PHTimeStamp ts;

  if(run>0){
    RunToTime *run_to_time=RunToTime::instance();
    if(!run_to_time){return -1;}

    PHTimeStamp *pts=run_to_time->getBeginTime(run);
    if(!pts){
      printf("Error - MutrgTrkMapping_v1 : Can't find begin time for run %d\n",run);
      return -1;
    }

    ts=*pts;
    delete pts;
  }

  return SetMapDB(ts,ver);
}

///////////////////////////////////////////////////////////

int MutrgTrkMapping_v1::SetMapDB(PHTimeStamp ts,MutrgPar::TrkDBVersion ver){
  Reset();

  MutrgDBIOTrkMap dbio_trk;
  dbio_trk.SetVersion(ver);
  dbio_trk.SetTimeSearch(ts);
  if(dbio_trk.ReadDB()){return -1;}

  const vector<PdbMutrgRpcTrk*>& mutrg_map=dbio_trk.GetMap();
  for (unsigned int itrk = 0; itrk < mutrg_map.size(); itrk++) {
    bool flag_valid=true;
    int strip[MutrgPar::NSTATION]={-9999,-9999,-9999};
    for(int ist=0; ist<MutrgPar::NSTATION; ist++){
      strip[ist]=mutrg_map[itrk]->GetStripMutrg(ist);
      if(strip[ist]<0){flag_valid=false; break;}
    }

    if (!flag_valid) {
      continue;
    }

    int arm=mutrg_map[itrk]->GetArm();
    int oct=mutrg_map[itrk]->GetOctant();

    pair<unsigned short, unsigned short> p;
    p.first = (unsigned short)strip[0];
    p.second = (unsigned short)strip[1];
    mapping[arm][oct][strip[2]].insert(p);
  }

  return 0;
}

///////////////////////////////////////////////////////////

// strip is strip_mutr + hoct*nstrip_in_hoct0;
int MutrgTrkMapping_v1::FindFromSt2(int arm,int octant,int strip_st2,
				    vector<unsigned short> &strips_st0,
				    vector<unsigned short> &strips_st1){
  strips_st0.clear();
  strips_st1.clear();

  set<pair<unsigned short,unsigned short> >::iterator itr=
    mapping[arm][octant][strip_st2].begin();
  for(; itr!=mapping[arm][octant][strip_st2].end(); itr++){
    strips_st0.push_back(itr->first);
    strips_st1.push_back(itr->second);
  }

  return 0;
}

///////////////////////////////////////////////////////////

int MutrgTrkMapping_v1::Print(std::ostream &os){
  os << "# arm oct strip_st0 strip_st1 strip_st2" << endl;

  for(int iarm=0; iarm<MutrgPar::NARM; iarm++){
    for(int ioct=0; ioct<MutrgPar::NOCTANT; ioct++){
      for(int istp2=0; istp2<MutrgPar::MAX_NSTRIP_IN_OCTANT; istp2++){
	set<pair<unsigned short,unsigned short> >::iterator itr
	  =mapping[iarm][ioct][istp2].begin();
	while(1){
	  if(itr==mapping[iarm][ioct][istp2].end()){break;}
	  unsigned short stp0=itr->first;
	  unsigned short stp1=itr->second;
	  os << setw(1) << iarm
	     << setw(2) << ioct
	     << setw(6) << stp0
	     << setw(6) << stp1
	     << setw(6) << istp2 << endl;
	  itr++;
	}
      }
    }
  }

  return 0;
}

///////////////////////////////////////////////////////////
