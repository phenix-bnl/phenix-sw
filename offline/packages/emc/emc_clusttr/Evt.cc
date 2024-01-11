
#include "Evt.hh"

ClassImp(Evt)

//============================================================
Evt::Evt(){
  _mode = kMode_none;
  _vec_clt.reserve(NUMOBJMAX);
  _vec_trk.reserve(NUMOBJMAX);
  _vec_assclt.reserve(NUMOBJMAX);
  _vec_asstrk.reserve(NUMOBJMAX);
  _vec_assclt_s.reserve(NUMOBJMAX);
  _vec_asstrk_s.reserve(NUMOBJMAX);
  Reset();
};
//============================================================
Evt::~Evt(){
  if( _mode == kMode_run1udst ){
#ifdef CLASSMICROEVENT_READING
    delete _pt_microevent;
#else
    delete _pt_nt_evt;
#endif
    delete _pt_nt_emc;
    delete _pt_nt_trk;
  }  
  if( _mode == kMode_run2tree ) {
    delete _pt_run2tree;
  }
#ifdef DST_READING
  if( _mode == kMode_DST ) {
    delete _dst;
  }
#endif
};
//============================================================
Evt& Evt::operator=(const Evt& evt){
  _mode = kMode_none;
  _glb = evt._glb;
  _vec_clt = evt._vec_clt;
  _vec_trk = evt._vec_trk;
  _vec_assclt = evt._vec_assclt;
  _vec_asstrk = evt._vec_asstrk;
  _vec_assclt_s = evt._vec_assclt_s;
  _vec_asstrk_s = evt._vec_asstrk_s;
  _current_run = evt._current_run;
  _current_seq = evt._current_seq;
  _current_evn = evt._current_evn;
  return *this;
};

Evt::Evt(const Evt& evt){
  _mode = kMode_none;
  _glb = evt._glb;
  _vec_clt = evt._vec_clt;
  _vec_trk = evt._vec_trk;
  _vec_assclt = evt._vec_assclt;
  _vec_asstrk = evt._vec_asstrk;
  _vec_assclt_s = evt._vec_assclt_s;
  _vec_asstrk_s = evt._vec_asstrk_s;
  _current_run = evt._current_run;
  _current_seq = evt._current_seq;
  _current_evn = evt._current_evn;
  return;
};
//============================================================
int Evt::Init_run1udst(TTree* in_nt_evt,TTree* in_nt_trk, TTree* in_nt_emc){
  _mode = kMode_run1udst;
  if( in_nt_evt == NULL ){
    cout<<" Error:: Can't find nt_evt tree.."<<endl;
    exit(0);
  } else if( in_nt_evt->GetEntries() == 0 ){
    cout<<" Error:: No entries in nt_evt tree.. "<<endl;
    exit(0);
  }
  //
  if( in_nt_emc == NULL )
    cout<<" Warning:: Can't find nt_emc tree.."<<endl;
  else if( in_nt_emc->GetEntries() == 0 ){
    cout<<" Warning:: No entries in nt_emc tree.. "<<endl;
    in_nt_emc = NULL;
  }
  if( in_nt_trk == NULL )
    cout<<" Warning:: Can't find nt_trk tree.."<<endl;
  else if( in_nt_trk->GetEntries() == 0 ){
    cout<<" Warning:: No entries in nt_trk tree.. "<<endl;
    in_nt_trk = NULL;
  }
  //
  _current_run = -1;
  _current_seq = -1;
  _current_evn = -1;

  _mdst_evt = in_nt_evt;
  _mdst_trk = in_nt_trk;
  _mdst_emc = in_nt_emc;
  _current_evtnum = -1;
  _current_emcnum = -1;
  _current_trknum = -1;
#ifdef CLASSMICROEVENT_READING
  _pt_microevent = new classMicroEvent(_mdst_evt);
  _pt_microevent->Init(_mdst_evt);
#else
  _pt_nt_evt = new nt_evt(_mdst_evt);
#endif
  _pt_nt_trk = new nt_trk(_mdst_trk);
  _pt_nt_emc = new nt_emc(_mdst_emc);

#ifdef CLASSMICROEVENT_READING
  _pt_microevent->seq = 0;
#else
  _pt_nt_evt->seq = 0;
#endif
  _pt_nt_trk->seq = 0;
  _pt_nt_emc->seq = 0;

  // Read First events....
  int ibytes = 0;
  ibytes += _pt_nt_trk->GetEntry(++_current_trknum);
  ibytes += _pt_nt_emc->GetEntry(++_current_emcnum);
  return ibytes;
};
//============================================================
int Evt::Init_run2tree(TTree* mdst){
  _mode = kMode_run2tree;
  if( mdst == NULL ){
    cout<<" Error:: Can't find run2tree"<<endl;
    return(0);
  } else if( mdst->GetEntries() == 0 ){
    cout<<" Error:: No entries in run2tree.. "<<endl;
    return(0);
  }
  _mdst_run2tree = mdst;
  _current_run = -1;
  _current_seq = -1;
  _current_evn = -1;
  _current_evtnum = -1;
  _pt_run2tree = new mdst_run2tree(mdst);
  return 1;
};
//============================================================
int Evt::Init_DST(TTree* dst){
  int status = 0;
  _mode = kMode_DST;
  if( dst == NULL ){
    cout<<" Error:: Can't find DST Tree"<<endl;
    return(0);
  }
#ifdef DST_READING
  status = 1;
  _dst = new Dst("DST","DST reader ");
  char fname[128];
  sprintf(fname,"%s",dst->GetDirectory()->GetName());
  _dst->Open(fname); //FIX.ME........
  if( _dst->GetEntries() == 0 ){
    cout<<" Error:: No entries in DST.. "<<endl;
    return(0);
  }
#endif
  _current_run = -1;
  _current_seq = -1;
  _current_evn = -1;
  _current_evtnum = -1;
  return status;
};
//============================================================
int Evt::Reset(){
  _glb.Reset();
  _vec_clt.erase( _vec_clt.begin(), _vec_clt.end() );
  _vec_trk.erase( _vec_trk.begin(), _vec_trk.end() );
  _vec_assclt.erase( _vec_assclt.begin(), _vec_assclt.end() );
  _vec_asstrk.erase( _vec_asstrk.begin(), _vec_asstrk.end() );
  _vec_assclt_s.erase( _vec_assclt_s.begin(), _vec_assclt_s.end() );
  _vec_asstrk_s.erase( _vec_asstrk_s.begin(), _vec_asstrk_s.end() );
  return 1;
};
//============================================================
bool Evt::IsLastEvt(){
  bool status = false;
  if( _current_evtnum == _mdst_evt->GetEntries() - 1 ){
    cout<<" Evt::IsLastEvt() last event."<<endl;
    status = true;
  }
  return status;
}
//============================================================
int Evt::Next(){
  if( _mode == kMode_run1udst )
    return Next_run1udst();
  else if( _mode == kMode_run2tree )
    return Next_run2tree();
  else if( _mode == kMode_DST )
    return Next_DST();
  else {
    cout<<" "<<GetName()<<"::Next() is not initialized yet!! "<<endl;
    return 0;
  }
}
//============================================================
int Evt::Next_run1udst(){
  int total_size = 0;
  //
  this->Reset();
  if( _current_evtnum >= _mdst_evt->GetEntries() ){
    cout<<" Evt::Next() End of run.."<<endl;
    return total_size;
  }
  //----------------------------------------- Global
#ifdef CLASSMICROEVENT_READING
  _pt_microevent->GetEntry(++_current_evtnum);
  if( _pt_microevent->run != _current_run ||
      _pt_microevent->seq != _current_seq ){
    cout<<" Evt::Next() Found new run/seq "
	<<_pt_microevent->run<<"/"<<_pt_microevent->seq<<endl;
  }
  _current_run = (int) _pt_microevent->run;
  _current_seq = (int) _pt_microevent->seq;
  _current_evn = (int) _pt_microevent->evt;
  _glb = *_pt_microevent;
#else
  _pt_nt_evt->GetEntry(++_current_evtnum);
  _current_run = (int) _pt_nt_evt->run;
  _current_seq = (int) _pt_nt_evt->seq;
  _current_evn = (int) _pt_nt_evt->evt;
  _glb = *_pt_nt_evt;
#endif
  total_size++;
  //----------------------------------------- EMCal
  if( _mdst_emc != 0 &&
      _pt_nt_emc->run == _current_run &&
      _pt_nt_emc->seq == _current_seq &&
      _pt_nt_emc->evt == _current_evn ) {
    Clust cl;
    cl = *_pt_nt_emc;
    _vec_clt.push_back(cl);
    while( _mdst_emc->GetEntry(++_current_emcnum) ){
      if( _pt_nt_emc->run == _current_run &&
	  _pt_nt_emc->seq == _current_seq &&
	  _pt_nt_emc->evt == _current_evn ){
	cl = *_pt_nt_emc;
	_vec_clt.push_back(cl);
      } else
	break;
    }
    total_size += _vec_clt.size();
  } else {
    if( _mdst_emc != 0 &&
	( _pt_nt_emc->run != _current_run ||
	  _pt_nt_emc->seq != _current_seq  )  ){
      cout<<" Evt::Next() Error!!! run# and seq# of nt_emc are not consistent with those of nt_evt "<<endl;
      Print();
      MakeHealthClust();
    }
  }
  //------------------------------------------ Tracking
  if( _mdst_trk != 0 && 
      _pt_nt_trk->run == _current_run &&
      _pt_nt_trk->seq == _current_seq &&
      _pt_nt_trk->evt == _current_evn ) {
    Track tr;
    tr = *_pt_nt_trk;
    _vec_trk.push_back(tr);
    while( _mdst_trk->GetEntry(++_current_trknum) ){
      if( _pt_nt_trk->run == _current_run &&
	  _pt_nt_trk->seq == _current_seq &&
	  _pt_nt_trk->evt == _current_evn ){
	tr = *_pt_nt_trk;
	_vec_trk.push_back(tr);
      } else
	break;
    }
    total_size += _vec_trk.size();
  } else {
    if( _mdst_trk != 0 &&
	(_pt_nt_trk->run != _current_run ||
    	_pt_nt_trk->seq != _current_seq )    ){
      cout<<" Evt::Next() Error!!! run# and seq# of nt_trk are not consistent with those of nt_evt "<<endl;
      MakeHealthTrack();
    }
  }
  total_size += MakeAss();

  return total_size;
};
//============================================================
int Evt::Next_run2tree(){
  int total_size = 0;
  int n;
  //
  this->Reset();
  if( _current_evtnum >= _mdst_run2tree->GetEntries() ){
    cout<<" Evt::Next() End of run.."<<endl;
    return total_size;
  }
  //----------------------------------------- Global
  _pt_run2tree->GetEntry(++_current_evtnum);
  if( _pt_run2tree->run != _current_run ){
    cout<<" Evt::Next() Found new run "<<_pt_run2tree->run<<endl;
  }
  _current_run = (int) _pt_run2tree->run;
  _current_seq = (int) 0;
  _current_evn = (int) _pt_run2tree->evt;
  _glb = *_pt_run2tree;
  total_size++;
  //------------------------------------------ EMCal
  n = _pt_run2tree->nemc;
  while( n-- ){
    Clust cl;
    cl.Set(*_pt_run2tree,n);
    _vec_clt.push_back(cl);
  }
  total_size += _vec_clt.size();
  //------------------------------------------ Tracking
  n = _pt_run2tree->ntrk;
  while( n-- ){
    Track tr;
    tr.Set(*_pt_run2tree,n);
    _vec_trk.push_back(tr);
  }
  total_size += _vec_trk.size();
  //----------------------------------------- Association
  total_size += MakeAss();

  return total_size;
};
//============================================================
int Evt::Next_DST(){
  int total_size = 0;
  int n;
#ifdef DST_READING
  //
  this->Reset();
  //----------------------------------------- Global
  n = (int)_dst->Next();
  if( n == 0 ){
    cout<<" Evt::Next() End of run "<<endl;
    return total_size;
  }
  _glb.Set(_dst);
  if( _glb.run != _current_run ){
    cout<<" Evt::Next() Found new run "<<_glb.run<<endl;
  }
  _current_evtnum++;
  _current_run = (int) _glb.run;
  _current_seq = (int) 0;
  _current_evn = (int) _dst->GetEvnum();
  total_size++;
  //------------------------------------------ EMCal
  n = _dst->dEmcClusterLocalExt->RowCount();
  while( n-- ){
    Clust cl;
    cl.Set(_dst,n);
    _vec_clt.push_back(cl);
  }
  total_size += _vec_clt.size();
  //------------------------------------------ Tracking
  n = _dst->dPHTrack->RowCount();
  while( n-- ){
    Track tr;
    tr.Set(_dst,n);
    _vec_trk.push_back(tr);
  }
  total_size += _vec_trk.size();
  //----------------------------------------- Association
  total_size += MakeAss();

#endif //end of DST_READING  
  return total_size;
};
//============================================================
#define SWAPDIST 55.
float Evt::swapz(float inz){
  float sz = -10000;
  if( inz >= SWAPDIST || inz <= -1. * SWAPDIST )
    sz = -1. * inz;
  else if( inz >=0 && inz < SWAPDIST )
    sz = inz - SWAPDIST;
  else if( inz <0 && inz > -1.*SWAPDIST )
    sz = inz + SWAPDIST;
  return sz;
}
//============================================================
int Evt::GetEntries(){
  if( _mode == kMode_run1udst ){
    return (int)_mdst_evt->GetEntries();
  }
  if( _mode == kMode_run2tree ){
    return (int)_mdst_run2tree->GetEntries();
  }
  if( _mode == kMode_DST  ){
    return (int)_dst->GetEntries();
  }
  return 0;
}
//============================================================
int Evt::MakeAss(){
  float dist,distxyz[3];
  int total_ass_num = 0;
  int i,j;
  Ass tmp_ass;
  //
  i = _vec_clt.size();
  while( i-- ){
    _vec_assclt.push_back(tmp_ass);
    _vec_assclt_s.push_back(tmp_ass);
  }
  i = _vec_trk.size();
  while( i-- ){
    _vec_asstrk.push_back(tmp_ass);
    _vec_asstrk_s.push_back(tmp_ass);
  }
  i = _vec_clt.size();
  while( i-- ){
    j = _vec_trk.size();
    while( j-- ){
      distxyz[0] = _vec_trk[j].proj[0] - _vec_clt[i].pos[0];
      distxyz[1] = _vec_trk[j].proj[1] - _vec_clt[i].pos[1];
      distxyz[2] = _vec_trk[j].proj[2] - _vec_clt[i].pos[2];
      dist = distxyz[0]*distxyz[0] + distxyz[1]*distxyz[1] + distxyz[2]*distxyz[2];
      dist = sqrt( dist );
      if( fabs(dist) < 50. ){
	_vec_assclt[i].Insert(j,dist,distxyz[0],distxyz[1],distxyz[2]);
	_vec_asstrk[j].Insert(i,dist,-1.*distxyz[0],-1.*distxyz[1],-1.*distxyz[2]);
	total_ass_num++;
      }
      //
      distxyz[2] = _vec_trk[j].proj[2] - swapz(_vec_clt[i].pos[2]);
      dist = distxyz[0]*distxyz[0] + distxyz[1]*distxyz[1] + distxyz[2]*distxyz[2];
      dist = sqrt( dist );
      if( fabs(dist) < 50. ){
	_vec_assclt_s[i].Insert(j,dist,distxyz[0],distxyz[1],distxyz[2]);
	_vec_asstrk_s[j].Insert(i,dist,-1.*distxyz[0],-1.*distxyz[1],-1.*distxyz[2]);
	total_ass_num++;
      }
      //
    }
  }
  return( total_ass_num );
}
//============================================================
int Evt::MakeHealthClust(){
  int total_num;
  int num;
  num = _current_emcnum;
  while( _mdst_emc->GetEntry(++_current_emcnum) ){
    if( _pt_nt_emc->run == _current_run &&
	_pt_nt_emc->seq == _current_seq &&
	_pt_nt_emc->evt > _current_evn )
      break;
    if( _pt_nt_emc->run != _current_run ||
	_pt_nt_emc->seq != _current_seq )
      break;
    if( num - _current_emcnum > 1000 ){
      cout<<" Evt::  Error!!! Can't find health EMCal event #"<<endl;
      _current_emcnum = num;
    }
  }
  total_num += _current_emcnum - num;
  cout<<" Evt:: Skip EMCal event by "<<_current_emcnum-num<<endl;
  //
  return total_num;
}
//============================================================
int Evt::MakeHealthTrack(){
  int total_num;
  int num;
  num = _current_trknum;
  while( _mdst_trk->GetEntry(++_current_trknum) ){
    if( _pt_nt_trk->run == _current_run &&
	_pt_nt_trk->seq == _current_seq &&
	_pt_nt_trk->evt > _current_evn )
      break;
    if( _pt_nt_trk->run != _current_run ||
	_pt_nt_trk->seq != _current_seq )
      break;
    if( num - _current_trknum > 1000 ){
      cout<<" Evt:: Error!!! Can't find health Tracking event #"<<endl;
      _current_trknum = num;
    }
  }
  total_num += _current_trknum - num;
  cout<<" Evt:: Skip Tracking event by "<<_current_trknum - num<<endl;
  //
  return total_num;
}
//============================================================
void Evt::Print(){
  cout<<" --- Evt::Print() ------------------------------------------ "<<endl;
  cout<<" ----- current_event/seq/run = "
      <<_current_evn<<"/"<<_current_seq<<"/"<<_current_run<<endl;
  cout<<" ----- current_evtnum = "<<_current_evtnum<<endl;
  cout<<" ----- cluster number = "<<_vec_clt.size()<<endl;
  cout<<" ----- track number = "<<_vec_trk.size()<<endl;
  cout<<" ----- assclt number = "<<_vec_assclt.size()<<endl;
  cout<<" ----- asstrk number = "<<_vec_asstrk.size()<<endl;
  int total_ass_num,total_ass_num_s;
  total_ass_num = 0;
  vector<Ass>::iterator it_vec_assclt = _vec_assclt.begin();
  while( it_vec_assclt != _vec_assclt.end() ){
    total_ass_num += it_vec_assclt->GetSize();
    it_vec_assclt++;
  }
  total_ass_num_s = 0;
  vector<Ass>::iterator it_vec_assclt_s = _vec_assclt_s.begin();
  while( it_vec_assclt_s != _vec_assclt_s.end() ){
    total_ass_num_s += it_vec_assclt_s->GetSize();
    it_vec_assclt_s++;
  }
  cout<<" ----- assclt id num (real, swap) = ( "
      <<total_ass_num<<","<<total_ass_num_s<<")"<<endl;
  //
  total_ass_num = 0;
  vector<Ass>::iterator it_vec_asstrk = _vec_asstrk.begin();
  while( it_vec_asstrk != _vec_asstrk.end() ){
    total_ass_num += it_vec_asstrk->GetSize();
    it_vec_asstrk++;
  }
  total_ass_num_s = 0;
  vector<Ass>::iterator it_vec_asstrk_s = _vec_asstrk_s.begin();
  while( it_vec_asstrk_s != _vec_asstrk_s.end() ){
    total_ass_num_s += it_vec_asstrk_s->GetSize();
    it_vec_asstrk_s++;
  } 
  cout<<" ----- asstrk id num (real, swap) = ( "
      <<total_ass_num<<","<<total_ass_num_s<<")"<<endl;
  //
  if( _mode == kMode_run1udst )
    Print_run1udst();
  if( _mode == kMode_run2tree )
    Print_run2tree();
  cout<<" ----------------------------------------------------------- "<<endl;
};
//============================================================
void Evt::Print_run1udst(){
  cout<<" --- Evt:: nt_evt event/seq/run = "
#ifdef CLASSMICROEVENT_READING
      <<_pt_microevent->evt<<"/"<<_pt_microevent->seq<<"/"<<_pt_microevent->run<<endl;
#else
  <<_pt_nt_evt->evt<<"/"<<_pt_nt_evt->seq<<"/"<<_pt_nt_evt->run<<endl;
#endif
  cout<<" --- Evt:: nt_emc event/seq/run = "
      <<_pt_nt_emc->evt<<"/"<<_pt_nt_emc->seq<<"/"<<_pt_nt_emc->run<<endl;
  cout<<" --- Evt:: nt_trk event/seq/run = "
      <<_pt_nt_trk->evt<<"/"<<_pt_nt_trk->seq<<"/"<<_pt_nt_trk->run<<endl;
  cout<<" ----- _current_evtnum = "<<_current_evtnum;
  if( _mdst_evt )
    cout<<"/"<<_mdst_evt->GetEntries();
  cout<<endl;
  cout<<" ----- _current_emcnum = "<<_current_emcnum-1;
  if( _mdst_emc )
    cout<<"/"<<_mdst_emc->GetEntries();
  cout<<endl;
  cout<<" ----- _current_trknum = "<<_current_trknum-1;
  if( _mdst_trk )
    cout<<"/"<<_mdst_trk->GetEntries();
  cout<<endl;
  
};
//============================================================
void Evt::Print_run2tree(){
};
//=============================================================
int Evt::GetAssTrk(int trkid,int num){
  if( trkid >= _vec_asstrk.size() )
    return -1;
  if( num < _vec_asstrk[trkid].GetSize() )
    return _vec_asstrk[trkid].GetID(num);
  else
    return -1;
};
//=============================================================
int Evt::GetAssClt(int cltid,int num){
  if( cltid >= _vec_assclt.size() )
    return -1;
  if( num < _vec_assclt[cltid].GetSize() )
    return _vec_assclt[cltid].GetID(num);
  else
    return -1;
};
//============================================================
//=============================================================
//=============================================================
//=============================================================
