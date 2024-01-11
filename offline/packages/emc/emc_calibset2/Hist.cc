
#include "Hist.hh"

bool Hist::_debug = false;

ClassImp(Hist)
//=====================================================================
Hist::Hist(){
  ientries = 0;
  CalculateAngle();
};
//=====================================================================
Hist::Hist(char* pname,char* ptitle,char* opt) : TNamedDir(pname,ptitle){
  ientries = 0;
  CalculateAngle();
};
//=====================================================================
Hist::~Hist(){
  if( _debug ) cout<<" Hist:: default de-const"<<endl;
  TIterator* itr = _ptarray.MakeIterator();
  TObject* obj;
  while( obj = itr->Next() ){
    if( _debug ) cout<<" Hist:: de-const delete ";
    if( _debug ) obj->ls();
    obj->Delete();
  }
  delete itr;
};
//=====================================================================
TH1* Hist::Register(TH1* h1){
  _ptarray.Add(h1);
  return h1;
};
//=====================================================================
bool Hist::Reset(){
  TIterator* itr = _ptarray.MakeIterator();
  TObject* obj;
  TH1* h1;
  while( obj = itr->Next() ){
    h1 = dynamic_cast<TH1*>(obj);
    h1->Reset();
  }
  delete itr;
  ientries = 0;
  return true;
};
//=====================================================================
bool Hist::Add(Hist* that){
  if( this->_ptarray.GetEntries() != that->_ptarray.GetEntries() ){
    cout<<" "<<GetName()<<"::Add() Error!! at index "<<index<<endl;
    return false;
  }
  TIterator* that_itr = that->_ptarray.MakeIterator();
  TIterator* this_itr = this->_ptarray.MakeIterator();
  TObject* that_obj;
  TObject* this_obj;
  TH1* that_h1;
  TH1* this_h1;
  while( (this_obj = this_itr->Next()) && (that_obj = that_itr->Next()) ){
    that_h1 = dynamic_cast<TH1*>(that_obj);
    this_h1 = dynamic_cast<TH1*>(this_obj);
    this_h1->Add(that_h1);
  }
  delete that_itr;
  delete this_itr;
  ientries += that->ientries;
  return true;
};
//=====================================================================
bool Hist::Add(TFile* f){
  Hist* t_hist = (Hist*)f->Get(GetName());
  bool status = false;
  if( t_hist != NULL ){
    cout<<" "<<GetName()<<"::Add() entries : "<<GetEntries()<<" --> ";
    Add(t_hist);
    cout<<GetEntries()<<endl;
    delete t_hist;
    status = true;
  } else {
    cout<<" "<<GetName()<<"::Add() Can't read any entries :: "<<GetName()<<endl;
  }
  return status;
};
//=====================================================================
void Hist::Print(){
  cout<<" "<<GetName()<<"::Print() ------------------------------------------- "<<endl;
  this->ls();
  //_ptarray.ls();
  TIterator* itr = _ptarray.MakeIterator();
  TObject* obj;
  TH1* h1;
  int total_bin = 0;
  int total_num = 0;
  while( obj = itr->Next() ){
    h1 = dynamic_cast<TH1*>(obj);
    int dim = h1->GetDimension();
    int bin = h1->GetNbinsX() * h1->GetNbinsY() * h1->GetNbinsZ();
    total_num++;
    total_bin += bin;
    cout<<"  |-> "<<dim<<" "<<bin<<" "<<h1->GetName()<<endl;
  }
  delete itr;
  cout<<" "<<GetName()<<":: Total "<<total_num<<" objects  "<<total_bin<<" bins"<<endl;
  cout<<" "<<GetName()<<":: Total Entries "<<ientries<<endl;
  cout<<" -------------------------------------------------------------- "<<endl;
  return;
};
//=====================================================================
bool Hist::Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid,float weight=1.){
  //----------------- Global variables
  ientries++;
  isect = clt.sector;
  if( clt.arm == 1 ){
    if( isect == 2 || isect == 3 )
      isect = isect + 2;
    else
      isect = isect + 6;
  }
  if( isect<6 ) ipbscgl = 0;
  else ipbscgl = 1;
  iz = (int)clt.ind[0];
  iy = (int)clt.ind[1];
  ismz_sect = ( isect>=6 ? (int)(clt.ind[0]/6):(int)(clt.ind[0]/12) );
  ismy_sect = ( isect>=6 ? (int)(clt.ind[1]/4):(int)(clt.ind[1]/12) );
  ism_sect = ( isect>=6 ? ismy_sect*8+ismz_sect : ismy_sect*6+ismz_sect );
  itwr_sect = (isect>=6) ? 96*iy+iz : 72*iy+iz;
  itwr = (isect>=6 ?  15552+4608*(isect-6)+96*iy+iz : 2592*isect+72*iy+iz);
  angle = fabs(trk.dir[0]*v_angsect[0][isect] + trk.dir[1]*v_angsect[1][isect] + trk.dir[2]*v_angsect[2][isect]);
  //cout<<"isect,ismz,ismy,iz,iy,itwr,angle = "<<isect<<","<<ismz<<","<<ismy<<","<<iz<<","<<iy<<","<<itwr
  //  <<","<<angle<<endl;
  // --------------------------- PID ------------------------
  if( clt.prob > 0.9 ) iprob = 1;
  else iprob = -1;
  ipid = emcpid.pid;
  iepid = emcpid.epid;
  iemce = 0;
  if( clt.prob > 0.9 ) iemce = 1;
  if( emcpid.mass < 0.1 ) iemce = 2;
  if( clt.prob > 0.9 && emcpid.mass < 0.1 ) iemce = 3;
  // --- multiplicity
  imul = (int)( glb.emcnhit / 50 );
  if( imul < 0 || imul > 5 )
    imul = -1;
  //
  return true;
};
//=====================================================================
void Hist::CalculateAngle(){
  isect = 8;
  while( isect-- ){
    //    v_angsect[0][isect] = 0.9238;
    //    v_angsect[1][isect] = -0.38268;
    //    v_angsect[2][isect] = 0;
    // { cos(-22.5deg), sin(-22.5deg), 0 }
    if( isect == 0 ) angle = -22.5;
    if( isect == 1 ) angle = 0.00;
    if( isect == 2 ) angle = 22.5;
    if( isect == 3 ) angle = 45.0;
    if( isect == 4 ) angle = 180.0 - 22.5;
    if( isect == 5 ) angle = 180.0 - 45.0;
    if( isect == 6 ) angle = 180.0 + 45.0;
    if( isect == 7 ) angle = 180.0 + 0.00;
    angle = angle * 3.1415 / 180.0;
    v_angsect[0][isect] = cos(angle);
    v_angsect[1][isect] = sin(angle);
    v_angsect[2][isect] = 0;
  }
};
//=====================================================================
bool Hist::IsValidSector(int isect){
  //  if( isect == 0 || isect == 1 || isect == 7 )
  if( isect >= 0 && isect < 8 )
    return true;
  else
    return false;
};
//=====================================================================
