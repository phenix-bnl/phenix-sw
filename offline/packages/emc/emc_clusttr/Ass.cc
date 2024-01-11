
#include "Ass.hh"

ClassImp(Ass)

//============================================================
Ass::Ass(int reserve_size){
  _dist.reserve(reserve_size);
  _id.reserve(reserve_size);
  _distxyz[0].reserve(reserve_size);
  _distxyz[1].reserve(reserve_size);
  _distxyz[2].reserve(reserve_size);
};
//============================================================
Ass::~Ass(){
  Reset();
};
//============================================================
int Ass::Reset(){
  _id.erase(_id.begin(),_id.end());
  _dist.erase(_dist.begin(),_dist.end());
  _distxyz[0].erase(_distxyz[0].begin(),_distxyz[0].end());
  _distxyz[1].erase(_distxyz[1].begin(),_distxyz[1].end());
  _distxyz[2].erase(_distxyz[2].begin(),_distxyz[2].end());
  return 1;
};
//============================================================
int Ass::GetSize(){
  return _id.size();
}
//============================================================
int Ass::GetID(int key){
  if( key < _id.size() )
    return _id[key];
  else
    return -1;
}
//============================================================
float Ass::GetDist(int key){
  if( key < _dist.size() )
    return _dist[key];
  else
    return 1500;
}
//============================================================
int Ass::Insert(int id,float dist,float distx,float disty,float distz){
  vector<int>::iterator it_id = _id.begin();
  vector<float>::iterator it_dist = _dist.begin();
  vector<float>::iterator it_distxyz[3];
  it_distxyz[0] = _distxyz[0].begin();
  it_distxyz[1] = _distxyz[1].begin();
  it_distxyz[2] = _distxyz[2].begin();
  int ass_num = 0;
  //cout<<" Ass:Insert("<<id<<","<<dist<<") :: ";
  while( it_dist != _dist.end() && it_id != _id.end() && ass_num == 0 ){
    //cout<<"  "<<*it_dist;
    if( fabs(dist) < *it_dist ){
      _id.insert(it_id,id);
      _dist.insert(it_dist,dist);
      _distxyz[0].insert(it_distxyz[0],distx);
      _distxyz[1].insert(it_distxyz[1],disty);
      _distxyz[2].insert(it_distxyz[2],distz);
      ass_num++;
      //cout<<" inserted by  : "<<dist<<endl;
    }
    it_dist++;
    it_id++;
    it_distxyz[0]++;
    it_distxyz[1]++;
    it_distxyz[2]++;
  }
  if( ass_num == 0 ){
    _id.push_back(id);
    _dist.push_back(dist);
    _distxyz[0].push_back(distx);
    _distxyz[1].push_back(disty);
    _distxyz[2].push_back(distz);
    ass_num++;
    //cout<<" push_back by "<<dist<<endl;
  }
  return ass_num;
}
//=============================================================
//=============================================================
