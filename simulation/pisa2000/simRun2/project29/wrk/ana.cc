#include "ana.hh"
//======================================================================
//ClassImp(ana)
//======================================================================
void CL_primary::Print(const char* opt) const {
  float ptot = px_momentum*px_momentum + py_momentum*py_momentum + pz_momentum*pz_momentum;
  ptot = sqrt( ptot );
  cout<<opt<<" primary :("<<true_track<<","<<idpart<<","<<ptot<<")"
      <<"\t subevent: "<<subevent<<" subevent_track: "<<subevent_track<<endl;
};
//======================================================================
void CL_fkin::Print(const char* opt) const{
  cout<<opt<<" fkin :("<<true_track<<","<<idpart<<","<<ptot<<")"
      <<"\titparent:"<<itparent
      <<"\tidparent:"<<idparent
      <<"\tr_vertex:"<<r_vertex<<endl;
};
//======================================================================
void CL_dEmcGeaTrackCluster::Print(const char* opt) const{
  cout<<" geatrcl :("<<trkno<<", "<<pid<<","<<ptot<<")"
      <<"\t edep[0]:"<<edep[0]<<endl;
};
//======================================================================
CL_part::CL_part(){
  b_fkin = false;
  i_sect = -1;
  i_accept = -1;
  b_dEmcGeaTrackCluster = false;
  ar_dEmcGeaClusterTrack = new TClonesArray("CL_dEmcGeaClusterTrack");
  ar_part = new TClonesArray("CL_part");
};
//======================================================================
CL_part::~CL_part(){
  ar_part->Delete();
  delete ar_dEmcGeaClusterTrack;
  delete ar_part;
};
//======================================================================
int CL_part::put_fkin(CL_fkin* f){
  //Print(" DEBUG:: ");
  int num = 0;
  if( b_fkin == false ){
    fkin = *f;
    b_fkin = true;
    float PHI_EDGE = 2.0;
    float pz = fkin.ptot * cos(fkin.pthet * 3.1414/180.0);
    //px = fkin.ptot * sin (fkin.pthet) * cos (fkin.pphi);
    //py = fkin.ptot * sin (fkin.pthet) * sin (fkin.pphi);
    f_phi = fkin.pphi;
    f_pt = fkin.ptot;
    f_eta = TMath::ATanH( pz / fkin.ptot );
    i_sect = -1;
    //if( f_phi < 0 ) f_phi += 180;
    if( fabs(f_eta) < 0.3 && f_phi > -33.75 + PHI_EDGE && f_phi < -11.25 - PHI_EDGE ) i_sect = 0;  //W0
    if( fabs(f_eta) < 0.3 && f_phi > -11.25 + PHI_EDGE && f_phi < 11.25  - PHI_EDGE ) i_sect = 1;  //W1
    if( fabs(f_eta) < 0.3 && f_phi > 11.25  + PHI_EDGE && f_phi < 33.75  - PHI_EDGE ) i_sect = 2;  //W2
    if( fabs(f_eta) < 0.3 && f_phi > 33.75  + PHI_EDGE && f_phi < 56.25  - PHI_EDGE ) i_sect = 3;  //W3
    if( fabs(f_eta) < 0.3 && f_phi > 123.75 + PHI_EDGE && f_phi < 146.25 - PHI_EDGE ) i_sect = 5;  //E3
    if( fabs(f_eta) < 0.3 && f_phi > 146.25 + PHI_EDGE && f_phi < 168.75 - PHI_EDGE ) i_sect = 4;  //E2
    if( fabs(f_eta) < 0.3 && 
	( ( f_phi > 168.75 + PHI_EDGE && f_phi <= 180.0) || ( f_phi < -168.75 - PHI_EDGE && f_phi >= -180.0 ) )
	) i_sect = 7;  //E1
    if( fabs(f_eta) < 0.3 && f_phi > -168.75 + PHI_EDGE && f_phi < -146.25 - PHI_EDGE ) i_sect = 6;  //E0
    //    if( fabs(f_eta) < 0.3 && f_phi > 168.75 + PHI_EDGE && f_phi < 191.25 - PHI_EDGE ) i_sect = 7;  //E1
    //    if( fabs(f_eta) < 0.3 && f_phi > 191.25 + PHI_EDGE && f_phi < 213.75 - PHI_EDGE ) i_sect = 6;  //E0
    i_accept = -1;
    if( i_sect == -1 ) i_accept = 0;
    if( i_sect >= 0 && i_sect < 4 ) i_accept = 1;
    if( i_sect == 4 || i_sect == 5 ) i_accept = 2;
    if( i_sect == 6 || i_sect == 7 ) i_accept = 3;
    num++;
  } else if( f->itparent == fkin.true_track ){ // If this particle is the parent of CL_fkin* f...
    int nn = ar_part->GetEntries();
    CL_part* part = new ( (*ar_part)[nn] ) CL_part;
    num = part->put_fkin(f);
  } else {
      int nn = ar_part->GetEntries();
      while( nn-- ) num += ((CL_part*)ar_part->At(nn))->put_fkin(f);
  }
  return num;
};
//======================================================================
int CL_part::put_dEmcGeaTrackCluster(CL_dEmcGeaTrackCluster* g){
  int num = 0;
  if( b_dEmcGeaTrackCluster == false &&
      fkin.true_track == g->trkno ){
    dEmcGeaTrackCluster = *g;
    b_dEmcGeaTrackCluster = true;
    num++;
  } else {
    int nn = ar_part->GetEntries();
    while( nn-- ) num += ((CL_part*)ar_part->At(nn))->put_dEmcGeaTrackCluster(g);
  }
  return num;
};
//======================================================================
int CL_part::put_dEmcGeaClusterTrack(CL_dEmcGeaClusterTrack* g){
  int nn;
  int num = 0;
  if( b_dEmcGeaTrackCluster ){
    nn = 3;
    bool b_stat = false;
    while(nn--){
      if(dEmcGeaTrackCluster.clusid[nn] == g->clusid )
	b_stat = true;
    }
    if( b_stat ){
      num++;
      nn = ar_dEmcGeaClusterTrack->GetEntries();
      CL_dEmcGeaClusterTrack* pt = 
	new ( (*ar_dEmcGeaClusterTrack)[nn] ) CL_dEmcGeaClusterTrack;
      (*pt) = *g;
    }
  }
  nn = ar_part->GetEntries();
  while( nn-- ) num += ((CL_part*)ar_part->At(nn))->put_dEmcGeaClusterTrack(g);
  return num;
};
//======================================================================
TObjArray CL_part::getarray_dEmcGeaClusterTrack(int depth){
  int nn,num;
  bool b_stat = false;
  CL_dEmcGeaClusterTrack* dEmcGeaClusterTrack = NULL;
  TObjArray ar;
  //
  nn = ar_dEmcGeaClusterTrack->GetEntries();
  while( nn-- ){
    dEmcGeaClusterTrack = (CL_dEmcGeaClusterTrack*) ar_dEmcGeaClusterTrack->At(nn);
    b_stat = false;
    num = ar.GetEntries();
    while( num-- ){
      if( ( (CL_dEmcGeaClusterTrack*)ar[num] )->clusid == dEmcGeaClusterTrack->clusid )
	b_stat = true;
    }
    if( b_stat == false )
      ar.Add(dEmcGeaClusterTrack);
  }
  if( depth > 0 ){ 
    int id = ar_part->GetEntries();
    while( id-- ){
      CL_part* part = (CL_part*)ar_part->At(id);
      TObjArray part_ar = part->getarray_dEmcGeaClusterTrack(depth-1);
      nn = part_ar.GetEntries();
      while( nn-- ){
	dEmcGeaClusterTrack = (CL_dEmcGeaClusterTrack*) part_ar.At(nn);
	b_stat = false;
	num = ar.GetEntries();
	while( num-- ){
	  if( ( (CL_dEmcGeaClusterTrack*)ar[num] )->clusid == dEmcGeaClusterTrack->clusid )
	    b_stat = true;
	}
	if( b_stat == false )
	  ar.Add(dEmcGeaClusterTrack);
      }
    }
  }
  return ar;
};
//======================================================================
TObjArray CL_part::getarray_part_pid(int pid,int depth){
  int nn,num;
  CL_part* part = NULL;
  TObjArray ar;
  //
  if( b_fkin ){
    if( fkin.idpart == pid )
      ar.Add(this);
  }
  if( depth > 0 ){ 
    num = ar_part->GetEntries();
    while( num-- ){
      CL_part* child_part = (CL_part*)ar_part->At(num);
      TObjArray part_ar = child_part->getarray_part_pid(pid,depth-1);
      nn = part_ar.GetEntries();
      while( nn-- ){
	part = (CL_part*) part_ar.At(nn);
	ar.Add(part);
      }
    }
  }
  return ar;
};
//======================================================================
void CL_part::Print(const char* opt) const{
  int nn,id;
  cout.precision(4);
  if( b_fkin )
    cout<<opt
	<<" part : ("<<fkin.true_track<<","<<fkin.idpart<<","<<fkin.ptot<<")"
	<<"\titparent:"<<fkin.itparent<<" idparent:"<<fkin.idparent
	<<"\t r_vertex:"<<fkin.r_vertex<<" i_accept:"<<i_accept
	<<endl;
  if( b_dEmcGeaTrackCluster ){
    cout<<opt
	<<" |geatr: ("<<dEmcGeaTrackCluster.trkno<<","<<fkin.idpart<<","<<dEmcGeaTrackCluster.ptot<<")"
	<<" track_ptr: "<<dEmcGeaTrackCluster.track_ptr;
    id = 3;
    while( id-- ){
      if( dEmcGeaTrackCluster.edep[id] > 0 )
	cout<<" edep["<<id<<"]:"<<dEmcGeaTrackCluster.edep[id];
    }
    cout<<endl;
  }
  nn = ar_dEmcGeaClusterTrack->GetEntries();
  while( nn-- ){
    CL_dEmcGeaClusterTrack* dEmcGeaClusterTrack = (CL_dEmcGeaClusterTrack*) ar_dEmcGeaClusterTrack->At(nn);
    cout<<opt;
    id = 3;
    while(id--){
      if( dEmcGeaClusterTrack->trkno[id] > 0 )
	cout<<" |geacl: ("<<dEmcGeaClusterTrack->trkno[id]<<","<<dEmcGeaClusterTrack->pid[id]<<","<<dEmcGeaClusterTrack->ptot[id]<<") ";
    }
    cout<<" ecore: "<<dEmcGeaClusterTrack->ecore<<endl;
  }
  TString st = opt;
  st.Append(" | ");
  nn = ar_part->GetEntries();
  while( nn-- ) ((CL_part*)ar_part->At(nn))->Print(st.Data());
};
//======================================================================
bool CL_recpri::put_fkin(CL_fkin* f){
  if( b_part ||
      ( !b_part && f->itparent < 0 &&
	f->true_track == primary.true_track ) ){
    int num = part.put_fkin(f);
    if( num == 1 ){ b_part = true; return true; }
    if( num > 1 )
      cout<<" Warning!!!! CL_recpri::put_fkin() got : "<<num<<endl;
    return false;
  } else
    return false;
};
//======================================================================
bool CL_recpri::put_dEmcGeaTrackCluster(CL_dEmcGeaTrackCluster* g){
  int num = part.put_dEmcGeaTrackCluster(g);
  if( num == 1 ) return true;
  if( num > 1 ){ cout<<" Warning!!!! CL_recpri::put_dEmcGeaTrackCluster() got more than 2 hit "<<endl; }
  return false;
};
//======================================================================
bool CL_recpri::put_dEmcGeaClusterTrack(CL_dEmcGeaClusterTrack* g){
  int num = part.put_dEmcGeaClusterTrack(g);
  if( num > 0 ) return true;
  return false;
};
//======================================================================
void CL_recpri::Print(const char* opt) const{
  float ptot;
  ptot = primary.px_momentum*primary.px_momentum + primary.py_momentum*primary.py_momentum + primary.pz_momentum*primary.pz_momentum;
  ptot = sqrt( ptot );
  cout<<opt<<" recpri -------------------------------------------------- "<<endl;
  TString st = opt;
  st.Append(" | ");
  cout<<opt
      <<" primary : ("<<primary.true_track<<","<<primary.idpart<<","<<ptot<<")"
      <<"\t: "<<primary.subevent<<" : "<<primary.subevent_track<<endl;
  st.Append(" | ");
  part.Print(st.Data());
  cout<<opt<<" --------------------------------------------------------- "<<endl;
};
//======================================================================
CL_event::CL_event(){
  _debug = false;
  ar_recpri = new TClonesArray("CL_recpri");
  ar_part = new TClonesArray("CL_part");
  nn_dEmcGeaTrackCluster_all = 0;
  nn_dEmcGeaTrackCluster_rej = 0;
  nn_dEmcGeaClusterTrack_all = 0;
  nn_dEmcGeaClusterTrack_rej = 0;
};
//======================================================================
CL_event::~CL_event(){
  delete ar_recpri;
  delete ar_part;
};
//======================================================================
bool CL_event::set_primary(TClonesArray* ar_primary){
  TClonesArray& ref_recpri = *ar_recpri;
  int nn,num;
  nn = ar_primary->GetEntries();
  while( nn-- ){
    CL_primary* pr = (CL_primary*) ar_primary->At(nn);
    if( pr->true_track > 0 ){  // At least one hit at the detector.
      num = ar_recpri->GetEntries();
      CL_recpri* recpri = new (ref_recpri[num]) CL_recpri;
      recpri->set_primary(pr);
    }
  }
  return true;
};
//======================================================================
bool CL_event::set_fkin(TClonesArray* ar_fkin){
  TClonesArray& ref_recpri = *ar_recpri;
  TClonesArray& ref_part = *ar_part;
  TClonesArray& ref_fkin = *ar_fkin;
  // --- Put into recpri
  bool b_decay;
  int nn,num;
  int i_put, i_recurse;
  nn = ref_fkin.GetEntries();
  bool* vec_b_fkin = new bool[nn];
  while( nn-- ) vec_b_fkin[nn] = true;
  i_recurse = 0;
  while( i_recurse == 0 || ( i_put != 0 && i_recurse < 10 ) ){
    i_recurse++;
    i_put = 0;
    nn = ref_fkin.GetEntries();
    while( nn-- ){
      CL_fkin* fkin = (CL_fkin*)ref_fkin.At(nn);
      if( vec_b_fkin[nn] ){
	num = ref_recpri.GetEntries();
	while( num-- ){
	  CL_recpri* recpri = (CL_recpri*) ref_recpri.At(num);
	  if( recpri->put_fkin(fkin) ){
	    vec_b_fkin[nn] = false;
	    i_put ++;
	  }
	}
      }
    }
    if( _debug ){
      num = 0;
      nn = ref_fkin.GetEntries();
      while( nn-- ) if( vec_b_fkin[nn] ) num++;
      cout<<" DEBUG:: Analyzing fkin:: i_recurse = "<<i_recurse<<" : i_put = "<<i_put<<endl;
      cout<<"      :: Remained fkin = "<<num<<endl;
    }
  }
  // --- Put into part
  nn = ref_fkin.GetEntries();
  while( nn-- ){
    if( vec_b_fkin[nn] ){
      CL_fkin* fkin = (CL_fkin*)ref_fkin.At(nn);
      b_decay = false;
      num = ref_part.GetEntries();
      while( num-- ){
	CL_part* part = (CL_part*) ref_part.At(num);
	if( part->put_fkin(fkin) > 0 ) b_decay = true;
      }
      if( b_decay == false ){
	num = ref_part.GetEntries();
	CL_part* part = new (ref_part[num]) CL_part;
	part->put_fkin(fkin);
      }
      if( _debug ){
	cout<<" DEBUG:: Remained fkin:("<<fkin->true_track<<","<<fkin->idpart<<","<<fkin->ptot<<")"
	    <<"\titparent:"<<fkin->itparent<<" idparent:"<<fkin->idparent
	    <<"\t r_vertex:"<<fkin->r_vertex
	    <<endl;
      }
    }
  }
  delete[] vec_b_fkin;
  return true;
};
//======================================================================
bool CL_event::set_dEmcGeaTrackCluster(TClonesArray* ar_dEmcGeaTrackCluster){
  TClonesArray& ref_recpri = *ar_recpri;
  TClonesArray& ref_part = *ar_part;
  TClonesArray& ref_dEmcGeaTrackCluster = *ar_dEmcGeaTrackCluster;
  // ========== Analyzing dEmcGeaTrackCluster
  int nn,num;
  nn = ref_dEmcGeaTrackCluster.GetEntries();
  bool* vec_b_dEmcGeaTrackCluster = new bool[nn];
  while( nn-- ) vec_b_dEmcGeaTrackCluster[nn] = true;
  //
  nn = ref_dEmcGeaTrackCluster.GetEntries();
  while( nn-- ){
    CL_dEmcGeaTrackCluster* dEmcGeaTrackCluster = (CL_dEmcGeaTrackCluster*)ref_dEmcGeaTrackCluster.At(nn);
    // --- Input to recpri
    if( vec_b_dEmcGeaTrackCluster[nn] ){
      num = ref_recpri.GetEntries();
      while( num-- ){
	CL_recpri* recpri = (CL_recpri*) ref_recpri.At(num);
	if( recpri->put_dEmcGeaTrackCluster(dEmcGeaTrackCluster) > 0 ){
	  vec_b_dEmcGeaTrackCluster[nn] = false;
	}
      }
    }
    // --- Input to part
    if( vec_b_dEmcGeaTrackCluster[nn] ){
      num = ref_part.GetEntries();
      while( num-- ){
	CL_part* part = (CL_part*) ref_part.At(num);
	if( part->put_dEmcGeaTrackCluster(dEmcGeaTrackCluster) > 0 ){
	  vec_b_dEmcGeaTrackCluster[nn] = false;
	}
      }
    }
  }
  
  num = 0;
  nn = ref_dEmcGeaTrackCluster.GetEntries();
  while( nn-- ) if( vec_b_dEmcGeaTrackCluster[nn] ) num++;
  if( num > 0 )
    cout<<" Warning:: Remained dEmcGeaTrackCluster = "<<num<<" / "<<
      ref_dEmcGeaTrackCluster.GetEntries()<<endl;
  nn_dEmcGeaTrackCluster_all += ref_dEmcGeaTrackCluster.GetEntries();
  nn_dEmcGeaTrackCluster_rej += num;
  delete[] vec_b_dEmcGeaTrackCluster;

  return true;
};
//======================================================================
bool CL_event::set_dEmcGeaClusterTrack(TClonesArray* ar_dEmcGeaClusterTrack){
  TClonesArray& ref_recpri = *ar_recpri;
  TClonesArray& ref_part = *ar_part;
  TClonesArray& ref_dEmcGeaClusterTrack = *ar_dEmcGeaClusterTrack;
  // ========== Analyzing dEmcGeaClusterTrack
  int nn,num;
  nn = ref_dEmcGeaClusterTrack.GetEntries();
  bool* vec_b_dEmcGeaClusterTrack = new bool[nn];
  while( nn-- ) vec_b_dEmcGeaClusterTrack[nn] = true;
  //
  nn = ref_dEmcGeaClusterTrack.GetEntries();
  while( nn-- ){
    CL_dEmcGeaClusterTrack* dEmcGeaClusterTrack = (CL_dEmcGeaClusterTrack*)ref_dEmcGeaClusterTrack.At(nn);
    // --- Input to recpri
    if( vec_b_dEmcGeaClusterTrack[nn] ){
      num = ref_recpri.GetEntries();
      while( num-- ){
	CL_recpri* recpri = (CL_recpri*) ref_recpri.At(num);
	if( recpri->put_dEmcGeaClusterTrack(dEmcGeaClusterTrack) > 0 ){
	  vec_b_dEmcGeaClusterTrack[nn] = false;
	}
      }
    }
    // --- Input to part
    if( vec_b_dEmcGeaClusterTrack[nn] ){
      num = ref_part.GetEntries();
      while( num-- ){
	CL_part* part = (CL_part*) ref_part.At(num);
	if( part->put_dEmcGeaClusterTrack(dEmcGeaClusterTrack) > 0 ){
	  vec_b_dEmcGeaClusterTrack[nn] = false;
	}
      }
    }
  }
  
  num = 0;
  nn = ref_dEmcGeaClusterTrack.GetEntries();
  while( nn-- ) if( vec_b_dEmcGeaClusterTrack[nn] ) num++;
  if( num > 0 )
    cout<<" Warning:: Remained dEmcGeaClusterTrack = "<<num<<" / "<<
      ref_dEmcGeaClusterTrack.GetEntries()<<endl;
  nn_dEmcGeaClusterTrack_all += ref_dEmcGeaClusterTrack.GetEntries();
  nn_dEmcGeaClusterTrack_rej += num;
  delete[] vec_b_dEmcGeaClusterTrack;

  return true;
};
//======================================================================
void CL_event::Print(const char* opt) const{
  int num;
  TClonesArray& ref_recpri = *ar_recpri;
  TClonesArray& ref_part = *ar_part;
  float v_dr = sqrt(header.vertex[0]*header.vertex[0] + header.vertex[1]*header.vertex[1]);
  cout<<opt<<" --------------------------------------------------------------------------- "<<endl;
  cout<<opt<<" Run # : "<<header.run<<" Event # : "<<header.event<<" v=("<<header.vertex[0]<<","<<header.vertex[1]<<","<<header.vertex[2]<<") dr:"<<v_dr<<endl;
  cout<<opt<<" nn_dEmcGeaTrackCluster_all = "<<nn_dEmcGeaTrackCluster_all<<endl;
  cout<<opt<<" nn_dEmcGeaTrackCluster_rej = "<<nn_dEmcGeaTrackCluster_rej<<endl;
  cout<<opt<<" nn_dEmcGeaClusterTrack_all = "<<nn_dEmcGeaClusterTrack_all<<endl;
  cout<<opt<<" nn_dEmcGeaClusterTrack_rej = "<<nn_dEmcGeaClusterTrack_rej<<endl;
  num = ref_recpri.GetEntries();
  while( num-- ){
    CL_recpri* recpri = (CL_recpri*) ref_recpri.At(num);
    recpri->Print(opt);
  }
  num = ref_part.GetEntries();
  while( num-- ){
    CL_part* part = (CL_part*) ref_part.At(num);
    part->Print(opt);
  }
  cout<<opt<<" --------------------------------------------------------------------------- "<<endl;
};
//======================================================================
void CL_event::Reset(){
  ar_recpri->RemoveAll();
  ar_part->RemoveAll();
};
//======================================================================
