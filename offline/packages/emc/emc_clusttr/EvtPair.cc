
#include "EvtPair.hh"

ClassImp(EvtPair)

//=====================================================================
EvtPair::EvtPair(int maxsize){
  _vec_size = 0;
  _vec_capacity = maxsize;
  _vec_cltpair.reserve(_vec_capacity);
  ClustPair cltpair;
  int i = _vec_capacity;
  while( i-- )
    _vec_cltpair.push_back(cltpair);
  _pt_threshold = 1.0;
  _asym_cut = 1.0;

  _time_all = 0;
  _time_calc = 0;
  _time_input = 0;

};
//=====================================================================
EvtPair::~EvtPair(){
  _vec_cltpair.erase( _vec_cltpair.begin(), _vec_cltpair.end() );
  cout<<" EvtPair::Fill() takes "<<endl;
  cout<<"    All process : "<<-1.*(float)(_time_all)/1000.<<" seconds."<<endl;
  cout<<"    Calculation : "<<-1.*(float)(_time_calc)/1000.<<" seconds."<<endl;
  cout<<"    Set into vector : "<<-1.*(float)(_time_input)/1000.<<" seconds."<<endl;

};
//=====================================================================
void EvtPair::Reset(){
  // Global Information
  _glb0.Reset();
  _glb1.Reset();

  // Pair Information
  _vec_size = 0;
  _pt_threshold = 1.0;
  _asym_cut = 1.0;

};
//
//=====================================================================
void EvtPair::Fill(Evt& ev0,Evt& ev1,char* clust_selection=""){
  bool verbose = false;
  _glb0 = ev0._glb;
  _glb1 = ev1._glb;

  ClustPair* pt_cltpair = NULL;
  if( _vec_capacity == 0 ){
    _vec_cltpair.erase( _vec_cltpair.begin(), _vec_cltpair.end() );
    pt_cltpair = new ClustPair();
  }
  _vec_size = 0;

  //-------------------------------------------  Create Selector
  TTree* sel_tree;
  Clust* sel_clust;
  Clust* sel_pair;
  TTreeFormula* sel_form;
  bool sel_onoff = false;
  if( strlen(clust_selection) > 0 ){
    sel_tree = new TTree("tree","test");
    sel_tree->Branch("b_clust_sel","Clust",&sel_clust,32000,2);
    sel_form = new TTreeFormula("sel_form",clust_selection,sel_tree);
    sel_onoff = true;
  }

  // Pizero loop.............................................
  Float_t m,m0,mv0,e,pt,px,py,pz,cosine,cosine0,asym;
  Float_t len_tr0, len_tr1;
  Float_t norm_tr0[3], norm_tr1[3];
  int num,xyz;
  
  Int_t ncounter=0;
  Int_t passed=0;
  vector<Clust>& vec0 = ev0._vec_clt;
  vector<Clust>& vec1 = ev1._vec_clt;
  Global& glb0 = ev0._glb;
  Global& glb1 = ev1._glb;

  bool stat0 = true;
  bool stat1 = true;
  Int_t n0,n1;
  n0 = vec0.size();
  _time_all += (long)gSystem->Now();
  if(verbose)cout<<" scan:: n0 = "<<vec0.size()<<"   n1 = "<<vec1.size()<<endl;
  while( n0-- ){
    if( glb0.run == glb1.run && glb0.evn == glb1.evn )
      n1 = n0;
    else
      n1 = vec1.size();
    while( n1-- ){
      if( sel_onoff ){
	sel_clust = &(vec0[n0]);
	stat0 = sel_form->EvalInstance();
	sel_clust = &(vec0[n1]);
	stat1 = sel_form->EvalInstance();
      }
      if( stat0 && stat1 ){
	_time_calc += (long)gSystem->Now();
	//============================ Calculate Invariant mass
	len_tr0=sqrt((vec0[n0].pos[0])*(vec0[n0].pos[0])
		     +(vec0[n0].pos[1])*(vec0[n0].pos[1])
		     +(vec0[n0].pos[2]-glb0.bbcz)*(vec0[n0].pos[2]-glb0.bbcz));
	len_tr1=sqrt((vec1[n1].pos[0])*(vec1[n1].pos[0])
		     +(vec1[n1].pos[1])*(vec1[n1].pos[1])
		     +(vec1[n1].pos[2]-glb1.bbcz)*(vec1[n1].pos[2]-glb1.bbcz));
	
	norm_tr0[0] = vec0[n0].pos[0]/len_tr0;
	norm_tr0[1] = vec0[n0].pos[1]/len_tr0;
	norm_tr0[2] = (vec0[n0].pos[2]-glb0.bbcz)/len_tr0;
	norm_tr1[0] = vec1[n1].pos[0]/len_tr1;
	norm_tr1[1] = vec1[n1].pos[1]/len_tr1;
	norm_tr1[2] = (vec1[n1].pos[2]-glb1.bbcz)/len_tr1;
	
	px = vec0[n0].ecore * norm_tr0[0] + vec1[n1].ecore * norm_tr1[0];
	py = vec0[n0].ecore * norm_tr0[1] + vec1[n1].ecore * norm_tr1[1];
	pz = vec0[n0].ecore * norm_tr0[2] + vec1[n1].ecore * norm_tr1[2];
	pt = sqrt( px*px + py*py );

	e = vec0[n0].ecore + vec1[n1].ecore;
	asym = fabs( vec0[n0].ecore - vec1[n1].ecore )/e;
	//====================
	_time_calc -= (long)gSystem->Now();
	_time_input += (long)gSystem->Now();
	//====================
	if(pt >= _pt_threshold && _vec_size < _vec_capacity && asym < _asym_cut ){
	  passed++;
	  //====================
	  cosine = norm_tr0[0]*norm_tr1[0]
	    + norm_tr0[1]*norm_tr1[1]+norm_tr0[2]*norm_tr1[2];
	  m = sqrt(2.0 * vec0[n0].ecore * vec1[n1].ecore * (1.0-cosine));
	  m0 = sqrt(2.0 * vec0[n0].e * vec1[n1].e * (1.0-cosine));
	  mv0 = 0;
	  //============================ Calculate invariant mass at 0 fixed vertex
	  ClustPair& cltpair = _vec_capacity==0 ? *pt_cltpair  :_vec_cltpair[_vec_size];
	  cltpair.Reset();
	  cltpair.glb0 = glb0;
	  cltpair.glb1 = glb1;
	  cltpair.clt_num0 = vec0.size();
	  cltpair.clt_num1 = vec1.size();
	  cltpair.m = m;
	  cltpair.mv0 = mv0;
	  cltpair.m0 = m0;
	  cltpair.e = e;
	  cltpair.pt = pt;
	  cltpair.px = px;
	  cltpair.py = py;
	  cltpair.pz = pz;
	  cltpair.cosine = cosine;
	  cltpair.asym = asym;
	  cltpair.clt0 = vec0[n0];
	  cltpair.clt1 = vec1[n1];
	  cltpair.clt_id0 = n0;
	  cltpair.clt_id1 = n1;
	  num = ev0._vec_assclt[n0].GetSize();
	  cltpair.trk_num0 = num;
	  if( ev0.GetAssTrk(n0,0) > 0 )
	    cltpair.trk0 = ev0._vec_trk[ev0.GetAssTrk(n0,0)];
	  if( num > 5 ) num = 5;
	  while( num-- ){
	    cltpair.trk_id0[num] = ev0._vec_assclt[n0].GetID(num);
	    cltpair.dist0[num] = ev0._vec_assclt[n0].GetDist(num);
	    xyz = 3;
	    while( xyz-- )
	      cltpair.dist0xyz[num][xyz] = ev0._vec_assclt[n0]._distxyz[xyz][num];
	  }
	  num = ev1._vec_assclt[n1].GetSize();
	  cltpair.trk_num1 = num;
	  if( ev1.GetAssTrk(n1,0) > 0 )
	    cltpair.trk1 = ev1._vec_trk[ev1.GetAssTrk(n1,0)];
	  if( num > 5 ) num = 5;
	  while( num-- ){
	    cltpair.trk_id1[num] = ev1._vec_assclt[n1].GetID(num);
	    cltpair.dist1[num] = ev1._vec_assclt[n1].GetDist(num);
	    xyz = 3;
	    while( xyz-- )
	      cltpair.dist1xyz[num][xyz] = ev1._vec_assclt[n1]._distxyz[xyz][num];
	  }
	  //====================================================================
	  _vec_size++;
	  if( _vec_capacity == 0 )
	    _vec_cltpair.push_back(cltpair);
	  //====================================================================
	} // End of pt cut 
	_time_input -= (long)gSystem->Now();
	ncounter++;
      }
    } // End of n1 loop
  } // End of n0 loop

  _time_all -= (long)gSystem->Now();

  if( _vec_capacity == 0 )
    delete pt_cltpair;

  if( sel_onoff ){
    delete sel_form;
    delete sel_tree;
  }

};

//=====================================================================

