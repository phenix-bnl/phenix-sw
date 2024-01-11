#include "MutrgSelectHit.hh"

#include "MutrgDataProcessUtil.hh"
#include "MutrgHitArray.hh"

using namespace std;

///////////////////////////////////////////////////////////////

MutrgSelectHit::MutrgSelectHit(bool init_flag){
  class_name="MutrgSelectHit";
  mutrg_hits_sel=NULL;
  if(init_flag){CreateObject();} // MutrgSelectHit::CreateObject

  hit_clk_ext=0;
  hit_clk_shift=0;
  do_multiplicity_cut=false;
  do_clsize_cut=false;
  do_clustering=false;
  max_cluster_size=1;

  for(int iarm=0; iarm<MutrgPar::NARM; iarm++){
    for(int ist=0; ist<MutrgPar::NSTATION; ist++){
      multiplicity_threshold[iarm][ist]=9999;
      clsize_threshold[iarm][ist]=9999;
    }
  }
}

///////////////////////////////////////////////////////////////

MutrgSelectHit::~MutrgSelectHit(void){
}

////////////////////////////////////////////////////////////////

void MutrgSelectHit::CreateObject(void){
  if(!mutrg_hits_sel){mutrg_hits_sel=new MutrgHitArray("MutrgHit");}
  return;
}

////////////////////////////////////////////////////////////////

int MutrgSelectHit::SetMutrgSelHitArray(MutrgHitArray *hits,bool flag_delete){
  return MutrgDataProcessUtil::SetMutrgObject(mutrg_hits_sel,hits,flag_delete);
}

////////////////////////////////////////////////////////////////

MutrgHitArray*
MutrgSelectHit::RegMutrgSelHitArray(PHCompositeNode *node,
				    const char *name,const char *rename){
  return MutrgDataProcessUtil::RegMutrgObject(mutrg_hits_sel,
					      node,name,rename);
}

//////////////////////////////////////////////////////////////////

int MutrgSelectHit::Init(PHCompositeNode *node,bool flag_reg){
  MutrgDataProcessUtil::WarnDefFuncCall(ClassName(),"Init");
  return 0;
}

/////////////////////////////////////////////////////////////////

int MutrgSelectHit::InitRun(PHCompositeNode *node,bool flag_reg){
  MutrgDataProcessUtil::WarnDefFuncCall(ClassName(),"InitRun");
  return 0;
}

/////////////////////////////////////////////////////////////////////

int MutrgSelectHit::ProcessEvent(PHCompositeNode *node){
  MutrgDataProcessUtil::WarnDefFuncCall(ClassName(),"ProcessEvent");
  return 0;
}

////////////////////////////////////////////////////////////////////

void MutrgSelectHit::DoExtendHitClock(int clk_ext,int clk_shift){
  hit_clk_ext=clk_ext;
  hit_clk_shift=clk_shift;
  return;
}

/////////////////////////////////////////////////////////////////////////

void MutrgSelectHit::DoMultiplicityCut(int thre){
  DoMultiplicityCut(thre,thre,thre,thre,thre,thre);
  return;
}

/////////////////////////////////////////////////////////////////////////

void MutrgSelectHit::DoMultiplicityCut(int thre_s0,int thre_s1,int thre_s2,
					  int thre_n0,int thre_n1,int thre_n2){
  multiplicity_threshold[0][0]=thre_s0;
  multiplicity_threshold[0][1]=thre_s1;
  multiplicity_threshold[0][2]=thre_s2;
  multiplicity_threshold[1][0]=thre_n0;
  multiplicity_threshold[1][1]=thre_n1;
  multiplicity_threshold[1][2]=thre_n2;

  if(thre_s0>0 || thre_s1>0 || thre_s2>0 ||
     thre_n0>0 || thre_n1>0 || thre_n2>0){
    do_multiplicity_cut=true;
  }
  else{
    do_multiplicity_cut=false;
  }

  return;
}

////////////////////////////////////////////////////////////

void MutrgSelectHit::DoClusterSizeCut(int thre){
  DoClusterSizeCut(thre,thre,thre,thre,thre,thre);
  return;
}

////////////////////////////////////////////////////////////////

void MutrgSelectHit::DoClusterSizeCut(int thre_s0,int thre_s1,int thre_s2,
					 int thre_n0,int thre_n1,int thre_n2){
  clsize_threshold[0][0]=thre_s0;
  clsize_threshold[0][1]=thre_s1;
  clsize_threshold[0][2]=thre_s2;
  clsize_threshold[1][0]=thre_n0;
  clsize_threshold[1][1]=thre_n1;
  clsize_threshold[1][2]=thre_n2;

  if(thre_s0>0 || thre_s1>0 || thre_s2>0 ||
     thre_n0>0 || thre_n1>0 || thre_n2>0){
    do_clsize_cut=true;
  }
  else{
    do_clsize_cut=false;
  }

  return;
}

////////////////////////////////////////////////////////////

void MutrgSelectHit::DoClustering(bool flag,int max_size){
  if(flag && max_size<1){
    printf("Error - %s::DoClustering : max_cluster_size must be >=1.",
	   ClassName());
    printf("Error - %s::DoClustering : clustering is disabled.\n",ClassName());
    return;
  }

  do_clustering=flag;
  max_cluster_size=max_size;
  return;
}

////////////////////////////////////////////////////////////

void MutrgSelectHit::PrintParameters(ostream &os) const{
  os << ClassName() << "::PrintParameters : "
     << "hit_clk_ext = " << hit_clk_ext << endl;
  os << ClassName() << "::PrintParameters : "
     << "hit_clk_shift = " << hit_clk_shift << endl;

  os << ClassName() << "::PrintParameters : Multiplicity Cut : South";
  for(int ist=0; ist<MutrgPar::NSTATION; ist++){
    os << " " << multiplicity_threshold[0][ist];
  }
  os << endl;

  os << ClassName() << "::PrintParameters : Multiplicity Cut : North";
  for(int ist=0; ist<MutrgPar::NSTATION; ist++){
    os << " " << multiplicity_threshold[1][ist];
  }
  os << endl;

  os << ClassName() << "::PrintParameters : Cluster Size Cut : South";
  for(int ist=0; ist<MutrgPar::NSTATION; ist++){
    os << " " << clsize_threshold[0][ist];
  }
  os << endl;

  os << ClassName() << "::PrintParameters : Cluster Size Cut : North";
  for(int ist=0; ist<MutrgPar::NSTATION; ist++){
    os << " " << clsize_threshold[1][ist];
  }
  os << endl;

  os << ClassName() << "::PrintParameters : Clustering = " << do_clustering
     << " : Max size = " << max_cluster_size << endl;

  return;
}

////////////////////////////////////////////////////////////////////
