#include "mToHDF5.h"
#include <MpcExConstants.h>
#include <MpcExMapper.h>
#include <TMpcExShower.h>
#include <TMpcExHitContainer.h>
#include <map>
#include <H5Cpp.h>
#include <TFile.h>

const double MINSPACE=-20; //20cm
const double DSPACE=40./214.;//in cm
const double SHOWERSIZE=6;//a size of 6cm x 6cm
const int NSPACE=int(2*SHOWERSIZE/DSPACE)+1;


mToHDF5::mToHDF5(std::string name){
  ResetData();
  _entries=0;
  _h5 = new H5::H5File(name,H5F_ACC_TRUNC);
  _compress=6;
  _mpcex_raw_e=0;
  memset(_one_hot,0,sizeof(float)*ToH5::NTYPE);

  //create dims for 2d hit array
  ToH5::Dims dims;

  //mpcex raw
  AddVar("dp_mpcex_raw_e","Float",&_mpcex_raw_e,dims);

  dims.push_back(ToH5::NX);
  dims.push_back(ToH5::NY);
  dims.push_back(ToH5::NZ);

  AddVar("2d_hits","Float",_data,dims);

  dims.clear();
  dims.push_back(ToH5::NTYPE);
  AddVar("dp_type","Float",_one_hot,dims);

}

void mToHDF5::AddVar(std::string name,std::string dtype,void* var_pt,const ToH5::Dims dims){
  if(dtype.compare("Int")!=0 && dtype.compare("Float")!=0){
    std::cout<<"Input Type should be Float or Int !"<<std::endl;
    return;
  }

  if(_var_name_set.find(name)!=_var_name_set.end()){
    std::cout<<"Variables used"<<std::endl;
    return;
  }

  if(_entries>0){
    std::cout<<"H5 File already has been filled ! can not add the variable !"<<std::endl;
    return;
  }

  _name_list.push_back(name);
  _var_pt_list.push_back(var_pt);
  _dm_list.push_back(dims);
  _dtype_list.push_back(dtype);
  _var_name_set.insert(name);

}

void mToHDF5::CreateAllVars(){
  for(unsigned int i=0;i<_name_list.size();i++){
    std::string name = _name_list[i];
    std::string dtype = _dtype_list[i];
    void* var_pt = _var_pt_list[i];
    ToH5::Dims dims = _dm_list[i];
    CreateVar(name,dtype,var_pt,dims);
  }
}


void mToHDF5::CreateVar(std::string name,std::string dtype,void* var_pt,const ToH5::Dims dims){
  if(dtype.compare("Int")!=0 && dtype.compare("Float")!=0){
    std::cout<<"Input Type should be Float or Int !"<<std::endl;
    return;
  }

  //the added additional dimentional is the 
  //row number
  unsigned int rank = dims.size()+1;
//  std::cout<<"number of total rank: "<<rank<<std::endl;
  hsize_t hdims[rank];
  hsize_t max_hdims[rank];
  hdims[0]=1;
  //make the first dimension extenable
  max_hdims[0] = H5S_UNLIMITED;
  for(unsigned int i=0;i<dims.size();i++){
    hdims[i+1] = dims[i];
    max_hdims[i+1] = dims[i];
  }
  
  H5::DataSpace dataspace(rank,hdims,max_hdims);
  
  H5::DSetCreatPropList cparms;
  cparms.setChunk(rank,hdims);
  cparms.setDeflate(_compress);
  
  if(dtype.compare("Float")==0){
    float fill_val = 0;
    cparms.setFillValue(H5::PredType::NATIVE_FLOAT,&fill_val);
    H5::DataSet* ds = new H5::DataSet(_h5->createDataSet(name,H5::PredType::NATIVE_FLOAT,dataspace,cparms));
    _ds_list.push_back(ds);
  }

  if(dtype.compare("Int")==0){
    int fill_val = 0;
    cparms.setFillValue(H5::PredType::NATIVE_INT,&fill_val);
    H5::DataSet* ds = new H5::DataSet(_h5->createDataSet(name,H5::PredType::NATIVE_INT,dataspace,cparms));
    _ds_list.push_back(ds);
  }
}

bool mToHDF5::Set2DHits(TMpcExShower* shower,TMpcExHitContainer* hits){
  ResetData();
  if(!shower || !hits){
    std::cout<<"invalid shower or hits pointer !"<<std::endl;
  }
  
  _mpcex_raw_e = shower->get_raw_esum();

  double vertex = shower->get_vertex();
  // Want front face of first Si layer so account for 2mm W layer
  double z=MpcExConstants:: PS_REFERENCE_Z_N + 0.2;
  if(shower->get_arm()==0) z=MpcExConstants::PS_REFERENCE_Z_S - 0.2;
  double ct_x = shower->get_hsx()*(z-vertex);
  double ct_y = shower->get_hsy()*(z-vertex);
  
  //GeV->keV
  double raw_ex_e = shower->get_raw_esum()*1.0e6;

  // //set the hit for fast look up
  // std::map<int,TMpcExHit*> hit_map;
  // for(unsigned int i=0;i<hits->size();i++){
  //   TMpcExHit* hit = hits->getHit(i);
  //   hit_map[hit->key()]=hit;
  // }

  unsigned int nhits = shower->sizeHits();
  for(unsigned int i=0;i<nhits;i++){
    //int key = shower->getHit(i);
    //TMpcExHit* hit = hit_map[key];
    TMpcExHit* hit = shower->getHit(i,hits);
    double x = hit->x()-ct_x;
    double y = hit->y()-ct_y;
    double e = hit->combined()/raw_ex_e;
    int layer = hit->layer();

    bool is_valid=SetArrayByHit(layer,x,y,e);
    if(!is_valid) return false;
  }
  
  SetOneHot(int(shower->get_roughTotE()));

  return true;
}

void mToHDF5::SetOneHot(int e){
  for(int i=0;i<14;i++) _one_hot[i]=0;
  if(e<30) {
      _one_hot[0] = 1;
          return;
  }
  if(e>=100){
      _one_hot[13]=1;
          return;
  }

  int index = (e-30)/5;
  _one_hot[index]=1;
}


bool mToHDF5::SetArrayByHit(int layer,double x,double y,double e){
  int binx0 = int((x+SHOWERSIZE)/DSPACE);
  int binx1 = binx0+1;
  int biny0 = int((y+SHOWERSIZE-DSPACE/2.*7.)/DSPACE);
  int biny1 = int((y+SHOWERSIZE+DSPACE/2.*7.)/DSPACE)+1;

  if(layer%2==1){
    biny0 = int((y+SHOWERSIZE)/DSPACE);
    biny1 = biny0+1;
    binx0 = int((x+SHOWERSIZE-DSPACE/2.*7)/DSPACE);
    binx1 = int((x+SHOWERSIZE+DSPACE/2.*7)/DSPACE)+1;
  }
     
  for(int ix=binx0;ix<binx1;ix++){
    for(int iy=biny0;iy<biny1;iy++){
      if(ix<0||ix>=NSPACE){
          continue;
      }
      if(iy<0||iy>=NSPACE){
	continue;
      }
      _data[0][ix][iy][layer] = e;
    }
  }
  return true;
}

void mToHDF5::Fill(){
  if(_entries==0) CreateAllVars(); 

  for(unsigned int i=0;i<_ds_list.size();i++){
    unsigned int rank = _dm_list[i].size()+1;
    hsize_t extend_size[rank];
    hsize_t offset[rank];
    hsize_t fill_dims[rank];
    extend_size[0] = _entries+1;
    offset[0] = _entries;
    fill_dims[0] = 1;
    for(unsigned int j=1;j<rank;j++){
      hsize_t d = _dm_list[i][j-1];
      extend_size[j] = d;
      fill_dims[j] = d;
      offset[j] = 0;
      fill_dims[j] = d;
    }
    
    H5::DataSet* ds = _ds_list[i];
    void* var_pt = _var_pt_list[i];
    
    ds->extend(extend_size);
  
    H5::DataSpace fill_space = ds->getSpace();

    //memory space
    H5::DataSpace mspace(rank,fill_dims);
  
    //select the read position
    fill_space.selectHyperslab(H5S_SELECT_SET,fill_dims,offset);
    if(_dtype_list[i].compare("Float")==0){
      ds->write(var_pt,H5::PredType::NATIVE_FLOAT,mspace,fill_space);
    }

    if(_dtype_list[i].compare("Int")==0){
      ds->write(var_pt,H5::PredType::NATIVE_INT,mspace,fill_space);
    }
  } 
  _entries++;
}


mToHDF5::~mToHDF5(){
  for(unsigned int i=0;i<_ds_list.size();i++){
//    _ds_list[i]->close();  
    if(_ds_list[i]) delete _ds_list[i];
    _ds_list[i]=0;
  }
  _ds_list.clear();

  if(_h5) delete _h5;
  _h5 = 0;
  
}

void mToHDF5::ResetData(){
  _mpcex_raw_e=0;
  for(unsigned int i=0;i<ToH5::N0;i++){
    for(unsigned int j=0;j<ToH5::NX;j++){
      for(unsigned int k=0;k<ToH5::NY;k++){
	for(unsigned int m=0;m<ToH5::NZ;m++){
	  _data[i][j][k][m]=0;
	}
      }
    }
  }

  for(unsigned int i=0;i<ToH5::NTYPE;i++){
    _one_hot[i] = 0;
  }
}
