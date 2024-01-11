#ifndef __MTOHDF5__H__
#define __MTOHDF5__H__
/**
 *Transfer TMpcExShower or ExShower
 *to a file of HDF5
 * **/
#include <string>
#include <vector>
#include <set>

//important, or it will conflict
//with the Root CINT
namespace H5 {
  class H5File;
  class DataSet;
  class DataSpace;
}

//dimension for the array
//the array can extendable
//in N0 direction
namespace ToH5{
  const int RANK = 4;
  const int N0=1;
  const int NX=65;
  const int NY=65;
  const int NZ=8;
  const int NTYPE=14;
  //defines the vector as Dims
  typedef std::vector<int> Dims;
}


class ExShower;
class TMpcExShower;
class TMpcExHitContainer;


class mToHDF5{
  private:
    H5::H5File* _h5;
    //each data set is a table
    std::vector<H5::DataSet*> _ds_list;
    //memory space list
    //std::vector<H5::DataSpace*> _mem_space_list;
    //demension of each one
    std::vector<ToH5::Dims> _dm_list;
    //pointer to each value
    std::vector<void*> _var_pt_list;
    //name list
    std::vector<std::string> _name_list;
    //data type
    //Only Int or Float
    std::vector<std::string> _dtype_list;
    
    //record the position for the var
    std::set<std::string> _var_name_set;
    
    
    //this use this to hold the hits 2d for classification;
    float _data[ToH5::N0][ToH5::NX][ToH5::NY][ToH5::NZ];
    float _mpcex_raw_e;
    float _one_hot[ToH5::NTYPE];
    void ResetData();

    //record number of entries
    unsigned int _entries;
    unsigned int _compress;

    void CreateVar(std::string name,std::string dtype,void* var_pt,const ToH5::Dims dims);
    void CreateAllVars();


  public:
    mToHDF5(std::string name="Ex.h5");
    virtual ~mToHDF5();
    bool Set2DHits(TMpcExShower* shower,TMpcExHitContainer* hits);
    bool SetArrayByHit(int layer,double x,double y,double e);
    void SetCompressLevel(unsigned int val) {_compress = val;}
    //only supoort float type
    void AddVar(std::string name,std::string dtype,void* var_pt,const ToH5::Dims dims);
    void SetOneHot(int e);
    void Fill();
};

#endif
