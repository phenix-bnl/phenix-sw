#include "emcEmbedMipCorrAfterBurnerv1.h"

#include "PHCompositeNode.h"
#include "PHTypedNodeIterator.h"
#include "dEmcCalibTowerWrapper.h"
#include "PHNodeHelper.h"
#include "emcEmbedUtils.h"
#include "TF1.h"
#include <iostream>
#include <fstream>

ClassImp(emcEmbedMipCorrAfterBurnerv1)

//_____________________________________________________________________________
emcEmbedMipCorrAfterBurnerv1::emcEmbedMipCorrAfterBurnerv1() : 
  PHAfterBurner()
{
}

//_____________________________________________________________________________
emcEmbedMipCorrAfterBurnerv1::~emcEmbedMipCorrAfterBurnerv1()
{
}

//_____________________________________________________________________________
void
emcEmbedMipCorrAfterBurnerv1::apply(PHCompositeNode* top)
{

  
  dEmcCalibTowerWrapper* tower_ptr =
    PHNodeHelper<dEmcCalibTowerWrapper>::getTable("dEmcCalibTower",top);



  int ntowers = tower_ptr->RowCount();
  int iarm,isect,iy,iz;

  for(int irow = 0;irow<ntowers;irow++){
    iarm = tower_ptr->get_arm(irow);
    isect = tower_ptr->get_sector(irow);
    // 8-12-03...jfrantz only apply to pbgl, as this was only generated for pbgl
    if (iarm != 1 || (iarm == 1 && isect > 1)) continue; 
    iz = tower_ptr->get_ind(0,irow);
    iy = tower_ptr->get_ind(1,irow);
    if( _corr_twr_stat[iarm][isect][iz][iy] ){
      float energy = tower_ptr->get_ecal(irow) * _corr_twr_mip[iarm][isect][iz][iy];
      tower_ptr->set_ecal( irow, energy );
    }
  }


  /*
  dEmcCalibTowerWrapper towers = *tower_ptr; 
  int ntowers = towers.RowCount();
  int iarm,isect,iy,iz;

  for(int irow = 0;irow<ntowers;irow++){
    iarm = towers.get_arm(irow);
    isect = towers.get_sector(irow);
    iz = towers.get_ind(0,irow);
    iy = towers.get_ind(1,irow);
    if( _corr_twr_stat[iarm][isect][iz][iy] ){
      float energy = towers.get_ecal(irow) * _corr_twr_mip[iarm][isect][iz][iy];
      towers.set_ecal( irow, energy );
    }
  }
  */

}

//_____________________________________________________________________________
void
emcEmbedMipCorrAfterBurnerv1::identify(ostream& os) const
{
  os << "<I> emcEmbedMipCorrAfterBurnerv1 - "
     << "RUN2 SIMULATED EmCal Energy After Burner (tower level)" << endl;
}

//_____________________________________________________________________________

void
emcEmbedMipCorrAfterBurnerv1::initialize(PHCompositeNode*)
{
  // default ctor
  int iarm,isect,iz,iy;
  iarm = 2;
  while( iarm-- ){
    isect = 4;
    while( isect-- ){
      iz = 96;
      while( iz-- ){
        iy = 48;
        while( iy-- ){
          _corr_twr_mip[iarm][isect][iz][iy] = 1;
          _corr_twr_err[iarm][isect][iz][iy] = 0;
          _corr_twr_stat[iarm][isect][iz][iy] = false;
        }
      }
    }
  }

  int read_num;
  int swkey,status;
  float corr,corr_err;
  char tmpline[128];
  char *filename = "/phenix/workarea/kleinb/run02/corr/CF_comb_all_1_2_3_4_1_inv_i_fac115.dat";
  ifstream fin (filename);
  if(!fin.good()) {
    cerr<< PHWHERE <<"  Can't open file: "<<filename<<endl;
    return;
  }
  cout<<"<I> emcEmbedMipCorrAfterBurnerv1: Opening tower correction file : "<<filename<<endl;
  while( fin.getline(tmpline,128) ){
    if( tmpline[0] != '#' ){
      read_num = sscanf(tmpline,"%d %f %f %d",&swkey,&corr,&corr_err,&status);
      if( read_num == 4 ){
        iarm = (int)( swkey / 100000 );
        isect = (int)( ( swkey - iarm * 100000 )/10000 );
        iy = (int)( ( swkey - iarm*100000 - isect*10000 )/100 );
        iz = (int)( swkey - iarm*100000 - isect*10000 - iy*100 );
        if( iarm >= 0 && iarm < 2 &&
            isect >= 0 && isect < 4 &&
            iy >= 0 && iy < 48 &&
            iz >= 0 && iz < 96 ){
          _corr_twr_mip[iarm][isect][iz][iy] = corr;
          _corr_twr_err[iarm][isect][iz][iy] = corr_err;
          if( status > 0 )
            _corr_twr_stat[iarm][isect][iz][iy] = true;
          else
            _corr_twr_stat[iarm][isect][iz][iy] = false;
        } else {
          cout<< PHWHERE <<endl;
          cout<<"             -- Tower id is out of range : "<<filename<<endl;
        }
      } else {
	// nothing for now
      }
    }
  }
  fin.close();

  return;

}



