//Hbd Cluster

#ifndef __HBDCLUSTER_H__
#define __HBDCLUSTER_H__

//c++ includes                                                                          
#include <iostream>
#include <vector>
#include <deque>
#include <map>
#include <string>
#include <algorithm>


//phenix includes                                                                       
#include <HbdCellList.h>
#include <HbdCell.h>
#include <HbdCellv1.h>
#include <SubsysReco.h>
#include <PHGlobal.h>

#include <TFile.h>
#include <TTree.h>
#include <TROOT.h>

class HbdCellv1;


//    
// Utility class to define cluster from cells
//
class HbdCluster : public TObject {
   public: 
      

      HbdCluster() {
         clusterid=-1;
         cluster_cells.clear();
    //add size, sector    
         TotChg=0.0;
         PosY=PosZ=RmsY=RmsZ=0.0;
         localmax=0.0;
      }

      HbdCluster(const HbdCluster& rhs)
      {
         rhs.copyto(*this);
      }
      
      ~HbdCluster()
      {
         cluster_cells.clear(); //
      }  
      void
         copyto(HbdCluster& dest) const
         {
            dest.clusterid=clusterid;
            dest.cluster_cells.clear();
            dest.cluster_cells=cluster_cells;
            dest.TotChg=TotChg;
            dest.PosY=PosY;
            dest.PosZ=PosZ;
            dest.RmsY=RmsY;
            dest.RmsZ=RmsZ;
         }

      int get_ncells(){return cluster_cells.size();};

      void add_cell(HbdCell *newcell, double weight=1.0){
         cluster_cells.push_back(newcell);
         TotChg+=(newcell->get_charge())*weight;

         // Find localmax ...
         float charge_temp=0.0;
         localmax =0.0;
         for(unsigned int i=0; i<cluster_cells.size(); i++){
            charge_temp = cluster_cells.at(i)->get_charge();
            if(charge_temp>localmax){
               localmax = charge_temp;
            }
         }

      }


      // This should be set to itrk after track association.
      void set_clusterid(int clusid){
         clusterid=clusid;
      }


      void clear(){
         clusterid=-1;
         cluster_cells.clear();
         TotChg=0.0;
         PosY=PosZ=RmsY=RmsZ=0.0;
         TotChg = 0.0;
      }

      // Return total charge for cluster
      float get_charge(){
         return TotChg;
      }

      float get_localmax(){
         return localmax;
      }


      HbdCluster * clone()
      {
         return new HbdCluster(*this);
      }

      void print(){
         std::vector<HbdCell*>::iterator ic;
         for(ic=cluster_cells.begin();ic!=cluster_cells.end(); ++ic){
            if((*ic)){
               std::cout << " padnumber " << (*ic)->get_padnum() << "  ";
               std::cout << " sector " << (*ic)->get_sector() << "  ";
               std::cout << " charge " << (*ic)->get_charge() << "  ";
               std::cout << std::endl;
            }
            else{
               std::cout << "Got a null Pointer to cell " << std::endl;
            }
         }
         std::cout << "PosY, PosZ " << PosY << " " << PosZ << " Total Charge " << TotChg << std::endl;
      }

      float TotChg;              // Sum of Charge of cells in this cluster 
      float PosY, PosZ;          // Center-Of-gravity Position (Local)
      float RmsY, RmsZ;          // Rms of Center-Of-gravity Position (Local)
      int clusterid;             // ith cluster
      float localmax;
      std::vector<HbdCell*> cluster_cells;

  private:

    ClassDef(HbdCluster, 0);


};


#endif

