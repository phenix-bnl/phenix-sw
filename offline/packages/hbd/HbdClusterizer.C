//  HBD Clustering
//  Anne Sickles 2/6/2007
//
#include "HbdClusterizer.h"
#include "HbdBlobList.h"
#include "HbdGhitList.h"
#include "HbdNeighbours.h"
#include "HbdFinalSimSupport.h"

using namespace std;


HbdClusterizer::HbdClusterizer()
{
   ChargeThreshold = 8;
}

HbdClusterizer::~HbdClusterizer()
{}

int HbdClusterizer::Init(PHCompositeNode *topNode)
{
  //run numbers from before 11/2006 will get the prototype geometry
  hbdgeo.fetch(215034); 
  return 0;
}

int HbdClusterizer::process_event(PHCompositeNode *topNode)
{
   CellList = findNode::getClass<HbdCellList>(topNode, "HbdCellList");
   if (!CellList)
      cout << PHWHERE << "HbdClusterizer:: No HbdCellList!" << endl;
   
   BlobList = findNode::getClass<HbdBlobList>(topNode, "HbdBlobList");
   if (!BlobList)
      cout << PHWHERE << "HbdClusterizer:: No HbdBlobList!" << endl;

   //
   // If there is no Ghit, just ignore. FillHbdBlob will determine
   //  whether it is simulation or real
   GhitList = findNode::getClass<HbdGhitList>(topNode, "HbdGhitList");
   cluster(CellList);
   split();
   return fill(BlobList);
}

void HbdClusterizer::cluster(HbdCellList *CellList){
   // Start clustering
   clusters.clear();
   int nclusters =0;
   for(int i=0; i<CellList->get_nCells(); i++){

      HbdCell *icell = (HbdCell*)CellList->get_cell(i);
         // Requires Hits in the cell
	 if(icell->get_charge()<ChargeThreshold)continue; 
//	 if(icell->get_sector()>5)continue;//for the time being remove the west from clustering
	 //this threshold is arb. right now
	       
	 int cell_sect = icell->get_sector(); 
	 int cell_pad = icell->get_padnum();

	 map<int,HbdClusUtil>::iterator ic;
         //
         // Scan over existing clusters to find the cluster
         // that the cell is supposed to be belonging to
         //
	 
         for(ic=clusters.begin(); ic!=clusters.end(); ++ic){
            // Scan over cells in the cluster
	    vector<HbdCell*>::iterator cci;
	    for(cci=ic->second.cluster_cells.begin(); cci!=ic->second.cluster_cells.end(); ++cci){
	       int sect = (*cci)->get_sector();
	       int pad = (*cci)->get_padnum();
	       if(isNeighbour(sect,pad,cell_sect,cell_pad)){
                  // has the cell has not been used in any clusters.
                  if(icell->get_clusterid()<0){
		     ic->second.add_cell(icell);
		     icell->set_clusterid(ic->first);
		     break; //we have found a cluster for this cell
		  }else{ //merge the clusters
		     merge((ic->first),icell->get_clusterid());
		     break;
		  }
	       }//end of if neighbors
	    }//end of loop over cells in cluster
	 }//end of loop over clusters
	 if(icell->get_clusterid()<0){//this cell starts a new cluster
	    int clusid=clusters.size(); //returns the number of clusters
	    clusters[nclusters]= HbdClusUtil();
	    clusters[nclusters].set_clusterid(clusid);
	    clusters[nclusters].add_cell(icell);
//	    HbdClusUtil *newcluster = new HbdClusUtil();
//	    clusters[nclusters]=*newcluster;
	    nclusters++;
	 }
	 

   }

   if(verbosity){
      cout << "HbdClusterizer::cluster(): " << clusters.size();
      cout << " blobs found " << endl;
   }

}


//
// This is to do some resetting stuff after processing each event
//
int HbdClusterizer::ResetEvent(PHCompositeNode *topNode)
{
   clusters.clear();

   return 0;
}


void HbdClusterizer::findlocalmaxima(){
   // --------------------------------------------------------------
   // Cluster splitting--find the local maxima
   //
   // If we find more than one local maximum, we tag it is merged
   
   if(verbosity){
      cout << "HbdClusterizer::findlocalmaxima()" << endl;
      cout << "starting with " << clusters.size() << " clusters" << endl;
   }
   // First, search for local maximums in clusters
   //  charge in local maximum supposed to be larger
   //   then charge of all neighbours
   map<int,HbdClusUtil>::iterator iclus;
   for(iclus=clusters.begin(); iclus!=clusters.end(); ++iclus){
      if(iclus->second.get_ncells()<3)continue;
      vector<HbdCell*>::iterator icell, jcell;
      //loop over all the cells in the cluster
      for(icell=(iclus->second.cluster_cells).begin();
	   icell!=(iclus->second.cluster_cells).end(); ++icell){
	 int neighbors=0, small_neighbors=0;
	 int sect1=(*icell)->get_sector();
	 int pad1=(*icell)->get_padnum();
	 //look for neighboring cells
	 for(jcell=(iclus->second.cluster_cells).begin();
	       jcell!=(iclus->second.cluster_cells).end(); ++jcell){
	    int sect2=(*jcell)->get_sector();
   	    int pad2=(*jcell)->get_padnum();
	    //check if the cells are neighboring...
	    if(!isNeighbour(sect1, pad1, sect2, pad2))continue;
	    neighbors++;
	    if((*icell)->get_charge()/(*jcell)->get_charge()>CDIFF){
	       small_neighbors++; 
	    }
	 } //end loop over j cells
	 if(neighbors==small_neighbors){ //found local max
	    iclus->second.local_max[(*icell)->get_padnum()]=(*icell);
	 }
      } //end loop over i cells
   } //end loop over clusters
   if(verbosity){
      cout << "HbdClusterizer::findlocalmaxima()" << endl;
      cout << "ending with " << clusters.size() << " clusters" << endl;
   }
}

void HbdClusterizer::split(){
   findlocalmaxima();
   map<int,HbdClusUtil>::iterator iclus;
   for(iclus=clusters.begin(); iclus!=clusters.end(); ++iclus){
      if(iclus->second.parent_clusterid>-1)continue;//the cluster is already split
      //don't split if only one local maximum
      if(iclus->second.local_max.size()<2)continue;
      else{ //make the split clusters
	 map<int,HbdCell*>::iterator icell;
	 //make a new cluster for each local maximum
	 for(icell=(iclus->second.local_max).begin();
		  icell!=(iclus->second.local_max).end(); ++icell){
      		  HbdClusUtil *newcluster = new HbdClusUtil();
      		  int clusid=clusters.size(); //returns the number of clusters
//	 	  newcluster->parent_clusterid=iclus->second.clusterid;
//     		  clusters[clusid+1]=*newcluster;
      		  clusters[clusid+1] = HbdClusUtil();
      		  clusters[clusid+1].parent_clusterid = iclus->second.clusterid;
      		  clusters[clusid+1].add_cell(icell->second);
		  clusters[clusid+1].local_max[icell->second->get_padnum()]=icell->second;
		  iclus->second.daughterclusters.push_back(clusid+1);
		  delete newcluster;
	 }
      }
   }//we have just made the split clusters
   //now we want to add the non-local maximum cells to the new clusters
   int ic = 0;
   for(iclus=clusters.begin(); iclus!=clusters.end(); ++iclus){
      ic++;
      vector<HbdCell*>::iterator icell;
      vector<HbdCell*> tmp_cells;
      tmp_cells.clear();
      if(iclus->second.local_max.size()<2)continue;
      //icell is a non-local maximum cell in the cluster
      if(iclus->second.parent_clusterid>-1)continue; //only loop over original clusters
      if(iclus->second.cluster_cells.size()<2)continue; //don't split single pad clusters
      for(icell=iclus->second.cluster_cells.begin();
	   icell!=iclus->second.cluster_cells.end(); ++icell){//loop over cells in parent cluster
   	 int pad2 = (*icell)->get_padnum();
	 //don't re-add local maxima
	 if((iclus->second.local_max.find(pad2))
	       !=(iclus->second.local_max.end())) continue;
	 int sect2 = (*icell)->get_sector();

	 vector<int> neighbouring_maxima;
	 neighbouring_maxima.clear();
	 vector<int>::iterator dc;
	 for(dc=iclus->second.daughterclusters.begin();
	       dc!=iclus->second.daughterclusters.end(); ++dc){
	    int sect1=clusters[*dc].cluster_cells[0]->get_sector(); //cell from the daughter cluster
   	    int pad1=clusters[*dc].cluster_cells[0]->get_padnum();

   	    if(isNeighbour(sect1,pad1,sect2,pad2)){//cells are neighbors
	       neighbouring_maxima.push_back(*dc);
   	    }
	 }
	 if(neighbouring_maxima.size()>0){//cell has local max neighbors
	    vector<int>::iterator im;
	    double sum_max=0.0;
	    for(im=neighbouring_maxima.begin(); im!=neighbouring_maxima.end(); ++im){
	       sum_max+=clusters[*im].cluster_cells[0]->get_charge();
	    }
	    for(im=neighbouring_maxima.begin(); im!=neighbouring_maxima.end(); ++im){
	       double tmp1=clusters[*im].cluster_cells[0]->get_charge();
	       if(sum_max>0.00001)clusters[*im].add_cell(*icell,tmp1/sum_max);
	    }
	 }else{ //end of loop over neighbouring maxima
	    //no neighboring local maxima
	    //so add to the list of unplaced clusters
	    tmp_cells.push_back(*icell);
	 }
      }//end of cell loop
      while(tmp_cells.size()>0){
	 vector<HbdCell*>::iterator lcell;
	 int itmp =0;
   	 for(lcell=tmp_cells.begin(); lcell!=tmp_cells.end(); ++lcell){
	    itmp++;
	    
   	    int sect1 = (*lcell)->get_sector();
   	    int pad1 = (*lcell)->get_padnum();
   	    vector<int>::iterator dc;
   	    vector<int> clusterstojoin;
   	    for(dc=iclus->second.daughterclusters.begin();
   		  dc!=iclus->second.daughterclusters.end(); ++dc){
   	       //iterate over the daughter clusters cells
   	       vector<HbdCell*>::iterator jcell;
   	       for(jcell=clusters[*dc].cluster_cells.begin();
   		     jcell!=clusters[*dc].cluster_cells.end(); ++jcell){
   		  int sect2 = (*jcell)->get_sector();
   		  int pad2 = (*jcell)->get_padnum();
   		  if(isNeighbour(sect1,pad1,sect2,pad2)){
   		     //found a neighbour for this cell
   		     clusterstojoin.push_back(*dc);
		  }
   	       }
   	    }
   	    if(clusterstojoin.size()>0){
   	       vector<int>::iterator ctj;
   	       for(ctj=clusterstojoin.begin(); ctj!=clusterstojoin.end(); ++ctj){
   		  clusters[*ctj].add_cell(*lcell,1.0/clusterstojoin.size());
   	       }
   	       if(tmp_cells.size()>0)tmp_cells.erase(lcell);
	       break;
   	    }//end of adding to clusters
      	 }
      }
   }//end of cluster loop
}

void HbdClusterizer::merge(int cid1, int cid2){
   vector<HbdCell*>::iterator old_cell;
   for(old_cell=clusters[cid2].cluster_cells.begin(); old_cell!=clusters[cid2].cluster_cells.end();
	 ++old_cell){
      clusters[cid1].add_cell(*old_cell);
      (*old_cell)->set_clusterid(cid1);
      
   }
   clusters[cid2].TotChg = 0.0;
   clusters[cid2].cluster_cells.clear();
}
//
//  FillHbdBlob
//   Calculate hit position, etc.., also associate with Geant Hit
//

int HbdClusterizer::fill(HbdBlobList *BlobList)
//int HbdClusterizer::FillHbdBlob(HbdClusUtil **hbdclus,
//                            HbdGhitList *GhitList,
//                            HbdBlobList *BlobList, int NClusters)
{
   //
   // Loop over all the clusters, and calculate position, etc.
   //  also fill HbdBlobList
   //  GEANT information will also be preserved if sim
   // --geant information currently not saved--Anne 12/18/2006
   int nHbdBlobs=0; 
   
   HbdFinalSimSupport *simsupport = new HbdFinalSimSupport(); //included to get the pad coords.
   map<int,HbdClusUtil>::iterator iclus;
   for(iclus=clusters.begin();iclus!=clusters.end();++iclus){
      // Check if the total charge of the cluster exceeds threshold
      if(iclus->second.get_charge()<ClustThreshold)continue;
      if(iclus->second.daughterclusters.size()>0)continue; //don't write out split 
      	//clusters
      /*
      vector<HbdCell*>::iterator cells;
      int high_cell=0;
      for(cells=iclus->second.cluster_cells.begin();
	    cells!=iclus->second.cluster_cells.end(); ++cells){
	 if((*cells)->get_charge()>HighCellThreshold)high_cell=1;
      }
      if(high_cell==0)continue;
*/
      
      Float_t PosY=0.0, PosZ=0.0;
//      Float_t RmsY=0.0, RmsZ=0.0; 
      int sector=-1;
      vector<HbdCell*>::iterator cci;
      for(cci=iclus->second.cluster_cells.begin(); //loop over cells in 
   	    cci!=iclus->second.cluster_cells.end(); ++cci){ //the cluster

   	 //center of gravity in local coords.
	PosY += simsupport->get_pad_center((*cci)->get_padnum(),1) * (*cci)->get_charge();
	PosZ += simsupport->get_pad_center((*cci)->get_padnum(),0) * (*cci)->get_charge();
	 sector = (*cci)->get_sector();
      }//end loop over cells
//      if(GhitList)FillGEANT(GhitList);//fill GEANT for sim.
      
      iclus->second.PosY = PosY/iclus->second.get_charge();
      iclus->second.PosZ = PosZ/iclus->second.get_charge();

      PosY=PosY/iclus->second.get_charge();
      PosZ=PosZ/iclus->second.get_charge();
      /*
      RmsY = RmsY/iclus->second.get_charge() - (iclus->second.PosY * iclus->second.PosY);
      RmsZ = RmsZ/iclus->second.get_charge() - (iclus->second.PosZ * iclus->second.PosZ);
      if(RmsY > Rs3*Rs3/12.) iclus->second.RmsY = sqrt(RmsY);
      else iclus->second.RmsY = Rs3/sqrt(12.);
   
      if(RmsZ > R*R/12.) iclus->second.RmsZ = sqrt(RmsZ);
      else iclus->second.RmsZ = R/sqrt(12.);  //assume uniform charge dist. on pads.*/

	 
      if(verbosity)iclus->second.print();
      BlobList->AddBlob(nHbdBlobs);
      BlobList->set_nBlobs(nHbdBlobs+1);
      
      HbdBlob *d_blob = BlobList->get_blob(nHbdBlobs);
      d_blob->set_charge(iclus->second.get_charge()); // amp
      d_blob->set_sector(sector); //sector
      d_blob->set_size(iclus->second.get_ncells()); // size
      d_blob->set_id(iclus->first); // id
      d_blob->set_bloby_local(iclus->second.PosY);
      d_blob->set_blobz_local(iclus->second.PosZ); //used for evt disp.
      d_blob->set_nlocalmax(iclus->second.local_max.size());
      d_blob->set_parentid(iclus->second.parent_clusterid);
      
      double x_glob,y_glob,z_glob; //global coordinates
      hbdgeo.LocToGlob(PosY, PosZ,x_glob,y_glob,z_glob,sector);

      d_blob->set_blobx(x_glob);
      d_blob->set_bloby(y_glob);
      d_blob->set_blobz(z_glob);
      nHbdBlobs++;

   }//end loop over clusters

   nHbdBlobs = BlobList->get_nBlobs();
   if(verbosity){
      cout << "HbdClusterizer::FillHbdBlob:";

      cout << " Number of blobs stored = " << nHbdBlobs << endl;
   }
   delete simsupport;
   return 0;
}
//
// End of FillHbdBlob
//
int HbdClusterizer::FillGEANT(HbdGhitList*){
   /*
         //
         // From Here, GEANT association part
         //   Find partial amplitudes
         //
         for(int j=0; j<3 && CellContribPartAmp[j][row][col] > 0; j++) {
            int assigned_cell = -1;
            for(int icont = 0; icont < NContrib; icont++) {
               int ihit = CellContribTrack[j][row][col];

               //
               // If track number of this contributor is the same as of any
               // other contributor and it's not noise (-1)
               //
               Int_t TrkIhit = GhitList->get_ghit(ihit)->get_track(); 
               Int_t TrkIcont = GhitList->get_ghit(icont)->get_track(); 
               if ( (ihit != -1 && TrkIhit == TrkIcont) ||
                    (ihit == -1 && ClusterContribTrack[icont] == -1 ) )
                    
                    assigned_cell = icont;
	    }
            //
            // New contributor
            //
            if (assigned_cell == -1) {
               assigned_cell = NContrib;
               NContrib++;
            }
               
            //
            // Assign index and amplitude
            //
            ClusterContribTrack[assigned_cell]  = CellContribTrack[j][row][col];
            ClusterContribAmp[assigned_cell]  += (CellContribPartAmp[j][row][col] * NowCluster->chg[k]);
            j++;
         }
      // From Here, GEANT association part
      for (int j = 0; j < HBDMAXGCONTRIB; j++) {
         float max_amplitude = -1000000.;
         ClusterId[iclus][j] = -2;

         if(j >= NContrib) continue;
         for (int icont = 0; icont < NContrib; icont++){
            int k=0;
            if(max_amplitude < ClusterContribAmp[icont]) {
               max_amplitude = ClusterContribAmp[icont];
               ClusterId[iclus][j] = ClusterContribTrack[icont];
               k = icont;
            }
            ClusterAmplitude[iclus][j] = max_amplitude / NowCluster->TotChg;
            ClusterContribAmp[k] = 0;
         }
      }
	    */
   return 0;
}

bool HbdClusterizer::isNeighbour(int sect1, int pad1, int sect2, int pad2){
   if(sect1!=sect2)return false;
   for(int i=0; i<6; i++){
      if(pad1<96){
	 if(pad_neighbours_0_95[pad1][i]==(pad2+1))
	    return true;
      }else{
	 if(pad_neighbours_96_191[pad1-96][i]==(pad2+1))
	       return true;
	       
      }
   }
   return false;
}
