#include <Fun4AllReturnCodes.h>
#include "mEmcGeaMakeClusterEvaluation.h"
#include <iostream>
#include <cassert>
#include "phool.h"
#include <cmath>

#include "emcNodeHelper.h"

#include "dEmcGeaTrackWrapper.h"
#include "dEmcGeaTowerTrackWrapper.h"
#include "dEmcGeaTrackClusterWrapper.h"
#include "dEmcGeaClusterTrackWrapper.h"

#include "EmcIndexer.h"

#include "emcClusterContainer.h"
#include "emcClusterContent.h"

using namespace std;

//_____________________________________________________________________________
mEmcGeaMakeClusterEvaluation::mEmcGeaMakeClusterEvaluation(): SubsysReco("mEmcGeaMakeClusterEvaluation")
{
}

//_____________________________________________________________________________
mEmcGeaMakeClusterEvaluation::~mEmcGeaMakeClusterEvaluation()
{}

//_____________________________________________________________________________
int
mEmcGeaMakeClusterEvaluation::process_event(PHCompositeNode* topNode)
{
  emcNodeHelper nh;

  dEmcGeaTrackWrapper* dEmcGeaTrack = nh.getTable<dEmcGeaTrackWrapper>
    ("dEmcGeaTrack",topNode);

  if (!dEmcGeaTrack)
    {
      cerr << PHWHERE << " Could not find dEmcGeaTrack" << endl;
      return ABORTRUN;
    }

  dEmcGeaTowerTrackWrapper* dEmcGeaTowerTrack = 
    nh.getTable<dEmcGeaTowerTrackWrapper>("dEmcGeaTowerTrack",topNode);

  if (!dEmcGeaTowerTrack)
    {
      cerr << PHWHERE << " Could not find dEmcGeaTowerTrack" << endl;
      return ABORTRUN;
    }
  
  emcClusterContainer* clusterList = 
    nh.getObject<emcClusterContainer>("emcClusterContainer",topNode);

  if (!clusterList)
    {
      cerr << PHWHERE << " Could not find emcClusterContainer" << endl;
      return ABORTRUN;
    }

  dEmcGeaClusterTrackWrapper* dEmcGeaClusterTrack = 
    nh.getTable<dEmcGeaClusterTrackWrapper>("dEmcGeaClusterTrack",topNode);

  if (!dEmcGeaClusterTrack)
    {
      cerr << PHWHERE << " Could not find dEmcGeaClusterTrack" << endl;
      return ABORTRUN;
    }

  dEmcGeaTrackClusterWrapper* dEmcGeaTrackCluster =
    nh.getTable<dEmcGeaTrackClusterWrapper>("dEmcGeaTrackCluster",topNode);

  if (!dEmcGeaTrackCluster)
    {
      cerr << PHWHERE << " Could not find dEmcGeaTrackCluster" << endl;
      return ABORTRUN;
    }

  if ( dEmcGeaTrack->RowCount() == 0 ||
       dEmcGeaTowerTrack->RowCount() == 0 ||
       clusterList->size() == 0 )
    {
      return EVENT_OK;
    }

  // Global boolean if one contributor is ever found in the cluster
  // needed for the real+simul merging !
  int AtLeastOneContributorFound = 0 ;

  int maxNumOfTracksPerTower = 50;

  // We consider a realistic maximum of *3* tracks contributing to 
  // the same tower.
  // The most common case is, of course, 1 ...
  int maxNumOfTracksKeptPerTower = 3;

  /* ia_track will store the track numbers of the tracks contributing
     to a specific cluster 
  */
  int ia_track[maxNumOfTracksPerTower];

  /* ra_track will store data of tracks listed in ia_track.
     Field: 0 - energy (cumulates "edep" field of TowerTrack)
     1 - pid
     2 - ptot
     3-5 - impact xyz
     6 - twrhit
     7 - edep ("edep" field of GeaTrack)
     8 - anclvl
     9-11 - vertex xyz
  */

  float ra_track[12][maxNumOfTracksPerTower];

  float ra_work[12];

  int i_cltr_id = 0;

  /* First part.  Figure out cluster overlaps, i.e. which track deposited */
  /* energy in a specific cluster */

  /* Loop over all clusters found */

  for ( size_t i = 0; i < clusterList->size(); ++i)
    {
      emcClusterContent* cluster = clusterList->getCluster(i);
      assert(cluster!=0);

      // reset "AtLeastOneContributorFound" for each cluster
      AtLeastOneContributorFound = 0 ;

      /* Zeroing temporary arrays */
      for (ssize_t k = 0; k < 12; k++)
	{
	  for (ssize_t j = 0; j < maxNumOfTracksPerTower; j++)
	    {
	      ra_track[k][j] = 0;
	    }
	}
      for (ssize_t j = 0; j < maxNumOfTracksPerTower; j++)
	{
	  ia_track[j] = 0;
	}

      /* Loop over all towers included in a cluster */

      int ntowers = cluster->multiplicity();
      /* There was a logic flaw in assigning dominant contributor (GEANT-speak)
	 when the clustering routine split up a big cluster into two showers.
	 The measured energies were shared properly between the clusters,
	 but the evaluation never knew about sharing.
	 We circumvent this problem by modifying dEmcGeaTowerTrack on the
	 fly: when dealing with a cluster, we subtract from tower "edep" as much
	 energy as has been "used up" for the first cluster.  In the process we
	 cheat a bit, never allowing tower "edep" to be zero, to avoid other
	 problems with "holes" in the energy deposit...*/
      float e_assigned_this_tower = 0.0;
      float e_new_edep;
      
      for ( int j = 0; j < ntowers; ++j )
        {
	  assert(cluster->partesum(j) > 0);
	  e_assigned_this_tower = (j==0) ? 
	    cluster->partesum(0) : cluster->partesum(j) - cluster->partesum(j-1);
	  int towerid = cluster->towerid(j);

          int i_twrkey = EmcIndexer::SoftwareKey(towerid);

          /* Loop over all tracks contributing to a given tower */

          int l_found = -1;
          size_t k = 0;
          while ( l_found < 0 && k < dEmcGeaTowerTrack->RowCount() )
            {
              if ( i_twrkey == dEmcGeaTowerTrack->get_twrkey(k) )
                {
                  l_found = k;
                  AtLeastOneContributorFound = 1 ;
                }
              ++k;
            } /* while Loop over GeaTowerTrack to find contributor */

          if ( l_found >= 0)  // Contributor found
            {

              // We consider a maximum of *3* tracks contributing 
	      // to the same tower

              for ( int k = 0; k <= maxNumOfTracksKeptPerTower - 1; k++)
                {
                  int i_track = dEmcGeaTowerTrack->get_trkno(k,l_found);
                  if ( i_track > 0)
                    {
                      int l_found1 = -1;
                      int k1 = 0;

                      while ( l_found1 < 0 && k1 < maxNumOfTracksPerTower)
                        {
                          if ( (ia_track[k1] == i_track) || 
			       (ia_track[k1] == 0))
			    {
			      l_found1 = k1;
			    }
                          ++k1;
                        }  /* while Loop over ia_track (already registered?) */

                      /* Let's hope you either found the track or there is
			 still free space in the array */

                      if ( l_found1 >= 0)
                        {
                          ia_track[l_found1] = i_track;
                          ra_track[0][l_found1] = ra_track[0][l_found1] +
			    dEmcGeaTowerTrack->get_edep(k,l_found);
			  /*
			  printf(" Cluster %d tower %d partesum %f e_assigned %f twrkey %d track %d edep %f \n",
				 i,j,cluster->partesum(j),e_assigned_this_tower,i_twrkey,i_track,
				 dEmcGeaTowerTrack->get_edep(k,l_found));
			  */
			  e_new_edep = (dEmcGeaTowerTrack->get_edep(k,l_found) >= e_assigned_this_tower) ?
			    dEmcGeaTowerTrack->get_edep(k,l_found) - e_assigned_this_tower : 0.001;
			  dEmcGeaTowerTrack->set_edep(k,l_found,e_new_edep);
			  
                        }

                    } /* trkno[k] > 0 */

                } /* for Loop over k within a specific GeaTowerTrack record */

            }  /* l_found > 0, found tower */


        } /* for Loop over j, non-zero towers in a specific cluster */

      /*  You are done with all tower contributors of a cluster */

      if ( AtLeastOneContributorFound )
        {
          /* Sort them decreasing */
          for ( int j = 0; j < maxNumOfTracksPerTower - 1; j++)
            {
              /*	  for( k = j + 1; k < maxNumOfTracksPerTower; k++) */
              for ( int k = 1; k < maxNumOfTracksPerTower; k++)
                {
                  if (ra_track[0][k - 1] < ra_track[0][k])
                    {
                      int i_work1 = ia_track[k - 1];
                      for ( int k1 = 0; k1 <= 11; k1++)
			{
			  ra_work[k1] = ra_track[k1][k - 1];
			}
                      for ( int k1 = 0; k1 <= 11; k1++) 
			{
			  ra_track[k1][k - 1] = ra_track[k1][k];
			}
                      for ( int k1 = 0; k1 <= 11; k1++)
			{
			  ra_track[k1][k] = ra_work[k1];
			}
                      ia_track[k - 1] = ia_track[k];
                      ia_track[k] = i_work1;
                    }  /* End interchange of elements */
                }      /* End sort inner loop */
            }          /* End sort outer loop */

          /* Search pid and ptot for the top 3 tracks */

          for ( int j = 0; j <= maxNumOfTracksKeptPerTower - 1; j++)
            {
              int l_found2 = -1;
              size_t k = 0;

              while ( k < dEmcGeaTrack->RowCount() && 
		      l_found2 < 0 && 
		      ia_track[j] > 0)
                {
                  if (dEmcGeaTrack->get_trkno(k) == ia_track[j])
                    {
                      l_found2 = k;
                    }
                  ++k;
                } /* for Loop over k, entries in dEmcGeaTrack */

              if ( l_found2 >= 0)
                {
                  ra_track[1][j] = dEmcGeaTrack->get_pid(l_found2);
                  /*	      ra_track[2][j] = dEmcGeaTrack[l_found2].ekin; */
                  ra_track[2][j] = dEmcGeaTrack->get_ptot(l_found2);
                  ra_track[3][j] = dEmcGeaTrack->get_impxyz(0,l_found2);
                  ra_track[4][j] = dEmcGeaTrack->get_impxyz(1,l_found2);
                  ra_track[5][j] = dEmcGeaTrack->get_impxyz(2,l_found2);
                  ra_track[6][j] = dEmcGeaTrack->get_twrhit(l_found2);
                  ra_track[7][j] = dEmcGeaTrack->get_edep(l_found2);
                  ra_track[8][j] = dEmcGeaTrack->get_anclvl(l_found2);
                  ra_track[9][j] = dEmcGeaTrack->get_xyz(0,l_found2);
                  ra_track[10][j] = dEmcGeaTrack->get_xyz(1,l_found2);
                  ra_track[11][j] = dEmcGeaTrack->get_xyz(2,l_found2);
                }

            }    /* for Loop over j to search for pid at ptot of top 3 tracks */

        }  /* Contributor found */

      /* Write output record for this cluster */

      // In the case of embedded sim+real data, you often DO NOT find GEANT
      // contributors to a reconstructed cluster. At this point we have,
      // thus, 2 options:
      //
      // 1) Do not apply the AtLeastOneContributorFound condition:
      //    Fill dEmcGeaClusterTrack in all cases (though the *pure* dEmcGeaClusterTrack
      //    fields below would be empty in most of the cases ...).
      //    In the analysis afterwards, the way to distinguish a "pure" ClusterTrack
      //    (with some GEANT track in it) and a ClusterLocal without contribution
      //    will be, e.g., asking that the first track not be there (trkno_1 ==0).

      // 2) Apply the AtLeastOneContributorFound condition: Fill only dEmcGeaClusterTrack
      //    when a GEANT contributor is found. The resulting dEmcGeaClusterTrack
      //    tables are much less heavy, but we have to propagate the (real) event
      //    cluster multiplicity information in an adhoc manner ...
      //
      // We choose now option 2) and propagate the clus. mult. info in the
      // unused "Charged" field of dEmcGeaClusterTrack D.d'E. (2002)
      //

      if ( AtLeastOneContributorFound )
        {
          dEmcGeaClusterTrack->set_id(i_cltr_id,i_cltr_id);
          dEmcGeaClusterTrack->set_clusid(i_cltr_id,i);
          dEmcGeaClusterTrack->set_input(i_cltr_id,0);

          // Fields common to emcClusterContainer (hum, more or less, e.g.
	  // evno is not there.
          dEmcGeaClusterTrack->set_evno(i_cltr_id,0); // evno not in clustCont
	  int softkey = EmcIndexer::SoftwareKey(cluster->towerid(0));
          dEmcGeaClusterTrack->set_keycent(i_cltr_id,softkey);
          dEmcGeaClusterTrack->set_type(i_cltr_id,cluster->type());
          dEmcGeaClusterTrack->set_arm(i_cltr_id,cluster->arm());
          dEmcGeaClusterTrack->set_sector(i_cltr_id,cluster->sector());
          dEmcGeaClusterTrack->set_mease(i_cltr_id,cluster->e());
       
	  dEmcGeaClusterTrack->set_ecore(i_cltr_id,cluster->ecore());

          dEmcGeaClusterTrack->set_tof(i_cltr_id,cluster->tof());
          dEmcGeaClusterTrack->set_tofmin(i_cltr_id,cluster->tofmin());
          dEmcGeaClusterTrack->set_tofmax(i_cltr_id,cluster->tofmax());
          // the energy of the first-TOF tower is the energy 
	  // of the highest tower:
          dEmcGeaClusterTrack->set_etof(i_cltr_id,cluster->ecent());
          dEmcGeaClusterTrack->set_etofmin(i_cltr_id,cluster->etofmin());
          dEmcGeaClusterTrack->set_etofmax(i_cltr_id,cluster->etofmax());

          dEmcGeaClusterTrack->set_twrhit(i_cltr_id,cluster->multiplicity());
	  
          for ( int k = 0 ; k < 2 ; ++k)
            {
              dEmcGeaClusterTrack->set_e_sh(k,i_cltr_id,0); 
	    }
	  dEmcGeaClusterTrack->set_disp(1,i_cltr_id,cluster->dispy());
	  dEmcGeaClusterTrack->set_disp(0,i_cltr_id,cluster->dispz());
	  dEmcGeaClusterTrack->set_padisp(1,i_cltr_id,cluster->padispy());
	  dEmcGeaClusterTrack->set_padisp(0,i_cltr_id,cluster->padispz());
	  //	  dEmcGeaClusterTrack->set_measxyz(0,i_cltr_id,cluster->z());
	  //	  dEmcGeaClusterTrack->set_measxyz(1,i_cltr_id,cluster->y());
	  dEmcGeaClusterTrack->set_measxyz(0,i_cltr_id,cluster->x());
	  dEmcGeaClusterTrack->set_measxyz(1,i_cltr_id,cluster->y());
	  dEmcGeaClusterTrack->set_measxyz(2,i_cltr_id,cluster->z());

          for ( int k = 0; k < 8; ++k)
            {
	      if ( k < cluster->multiplicity() )
		{
		  dEmcGeaClusterTrack->set_partesum(k,i_cltr_id,
						    cluster->partesum(k));
		}
	      else
		{
		  dEmcGeaClusterTrack->set_partesum(k,i_cltr_id,0);
		}
            }

          // Fields common to emcClusterContainer but somehow different ...
          dEmcGeaClusterTrack->set_chi2_sh(i_cltr_id,cluster->chi2());
          dEmcGeaClusterTrack->set_prob_photon_sh(i_cltr_id,cluster->prob_photon());

          // "Pure" dEmcGeaClusterTrack fields

          for ( int k = 0 ; k < maxNumOfTracksKeptPerTower  ; ++k )
            {
              dEmcGeaClusterTrack->set_trkno(k,i_cltr_id,ia_track[k]);
              dEmcGeaClusterTrack->set_edep(k,i_cltr_id,ra_track[0][k]);
              dEmcGeaClusterTrack->set_pid(k,i_cltr_id,ra_track[1][k]);
              dEmcGeaClusterTrack->set_ptot(k,i_cltr_id,ra_track[2][k]);
              dEmcGeaClusterTrack->set_tracktwrhit(k,i_cltr_id,
						   static_cast<int>(rint(ra_track[6][k])));
              dEmcGeaClusterTrack->set_edep_nom(k,i_cltr_id,ra_track[7][k]);
              dEmcGeaClusterTrack->set_ancestry(k,i_cltr_id,ra_track[8][k]);
              for ( int j = 0; j <= 2; ++j )
                {
                  dEmcGeaClusterTrack->set_xyz(j,k,i_cltr_id,
					       ra_track[3 + j][k]);
                  dEmcGeaClusterTrack->set_vertex(j,k,i_cltr_id,
						  ra_track[9 + j][k]);
                }
            }

          if (dEmcGeaClusterTrack->get_mease(i_cltr_id) > 0.0)
            {
              for ( int k = 0; k < maxNumOfTracksKeptPerTower  ; ++k )
                {
		  float frac =  dEmcGeaClusterTrack->get_edep(k,i_cltr_id) /
		    dEmcGeaClusterTrack->get_mease(i_cltr_id);
                  dEmcGeaClusterTrack->set_efrac(k,i_cltr_id,frac);
                }
            }
          // "Pure" dEmcGeaClusterTrack fields not used

          // WATCH-OUT: we (ab)use of this field to store the real evt. cluster info
          // dEmcGeaClusterTrack[i_cltr_id].charged = 0;
	  // FIXME : what should it be really ? + have a field in
	  // the yet-to-come phenix-standard replacement for the STAF table
	  // dEmcGeaClusterTrack
          dEmcGeaClusterTrack->set_charged(i_cltr_id,clusterList->size());

          for ( int k = 0; k < 8; ++k )
            {
              dEmcGeaClusterTrack->set_chglist(k,i_cltr_id,0);
            }

          // WATCH-OUT: we (ab)use of this field to store the dead/warn map info
          //for( k=0; k<3; k++)
          //  {
          //   dEmcGeaClusterTrack[i_cltr_id].pc3proj[k] = 0.;
          //  }

	  // FIXME : have the corresponding fields in
	  // the yet-to-come phenix-standard replacement for the STAF table
	  // dEmcGeaClusterTrack
	  // WARNING : (21+9 bits int stored in floats)
          dEmcGeaClusterTrack->set_pc3proj(0,i_cltr_id,cluster->deadmap());
          dEmcGeaClusterTrack->set_pc3proj(1,i_cltr_id,cluster->warnmap());
          dEmcGeaClusterTrack->set_pc3proj(2,i_cltr_id,0);

          /* Increment STAF row counter */
          ++i_cltr_id;

        } /* End of  if( AtLeastOneContributorFound ) , i.e. you found at least one GEANT
      	     contributor to this cluster */

    }    /* Loop over i, all clusters recognized in dEmcClusterLocalExt */

  dEmcGeaClusterTrack->SetRowCount(i_cltr_id);

  /* -------------------------------------------------------------   */

  /* Second part.  Loop over all tracks that deposited anything in the
     calorimeter.  Point to clusters where a specific track contributed */

  int i_trcl_id = 0;

  for ( size_t i = 0; i < dEmcGeaTrack->RowCount(); ++i ) 
    {

      int l_found = -1;   /* Found at least one cluster where it contributed */
      AtLeastOneContributorFound = 0 ;

      // Zeroing arrays
      for (ssize_t k = 0; k < 12; k++)
	{
	  for (ssize_t j = 0; j < maxNumOfTracksPerTower; j++)
	    {
	      ra_track[k][j] = 0;
	    }
	}
      
      /* Since 0 is a valid pointer now, "not found" is indicated
	 by -1 */
      for ( int j = 0; j < maxNumOfTracksKeptPerTower ; j++)
	{
	  ia_track[j] = -1;
	}
      int i_track = dEmcGeaTrack->get_trkno(i);
      float nom_edep = dEmcGeaTrack->get_edep(i);

      for ( size_t j = 0; j < dEmcGeaClusterTrack->RowCount(); ++j ) 
        {
          for ( int k = 0; k < maxNumOfTracksKeptPerTower  ; ++k )
            {
              if ( dEmcGeaClusterTrack->get_trkno(k,j) == i_track
                   && l_found < maxNumOfTracksPerTower - 1 )
                {
                  ++l_found;
                  AtLeastOneContributorFound = 1 ;
                  ia_track[l_found] = dEmcGeaClusterTrack->get_clusid(j);
                  ra_track[0][l_found] = dEmcGeaClusterTrack->get_pid(k,j);
                  ra_track[1][l_found] = dEmcGeaClusterTrack->get_ptot(k,j);
                  ra_track[2][l_found] = dEmcGeaClusterTrack->get_edep(k,j);
                  /*
		    ra_track[3][l_found] = dEmcGeaClusterTrack[j].efrac[k];
                  */
                  if (nom_edep > 0.1)
                    {
                      ra_track[3][l_found] =
                        dEmcGeaClusterTrack->get_edep(k,j) / nom_edep;
                    }
                  else
                    {
                      ra_track[3][l_found] = 0.0;
                    }
                }  /* New cluster found where it contributed */

            }  /* Loop over k = 0, 2, within one dEmcGeaClusterTrack record */

        }   /* Loop over j = 0,dEmcGeaClusterTrack_h.nok   */

      /* Sort descending the clusters where the track contributed */

      if ( AtLeastOneContributorFound )
        {
          for (int j = 0; j < l_found; j++)
            {
              for ( int k = 1; k <= l_found; k++)
                {
                  if (ra_track[3][k - 1] < ra_track[3][k])
                    {
                      int i_work1 = ia_track[k - 1];
                      for ( int k1 = 0; k1 <= 11; k1++)
			{
			  ra_work[k1] = ra_track[k1][k - 1];
			}
                      for ( int k1 = 0; k1 <= 11; k1++)
			{
			  ra_track[k1][k - 1] = ra_track[k1][k];
			}
                      for ( int k1 = 0; k1 <= 11; k1++)
			{
			  ra_track[k1][k] = ra_work[k1];
			}
                      ia_track[k - 1] = ia_track[k];
                      ia_track[k] = i_work1;
                    }
                }  /* End loop over k, inner sort loop */
            }      /* End loop over j, outer sort loop */

          /* Write out a new record of dEmcGeaTrackCluster */

          dEmcGeaTrackCluster->set_id(i_trcl_id,i_trcl_id + 1);
          dEmcGeaTrackCluster->set_trkno(i_trcl_id,i_track);
          dEmcGeaTrackCluster->set_track_ptr(i_trcl_id,i);
          dEmcGeaTrackCluster->set_nom_edep(i_trcl_id,nom_edep);
          dEmcGeaTrackCluster->set_input(i_trcl_id,0);

          for ( int k = 0; k < maxNumOfTracksKeptPerTower ; k++)
            {
              dEmcGeaTrackCluster->set_clusid(k,i_trcl_id,ia_track[k]);
              dEmcGeaTrackCluster->set_pid(i_trcl_id,ra_track[0][0]);
              dEmcGeaTrackCluster->set_ptot(i_trcl_id,ra_track[1][0]);
              dEmcGeaTrackCluster->set_edep(k,i_trcl_id,ra_track[2][k]);
              dEmcGeaTrackCluster->set_efrac(k,i_trcl_id,ra_track[3][k]);
            }
	  
          ++i_trcl_id;
	}   /* End if(l_found) - found at least one cluster where contrib. */
      
    }   /*  Loop over i = 0,dEmcGeaTrack_h.nok , all tracks depositing E */
  
  
  dEmcGeaTrackCluster->SetRowCount(i_trcl_id);

  return EVENT_OK;
}
