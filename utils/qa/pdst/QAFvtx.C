/*
 * QAFvtx.C
 * $Id: QAFvtx.C,v 1.7 2015/06/24 17:19:12 pinkenbu Exp $
 *
 * History:
 * 10-23-2012: First version. Cesar
*/

#include "QADefs.h"

#include <Fun4AllHistoManager.h>
#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>

// MUTOO includes
#include <TMutTrkMap.h>
#include <TMuiRoadMapO.h>
#include <TMutVtxMap.h>

// FVTXOO
#include <TFvtxTrkMap.h>
#include <TFvtxClusMap.h>
#include <TFvtxCoordMap.h>
#include <TFvtxHitMap.h>
#include <TFvtxMCHitMap.h>
#include <TFvtxTrk.h>
#include <TFvtxClus.h>
#include <TFvtxCoord.h>
#include <TFvtxHit.h>
#include <FvtxGeom.h>

#include <MUTOO.h>

#include <TriggerHelper.h>
#include <RunHeader.h>
#include <getClass.h>
#include <BbcOut.h>
#include <getClass.h>

#include <QAFvtx.h>

using namespace std;

//___________________________________________________________________________________________________________
int QAFvtx::InitRun(PHCompositeNode* top_node)
{
  Fun4AllServer *se = Fun4AllServer::instance();

  try
    {
      TMutNode<TFvtxHitMap>::find_node(top_node, "TFvtxHitMap");
      TMutNode<TFvtxCoordMap>::find_node(top_node, "TFvtxCoordMap");
      TMutNode<TFvtxTrkMap>::find_node(top_node, "TFvtxTrkMap");
    }
  catch ( exception& e)
    {
      se->unregisterSubsystem(this);
      return 0;
    }

  Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
  if (!hm)
    {
      hm = new Fun4AllHistoManager(HistoManagerName);
      se->registerHistoManager(hm);
    }

  // init histograms, use maximum limits
  for (int arm = 0; arm < FVTXOO::MAX_ARM; arm++)
    {
    for (int cage = 0; cage < FVTXOO::MAX_CAGE; cage++)
      {
	FVTX_hit[arm][cage] = new TH3F(Form("FVTX_hit_%d_%d",arm,cage),
				       Form("FVTX_hit arm=%d cage=%d; strip; sector*2+column; station",arm,cage),
				       FVTXOO::MAX_STRIP, -0.5, FVTXOO::MAX_STRIP-0.5,
				       FVTXOO::MAX_SECTOR*2, -0.5, FVTXOO::MAX_SECTOR*2-0.5,
				       FVTXOO::MAX_STATION, -0.5, FVTXOO::MAX_STATION-0.5);
	hm->registerHisto(FVTX_hit[arm][cage]);
	
	FVTX_hit_q[arm][cage] = new TH3F(Form("FVTX_hit_q_%d_%d",arm,cage),
					 Form("FVTX_hit charge arm=%d cage=%d; strip; sector*2+column; station",arm,cage),
				       FVTXOO::MAX_STRIP, -0.5, FVTXOO::MAX_STRIP-0.5,
				       FVTXOO::MAX_SECTOR*2, -0.5, FVTXOO::MAX_SECTOR*2-0.5,
				       FVTXOO::MAX_STATION, -0.5, FVTXOO::MAX_STATION-0.5);
	hm->registerHisto(FVTX_hit_q[arm][cage]);

	FVTX_coord[arm][cage] = new TH3F(Form("FVTX_coord_%d_%d",arm,cage),
					 Form("FVTX_coordinate arm=%d cage=%d; strip; sector*2+column; station",arm,cage),
					 FVTXOO::MAX_STRIP, -0.5, FVTXOO::MAX_STRIP-0.5,
					 FVTXOO::MAX_SECTOR*2, -0.5, FVTXOO::MAX_SECTOR*2-0.5,
					 FVTXOO::MAX_STATION, -0.5, FVTXOO::MAX_STATION-0.5);
	hm->registerHisto(FVTX_coord[arm][cage]);
	
	FVTX_coord_size[arm][cage] = new TH3F(Form("FVTX_coord_size_%d_%d",arm,cage),
					      Form("FVTX_coordinate size arm=%d cage=%d; number of strips; sector*2+column; station",arm,cage),
					      15, 0.5, 15.5,
					      FVTXOO::MAX_SECTOR*2, -0.5, FVTXOO::MAX_SECTOR*2-0.5,
					      FVTXOO::MAX_STATION, -0.5, FVTXOO::MAX_STATION-0.5);
	hm->registerHisto(FVTX_coord_size[arm][cage]);

	FVTX_coord_q[arm][cage] = new TH3F(Form("FVTX_coord_q_%d_%d",arm,cage),
					   Form("FVTX_coordinate total charge arm=%d cage=%d; total charge; sector*2+column; station",arm,cage),
					      50, 0.5, 50.5,
					      FVTXOO::MAX_SECTOR*2, -0.5, FVTXOO::MAX_SECTOR*2-0.5,
					      FVTXOO::MAX_STATION, -0.5, FVTXOO::MAX_STATION-0.5);
	hm->registerHisto(FVTX_coord_q[arm][cage]);

	FVTX_coord_xy[arm][cage] = new TH3F(Form("FVTX_coord_xy_%d_%d",arm,cage),
					    Form("FVTX_coordinate xy arm=%d cage=%d; X [cm]; Y [cm]; station",arm,cage),
					    100, -18, 18,
					    100, -18, 18,
					    FVTXOO::MAX_STATION, -0.5, FVTXOO::MAX_STATION-0.5);
	hm->registerHisto(FVTX_coord_xy[arm][cage]);

	FVTX_coord_dw[arm][cage]    = new TH3F(Form("FVTX_coord_dw_%d_%d",arm,cage),
					 Form("FVTX coordinate residual arm=%d cage=%d; residual [cm]; strip; sector",arm,cage),
					 100, -0.2, 0.2,
					 FVTXOO::MAX_SECTOR, -0.5, FVTXOO::MAX_SECTOR-0.5,
					 FVTXOO::MAX_STATION, -0.5, FVTXOO::MAX_STATION-0.5);
	hm->registerHisto(FVTX_coord_dw[arm][cage]);
	
	FVTX_trk_nhits_chi2[arm][cage] = new TH3F(Form("FVTX_trk_nhits_chi2_%d_%d",arm,cage),
					    Form("FVTX track nhits vs chi2/ndf arm=%d cage=%d; nhits; chi2/ndf; sector",arm,cage),
					    8, 0.5, 8.5,
					    100, 0, 30,
					    FVTXOO::MAX_SECTOR, -0.5, FVTXOO::MAX_SECTOR-0.5);
	hm->registerHisto(FVTX_trk_nhits_chi2[arm][cage]);

      }
    FVTX_trk_phi_theta[arm] = new TH2F(Form("FVTX_trk_phi_theta_%d",arm),
				       Form("FVTX track theta vs phi arm=%d; phi; theta",arm),
				       100, -M_PI, M_PI,
				       100, 0, 0.7);
    hm->registerHisto(FVTX_trk_phi_theta[arm]);
      }

  return 0;
}

//___________________________________________________________________________________________________________
int QAFvtx::process_event(PHCompositeNode *top_node)
{
  _counter++;
  
  // fill hits related histograms
  fill_fvtx_hits_hists( top_node );

  // fill coordinates related histograms
  fill_fvtx_coord_hists( top_node );

  // fill tracks related histograms
  fill_fvtx_tracks_hists( top_node );

  return 0;
}

void QAFvtx::fill_fvtx_hits_hists(PHCompositeNode *top_node)
{
  TFvtxHitMap* _hit_map = TMutNode<TFvtxHitMap>::find_node(top_node, "TFvtxHitMap" );
  if (!_hit_map)
    {
      cout << PHWHERE << "node _hit_map not found" << endl;
      return;
    }

  for (int iarm = 0; iarm < FVTXOO::MAX_ARM ; iarm++)
    for (int icage = 0; icage < FVTXOO::MAX_CAGE ; icage++)
      for (int isector = 0; isector < FVTXOO::MAX_SECTOR ; isector++)
	for (int istation = 0; istation < FVTXOO::MAX_STATION; istation++)
	  {
	    TFvtxHitMap::iterator hit_iter = _hit_map->get(iarm, icage, istation, isector);
	    while (TFvtxHitMap::pointer hit_ptr = hit_iter.next())
	      {
		int column = isector*2 + hit_ptr->get()->get_column();
		FVTX_hit[iarm][icage]->Fill(hit_ptr->get()->get_strip(),
					    column,
					    istation);
		FVTX_hit_q[iarm][icage]->Fill(hit_ptr->get()->get_strip(),
					      column,
					      istation,
					      hit_ptr->get()->get_q());
	      }
	  }

}

void QAFvtx::fill_fvtx_coord_hists(PHCompositeNode *top_node)
{
  TFvtxCoordMap* _coord_map = TMutNode<TFvtxCoordMap>::find_node(top_node, "TFvtxCoordMap" );
  if (!_coord_map)
    {
      cout << PHWHERE << "node _coord_map not found" << endl;
      return;
    }

  for (int iarm = 0; iarm < FVTXOO::MAX_ARM ; iarm++)
    for (int icage = 0; icage < FVTXOO::MAX_CAGE ; icage++)
      for (int isector = 0; isector < FVTXOO::MAX_SECTOR ; isector++)
	for (int istation = 0; istation < FVTXOO::MAX_STATION; istation++)
	  {
	    TFvtxCoordMap::iterator coord_iter = _coord_map->get(iarm, icage, istation, isector);
	    while (TFvtxCoordMap::pointer coord_ptr = coord_iter.next())
	      {
		int column = isector*2 + coord_ptr->get()->get_column();
		int strip = coord_ptr->get()->get_peak_strip();
		FVTX_coord[iarm][icage]->Fill(strip, column, istation);

		TFvtxClusMap::key_iterator clus_iter = coord_ptr->get()->get_associated<TFvtxClus>();
		if (clus_iter.count()>0)
		  {
		    TFvtxHitMap::key_iterator hit_iter = clus_iter.current()->get()->get_associated<TFvtxHit>();
		    int coord_size = hit_iter.count();
		    FVTX_coord_size[iarm][icage]->Fill(coord_size, column, istation);
		  }

		double qtot = coord_ptr->get()->get_q_tot();
		FVTX_coord_q[iarm][icage]->Fill(qtot, isector, istation);

		PHPoint CoordMidPoint = coord_ptr->get()->get_coord_midpoint();
		FVTX_coord_xy[iarm][icage]->Fill(CoordMidPoint.getX(), CoordMidPoint.getY(), istation);	   
	      }
	  }
}

void QAFvtx::fill_fvtx_tracks_hists(PHCompositeNode *top_node)
{
  TFvtxTrkMap* _trk_map = TMutNode<TFvtxTrkMap>::find_node(top_node, "TFvtxTrkMap" );
  if (!_trk_map)
    {
      cout << PHWHERE << "node _trk_map not found" << endl;
      return;
    }

  TFvtxTrkMap::iterator trk_iter = _trk_map->range();
  while(TFvtxTrkMap::pointer trk_ptr = trk_iter.next())
    {
      try
        {
          if ( trk_ptr->get()->get_ghost() ) continue;
          int iarm = trk_ptr->get()->get_arm();
          TFvtxCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();
          int nhits = coord_iter.count(); 
          if ( nhits == 0 ) continue;
          TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next();
          int icage = coord_ptr->get()->get_cage();
          int isector = coord_ptr->get()->get_sector();
          
          double chi2_ndf = trk_ptr->get()->get_w_chi_square_pdf();
          FVTX_trk_nhits_chi2[iarm][icage]->Fill(nhits, chi2_ndf, isector);
          
          double fvtx_x = trk_ptr->get()->get_trk_par_vtx()->get_px();
          double fvtx_y = trk_ptr->get()->get_trk_par_vtx()->get_py();
          double fvtx_z = trk_ptr->get()->get_trk_par_vtx()->get_pz();
          double norm = sqrt(fvtx_x*fvtx_x + fvtx_y*fvtx_y + fvtx_z*fvtx_z);
          double fvtx_phi = atan2(fvtx_y, fvtx_x);
          double fvtx_theta = atan(sqrt(fvtx_x*fvtx_x + fvtx_y*fvtx_y)/norm);
          FVTX_trk_phi_theta[iarm]->Fill(fvtx_phi, fvtx_theta);
          
          fill_fvtx_residuals_hist(trk_ptr->get());
        }
      catch(...)
        {
        }
    }

}


//________________________________________________________________________
void QAFvtx::fill_fvtx_residuals_hist(TFvtxTrk* trk)
{
  float x[4], y[4];

  std::fill(x,x+sizeof(x)/sizeof(float),-999.9);
  std::fill(y,y+sizeof(y)/sizeof(float),-999.9);

  const vector<TMutTrkPar>& trk_par_list( *trk->get_trk_par_list() );
  for( vector<TMutTrkPar>::const_iterator iter = trk_par_list.begin(); iter != trk_par_list.end(); iter++ )
    {
      if (fabs(iter->get_z()) > 18.0 && fabs(iter->get_z()) < 21.5)
	{
	  x[0] = iter->get_x();
	  y[0] = iter->get_y();
      }
      if (fabs(iter->get_z()) > 23.0 && fabs(iter->get_z()) < 28)
	{
	  x[1] = iter->get_x();
	  y[1] = iter->get_y();
	}
      if (fabs(iter->get_z()) > 28.0 && fabs(iter->get_z()) < 35.0)
	{
	  x[2] = iter->get_x();
	  y[2] = iter->get_y();
	}
      if (fabs(iter->get_z()) > 35.0 && fabs(iter->get_z()) < 41.0)
	{
	  x[3] = iter->get_x();
	  y[3] = iter->get_y();
	}
    }

  TFvtxCoordMap::key_iterator coord_iter = trk->get_associated<TFvtxCoord>();
  while ( TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() )
    {
      int iarm  = coord_ptr->get()->get_arm();
      int icage = coord_ptr->get()->get_cage();
      int istation = coord_ptr->get()->get_station();
      int isector = coord_ptr->get()->get_sector();
      
      PHPoint CoordMidPoint = coord_ptr->get()->get_coord_midpoint();
      float dr = sqrt(pow(CoordMidPoint.getX() - x[istation],2) + pow(CoordMidPoint.getY() - y[istation],2));

      FVTX_coord_dw[iarm][icage]->Fill(dr, isector, istation);
    }
}

//________________________________________________________________________
int QAFvtx::End(PHCompositeNode *topNode)
{
  if( _counter <= 0 )
  {
    cerr 
      << "QAFvtx::End -  exiting; invalid counter value: "
      << _counter << endl;
    return 0;
  }
 
  return 0;
}
