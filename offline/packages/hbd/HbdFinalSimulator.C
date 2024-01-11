//******************************************************************************
//
// Class: HbdFinalSimulator.h implementation
//
// Purpose: Slow Simulation code for the PHENIX
//          Hadron Blind Detector upgrade.
//
//     !!! Now, this is response code only !!!
//
// Authors:
//
// Ilia Ravinovich (WIS) - Ilia.Ravinovich@weizmann.ac.il
// Maxim Naglis (WIS)   - Maxim.Naglis@weizmann.ac.il
//
// Revisions: 02/15/04, v1.0, IR
// Revisions: 08/15/04, v2.0, IR
// Revisions: 09/05/04, v3.0, IR + MN
// Revisions: 04/05/06, v4.0, Takao Sakaguchi (divide simulation and reconstruction part)
// Revisions: 11/02/06, v5.0, IR + MN
// Revisions: 08/20/10, v6.0, IR
//
// Please contact the authors before committing any changes
// to this file to the PHENIX repository.
//
//******************************************************************************
#include "phool.h"

#include "HbdFinalSimulator.h"
#include "HbdGhitv1.h"
#include "HbdGhitListv1.h"
#include "HbdCellv1.h"
#include "HbdCellListv1.h"
#include "hbdghitWrapper.h"
#include "fkinWrapper.h"
#include "primaryWrapper.h"
#include "HbdPISAHit.h"
#include "PISARun.h"

#include "PHIODataNode.h"
#include "TH1F.h"
#include "Fun4AllReturnCodes.h"
using namespace std;

//
// Helpers for scanning Node Tree...
//
typedef PHIODataNode <PHObject>   PHObjectNode_t;
typedef PHIODataNode<fkinWrapper> fkinNode_t;
typedef PHIODataNode<primaryWrapper> primaryNode_t;
typedef PHIODataNode<hbdghitWrapper> hbdghitNode_t;
typedef PHIODataNode <HbdGhitList>    HbdGhitListNode_t;
typedef PHIODataNode <HbdCellList> HbdCellListNode_t;

#include "RunHeader.h"
typedef PHIODataNode<RunHeader> RunHeaderNode_t;

//_______________________________________________________
HbdFinalSimulator::HbdFinalSimulator():
  Verbose(0)
{
  ThisName = "HBD Slow Simulator Code";
  cout << "HbdFinalSimulator-I::Constructor executed..." <<endl;
}

//_______________________________________________________________
int HbdFinalSimulator::Init(PHCompositeNode *topNode)
{
  PISAeventSeq=0;
  hbdgeo.fetch(215034);
  hbdgeo.fetchPad(215034);
  sgn1 = new TH1F("sgn1","",600,0.,300.);
  
  cout << "HbdFinalSimulator::Init-I: Execution started." << endl;
    
  //
  // Set up the node tree
  //
  int i = CreateNodeTree(topNode);
  
  cout << "Error code from CreateNodeTree is " << i << endl;
  cout << "HbdFinalSimulator::Init-I: Node tree created." << endl;
  
  //
  // Shaper function: fast rise, slow drop, average width ~40ns, peak response: 1
  //
  shaper_f = new TH1F("shaper_f","Shaper profile",300,-0.5,299.5);
  
  //
  // Leading edge discriminator delay in ns
  //
  int   delay = 35;
  float time;
  
  for (i = 0; i < 290; i++)
    {
      time = i;
      shaper[i] = time * time * exp(-time / 35.) * .00205;
      if (i > delay)
	{
	  time = i - delay;
	  shaper[i] -=  time * time * exp(-time / 35.) * .00205;
	}
      shaper_f -> SetBinContent(i+1,shaper[i]);
    }
  
  cout << "HbdFinalSimulator::Init-I: Shaper function filled ..." << endl;
  
  return EVENT_OK;
  
}

//
// Run-dependent initialization
//
int HbdFinalSimulator::InitRun(PHCompositeNode *topNode)
{
  
  return EVENT_OK;
  
}

int HbdFinalSimulator::Reset(PHCompositeNode *topNode)
{
  
  if(Verbose>0) cout << "HbdFinalSimulator::Reset Resetting the HBD " <<endl;
  
  return EVENT_OK;
  
}

//
// Analyze an event
//
int HbdFinalSimulator::process_event(PHCompositeNode *topNode)
{
  
  PHNodeIterator iter(topNode);
  
  int iError;
  
  if(Verbose>0) cout << "HbdFinalSimulator::process_event-I: Execution started ..." <<endl;
  
  //
  // Initialization
  //
  nHbdGhits = 0;
  nHbdCells = 0;

  if(Verbose>0) cout << "HbdFinalSimulator::process_event-I: Getting nodes ..." <<endl;
  GetNodes(topNode);
  
  //
  // Get hbdghit tables
  //
  hbdghitNode_t* tgN = static_cast<hbdghitNode_t*>(iter.findFirst("PHIODataNode", "hbdghit"));
  if (!tgN)
    {
      cerr << "HbdFinalSimulator -> ERROR: hbdghit table not found !!!" << endl;
      return False;
    }
  hbdghitWrapper* hbdghit = tgN->getData();
  
  PISAnHits = (int) hbdghit->RowCount();
  if(Verbose>0) cout << "HbdFinalSimulator::process-event: Grabbed " << PISAnHits << " GEANT HBD hits" << endl;
  
  PISAeventSeq++;
  
  if(Verbose>0) cout << "HbdFinalSimulator::process_event-I: filling GEANT hits" <<endl;
  
  //
  // Fill the GEANT hit objects
  //
  iError = fillGhitList(topNode);
  if(iError!=0) return False;
  
  if(Verbose>0) cout << "HbdFinalSimulator::process_event-I: Pulse Simulation and filling HbdCell" <<endl;
  
  //
  // Copy GEANT hits into blobs
  //
  iError = PulseSimAndfillCell();
  if(iError!=0) return False;
  
  if(Verbose>0) cout << "HbdFinalSimulator::process_event-I: Event processed." <<endl;
  
  return EVENT_OK;
  
}

//
// Create the data
//
int HbdFinalSimulator::CreateNodeTree(PHCompositeNode *topNode)
{
  
  cout << "HbdFinalSimulator::CreateNodeTree-I: Execution started" << endl;
  
  PHNodeIterator iter(topNode);
  
  PHCompositeNode* hbdNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!hbdNode)
    {
      cerr << "HbdFinalSimulator ERROR: DST node does not exist." << endl;
      return False;
    }
  else
    {
      if(Verbose>0) cerr << "HbdFinalSimulator: hbdNode FOUND." << endl;
    }
  
  //
  // Add new HBD nodes
  //
  PHIODataNode<PHObject>* HbdGhitListNode = NULL;
  HbdGhitListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "HbdGhitList");
  if (!HbdGhitListNode)
    {
      HbdGhitList* hbdghits = new HbdGhitListv1();
      HbdGhitListNode =
	new PHIODataNode<PHObject>(hbdghits, "HbdGhitList", "PHObject");
      hbdNode->addNode(HbdGhitListNode);
    }

  PHIODataNode<PHObject>* HbdCellListNode = NULL;
  HbdCellListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "HbdCellList");
  if (!HbdCellListNode)
    {
      HbdCellList* hbdcells = new HbdCellListv1();
      HbdCellListNode =
	new PHIODataNode<PHObject>(hbdcells, "HbdCellList", "PHObject");
      hbdNode->addNode(HbdCellListNode);
    }

  cout << "HbdFinalSimulator::CreateNodeTree-I: Execution completed" << endl;
  
  return EVENT_OK;
  
}

//
// Grab the data
//
void HbdFinalSimulator::GetNodes(PHCompositeNode *topNode)
{
  
  if(Verbose>0) cout << "HbdFinalSimulator::GetNodes-I: Execution started" << endl;
  
  //
  // Set all pointers to null ...
  //
  d_ghit    = 0;
  d_cell = 0;

  //
  // Search out the nodes from the node tree...
  //
  PHTypedNodeIterator<HbdGhitList> iGHIT(topNode);
  HbdGhitListNode_t *GHIT = iGHIT.find("HbdGhitList");
  if (GHIT) d_ghit = GHIT->getData();
  if (!d_ghit) cerr << PHWHERE << "HbdFinalSimulator:: ghit data not in Node Tree" << endl;

  PHTypedNodeIterator<HbdCellList> iCELL(topNode);
  HbdCellListNode_t *CELL = iCELL.find("HbdCellList");
  if (CELL) d_cell = CELL->getData();
  if (!d_cell) cerr << PHWHERE << "HbdFinalSimulator:: cell data not in Node Tree" << endl;

  if(Verbose>0) cout << "HbdFinalSimulator::GetNodes-I: Execution completed" << endl;
  
  return;
  
}

int HbdFinalSimulator::End(PHCompositeNode *topNode)
{
  
  return EVENT_OK;
  
}

//
// Copy GEANT hit from the hbdghit (GEA) file into HbdGhitist
//
int HbdFinalSimulator::fillGhitList(PHCompositeNode *topNode)
{
  
  int detFlag,partid,khit;
  float xin, yin, zin, ptof;
  short track, id_part;
  int true_track;
  int nfile;
  int error;
  float ptheta;
  float pphi;
  float r_vertex;
  float z_vertex;
  float theta_vertex;
  float phi_vertex;
  float ptot;
  int itparent;
  int idparent;
  int idpart;
  int itorigin;
  int idorigin;
  
  if(Verbose>0) cout << "HbdFinalSimulator::fillGhitList-I: Execution started." << endl;

  PHNodeIterator iter(topNode);
//  primaryWrapper* d_primary;
//  fkinWrapper* d_fkin;
//  hbdghitWrapper* d_hbdghit;
  
  //
  // Set all pointers to null...
  //
//  d_primary    = 0;
//  d_fkin = 0;
//  d_hbdghit = 0;
  
  primaryNode_t* fpM = static_cast<primaryNode_t*>(iter.findFirst("PHIODataNode", "primary"));
  if (!fpM)
    {
      cerr << "mHbdFinalSimulator -> ERROR: primary table not found." << endl;
      return False;
    }
//  d_primary = fpM->getData();
  
  fkinNode_t* fkN = static_cast<fkinNode_t*>(iter.findFirst("PHIODataNode", "fkin"));
  if (!fkN)
    {
      cerr << "mHbdFinalSimulator -> ERROR: fkin table not found." << endl;
      return False;
    }
//  d_fkin = fkN->getData();
  
  hbdghitNode_t* tgN = static_cast<hbdghitNode_t*>(iter.findFirst("PHIODataNode", "hbdghit"));
  if (!tgN)
    {
      cerr << "mHbdFinalSimulator -> ERROR: hbdghit table not found." << endl;
      return False;
    }
//  d_hbdghit = tgN->getData();
  
  if(Verbose>0) cout << "HbdFinalSimulator::fillGhitList-I: Processing " << PISAnHits
       << " hits." << endl;
  
  //
  // Prescanning event
  //
  if(Verbose>0) cout << "HbdFinalSimulator::fillGhitList-I: Prescanning event: " << PISAeventSeq << endl;
  
  HbdPISAHit *hbdhitTmp = HbdPISAHit::GetHbdHitEvt();
  int particle_flag = 0;
  for (khit=0; khit<PISAnHits; khit++)
    {

      true_track = hbdhitTmp[khit].GetMctrack();
      if(true_track < 1) continue;
      dio_ptrkorigin(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		     &r_vertex, &z_vertex, &theta_vertex, &phi_vertex,
		     &itorigin, &idorigin, &idpart);

      dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		    &r_vertex, &z_vertex, &theta_vertex, &phi_vertex,
		    &itparent, &idparent, &idpart);

      //
      // Consider if it produces activity in HBD (on CsI)
      //
      detFlag = hbdhitTmp[khit].GetDetFlag();
      if(detFlag != 3)  continue;
      partid = hbdhitTmp[khit].GetIdPart();
      
      //
      // Neutral particles should not produce Cherenkov light, do not consider them
      //
      if(partid == 1 || partid == 4 || partid == 7 || partid == 10 || partid == 13 || partid == 16 || partid == 17 || partid == 18 || partid == 20 || partid == 22 || partid == 25 || partid == 26 || partid == 28 || partid == 30 || partid == 37 || partid == 38 || partid == 44 || partid == 48) continue;
      particle_flag = 1;
    }

  //
  // Start reading event
  //
  int npe = 0;
  for (khit=0; khit<PISAnHits; khit++)
    { // loop over hbd hits

      true_track = hbdhitTmp[khit].GetMctrack();
      if(true_track < 1) continue;
      dio_ptrkorigin(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		     &r_vertex, &z_vertex, &theta_vertex, &phi_vertex,
		     &itorigin, &idorigin, &idpart);

      dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		    &r_vertex, &z_vertex, &theta_vertex, &phi_vertex,
		    &itparent, &idparent, &idpart);

      //
      // Discard detector volume 2 (HBD MIP) and take only HBD CsI
      //
      detFlag = hbdhitTmp[khit].GetDetFlag();
      if (particle_flag==1 && detFlag == 3)
	{
	  
	  track = true_track;
	  id_part = hbdhitTmp[khit].GetIdPart();
	  
	  xin = hbdhitTmp[khit].GetXin();
	  yin = hbdhitTmp[khit].GetYin();
	  zin = hbdhitTmp[khit].GetZin();
	  ptof = hbdhitTmp[khit].GetTOF();
	  d_ghit->AddGhit(nHbdGhits);
	  d_ghit->set_nGhits(nHbdGhits+1);
	  d_ghit->get_ghit(nHbdGhits)->set_track(track);     // track
	  d_ghit->get_ghit(nHbdGhits)->set_idPart(id_part);    // idpart
	  d_ghit->get_ghit(nHbdGhits)->set_xyzin(0,xin);  // xin
	  d_ghit->get_ghit(nHbdGhits)->set_xyzin(1,yin);  // yin
	  d_ghit->get_ghit(nHbdGhits)->set_xyzin(2,zin);  // zin
	  d_ghit->get_ghit(nHbdGhits)->set_tof(ptof); // tof
	  nHbdGhits++;
	  if (id_part==50) npe++;
	}
    }

  //
  // No error
  //
  return 0;
  
}

//
// Copy GEANT hits information directly into HBD blobs
//
int HbdFinalSimulator::PulseSimAndfillCell()
{
  
  int ighit,nghits;
  int  j,k,i;
  int depth=-9999;
  int ihit;
  short i_contrib, N_contrib;
  short hit_time=-9999;

  //
  // Signal treatment
  //
  float diffuse = 3.;                              // time jitter in GEMs due to diffusion in gas
  float totDelay = 10.;                            // total delay
  float gain = 4000;                              // electrons after gain per primary electron
  float noise = 1000;                              // noise per channel (intergated)
  //
  // This number was discussed with Vejlko Radeka, and he says it's not very optimistic. The same for the shape.
  //
  
  float LEDthresh = 1.5 * noise;                   // discriminator threshlod
  
  //
  // Masses       emp  g   pos   elec  neut mu   mu    pi0 pi     pi     Kl  K      K      neut
  //
  float mP[33] =
    {
      -1.,-1.,0.511,0.511,-1.,105.7,105.7,-1.,139.57,139.57,-1.,493.68,493.68,-1,
      //		  prot   aprot  Ks  eta Lam Sig   Sig Sig   Xi  Xi    Om    aN  aL  aSig aSig aSig  aXi aXi   aOm
      938.27,938.27,-1.,-1.,-1.,1189.,-1.,1189.,-1.,1321.,1672.,-1.,-1.,1189.,-1.,1189.,-1.,1321.,1672.
    };
  
  //
  // Particle masses in MeV
  //
  float particle_mass;
  float scint_light;
  
  //
  // End detector describtion
  //
  
  float max_amplitude;
  short n_electron=-9999;
  short contrib_index[HBDMAXGCONTRIB] = {-1,0,0,0,0,0,0,0,0,0};
  float cell_threshold  = 1.0;   // p.e.
  float cell_time_threshold  = 135; // ns  
  short assigned_cell;
  int nhit = 0;
  float nparticles=0;
  
  //
  // Clean the arrays
  //
  for (int sect = 0; sect < NSECT; sect++)
    for (int pad = 0; pad < NPADS; pad++)
      {
	cell_end[sect][pad] = 0;
	cell_fire[sect][pad] = 0;
      }
  
  //
  // Loop over all GEANT hits in the event
  //
  nghits = d_ghit->get_nGhits();
  if(Verbose>0) cout << "HbdFinalSimulator::PulseSimAndfillCell-I: Number of input GEANT hits = "<< nghits << endl;
  
  for (ighit=0; ighit<nghits; ighit++)
    {
      
      //
      // Read event
      //
      
      //
      // Collect info of all hits
      //
      hit_int[nhit][0] = (int) d_ghit->get_ghit(ighit)->get_track();
      hit_int[nhit][3] = (int) d_ghit->get_ghit(ighit)->get_idPart();

      hit_float[nhit][7] = d_ghit->get_ghit(ighit)->get_xyzin(0);
      hit_float[nhit][8] = d_ghit->get_ghit(ighit)->get_xyzin(1);
      hit_float[nhit][9] = d_ghit->get_ghit(ighit)->get_xyzin(2);
      hit_float[nhit][23] = d_ghit->get_ghit(ighit)->get_tof();
      
      int panel_loc=0; 
      
      hbdgeo.GlobToLoc(hit_float[nhit][7],hit_float[nhit][8],hit_float[nhit][9], y_loc, z_loc, panel_loc);
      hbdgeo.PanelToSect(panel_loc, side_loc, sect_loc);
      sect_loc = panel_loc;  // Quick fix, since sect_loc defined as 0-11 not 0-5!!!
      int pad_loc;
      hbdgeo.FindNearestPad(y_loc,z_loc, pad_loc);

      //
      // Cut margines away. cells in columns -1 and NZ should go away.
      //
      if (pad_loc !=-9999 && sect_loc != -9999)
	{
	  //
	  // Add this hit to a cell
	  //
	  depth =cell_end[(int)sect_loc][(int)pad_loc];
	  cell_trail[(int)sect_loc][(int)pad_loc][depth] = nhit;
	  cell_end[(int)sect_loc][(int)pad_loc]++;
	  
	  //
	  // Okey, in fact we assumed that the electron falls in one cell completely,
	  // in other words GEM avalanche size
	  // for one electron is << than R cell (which is true).
	  // For smaller cells one can improve on sharing
	  // charge between adjacent cells. Search Sauli, or Bo Yu, or Carlien or etc,
	  //for their measurements.
	  //
	}
      
      nhit++;
      if(hit_int[ighit][3] != 50) nparticles++;
    }
  
  //
  // Analyze event (cell response)
  //
  
  for (int sect = 0; sect < NSECT; sect++)
    for (int pad = 0; pad < NPADS; pad++)
      {
	//
	// Clean it
	//
	for (j = 0; j < 200; j++)
	  {
	    sum_shaped_profile[0][j] = 0.0;
	    for (k = 0; k < HBDMAXGCONTRIB; k++)
	      {
		contrib_trail[k][j] = 0;
		contrib_profile[k][j] = 0.0;
		contrib_shaped_profile[k][j] = 0.0;
	      }
	  }

	//
	// for the moment NO scintillation
	//
	scint_light = 0;
	
	contrib_trail[0][2] = gRandom->Poisson(scint_light);
	contrib_trail[0][3] = gRandom->Poisson(scint_light);
	contrib_trail[0][4] = gRandom->Poisson(scint_light);
	
	//
	// Empty cell, save time
	//
	N_contrib = 1;  // 1st contributor (index 0) is noise
	if ( cell_end[sect][pad] != 0 )
	  {
	    
	    //
	    // For cell with a signal evaluate on particles in it
	    //
	    depth = cell_end[sect][pad];
	    
	    for (j = 0; j < depth; j++)
	      {
		ihit = cell_trail[sect][pad][j];
		
		//
		// Calculate hit time down to 1ns accuracy.
		//
		hit_time = (int) hit_float[ihit][23];
		if (hit_time < 0 || hit_time >= 200) continue;
		
		//
		// Check if the particle or its secondary are in the list of contributors
		//
		i_contrib = -1; // default
		
		//
		// If the track number of the hit is the same as of any of contributors,
		// get its contributer number
		//
		for (k = 1; k < N_contrib; k++)
		  if (hit_int[ihit][0] == hit_int[contrib_index[k]][0]) i_contrib = k;
		
		//
		// If not save this hit number position. (it might be just a p.e. not a particle itself)
		//
		if (i_contrib == -1)
		  {
		    i_contrib = N_contrib;
		    contrib_index[i_contrib] = ihit;
		    N_contrib++;
		  }
		
		//
		// We don't handle more than 10 contributors in one cell (very rare case btw...)
		//
	        // In practice, this happens 1/1000 events. The way this was coded creates seg faults 
		// and memory corruption. Because memory is corrupted valgrind and gdb were unable to 
		// find location of index out of bounds error. 
		// The only way to find error was to use brute force print statements.  
                // There is no way of knowing the effect this bug had on original HBD simulations. 
                // Since memory is corrupted it is likely that whatever was written to array was garbage.
		// The good news is that this happens only 1/1000 events so is at the 0.1% level error.. 
	
		//if (N_contrib == HBDMAXGCONTRIB) continue;
                if (N_contrib >= HBDMAXGCONTRIB) return -1;  

		
		//
		// Calculate signal amplitude in p.e.;
		//
		if ( hit_int[ihit][3] == 50)
		  {
		    //
		    // This is a photoelectron
		    //
		    n_electron = 1;
		  }
		else
		  {
		    
		    //
		    // This is a charged particle
		    //
		    n_electron = 0;
		    
		    //
		    // its mass
		    //
		    if(hit_int[ihit][3] < 33 && hit_int[ihit][3] >0 ) particle_mass = mP[hit_int[ihit][3]];
		    else  particle_mass = -1;
		    if ( particle_mass > 0 )
		      {
			n_electron = (int) gRandom->Landau(0.72,0.58);
		      }
		  }
		
		//
		// Add electrons to the time bin of the participant
		//
		contrib_trail[i_contrib][hit_time] += n_electron;
	      }
	  }
	
	//
	// Now let's make a signal profile
	//

	for (i_contrib = 0; i_contrib < N_contrib; i_contrib++)
	  for (j = 0; j < 200; j++)
	    {
	      
	      //
	      // Each electron independently should be transfered to its place in the profile
	      //

	      if (contrib_trail[i_contrib][j] > 0)
		for (k = 0; k < contrib_trail[i_contrib][j]; k++)
		  {
		    hit_time = j + (int) gRandom -> Gaus(totDelay,diffuse);
		    max_amplitude = 0.05 * gain * gRandom -> Exp(1.0);
		    
		    if (hit_time >= 0 && hit_time + 20 < 200)
		      {
			
			//
			// We assume exponentional response for a single electron,
			// let's CLT make it gaussian.
			// After talking to Vejlko:
			// GEM is fast, however it takes time about
			// (last gap)/(drift velosity) to collect the induced charge.
			// In order to simulate it we add one more cycle on the way,
			// spreading signal uniformly over some
			// nanoseconds. estimate of: 2[mm]/10[cm/mks] would give 20ns.
			//

			for (i = 0; i < 20; i++)
			  contrib_profile[i_contrib][hit_time+i] += max_amplitude;
		      }
		  }
	    }
	
	//
	// Pass it through the shaper and find maximum amplitude
	//


	max_amplitude = -999999.;
	for (j = 0; j < 200; j++)
	  {
	    
	    //
	    // Shape the signal
	    //
	    for(i_contrib = 0; i_contrib < N_contrib; i_contrib++)
	      for (k = j; k > 0; k--)
		contrib_shaped_profile[i_contrib][j] += (contrib_profile[i_contrib][k] * shaper[j-k]);
	    
	    //
	    // Add electronic noise to the first contributor
	    //
	    contrib_shaped_profile[0][j] += (gRandom -> Gaus(0.0,noise));
	    
	    //
	    // Add them all together
	    //
	    for(i_contrib = 0; i_contrib < N_contrib; i_contrib++)
	      sum_shaped_profile[0][j] += contrib_shaped_profile[i_contrib][j];
	    
	    //
	    // Find maximum
	    //
	    if (sum_shaped_profile[0][j] > max_amplitude)
	      {
		max_amplitude = sum_shaped_profile[0][j];
		hit_time = j;
	      }
	  }
	
	//
	// Max amplitude
	//
	cell_contrib_ampl[sect][pad][3] = max_amplitude;
	
	//
	// Partial amplitudes and idicies of 3 major contributors
	//
	if (N_contrib > 3) j = 3; else j = N_contrib;
	
	for (k = 0; k < j; k++)
	  {
	    
	    //
	    // Some contributors may legally have negative amplitude,
	    // if they arrived early enough and their shaper turned negative
	    // it will also cause in some cases the soem particle to contribute more
	    // signal than a total charge in this cell.
	    // I guess it' okey.
	    //
	    max_amplitude = -100000.;
	    for (i_contrib = 0; i_contrib < N_contrib; i_contrib++)
	      if (contrib_shaped_profile[i_contrib][hit_time] > max_amplitude)
		{
		  max_amplitude = contrib_shaped_profile[i_contrib][hit_time];
		  depth = i_contrib;
		}
	    
	    //
	    // Write partial amplitude and index
	    //
	    cell_contrib_ampl[sect][pad][k] = contrib_shaped_profile[depth][hit_time]/cell_contrib_ampl[sect][pad][3];
	    cell_contrib_index[sect][pad][k] = contrib_index[depth];
	    
	    //
	    // This contributor's out of game. back to the loop till we get all three.
	    //
	    contrib_shaped_profile[depth][hit_time] = -1;
	  }
	
	//
	// Find where signal becomes negative (imitation of leading edge discriminator)
	// 1ns buinning accuracy is an imitation of the time-to-digit convertor.
	//
	j = 0;
	while  ( sum_shaped_profile[0][j] >= -LEDthresh && j < 200 ) j++;
	cell_time[sect][pad] = j;
	
	//
	// Fill whatever is ready
	//
	d_cell->AddCell(nHbdCells);
	d_cell->set_nCells(nHbdCells+1);
	
	//
	// Dummies
	//
	d_cell->get_cell(nHbdCells)->set_sector(sect);  // sect
	d_cell->get_cell(nHbdCells)->set_padnum(pad);   // pad
	d_cell->get_cell(nHbdCells)->set_charge(cell_contrib_ampl[sect][pad][3]*0.00025);  // charge in p.e.
	
	// if (N_contrib >= 3) j = 3;
	// else
	//   {
	//     j = N_contrib;
	//   }
	
	// for (i_contrib = 0; i_contrib < j; i_contrib++)
	//   {
	//     ihit = cell_contrib_index[sect][pad][i_contrib];
	//   }
	nHbdCells++;
	
	//
	// end loop over all cells
	//
      }

  //
  // Compare all to threshlod
  //
  assigned_cell = 0;
  
  for (int sect = 0; sect < NSECT; sect++)
    for (int pad = 0; pad < NPADS; pad++)
      {
	cell_contrib_ampl[sect][pad][3] *= 0.00025;  // make them in p.e.
	
	if( (cell_contrib_ampl[sect][pad][3] > cell_threshold ) &&
	    (cell_time[sect][pad] <= cell_time_threshold )   )
	  {
	    cell_fire[sect][pad] = 1;
	    assigned_cell++;
	  }
      }

  nHbdCells = d_cell->get_nCells();
  if(Verbose>0) cout << "HbdFinalSimulator::PulseSimAndfillCell-I: Number of cells stored = "
       << nHbdCells << endl;

  //
  // no error
  //
  return 0;
  
}

