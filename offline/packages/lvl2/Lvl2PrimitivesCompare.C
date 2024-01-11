#include "Lvl2PrimitivesCompare.h"
#include <Event.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHTypedNodeIterator.h>
#include <L2DecisionHelper.h> // offline/packages/lvl2
#include <TrigRunLvl1.h>
#include <TrigLvl1.h>
#include <TrigRunLvl2.h>
#include <recoConsts.h>
#include <Lvl2OutArrayv1.h>

#include <getClass.h>
#include <iomanip>
#include <cstdlib>

#include <L2BbcPrimitive.h>
#include <L2EMCHighPtTileList.h>
#include <L2MuiSymsetCalcPerPanel.h>
#include <L2MuiTracks.h>
#include <L2MuiPairs.h>
#include <L2AuAuElectronCandidate.h>
#include <L2AuAuElectronLowMassPairs.h>

using namespace std;

typedef PHIODataNode<Lvl2OutArray> Lvl2OutArrayNode_t;
static Lvl2OutArray *lvl2outarray = 0;
static Lvl2OutArray *lvl2outarraycal = 0;

Lvl2PrimitivesCompare::Lvl2PrimitivesCompare(const char *name): SubsysReco(name)
{
  nevt = 0;

}

int Lvl2PrimitivesCompare::Init(PHCompositeNode *topNode)
{
  nfoundprims = 0;
  nfoundlength = 0;

  for(int i=0;i<50;i++)
    primevtreset[i]=0;

  return 0;
}
  
void Lvl2PrimitivesCompare::identify(ostream& out) const
{
  cout << "LVL2PRIMITIVESCOMPARE" << endl;
  return;
}


int Lvl2PrimitivesCompare::process_event(PHCompositeNode *topNode)
{
  // verbosity is defined in SubsysReco

  /////////////////////////////////////////////////////////////////
  // Increment and set the event number counter
  ////////////////////////////////////////////////////////////////

  nevt++;
  if(nevt%1 == 0 && verbosity>3)
    {
      cout << "Lvl2PrimitivesCompare: Nevts = " << nevt << endl;
    } 
 
  /////////////////////////////////////////////////////////////////
  // Check to see if this is a data event
  /////////////////////////////////////////////////////////////////

  PHNodeIterator iter(topNode);
  
  Event *evt = findNode::getClass<Event>(topNode,"PRDF");
  
      if (!evt)
	{
	  cout << PHWHERE << "NULL Event Pointer" << endl;
	  return -1;
	}  

  // If this is not a data event, skip it
  if( evt->getEvtType() != 1 )
    {
      cout << "Lvl2PrimitivesCompare: Not a data event, skip it - event type = " 
	   << evt->getEvtType() << endl;
      return 0;
    }
  
  /////////////////////////////////////////////////////////////
  // Find the Lvl2OutArray object
  ////////////////////////////////////////////////////////////

  PHCompositeNode *dstNode = NULL;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if(!dstNode)
    {
      cout << "Lvl2PrimitivesCompare::process_event: No DST node, do nothing and return!"
	   << endl;
      return 1;
    }


  PHTypedNodeIterator<Lvl2OutArray> l2iter(dstNode);
 
  // First find Lvl2OutArray on the dst node
  // This is the primitive object from the DAQ

  Lvl2OutArrayNode_t *lvl2node = l2iter.find("Lvl2OutArray");
  
  if (!lvl2node)
    {
      cout << "Lvl2PrimitivesCompare: no Lvl2OutArray Node" << endl;
      return -1;
    }
  
  lvl2outarray = lvl2node->getData();
  
  if ( !lvl2outarray )
    {
      cout << "Lvl2PrimitivesCompare: no Lvl2OutArray object " << endl;
      return -1;
    }

  // Now find Lvl2OutArrayCal, this is the one from the offline triggers

  Lvl2OutArrayNode_t *lvl2nodecal = l2iter.find("Lvl2OutArrayCal");
  
  if (!lvl2nodecal)
    {
      cout << "Lvl2PrimitivesCompare: no Lvl2OutArrayCal Node" << endl;
      return -1;
    }
  
  lvl2outarraycal = lvl2nodecal->getData();
  
  if ( !lvl2outarraycal)
    {
      cout << "Lvl2PrimitivesCompare: no Lvl2OutArrayCal object " << endl;
      return -1;
    }

  ///////////////////////////////////////////////////////////////  
  // Now loop over all of the primitives in Lvl2OutArray
  ///////////////////////////////////////////////////////////////

  // Reset for a new event
  for(int ifoundprim=0;ifoundprim<nfoundprims;ifoundprim++)
    primevtreset[ifoundprim]=0;
  
  
  int Nprim=50;

  if(verbosity>1)
    cout << "Event: " << nevt << " examine primitives from Lvl2OutArray" << endl; 

  for (int iprim=0;iprim<Nprim;iprim++)
    {
      if(lvl2outarray->getdatalength(iprim) >= 0)
	{
	  // This one has non zero length, look for the counterpart 
	  // in Lvl2OutArrayCal

	  for (int iprimcal=0;iprimcal<Nprim;iprimcal++)
	    {
	      if(!strcmp(lvl2outarray->getname(iprim),lvl2outarraycal->getname(iprimcal)))
		{
		  if(verbosity>1)
		      cout << "    " << lvl2outarray->getname(iprim) 
			   << " Lvl2outArray length " << lvl2outarray->getdatalength(iprim) 
			   << " lvl2outArrayCal length "<< lvl2outarraycal->getdatalength(iprimcal) 
			   << endl;

		  if(lvl2outarray->getdatalength(iprim) != lvl2outarraycal->getdatalength(iprimcal))
		    {
		      // length discrepancy

		      bool foundlength = false;
		      
		      for(int ifoundprim=0;ifoundprim<nfoundlength;ifoundprim++)
			{
			  if(!strcmp(lvl2outarray->getname(iprim),lengthfoundname[ifoundprim]))
			    {
			      // This primitive has had earlier discrepancies
			      
			      foundlength = true;
			      
			      // increment the numbers for this primitive
			      // Increment primevtcount only once per event for each prim
			      lengthcount[ifoundprim]++;
			      
			      if(verbosity>1)
				cout << " old: " << lengthfoundname[ifoundprim] << " evtcount " 
				     << lengthcount[ifoundprim] 
				     << endl;
			    }
			}
		      
		      if(!foundlength)
			{
			  // This is the first discrepancy for this primitive
			  
			  strcpy(lengthfoundname[nfoundlength],lvl2outarray->getname(iprim));
			  lengthcount[nfoundlength]++;
			  
			  if(verbosity>1)
			    cout << "new: " << lengthfoundname[nfoundlength] << " evtcount " 
				 << lengthcount[nfoundprims] << endl;
			  
			  nfoundlength++;
			}
		    
		  
		      cout << "Lvl2PrimitivesCompare: Found length discrepancy: " << endl;
		      evt->identify(); 
		      cout << " for primitive " 
			   << lvl2outarray->getname(iprim) << endl << "    DAQ length = "  
			   << lvl2outarray->getdatalength(iprim) << " offline length = "
			   << lvl2outarraycal->getdatalength(iprimcal)
			   << endl;

		      cout << "    ATP number = " 
			   << hex <<  evt->getFrameEntry("FRAMESOURCEID", 14001, 0) << "  hex or "
			   << dec <<  evt->getFrameEntry("FRAMESOURCEID", 14001, 0) << " decimal" << endl;
		      
		      if(!strcmp(lvl2outarray->getname(iprim),"L2EMCHighPtTileList" ))
			PrintL2EMCHighPtTilePrimitive(lvl2outarray,lvl2outarraycal);
		    
		      if(!strcmp(lvl2outarray->getname(iprim),"L2MuiPairs") ||
			 !strcmp(lvl2outarray->getname(iprim),"L2MuiTracks") ||
			 !strcmp(lvl2outarray->getname(iprim),"L2MuiSymsetCalcPerPanel") )
			PrintL2MuiPrimitives(lvl2outarray,lvl2outarraycal);
		    
		      // Not much point in doing a binary comparison if the length is different
		      break;
		    }
		  
		  PHDWORD* outword_ptr = lvl2outarray->getdata(iprim);
		  PHDWORD* outword_ptr_cal = lvl2outarraycal->getdata(iprimcal);
		  
		  bool got_one=false;

		  for(int il=0;il<lvl2outarray->getdatalength(iprim);il++)
		    {
		      if(verbosity>3)
			cout << " DAQ " << *outword_ptr << " OFFL " << *outword_ptr_cal << endl;

		      if(*outword_ptr != *outword_ptr_cal)
			{
			  got_one=true;

			  if(verbosity>3)
			    {
			      cout << "Lvl2PrimitivesCompare: Event " << evt->getEvtSequence() 
				   << " found binary discrepancy for " << lvl2outarray->getname(iprim)
				   << "     word " << il << " DAQ value " << *outword_ptr 
				   << " offline value " << *outword_ptr_cal << " difference " 
				   <<  *outword_ptr -  *outword_ptr_cal
				   << endl;
			      
			      cout << "    ATP number = " 
				   << hex <<  evt->getFrameEntry("FRAMESOURCEID", 14001, 0) << "  hex or "
				   << dec <<  evt->getFrameEntry("FRAMESOURCEID", 14001, 0) << " decimal" << endl;
			    }

			  // Keep a count of discrepancies for each primitive
			  
			  bool foundit = false;
			  
			  for(int ifoundprim=0;ifoundprim<nfoundprims;ifoundprim++)
			    {
			      if(!strcmp(lvl2outarray->getname(iprim),primfoundname[ifoundprim]))
				{
				  // This primitive has had earlier discrepancies
				  
				  foundit = true;
				  
				  // increment the numbers for this primitive
				  // Increment primevtcount only once per event for each prim
				  if(primevtreset[ifoundprim] == 0)
				    {
				      primevtcount[ifoundprim]++;
				      primevtreset[ifoundprim]=1;
				    }
				  primprobcount[ifoundprim]++;
				  
				  if(verbosity>1)
				    cout << " old: " << primfoundname[ifoundprim] << " evtcount " 
					 << primevtcount[ifoundprim] << " primprobcount "
					 << primprobcount[ifoundprim] << endl;
				}
			    }
			  
			  if(!foundit)
			    {
			      // This is the first discrepancy for this primitive
			      
			      strcpy(primfoundname[nfoundprims],lvl2outarray->getname(iprim));
			      primevtcount[nfoundprims]++;
			      primevtreset[nfoundprims]=1;
			      primprobcount[nfoundprims]++;

			      
			      if(verbosity>1)
				cout << "new: " << primfoundname[nfoundprims] << " evtcount " 
				     << primevtcount[nfoundprims] << " primprobcount "
				     << primprobcount[nfoundprims] << endl;

			      nfoundprims++;
			    }
			}

		      outword_ptr++;
		      outword_ptr_cal++;
		    }

		  if(verbosity>0)
		    if(got_one)
		      {
			if(verbosity>0)
			  {
			    cout << "Lvl2PrimitivesCompare: Event " << evt->getEvtSequence() 
				 << " binary discrepancy for " << lvl2outarray->getname(iprim)
				 << "  ATP number " << hex 
				 <<  evt->getFrameEntry("FRAMESOURCEID", 14001, 0) << " hex" 
				 << dec << endl;
			  }

			if(!strcmp(lvl2outarray->getname(iprim),"L2AuAuElectronCandidate" ))
			  PrintL2AuAuElectronCandidate(lvl2outarray,lvl2outarraycal);
			
			if(!strcmp(lvl2outarray->getname(iprim),"L2AuAuElectronLowMassPairs" ))
			  PrintL2AuAuElectronLowMassPairs(lvl2outarray,lvl2outarraycal);

			
		      }
		  
		}		        
	    }
	}
    }
  
  
  
  if(verbosity>1)
    cout << "Leaving Lvl2PrimitivesCompare:: process_event" << endl;

  return 0;
}

int Lvl2PrimitivesCompare::BeginRun(const int runno) 
{
  return 0;
}

int Lvl2PrimitivesCompare::EndRun(const int runno) 
{
  cout << endl << "Lvl2PrimitivesCompare::EndRun: Total events = " << nevt
       << endl << endl;

  // Print summary of discrepancies here

  // Length discrepancies
  cout << "  Primitive length discrepancies:";

  if(nfoundlength >0)
    {
      cout << endl << endl;
      cout << setw(36) << " Primitive name " 
	   << setw(20) << " length discrepancies " 
	   << endl;
      
      for(int ifoundprim=0;ifoundprim<nfoundlength;ifoundprim++)
	{
	  cout << setw(36) << lengthfoundname[ifoundprim] 
	       << setw(20) << lengthcount[ifoundprim]
	       << endl; 
	  
	}
    }
  else
    cout << "  No length discrepancies found" << endl;
  
  cout << endl;

  // Binary comparison discrepancies
  cout << "  Binary comparison discrepancies:";

  if(nfoundprims>0)
    {
      cout << endl << endl;
      cout << setw(36) << " Primitive name " 
	   << setw(20) << " Problem events " 
	   << setw(20) << " Total Problems "
	   << endl;
  
      for(int ifoundprim=0;ifoundprim<nfoundprims;ifoundprim++)
	{
	  cout << setw(36) << primfoundname[ifoundprim] 
	       << setw(20) << primevtcount[ifoundprim]
	       << setw(20) << primprobcount[ifoundprim]
	       << endl; 
	}
    }
  else
    cout << "  No binary comparison discrepancies found" << endl;

  cout << endl;
  
  return 0;
}

void Lvl2PrimitivesCompare::PrintL2EMCHighPtTilePrimitive(Lvl2OutArray *lvl2outarray,Lvl2OutArray * lvl2outarraycal) 
{
  //dump the primitive here, need a readback pointer 
  if(lvl2outarray->GetPrimitive("L2EMCHighPtTileList"))
    {
      for(int sector=0;sector<8;sector++)
	{			
	  L2EMCHighPtTileListRBP emctilelist(sector);
	  if (emctilelist.wasRead())
	    {
	      int ntiles = emctilelist->tilesum.size();
	      if(ntiles>0)
		{
		  cout << "    DAQ L2EMCHighPtTileList sector " << sector 
		       << " ntiles = " << ntiles << endl;
		  for(int i =0;i<ntiles;i++)
		    {
		      cout << "      tileID " << emctilelist->tilesum[i].tileID
			   << " energy " <<  emctilelist->tilesum[i].energy
			   << " ntowers " << emctilelist->tilesum[i].ntowers
			   << endl;
		    }
		}
	    }
	}
    }
  
  if(lvl2outarraycal->GetPrimitive("L2EMCHighPtTileList"))
    {
      for(int sector=0;sector<8;sector++)
	{			
	  L2EMCHighPtTileListRBP emctilelistcal(sector);
	  if (emctilelistcal.wasRead())
	    {				  
	      int ntiles = emctilelistcal->tilesum.size();
	      if(ntiles>0)
		{
		  cout << "    Offline L2EMCHighPtTileList sector " 
		       << sector << " ntiles " << ntiles << endl;
		  for(int i =0;i<ntiles;i++)
		    {
		      cout << "      tileID " << emctilelistcal->tilesum[i].tileID
			   << " energy " <<  emctilelistcal->tilesum[i].energy
			   << " ntowers " << emctilelistcal->tilesum[i].ntowers
			   << endl;
		    }
		}
	    }
	}
    } 

  return;
}

void Lvl2PrimitivesCompare::PrintL2MuiPrimitives(Lvl2OutArray * lvl2outarray,Lvl2OutArray *lvloutarraycal)
{
  cout << endl << "Checking DAQ L2MuiPairs: " << endl;
  
  if(lvl2outarray->GetPrimitive("L2MuiPairs"))
    {
      for(int arm=0;arm<2;arm++)
	{			
	  L2MuiPairsRBP pairs(arm);
	  if (pairs.wasRead())
	    {
	      int npairs = pairs->mupairs.size();
	      
	      cout << "    DAQ L2MuiPairs for arm " << arm 
		   << " has npairs " << npairs << endl; 
	      
	      for(int i=0;i<npairs;i++)
		{
		  cout << "      id1 " << pairs->mupairs[i].id1
		       << "    id2 " << pairs->mupairs[i].id2
		       << "    openangle " << pairs->mupairs[i].openangle
		       << "    mass " << pairs->mupairs[i].mass
		       << endl;
		}
	    }
	}
    }
  
  cout << endl << "Checking OFFLINE L2MuiPairs: " << endl;
  
  if(lvl2outarraycal->GetPrimitive("L2MuiPairs"))
    {
      for(int arm=0;arm<2;arm++)
	{			
	  L2MuiPairsRBP pairscal(arm);
	  if (pairscal.wasRead())
	    {
	      int npairs = pairscal->mupairs.size();
	      
	      cout << "    OFFLINE L2MuiPairs for arm " << arm 
		   << " has npairs " << npairs << endl; 
	      
	      for(int i=0;i<npairs;i++)
		{
		  cout << "      id1 " << pairscal->mupairs[i].id1
		       << "    id2 " << pairscal->mupairs[i].id2
		       << "    openangle " << pairscal->mupairs[i].openangle
		       << "    mass " << pairscal->mupairs[i].mass
		       << endl;
		}
	    }
	}
    }
  
  cout << endl << "Checking DAQ L2MuiTracks: " << endl;
  
  if(lvl2outarray->GetPrimitive("L2MuiTracks"))
    {
      for(int arm=0;arm<2;arm++)
	{			
	  L2MuiTracksRBP tracks(arm);
	  if (tracks.wasRead())
	    {
	      int ntrks = tracks->tracks.size();
	      cout << "    DAQ L2MuiTracks for arm " 
		   << arm << " has ntrks " << ntrks << endl;
	      for(int i =0;i<ntrks;i++)
		{
		  cout << "      depth " << tracks->tracks[i].depth
		       << " pmin " << tracks->tracks[i].pmin
		       << " slope " << tracks->tracks[i].slope
		       << " hid " << tracks->tracks[i].hid
		       << " vid " << tracks->tracks[i].vid
		       << " panel " << tracks->tracks[i].panel
		       << " hitsum " << tracks->tracks[i].hitsum 
		       << endl
		       << "        dir 0 " << tracks->tracks[i].direction[0]
		       << " dir 0 " << tracks->tracks[i].direction[1]
		       << " dir 0 " << tracks->tracks[i].direction[2]
		       << endl;
		}
	    }
	}
    }
  
  cout << endl << "Checking OFFLINE L2MuiTracks: " << endl;
  
  if(lvl2outarraycal->GetPrimitive("L2MuiTracks"))
    {
      for(int arm=0;arm<2;arm++)
	{			
	  L2MuiTracksRBP trackscal(arm);
	  if(trackscal.wasRead())
	    {
	      int ntrks = trackscal->tracks.size();
	      
	      cout << "    Offline L2MuiTracks for arm " 
		   << arm << " has ntrks " << ntrks << endl;
	      for(int i =0;i<ntrks;i++)
		{
		  cout << "     depth " << trackscal->tracks[i].depth
		       << " pmin " << trackscal->tracks[i].pmin
		       << " slope " << trackscal->tracks[i].slope
		       << " hid " << trackscal->tracks[i].hid
		       << " vid " << trackscal->tracks[i].vid
		       << " panel " << trackscal->tracks[i].panel
		       << " hitsum " << trackscal->tracks[i].hitsum 
		       << endl
		       << "        dir 0 " << trackscal->tracks[i].direction[0]
		       << " dir 0 " << trackscal->tracks[i].direction[1]
		       << " dir 0 " << trackscal->tracks[i].direction[2]
		       << endl;
		}
	    }
	}
      
    }
  
  cout << endl << "Checking DAQ L2MuiSymsetCalcPerPanel: " << endl;
  
  if(lvl2outarray->GetPrimitive("L2MuiSymsetCalcPerPanel"))
    {
      cout << endl;
      
      for(int ip=0;ip<32;ip++)
	{
	  L2MuiSymsetCalcPerPanelRBP symset(ip);
	  if(symset.wasRead())
	    {
	      cout << "    DAQ symset " << ip << endl << "    "; 
	      for(unsigned int i=0;i<symset->nhitspergap.size();i++)
		{
		  cout << "  gap " << i << " hits  " << symset->nhitspergap[i];
		}
	      cout << endl << "    ";
	      
	      for(unsigned int i=0;i<symset->nroads.size();i++)
		{
		  cout << "  gap " << i << " roads " << symset->nroads[i];
		}
	      cout << endl;
	      
	      for(unsigned int i=0;i<symset->symsets.size();i++)
		{
		  cout << "       Symsets " << i 
		       << " idx " << symset->symsets[i].idx
		       << " depth " << symset->symsets[i].depth
		       << " slope " << symset->symsets[i].cos 
		       << " gapbit0 " << symset->symsets[i].gapbit[0] 
		       << " gapbit1 " << symset->symsets[i].gapbit[1] 
		       << " gapbit2 " << symset->symsets[i].gapbit[2] 
		       << " gapbit3 " << symset->symsets[i].gapbit[3] 
		       << " gapbit4 " << symset->symsets[i].gapbit[4] 
		       << endl;
		}
	    }     
	}
    }
  
  cout << endl << "Checking OFFLINE L2MuiSymsetCalcPerPanel: " << endl;
  
  if(lvl2outarraycal->GetPrimitive("L2MuiSymsetCalcPerPanel"))
    {
      cout << endl;
      
      for(int ip=0;ip<32;ip++)
	{
	  L2MuiSymsetCalcPerPanelRBP symsetcal(ip);
	  if(symsetcal.wasRead())
	    {
	      cout << "    OFFLINE symset " << ip << endl << "    ";
	      for(unsigned int i=0;i<symsetcal->nhitspergap.size();i++)
		{
		  cout << "  gap " << i << " hits  " << symsetcal->nhitspergap[i];
		}
	      cout << endl << "    ";
	      
	      for(unsigned int i=0;i<symsetcal->nroads.size();i++)
		{
		  cout << "  gap " << i << " roads " << symsetcal->nroads[i];
		}
	      cout << endl;
	      
	      for(unsigned int i=0;i<symsetcal->symsets.size();i++)
		{
		  cout << "       Symsets " << i 
		       << " idx " << symsetcal->symsets[i].idx
		       << " depth " << symsetcal->symsets[i].depth
		       << " slope " << symsetcal->symsets[i].cos 
		       << " gapbit0 " << symsetcal->symsets[i].gapbit[0] 
		       << " gapbit1 " << symsetcal->symsets[i].gapbit[1] 
		       << " gapbit2 " << symsetcal->symsets[i].gapbit[2] 
		       << " gapbit3 " << symsetcal->symsets[i].gapbit[3] 
		       << " gapbit4 " << symsetcal->symsets[i].gapbit[4] 
		       << endl;
		}				      
	    }
	}			    
    }
}

void Lvl2PrimitivesCompare::PrintL2AuAuElectronCandidate(Lvl2OutArray *lvl2outarray,Lvl2OutArray * lvl2outarraycal) 
{

  struct  electronKin 
  {
    bool  KilledByPC3;
    float charge;
    float theta0;
    float phi0;
    float ptot;
    
    float xrich;
    float yrich;
    float zrich;
    int   pmtCent;   //.. pmt ID of center of the ring 
    int   npmt;
    float npe;
    float TrkRingDist;  //.. distance between track and associated ring
    
    float xemc;
    float yemc;
    float zemc;
    float energy;      //.. associated EMCal cluster energy
    
    float xpc1;
    float ypc1;
    float zpc1;
    
    int armEMC;
    int sectorEMC;
    
    float ptotpc3;
    float xpc3;
    float ypc3;
    float zpc3;
    float theta0pc3;
    float phi0pc3;
    float dca;
    float alpha;    
    float zvertex;
  }; 

  electronKin e[100];
  electronKin ecal[100];
  
  //dump the primitive here, need a readback pointer 
  lvl2outarray->GetPrimitive("L2AuAuElectronCandidate");
  L2AuAuElectronCandidateRBP electron(0);
  if (!electron.wasRead())
    {
      cout << "Error: Could not read L2AuAuElectronCandidate from lvl2outarray" << endl;
      return;
    }  

  if(electron->Candidate.size() > 100)
    {
      cout << "WARNING: Exceeded electron track dimension in PrintL2AuAuElectronCandidate, size = " 
	   <<  electron->Candidate.size() << " return and do nothing" << endl;
      return;
    }

  cout.precision(15);
  
  for(unsigned int i=0;i<electron->Candidate.size();i++)
    {
      e[i].KilledByPC3 = electron->Candidate[i].KilledByPC3;
      e[i].charge = electron->Candidate[i].charge;
      e[i].theta0 = electron->Candidate[i].theta0;
      e[i].phi0 = electron->Candidate[i].phi0;
      e[i].ptot = electron->Candidate[i].ptot;
     e[i].xrich = electron->Candidate[i].xrich;
     e[i].yrich = electron->Candidate[i].yrich;
     e[i].zrich = electron->Candidate[i].zrich;
     e[i].pmtCent = electron->Candidate[i].pmtCent;
     e[i].npmt = electron->Candidate[i].npmt;
     e[i].npe = electron->Candidate[i].npe;
     e[i].TrkRingDist = electron->Candidate[i].TrkRingDist;
     e[i].xemc = electron->Candidate[i].xemc;
     e[i].yemc = electron->Candidate[i].yemc;
     e[i].zemc = electron->Candidate[i].zemc;
     e[i].energy = electron->Candidate[i].energy;
     e[i].xpc1 = electron->Candidate[i].xpc1;
     e[i].ypc1 = electron->Candidate[i].ypc1;
     e[i].zpc1 = electron->Candidate[i].zpc1;
     e[i].armEMC = electron->Candidate[i].armEMC;
     e[i].sectorEMC = electron->Candidate[i].sectorEMC;
     e[i].ptotpc3 = electron->Candidate[i].ptotpc3;
     e[i].xpc3 = electron->Candidate[i].xpc3;
     e[i].ypc3 = electron->Candidate[i].ypc3;
     e[i].zpc3 = electron->Candidate[i].zpc3;
     e[i].theta0pc3 = electron->Candidate[i].theta0pc3;
     e[i].phi0pc3 = electron->Candidate[i].phi0pc3;
     e[i].dca = electron->Candidate[i].dca;
     e[i].alpha = electron->Candidate[i].alpha;
     e[i].zvertex = electron->Candidate[i].zvertex;

    }

  if(verbosity>3)
    {
      cout << "Electron tracks:" << endl;
      
      for(unsigned int i=0;i<electron->Candidate.size();i++)
	{
	  cout << " Electron track " << i << endl;
	  cout << " KilledByPC3 " << electron->Candidate[i].KilledByPC3 << endl;
	  cout << " charge " << electron->Candidate[i].charge << endl;
	  cout << " theta0 " << electron->Candidate[i].theta0 << endl;
	  cout << " phi0 " << electron->Candidate[i].phi0 << endl;
	  cout << " ptot " << electron->Candidate[i].ptot << endl;
	  cout << " xrich " << electron->Candidate[i].xrich << endl;
	  cout << " yrich " << electron->Candidate[i].yrich << endl;
	  cout << " zrich " << electron->Candidate[i].zrich << endl;
	  cout << " pmtCent " << electron->Candidate[i].pmtCent << endl;
	  cout << " npmt " << electron->Candidate[i].npmt << endl;
	  cout << " npe " << electron->Candidate[i].npe << endl;
	  cout << " TrkRingDist " << electron->Candidate[i].TrkRingDist << endl;
	  cout << " xemc " << electron->Candidate[i].xemc << endl;
	  cout << " yemc " << electron->Candidate[i].yemc << endl;
	  cout << " zemc " << electron->Candidate[i].zemc << endl;
	  cout << " energy " << electron->Candidate[i].energy << endl;
	  cout << " xpc1 " << electron->Candidate[i].xpc1 << endl;
	  cout << " ypc1 " << electron->Candidate[i].ypc1 << endl;
	  cout << " zpc1 " << electron->Candidate[i].zpc1 << endl;
	  cout << " armEMC " << electron->Candidate[i].armEMC << endl;
	  cout << " sectorEMC " << electron->Candidate[i].sectorEMC << endl;
	  cout << " ptotpc3 " << electron->Candidate[i].ptotpc3 << endl;
	  cout << " xpc3 " << electron->Candidate[i].xpc3 << endl;
	  cout << " ypc3 " << electron->Candidate[i].ypc3 << endl;
	  cout << " zpc3 " << electron->Candidate[i].zpc3 << endl;
	  cout << " theta0pc3 " << electron->Candidate[i].theta0pc3 << endl;
	  cout << " phopc3 " << electron->Candidate[i].phi0pc3 << endl;
	  cout << " dca " << electron->Candidate[i].dca << endl;
	  cout << " alpha " << electron->Candidate[i].alpha << endl;
	  cout << "zvertex " << electron->Candidate[i].zvertex << endl;
	}
    }

  
  lvl2outarraycal->GetPrimitive("L2AuAuElectronCandidate");
  L2AuAuElectronCandidateRBP electroncal(0);
  if (!electroncal.wasRead())
    {
      cout << "Error: Could not read L2AuAuElectronCandidate from lvl2outarraycal" << endl;
      return;
    }  


  for(unsigned int i=0;i<electroncal->Candidate.size();i++)
    {
      ecal[i].KilledByPC3 = electroncal->Candidate[i].KilledByPC3;
      ecal[i].charge = electroncal->Candidate[i].charge;
      ecal[i].theta0 = electroncal->Candidate[i].theta0;
      ecal[i].phi0 = electroncal->Candidate[i].phi0;
      ecal[i].ptot = electroncal->Candidate[i].ptot;
      ecal[i].xrich = electroncal->Candidate[i].xrich;
      ecal[i].yrich = electroncal->Candidate[i].yrich;
      ecal[i].zrich = electroncal->Candidate[i].zrich;
      ecal[i].pmtCent = electroncal->Candidate[i].pmtCent;
      ecal[i].npmt = electroncal->Candidate[i].npmt;
      ecal[i].npe = electroncal->Candidate[i].npe;
      ecal[i].TrkRingDist = electroncal->Candidate[i].TrkRingDist;
      ecal[i].xemc = electroncal->Candidate[i].xemc;
      ecal[i].yemc = electroncal->Candidate[i].yemc;
      ecal[i].zemc = electroncal->Candidate[i].zemc;
      ecal[i].energy = electroncal->Candidate[i].energy;
      ecal[i].xpc1 = electroncal->Candidate[i].xpc1;
      ecal[i].ypc1 = electroncal->Candidate[i].ypc1;
      ecal[i].zpc1 = electroncal->Candidate[i].zpc1;
      ecal[i].armEMC = electroncal->Candidate[i].armEMC;
      ecal[i].sectorEMC = electroncal->Candidate[i].sectorEMC;
      ecal[i].ptotpc3 = electroncal->Candidate[i].ptotpc3;
      ecal[i].xpc3 = electroncal->Candidate[i].xpc3;
      ecal[i].ypc3 = electroncal->Candidate[i].ypc3;
      ecal[i].zpc3 = electroncal->Candidate[i].zpc3;
      ecal[i].theta0pc3 = electroncal->Candidate[i].theta0pc3;
      ecal[i].phi0pc3 = electroncal->Candidate[i].phi0pc3;
      ecal[i].dca = electroncal->Candidate[i].dca;
      ecal[i].alpha = electroncal->Candidate[i].alpha;
      ecal[i].zvertex = electroncal->Candidate[i].zvertex;
      
    }
  
  if(verbosity>3)
    {
      cout << "Electroncal tracks:" << endl;
      
      for(unsigned int i=0;i<electroncal->Candidate.size();i++)
	{
	  cout << " Electroncal track " << i << endl;
	  cout << " KilledByPC3 " << electroncal->Candidate[i].KilledByPC3 << endl;
	  cout << " charge " << electroncal->Candidate[i].charge << endl;
	  cout << " theta0 " << electroncal->Candidate[i].theta0 << endl;
	  cout << " phi0 " << electroncal->Candidate[i].phi0 << endl;
	  cout << " ptot " << electroncal->Candidate[i].ptot << endl;
	  cout << " xrich " << electroncal->Candidate[i].xrich << endl;
	  cout << " yrich " << electroncal->Candidate[i].yrich << endl;
	  cout << " zrich " << electroncal->Candidate[i].zrich << endl;
	  cout << " pmtCent " << electroncal->Candidate[i].pmtCent << endl;
	  cout << " npmt " << electroncal->Candidate[i].npmt << endl;
	  cout << " npe " << electroncal->Candidate[i].npe << endl;
	  cout << " TrkRingDist " << electroncal->Candidate[i].TrkRingDist << endl;
	  cout << " xemc " << electroncal->Candidate[i].xemc << endl;
	  cout << " yemc " << electroncal->Candidate[i].yemc << endl;
	  cout << " zemc " << electroncal->Candidate[i].zemc << endl;
	  cout << " energy " << electroncal->Candidate[i].energy << endl;
	  cout << " xpc1 " << electroncal->Candidate[i].xpc1 << endl;
	  cout << " ypc1 " << electroncal->Candidate[i].ypc1 << endl;
	  cout << " zpc1 " << electroncal->Candidate[i].zpc1 << endl;
	  cout << " armEMC " << electroncal->Candidate[i].armEMC << endl;
	  cout << " sectorEMC " << electroncal->Candidate[i].sectorEMC << endl;
	  cout << " ptotpc3 " << electroncal->Candidate[i].ptotpc3 << endl;
	  cout << " xpc3 " << electroncal->Candidate[i].xpc3 << endl;
	  cout << " ypc3 " << electroncal->Candidate[i].ypc3 << endl;
	  cout << " zpc3 " << electroncal->Candidate[i].zpc3 << endl;
	  cout << " theta0pc3 " << electroncal->Candidate[i].theta0pc3 << endl;
	  cout << " phopc3 " << electroncal->Candidate[i].phi0pc3 << endl;
	  cout << " dca " << electroncal->Candidate[i].dca << endl;
	  cout << " alpha " << electroncal->Candidate[i].alpha << endl;
	  cout << "zvertex " << electroncal->Candidate[i].zvertex << endl;
	}
    }

  if(verbosity>0)
    {
      cout << "    L2AuAuElectronCandidate differences: " << endl;
      for(unsigned int i=0;i<electroncal->Candidate.size();i++)
	{
	  cout << "      Electron track = " << i << endl;
	  if(e[i].KilledByPC3 != ecal[i].KilledByPC3)
	    cout << "KilledByPC3: " << e[i].KilledByPC3 << " " 
		 << ecal[i].KilledByPC3 << " diff = " <<  e[i].KilledByPC3 - ecal[i].KilledByPC3 << endl; 
	  if(e[i].charge != ecal[i].charge)
	    cout << "      charge: " << e[i].charge << " " 
		 << ecal[i].charge << " diff = " << e[i].charge - ecal[i].charge << endl; 
	  if(e[i].theta0 != ecal[i].theta0)
	    cout << "      theta0: " << e[i].theta0 << " " 
		 << ecal[i].theta0 << " diff = " << e[i].theta0  - ecal[i].theta0 << endl; 
	  if(e[i].phi0 != ecal[i].phi0)
	    cout << "      phi0: " << e[i].phi0 << " " 
		 << ecal[i].phi0 <<  " diff = " << e[i].phi0  - ecal[i].phi0 << endl; 
	  if(e[i].ptot != ecal[i].ptot)
	    cout << "      ptot: " << e[i].ptot << " " 
		 << ecal[i].ptot <<  " diff = " << e[i].ptot  - ecal[i].ptot << endl; 
	  if(e[i].xrich != ecal[i].xrich)
	    cout << "      xrich: " << e[i].xrich << " " 
		 << ecal[i].xrich <<  " diff = " << e[i].xrich  - ecal[i].xrich << endl; 
	  if(e[i].yrich != ecal[i].yrich)
	    cout << "      yrich: " << e[i].yrich << " " 
		 << ecal[i].yrich <<  " diff = " << e[i].yrich  - ecal[i].yrich << endl; 
	  if(e[i].zrich != ecal[i].zrich)
	    cout << "      zrich: " << e[i].zrich << " " 
		 << ecal[i].zrich <<  " diff = " << e[i].zrich  - ecal[i].zrich << endl; 
	  if(e[i].pmtCent != ecal[i].pmtCent)
	    cout << "      pmtCent: " << e[i].pmtCent << " " 
		 << ecal[i].pmtCent <<  " diff = " << e[i].pmtCent  - ecal[i].pmtCent << endl; 
	  if(e[i].npmt != ecal[i].npmt)
	    cout << "      npmt: " << e[i].npmt << " " 
		 << ecal[i].npmt <<  " diff = " << e[i].npmt  - ecal[i].npmt << endl; 
	  if(e[i].npe != ecal[i].npe)
	    cout << "      npe: " << e[i].npe << " " 
		 << ecal[i].npe <<  " diff = " << e[i].npe  - ecal[i].npe << endl; 
	  if(e[i].TrkRingDist != ecal[i].TrkRingDist)
	    cout << "      TrkRingDist: " << e[i].TrkRingDist << " " 
		 << ecal[i].TrkRingDist <<  " diff = " << e[i].TrkRingDist  - ecal[i].TrkRingDist << endl; 
	  if(e[i].xemc != ecal[i].xemc)
	    cout << "      xemc: " << e[i].xemc << " " 
		 << ecal[i].xemc <<  " diff = " << e[i].xemc  - ecal[i].xemc << endl; 
	  if(e[i].yemc != ecal[i].yemc)
	    cout << "      yemc: " << e[i].yemc << " " 
		 << ecal[i].yemc <<  " diff = " << e[i].yemc  - ecal[i].yemc << endl; 
	  if(e[i].zemc != ecal[i].zemc)
	    cout << "      zemc: " << e[i].zemc << " " 
		 << ecal[i].zemc <<  " diff = " << e[i].zemc  - ecal[i].zemc << endl; 
	  if(e[i].energy != ecal[i].energy)
	    cout << "      energy: " << e[i].energy << " " 
		 << ecal[i].energy <<  " diff = " << e[i].energy  - ecal[i].energy << endl; 
	  if(e[i].xpc1 != ecal[i].xpc1)
	    cout << "      xpc1: " << e[i].xpc1 << " " 
		 << ecal[i].xpc1 <<  " diff = " << e[i].xpc1  - ecal[i].xpc1 << endl; 
	  if(e[i].ypc1 != ecal[i].ypc1)
	    cout << "      ypc1: " << e[i].ypc1 << " " 
		 << ecal[i].ypc1 <<  " diff = " << e[i].ypc1  - ecal[i].ypc1 << endl; 
	  if(e[i].armEMC != ecal[i].armEMC)
	    cout << "      armEMC: " << e[i].armEMC << " " 
		 << ecal[i].armEMC <<  " diff = " << e[i].armEMC  - ecal[i].armEMC << endl; 
	  if(e[i].sectorEMC != ecal[i].sectorEMC)
	    cout << "      sectorEMC: " << e[i].sectorEMC << " " 
		 << ecal[i].sectorEMC <<  " diff = " << e[i].sectorEMC  - ecal[i].sectorEMC << endl; 
	  if(e[i].ptotpc3 != ecal[i].ptotpc3)
	    cout << "      ptotpc3: " << e[i].ptotpc3 << " " 
		 << ecal[i].ptotpc3 <<  " diff = " << e[i].ptotpc3  - ecal[i].ptotpc3 << endl; 
	  if(e[i].xpc3 != ecal[i].xpc3)
	    cout << "      xpc3: " << e[i].xpc3 << " " 
		 << ecal[i].xpc3 <<  " diff = " << e[i].xpc3  - ecal[i].xpc3 << endl; 
	  if(e[i].ypc3 != ecal[i].ypc3)
	    cout << "      ypc3: " << e[i].ypc3 << " " 
		 << ecal[i].ypc3 <<  " diff = " << e[i].ypc3  - ecal[i].ypc3 << endl; 
	  if(e[i].zpc3 != ecal[i].zpc3)
	    cout << "      zpc3: " << e[i].zpc3 << " " 
		 << ecal[i].zpc3 <<  " diff = " << e[i].zpc3  - ecal[i].zpc3 << endl; 
	  if(e[i].theta0pc3 != ecal[i].theta0pc3)
	    cout << "      theta0pc3: " << e[i].theta0pc3 << " " 
		 << ecal[i].theta0pc3  <<  " diff = " << e[i].theta0pc3  - ecal[i].theta0pc3 << endl; 
	  if(e[i].phi0pc3 != ecal[i].phi0pc3)
	    cout << "      phi0pc3: " << e[i].phi0pc3 << " " 
		 << ecal[i].phi0pc3 <<  " diff = " << e[i].phi0pc3  - ecal[i].phi0pc3 << endl; 
	  if(e[i].dca != ecal[i].dca)
	    cout << "      dca: " << e[i].dca << " " 
		 << ecal[i].dca << " diff = " <<  e[i].dca  - ecal[i].dca << endl; 
	  if(e[i].alpha != ecal[i].alpha)
	    cout << "      alpha: " << e[i].alpha << " " 
		 << ecal[i].alpha <<  " diff = " << e[i].alpha  - ecal[i].alpha << endl; 
	  if(e[i].zvertex != ecal[i].zvertex)
	    cout << "      zvertex: " << e[i].zvertex << " " 
		 << ecal[i].zvertex <<  " diff = " << e[i].zvertex  - ecal[i].zvertex << endl; 
	}
    }
  return;
}

void Lvl2PrimitivesCompare::PrintL2AuAuElectronLowMassPairs(Lvl2OutArray *lvl2outarray,Lvl2OutArray * lvl2outarraycal) 
{
  
  struct  PairMassPT 
  {
    bool     KilledByChargeID;  //.. same sign pair
    bool     KilledByPC3;
    float    Mass;
    float    Pt;
    float    Phiv;
    int      candID0;  //sequence number of the 1st candidate
    int      candID1;  //sequence number of the 2nd candidate
  };
  
  PairMassPT pmass[100];
  PairMassPT pmasscal[100];

  // First the DAQ version
  
  lvl2outarray->GetPrimitive("L2AuAuElectronLowMassPairs");
  L2AuAuElectronLowMassPairsRBP pairmass(0);
  if (!pairmass.wasRead())
    {
      cout << "Error: Could not read L2AuAuElectronLowMassPairs from lvl2outarray" << endl;
      return;
    }  

  if(pairmass->ElectronPair.size() > 100)
    {
      cout << "WARNING: Exceeded array size in PrintL2AuAuElectronLowMassPairs: size = " 
	   << pairmass->ElectronPair.size() << " Return and do nothing" << endl;
      return;
    }

  for(unsigned int i=0;i<pairmass->ElectronPair.size();i++)
    {
      pmass[i].KilledByChargeID = pairmass->ElectronPair[i].KilledByChargeID;
      pmass[i].KilledByPC3 = pairmass->ElectronPair[i].KilledByPC3;
      pmass[i].Mass = pairmass->ElectronPair[i].Mass;
      pmass[i].Pt = pairmass->ElectronPair[i].Pt;
      pmass[i].Phiv = pairmass->ElectronPair[i].Phiv;
      pmass[i].candID0 = pairmass->ElectronPair[i].candID0;
      pmass[i].candID1 = pairmass->ElectronPair[i].candID1;
    }
  
  
  
  // Now offline
  
  lvl2outarraycal->GetPrimitive("L2AuAuElectronLowMassPairs");
  L2AuAuElectronLowMassPairsRBP pairmasscal(0);
  if (!pairmasscal.wasRead())
    {
      cout << "Error: Could not read L2AuAuElectronLowMassPairs from lvl2outarraycal" << endl;
      return;
    }  
  
  
  for(unsigned int i=0;i<pairmasscal->ElectronPair.size();i++)
    {
      pmasscal[i].KilledByChargeID = pairmasscal->ElectronPair[i].KilledByChargeID;
      pmasscal[i].KilledByPC3 = pairmasscal->ElectronPair[i].KilledByPC3;
      pmasscal[i].Mass = pairmasscal->ElectronPair[i].Mass;
      pmasscal[i].Pt = pairmasscal->ElectronPair[i].Pt;
      pmasscal[i].Phiv = pairmasscal->ElectronPair[i].Phiv;
      pmasscal[i].candID0 = pairmasscal->ElectronPair[i].candID0;
      pmasscal[i].candID1 = pairmasscal->ElectronPair[i].candID1;
    }
  
  if(verbosity>3)
    {
      cout << "L2AuAuElectronLowMassPairs from the DAQ: " << endl;
      for(unsigned int i=0;i<pairmass->ElectronPair.size();i++)
	{
	  cout << "DAQ Pair " << i << endl;
	  
	  cout << pmass[i].KilledByChargeID << endl;
	  cout << pmass[i].KilledByPC3 << endl;
	  cout << pmass[i].Mass << endl;
	  cout << pmass[i].Pt << endl;
	  cout << pmass[i].Phiv << endl;
	  cout << pmass[i].candID0 << endl;
	  cout << pmass[i].candID1 << endl;
	}
      
      
      cout << "L2AuAuElectronLowMassPairs from OFFLINE: " << endl;
      for(unsigned int i=0;i<pairmasscal->ElectronPair.size();i++)
	{
	  cout << "OFFLINE Pair " << i << endl;
	  
	  cout << pmasscal[i].KilledByChargeID << endl;
	  cout << pmasscal[i].KilledByPC3 << endl;
	  cout << pmasscal[i].Mass << endl;
	  cout << pmasscal[i].Pt << endl;
	  cout << pmasscal[i].Phiv << endl;
	  cout << pmasscal[i].candID0 << endl;
	  cout << pmasscal[i].candID1 << endl;
	}
    }
  
  cout << "    L2AuAuElectronLowMassPairs comparison of differences: " << endl;
  for(unsigned int i=0;i<pairmass->ElectronPair.size();i++)
    {
      cout << "      Pair " << i << endl;

      if(pmass[i].KilledByChargeID != pmasscal[i].KilledByChargeID)
	cout << "      KilledByChargeID: DAQ " << pmass[i].KilledByChargeID 
	     << " OFFLINE " << pmasscal[i].KilledByChargeID 
	     << " diff = " << pmass[i].KilledByChargeID  -  pmasscal[i].KilledByChargeID  << endl;
      if(pmass[i].KilledByPC3 != pmasscal[i].KilledByPC3)
	cout << "      KilledByPC3: DAQ " << pmass[i].KilledByPC3 
	     << " OFFLINE " << pmasscal[i].KilledByPC3 
	     << " diff = " << pmass[i].KilledByPC3  -  pmasscal[i].KilledByPC3  << endl;
      if(pmass[i].Mass != pmasscal[i].Mass)
	cout << "      Mass: DAQ " << pmass[i].Mass 
	     << " OFFLINE " << pmasscal[i].Mass 
	     << " diff = " << pmass[i].Mass  -  pmasscal[i].Mass << endl;
      if(pmass[i].Pt != pmasscal[i].Pt)
	cout << "      Pt: DAQ " << pmass[i].Pt 
	     << " OFFLINE " << pmasscal[i].Pt 
	     << " diff = " << pmass[i].Pt  -  pmasscal[i].Pt << endl;
      if(pmass[i].Phiv != pmasscal[i].Phiv)
	cout << "      Phiv: DAQ " << pmass[i].Phiv 
	     << " OFFLINE " << pmasscal[i].Phiv 
	     << " diff = " << pmass[i].Phiv  -  pmasscal[i].Phiv << endl;
      if(pmass[i].candID0 != pmasscal[i].candID0)
	cout << "      candID0: DAQ " << pmass[i].candID0 
	     << " OFFLINE " << pmasscal[i].candID0 
	     << " diff = " << pmass[i].candID0  -  pmasscal[i].candID0 << endl;
      if(pmass[i].candID1 != pmasscal[i].candID1)
	cout << "      candID1: DAQ " << pmass[i].candID1 
	     << " OFFLINE " << pmasscal[i].candID1 
	     << " diff = " << pmass[i].candID1  -  pmasscal[i].candID1 << endl;

    }

  return;
}
