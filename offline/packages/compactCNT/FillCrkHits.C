#include "FillCrkHits.h"
#include "setIntflag.h"

#include <T0Out.h>
#include <dCrkHitWrapper.h>
#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>
#include <id_detector.h>
#include <TrackLineProjectionMap.h>
#include <TrackLineProjectionMapEntry.h>
#include <PHLine.h>
#include <PHPoint.h>
#include <CrkPID.hh>
#include <CglTrack.h>
#include <CrkProjv1.h>
#include <Fun4AllReturnCodes.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <RunHeader.h>
#include <getClass.h>

#include <half/half.h>
#include <useInt.h>

#include <fstream>
#include <set>
#include <sstream>

using namespace std;

// Uncomment this to store all RICH hits
// Otherwise only RICH hits associated with a track are kept
// ADF: October 4, 2012, changed this to keep all RICH hits by default
//      My tests showed that the increase in file size is ~ 0.5%
#define USEALLCRKHITS

union floatint
{
  float    f32;
  int      i32;
};

FillCrkHits::FillCrkHits(const std::string &name): 
  SubsysReco(name),
  crkpid(NULL)
{
#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/fillcrkhits.dump");
#endif

  return;
}

FillCrkHits::~FillCrkHits()
{
  delete crkpid;
  return;
}

int
FillCrkHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

  ostringstream tmpstream;

#ifdef useIntflag
  VariableArrayInt *crkhit = new VariableArrayInt(6000);
#else
  VariableArray *crkhit = new VariableArray(6000);
#endif

  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(crkhit, "CrkHit_VarArray", "PHObject");
  dstNode->addNode(PHObjectIONode);

  RunHeader *d_runhdr = findNode::getClass<RunHeader>(topNode,"RunHeader");
  int runnumber = d_runhdr->get_RunNumber();
  cout << "FillCrkhits using Run number " << runnumber << endl;

  // Get an instance of CrkPID to do the track association
  crkpid = new CrkPID(runnumber);

  return EVENT_OK;
}

int
FillCrkHits::process_event(PHCompositeNode *topNode)
{
  // Created in InitRun

#ifdef useIntflag
  VariableArrayInt *crkarray;
  crkarray = findNode::getClass<VariableArrayInt>(topNode, "CrkHit_VarArray");
  vector<int> savethis;
#else
  VariableArray *crkarray;
  crkarray = findNode::getClass<VariableArray>(topNode, "CrkHit_VarArray");
  vector<short int> savethis;
#endif

  if (!crkarray)
    return 0;

  // Calibrated RICH hits
  dCrkHitWrapper *d_crkw = findNode::getClass<dCrkHitWrapper>(topNode,"dCrkHit");
  if (!d_crkw)
    return 0;

  //-----------------------------------------------
  // Get a pointer to the RICH hits
  // and the number of entries
  //-----------------------------------------------

  int npmt = d_crkw->RowCount();
  DCRKHIT_ST *crk = d_crkw->TableData();

#ifdef USEALLCRKHITS

  //----------------------------
  // This saves all RICH hits
  // It is intended for testing
  //----------------------------
  for (int ih = 0; ih < npmt; ih ++) 
    {
      DCRKHIT_ST *chit = crk + ih;
      int pmtid  = chit->pmt;
      float npe  = chit->npe;
      float time = chit->time;

#ifdef DUMP
      dumpfile << "CrkHitMapEntry: id: " << ih;
#endif
      savethis.push_back(ih);
      savethis.push_back( pmtid );
      
      //fi.f32 = npe;
      //savethis.push_back(fi.i32);
      //fi.f32 = time;
      //savethis.push_back(fi.i32);

      savethis.push_back(FloatToInt(npe));
      savethis.push_back(FloatToInt(time));

#ifdef DUMP
      dumpfile << ", pmtid: " << pmtid 
	       << ", npe: " << npe
	       << ", time: " << time
	       << endl;
#endif
    }
  crkarray->set_val(savethis);

#else

  //-----------------------------------------------
  // This saves only those RICH hits that are 
  // associated with a track - this is the one to use
  //-----------------------------------------------

  int pmt_found[5120]={5120 * 0};

  int isave=0;

  // We need to initialize CrkPID with the BBC time zero
  T0Out *t0 = findNode::getClass<T0Out>(topNode, "T0Out");
  float TimeZero = t0->get_T0();
#ifdef DUMP
  dumpfile << "Time zero = " << TimeZero << endl;
#endif

  crkpid->SetBbcT0(TimeZero);

  // We will look in Cgltrack for real track associations, CglTrackBack for swapped ones
  static const string cglnodename[2] = {"CglTrack", "CglTrackBack"};

  // The tracks that were associated with the RICH have entries in CrkProj and CrkProjBG
  // We will need those to re-find the RICH hits associated with tracks
  static const string projnodename[2] = {"CrkProj", "CrkProjBG"};

  for (int k = 0; k < 2; k++)
    {
      CglTrack *cgltrk = findNode::getClass<CglTrack>(topNode, cglnodename[k].c_str());
      if(!cgltrk)
	{
	  cout << PHWHERE << cglnodename[k].c_str() << " not found! return and do nothing" << endl;
	  return EVENT_OK;
	}
      CrkProj *crkproj = findNode::getClass<CrkProj>(topNode, projnodename[k].c_str());
      if (!crkproj)
	{
	  cout << PHWHERE << projnodename[k].c_str() <<  " not found, return and do nothing" << endl;
	  return EVENT_OK;
	}

      for (unsigned int i = 0; i < cgltrk->get_CglNTrack(); i++)
	{
	  if(cgltrk->get_richringid(i) >= 0)
	    {
	      for(unsigned int j = 0; j < crkproj->get_CrkNProj(); j++)
		{
		  if (crkproj->get_cgltrackid(j) == (int) i)
		    {
		      // This track is associated with this RICH ring

		      PHPoint pstart(crkproj->get_pstartx(j), crkproj->get_pstarty(j), crkproj->get_pstartz(j));
		      PHPoint pend(crkproj->get_pendx(j), crkproj->get_pendy(j), crkproj->get_pendz(j));

#ifdef DUMP
		      dumpfile << "Track " <<  i
			       << " k " << k
			       << " crkid " << cgltrk->get_richringid(i)
			       << " line " << pstart.getX()
			       << " " << pstart.getY()
			       << " " << pstart.getZ()
			       << " " << pend.getX()
			       << " " << pend.getY()
			       << " " << pend.getZ()
			       << endl;
#endif
		      
		      PHLine track(pstart,pend);
		      track.normalize();
		      if(k==1)
			{
			  // association was with the swapped track, flip and slide it
			  track = ReflectInZ(track);
			}
		      
		      CrkPIDout result;
		      
		      if( crkpid->Associate(track, npmt, crk, &result) )
			{
			  int npmt1 = result.npmt1;

			  // npmt1 can in principle be greater than result.MAXPMTHIT, but "result" is truncated 
			  // and dimensioned only to MAXPMTHIT!
			  // This should be VERY rare. Prevent a segfault and announce what happened.

			  if(npmt1 > result.MAXPMTHIT)
			    {
			      cout << PHWHERE << "Found npmt1 = " << npmt1 << " , truncating Crk hit list to " 
				   << result.MAXPMTHIT
				   << endl;

			      npmt1=result.MAXPMTHIT;
			    }

			  // successful track association, capture the associated hits
			  for(int ir=0;ir<npmt1;ir++)
			    {
			      int pmtid  = result.pmt[ir]->pmt;
			      
#ifdef DUMP
			      dumpfile << ", pmtid: " << pmtid 
				       << ", pmt_found: " << pmt_found[pmtid]
				       << endl;
#endif
			      if(pmtid < 0 || pmtid > 5120)
				{
				  cout << PHWHERE << "Impossible RICH PMT number, quit!" << endl;
				  exit(1);
				}
			      
			      // Make sure we do not count PMT's more than once!
			      // The track association does NOT check for this.			      
			      if (pmt_found[pmtid] == 0)
				{
				  // This is a new hit
				  pmt_found[pmtid] = 1;
				}
			      else
				{				
				  // This hit was found already, skip it
				  continue;
				}

			      float npe  = result.pmt[ir]->npe;
			      float time = result.pmt[ir]->time;
#ifdef DUMP			      
			      dumpfile << "CrkHitMapEntry: id: " << isave;
#endif 
			      savethis.push_back(isave);
			      savethis.push_back( pmtid );
			      
			      savethis.push_back(FloatToInt(npe));
			      savethis.push_back(FloatToInt(time));

#ifdef DUMP			      
			      dumpfile << ", pmtid: " << pmtid 
				       << ", npe: " << npe
				       << ", time: " << time
				       << endl;
#endif
			      isave++;
			    }
			}
		    }

		}
	    }
	}
    }

  crkarray->set_val(savethis);

#endif

  return EVENT_OK;
}

int
FillCrkHits::End(PHCompositeNode *topNode)
{
#ifdef DUMP
  dumpfile.close();
#endif

  return 0;
}

PHLine
FillCrkHits::ReflectInZ(const PHLine &trk)
{
  PHPoint base_zref = trk.getBasepoint();
  PHVector dir_zref = trk.getDirection();
  base_zref.setZ( -1.0*base_zref.getZ());
  dir_zref.setZ( -1.0*dir_zref.getZ());
  return PHLine(base_zref, dir_zref);
}


#ifdef useIntflag
int
FillCrkHits::FloatToInt(const float rval) const
{
  floatint fi;
  fi.f32 = rval;
  return fi.i32;
}
#else
short int
FillCrkHits::FloatToInt(const float rval) const
{
  half ftoi(rval);
  return ftoi.bits();
}
#endif

