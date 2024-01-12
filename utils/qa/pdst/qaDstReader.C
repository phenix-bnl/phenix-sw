/*
* qaDstReader.C
* $Id: qaDstReader.C,v 1.33 2005/12/13 09:21:43 hpereira Exp $
*
* Ed Jackson <ejackson@iastate.edu>
*
* See explanatory notes in qaDstReader.h
*/

#include "qaDstReader.h"
#include <PHIODataNode.h>
#include <TString.h>
#include "dEmcClusterLocalExtWrapper.h"
#include "emcClusterContainer.h"
#include "emcTowerContainer.h"

//-------------------------------------------------------------------------
// Constructor for qaDstReader object

qaDstReader::qaDstReader()
{

    _runheader           = NULL;
    _fclrawnorth              = NULL;
    _fclrawsouth              = NULL;
    _zdcout              = NULL;
    _bbcout              = NULL;
    _ntcout              = NULL;
    _ntcpout             = NULL;
		_tzrout              = NULL;
		_mvdvtx              = NULL;
		_mvddnde             = NULL;
    _dchhitlinetable     = NULL;
    _dchtrack            = NULL;
    _pc1cluster          = NULL;
    _pc2cluster          = NULL;
    _pc3cluster          = NULL;
    _tecout              = NULL;
    _cgltrack            = NULL;
    _cglparticle         = NULL;
    _tofreconstructed    = NULL;
    _phtrack             = NULL;
    _emcclusterlocalext  = NULL;
    _emcclustercontainer = NULL;
    _emctowercontainer   = NULL;
    _crkhit              = NULL;
		_muiroads            = NULL;
		_mutcathodeclusters  = NULL;
		_mutcalibcathodesout = NULL;
		_muotracks           = NULL;
		_muotracksout        = NULL;
		
    _triggerhelper       = NULL;

    _triglvl1            = NULL;
    _ertout              = NULL;
    _phcentraltrack      = NULL;
    
    mutrack_wrapper = -1;

    return;

} /* end of qaDstReader constructor */

int
qaDstReader::update(PHNodeIterator *ni, TString *optstr)
{

  bool errorflag = false;
  PHIODataNode<PHTable>* dTempNode;

  //===================Run Header

  PHIODataNode<PHObject>* RunHeaderNode =
    (PHIODataNode<PHObject>*)ni->findFirst("PHIODataNode", "RunHeader");

  _runheader = NULL;
  if (!RunHeaderNode)  //The data node wasn't found.
    {
			//      cerr << "Didn't find run header node!" << endl;
      _runheader = NULL;
      errorflag = true;
			if (optstr) optstr->ReplaceAll("M","");
    }
  else  //Got the node.
    {
      _runheader = (RunHeader*)RunHeaderNode->getData();
    }

  //===================FCAL input
  // Find the fcl data node.

  PHIODataNode<PHObject>* FclRawNorthNode =
    (PHIODataNode<PHObject>*)ni->findFirst("PHIODataNode", "fclRawNorth");
  PHIODataNode<PHObject>* FclRawSouthNode =
    (PHIODataNode<PHObject>*)ni->findFirst("PHIODataNode", "fclRawSouth");

  _fclrawnorth = NULL;
  _fclrawsouth = NULL;
  if (!FclRawNorthNode || !FclRawSouthNode)  //The data node wasn't found.
    {
      //      cerr << "Didn't find fcal out node" << endl;
      _fclrawnorth = NULL;
      _fclrawsouth = NULL;
      errorflag = true;
      if (optstr) optstr->ReplaceAll("L","");
    }
  else  //Got the node.
    {
      _fclrawnorth = (FclCompactRawv1*)FclRawNorthNode->getData();
      _fclrawsouth = (FclCompactRawv1*)FclRawSouthNode->getData();
    }

  //===================ZDC input
  // Find the zdc data node.

  PHIODataNode<PHObject>* ZdcOutNode =
    (PHIODataNode<PHObject>*)ni->findFirst("PHIODataNode", "ZdcOut");

  _zdcout = NULL;
  if (!ZdcOutNode)  //The data node wasn't found.
    {
			//      cerr << "Didn't find zdc out node" << endl;
      _zdcout = NULL;
      errorflag = true;
			if (optstr) optstr->ReplaceAll("Z","");
    }
  else  //Got the node.
    {
      _zdcout = (ZdcOut*)ZdcOutNode->getData();
    }

  //===================BBC input

  dTempNode = (PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", "BbcOut");

  _bbcout = NULL;
  if (!dTempNode)
    {
			//      cerr << "Didn't find the bbc out node!" << endl;
      _bbcout = NULL;
      errorflag = true;
			if (optstr) optstr->ReplaceAll("B","");
    }
  else
    {
      _bbcout = (BbcOut*)dTempNode->getData();
    }

  //===================NTC input
	
  _ntcout = NULL;
  if ( ( _triggerhelper != NULL ) &&   // NTC only present in pp dataset
       ( _triggerhelper->IsRunPP() ) ) 
    {
      dTempNode = (PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", 
                                                        "NtcOut");
      if (!dTempNode)
        {
					//         cerr << "Didn't find the ntc out node!" << endl;
          _ntcout = NULL;
          errorflag = true;
			if (optstr) optstr->ReplaceAll("N","");
        }
      else
        {
          _ntcout = (NtcOut*)dTempNode->getData();
        }
    }
  else
    {
      _ntcout = NULL;
    }

  //===================NTC' (Ntcp) input
	
  _ntcpout = NULL;
  if ( ( _triggerhelper != NULL ) ) 
    {
      dTempNode = (PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", 
                                                        "NtcpOut");
      if (!dTempNode)
        {
	  //         cerr << "Didn't find the ntcp out node!" << endl;
          _ntcpout = NULL;
          errorflag = true;
	  if (optstr) optstr->ReplaceAll("O","");
        }
      else
        {
          _ntcpout = (NtcpOut*)dTempNode->getData();
        }
    }
  else
    {
      _ntcpout = NULL;
    }

  //===================TZR input

  _tzrout = NULL;
  if ( ( _triggerhelper != NULL ) &&   // TZR only present in pp dataset
       ( _triggerhelper->IsRunPP() ) ) 
    {
      dTempNode = (PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", 
                                                        "TzrOut");
      if (!dTempNode)
        {
					//          cerr << "Didn't find the tzc out node!" << endl;
          _tzrout = NULL;
          errorflag = true;
					if (optstr) optstr->ReplaceAll("S","");
        }
      else
        {
          _tzrout = (TzrOut*)dTempNode->getData();
        }
    }
  else
    {
      _tzrout = NULL;
    }

  //===================MVD input

  _mvdvtx  = NULL;
  _mvddnde = NULL;
  if ( ( _triggerhelper != NULL ) &&     // in Run02, MVD only present 
       ( _triggerhelper->IsRunAuAu() ) ) // in the AuAu dataset 
    {
      dTempNode = (PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", 
                                                        "MvdVertexOut");
      if (!dTempNode)
        {
					//          cerr << "Didn't find the mvd vertex node!" << endl;
          _mvdvtx  = NULL;
          errorflag = true;
					if (optstr) optstr->ReplaceAll("V","");
        }
      else
        {
          _mvdvtx = (MvdVertexOut*)dTempNode->getData();
        }
      dTempNode = (PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", 
                                                        "MvddNdEtaOut");
      if (!dTempNode)
        {
					//          cerr << "Didn't find the mvd dn/deta node!" << endl;
          _mvddnde = NULL;
          errorflag = true;
        }
      else
        {
          _mvddnde = (MvddNdEtaOut*)dTempNode->getData();
        }
    }
  else
    {
      _mvdvtx  = NULL;
      _mvddnde = NULL;
    }
	
  //===================DCH input

  PHIODataNode<PHObject>* DchTrackNode =
    (PHIODataNode<PHObject>*)ni->findFirst("PHIODataNode", "DchTrack");

  _dchtrack = NULL;
  if (!DchTrackNode)  // The data node wasn't found.
    {
			//      cerr << "Didn't find dch track node" << endl;
      _dchtrack = NULL;
      errorflag = true;
			if (optstr) optstr->ReplaceAll("D","");
    }
  else  //Got the node.
    {
      _dchtrack = (DchTrack*)DchTrackNode->getData();
    }
  
  PHIODataNode<PHObject>* DchHitLineTableNode =
    (PHIODataNode<PHObject>*)ni->findFirst("PHIODataNode", "DchHitLineTable");

  _dchhitlinetable = NULL;
  if (!DchHitLineTableNode)  //The data node wasn't found.
    {
			if (optstr) optstr->ReplaceAll("D","");
			//      cerr << "Didn't find dch hitline node!" << endl;
      _dchhitlinetable = NULL;
      errorflag = true;
    }
  else  //Got the node.
    {
      _dchhitlinetable = (DchHitLineTablev2*)DchHitLineTableNode->getData();
    }
  
  //===================Pad Chamber input

  dTempNode =
    (PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", "dPc1Cluster");

  _pc1cluster = NULL;
  if (!dTempNode)
    {
			//      cerr << "Didn't find the PC1 cluster node!" << endl;
      _pc1cluster = NULL;
      errorflag = true;
			if (optstr) optstr->ReplaceAll("P","");
    }
  else
    {
      _pc1cluster = (dPadClusterWrapper*)dTempNode->getData();
    }

  dTempNode =
    (PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", "dPc2Cluster");

  _pc2cluster = NULL;
  if (!dTempNode)
    {
			//      cerr << "Didn't find the PC2 cluster node!" << endl; 
      _pc2cluster = NULL;
      errorflag = true;
    }
  else
    {
      _pc2cluster = (dPadClusterWrapper*)dTempNode->getData();
    }

  dTempNode =
    (PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", "dPc3Cluster");

  _pc3cluster = NULL;
  if (!dTempNode)
    {
			//     cerr << "Didn't find the PC3 cluster node!" << endl;
      _pc3cluster = NULL;
      errorflag = true;
    }
  else
    {
      _pc3cluster = (dPadClusterWrapper*)dTempNode->getData();
    }

  //=====================TEC input
  // Find the tec data node.

  PHIODataNode<PHObject>* TecOutNode =
    (PHIODataNode<PHObject>*)ni->findFirst("PHIODataNode", "TecOut");

  _tecout = NULL;
  if (!TecOutNode)  // The data node wasn't found.
    {
			//      cerr << "Didn't find tec out node! (could be old DST format)" << endl;
      _tecout = NULL;
      errorflag = true;
			if (optstr) optstr->ReplaceAll("T","");
    }
  else              // Got the node, so remember it.
    {
      _tecout = (TecOutV2*)TecOutNode->getData();
    }


  //====================CGL

  dCglTrackWrapper *cgltrkwrp = NULL;
  dTempNode = (PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", 
                                                    "dCglTrack");
  _cgltrack = NULL;
  if (!dTempNode) 
    {
      PHIODataNode<PHObject>* CglTrackNode = // first look for class ...
       (PHIODataNode<PHObject>*)ni->findFirst("PHIODataNode", "CglTrack");
      if (CglTrackNode)
        {
          _cgltrack = (CglTrack*) CglTrackNode->getData();
        }
      else
        {
					//          cerr << "Didn't find CGL track node." << endl << flush;
          _cgltrack = NULL;
          errorflag = true;
					if (optstr) optstr->ReplaceAll("E","");
					if (optstr) optstr->ReplaceAll("T","");
					if (optstr) optstr->ReplaceAll("F","");
					if (optstr) optstr->ReplaceAll("S","");
        }
    }
  else
    {                                        // no class, so we got the table
      cgltrkwrp = (dCglTrackWrapper*) dTempNode->getData(); 
      if (_cgltrack == NULL)
	{
	  _cgltrack = new CglTrackv2();
	}
      _cgltrack->FillFromWrapper(cgltrkwrp);
    }

  dTempNode =
    (PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", "dCglParticle");

  _cglparticle = NULL;
  if (!dTempNode)
    {
			//      cerr << "Didn't find CGL particle node." << endl;
      _cglparticle = NULL;
      errorflag = true;
    }
  else
    {
      _cglparticle = (dCglParticleWrapper*)dTempNode->getData();
    }

  //====================TOF

  dTempNode =
    (PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", "dTofReconstructed");

  _tofreconstructed = NULL;
  if (!dTempNode)
    {
			//      cerr << "Didn't find TOF Reconstructed node." << endl;
      _tofreconstructed = NULL;
      errorflag = true;
			if (optstr) optstr->ReplaceAll("F","");
    }
  else
    {
      _tofreconstructed = (dTofReconstructedWrapper*)dTempNode->getData();
    }

  //=====================PHTrack

  PHIODataNode<PHObject>* PHTrackOutNode =
    (PHIODataNode<PHObject>*)ni->findFirst("PHIODataNode", "PHTrackOut");

  _phtrack = NULL;
  if (!PHTrackOutNode)  //The data node wasn't found.
    {
			//      cerr << "Didn't find phtrack out node! (could be old DST format)" 
			//           << endl;
      _phtrack = NULL;
      errorflag = true;
			if (optstr) optstr->ReplaceAll("E","");
			if (optstr) optstr->ReplaceAll("T","");
			if (optstr) optstr->ReplaceAll("F","");
			if (optstr) optstr->ReplaceAll("S","");
    }
  else  //Got the node.
    {
      _phtrack = (PHTrackOut*)PHTrackOutNode->getData();
    }

  //===================EMC

  PHIODataNode<emcClusterContainer>* emcClusterNode = 
    (PHIODataNode<emcClusterContainer>*)ni->findFirst
    ("PHIODataNode","emcClusterContainer");
  
  _emcclusterlocalext  = NULL;
  _emcclustercontainer = NULL;

  if ( !emcClusterNode )
    {
      // Search old objects.
      dTempNode =
	(PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", 
					      "dEmcClusterLocalExt");

      if (!dTempNode)
	{
		//	  cerr << "Didn't find any of EmcClusterLocalExt or "
		//	       << "emcClusterContainer nodes." << endl;
	  errorflag = true;
		if (optstr) optstr->ReplaceAll("E","");
	}
      else
	{
	  _emcclusterlocalext = (dEmcClusterLocalExtWrapper*)
	    dTempNode->getData();
	}
    }
  else
    {
     _emcclustercontainer = emcClusterNode->getData();
    }
  
  PHIODataNode<emcTowerContainer>* emcTowerNode =
    (PHIODataNode<emcTowerContainer>*)ni->findFirst
    ("PHIODataNode","emcTowerContainer");

  _emctowercontainer   = NULL;

  if( emcTowerNode )
    {
      _emctowercontainer = emcTowerNode->getData();
    }
  
  //===================CRK (RICH)

  dTempNode =
    (PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", "dCrkHit");

  _crkhit = NULL;
  if (!dTempNode)
    {
			//      cerr << "Didn't find dCrkHit node." << endl;
      _crkhit = NULL;
      errorflag = true;
			if (optstr) optstr->ReplaceAll("C","");
    }
  else
    {
      _crkhit = (dCrkHitWrapper*)dTempNode->getData();
    }
		
	//===================MUO
	dTempNode =
		(PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", "dMuoTracksOut");
	
	_muotracks    = NULL;
	_muotracksout = NULL;
	mutrack_wrapper = -1;
	if (!dTempNode)
		{
			dTempNode =
				(PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", "dMuoTracks");
			if (!dTempNode)
				{
					//					cerr << "Didn't find MuoTracks node." << endl;
					_muotracks    = NULL;
					_muotracksout = NULL;
					mutrack_wrapper = -1;
					errorflag = true;
					if (optstr) optstr->ReplaceAll("U","");
				}
			else
				{
					_muotracks      = (dMuoTracksWrapper*)dTempNode->getData();
					_muotracksout   = NULL;
					mutrack_wrapper = 1;
				}
		}
	else
		{
			_muotracks      = NULL;
			_muotracksout   = (dMuoTracksOut*)dTempNode->getData();
			mutrack_wrapper = 0;
		}
  */
  /*
		dTempNode =
		(PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode","dMuoTracks");
   
		if (!dTempNode)
		{
//		cerr << "Didn't find MuoTracks node." << endl;
		errorflag = true;
		}
		else
		_muotracks = (dMuoTracksWrapper*)dTempNode->getData();
  */

  //===================TRIGGER (LOCAL) LEVEL 1

  dTempNode =
    (PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", "TrigLvl1");

  _triglvl1 = NULL;
  if (!dTempNode)
    {
			//      cerr << "Didn't find TrigLvl1 node." << endl;
      _triglvl1 = NULL;
      errorflag = true;
			if (optstr) optstr->ReplaceAll("R","");
    }
  else
    {
      _triglvl1 = (TrigLvl1*)dTempNode->getData();
    }

  //===================ERT TRIGGER

  _ertout = NULL;
  if ( _triggerhelper != NULL )
    {
      dTempNode = (PHIODataNode<PHTable>*)ni->findFirst("PHIODataNode", 
                                                         "ErtOut");
      if (!dTempNode)
        {
					//         cerr << "Didn't find ErtOut node." << endl;
          _ertout   = NULL;
          errorflag = true;
					if (optstr) optstr->ReplaceAll("R","");
        }
      else
        {
          _ertout = (ErtOut*)dTempNode->getData();
        }
    }
  else
    {
      _ertout = NULL;
    }
  
  //===================CNT
  
  PHIODataNode<PHObject>* PHCentralTrackNode =
	      (PHIODataNode<PHObject>*)ni->findFirst("PHIODataNode", "PHCentralTrack");

    _phcentraltrack = NULL;
      if (!PHCentralTrackNode) 
 	{
	  PHCentralTrackNode =
	           (PHIODataNode<PHObject>*)ni->findFirst("PHIODataNode", "CNTCentralTrack"); // used in CNT files
	}
			if (!PHCentralTrackNode)
				{
					PHCentralTrackNode =
						(PHIODataNode<PHObject>*)ni->findFirst("PHIODataNode", "EWGCentralTrack"); // used in EWG files
				}
      if (!PHCentralTrackNode)
      {
				//       cerr << "Didn't find phcentraltrack node! (could be old DST format)" << endl;
	  _phcentraltrack = NULL;
	  errorflag = true;
	  if (optstr) optstr->ReplaceAll("electron","");
	}
       else  //Got the node.
        {
          _phcentraltrack = (PHCentralTrack*)PHCentralTrackNode->getData();
        }
  
//===================All done....

  if (errorflag)
    {
			//      cerr << "Error updating node information!" << endl;
      return 1;    // some of the nodes was not found
    }

  return 0;	//success

}

// EOF
