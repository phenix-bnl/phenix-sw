//INCLUDECHECKER: Removed this line: #include <iomanip>
#include <iostream>

//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"

#include "TMuiAddressTable.hh"
#include "mMuiRawUnpack.h"

#include <MUIOO.h>
#include <TMuiHitMapO.h>
#include <Event.h>

using namespace std;

//_______________________________________________________
mMuiRawUnpack::mMuiRawUnpack():
	_timer( PHTimeServer::get()->insert_new( "mMuiRawUnpack" ) )
 {}

//_______________________________________________________
PHBoolean mMuiRawUnpack::event(PHCompositeNode* top_node)
{
	
	_timer.get()->restart();
	
	// output boolean
	bool out( false );
	
	// Find the prdf node.
	try{
		
		set_interface_ptrs(top_node);
		_hit_map->clear();
		
		out = event( _event, top_node);
		
		if(_mod_par->get_verbosity() >= MUIOO::ALOT) _hit_map->print();	
	
	} catch(std::exception& e){
	
		MUIOO::TRACE(e.what());
		return False;
	
	}

	_timer.get()->stop();
	if(_mod_par->get_verbosity() >= MUIOO::SOME) _timer.get()->print();
	
	return out;
}

//_______________________________________________________
PHBoolean mMuiRawUnpack::event(Event* evt, PHCompositeNode* top_node)
{
	
	TMuiAddressTable* pAddress = TMuiAddressTable::Table();
	if (!pAddress) return False;

	int packetdata[MUIOO::kWordsPerFEM]={0};
	
	unsigned int row = 0;
	//static const unsigned long dcm_words_max = 136;	// size of DCM array
	// Assign module ID to associated packet ID, then get Module ID
	// according to current packet ID instead extract it from
	// DCM word. Warning!!!! If assigned packet ID for MuID DCMs are
	// changed, we need to modifiy assignment in par file.

	for (int dcm=0; dcm<MUIOO::kFEMsTotal; dcm++) {

		unsigned int packetid = _mod_par->get_id_base() + dcm;
		Packet* muipacket = evt->getPacket(packetid);
		int numwords = 0;	//Number of words actually returned by packet accessor
		
		//All muid packets are not necessarily present
		if(!muipacket) continue;
		
		TMuiReadoutID idHard;
		TMuiChannelId idSoft;

		unsigned int moduleID = _mod_par->get_module_id(dcm);
		
		short Arm = pAddress->Arm(moduleID);
		short orient = pAddress->Orient(moduleID);

		muipacket->fillIntArray(packetdata,
			MUIOO::kWordsPerFEM,
			&numwords,
			"SPARSE");
		delete muipacket;
		
		// Sanity check of the word count ...
		if (numwords > MUIOO::kWordsPerFEM) {
			ostringstream what;
			what << packetid
				<< " has " << numwords
				<< " words, maximum allowed is " << MUIOO::kWordsPerFEM;
				
			throw runtime_error( DESCRIPTION( what.str() ) );
		}

		for (unsigned int j=0; j<(unsigned int)numwords; j++) {
			unsigned int pattern = packetdata[j];

			short roc	= getbits(pattern,30,7);
			short word = getbits(pattern,23,4);

			if ( (roc < 0) || (roc >= TMuiReadoutID::kROCsPerFEM)
				|| (word < 0) || (word >= TMuiReadoutID::kWordsPerROC) ) {
				cout << "mMuiDCMUnpack-E1	ROC id " << roc
					<< " or word-in-ROC id " << word << " out of range" << endl;
			} else {

				for (short bit=0; bit<16; bit++) {
					if (getbits(pattern,bit,1) == 1) {
						idHard.Set(moduleID,roc,word,bit);
						idSoft = pAddress->SoftwareAddress(idHard);

						if ( (idSoft.Arm() != Arm) || (idSoft.Orient() != orient) ) {
							cout << "mMuiDCMUnpack-E3	DCM arm, orient = "
				 				<< Arm << "	" << orient
				 				<< "	Lookup arm, orient = "
				 				<< idSoft.Arm() << "	" << idSoft.Orient() << endl;
						} else {
#ifdef MUIOO_DEBUG								
							TMuiHitMapO::iterator hitIter =
#endif
							_hit_map->insert_new(
								idSoft.Arm(),
								idSoft.Plane(),
								idSoft.Panel(),
								idSoft.Orient(),
								idSoft.TwoPack()
								);
#ifdef MUIOO_DEBUG
							hitIter->get()->print();
#endif

							row++;
						}
					}

				} // for (bit=0 ...)
			} // if roc and word in range
      	} // for (j=0 ...)
	} // for (dcm=0 ...)

	return True;
}

//_____________________________________________________
/*! Reset IOC and external interface pointers */
void mMuiRawUnpack::set_interface_ptrs(PHCompositeNode* top_node)
{	

	// module parameters
	_mod_par = TMutNode<mMuiRawUnpackPar>::find_node(top_node,"mMuiRawUnpackPar");	

	// hit map pointer
	_hit_map = TMutNode<TMuiHitMapO>::find_node(top_node,"TMuiHitMapO");	

	// Event pointer
	_event = TMutNode<Event>::find_node(top_node,"PRDF");

} 
