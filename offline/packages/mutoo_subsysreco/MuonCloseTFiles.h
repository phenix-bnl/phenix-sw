// $Id: MuonCloseTFiles.h,v 1.1 2006/05/16 21:23:28 hpereira Exp $
#ifndef MuonCloseTFiles_h
#define MuonCloseTFiles_h

#include <SubsysReco.h>

/*!
	\file MuonCloseTFiles.h
	\ingroup supermodules 
	\brief closes PHTFileManager in End() method
	\author H. Pereira
	\version $Revision: 1.1 $
	\date $Date: 2006/05/16 21:23:28 $
*/

// Forward declerations
class PHCompositeNode;

/*!
	\class MuonCloseTFiles
	\ingroup supermodules 
	\brief reads MC and reconstructed muid maps, fills efficiency ntuples
*/
class MuonCloseTFiles: public SubsysReco
{
 public:

	//! constructor
	MuonCloseTFiles( const char* name= "MuonCloseTFiles" );
 
	//! destructor
	virtual ~MuonCloseTFiles() 
	{}
	
	//! event method
	int process_event(PHCompositeNode *topNode)
	{ return 0; }
	
	//! init method (begin of process)
	int Init(PHCompositeNode *topNode)
	{ return 0; }
	
	//! end of process method
	int End(PHCompositeNode *topNode);
	
};

#endif /* __MuonCloseTFiles_H__ */







