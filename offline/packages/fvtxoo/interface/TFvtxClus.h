// $Id: TFvtxClus.h,v 1.5 2011/12/01 04:16:20 slash Exp $
#ifndef TFvtxClus_h
#define TFvtxClus_h


/*!
	 \file		TFvtxClus.h
	 \brief	 cluster interface object
	 \author	H. Pereira
	 \version $Revision: 1.5 $
	 \date		$Date: 2011/12/01 04:16:20 $
*/

#include <PHKey.hh>

//! cluster interface object
class TFvtxClus: public PHKey {
	
	public:

	//! Default constructor 
	TFvtxClus()
	{}
	
	//! Construct with key 
	TFvtxClus(const Key& key) : PHKey(key) 
	{}
	
	//! Destructor 
	virtual ~TFvtxClus() 
	{}
	
	//@}
	
	//! @name Functional Interface
	//@{		

	//! number of associated strips
	virtual unsigned short	get_n_strip() const;

	//@}

	//! @name Locators
	//@{	
	//! Get arm number for this hit
	virtual unsigned short get_arm() const 
	{return 0;}

        //! Get cage number for this hit
        virtual unsigned short get_cage() const
        {return 0;}
	
	//! Get station number for this hit
	virtual unsigned short get_station() const 
	{return 0;}
	
	//! Get sector number for this hit
	virtual unsigned short get_sector() const 
	{return 0;}
	
	//! Get column number for this hit
	virtual unsigned short get_column() const
	{ return 0; }

	//! index
	virtual unsigned short get_index() const
	{ return 0; }
		
	//! Set arm number for this hit
	virtual void set_arm(const unsigned short& ) 
	{}

        //! Set cage number for this hit
        virtual void set_cage(const unsigned short& )
        {}
	
	//! Set station number for this hit
	virtual void set_station(const unsigned short&) 
	{}
	
	//! Set sector number for this hit
	virtual void set_sector(const unsigned short&) 
	{}
	
	//! set column number for this hit
	virtual void set_column( const unsigned short& )
	{}

	//! set column number for this hit
	virtual void set_index( const unsigned short& )
	{}
		
	//@}

	//! @name Cluster Status
	//@{	

	//! Get the status word 
	virtual unsigned long get_status() const 
	{ return 0;}

	//! Get the status word 
	virtual void set_status( const unsigned long& )
	{ return;}
	
	//! Clear the status word 
	virtual void clear_status() 
	{ set_status(0); }
	
	//@}
	
	//! @name Dumpers
	//@{		
	
	//! Print cluster contents 
	virtual void print(std::ostream& os = std::cout) const 
	{}
	
	//@}

	ClassDef(TFvtxClus,1)
};


#endif
