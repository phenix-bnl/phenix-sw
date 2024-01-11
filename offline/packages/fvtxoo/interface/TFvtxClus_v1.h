// $Id: TFvtxClus_v1.h,v 1.6 2011/12/01 04:16:20 slash Exp $
#ifndef TFvtxClus_v1_h
#define TFvtxClus_v1_h


/*!
	 \file TFvtxClus_v1.h
	 \brief cluster interface object
	 \author H. Pereira
	 \version $Revision: 1.6 $
	 \date $Date: 2011/12/01 04:16:20 $
*/

#include <TFvtxClus.h>
#include <PHKey.hh>
#include <map>

//! cluster interface object
class TFvtxClus_v1: public TFvtxClus 
{
	public:

	//! Default constructor 
	TFvtxClus_v1();
	
	//! Construct with key
	TFvtxClus_v1(const Key& key,
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector,
		const unsigned short& column,
		const unsigned short& index );
	
	//! Construct from base class
	TFvtxClus_v1(const TFvtxClus& base_ref );
	
	//! Construct from base class
	TFvtxClus_v1(const TFvtxClus* base_pointer );
		
	//! Destructor 
	virtual ~TFvtxClus_v1() 
	{}
	
	//@}

	//! @name Locators
	//@{	
	//! Get arm number for this hit
	virtual unsigned short get_arm() const 
	{return _arm;}

        //! Get cage number for this hit
        virtual unsigned short get_cage() const
        {return _cage;}
	
	//! Get station number for this hit
	virtual unsigned short get_station() const 
	{return _station;}
	
	//! Get sector number for this hit
	virtual unsigned short get_sector() const 
	{return _sector;}
	
	//! Get column number for this hit
	virtual unsigned short get_column() const
	{ return _column; }

	//! index
	virtual unsigned short get_index() const
	{ return _index; }
		
	//! Set arm number for this hit
	virtual void set_arm(const unsigned short& arm ) 
	{ _arm = arm; }
	
        //! Set cage number for this hit
        virtual void set_cage(const unsigned short& cage )
        { _cage = cage; }

	//! Set station number for this hit
	virtual void set_station(const unsigned short& station ) 
	{ _station = station; }
	
	//! Set sector number for this hit
	virtual void set_sector(const unsigned short& sector ) 
	{ _sector = sector; }
	
	//! set column number for this hit
	virtual void set_column( const unsigned short& column )
	{ _column = column; }

	//! set column number for this hit
	virtual void set_index( const unsigned short& index )
	{ _index = index; }
		
	//@}

	//! @name Cluster Status
	//@{	

	//! Get the status word 
	virtual unsigned long get_status() const 
	{ return _status;}

	//! Set the status word 
	virtual void set_status( const unsigned long& status ) 
	{ _status = status;}
		
	//@}
	
	//! @name Dumpers
	//@{		
	
	//! Print cluster contents 
	virtual void print(std::ostream& os = std::cout) const;
	
	//@}
	
	private:
	
	//! arm index
	unsigned short _arm;

        //! cage index
        unsigned short _cage;
	
	//! station index
	unsigned short _station;
	
	//! sector index
	unsigned short _sector;
	
	//! column index
	unsigned short _column;
	
	//! strip index
	unsigned short _index;
	
	//! status word
	unsigned long _status;
	
	ClassDef(TFvtxClus_v1,1)
};


#endif
