
// $Id: PHGeant.hh,v 1.6 2005/11/16 19:09:03 hpereira Exp $
//! geant singleton instanciation/initialization
class PHGeant
{

  public:

  //! Pointer to the unique instance of the class.
  static PHGeant* Geant();  
  
  //! return true if geant has been already initialized
  static const bool& Initialized()
  { return _initialized; }
  
  //! do geant initialization
  static void Init( bool quiet = false );
  
  //! delete geant singleton
  static void Destroy();
  
  //! load pisafile
  static int ReadFile( int &lun, char *filename );

 	private:
	  
	//! constructor
	PHGeant() 
	{}    
  
	//! destructor
	~PHGeant()
	{}
			
	//! singleton
  static PHGeant* _instance; 

	//! true when class has been initialized
  static bool _initialized;
	
	//! if true, reading of the file is quiet
	static bool _quiet;
	
};
