// $Id: mMuiBLTEmulator.cxx,v 1.8 2017/07/11 16:19:04 phnxbld Exp $

/*!
  \file mMuiBLTEmulator.cxx
  \brief muid Blue Logic Trigger Emulator
  \author chun zhang
  \version $Revision: 1.8 $
  \date $Date: 2017/07/11 16:19:04 $
*/

// MUIOO/MUTOO headers
#include "mMuiBLTEmulator.h"
#include <TMuiMCHitMapO.h>

// PHENIX db headers.
#include <RunToTime.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbMuiTriggerMLU.hh>
#include <PdbCalBank.hh>

// MUID Geometry header.
#include <TMuiAddressTable.hh>

/*! \ingroup modules */
// STL/BOOST
#include <iostream>
#include <string>
#include <boost/array.hpp>

using namespace std;

//_________________________________________________________________
mMuiBLTEmulator::mMuiBLTEmulator():
  _timer( PHTimeServer::get()->insert_new( "mMuiBLTEmulator") )
{
  MUIOO::TRACE("initializing module mMuiBLTEmulator");
  _non_used_plane = 4;

  // initialize mlu_data arrays (for valgrind to be happy)
  mlu_data_d.assign(0);
  mlu_data_s.assign(0);

}

//_________________________________________________________________
mMuiBLTEmulator::~mMuiBLTEmulator() 
{}

//_________________________________________________________________
PHBoolean mMuiBLTEmulator::event(PHCompositeNode* top_node)
{

  _timer.get()->restart();

  try {

    // Reset IOC pointers
    set_interface_ptrs(top_node);
    _blt_map->clear();

    // collects all the raw hits/hits information.
    if(_mod_par->get_mode()==mMuiBLTEmulatorPar::FROMPRDF) get_raw_data();
    if(_mod_par->get_mode()==mMuiBLTEmulatorPar::FROMDST) get_hits();

    // transfer the hit to trigger primary format.
    raw_to_trigpattern();

    // Fill trigger decisions into pseudo-trigger map.
    fill_map();

    // check blt emualtor according to the hit information.
    if(_mod_par->get_debug_flag()==1) check_blt();

    // dump maps
    if(_mod_par->get_verbosity() >= MUIOO::MAX ) _blt_map->print();

  } catch(std::exception& e) {
    MUIOO::TRACE(e.what());
    return False;
  }

  // Timer
  _timer.get()->stop();
  return True;
}

//_________________________________________________________________
void mMuiBLTEmulator::get_raw_data()
{

  // get raw data from event_node. then store it into
  // arry word[arm][orientation][iword].
  // Tatally there are 120 words per orientation, and the low 16 bits
  // in each word represents 16 twopacks.
  Packet* p;
  static const int id_base[max_arm] = {12001, 12003}; // for South and North

  for(int iarm = 0; iarm < MUIOO::NumberOfArms; iarm++) {
    for(int iorient = 0; iorient < MUIOO::MAX_ORIENTATION; iorient++) {
      int pack_id = id_base[iarm] + iorient;
      if((p=_event->getPacket(pack_id))!=0) {
        for(int iword = 0; iword < MUIOO::kWordsPerFEM; iword++) 
        { word[iarm][iorient][iword] = p->iValue(iword); }
        delete p;
      }
    }
  }
}

//_________________________________________________________________
void mMuiBLTEmulator::get_hits()
{
  // Loop through mui hit map, fill word array
  // muioo hit is organized by (arm, plane, panel, orientation, twopack),
  // but word array is organized by FEM address and every word uses its
  // low 16 bits to store 16 twopacks, we need to map muioo hit into
  // low 16 bits of each word in word array. This transform is taken care by
  // TMuiChannelId and TMuiAddressTable::Table()->HardwareAddress(TMuiChannelId).
  // Here I just call them.

  // clear word array for each event.
  //
  for(int iarm = 0; iarm < MUIOO::NumberOfArms; iarm++){
    for(int iorient = 0; iorient < MUIOO::MAX_ORIENTATION; iorient++) {
      for(int iword = 0; iword < MUIOO::kWordsPerFEM; iword++) 
      { word[iarm][iorient][iword] = 0; }
    }
  }

  TMuiHitMapO::const_iterator hit_iter = _hit_map->range();
  while(TMuiHitMapO::const_pointer hit_ptr = hit_iter.next())
  {
    EOrient_t orientation = kVERT;
    if(hit_ptr->get()->get_orientation()==0) orientation = kHORIZ;

    TMuiChannelId muichannel(hit_ptr->get()->get_arm(),
           hit_ptr->get()->get_plane(),
           hit_ptr->get()->get_panel(),
           orientation,
           hit_ptr->get()->get_twopack());
    TMuiReadoutID hardware = TMuiAddressTable::Table()->HardwareAddress(muichannel);
    int iword = hardware.ROC()*MUIOO::kWordsPerROC+hardware.Word();
    int ibit  = hardware.Channel();

    word[hit_ptr->get()->get_arm()][hit_ptr->get()->get_orientation()][iword] |= (0x01<<ibit);
  }
}

//_________________________________________________________________
void mMuiBLTEmulator::print_summary( std::ostream &out )
{
  MUIOO::PRINT( out, "mMuiBLTEmulator::print_summary" );
  for( int arm=0; arm < max_arm; arm++ )
  {
    out << ((arm==0) ? "South":"North") << " arm" << endl;
    out << "from sim:" << endl;
    out << "2D: " << _n_2D_sim[arm] << " 1D1S: " << _n_1D1S_sim[arm] << endl;

    out << "1D: [ ";
    for( int quad=0; quad<max_quad; quad++ ) out << _n_1D_sim[quad][arm] << " ";
    out << "]" << endl;

    out << "1S: [ ";
    for( int quad=0; quad<max_quad; quad++ ) out << _n_1S_sim[quad][arm] << " ";
    out << "]" << endl;

    out << "from reco:" << endl;
    out << "2D: " << _n_2D_reco[arm] << " 1D1S: " << _n_1D1S_reco[arm] << endl;

    out << "1D: [ ";
    for( int quad=0; quad<max_quad; quad++ ) out << _n_1D_reco[quad][arm] << " ";
    out << "]" << endl;

    out << "1S: [ ";
    for( int quad=0; quad<max_quad; quad++ ) out << _n_1S_reco[quad][arm] << " ";
    out << "]" << endl;
    out << endl;
  }

  MUIOO::PRINT( out, "**" );
}

//_________________________________________________________________
void mMuiBLTEmulator::raw_to_trigpattern()
{

  // This whole piece is taken directly from Hiroki's code without any changes. Except for
  // using three/four dimension array to handle two arm at the same time. I was very careful about
  // this little change.
  //

  // convert FEM word[iarm(0-1)][ifem(0-1)][word 0-119](0-0xffff) to trig_pattern[iarm(0-1)][quad(0-3)][plane(0-3)](0-15)
  const int max_sector = 8;
  unsigned int trig_bit[MUIOO::NumberOfArms][MUIOO::MAX_ORIENTATION][MUIOO::MAX_PLANE-1][max_sector];
  for(int iarm = 0; iarm < MUIOO::NumberOfArms; iarm++) {
    for ( int iorient = 0; iorient<MUIOO::MAX_ORIENTATION; iorient++ ) {
      for ( int iplane = 0; iplane<MUIOO::MAX_PLANE-1; iplane++ ){
        for ( int k = 0; k<max_sector; k++ ) {
          trig_bit[iarm][iorient][iplane][k] = 0;
        }
      }
    }
  }

  // use lower 16 bits of each word
  for(int iarm = 0; iarm < MUIOO::NumberOfArms; iarm++) {
    for( int iorient = 0; iorient<MUIOO::MAX_ORIENTATION; iorient++ ) {
      for ( int iword = 0; iword<MUIOO::kWordsPerFEM; iword++ ) {
        word[iarm][iorient][iword] = word[iarm][iorient][iword] & 0xffff;
      }
    }
  }
  
  // word -> trig_bit
  for(int iarm = 0; iarm < MUIOO::NumberOfArms; iarm++) 
  {
    int plane_new = 0;
    for ( int plane = 0; plane<MUIOO::MAX_PLANE; plane++ ) 
    {
      // There are 5 planes int word[][][], but only 4 planes in trig_bit[][][]
      if ( plane != get_non_used_plane()) {
        for ( int sector = 0; sector<max_sector; sector++ ) {
          if ( sector % 2 == 0 ) {
            trig_bit[iarm][ 0 ][ plane_new ][ sector ] =
              word[iarm][ 0 ][ plane*24 + sector*3  + 0 ]
              + word[iarm][ 0 ][ plane*24 + sector*3 + 1 ]
              + word[iarm][ 0 ][ plane*24 + sector*3 + 2 ]
              + word[iarm][ 0 ][ plane*24 + sector*3 + 3 ] ;
          } else {
            trig_bit[iarm][ 0 ][ plane_new ][ sector ] =
              word[iarm][ 0 ][ plane*24 + (sector-1)*3 + 2 ]
              + word[iarm][ 0 ][ plane*24 + (sector-1)*3 + 3 ]
              + word[iarm][ 0 ][ plane*24 + (sector-1)*3 + 4 ]
              + word[iarm][ 0 ][ plane*24 + (sector-1)*3 + 5 ];
          }
          
          for ( int iword = 0; iword<3; iword++ ) {
            trig_bit[iarm][ 1 ][ plane_new ][ sector ]  += word[iarm][ 1 ][ plane*24 + sector*3 + iword ];
          }
        }
        
        plane_new++;
      }
    }
  }
  
  // take "OR"
  for(int iarm = 0; iarm < MUIOO::NumberOfArms; iarm++) {
    for ( int iorient = 0; iorient < MUIOO::MAX_ORIENTATION; iorient++){ 
      for ( int iplane = 0; iplane < MUIOO::MAX_PLANE-1; iplane++ ){ 
        for ( int k = 0; k<max_sector; k++ ) { 
          if ( trig_bit[iarm][iorient][iplane][k] != 0 ) {
            trig_bit[iarm][iorient][iplane][k] = 1;
          }
        }
      }
    }
  }
  
  // trig_bit -> trig_pattern
  for(int iarm = 0; iarm < MUIOO::NumberOfArms; iarm++) {
    for ( int iquad = 0; iquad<max_quad; iquad++ ) {
      for ( int iplane = 0; iplane < MUIOO::MAX_PLANE-1; iplane++ ){ 
        trig_pattern[iarm][iquad][iplane] = 0;
      }
    }
  }
  
  // Chun :: horizental and vertial planes have different sector configuration.
  // horizental :: sector    6 4 2 0   quadrant 3 1
  //                         7 5 3 1            4 2
  //
  // vertical   :: sector    1 0   quadrant 3 1
  //                         3 2
  //                         5 4
  //                         7 6           4 2
  for(int iarm = 0; iarm < MUIOO::NumberOfArms; iarm++) {
    for( int plane = 0; plane<MUIOO::MAX_PLANE-1; plane++ ){
      
      trig_pattern[iarm][0][plane] |= 
        (trig_bit[iarm][0][plane][0] << 1)|
        (trig_bit[iarm][0][plane][2] << 0)|
        (trig_bit[iarm][1][plane][0] << 3)|
        (trig_bit[iarm][1][plane][2] << 2);
      
      trig_pattern[iarm][1][plane] |=
        (trig_bit[iarm][0][plane][1] << 1 )|
        (trig_bit[iarm][0][plane][3] << 0 )|
        (trig_bit[iarm][1][plane][4] << 2 )|
        (trig_bit[iarm][1][plane][6] << 3 );
      
      trig_pattern[iarm][2][plane] |= 
        (trig_bit[iarm][0][plane][4] << 0 )|
        (trig_bit[iarm][0][plane][6] << 1 )|
        (trig_bit[iarm][1][plane][1] << 3 )|
        (trig_bit[iarm][1][plane][3] << 2 );
      
      trig_pattern[iarm][3][plane] |=
        (trig_bit[iarm][0][plane][5] << 0 )|
        (trig_bit[iarm][0][plane][7] << 1 )|
        (trig_bit[iarm][1][plane][5] << 2 )|
        (trig_bit[iarm][1][plane][7] << 3 );
      
    }
  }
}

//_________________________________________________________________
int mMuiBLTEmulator::decision_event( const unsigned int& arm)
{

  // Copy of Hiroki's code, modification I made is
  // having arm as an input to handle both arm

  unsigned int trig_pattern_arm[max_quad][MUIOO::MAX_PLANE-1];

  for( unsigned int iquad = 0; iquad < max_quad; iquad++ )
  for( unsigned int iplane = 0; iplane < MUIOO::MAX_PLANE-1; iplane++)
  trig_pattern_arm[iquad][iplane] = trig_pattern[arm][iquad][iplane];

  for( int quad = 0; quad<max_quad; quad++ ) trig_accept[arm][quad] = 0;

  for ( int quad = 0; quad<max_quad; quad++ ) 
  {
    if( quad_fire( trig_pattern_arm[quad], mlu_data_d ) ) trig_accept[arm][quad] = DEEP_ROAD;
    else if ( quad_fire( trig_pattern_arm[quad], mlu_data_s ) ) trig_accept[arm][quad] = SHALLOW_ROAD;
  }
  return decision_mlu(arm);
}

//_________________________________________________________________
bool mMuiBLTEmulator::quad_fire( unsigned int quad_trig_pattern[max_plane],  mMuiBLTEmulator::mlu_array mlu_data  ) const
{
  
  // copy from Hiroki's code, take out 'cout'
  unsigned int address = 0;
  for ( int plane = 0; plane<max_plane - 1; plane++ ) {
    for ( int bit = 0; bit<4; bit++ ) {
      if ( ( ( quad_trig_pattern[plane] & (1<<bit) ) >> bit ) == 1 ) {
        address |= ( 1<< (bit + plane * 4) );
      }
    }
  }
  
  unsigned int data_bit = quad_trig_pattern[3];
  return ( ( ( mlu_data[address] & (1<<data_bit) ) >> data_bit ) == 1 );
  
}

//_________________________________________________________________
unsigned int mMuiBLTEmulator::decision_mlu( const unsigned int& arm) const
{

  // copy from hiroki's code, just take out 'cout'
  //

  short shallow = 0;
  short deep = 0;

  // if exclusive_swith is true, all trigger bits are true.
  // and only the "maximum" trigger bit is set
  bool exclusive_switch( false );

  // count number of deep and shallow road
  for( int quad = 0; quad<max_quad; quad++ ) {
    if ( trig_accept[arm][quad] == DEEP_ROAD ) deep++;
    else if ( trig_accept[arm][quad] == SHALLOW_ROAD ) shallow++;
  }

  // deep deep trigger
  if ( deep >= 2 ) return( exclusive_switch  ) ? DEEP_DEEP : DEEP_DEEP|DEEP_SHALLOW|SHALLOW_SHALLOW|SINGLE_DEEP|SINGLE_SHALLOW;
  else if ( deep == 1 && shallow >= 1 ) return (exclusive_switch) ? DEEP_SHALLOW : DEEP_SHALLOW|SHALLOW_SHALLOW|SINGLE_DEEP|SINGLE_SHALLOW;
  else if ( deep == 1 ) return ( exclusive_switch ) ? SINGLE_DEEP : SINGLE_DEEP|SINGLE_SHALLOW;
  else if ( shallow >= 2 ) return ( exclusive_switch ) ? SHALLOW_SHALLOW : SHALLOW_SHALLOW|SINGLE_SHALLOW;
  else if ( shallow == 1 ) return SINGLE_SHALLOW;
  return NONE;

}

//_________________________________________________________________
void mMuiBLTEmulator::fill_map()
{

  // Take the output from emulator, fill pseudo-BLT map.
  for( unsigned int iarm = 0; (int)iarm < MUIOO::NumberOfArms; iarm++) 
  {
    
    TMuiPseudoBLTMapO::iterator blt_iter = _blt_map->insert_new(iarm);
    unsigned int pseudoblt_bit = decision_event(iarm);
    
    // simulated trigger bit
    // deep-deep BLT trigger
    if( pseudoblt_bit & DEEP_DEEP ) 
    { 
      blt_iter->get()->fire_2D(); 
      _n_2D_sim[iarm]++; 
    }
    
    // deep-shallow BLT trigger
    if( pseudoblt_bit & DEEP_SHALLOW ) 
    { 
      blt_iter->get()->fire_1D1S(); 
      _n_1D1S_sim[iarm]++; 
    }
    
    // deep/shallow BLT trigger vs quadrant
    for(int iquad = 0; iquad < max_quad; iquad++) 
    {
      if(trig_accept[iarm][iquad]==DEEP_ROAD) 
      {
        
        blt_iter->get()->fire_1D(iquad); 
        blt_iter->get()->fire_1S(iquad); 
        _n_1D_sim[iquad][iarm]++;
        _n_1S_sim[iquad][iarm]++;
        
      } else if(trig_accept[iarm][iquad]==SHALLOW_ROAD) 
      {
        
        blt_iter->get()->fire_1S(iquad);
        _n_1S_sim[iquad][iarm]++;
      }
    }
    
    // reconstructed trigger bit
    // deep-deep BLT trigger
    if(is_reco_2D(iarm))   
    { 
      blt_iter->get()->fire_reco_2D();
      _n_2D_reco[iarm]++;
    }
    
    // deep-shallow BLT trigger
    if(is_reco_1D1S(iarm)) 
    { 
      blt_iter->get()->fire_reco_1D1S();
      _n_1D1S_reco[iarm]++;
    }
    
    // deep/shallow BLT trigger vs quadrant
    for(int iquad = 0; iquad < max_quad; iquad++) 
    {
      
      if(is_reco_1D(iarm,iquad)) 
      {
        blt_iter->get()->fire_reco_1D(iquad);
        _n_1D_reco[iquad][iarm]++;
      }
      
      if(is_reco_1S(iarm,iquad)) 
      {
        blt_iter->get()->fire_reco_1S(iquad);
        _n_1S_reco[iquad][iarm]++;
      }
     
    }
  }
}

//_________________________________________________________________
void mMuiBLTEmulator::set_interface_ptrs(PHCompositeNode* top_node)
{
  // module runtime parameters
  _mod_par = TMutNode<mMuiBLTEmulatorPar>::find_node(top_node,"mMuiBLTEmulatorPar");

  // interface map
  _blt_map = TMutNode<TMuiPseudoBLTMapO>::find_node(top_node,"TMuiPseudoBLTMapO");

  if(_mod_par->get_mode()==mMuiBLTEmulatorPar::FROMPRDF){
    _event = TMutNode<Event>::find_node(top_node,"PRDF");
    _hit_map = 0;
  }

  if(_mod_par->get_mode()==mMuiBLTEmulatorPar::FROMDST){
    _event = 0;
    _hit_map = TMutNode<TMuiHitMapO>::find_node(top_node,"TMuiHitMapO");
  }

}

//_________________________________________________________________
void mMuiBLTEmulator::initialize(int runNumber)
{

  // initialize of non_used_plane
  const int nup_max = 3; // non_used_plane
  int nup_begin[nup_max];
  int nup_end[nup_max];
  int nup[nup_max];

  nup_begin[0] = 35758;
  nup[0] = 1;
  nup_end[0] = 40655; // end of Run-2
  nup_begin[1] = 40656; // It is not certain when it changed?
  nup[1] = 2;
  nup_end[1] = 71869;
  nup_begin[2] = 71870;  // Nobu
  nup[2] = 4;
  nup_end[2] = 9999999; // current configuration

  int nup_found = 0;
  for ( int nup_set = 0; nup_set < nup_max; nup_set++ ) {
    if ( runNumber >= nup_begin[nup_set] && runNumber <= nup_end[nup_set] ) {
      set_non_used_plane(nup[nup_set]);
      nup_found = 1;
      cout << " mMuiPsedoTrigger Non_Used_Plane was set to " << get_non_used_plane() << endl;
    }
  }
  if( !nup_found ) {
    cout << " mMuiPsedoTrigger Non_Used_Plane was not found for Run " << runNumber  << endl;
    cout << "   Default value " << get_non_used_plane() << " will be used " << endl;
  }

  //Let's see if the database contains a time stamp for this run number,
  // if so we'll try to get the map from the database.
  bool db_init_done = false;
  RunToTime *runTime = RunToTime::instance();
  PHTimeStamp *beginTime = runTime->getBeginTime(runNumber);
  if (beginTime)
  {
    PdbBankManager *bankManager = PdbBankManager::instance();
    PdbApplication *application = bankManager->getApplication();
    if (application->startRead()) {
      PdbMuiTriggerMLU *achan;

      //Shallow MLU will be 0
      PdbBankID shortbankID(0);
      PdbCalBank* shortBank =  bankManager->fetchBank("PdbMuiTriggerMLUBank",shortbankID,"calib.mui.muitriggermlu",runNumber);

      //We are good to go!
      //First the shallow
      if(shortBank > 0)
      {
        shortBank->print();
        PdbMuiTriggerMLU *achan;
        if(shortBank->getLength()<=mlu_address_max)
        {
          for(unsigned int wordid = 0; wordid < shortBank->getLength(); wordid++)
          {
            achan = (PdbMuiTriggerMLU*)&(shortBank->getEntry(wordid));
            mlu_data_s[wordid]=achan->MLUword();
          }
          db_init_done = true;
        } else {
          cout << PHWHERE << "More data in database than expected."<<endl;
          db_init_done = false;
        }
        delete shortBank;
      } else cout << PHWHERE << "Map wasn't found in database."<<endl;

      //Deep MLU will be 1
      PdbBankID deepbankID(1);
      PdbCalBank* deepBank = bankManager->fetchBank("PdbMuiTriggerMLUBank",deepbankID,"calib.mui.muitriggermlu",runNumber);

      if(deepBank > 0)
      {
        deepBank->print();

        //Now the deep
        if(deepBank->getLength()<=mlu_address_max)
        {
          for(unsigned int wordid = 0; wordid < deepBank->getLength(); wordid++)
          {
            achan = (PdbMuiTriggerMLU*)&(deepBank->getEntry(wordid));
            mlu_data_d[wordid]=achan->MLUword();
          }
          db_init_done = true;
          delete deepBank;
        } else {
          cout << PHWHERE << "More data in database than expected."<<endl;
          db_init_done = false;
        }
      } else cout << PHWHERE << "Map wasn't found in database."<< endl;

      application->commit();
    } else cout << PHWHERE << "Failed to start application for update" << endl;
    delete beginTime;

  }

  if( db_init_done) return;
  else cout<< PHWHERE << "Map not found in database will try local files."<< endl;

  int map_select = _mod_par->get_mlu_map();
  const char* mlu_file_d;
  const char* mlu_file_d1 = "mui_pseudotrigmap_deep_mh1.dat";
  const char* mlu_file_d2 = "mui_pseudotrigmap_deep_mh2.dat";

  // MLU data file for deep roads
  const char* mlu_file_s = "mui_pseudotrigmap_shallow.dat";
  int run_pp_begin = 35758;
  int run_change_deep = 38189;
  int run_pp_end = 40655;

  if( map_select == 1 || ( map_select == 0 && ( (runNumber >= run_pp_begin) && (runNumber < run_change_deep) ) ) ) mlu_file_d = mlu_file_d1;
  else if( (map_select == 2 ) || ( map_select == 0 && ( (runNumber >= run_change_deep) && (runNumber <= run_pp_end) ) ) ) mlu_file_d = mlu_file_d2;
  else {
    cout << " mMuiPseudoTrigger: This is not pp Run2 or simulation" << endl;
    cout << "   No MLU map was read" << endl;
    return;
  }

  if( read_mlu_data( mlu_data_d, mlu_file_d ) ) cout << "mMuiPsedoTrigger; MLU data for deep roads " << mlu_file_d << " was read" << endl;
  else return;

  if( read_mlu_data( mlu_data_s, mlu_file_s ) ) cout << "mMuiPsedoTrigger; MLU data for shallow roads " << mlu_file_s << " was read" << endl;
  else return;
  return;
}

//_________________________________________________________________
bool mMuiBLTEmulator::read_mlu_data( mlu_array mlu_word, string mlu_data_file = "deep.dat")
{

  // initialize
  for ( unsigned int i = 0; i<mlu_address_max; i++ ) mlu_word[i] = 0;

  // open file
  FILE* fp;
  if ( !( fp = fopen( mlu_data_file.c_str(), "r" ))) 
  {
    printf(" mMuiPseudoTrigger: Input file '%s' can not be opened\n",mlu_data_file.c_str());
    return false;
  }
  
  // scan file
  unsigned int scan_data;
  for ( int i = 0; i<mlu_address_max; i++ ) {
    if ( fscanf( fp, "%x\n", &scan_data ) != EOF ) mlu_word[i] = scan_data;
    else {
      printf(" mMuiPseudoTrigger: unable to read %d th line of '%s'\n", i, mlu_data_file.c_str());
      return false;
    }
  }
  
  fclose( fp );
  return true;

}

//_________________________________________________________________
bool mMuiBLTEmulator::is_reco_2D( const unsigned int& arm) const
{

  for( unsigned int iquad1 = 0; iquad1 < 4; iquad1++) {
    if(is_reco_1D(arm, iquad1)) {
      for( unsigned int iquad2=0; iquad2<4; iquad2++) {
        if(iquad2==iquad1) continue;
        if(is_reco_1D(arm,iquad2)) return true;
      }
    }
  }
  return false;
}

//_________________________________________________________________
bool mMuiBLTEmulator::is_reco_1D1S( const unsigned int& arm) const
{
  for( unsigned int iquad1 = 0; iquad1 < 4; iquad1++)
  {
    if(is_reco_1D(arm, iquad1))
    {
      for( unsigned int iquad2=0; iquad2<4; iquad2++)
      {
        if(iquad2==iquad1) continue;
        if(is_reco_1D(arm,iquad2) || is_reco_1S(arm,iquad2)) return true;
      }
    }
  }
  return false;
}

//_________________________________________________________________
unsigned int mMuiBLTEmulator::get_quad( const float& x, const float& y ) const
{

  if( x>= 0 && y >= 0 ) return 3;
  else if( x>= 0 && y <= 0 ) return 1;
  else if( x<= 0 && y >= 0 ) return 2;
  else return 0;

}

//_________________________________________________________________
unsigned int mMuiBLTEmulator::get_fired_quad( const unsigned int& arm, const unsigned int& quad ) const
{
  unsigned int nhit_quad = 0;

  for( unsigned int iplane = 0; (int)iplane < MUIOO::MAX_PLANE-1; iplane++)
  {
    for( unsigned int iorient = 0; (int)iorient < MUIOO::MAX_ORIENTATION; iorient++)
    {
      bool found( false );
      for( unsigned int ipanel = 0; (int)ipanel < MUIOO::MAX_PANEL; ipanel++)
      {
        TMuiHitMapO::const_iterator mui_hit_iter = _hit_map->get(arm,iplane,ipanel,iorient);
        while(TMuiHitMapO::const_pointer mui_hit_ptr = mui_hit_iter.next())
        {
          TMuiMCHitMapO::const_key_iterator mc_hit_iter = mui_hit_ptr->get()->get_associated<TMuiMCHitO>();
          while(TMuiMCHitMapO::const_pointer mc_hit_ptr = mc_hit_iter.next())
          {
            
            // if the hit is in big panel( 0, 2, 3, 5) we can decide it's quadrant directly,
            // if the hit is in small panel(1, 4) and on horizental plane, it contributes to
            // both left and right quadrants, if the hit is in small panel and verticle plane,
            // we have to relay on mchit x/y position to decide the quadrant.
            if( (ipanel == 3 ||(ipanel == 4 && iorient == 0)) && quad == 0 ) found = true;
            if( (ipanel == 5 ||(ipanel == 4 && iorient == 0)) && quad == 1 ) found = true;
            if( (ipanel == 2 ||(ipanel == 1 && iorient == 0)) && quad == 2 ) found = true;
            if( (ipanel == 0 ||(ipanel == 1 && iorient == 0)) && quad == 3 ) found = true;
            if( (iorient == 1 && (ipanel==4||ipanel==1) ) && quad == get_quad( mc_hit_ptr->get()->get_x(), mc_hit_ptr->get()->get_y() ) ) found = true;
            
          }
        }
      }
      if( found ) nhit_quad++;
    }
  }
  
  return nhit_quad;
}

//_________________________________________________________________
void mMuiBLTEmulator::check_blt()
{

  // define a flag for debug.
  for(int iarm = 0; iarm < MUIOO::NumberOfArms; iarm++)
  {
    TMuiPseudoBLTMapO::const_iterator blt_iter = _blt_map->get(iarm);
    while(TMuiPseudoBLTMapO::const_pointer blt_ptr = blt_iter.next())
    {
      if(blt_ptr->get()->is_2D_fired() != is_reco_2D(iarm))
      {

        std::cout << " 2D trigger shows inconsistent " << std::endl;
        dump_reco_blt(iarm);
        blt_ptr->get()->print();
        _hit_map->print();

      } else if(blt_ptr->get()->is_1D1S_fired()!=is_reco_1D1S(iarm)) {

        std::cout << " 1D1S trigger shows inconsistent " << std::endl;
        dump_reco_blt(iarm);
        blt_ptr->get()->print();
        _hit_map->print();

      }
    }
  }
}

//_________________________________________________________________
void mMuiBLTEmulator::dump_reco_blt( const unsigned int& arm) const
{

  std::cout << " check on arm : " << arm << " :: " <<std::endl;
  std::cout << " reco trigger check from mMuiBLTEmulator " << std::endl;
  std::cout << " is_reco_2D::       " << is_reco_2D(arm) << std::endl;
  std::cout << " is_reco_1D1S::     " << is_reco_1D1S(arm) << std::endl;
  std::cout << " is_reco_1D quad0:: " << is_reco_1D(arm, 0) << std::endl;
  std::cout << " is_reco_1D quad1:: " << is_reco_1D(arm, 1) << std::endl;
  std::cout << " is_reco_1D quad2:: " << is_reco_1D(arm, 2) << std::endl;
  std::cout << " is_reco_1D quad3:: " << is_reco_1D(arm, 3) << std::endl;
  std::cout << " is_reco_1S quad0:: " << is_reco_1S(arm, 0) << std::endl;
  std::cout << " is_reco_1S quad1:: " << is_reco_1S(arm, 1) << std::endl;
  std::cout << " is_reco_1S quad2:: " << is_reco_1S(arm, 2) << std::endl;
  std::cout << " is_reco_1S quad3:: " << is_reco_1S(arm, 3) << std::endl;
}


