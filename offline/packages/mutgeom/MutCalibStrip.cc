// $Id: MutCalibStrip.cc,v 1.21 2017/10/12 01:26:30 shlim Exp $

/*!
  \file MutCalibStrip.cc
  \brief declare calibration data class that will store the
  calibration data and be able to retrieve it from the database.
*/

#include <cassert>
#include <RunToTime.hh>
#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <iostream>
#include <fstream>

#include "MutCalibStrip.hh"
#include "MUTGEOM.h"

using namespace std;

//________________________________________________
MutCalibStrip::MutCalibStrip():
  // by default we do not use gain corrections
  _use_gain_corrections( true ),
  _use_new_calibration_method( false )
{  _gain_corrections.assign( 1.0 ); }

//________________________________________________
void MutCalibStrip::initialize_gain_corrections_run8( void )
{

  printf("MutCalibStrip::initialize_gain_corrections_run8.\n");

  // initialize gain corrections
  float corrections[NUM_OCTANTS] =
  {
    // south arm, station 0
    1.05024,    1.04775,    1.06060,
    1.09145,    1.06567,    1.15148,
    1.08170,    1.06035,    1.12154,
    1.13216,    1.06261,    1.00634,
    1.01441,    1.02341,    1.01664,
    1.00277,    1.01381,    1.04209,
    1.07317,    1.04239,    1.05638,
    1.00000,    1.00025,    1.04939,

    // south arm, station 1
    1.10608,    1.14387,    1.12472,
    1.11119,    1.13730,    1.07711,
    1.39233,    1.10706,    1.10336,
    1.04701,    1.16817,    1.07407,
    1.00000,    1.08592,    1.10095,
    1.08770,    1.16893,    1.12050,
    1.08466,    1.13448,    1.12272,
    1.05457,    1.12161,    1.05276,

    // south arm, station 2
    1.04730,    1.09088,    0.00000,
    1.01528,    1.06563,    0.00000,
    1.01727,    1.02650,    0.00000,
    1.09209,    1.07771,    0.00000,
    1.06896,    1.04373,    0.00000,
    1.01087,    1.00000,    0.00000,
    1.04103,    1.04359,    0.00000,
    1.03762,    1.05971,    0.00000,

    // north arm, station 0
    1.81983,    1.75184,    1.80625,
    1.00000,    1.58414,    1.84756,
    2.66937,    2.43301,    1.08467,
    2.46520,    2.40310,    1.07435,
    1.98915,    1.82880,    1.82993,
    1.72391,    1.67214,    1.01432,
    1.79073,    1.74053,    1.84747,
    1.64593,    1.06812,    1.73868,

    // north arm, station 1
    1.00537,    1.09270,    1.04662,
    1.04454,    1.06591,    1.02018,
    1.02331,    1.08320,    1.03551,
    1.74728,    1.03251,    1.04502,
    1.29803,    1.04823,    1.05484,
    1.03538,    1.07373,    1.00000,
    1.00079,    1.06991,    1.03585,
    1.02877,    1.10559,    1.03047,

    // north arm, station 2
    2.74154,    2.86905,    0.00000,
    2.96542,    3.03792,    0.00000,
    3.15878,    3.12602,    0.00000,
    3.01031,    2.94122,    0.00000,
    2.88774,    2.71675,    0.00000,
    2.64313,    1.24592,    0.00000,
    2.81327,    1.00000,    0.00000,
    2.98590,    2.93927,    0.00000

  };

  // initialize and copy into member
  _gain_corrections.assign( 1.0 );
  for( unsigned int i=0; i<NUM_OCTANTS; i++ )
  { _gain_corrections[i] = corrections[i]; }

}

//________________________________________________
void MutCalibStrip::initialize_gain_corrections_run9( void )
{

  cout << "MutCalibStrip::initialize_gain_corrections_run9." << endl;

  // initialize gain corrections (from Mike)
  float corrections[NUM_OCTANTS] =
  {
    // south station 1
    1.021,1.078,1.087,
    1.072,1.054,1.054,
    1.049,1.044,1.070,
    1.042,1.072,1.069,
    1.024,0.999,1.024,
    1.064,1.023,1.041,
    1.081,1.039,1.059,
    1.043,1.035,1.040,

    // south station 2
    1.110,1.080,1.133,
    1.106,1.044,1.064,
    1.403,1.039,1.144,
    1.070,1.129,1.078,
    1.026,1.000,1.094,
    1.079,1.105,1.127,
    1.082,1.051,1.121,
    1.068,1.038,1.052,

    // south station 3
    /* note: third column, corresponding to gap 3 is never used*/
    1.020,1.033, 1.0,
    1.029,1.025, 1.0,
    1.029,1.017, 1.0,
    1.080,1.068, 1.0,
    1.074,1.052, 1.0,
    0.999,1.005, 1.0,
    1.021,1.029, 1.0,
    1.045,1.033, 1.0,

    // north station 1
    1.751,1.798,1.757,
    0.999,1.700,1.906,
    2.662,2.453,1.046,
    2.521,2.413,1.084,
    1.877,1.787,1.772,
    1.770,1.660,1.030,
    1.775,1.773,1.804,
    1.509,1.029,1.759,

    // north station 2
    1.016,1.054,1.044,
    1.073,0.999,1.057,
    1.003,1.005,1.013,
    1.969,1.007,1.100,
    1.989,1.056,1.129,
    1.061,1.027,1.051,
    1.022,1.055,1.072,
    1.059,1.057,1.068,

    // north station 3
    /* note: third column, corresponding to gap 3 is never used*/
    1.216,1.232, 1.0,
    1.242,1.248, 1.0,
    1.303,1.289, 1.0,
    1.290,1.298, 1.0,
    1.255,1.217, 1.0,
    1.200,0.999, 1.0,
    1.308,1.184, 1.0,
    1.319,1.295, 1.0
  };

  // initialize and copy into member
  _gain_corrections.assign( 1.0 );
  for( unsigned int i=0; i<NUM_OCTANTS; i++ )
  { _gain_corrections[i] = corrections[i]; }

}

//________________________________________________
void MutCalibStrip::initialize_gain_corrections_run11( void )
{

  printf("MutCalibStrip::initialize_gain_corrections_run11.\n");

  // initialize gain corrections (run9 values with newly terminated lines in north sta-1 reset - mjl)
  float corrections[NUM_OCTANTS] =
  {
    // south station 1
    1.021,1.078,1.087,
    1.072,1.054,1.054,
    1.049,1.044,1.070,
    1.042,1.072,1.069,
    1.024,0.999,1.024,
    1.064,1.023,1.041,
    1.081,1.039,1.059,
    1.043,1.035,1.040,

    // south station 2
    1.110,1.080,1.133,
    1.106,1.044,1.064,
    1.403,1.039,1.144,
    1.070,1.129,1.078,
    1.026,1.000,1.094,
    1.079,1.105,1.127,
    1.082,1.051,1.121,
    1.068,1.038,1.052,

    // south station 3
    /* note: third column, corresponding to gap 3 is never used*/
    1.020,1.033, 1.0,
    1.029,1.025, 1.0,
    1.029,1.017, 1.0,
    1.080,1.068, 1.0,
    1.074,1.052, 1.0,
    0.999,1.005, 1.0,
    1.021,1.029, 1.0,
    1.045,1.033, 1.0,

    // north station 1
    1.215,1.229,1.121,
    1.169,1.237,1.308,
    1.839,1.763,1.217,
    1.748,1.716,1.206,
    1.262,1.245,1.243,
    1.223,1.171,1.180,
    1.222,1.235,1.250,
    1.000,1.179,1.223,

    // north station 2
    1.016,1.054,1.044,
    1.073,0.999,1.057,
    1.003,1.005,1.013,
    1.969,1.007,1.100,
    1.989,1.056,1.129,
    1.061,1.027,1.051,
    1.022,1.055,1.072,
    1.059,1.057,1.068,

    // north station 3
    /* note: third column, corresponding to gap 3 is never used*/
    1.216,1.232, 1.0,
    1.242,1.248, 1.0,
    1.303,1.289, 1.0,
    1.290,1.298, 1.0,
    1.255,1.217, 1.0,
    1.200,0.999, 1.0,
    1.308,1.184, 1.0,
    1.319,1.295, 1.0
  };

  // initialize and copy into member
  _gain_corrections.assign( 1.0 );
  for( unsigned int i=0; i<NUM_OCTANTS; i++ )
  { _gain_corrections[i] = corrections[i]; }

}

//________________________________________________
MutCalibStrip::~MutCalibStrip()
{}

//_____________________________________________________
int MutCalibStrip::dbGetAll( int runnumber)
{
  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(runnumber));
  PHTimeStamp Tstart = *ts;
  delete ts;

  return dbGetAll( Tstart);  
}

//________________________________________________
int MutCalibStrip::dbGetAll(PHTimeStamp tsearch)
{

  assert( _strips.empty() );

  printf("MutCalibStrip::dbGetAll - use_gain_corrections: %s\n", (_use_gain_corrections ? "true" : "false"));

  // initialize corrections based on search time
  if ( _use_gain_corrections )
    {
      static const PHTimeStamp begin_of_run9( 2009, 01, 15, 0, 0, 0 );
      static const PHTimeStamp begin_of_run11( 2011, 01, 15, 0, 0, 0 );
      if ( tsearch < begin_of_run9 ) initialize_gain_corrections_run8();
      else if ( tsearch < begin_of_run11 ) initialize_gain_corrections_run9();
      else initialize_gain_corrections_run11();
    }

  // start out fresh
  // Access the database, pull data into the internal structure
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  PHString calibname = "calib.mut.strip";
  PHString classname = "PdbMutCalibStripBank";
  PdbBankID bankid(2);
  if ( getUseNewCalibrationMethod() )
    {
      classname="PdbMutCalibStrip_v2Bank";
      bankid = 3;
    }
  
  // Open the Objectivity database for reading and pull all the values for
  // a given bank (i.e. a strip) and put it into our set
  if ( application->startRead() )
    {

      PdbCalBank *mutBank =
        bankManager->fetchBank(classname.getString(),
                               bankid, calibname.getString(), tsearch);

      if (mutBank)
        {

	  //          printf("MutCalibStrip::dbGetAll - start validity : %s (%u)\n", mutBank->getStartValTime().formatTimeString(), (unsigned int)mutBank->getStartValTime().getTics());
	  //          printf("MutCalibStrip::dbGetAll - end validity   : %s (%u)\n", mutBank->getEndValTime().formatTimeString(), (unsigned int)mutBank->getEndValTime().getTics());
	  //          printf("MutCalibStrip::dbGetAll - insertion      : %s (%u)\n", mutBank->getInsertTime().formatTimeString(), (unsigned int) mutBank->getInsertTime().getTics());
	  //          printf("MutCalibStrip::dbGetAll - description    : %s\n", mutBank->getDescription().getString());

          //mutBank->print();
          int length = mutBank->getLength();

          for (int i = 0; i < length; i++)
            {
	      if ( getUseNewCalibrationMethod() )
		{
		  PdbMutCalibStrip_v2 strip = (PdbMutCalibStrip_v2&)(mutBank->getEntry(i));
		  if ( _use_gain_corrections )
		    apply_gain_corrections(strip);
		  _strips.insert( strip );
		}
	      else
		{
		  PdbMutCalibStrip strip = (PdbMutCalibStrip&)(mutBank->getEntry(i));
		  if ( _use_gain_corrections )
		    apply_gain_corrections(strip);
		  _strips.insert( strip );		  
		}
	    }
          delete mutBank;

        }
      else
        {
          printf("MutCalibStrip::dbGetAll - bankManager returned zero-pointer\n");
        }

    }
  else
    {

      application->abort();
      printf("MutCalibStrip::dbGetAll - Transaction aborted.\n");

    }

  return 0;
}

//_____________________________________________________
void MutCalibStrip::apply_gain_corrections(PdbMutCalibStrip& strip)
{
  // apply gain correction
  int tarm = strip.getArm();
  int tstation = strip.getStation();
  int toctant = strip.getOctant();
  int tgap = strip.getGap();
  float gain_correction = get_gain_correction( tarm, tstation, toctant, tgap );
  
  float gain_new = strip.get_gain() * gain_correction;
  float Center_new = strip.getCenter() / gain_correction;
  float SqrLow_new = strip.getSqrLow() * gain_correction;
  float SqrHigh_new = strip.getSqrHigh() * gain_correction;
  float LinSat_new = strip.getLinSat() * gain_correction;
  //int Saturation_new = strip.getSaturation() / gain_correction;
	int Saturation_new = int(strip.getSaturation() / gain_correction + 0.00001);

	//Sanghoon
	//In SL6, a strange conversion has been found like 2.0 -> 1
	//Add a small number before convert to integer 
  
  // some printout
  //           if(i<10) {
  //             cout << "a,s,o,g = " << tarm << " " << tstation << " " << toctant << " " << tgap << endl;
  //             strip.print();
  //           }
  
  // store corrected values
  strip.set_gain(gain_new);
  strip.setCenter(Center_new);
  strip.setSqrLow(SqrLow_new);
  strip.setSqrHigh(SqrHigh_new);
  strip.setLinSat(LinSat_new);
  strip.setSaturation(Saturation_new);
  //          if(i<10) strip.print();
}

//_____________________________________________________
int MutCalibStrip::dbPutAll( int beginrun, int endrun, PHString descriptor) const
{
  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(beginrun));
  PHTimeStamp Tstart = *ts;
  delete ts;
  PHTimeStamp Tstop;

  // The runnumber is encoded into PHTimeStamp.
  if (endrun > 0) {
    ts = runTime->getEndTime(endrun);
    Tstop = *ts;
    delete ts;
  }
  else {
    Tstop.setToFarFuture();
  }

  return dbPutAll( Tstart, Tstop, descriptor);  
}

//_____________________________________________________
int MutCalibStrip::dbPutAll( PHTimeStamp start, PHTimeStamp stop, PHString descriptor) const
{

  cout << "MutCalibStrip::dbPutAll - startvaltime: " << start << endl;
  cout << "MutCalibStrip::dbPutAll - endvaltime: " << stop << endl;
  cout << "MutCalibStrip::dbPutAll - description: " << descriptor << endl;

  // do we have something to put into the database?
  int length = _strips.size();
  if (length < 1)
  {
    cout << "MutCalibStrip::dbPutAll - Nothing to put into DB " << endl;
    cout << "length = " << length << endl;
    return -1;
  }

  //timestamp values: sanity check
  if (start>stop)
  {
    cout << "MutCalibStrip::dbPutAll - invalid start and stop:" << start << stop << endl;
    cout << "ignored" << endl;
    return -1;
  }

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  PHString calibname="calib.mut.strip";
  PHString classname="PdbMutCalibStripBank";
  PdbBankID bankid(2);

  if ( getUseNewCalibrationMethod() )
    {
      classname="PdbMutCalibStrip_v2Bank";
      bankid = 3;
    }

  #ifdef DEBUG
  cout << "MutCalibStrip::dbPutAll - Opening FD in update mode.." << endl;
  #endif
  if (application->startUpdate())
  {

    PdbCalBank *mutBank =
      bankManager->createBank( classname.getString(),
      bankid, descriptor.getString(),
      start, stop, calibname.getString());

    cout << "MutCalibStrip::dbPutAll - new bank created" << endl;
    mutBank->setLength(length);
    mutBank->print();

    for(int i=0; i<_strips.size(); i++)
    {
      if ( getUseNewCalibrationMethod() )
	{
	  PdbMutCalibStrip_v2 *strip_pointer = (PdbMutCalibStrip_v2*)&(mutBank->getEntry(i));
	  *strip_pointer = (*_strips.get_v2(i));
	}
      else
	{
	  PdbMutCalibStrip *strip_pointer = (PdbMutCalibStrip*)&(mutBank->getEntry(i));
	  *strip_pointer = (*_strips.get_v1(i));
	}
    }

    // commit new bank to the db and delete
    application->commit( mutBank );
    delete mutBank;

  } else cout << "MutCalibStrip::dbPutAll - failed to start application for update" << endl;

  cout << "MutCalibStrip::dbPutAll - done." << endl;
  cout << endl;

  return 0;
}

//____________________________________________________________
int MutCalibStrip::txtGetAll( const char* infile )
{

  assert( _strips.empty() );

  ifstream fin(infile);
  if (!fin)
  {

    cout << "MutCalibStrip::txtGetAll - can't open " << infile << "for input" << endl;
    return 1;

  } else cout << "MutCalibStrip::txtGetAll - reading calibrations from file " << infile << endl;

  //setup
  _strips.clear();

  // read from the file as long as we can
  // add strips to set as we go along
  while ( fin.good() )
  {
    if ( getUseNewCalibrationMethod() )
      {
	PdbMutCalibStrip_v2 strip;
	strip.read(fin);
	if (! fin.good() ) break;
	_strips.insert( strip );
      }
    else
      {
	PdbMutCalibStrip strip;
	strip.read(fin);
	if (! fin.good() ) break;
	_strips.insert( strip );
      }
  }
  cout << "MutCalibStrip::txtGetAll - Read " << _strips.size() << " strips" << endl;

  fin.close();
  return 0;
}

//___________________________________________________________________
int MutCalibStrip::txtPutAll( const char* outfile ) const
{
  ofstream fout(outfile);
  if (!fout)
  {
    cout << "MutCalibStrip::txtPutAll - can't open " << outfile << "for output" << endl;
    return 1;
  }

  // go thru the set and print all to fout
  for(int i=0; i<_strips.size(); i++)
  { 
    if ( getUseNewCalibrationMethod() )
      {
	const PdbMutCalibStrip_v2* strip = _strips.get_v2(i);
	strip->write(fout); 
      }
    else
      {
	const PdbMutCalibStrip* strip = _strips.get_v1(i);
	strip->write(fout); 
      }
}

  fout.close();
  return 0;
}

//________________________________________________________________________
const PdbMutCalibStrip* MutCalibStrip::getPdbMutCalibStrip(
  const int& arm,
  const int& station, const int& octant, const int& halfoctant,
  const int& gap, const int& plane,
  const int& strip_id ) const
{
  if ( getUseNewCalibrationMethod() )
    {
      // find object in the set and return pointer to it (NULL, if not found)
      PdbMutCalibStrip_v2 strip;
      strip.set_indices(arm,station,octant,halfoctant,gap,plane,strip_id);
      const PdbMutCalibStrip_v2* strip_found = _strips.get( strip );
      if (strip_found) return strip_found;
    }
  else
    {
      // find object in the set and return pointer to it (NULL, if not found)
      PdbMutCalibStrip strip;
      strip.set_indices(arm,station,octant,halfoctant,gap,plane,strip_id);
      const PdbMutCalibStrip* strip_found = _strips.get( strip );
      if (strip_found) return strip_found;
    }

  cout
    << "MutCalibStrip::getPdbMutCalibStrip - "
    << " arm " << arm
    << " sta " << station
    << " oct " << octant
    << " hoct " << halfoctant
    << " gap " << gap
    << " pla " << plane
    << " strip " << strip_id
    << " - not found in database " << endl;
  return NULL;
}

//___________________________________________________________________________________
void MutCalibStrip::putPdbMutCalibStrip(const PdbMutCalibStrip *new_strip)
{
  _strips.erase( *new_strip );
  _strips.insert( *new_strip );
}

//___________________________________________________________________________________
void MutCalibStrip::putPdbMutCalibStrip(const PdbMutCalibStrip_v2 *new_strip)
{
  _strips.erase( *new_strip );
  _strips.insert( *new_strip );
}

//______________________________________________________________________
void MutCalibStrip::swap( PdbMutCalibStrip first, PdbMutCalibStrip second )
{

  // temporary copy of first strip
  PdbMutCalibStrip tmp( first );

  // copy second strip parameters into first
  first.set_pedestal( second.get_pedestal() );
  first.set_gain( second.get_gain() );
  first.set_rms( second.get_rms() );
  first.setStatus( second.getStatus() );
  first.setSaturation( second.getSaturation() );
  first.setStep( second.getStep() );

  for( int i = 0; i < second.get_ncalibpar(); i++ )
  { first.set_calibpar( i, second.get_calibpar(i) ); }

  // copy tmp strip into second
  second.set_pedestal( tmp.get_pedestal() );
  second.set_gain( tmp.get_gain() );
  second.set_rms( tmp.get_rms() );
  second.setStatus( tmp.getStatus() );
  second.setSaturation( tmp.getSaturation() );
  second.setStep( tmp.getStep() );

  for( int i = 0; i < tmp.get_ncalibpar(); i++ )
  { second.set_calibpar( i, tmp.get_calibpar(i) ); }

  // re-insert into list of strips
  putPdbMutCalibStrip( &first );
  putPdbMutCalibStrip( &second );

  return;

}

//______________________________________________________________________
void MutCalibStrip::print( ostream& out ) const
{

  MUTGEOM::PRINT( out, "MutCalibStrip::print" );

  out << "Arm Station Octant HalfOct. Gap Plane Strip pedestal gain rms" << endl;

  for(int i=0; i<_strips.size(); i++)
    {
      if ( getUseNewCalibrationMethod() )
      {
	const PdbMutCalibStrip_v2* strip = _strips.get_v2(i);
	strip->print();
      }
      else
	{
	  const PdbMutCalibStrip* strip = _strips.get_v1(i);
	  strip->print();
	}      
    }

  MUTGEOM::PRINT( out, "**" );
  
}
