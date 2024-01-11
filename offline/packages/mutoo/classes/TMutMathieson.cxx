// $Id: TMutMathieson.cxx,v 1.25 2011/12/24 04:48:21 slash Exp $
#include<TMutMathieson.h>
#include<TMutMathiesonPar.h>
#include<gsl/gsl_math.h>
#include<PHException.h>
#include<TMutGeo.h>
#include<MutStrip.h>

using namespace std;

//_______________________________________________
// Decleration for static member
bool TMutMathieson::_verbose = false;
bool TMutMathieson::_initialized = false;
bool TMutMathieson::_no_stereo = false;
TMutMathieson::mathieson_data_map TMutMathieson::_lookup;

//_______________________________________________
TMutMathieson::strip_charge_array TMutMathieson::get_charge_array(double q_total, double x_local, MutStrip* peak_strip)
{

  /*
    ensure initialization was done
    you get a warning at compilation time but its the only way the test is done only once
  */
  static bool init_done __attribute__ ((unused)) = initialize();

  // Loop over all entries in lookup table.  Upon completion of the loop
  // the XToChargeFtor contains the array of q/strip pairs that corresponds
  // to the specified x_local.
  XToChargeFtor val = for_each(mathieson_lookup(peak_strip).begin(),
    mathieson_lookup(peak_strip).end(),
    XToChargeFtor(q_total,x_local,peak_strip));

  // Return the strip charge array in the ftor by value
  return val.get_strip_charge_array();
}

//_______________________________________________
pair<double,double> TMutMathieson::get_x_local(const charge_array& charges, MutStrip* peak_strip)
{

  /*
    ensure initialization was done
    you get a warning at compilation time but its the only way the test is done only once
  */
  static bool init_done __attribute__ ((unused)) = initialize();

  // Loop over all entries in lookup table.  Upon completion of the loop
  // the ChargeToXFtor contains the x_local that best approximates the
  // specified array of charges
  ChargeToXFtor val = for_each(mathieson_lookup(peak_strip).begin(),
    mathieson_lookup(peak_strip).end(),
    ChargeToXFtor(charges));

  return make_pair(val.get_x(), val.get_chi_sqr());
}

//_______________________________________________
bool TMutMathieson::initialize( bool forced )
{
  if( _initialized && !forced ) return true;
  if( forced ) _lookup.clear();
  _lookup = initialize_mathieson();
  _initialized = true;
  return true;
}

//_______________________________________________
TMutMathieson::mathieson_data_vector& TMutMathieson::mathieson_lookup(MutStrip* peak_strip)
{
  //since we want to get cathode by cathode mathieson lookup tabel,
  //we get the peak strip's information until the cathode level.
  unsigned short arm = peak_strip->getArm();
  unsigned short station = peak_strip->getStation();
  unsigned short gap = peak_strip->getGap();
  unsigned short cathode = peak_strip->getPlane();
  unsigned short octant = peak_strip->getOctant();

  // See if a table for this angle exists.  If we can't find it then we throw.
  mathieson_data_map::const_iterator iter = _lookup.find(get_hash(arm,station,gap,cathode,octant));

  if(iter != _lookup.end()) {
    return _lookup[get_hash(arm,station,gap,cathode,octant)];
  } else {
    throw invalid_argument(DESCRIPTION("No entry in lookup for given arm/station/gap/octant"));
  }
}

//_______________________________________________
// Hash from stereo angle to int
int TMutMathieson::get_angle_hash(double angle)
{ return static_cast<int>(floor(angle*MUTOO::RAD_TO_DEG+0.5)); }

//_______________________________________________
// Here we initialize mathieson lookup up tables.  One table is initialized
// for each unique value of the stereo angle.  The tables are stored in a
// hash_map.  The key is a hashed value of the stereo angle.
TMutMathieson::mathieson_data_map TMutMathieson::initialize_mathieson()
{
  const float qk3 = 0.5;
  const float qk2 = M_PI_2  * (1.0 - 0.5 * sqrt(qk3));
  const float qk1 = 2.0 * (qk2 * sqrt(qk3)) / (4.0 * atan(sqrt(qk3)));

  //in the below loop, we will get cathode by cathode STRIPWID infomation from the geom db
  float STRIPWID = 0.500;
  float strip_space = 1;

  // Intantiate lookup locally -- return initialized lookup by value.
  // (Expensive but we only do this once)
  mathieson_data_map lookup_map;

  unsigned short NumberOfGaps;
  MutStrip* strip_geo;

  // Loop over values of cap coupling
  for(int iarm=0; iarm<2; ++iarm)
    for(int ista=0; ista<3; ++ista)
    {
      NumberOfGaps = TMutGeo::get_n_gaps((unsigned short)iarm, (unsigned short)ista);

      for(int igap=0; igap<NumberOfGaps; ++igap)
        for(int ioct=0; ioct<8; ++ioct)
	{

	  double cap_coupling = TMutMathiesonPar::get_mathieson_parameters(iarm, ista, ioct, igap ).first;
	  double AC = TMutMathiesonPar::get_mathieson_parameters(iarm, ista, ioct, igap).second;

	  for(int icath=0; icath<2; ++icath)
	    {

	      //we have different cap_coupling and anode-cathode spacing per station in the current software,
	      //here, we read each values for station
	      strip_geo = TMutGeo::get_strip_geom((unsigned short)iarm, (unsigned short)ista, 0, 0, (unsigned short)igap, (unsigned short)icath, 0);

	      // Figure out the strip spacing for this plane(actually, typical one strip in this plane)
	      if(strip_geo) { strip_space = strip_geo->getStripSpacing(); }
	      else MUTOO::TRACE("TMutMathieson::initialize_mathieson: TMutGeom returned null pointer");

	      //STRIPWID is half of the read out strip space since we only read out 1,3,5 strips for example
	      STRIPWID = strip_space/2.0;

	      unsigned short hash = TMutMathieson::get_hash((unsigned short)iarm,(unsigned short)ista,(unsigned short)igap,(unsigned short)icath,(unsigned short)ioct);

	      // Check to see if an entry for this stereo angle exists, if it does
	      // then skip to next plane.
	      lookup_map.insert(make_pair(hash,mathieson_data_vector()));

	      // Fill the lookup table
	      for( unsigned int i=0; i< TMutMathiesonPar::get_grid_size(); i++)
		{

		  // let's start xpeak from the point in the middle of the 1st bin ie. STRIPWID*(-1) + STRIPWID/500.0
		  // and then go to the point in the middle of the last bin ie. STRIPWID - STRIPWID/500.0
		  float xpeak = (STRIPWID*(-1)+STRIPWID/TMutMathiesonPar::get_grid_size()) + STRIPWID*2/TMutMathiesonPar::get_grid_size()*i;

		  // find charge that would have been on 7 strips around the hit strip
		  float qtemp[7];
		  for (int j=0;j<7;j++)
		    {
		      float xlow  = (STRIPWID*(-3.5) + STRIPWID*j);
		      float xhigh = (STRIPWID*(-2.5) + STRIPWID*j);
		      qtemp[j] = ((qk1 / qk2) / sqrt(qk3))*
			(atan(sqrt(qk3) * tanh(qk2 * ((xhigh-xpeak)/AC))) -
			 atan(sqrt(qk3) * tanh(qk2 * ((xlow-xpeak)/AC))));
		    }

		  // strips 1, 3, 5 are read out with charge on strip + 50% (capacitively coupled strips)
		  float q1 = qtemp[1] + cap_coupling * qtemp[0] + cap_coupling * qtemp[2];
		  float q3 = qtemp[3] + cap_coupling * qtemp[2] + cap_coupling * qtemp[4];
		  float q5 = qtemp[5] + cap_coupling * qtemp[4] + cap_coupling * qtemp[6];
		  float qtotal = q1 + q3 + q5;

		  mathieson_data data = {xpeak, q1/qtotal, q3/qtotal, q5/qtotal};
		  lookup_map[hash].push_back(data);
		} // end of x position loop in one strip
	    } //cathode loop end
	} // gap loop end
    } // station loop end

  if(_verbose)
  {
    int hash_size = lookup_map.size();
    for (int ihash=0; ihash<hash_size; ihash++)
    {
      mathieson_data_vector imap = lookup_map[ihash];
      for (size_t iindex=0; iindex< imap.size(); iindex++)
      { cout << imap[iindex].x <<" "<< imap[iindex].q1 <<" "<<imap[iindex].q2 <<" "<<imap[iindex].q3 <<endl; }
    }

    MUTOO::PRINT(cout,"Mathieson Lookup Initialization");
    cout << " capacitive coupling: ";
    for(mathieson_data_map::const_iterator iter = lookup_map.begin();iter!=lookup_map.end();++iter)
    { cout << iter->first << " " ; }
    cout << endl;

    MUTOO::PRINT(cout,"**");

    //print out all the mathieson map info
    for (int ihash=0; ihash<hash_size; ihash++)
    {
      mathieson_data_vector imap = lookup_map[ihash];
      for (size_t iindex=0; iindex< imap.size(); iindex++)
      { cout << imap[iindex].x <<" "<< imap[iindex].q1 <<" "<<imap[iindex].q2 <<" "<<imap[iindex].q3 <<endl; }
    }
  }

  // Return the lookup map by value -- inefficient but we only do this
  // once since we are using standard construct upon first use semantics
  // for the statically scoped lookup table.
  return lookup_map;
}


