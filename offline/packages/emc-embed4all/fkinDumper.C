#include "fkinDumper.h"
#include <cmath>
//INCLUDECHECKER: Removed this line: #include <iostream>
#include <iomanip>

//_____________________________________________________________________________
void 
fkinDumper::dump(const fkinWrapper& fkin, const char* title,
		 std::ostream& out)
{
  out << title << std::string(50,'*') << "\n";
  for ( size_t i = 0; i < fkin.RowCount(); ++i )
    {
      std::ostream::fmtflags oldflags = out.flags();
      out << std::setw(3) << "TRUETRACK=" 
	  << fkin.get_true_track(i) << " "
	  << std::setw(3) << fkin.get_subevent(i) << " "
	  << std::setw(3) << fkin.get_ntrack(i) << " ";
      out.setf(std::ios::scientific);
      out.precision(3);
      out << " PTOT=" << fkin.get_ptot(i)
	  << " R_VERTEX=" << fkin.get_r_vertex(i)
	  << " Z_VERTEX=" << fkin.get_z_vertex(i);
      out << " ITPARENT=" << fkin.get_itparent(i)
	  << " IDPARENT=" << fkin.get_idparent(i)
	  << " IDPART=" << fkin.get_idpart(i)
	  << "\n";
      out.setf(oldflags);
    }
}

//_____________________________________________________________________________
void 
fkinDumper::dump(const primaryWrapper& primary, const char* title,
		 std::ostream& out)
{
  out << title << std::string(50,'*') << std::endl;
  for ( size_t i = 0; i < primary.RowCount(); ++i )
    {
      std::ostream::fmtflags oldflags = out.flags();
      out << std::setw(3) << primary.get_key(i) << " "
		<< std::setw(3) 
		<< "TRACK=" << primary.get_true_track(i) << " ";
      out.setf(std::ios::scientific);
      out.precision(3);

      float px = primary.get_px_momentum(i);
      float py = primary.get_py_momentum(i);
      float pz = primary.get_pz_momentum(i);

      out << " PX=" << px
		<< " PY=" << py
		<< " PZ=" << pz
		<< " PTOT=" << sqrt(px*px+py*py+pz*pz)
		<< std::setw(3) 
		<< " IDPART= " << primary.get_idpart(i)
		<< std::endl;
      out.setf(oldflags);
    }
}
