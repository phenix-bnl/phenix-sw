// $Id: TMutClusCentroid.hh,v 1.6 2011/12/29 20:19:29 slash Exp $

#ifndef __TMUTCLUSCENTROID_H__
#define __TMUTCLUSCENTROID_H__

/*!
	\file TMutClusCentroid.h
	\brief Muon tracker cluster centroid
	\author S. Kelly
  \version $Revision: 1.6 $
  \date    $Date: 2011/12/29 20:19:29 $
*/

/*! @ingroup interface */

//!  The Muon tracker cluster centroid 
/*!  The Muon tracker cluster centroid. This object provides the
interface to centroids associated with a TMutClus. Each track that
contributes charge to the strips associated with this cluster will
after cluster fitting have an associated centroid.  The centroid
coordinate is in the so-called w space which is defined relative
to the end point strip normal in the direction normal to the strip
direction.*/

#include<TDataType.h>
#include<TObject.h>
#include<iostream>



class TMutClusCentroid : public TObject
{

public:

  //! @name Constructors/Destructors
  //@{    
  /*! Default constructor */
  TMutClusCentroid() 
  {;}

  /*! Construct with peak strip, w, w_error, and q */
  TMutClusCentroid(UShort_t peak_strip ):
    _peak_strip( peak_strip )
  {;}

  //! destructor
  virtual ~TMutClusCentroid()
  {;}

  //@}

  //! @name Functional Interface
  //@{    
  /*! Peak strip number */
  virtual UShort_t get_peak_strip() const 
  {return _peak_strip;}
  
  /*! W coordinate */
  virtual double get_w() const 
  {return _w;}
  
  /*! W coordintate error */
  virtual double get_w_error() const 
  {return _w_error;}
  
  /*! Peak charge associated with this centroid fit */
  virtual double get_q_peak() const 
  {return _q_peak;}
  
  /*! Total charge associated with this centroid fit */
  virtual double get_q_tot() const 
  {return _q_tot;}
  
  /*! error on total charge */
  virtual double get_q_tot_error() const
  { return 0; }
  
  /*! Peak strip number */
  virtual void set_peak_strip(UShort_t strip) 
  { _peak_strip = strip;}
  
  /*! W coordinate */
  virtual void set_w(double w) 
  { _w = w;}
  
  /*! W coordintate error */
  virtual void set_w_error(double w_error) 
  { _w_error = w_error;}
  
  /*! Peak charge associated with this centroid fit */
  virtual void set_q_peak(double q_peak) 
  { _q_peak = q_peak;}
  
  /*! Total charge associated with this centroid fit */
  virtual void set_q_tot(double q_tot) 
  { _q_tot = q_tot;}
  
  virtual void set_q_tot_error( double value )
  {}
  
  //@}

  //! @name Dumpers
  //@{    
  /*! Print centroid data*/
  virtual void print(std::ostream& os = std::cout) const 
  {
    os 
      << " TMutClusCentroid - "
      << " peak strip: " << get_peak_strip()
      << " w: " << get_w() << " w_error: " << get_w_error()
      << " q_peak: " << get_q_peak() 
      << " q_tot: " << get_q_tot()  << " q_tot_error: (not implemented)"
      << std::endl;
  }
  //@}

private:

  //! peak strip
  UShort_t _peak_strip;
  
  //! position (perp to the strips)
  double _w;
  
  //! error on position
  double _w_error;
  
  //! peak strip charge
  double _q_peak;
  
  //! total charge (from fit)
  double _q_tot;
  
  ClassDef(TMutClusCentroid,1)

};

#endif
