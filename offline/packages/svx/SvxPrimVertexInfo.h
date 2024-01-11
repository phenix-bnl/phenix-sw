#ifndef __SvxPrimVertexFinder_H__
#define __SvxPrimVertexFinder_H__

/*!
 * \file    SvxPrimVertexInfo.h
 * \brief   Object to hold information on the Svx Precise vertex
 * \author  D. McGlinchey
 * \version $Revision: 1.1 $
 * \date    $Date: 2016/05/10 19:27:53 $
 */

#include <phool.h>
#include <PHObject.h>
#include <PHPoint.h>

#include <iostream>


class SvxPrimVertexInfo : public PHObject
{

public:

  /*! Constructor */
  SvxPrimVertexInfo() {}

  /*! Destructor */
  virtual ~SvxPrimVertexInfo() {}

  // Standard functions of all inheritors of PHObject classes...
  // """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  /// Identify object
  virtual void identify(std::ostream &os = std::cout) const {
    os << "Identify yourself: virtual SvxPrimVertexInfo object" << std::endl;
  }

  /// Clear Event
  virtual void Reset() { PHOOL_VIRTUAL_WARN("Reset"); }

  /// isValid returns non zero if object contains valid data
  virtual int isValid() const 
  {
    PHOOL_VIRTUAL_WARN("isValid");
    return -9999;
  }

  /// Clone object
  virtual SvxPrimVertexInfo* Clone() 
  {
    PHOOL_VIRTUAL_WARN("Clone()");
    return 0;
  }
  TObject* Clone(const char* = "") const 
  {
    PHOOL_VIRTUAL_WARNING;
    return 0;
  }

  /// Copy object
  virtual void Copy(SvxPrimVertexInfo* info) { PHOOL_VIRTUAL_WARN("Copy()"); }
  virtual void Copy(TObject& object) const { PHOOL_VIRTUAL_WARNING; }



  //! @name Setters
  //@{

  /*! Set the vertex position */
  virtual void set_vertex(double x, double y, double z)
  {
    PHOOL_VIRTUAL_WARN("set_vertex()");
  }

  /*! Set the vertex resolution */
  virtual void set_vertexResolution(double sig_x, double sig_y, double sig_z)
  {
    PHOOL_VIRTUAL_WARN("set_vertexResolution()");
  }

  /*! Add the ID of SvxSegment used in vertex calculation */
  virtual void add_segmentID(int id)
  {
    PHOOL_VIRTUAL_WARN("add_segmentID()");
  }

  //@}



  //! @name Getters
  //@{

  /*! Get the vertex position (PHPoint) */
  virtual PHPoint get_vertex() 
  { 
    PHOOL_VIRTUAL_WARN("get_vertex()"); 
    return PHPoint(-9999., -9999., -9999.);
  }

  /*! Get the vertex position in X */
  virtual double get_vertexX() 
  { 
    PHOOL_VIRTUAL_WARN("get_vertexX()"); 
    return -9999.;
  }

  /*! Get the vertex position in Y */
  virtual double get_vertexY() 
  { 
    PHOOL_VIRTUAL_WARN("get_vertexY()"); 
    return -9999.;
  }

  /*! Get the vertex position in Z */
  virtual double get_vertexZ() 
  { 
    PHOOL_VIRTUAL_WARN("get_vertexZ()"); 
    return -9999.;
  }

  /*! Get the vertex resolution in X */
  virtual double get_vertexResolutionX()
  {
    PHOOL_VIRTUAL_WARN("get_vertexResolutionX()");
    return -9999.;
  }

  /*! Get the vertex resolution in Y */
  virtual double get_vertexResolutionY()
  {
    PHOOL_VIRTUAL_WARN("get_vertexResolutionY()");
    return -9999.;
  }

  /*! Get the vertex resolution in Z */
  virtual double get_vertexResolutionZ()
  {
    PHOOL_VIRTUAL_WARN("get_vertexResolutionZ()");
    return -9999.;
  }

  /*! Get the number of segments used in the vertex calculation */
  virtual unsigned int get_nUsedSegments() 
  { 
    PHOOL_VIRTUAL_WARN("get_nUsedSegments()"); 
    return -9999;
  }

  /*! Get the ID of the ith segment used in the vertex calculationg */
  virtual int get_segmentID(unsigned int i) 
  { 
    PHOOL_VIRTUAL_WARN("get_segmentID()"); 
    return -9999;
  }

  //@}


  ClassDef(SvxPrimVertexInfo, 1);

};

#endif /* __SvxPrimVertexFinder_H__ */