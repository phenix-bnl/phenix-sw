// $Id: MUTOO_ROTATION.h,v 1.2 2010/02/12 18:18:39 hpereira Exp $
#ifndef __MUTOO_ROTATION_H__
#define __MUTOO_ROTATION_H__

namespace MUTOO
{

    enum RotationMode
    {
        CLOCKWISE,
        COUNTERCLOCKWISE
    };

    //! static, modifyable rotation mode
    inline RotationMode& _rotation_mode( void )
    {
        static RotationMode _mode = CLOCKWISE;
        return _mode;
    }

    //! rotationMode
    inline void set_rotation_mode( RotationMode mode )
    { _rotation_mode() = mode; }

    //! rotationMode
    inline RotationMode get_rotation_mode( void )
    { return _rotation_mode(); }


}

#endif
