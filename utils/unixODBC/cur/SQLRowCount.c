/*********************************************************************
 *
 * unixODBC Cursor Library
 *
 * Created by Nick Gorham
 * (nick@easysoft.com).
 *
 * copyright (c) 1999 Nick Gorham
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 **********************************************************************
 *
 * $Id: SQLRowCount.c,v 1.1.1.1 2004/02/18 20:37:42 dave Exp $
 *
 * $Log: SQLRowCount.c,v $
 * Revision 1.1.1.1  2004/02/18 20:37:42  dave
 * first import of unixODBC
 *
 * Revision 1.2  2001/12/13 13:00:33  lurcher
 *
 * Remove most if not all warnings on 64 bit platforms
 * Add support for new MS 3.52 64 bit changes
 * Add override to disable the stopping of tracing
 * Add MAX_ROWS support in postgres driver
 *
 * Revision 1.1.1.1  2001/10/17 16:40:15  lurcher
 *
 * First upload to SourceForge
 *
 * Revision 1.1.1.1  2000/09/04 16:42:52  nick
 * Imported Sources
 *
 * Revision 1.1  1999/09/19 22:22:50  ngorham
 *
 *
 * Added first cursor library work, read only at the moment and only works
 * with selects with no where clause
 *
 *
 **********************************************************************/

#include "cursorlibrary.h"

SQLRETURN CLRowCount( SQLHSTMT statement_handle,
       SQLINTEGER *rowcount )
{
    CLHSTMT cl_statement = (CLHSTMT) statement_handle; 

    if ( cl_statement -> first_fetch_done )
    {
        if ( rowcount )
        {
            *rowcount = cl_statement -> rowset_count;
        }
        return SQL_SUCCESS;
    }
    else
    {
        return DEF_SQLROWCOUNT( cl_statement -> cl_connection,
            cl_statement -> driver_stmt,
            rowcount );
    }
}
