/*********************************************************************
 *
 * This is based on code created by Peter Harvey,
 * (pharvey@codebydesign.com).
 *
 * Modified and extended by Nick Gorham
 * (nick@easysoft.com).
 *
 * Any bugs or problems should be considered the fault of Nick and not
 * Peter.
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
 * $Id: SQLNativeSqlW.c,v 1.1.1.1 2004/02/18 20:37:42 dave Exp $
 *
 * $Log: SQLNativeSqlW.c,v $
 * Revision 1.1.1.1  2004/02/18 20:37:42  dave
 * first import of unixODBC
 *
 * Revision 1.6  2003/10/30 18:20:46  lurcher
 *
 * Fix broken thread protection
 * Remove SQLNumResultCols after execute, lease S4/S% to driver
 * Fix string overrun in SQLDriverConnect
 * Add initial support for Interix
 *
 * Revision 1.5  2002/12/05 17:44:31  lurcher
 *
 * Display unknown return values in return logging
 *
 * Revision 1.4  2002/08/23 09:42:37  lurcher
 *
 * Fix some build warnings with casts, and a AIX linker mod, to include
 * deplib's on the link line, but not the libtool generated ones
 *
 * Revision 1.3  2002/07/24 08:49:52  lurcher
 *
 * Alter UNICODE support to use iconv for UNICODE-ANSI conversion
 *
 * Revision 1.2  2001/12/13 13:00:32  lurcher
 *
 * Remove most if not all warnings on 64 bit platforms
 * Add support for new MS 3.52 64 bit changes
 * Add override to disable the stopping of tracing
 * Add MAX_ROWS support in postgres driver
 *
 * Revision 1.1.1.1  2001/10/17 16:40:06  lurcher
 *
 * First upload to SourceForge
 *
 * Revision 1.3  2001/04/12 17:43:36  nick
 *
 * Change logging and added autotest to odbctest
 *
 * Revision 1.2  2001/01/02 09:55:04  nick
 *
 * More unicode bits
 *
 * Revision 1.1  2000/12/31 20:30:54  nick
 *
 * Add UNICODE support
 *
 *
 **********************************************************************/

#include "drivermanager.h"

static char const rcsid[]= "$RCSfile: SQLNativeSqlW.c,v $";

SQLRETURN SQLNativeSqlW(
    SQLHDBC            hdbc,
    SQLWCHAR            *sz_sql_str_in,
    SQLINTEGER         cb_sql_str_in,
    SQLWCHAR            *sz_sql_str,
    SQLINTEGER         cb_sql_str_max,
    SQLINTEGER         *pcb_sql_str )
{
    DMHDBC connection = (DMHDBC)hdbc;
    SQLRETURN ret;
    SQLCHAR *s1;
    SQLCHAR s2[ 100 + LOG_MESSAGE_LEN ];

    /*
     * check connection
     */

    if ( !__validate_dbc( connection ))
    {
        dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: SQL_INVALID_HANDLE" );

        return SQL_INVALID_HANDLE;
    }

    function_entry( connection );

    if ( log_info.log_flag )
    {
        /*
         * allocate some space for the buffer
         */

        if ( sz_sql_str_in && cb_sql_str_in == SQL_NTS )
        {
            s1 = malloc(( wide_strlen( sz_sql_str_in ) * 2 ) + 100 );
        }
        else if ( sz_sql_str_in )
        {
            s1 = malloc( cb_sql_str_in + 100 );
        }
        else
        {
            s1 = malloc( 101 );
        }

        sprintf( connection -> msg, "\n\t\tEntry:\
            \n\t\t\tConnection = %p\
            \n\t\t\tSQL In = %s\
            \n\t\t\tSQL Out = %p\
            \n\t\t\tSQL Out Len = %d\
            \n\t\t\tSQL Len Ptr = %p",
                connection,
                __wstring_with_length( s1, sz_sql_str_in, cb_sql_str_in ),
                sz_sql_str,
                (int)cb_sql_str_max,
                pcb_sql_str );

        free( s1 );

        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                connection -> msg );
    }

    thread_protect( SQL_HANDLE_DBC, connection );

    if ( !sz_sql_str_in )
    {
        __post_internal_error( &connection -> error,
                ERROR_HY009, NULL,
                connection -> environment -> requested_version );

        return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
    }

    if ( cb_sql_str_in < 0 &&
            cb_sql_str_in != SQL_NTS )
    {
        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                "Error: HY090" );

        __post_internal_error( &connection -> error,
                ERROR_HY090, NULL,
                connection -> environment -> requested_version );

        return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
    }

    if ( sz_sql_str &&
            cb_sql_str_max < 0 )
    {
        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                "Error: HY090" );

        __post_internal_error( &connection -> error,
                ERROR_HY090, NULL,
                connection -> environment -> requested_version );

        return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
    }

    if ( connection -> state == STATE_C2 ||
            connection -> state == STATE_C3 )
    {
        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                "Error: 08003" );

        __post_internal_error( &connection -> error,
                ERROR_08003, NULL,
                connection -> environment -> requested_version );

        return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
    }

    if ( connection -> unicode_driver )
    {
        if ( !CHECK_SQLNATIVESQLW( connection ))
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: IM001" );

            __post_internal_error( &connection -> error,
                    ERROR_IM001, NULL,
                    connection -> environment -> requested_version );

            return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
        }

        ret = SQLNATIVESQLW( connection,
                connection -> driver_dbc,
                sz_sql_str_in,
                cb_sql_str_in,
                sz_sql_str,
                cb_sql_str_max,
                pcb_sql_str );
    }
    else
    {
        SQLCHAR *as1 = NULL, *as2 = NULL;

        if ( !CHECK_SQLNATIVESQL( connection ))
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: IM001" );

            __post_internal_error( &connection -> error,
                    ERROR_IM001, NULL,
                    connection -> environment -> requested_version );

            return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
        }

        as1 = (SQLCHAR*) unicode_to_ansi_alloc( sz_sql_str_in, cb_sql_str_in, connection );

        if ( cb_sql_str_max > 0 && sz_sql_str )
        {
            as2 = malloc( cb_sql_str_max + 1 );
        }

        ret = SQLNATIVESQL( connection,
                connection -> driver_dbc,
                as1 ? as1 : (SQLCHAR*) sz_sql_str_in,
                cb_sql_str_in,
                as2 ? as2 : (SQLCHAR*) sz_sql_str,
                cb_sql_str_max,
                pcb_sql_str );

        if ( SQL_SUCCEEDED( ret ) && as2 && sz_sql_str )
        {
            ansi_to_unicode_copy( sz_sql_str, (char*) as2, SQL_NTS, connection );
        }

        if ( as1 ) free( as1 );
        if ( as2 ) free( as2 );
    }

    if ( log_info.log_flag )
    {
        /*
         * allocate some space for the buffer
         */

        if ( sz_sql_str && pcb_sql_str && *pcb_sql_str == SQL_NTS )
        {
            s1 = malloc( wide_strlen( sz_sql_str ) * 2 + 100 );
        }
        else if ( sz_sql_str &&  pcb_sql_str )
        {
            s1 = malloc( *pcb_sql_str + 100 );
        }
        else if ( sz_sql_str )
        {
            s1 = malloc( wide_strlen( sz_sql_str ) * 2 + 100 );
        }
        else
        {
            s1 = malloc( 101 );
        }

        sprintf( connection -> msg, 
                "\n\t\tExit:[%s]\
                \n\t\t\tSQL Out = %s",
                    __get_return_status( ret, s2 ),
                    __data_as_string( s1, SQL_CHAR, 
                        (SQLLEN*)pcb_sql_str, sz_sql_str ));

        free( s1 );

        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                connection -> msg );
    }

    return function_return( SQL_HANDLE_DBC, connection, ret );
}
