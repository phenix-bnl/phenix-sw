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
 * $Id: SQLForeignKeysW.c,v 1.1.1.2 2004/04/01 16:48:09 dave Exp $
 *
 * $Log: SQLForeignKeysW.c,v $
 * Revision 1.1.1.2  2004/04/01 16:48:09  dave
 * import of v2.2.8
 *
 * Revision 1.6  2004/01/12 09:54:39  lurcher
 *
 * Fix problem where STATE_S5 stops metadata calls
 *
 * Revision 1.5  2003/10/30 18:20:45  lurcher
 *
 * Fix broken thread protection
 * Remove SQLNumResultCols after execute, lease S4/S% to driver
 * Fix string overrun in SQLDriverConnect
 * Add initial support for Interix
 *
 * Revision 1.4  2002/12/05 17:44:30  lurcher
 *
 * Display unknown return values in return logging
 *
 * Revision 1.3  2002/08/23 09:42:37  lurcher
 *
 * Fix some build warnings with casts, and a AIX linker mod, to include
 * deplib's on the link line, but not the libtool generated ones
 *
 * Revision 1.2  2002/07/24 08:49:52  lurcher
 *
 * Alter UNICODE support to use iconv for UNICODE-ANSI conversion
 *
 * Revision 1.1.1.1  2001/10/17 16:40:05  lurcher
 *
 * First upload to SourceForge
 *
 * Revision 1.3  2001/07/03 09:30:41  nick
 *
 * Add ability to alter size of displayed message in the log
 *
 * Revision 1.2  2001/04/12 17:43:36  nick
 *
 * Change logging and added autotest to odbctest
 *
 * Revision 1.1  2000/12/31 20:30:54  nick
 *
 * Add UNICODE support
 *
 *
 **********************************************************************/

#include "drivermanager.h"

static char const rcsid[]= "$RCSfile: SQLForeignKeysW.c,v $";

SQLRETURN SQLForeignKeysW(
    SQLHSTMT           statement_handle,
    SQLWCHAR            *szpk_catalog_name,
    SQLSMALLINT        cbpk_catalog_name,
    SQLWCHAR            *szpk_schema_name,
    SQLSMALLINT        cbpk_schema_name,
    SQLWCHAR            *szpk_table_name,
    SQLSMALLINT        cbpk_table_name,
    SQLWCHAR            *szfk_catalog_name,
    SQLSMALLINT        cbfk_catalog_name,
    SQLWCHAR            *szfk_schema_name,
    SQLSMALLINT        cbfk_schema_name,
    SQLWCHAR            *szfk_table_name,
    SQLSMALLINT        cbfk_table_name )
{
    DMHSTMT statement = (DMHSTMT) statement_handle;
    SQLRETURN ret;
    SQLCHAR s1[ 100 + LOG_MESSAGE_LEN ], s2[ 100 + LOG_MESSAGE_LEN ], s3[ 100 + LOG_MESSAGE_LEN ], s4[ 100 + LOG_MESSAGE_LEN ];
    SQLCHAR s5[ 100 + LOG_MESSAGE_LEN ], s6[ 100 + LOG_MESSAGE_LEN ];

    /*
     * check statement
     */

    if ( !__validate_stmt( statement ))
    {
        dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: SQL_INVALID_HANDLE" );

        return SQL_INVALID_HANDLE;
    }
    
    function_entry( statement );

    if ( log_info.log_flag )
    {
        sprintf( statement -> msg, "\n\t\tEntry:\
            \n\t\t\tStatement = %p\
            \n\t\t\tPK Catalog Name = %s\
            \n\t\t\tPK Schema Name = %s\
            \n\t\t\tPK Table Name = %s\
            \n\t\t\tFK Catalog Name = %s\
            \n\t\t\tFK Schema Name = %s\
            \n\t\t\tFK Table Name = %s",
                statement,
                __wstring_with_length( s1, szpk_catalog_name, cbpk_catalog_name ), 
                __wstring_with_length( s2, szpk_schema_name, cbpk_schema_name ), 
                __wstring_with_length( s3, szpk_table_name, cbpk_table_name ), 
                __wstring_with_length( s4, szfk_catalog_name, cbfk_catalog_name ), 
                __wstring_with_length( s5, szfk_schema_name, cbfk_schema_name ), 
                __wstring_with_length( s6, szfk_table_name, cbfk_table_name ));

        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                statement -> msg );
    }

    thread_protect( SQL_HANDLE_STMT, statement );

    if (( cbpk_catalog_name < 0 && cbpk_catalog_name != SQL_NTS ) ||
            ( cbpk_schema_name < 0 && cbpk_schema_name != SQL_NTS ) ||
            ( cbpk_table_name < 0 && cbpk_table_name != SQL_NTS ) ||
            ( cbfk_catalog_name < 0 && cbfk_catalog_name != SQL_NTS ) ||
            ( cbfk_schema_name < 0 && cbfk_schema_name != SQL_NTS ) ||
            ( cbfk_table_name < 0 && cbfk_table_name != SQL_NTS ))
    {
        __post_internal_error( &statement -> error,
                ERROR_HY090, NULL,
                statement -> connection -> environment -> requested_version );

        return function_return( SQL_HANDLE_STMT, statement, SQL_ERROR );
    }

    /*
     * check states
     */

#ifdef NR_PROBE
    if ( statement -> state == STATE_S5 ||
            statement -> state == STATE_S6 ||
            statement -> state == STATE_S7 )
#else
    if ( statement -> state == STATE_S6 ||
            statement -> state == STATE_S7 )
#endif
    {
        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                "Error: 24000" );

        __post_internal_error( &statement -> error,
                ERROR_24000, NULL,
                statement -> connection -> environment -> requested_version );

        return function_return( SQL_HANDLE_STMT, statement, SQL_ERROR ); 
    }
    else if ( statement -> state == STATE_S8 ||
            statement -> state == STATE_S9 ||
            statement -> state == STATE_S10 )
    {
        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                "Error: HY010" );

        __post_internal_error( &statement -> error,
                ERROR_HY010, NULL,
                statement -> connection -> environment -> requested_version );

        return function_return( SQL_HANDLE_STMT, statement, SQL_ERROR );
    }

    if ( statement -> state == STATE_S11 ||
            statement -> state == STATE_S12 )
    {
        if ( statement -> interupted_func != SQL_API_SQLFOREIGNKEYS )
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: HY010" );

            __post_internal_error( &statement -> error,
                    ERROR_HY010, NULL,
                    statement -> connection -> environment -> requested_version );

            return function_return( SQL_HANDLE_STMT, statement, SQL_ERROR );
        }
    }

    /*
     * TO_DO Check the SQL_ATTR_METADATA_ID settings
     */

    if ( statement -> connection -> unicode_driver )
    {
        if ( !CHECK_SQLFOREIGNKEYSW( statement -> connection ))
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: IM001" );

            __post_internal_error( &statement -> error,
                    ERROR_IM001, NULL,
                    statement -> connection -> environment -> requested_version );

            return function_return( SQL_HANDLE_STMT, statement, SQL_ERROR );
        }

        ret = SQLFOREIGNKEYSW( statement -> connection ,
                statement -> driver_stmt,
                szpk_catalog_name,
                cbpk_catalog_name,
                szpk_schema_name,
                cbpk_schema_name,
                szpk_table_name,
                cbpk_table_name,
                szfk_catalog_name,
                cbfk_catalog_name,
                szfk_schema_name,
                cbfk_schema_name,
                szfk_table_name,
                cbfk_table_name );
    }
    else
    {
        SQLCHAR *as1, *as2, *as3, *as4, *as5, *as6;

        if ( !CHECK_SQLFOREIGNKEYS( statement -> connection ))
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: IM001" );

            __post_internal_error( &statement -> error,
                    ERROR_IM001, NULL,
                    statement -> connection -> environment -> requested_version );

            return function_return( SQL_HANDLE_STMT, statement, SQL_ERROR );
        }

        as1 = (SQLCHAR*) unicode_to_ansi_alloc( szpk_catalog_name, cbpk_catalog_name, statement -> connection );
        as2 = (SQLCHAR*) unicode_to_ansi_alloc( szpk_schema_name, cbpk_schema_name, statement -> connection );
        as3 = (SQLCHAR*) unicode_to_ansi_alloc( szpk_table_name, cbpk_table_name, statement -> connection );
        as4 = (SQLCHAR*) unicode_to_ansi_alloc( szfk_catalog_name, cbfk_catalog_name, statement -> connection );
        as5 = (SQLCHAR*) unicode_to_ansi_alloc( szfk_schema_name, cbfk_schema_name, statement -> connection );
        as6 = (SQLCHAR*) unicode_to_ansi_alloc( szfk_table_name, cbfk_table_name, statement -> connection );

        ret = SQLFOREIGNKEYS( statement -> connection ,
                statement -> driver_stmt,
                as1,
                cbpk_catalog_name,
                as2,
                cbpk_schema_name,
                as3,
                cbpk_table_name,
                as4,
                cbfk_catalog_name,
                as5,
                cbfk_schema_name,
                as6,
                cbfk_table_name );

        if ( as1 ) free( as1 );
        if ( as2 ) free( as2 );
        if ( as3 ) free( as3 );
        if ( as4 ) free( as4 );
        if ( as5 ) free( as5 );
        if ( as6 ) free( as6 );
    }

    if ( SQL_SUCCEEDED( ret ))
    {
#ifdef NR_PROBE
		/********
		 * Added this to get num cols from drivers which can only tell
		 * us after execute - PAH
		 */

        /*
         * grab any errors
         */

        if ( ret == SQL_SUCCESS_WITH_INFO )
        {
            function_return_ex( SQL_HANDLE_STMT, statement, ret, TRUE );
        }

        SQLNUMRESULTCOLS( statement -> connection,
                statement -> driver_stmt, &statement -> numcols );
		/******/
#endif
        statement -> hascols = 1;
        statement -> state = STATE_S5;
        statement -> prepared = 0;
    }
    else if ( ret == SQL_STILL_EXECUTING )
    {
        statement -> interupted_func = SQL_API_SQLFOREIGNKEYS;
        if ( statement -> state != STATE_S11 &&
                statement -> state != STATE_S12 )
            statement -> state = STATE_S11;
    }
    else
    {
        statement -> state = STATE_S1;
    }

    if ( log_info.log_flag )
    {
        sprintf( statement -> msg, 
                "\n\t\tExit:[%s]",
                    __get_return_status( ret, s1 ));

        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                statement -> msg );
    }

    return function_return( SQL_HANDLE_STMT, statement, ret );
}
