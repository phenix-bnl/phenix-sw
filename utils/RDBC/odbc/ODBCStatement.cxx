// $Id: ODBCStatement.cxx,v 1.1.1.1 2004/02/18 20:58:02 dave Exp $
//*-- Author : Valeriy Onuchin 14/02/2000 
//

/**************************************************************************

   ROOT wrappers of libodbc++ library
    
   Copyright (C) 1999-2000 Manush Dodunekov <manush@stendahls.net>
   
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.
   
   You should have received a copy of the GNU Library General Public License
   along with this library; see the file COPYING.  If not, write to
   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

**************************************************************************/

/////////////////////////////////////////////////////////////////////
//
// The object used for executing a static SQL statement and 
// obtaining the results produced by it. 
// 
// Only one TSQLResultSet per ODBCStatement can be open at any point 
// in time. Therefore, if the reading of one TSQLResultSet is
// interleaved with the reading of another, each must have been 
// generated by different ODBCStatements. All statement execute 
// methods  implicitly close a statment's current TSQLResultSet if 
// an open  one exists. 
//
// See also: 
//    TSQLConnection::CreateStatement(), TSQLResultSet 
//    TSQLCallableStatement TSQLPreparedStatement
//Begin_Html
/*
<P>
   The <TT>ODBCStatement</TT> class encapsulates SQL queries to your database. 
Using several methods, these calls return objects that contain the 
results of your SQL query. When you execute an SQL query, the data 
that is returned to you is commonly called the result set. You can 
choose from several result sets, depending on your needs:
<UL>
<LI><TT>TSQLResultSet* ExecuteQuery( const TString& sqlStatement)<BR></TT>
This method sends the SQL query contained in <TT>sqlStatement</TT>
and returns a single set of results. This method is best used 
in sending  <TT>SELECT</TT> statements. These statements typically 
return a result set. This method implicitly deletes previous resultset.

<LI><TT>Int_t ExecuteUpdate( const TString& sqlStatement )<BR></TT>
This method sends the SQL query contained in <TT>sqlStatement</TT> 
and returns an integer. This method is useful when you send SQL 
<TT>INSERT</TT>s,  <TT>DELETE</TT>s, and <TT>UPDATE</TT>s.  These commands return 
a count of rows  that were affected  by your query. This statement 
should not be used for queries that  return result sets.

<LI><TT>Bool_t Execute( const TString& sqlStatement )<BR></TT>
This method sends the <TT>sqlStatement</TT> to the database and returns 
<TT>kTRUE</TT> if the statement returns a result set or <TT>kFALSE</TT> if the 
statement returns an integer. This method is best used when multiple 
result sets can be returned.
</UL>
<P>
Use the following methods to easily navigate the results a query returns:
<UL>
<LI><TT>Bool_t GetMoreResults()<BR> </TT>
This moves to the next result set in the <TT>ODBCStatement</TT>. This, 
like the <TT>Execute()</TT> method, returns <TT>kTRUE</TT> if the  next 
result is a result set or <TT>kFALSE</TT> if it is an  integer.  
If you have  already retrieved a <TT>TSQLResultSet</TT> from  the  
<TT>ODBCStatement</TT>, this method will close it before returning.

<LI><TT>TSQLResultSet* GetResultSet()<BR></TT>
This method returns to you a result set in a <TT>TSQLResultSet</TT> 
object. This result set is the current result set.

<LI><TT>Int_t GetUpdateCount()<BR></TT>
This method returns to you the integer result that an 
<TT>Execute()</TT> method returned.
</UL>
<P>
*/
//End_Html
/////////////////////////////////////////////////////////////////////

#include "ODBCStatement.h"
#include "ODBCResultSet.h"
#include <RDBC/TSQLConnection.h>
#include <TList.h>
#include <RDBC/odbc++/statement.h>
#include <RDBC/odbc++/resultset.h>

using namespace odbc;

ClassImpQ(ODBCStatement)

/////////////////////////////////////////////////////////////////////
//___________________________________________________________________
ODBCStatement::ODBCStatement( TSQLConnection* con, void* imp ):
      TSQLStatement(con,imp)
{
   // ctor
}

//___________________________________________________________________
ODBCStatement::~ODBCStatement( )
{
   // Destructor.
   //
   //  Note: When a ODBCStatement is closed, its current 
   //       TSQLResultSet,  if one exists, is also closed.
   // 
 
   odbc::Statement* imp = (odbc::Statement*)fImp;

   try { 
      if(imp) delete imp;
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
   }

   // implementation part of fCurrentResult is deleted with statement
   if(fCurrentResult) ((ODBCResultSet*)fCurrentResult)->fImp = 0;
   fImp = 0;
}

//___________________________________________________________________
TSQLResultSet* ODBCStatement::ExecuteQuery( const TString& sql )
{
   // Executes a SQL statement that returns a single TSQLResultSet
   //
   // This method also implicitly closes current TSQLResultSet 
   //
   // Returns:
   //       a TSLResultSet that contains the data produced by the query; 
   //       NULL - in case of error
   //
   //   Throws:
   //       TSQLException - if a database access error occurs

   if(!fImp) { Destroyed(); return 0; }
   odbc::Statement* stmt = (odbc::Statement*)fImp;
   odbc::ResultSet* rs = 0; 
   ClearWarnings();

   if(fCurrentResult)  { delete fCurrentResult; fCurrentResult = 0; }

   try {
      rs = stmt->executeQuery(ODBCXX_STRING_C(sql.Data()));
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
      if(rs) delete rs;
      fCurrentResult = 0;
      return 0;
   }
   
   return fCurrentResult = new ODBCResultSet(this,(void*)rs);;
}

//___________________________________________________________________
Bool_t ODBCStatement::Execute( const TString& sql )
{
   // Executes a SQL statement that may return multiple results.
   // Under some (uncommon) situations a single SQL statement may 
   // return multiple result sets and/or update counts. Normally you 
   // can ignore this unless you are (1) executing a stored
   // procedure that you know may return multiple results or (2) you 
   // are dynamically executing an unknown SQL string. The methods 
   // execute, GetMoreResults(), GetResultSet(), and GetUpdateCount()
   // let you navigate through multiple results.
   //  The execute method executes a SQL statement and indicates the 
   // form of the first result. You can then use GetResultSet() or
   // GetUpdateCount() to retrieve the result, and GetMoreResults() 
   // to move to any subsequent result(s).
   //
   // Parameters:
   //          sql - any SQL statement
   // Returns:
   //          kTRUE if the next result is a TSQLResultSet; 
   //          kFALSE if it is an update count or there are no more
   //          results
   // Throws:
   //            TSQLException - if a database access error occurs
   // See Also: 
   //       GetResultSet(), GetUpdateCount(), GetMoreResults()
  
   if(!fImp) { Destroyed(); return kFALSE; }

   Bool_t return_value = kFALSE;
   ClearWarnings();
   odbc::Statement* stmt = (odbc::Statement*)fImp;

   if(fCurrentResult) { delete fCurrentResult; fCurrentResult = 0; }
 
   try {
      return_value = (Bool_t)stmt->execute(ODBCXX_STRING_C(sql.Data()));  
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
      return_value = kFALSE;
   }
   return return_value;
}

//___________________________________________________________________
Int_t ODBCStatement::ExecuteUpdate( const TString& sql )
{
   // Executes an SQL INSERT, UPDATE or DELETE statement. 
   // In addition, SQL statements that return nothing, 
   // such as SQL DDL statements, can be executed.
   //
   //  Parameters:
   //      sql - a SQL INSERT, UPDATE or DELETE statement or 
   //            a SQL statement that  returns nothing
   //
   //  Returns:
   //      either the row count for INSERT, UPDATE or DELETE or 
   //      0 for SQL statements that return nothing
   //  Throws:
   //      TSQLException - if a database access error occurs
   
   if(!fImp) { Destroyed(); return 0; }

   Int_t return_value = 0;
   ClearWarnings();
   odbc::Statement* stmt = (odbc::Statement*)fImp;

   if(fCurrentResult)  { delete fCurrentResult; fCurrentResult = 0; }

   try {
      return_value = stmt->executeUpdate(ODBCXX_STRING_C(sql.Data()));      
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
      return_value = 0;
   }
   return return_value;
}

//___________________________________________________________________
TSQLResultSet* ODBCStatement::GetResultSet()
{
   // Returns the current result as a TSQLResultSet object. 
   // This method should be called only once per result.
   //
   // This method also implicitly closes any current TSQLResultSet 
   //   
   // Returns:
   //       the current result as a TSQLResultSet; null if the result 
   //       is an update count or there are no more results
   // Throws:
   //       TSQLException - if a database access error occurs
   // See Also: 
   //       Execute(const TString&)
   
   if(!fImp) { Destroyed(); return 0; }
   odbc::ResultSet* rs = 0;
   odbc::Statement* stmt = (odbc::Statement*)fImp;

   if(fCurrentResult) { delete fCurrentResult; fCurrentResult = 0; }

   try {     
      rs = stmt->getResultSet(); 
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
      if(rs) delete rs;
      fCurrentResult = 0;
      return 0;
   }

   return fCurrentResult = new ODBCResultSet(this,(void*)rs); 
}   

//___________________________________________________________________
Int_t ODBCStatement::GetUpdateCount()
{
   // Returns the current result as an update count; 
   // if there are no more results, -1 is returned.
   // This method should be called only once per result.
   //
   // Returns:
   //       the current result as an update count; -1 if it is a 
   //       TSQLResultSet or there are no more results
   // Throws:
   //       TSQLException - if a database access error occurs
   // See Also: 
   //       Execute(const TString&)
   
   if(!fImp) { Destroyed(); return 0; }

   Int_t return_value = 0;
   odbc::Statement* stmt = (odbc::Statement*)fImp;
      
   try {
      return_value = stmt->getUpdateCount();
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
      return  0;
   }
   return return_value;
}

//___________________________________________________________________
Bool_t ODBCStatement::GetMoreResults()
{
   // Moves to a ODBCStatement's next result. It returns kTRUE if 
   // this result is a TSQLResultSet. 
   // 
   // There are no more results when 
   //       (!GetMoreResults() && (GetUpdateCount() == -1)
   //
   // Returns:
   //    kTRUE if the next result is a TSQLResultSet; 
   //    kFALSE if it is an update count or there are no more results
   //
   // Throws:
   //       TSQLException - if a database access error occurs
   // See Also: 
   //       Execute(const TString&)

   Bool_t return_value = kFALSE;
   
   if(!fImp) { Destroyed(); return return_value; }
   odbc::Statement* stmt = (odbc::Statement*)fImp;
      
   try {
      return_value = (Bool_t)stmt->getMoreResults();      
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
      return  kFALSE;
   }
   return return_value;
}

//___________________________________________________________________
Int_t ODBCStatement::GetMaxFieldSize()
{
   // Returns the maximum number of bytes allowed for any column 
   // value. This limit is the maximum number of bytes that can be
   // returned for any column value. The limit applies only to 
   // kBINARY, kVARBINARY, kLONGVARBINARY, kCHAR, kVARCHAR, and 
   // kLONGVARCHAR columns (see TSQLTypes.h). If the limit is exceeded, 
   // the excess data  is silently discarded.
   //
   // Returns:
   //    the current max column size limit; zero means unlimited
   // Throws:
   //    TSQLException - if a database access error occurs

   if(!fImp) { Destroyed(); return 0; }

   Int_t return_value = 0;
   odbc::Statement* stmt = (odbc::Statement*)fImp;
   
   try {
      return_value = stmt->getMaxFieldSize();
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
      return 0;
   }
   return return_value;
}

//___________________________________________________________________
void ODBCStatement::SetMaxFieldSize( Int_t max )
{
   // Sets the limit for the maximum number of bytes in a column to 
   // the given number of bytes. This is the maximum number of bytes 
   // that can be returned for any column value. This limit applies 
   // only to kBINARY, kVARBINARY, kLONGVARBINARY, kCHAR, kVARCHAR,
   // and kLONGVARCHAR fields (see TSQLTypes.h) . If the limit is exceeded, 
   // the excess  data is silently discarded. For maximum portability, 
   // use values greater than 256.
   //
   // Parameters:
   //       max - the new max column size limit; zero means unlimited
   // Throws:
   //       TSQLException - if a database access error occurs
   
   if(!fImp) { Destroyed(); return; }
   odbc::Statement* stmt = (odbc::Statement*)fImp;
   
   try {
      stmt->setMaxFieldSize(max);   
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
   }
}

//___________________________________________________________________
Int_t ODBCStatement::GetMaxRows()
{
   // Retrieves the maximum number of rows that a TSQLResultSet can 
   // contain. If the limit is exceeded, the excess rows are silently 
   // dropped.
   //
   // Returns:
   //       the current max row limit; zero means unlimited
   // Throws:
   //       TSQLException - if a database access error occurs

   if(!fImp) { Destroyed(); return 0; }

   Int_t return_value = 0;
   odbc::Statement* stmt = (odbc::Statement*)fImp;
   
   try {
      return_value = stmt->getMaxRows();
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
      return 0;
   }
   return return_value;
}

//___________________________________________________________________
void ODBCStatement::SetMaxRows( Int_t max )
{
   // Sets the limit for the maximum number of rows that any 
   // TSQLResultSet can contain to the given number. If the limit is 
   // exceeded, the excess rows are silently dropped.
   //
   // Parameters:
   //       max - the new max rows limit; zero means unlimited
   // Throws:
   //       TSQLException - if a database access error occurs
   
   if(!fImp) { Destroyed(); return; }
   odbc::Statement* stmt = (odbc::Statement*)fImp;
   
   try {
      stmt->setMaxRows(max);  
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
   }
}

//___________________________________________________________________
void ODBCStatement::SetEscapeProcessing( Bool_t enable )
{
   // Sets escape processing on or off. If escape scanning is on 
   // (the default), the driver will do escape substitution before 
   // sending the SQL to the database.
   // 
   // Note:
   //    Since prepared statements have usually been parsed prior to 
   //    making this call, disabling escape processing for prepared 
   //    statements will have no effect.
   //
   // Parameters:
   //       enable - kTRUE to enable; kFALSE to disable
   // Throws:
   //       TSQLException - if a database access error occurs
   
   if(!fImp) { Destroyed(); return; }
   odbc::Statement* stmt = (odbc::Statement*)fImp;
   
   try {
      stmt->setEscapeProcessing(enable);
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
   }
}

//___________________________________________________________________
Bool_t ODBCStatement::GetEscapeProcessing()
{
   //  Returns if escape processing is on or off. 
   // If escape scanning is on (the default), the driver will do escape 
   // substitution before  sending the SQL to the database.
   // 
   // Note:
   //    Since prepared statements have usually been parsed prior to 
   //    making this call, disabling escape processing for prepared 
   //    statements will have no effect.
   //
   // Parameters:
   //       enable - kTRUE to enable; kFALSE to disable
   // Throws:
   //       TSQLException - if a database access error occurs

   if(!fImp) { Destroyed(); return kFALSE; }   

   Bool_t return_value = kFALSE;
   odbc::Statement* stmt = (odbc::Statement*)fImp;
   
   try {
      return_value = stmt->getEscapeProcessing();
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
      return kFALSE;
   }
   return return_value;
}

//___________________________________________________________________
Int_t ODBCStatement::GetQueryTimeout()
{
   // Retrieves the number of seconds the driver will wait for a
   // ODBCStatement to execute. If the limit is exceeded, a 
   // TSQLException is thrown.
   //
   // Returns:
   //    the current query timeout limit in seconds; zero means
   //    unlimited
   // Throws:
   //    TSQLException - if a database access error occurs
   
   Int_t return_value = 0;
   
   if(!fImp) { Destroyed(); return return_value; }
   odbc::Statement* stmt = (odbc::Statement*)fImp;
      
   try {
      return_value = stmt->getQueryTimeout();
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
      return 0;
   }
   return return_value;
}

//___________________________________________________________________
void ODBCStatement::SetQueryTimeout( Int_t seconds )
{
   // Sets the number of seconds the driver will wait for a 
   // ODBCStatement to execute to the given number of seconds. 
   // If the limit is exceeded, a TSQLException is thrown.
   //
   // Parameters:
   //          seconds - the new query timeout limit in seconds; 
   //          zero means unlimited
   // Throws:
   //          TSQLException - if a database access error occurs
   
   if(!fImp) { Destroyed(); return; }
   odbc::Statement* stmt = (odbc::Statement*)fImp;
   
   try {
      stmt->setQueryTimeout(seconds);
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
   }
}

//___________________________________________________________________
void ODBCStatement::Cancel() 
{
   // Cancels this statement object if both the DBMS and driver 
   // support aborting an SQL statement. This method can be used by 
   // one thread to cancel a statement that is being executed by 
   // another thread.
   //
   // Throws:
   //       TSQLException - if a database access error occurs
   
   if(!fImp) { Destroyed(); return; }
   odbc::Statement* stmt = (odbc::Statement*)fImp;
   
   try {
      stmt->cancel();
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
   }
}

//___________________________________________________________________
void ODBCStatement::Close()
{
   // Avoid using this method. Use delete ODBCStatement instead.
   //
   //  Note: When a ODBCStatement is closed, its current 
   //       TSQLResultSet,  if one exists, is also closed.
   //
   //     Throws:
   //      TSQLException - if a database access error occurs
   
   if(!fImp) { Destroyed(); return; }
          
   try {    
      if(fCurrentResult)  { 
         delete fCurrentResult;
         fCurrentResult = 0;
      }
      ClearBatch();
      SafeDelete(fBatches);

      odbc::Statement* imp = (odbc::Statement*)fImp;
      if(imp) delete  imp;
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
   }
   fImp = 0;
   Destroyed();
}

//___________________________________________________________________
void ODBCStatement::SetCursorName( const TString& name )
{
   // Defines the SQL cursor name that will be used by subsequent
   // ODBCStatement execute methods. This name can then be used in 
   // SQL positioned update/delete statements to identify the 
   // current row in the TSQLResultSet generated by this statement. 
   // If the database doesn't support positioned update/delete, 
   // this method is a noop. To insure that a cursor has the proper 
   // isolation level to support updates, the cursor's SELECT 
   // statement should be of the form 'SELECT FOR UPDATE ...'. If
   // the 'FOR UPDATE' phrase is omitted, positioned updates may 
   // fail. 
   //
   // Note: By definition, positioned update/delete execution must 
   //    be done by a different ODBCStatement than the one which 
   //    generated the TSQLResultSet being used for positioning.
   //    Also, cursor names must be unique within a connection.
   //
   // Parameters:
   //       name - the new cursor name, which must be unique within
   //       a connection
   // Throws:
   //       TSQLException - if a database access error occurs
   
   if(!fImp) { Destroyed(); return; }
   odbc::Statement* stmt = (odbc::Statement*)fImp;
   
   try {
      stmt->setCursorName(ODBCXX_STRING_C(name.Data()));
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
   }
}

//___________________________________________________________________
void ODBCStatement::SetFetchDirection( Int_t /* direction */ )
{
   // Gives the driver a hint as to the direction in which the
   // rows in a result set will be processed. The hint applies only 
   // to result sets created using this statement object. 
   // The default value is TSQLResultSet::kTYPE_FORWARD_ONLY
   //
   // Note that this method sets the default fetch direction for 
   // result sets generated by this statement object.
   //
   // Parameters:
   //    direction - the initial direction for processing rows
   // Throws:
   //    TSQLException - if a database access error occurs or the 
   //                   given direction is not one of 
   // 
   
   if(!fImp) { Destroyed(); return; }
}

//___________________________________________________________________
Int_t ODBCStatement::GetFetchDirection()
{
   // Retrieves the direction for fetching rows from database
   // tables that is the default for result sets generated from this 
   // statement object. If this statement object has not set 
   // a fetch direction by calling the method SetFetchDirection(), 
   // the return value is implementation-specific.
   //
   // Returns:
   //       the default fetch direction for result sets generated 
   //       from this statement object
   // Throws:
   //       TSQLException - if a database access error occurs

   return 0;
}

//___________________________________________________________________
void ODBCStatement::SetFetchSize( Int_t /* rows */ )
{
   // Gives the driver a hint as to the number of rows that
   // should be fetched from the database when more rows are needed. 
   // The number of rows specified affects only result sets created 
   // using this statement. If the value specified is zero, then the
   // hint is ignored. The default value is zero.
   //
   // Parameters:
   //       rows - the number of rows to fetch
   // Throws:
   //       TSQLException - if a database access error occurs, or 
   //       the condition 0 <= rows <= GetMaxRows() is not satisfied.
   
   if(!fImp) { Destroyed(); return; }
}

//___________________________________________________________________
Int_t ODBCStatement::GetFetchSize()
{
   // Retrieves the number of result set rows that is the default 
   // fetch size for result sets generated from this ODBCStatement 
   // object. If this statement object has not set a fetch size
   // by calling the method SetFetchSize(), the return value is
   // implementation-specific.
   //
   // Returns:
   //       the default fetch size for result sets generated from 
   //       this statement object
   // Throws:
   //       TSQLException - if a database access error occurs
   
   Int_t return_value = 0;
   
   if(!fImp) { Destroyed(); return return_value; }
   odbc::Statement* stmt = (odbc::Statement*)fImp;
   
   try {
      return_value = stmt->getFetchSize();
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
      return 0;
   }
   return return_value;
}

//___________________________________________________________________
Int_t ODBCStatement::GetResultSetConcurrency()
{
   // Retrieves the result set concurrency.
   //
   // enum EResultSetConcurrency{
   //       kCONCUR_READ_ONLY,
   //       kCONCUR_UPDATABLE
   //    };

   Int_t return_value = 0;
   
   if(!fImp) { Destroyed(); return return_value; }
   odbc::Statement* stmt = (odbc::Statement*)fImp;
   
   try {
      return_value = stmt->getResultSetConcurrency();
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
      return 0;
   }   
   return return_value;
}

//___________________________________________________________________
Int_t ODBCStatement::GetResultSetType()
{
   // Determine the result set type.
   //
   // enum EResultSetType{
   //       kTYPE_FORWARD_ONLY,
   //       kTYPE_SCROLL_INSENSITIVE,
   //       kTYPE_SCROLL_SENSITIVE
   //       };
   //
     
   Int_t return_value = 0;
   
   if(!fImp) { Destroyed(); return return_value; }
   odbc::Statement* stmt = (odbc::Statement*)fImp;
   
   try {
      return_value = stmt->getResultSetType(); 
   } catch(odbc::SQLException& e) {
      Throw( new TSQLException( ODBCXX_STRING_CSTR(e.getMessage()),
                                ODBCXX_STRING_CSTR(e.getSQLState()),
                                e.getErrorCode()) );
      return 0;
   }
   return return_value;
}

//___________________________________________________________________
void ODBCStatement::AddBatch( const TString& sql )
{
   // Adds a SQL command to the current batch of commmands for
   // the statement. This method is optional.
   //
   // Parameters:
   //       sql - typically this is a static SQL INSERT or UPDATE 
   //       statement
   // Throws:
   //       TSQLException - if a database access error occurs, or 
   //       the  driver does not support batch statements

}

//___________________________________________________________________
void ODBCStatement::ClearBatch()
{
   // Makes the set of commands in the current batch empty. This
   // method is optional.
   //
   // Throws:
   //       TSQLException - if a database access error occurs or 
   //       the driver does not support batch statements

}

//___________________________________________________________________
Int_t* ODBCStatement::ExecuteBatch()
{
   // Submits a batch of commands to the database for execution.
   // This method is optional.
   //
   // Returns:
   //       an array of update counts containing one element for 
   //       each command in the batch. The array is ordered 
   //       according  to the order in which commands were inserted 
   //       into the  batch.
   //
   // Throws:
   //       TSQLException - if a database access error occurs or 
   //       the driver  does not support batch statements

   return 0;
}
