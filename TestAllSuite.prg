//-- copyright
// hbunit is a unit-testing framework for the Harbour language.
//
// Copyright (C) 2019 Manuel Calero Solis <manuelcalerosolis _at_ gmail _dot_ com>
//
// Based on hbunit from Enderson maia <endersonmaia _at_ gmail _dot_ com>
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// See COPYRIGHT for more details.
//++

#include "hbunit.ch"

//---------------------------------------------------------------------------//

CLASS TestAssert FROM TestCase

   METHOD testAssertErrors()
   METHOD testLogicals()
   METHOD testAssertErrors()
   METHOD testAssertEquals()
   METHOD testAssertNotEquals()
   METHOD testAssertNull()
   METHOD testAssertNotNull()

ENDCLASS

//---------------------------------------------------------------------------//

METHOD testAssertErrors() CLASS TestAssert

   local a
   local oError

   BEGIN SEQUENCE
      a := 1/0
      ::fail( "division by zero not caught" )
   RECOVER USING oError
   END

   BEGIN SEQUENCE
      ::assert( a, "test variable not found" )
      ::Assert():fail( "unable to catch 'Variable not found'" )
   RECOVER
   END


RETURN ( nil )

//---------------------------------------------------------------------------//

METHOD testLogicals() CLASS TestAssert

   ::Assert():true( .t., "test ::Assert():true with .t." )
   ::Assert():false( .f., "test ::Assert():false with .f." )

RETURN ( nil )

//---------------------------------------------------------------------------//

METHOD testAssertEquals() CLASS TestAssert

   // test with nil
   ::Assert():equals( nil, nil, "test ::Assert():equals nil with nil" )

   // test with logicals
   ::Assert():equals( .t., .t., "test ::Assert():equals with logical .t." )
   ::Assert():equals( .f., .f., "test ::Assert():equals with logical .f." )

   // test with characters
   ::Assert():equals( "", '', "test ::Assert():equals with empty string" )
   ::Assert():equals( " ", ' ', "test ::Assert():equals with single space" )
   ::Assert():equals( "a", 'a', "test ::Assert():equals with single character" )

   // test with numerics
   ::Assert():equals( 0, 0, "test ::Assert():equals on small integers" )
   ::Assert():equals( 1234567890, 1234567890.0, "test ::Assert():equals on large integers" )
   ::Assert():equals( -2, -2, "test ::Assert():equals on small negative integers" )
   ::Assert():equals( -2342342342342, -2342342342342.0, "test ::Assert():equals on large negative integers" )
   ::Assert():equals( 0.1, 0.1, "test ::Assert():equals on single decimal float" )
   ::Assert():equals( 0.12345678, 0.123456780, "test ::Assert():equals on multiple decimal floats" )
   ::Assert():equals( 0.01, 0.010000, "test ::Assert():equals different decimal floats" )

RETURN ( nil )

//---------------------------------------------------------------------------//

METHOD testAssertNotEquals() CLASS TestAssert

   ::Assert():notEquals( 0, 1, "test ::Assert():notEquals on small integers" )
   ::Assert():notEquals( 1234567890, 1234567891, "test ::Assert():notEquals on large integers" )
   ::Assert():notEquals( -2, -3, "test ::Assert():notEquals on small negative integers" )
   ::Assert():notEquals( -23452342342342, -23452342342343, "test ::Assert():notEquals on large negative integers" )

RETURN ( nil )

//---------------------------------------------------------------------------//

METHOD testAssertNull() CLASS TestAssert

   ::Assert():null( , "test ::Assert():notNull with empty parameter" )
   ::Assert():null( nil, "test ::Assert():notNull with coded nil" )

RETURN ( nil )

//---------------------------------------------------------------------------//

METHOD testAssertNotNull() CLASS TestAssert

   ::Assert():notNull( 1, "test ::Assert():notNull on numerics" )
   ::Assert():notNull( 'a', "test ::Assert():notNull with character" )
   ::Assert():notNull( date(), "test ::Assert():notNull with date" )
   ::Assert():notNull( .f., "test ::Assert():notNull with logical" )
   ::Assert():notNull( array(3), "test ::Assert():notNull with array" )
   ::Assert():notNull( {|| nil }, "test ::Assert():notNull with codeblock" )

RETURN ( nil )

//---------------------------------------------------------------------------//
