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

CLASS Test

   METHOD New()                        CONSTRUCTOR
   METHOD End()                        VIRTUAL

   METHOD countTestCases()             VIRTUAL
   METHOD Run()                        VIRTUAL

   PROTECTED:
      DATA oResult

ENDCLASS

//---------------------------------------------------------------------------//

METHOD new() CLASS Test

   ::oResult   := TestResult():new()
   
RETURN ( self )

//---------------------------------------------------------------------------//
