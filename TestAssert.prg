//-- copyright
// hbunit is a unit-testing framework for the Harbour language.
//
// Copyright (C) 2020 Visionwin Software S.L. <info _at_ gmail _dot_ com>
//
// Based on hbunit from Enderson maia <endersonmaia _at_ gmail _dot_ com>
// Based on hbunit modified from Manuel Calero Solis in 2019 <manuelcalerosolis _at_ gmail _dot_ com>
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

CLASS TAssert

   EXPORTED:

      DATA oResult   AS OBJECT   INIT NIL

      METHOD new( oResult ) CONSTRUCTOR

      METHOD equals( xExp, xAct, cMsg )
      METHOD arrayequals( axExp, axAct, cMsg )
      METHOD objectequals( oExp, oAct, cMsg )
      METHOD isinarray( axExp, uVal, cMsg )
      METHOD notEquals( xExp, xAct, cMsg )
      METHOD true( xAct, cMsg )
      METHOD false( xAct, cMsg )
      METHOD null( xAct, cMsg )
      METHOD notNull( xAct, cMsg )
      METHOD minor( xExp, xAct, cMsg )
      METHOD higher( xExp, xAct, cMsg )

   PROTECTED:
      METHOD isEqual( xExp, xAct )
      METHOD isMinor( xExp, xAct )
      METHOD isHigher( xExp, xAct )
      METHOD assert( xExp, xAct, cMsg, lInvert )
      METHOD fail( cMsg )
      METHOD toStr( xVal, lUseQuote )
      METHOD _hashequals( axExp, axAct, cMsg )
      METHOD _arrayequals( axExp, axAct, cMsg )
      METHOD generateerror( cMSg )

ENDCLASS

//---------------------------------------------------------------------------//

METHOD new( oResult ) CLASS TAssert

   ::oResult      := oResult

RETURN ( self )

//---------------------------------------------------------------------------//

METHOD fail( cMsg ) CLASS TAssert

RETURN ( ::assert( .f.,, "Failure: " + NoMsg( cMsg ) ) )

//---------------------------------------------------------------------------//

METHOD arrayequals( axExp, axAct, cMsg ) CLASS TAssert

   ::equals( Len( axExp ), Len( axAct ), NoMsg( cMsg ) + ' Array with different size' )

   if Len( axExp ) == Len( axAct )

      if ValType( axExp ) == 'H'

         ::_hashequals( axExp, axAct, NoMsg( cMsg ) )

      Else

         ::_arrayequals( axExp, axAct, NoMsg( cMsg ) )

      Endif

   Endif

RETURN ( Nil )

//---------------------------------------------------------------------------//

METHOD objectequals( oExp, oAct, cMsg ) CLASS TAssert

   Local aDatasExp
   Local aDatasAct
   Local aMethodsExp
   Local aMethodsAct

   If oAct == Nil 

      ::oResult:oData:addFailure( ::generateerror( 'Act Object is Nil' ) )

   elseif oExp == Nil 

      ::oResult:oData:addFailure( ::generateerror( 'Exp Object is Nil' ) )

   else

      aDatasExp := __objGetValueList( oExp )
      aDatasAct := __objGetValueList( oAct )
      aMethodsExp := __objGetMethodList( oExp )
      aMethodsAct := __objGetMethodList( oExp )

      ::_arrayequals( aDatasExp, aDatasAct, NoMsg( cMsg ) )
      ::_arrayequals( aMethodsExp, aMethodsAct, NoMsg( cMsg ) )

   Endif

RETURN ( Nil )

//---------------------------------------------------------------------------//

METHOD isinarray( axExp, uVal, cMsg ) CLASS TAssert
Local lIsInArray := .F.

   if aScan( axExp, uVal) == 0

      ::oResult:oData:addFailure( ::generateerror( 'key [ ' + hb_ValToStr( uVal ) + ' ] not exists ') )

   else

      lIsInArray := .T.

   endif

RETURN ( lIsInArray )

//---------------------------------------------------------------------------//

METHOD _arrayequals( axExp, axAct, cMsg ) CLASS TAssert

Local nCounter := 0   

   For nCounter := 1 To Len( axExp )

      If ValType( axExp[ nCounter ] ) == 'A'

         ::arrayequals( axExp[ nCounter ], axAct[ nCounter ], NoMsg( cMsg ) ) 

      else

         ::equals( axExp[ nCounter ], axAct[ nCounter ], NoMsg( cMsg ) )

      Endif

   Next

RETURN ( Nil) 

//---------------------------------------------------------------------------//

METHOD _hashequals( hxExp, hxAct, cMsg ) CLASS TAssert

Local aKeysExp := hb_HKeys( hxExp )
Local aKeysAct := hb_HKeys( hxAct )
Local nCounter := 1

   For nCounter := 1 To Len( aKeysExp )

      if ::isinarray(aKeysExp, aKeysAct[ nCounter ] )

         ::equals( hxExp[ aKeysExp [ nCounter ] ], hxAct[ aKeysExp [ nCounter ] ], ' key[ ' + aKeysExp [ nCounter ] + ' ] '+NoMsg( cMsg ))

      Endif

   Next

RETURN ( Nil )

//---------------------------------------------------------------------------//

METHOD equals( xExp, xAct, cMsg ) CLASS TAssert

local cErrMsg := ""

   cErrMsg += "Exp: " + ::toStr( xExp, .t. )
   cErrMsg += ", Act: " + ::toStr( xAct, .t. )
   cErrMsg += "( " + NoMsg( cMsg ) + " )"

RETURN ( ::assert( xExp, xAct, cErrMsg ) )

//---------------------------------------------------------------------------//

METHOD notEquals( xExp, xAct, cMsg ) CLASS TAssert

local cErrMsg := ""

   cErrMsg += "Exp: not " + ::toStr( xExp, .t. )
   cErrMsg += ", Act: " + ::toStr( xAct )
   cErrMsg += "( " + NoMsg( cMsg ) + " )"

RETURN ( ::assert( xExp, xAct, cErrMsg, .t. ) )

//---------------------------------------------------------------------------//

METHOD true( xAct, cMsg ) CLASS TAssert

local cErrMsg := ""

   cErrMsg += "Exp: .t., Act: "
   cErrMsg += ::toStr( xAct, .t. )
   cErrMsg += "( " + NoMsg( cMsg ) + " )"

RETURN ( ::assert( .t., xAct , cErrMsg ) )

//---------------------------------------------------------------------------//

METHOD false( xAct, cMsg ) CLASS TAssert

local cErrMsg := ""

   cErrMsg += "Exp: .f., Act: "
   cErrMsg += ::toStr( xAct, .t. )
   cErrMsg += "( " + NoMsg( cMsg ) + " )"

RETURN ( ::assert( .f., xAct , cErrMsg ) )

//---------------------------------------------------------------------------//

METHOD null( xAct, cMsg ) CLASS TAssert

local cErrMsg := ""

   cErrMsg += "Exp: nil, Act: "
   cErrMsg += ::toStr( xAct, .t. )
   cErrMsg += "( " + NoMsg( cMsg ) + " )"

RETURN ( ::assert( nil, xAct , cErrMsg ) )

//---------------------------------------------------------------------------//

METHOD notNull( xAct, cMsg ) CLASS TAssert

local cErrMsg := ""

   cErrMsg += "Exp: not nil, Act: "
   cErrMsg += ::toStr( xAct, .t. )
   cErrMsg += "( " + NoMsg( cMsg ) + " )"

RETURN ( ::assert( nil, xAct , cErrMsg, .t. ) )

//---------------------------------------------------------------------------//

METHOD minor( xExp, xAct, cMsg ) CLASS TAssert

local cErrMsg := ""

   cErrMsg += "Exp: " + ::toStr( xExp, .t. )
   cErrMsg += ", Act: " + ::toStr( xAct, .t. )
   cErrMsg += "( " + NoMsg( cMsg ) + " )"
   
RETURN ( ::assert( xExp, xAct, cErrMsg, , .t. ) )

//---------------------------------------------------------------------------//

METHOD higher( xExp, xAct, cMsg ) CLASS TAssert

local cErrMsg := ""

   cErrMsg += "Exp: " + ::toStr( xExp, .t. )
   cErrMsg += ", Act: " + ::toStr( xAct, .t. )
   cErrMsg += "( " + NoMsg( cMsg ) + " )"
   
RETURN ( ::assert( xExp, xAct, cErrMsg, , , .t. ) )


//---------------------------------------------------------------------------//

METHOD assert( xExp, xAct, cMsg, lInvert, lminor, lhigher ) CLASS TAssert

local oError
Local lOk := .F.

   if( lInvert == nil, lInvert := .f., )
   if( lminor == nil, lminor := .f., )
   if( lhigher == nil, lhigher := .f., )

   BEGIN SEQUENCE

      ::oResult:oData:incrementAssertCount()

      If lminor

         lOk := ::isminor( xExp, xAct )

      elseif lhigher

         lOk := ::ishigher( xExp, xAct )

      else

         if ( ( lInvert .and. ::isEqual( xExp, xAct ) ) .or. ( !( lInvert ) .and. ( !( ::isEqual( xExp, xAct ) ) ) ) )

            lOk := .T.

         endif

      endif

      if lOk


         oError := ::generateerror( NoMsg( cMsg ) )
         ::oResult:oData:addFailure( oError )

      else 

         ::oResult:oData:addAssert( NoMsg( cMsg ) )

      endif

   RECOVER USING oError

      ::oResult:oData:generateerror( oError )

   END 

RETURN ( nil )

METHOD generateerror( cMSg ) CLASS TAssert
Local oError   
Local nFirstProcedureOutTestAssert := 0

   nFirstProcedureOutTestAssert := GetFirstProcedureOutTestAssert()

   oError               := ErrorNew()
   oError:description   := procfile( nFirstProcedureOutTestAssert ) + ":" + ltrim( str( procline( nFirstProcedureOutTestAssert ) ) ) + ":" + procname( nFirstProcedureOutTestAssert ) + " => "
   oError:description   += NoMsg( cMsg )
   oError:filename      := Procfile( nFirstProcedureOutTestAssert )

Return oError

//---------------------------------------------------------------------------//

METHOD isEqual( xExp, xAct ) CLASS TAssert

   local lResult := .f.

   do case
      case ValType( xExp ) != ValType( xAct )
      case ( !( xExp == xAct ))
      otherwise
         lResult := .t.
   endcase

RETURN ( lResult )

//---------------------------------------------------------------------------//

METHOD isMinor( xExp, xAct ) CLASS TAssert
RETURN ( xExp < xAct )

//---------------------------------------------------------------------------//

METHOD isHigher( xExp, xAct ) CLASS TAssert
RETURN ( xExp > xAct )

//---------------------------------------------------------------------------//

METHOD toStr( xVal, lUseQuote ) CLASS TAssert

   local cStr, i

   if( lUseQuote == nil, lUseQuote := .f., )

   do case
   case ( valtype( xVal ) == "C" )
      cStr := xVal
   case ( valtype( xVal ) == "M" )
      cStr := xVal
   case ( valtype( xVal ) == "L" )
      cStr := if( xVal, ".t.", ".f." )
   case ( valtype( xVal ) ==  "D" )
      cStr := DToC( xVal )
   case ( valtype( xVal ) == "N" )
      cStr := alltrim( str( xVal ) )
   case ( valtype( xVal ) == "A" )
      cStr := hb_valtoexp( xVal )
   case ( valtype( xVal ) == "O" )
      cStr := "obj"
   case ( valtype( xVal ) == "B" )
      cStr := "blk"
   otherwise
      cStr := "nil"
   end

   if ( lUseQuote .and. ValType( xVal ) == "C" )
      cStr := "'" + cStr+ "'"
   endif

RETURN ( cStr )

//---------------------------------------------------------------------------//

Static Function GetFirstProcedureOutTestAssert()
Local nProcedureCount := 0
Local nFirstProcedureOutTestAssert := 0

   nProcedureCount := 0
   While nProcedureCount <=10 .And. nFirstProcedureOutTestAssert == 0

      nProcedureCount++

      if hb_at( 'TESTASSERT.PRG', Upper( Alltrim( ProcFile( nProcedureCount ) ) ) ) == 0
         nFirstProcedureOutTestAssert := nProcedureCount
      Endif
   
   Enddo
   nFirstProcedureOutTestAssert--

   
Return nFirstProcedureOutTestAssert

Static Function NoMsg( cMsg )

   hb_default( @cMsg, 'sin mensaje' )

   If cMsg == Nil
      cMsg := 'sin mensaje'
   Endif

Return ( cMsg )