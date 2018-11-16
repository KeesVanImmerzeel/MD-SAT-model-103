library dsmodel103;

  {-Uitspoeling van pesticide volgens Aaldrik Tiktak in:
    "Methods for regional-scale vulnerability assessments".
    De technische basis van het model is:
    "Effects of Soil Heterogeneity on Pesticide Leaching to Groundwater",
    Sjoerd E.A.T.M. van der Zee en Jos J.T.I. Boesten (Water Resources Research,
    vol 27, no. 12, p3051-3063, dec. 1991).
    In het model is, (op initiatief van dhr. Tiktak) een vereenvoudiging aan-
    gebracht: het effect van de gewasopname van pesticide op de afbraak-
    coefficient wordt verwaarloosd. M.a.w. mu_e=mu.
    KVI okt. 2001 }

  { Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{.$define test}

uses
  ShareMem,
  {.$ifdef test} forms, {.$endif} windows, SysUtils, Classes, LargeArrays,
  ExtParU, USpeedProc, uDCfunc,UdsModel, UdsModelS, xyTable, DUtils, uError, Math;

Const
  cModelID    = 103;  {-Key to this model (=unique-ID)}

  {-Mapping of dependent variable vector (=aantal te integreren snelheden)}
  cNrOfDepVar = 2;    {-Length of dependent variable vector}

  cMf    = 1;    {-Fractie van toegepaste hoeveelheid pesticide die uitspoelt (-)}
  cUitsp = 2;    {-Uitspoeling van pesticide (kg/ha/jr)}
  
  {-Aantal keren dat een discontinuiteitsfunctie wordt aangeroepen in de procedure met
    snelheidsvergelijkingen (DerivsProc)}
  nDC = 0;

  {***** Used when booted for shell-usage ***}
  cnRP    = 7;  {-Nr. of RP-time-series that must be supplied by the shell in
                  EP[ indx-1 ].}
  cnSQ    = 0;  {-Nr. of point-time-series that must be supplied by the shell
                  in EP[ indx-1 ]. REM: point- sources niet aan de orde bij
                  stikstof-uitspoeling!}
  cnRQ    = 0;  {-Nr. of line-time-series that must be supplied
                  by the shell in EP[ indx-1 ]}

  {-Mapping of EP[cEP0]}
  cNrXIndepTblsInEP0 = 8;  {-Nr. of XIndep-tables in EP[cEP0]}
  cNrXdepTblsInEP0   = 0;  {-Nr. of Xdep-tables   in EP[cEP0]}

  {-EP[cEP0]: xIndep-Table numbering; 0&1 are reserved}
  cTb_MinMaxValKeys   = 2; {-Min/max. waarden van shell-gegevens}
  cTb_BodemPrmtrsA    = 3; {-Bodem-gerelateerde parameters: akkerbouw}
  cTb_BodemPrmtrsB    = 4; {-Bodem-gerelateerde parameters: Bos}
  cTb_BodemPrmtrsG    = 5; {-Bodem-gerelateerde parameters: Gras}
  cTb_BodemPrmtrsN    = 6; {-Bodem-gerelateerde parameters: Natuur}
  cTB_PestPrmtrs      = 7; {-Pesticide-gerelateerde parameters }

  {-Mapping of EP[ indx-1 ]: xdep-Table numbering}
  cTb_PestType        = 0; {-Soort Pesticide}
  cTb_PestFlx         = 1; {-Toegepaste hoeveelheid pesticide (kg/ha/jr)}
  cTb_LandgebruikType = 2; {-LandgebruikType}
  cTb_BodemType       = 3; {-BodemType}
  cTb_q               = 4; {-Natuurlijk q (m/jr)}
  cTb_L               = 5; {-Diepte van de jaargemiddelde grondwaterstand (m-mv)}
  cTb_CalcType        = 6; {-CalcType: (0=av, 1=min, 2=max)}

  {-PestType-codes: zie "dsmodels103.ep0" table[7]}

  {-LandgebruikType-codes}
  cd_akkerbouw = 1;
  cd_bos       = 2;
  cd_gras      = 3;
  cd_natuur    = 4;
  MaxLandgebr  = 4;

  {-BodemType-codes: zie 'dsmodel 103 tabellen.xls'}

  {-Model specifieke fout-codes}
  cInvld_PestType        = -9200;
  cInvld_LandgebruikType = -9201;
  cInvld_BodemType       = -9202;
  cInvld_PestFlx         = -9203;
  cInvld_L               = -9204;
  cInvld_q               = -9205;
  cInvld_CalcType        = -9206;
  cErr_calc_mf           = -9207;

var
  Indx: Integer; {-Index of Boot-procedure used. Must be set by boot-procedure!}
  {-Als verschillende TBootEPArray-functies de 'mapping' beinvloeden van
    de externe parameters op de EPArray, dan kan deze variabele binnen de
    Speed-procedures worden benut om toch de gewenste gegevens te vinden}
  {-Min/max values of key-values: must be set by boot-procedure!}
  ModelProfile: TModelProfile;
                 {-Object met met daarin de status van de discontinuiteitsfuncties
				   (zie nDC) }
  cMinPestType, cMaxPestType,
  cMinLandgebruikType, cMaxLandgebruikType,
  cMinBodemType, cMaxBodemType: Integer;
  cMinPestFlx, cMaxPestFlx,
  cMinq, cMaxq,
  cMinL, cMaxL: Double;
  cMinCalcType, cMaxCalcType: Integer;

Procedure MyDllProc( Reason: Integer );
begin
  if Reason = DLL_PROCESS_DETACH then begin {-DLL is unloading}
    {-Cleanup code here}
	if ( nDC > 0 ) then
      ModelProfile.Free;
  end;
end;

Procedure DerivsProc( var x: Double; var y, dydx: TLargeRealArray;
                      var EP: TExtParArray; var Direction: TDirection;
                      var Context: Tcontext; var aModelProfile: PModelProfile;
                      var IErr: Integer );
{-Returns the derivatives dydx at location x, given, x, the function values y
  and the external parameters EP. IErr=0 if no error occured during the
  calculation of dydx}
Type
  TBodRelDat = Record
    Laagdikte,    {Laagdikte (D (m))}
    OMCav,        {av.org.matter cont.(-)}
    OMCmin,       {min.org.matter cont.(-)}
    OMCmax,       {max.org.matter cont.(-)}
    dBD,          {Dry bulk density (kg/m3)}
    WcFc,         {Water cont. at field cap.(-)}
    TotPor,       {Total porosity (-)}
    Ldis: Double; {Dispersion length (m)}
  end;
  TCalcType = (Calc_av, Calc_min, Calc_max);
var
  PestType, LandgebruikType, BodemType: Integer; {key-values}
  LGT,            {-Landgebr.type waarbij geg. is gevonden}
  RecNr: Integer; {-Recordnr. idem}
  PestFlx, L, q, mfTot: Double;
  BodRelDat: TBodRelDat;
  CalcType: TCalcType;
  i: Integer;

Function GetBodRelTableNr( const LandgebruikType: Integer ): Integer;
begin
  case LandgebruikType of
  cd_akkerbouw : Result := cTb_BodemPrmtrsA;
  cd_bos       : Result := cTb_BodemPrmtrsB;
  cd_natuur    : Result := cTb_BodemPrmtrsN;
  else
    Result := cTb_BodemPrmtrsG; {-Gras}
  end;
end;

Procedure FillBodRelDat( const LandgebruikType, RecNr: Integer );
begin
  with EP[ cEP0 ].xInDep.Items[ GetBodRelTableNr( LandgebruikType ) ] do begin
    with BodRelDat do begin
      Laagdikte := GetValue( RecNr, 2 );
      OMCav     := GetValue( RecNr, 3 );
      OMCmin    := GetValue( RecNr, 4 );
      OMCmax    := GetValue( RecNr, 5 );
      dBD       := GetValue( RecNr, 6 );
      WcFc      := GetValue( RecNr, 7 );
      TotPor    := GetValue( RecNr, 8 );
      Ldis      := GetValue( RecNr, 9 );
    end;
  end;
end;

Function SetKeyValues( var IErr: Integer ): Boolean;

var
  hlp: Integer;

Function SetBodRelDat( const BodemType, LandgebruikType: Integer;
                       var LGT,  {-Landgebr.type waarbij geg. is gevonden}
                           RecNr,{-Record idem }
                           IErr: Integer ): Boolean;
const  cSearchOrderArray: array[1..MaxLandgebr] of Integer =
                         (cd_gras, cd_akkerbouw, cd_bos, cd_natuur);
var
  i: Integer;

Function FindAndFillRecord( const BodemType, LandgebruikType: Integer;
                            var RecNr: Integer ): Boolean;
var
  NRows, BT: Integer;
begin
  Result := false;
  RecNr  := 0;
  with EP[ cEP0 ].xInDep.Items[ GetBodRelTableNr( LandgebruikType ) ] do begin
    NRows := GetNRows;
    repeat
      Inc( RecNr );
      BT := BodemType -1;
      if ( RecNr <= NRows ) then begin
        BT     := Trunc( GetValue( RecNr, 1 ) );
        Result := ( BodemType = BT );
        if Result then
          FillBodRelDat( LandgebruikType, RecNr );
      end;
    until ( Result or ( BT > BodemType ) or ( RecNr = NRows ) );
  end; {with}
end; {-Function FindAndFillRecord}

begin
  {-Probeer eerst de gegevens te vinden in de tabel van het gespecificeerde
    landgebruik-type}
  IErr   := cNoError;
  Result := false;
  LGT    := LandgebruikType;

  if FindAndFillRecord( BodemType, LGT, RecNr ) then begin
    Result := true; Exit;
  end;

  {-Als dit niet is gelukt, probeer dan de tabellen met de andere landgebruiks-
    typen, waarbij de volgorde van cSearchOrderArray wordt gehandhaafd.}
  for i:=1 to MaxLandgebr do begin
    LGT := cSearchOrderArray[ i ];
    if ( LGT <> LandgebruikType ) then begin
      if FindAndFillRecord( BodemType, LGT, RecNr ) then begin
        Result := true; Exit;
      end;
    end;
  end;

  {-Het gespecificeerde bodemtype is niet aangetroffen}
  IErr := cInvld_BodemType;

end; {-Function SetBodRelDat}

begin
  Result := False;
  with EP[ indx-1 ].xDep do begin {-Value of indx MUST be set by boot-procedure}
    PestType := Trunc( Items[ cTb_PestType ].EstimateY( x, Direction ) );
    if ( PestType < cMinPestType ) or ( PestType > cMaxPestType ) then begin
      IErr := cInvld_PestType; Exit;
    end;
    {$ifdef test}
    Application.MessageBox( 'PestType= ', 'Info', MB_OKCANCEL );
    {$endif}
    LandgebruikType := Trunc( Items[ cTb_LandgebruikType ].EstimateY( x, Direction ) );
    if ( LandgebruikType < cMinLandgebruikType ) or ( LandgebruikType > cMaxLandgebruikType ) then begin
      IErr := cInvld_LandgebruikType; Exit;
    end;
    {$ifdef test}
    Application.MessageBox( 'LandgebruikType= ' , 'Info', MB_OKCANCEL );
    {$endif}
    BodemType := Trunc( Items[ cTb_BodemType ].EstimateY( x, Direction ) );
    if ( BodemType < cMinBodemType ) or ( BodemType > cMaxBodemType ) then begin
      IErr := cInvld_BodemType; Exit;
    end;
    {$ifdef test}
    Application.MessageBox( 'BodemType= ' , 'Info', MB_OKCANCEL );
    {$endif}

    PestFlx := Items[ cTb_PestFlx ].EstimateY( x, Direction );
    if ( PestFlx < cMinPestFlx ) or ( PestFlx > cMaxPestFlx ) then begin
      IErr := cInvld_PestFlx; Exit;
    end;
    {$ifdef test}
    Application.MessageBox( 'PestFlx= ' , 'Info', MB_OKCANCEL );
    {$endif}

    q := Items[ cTb_q ].EstimateY( x, Direction );
    if ( q < cMinq ) or ( q > cMaxq ) then begin
      IErr := cInvld_q; Exit;
    end;
    {$ifdef test}
    Application.MessageBox( 'q= ' , 'Info', MB_OKCANCEL );
    {$endif}

    L := max( Items[ cTb_L ].EstimateY( x, Direction ), 0 );
    if ( L < cMinL ) or ( L > cMaxL ) then begin
      IErr := cInvld_L; Exit;
    end;
    {$ifdef test}
    Application.MessageBox( 'L= ' , 'Info', MB_OKCANCEL );
    {$endif}
    hlp := Trunc( Items[ cTb_CalcType ].EstimateY( x, Direction ) );
    if ( hlp < cMinCalcType ) or ( hlp > cMaxCalcType ) then begin
      IErr := cInvld_CalcType; Exit;
    end;
    {$ifdef test}
    Application.MessageBox( 'hlp= ' , 'Info', MB_OKCANCEL );
    {$endif}

    CalcType := TCalcType( hlp );
    if ( not SetBodRelDat( BodemType, LandgebruikType, LGT, RecNr, IErr ) ) then
      Exit;
    {$ifdef test}
    Application.MessageBox( 'CalcType= ' , 'Info', MB_OKCANCEL );
    {$endif}
  end;
  Result := True;
end; {-Function SetKeyValues}

Function Mom( const CalcType: TCalcType ): Double;
begin
  Case CalcType of
    Calc_min:  Result := BodRelDat.OMCmax;
    Calc_max:  Result := BodRelDat.OMCmin;
  else
    Result := BodRelDat.OMCav;
  end;
end;

Function WcFc: Double;
begin
  Result := BodRelDat.WcFc;
end;

Function dBD: Double;
begin
  Result := BodRelDat.dBD;
end;

Function LaagDikte: Double;
begin
  Result := BodRelDat.LaagDikte;
end;

Function TotPor: Double;
begin
  Result := BodRelDat.TotPor;
end;

Function D( const v: Double ): Double;
  {-Hydrodynamic dispersion coefficient (m2/d)}
begin
  Result := BodRelDat.Ldis * abs( v );
end;

Function Kom( const PestType: Integer ): Double;
begin
  with EP[ cEP0 ].xInDep.Items[ cTB_PestPrmtrs ] do
    Result := GetValue( PestType, 1 );
end;

Function Mu( const PestType: Integer ): Double;
begin
  with EP[ cEP0 ].xInDep.Items[ cTB_PestPrmtrs ] do
    Result := GetValue( PestType, 2 );
end;

Function KH( const PestType: Integer ): Double;
begin
  with EP[ cEP0 ].xInDep.Items[ cTB_PestPrmtrs ] do
    Result := GetValue( PestType, 3 );
end;

Function fu( const PestType: Integer ): Double;
begin
  with EP[ cEP0 ].xInDep.Items[ cTB_PestPrmtrs ] do
    Result := GetValue( PestType, 4 );
end;

Function Getv: Double;
  {-Rate of flow of the pore water (m/d)}
begin
  Result := q / ( WcFc * cNrOfDaysPerYear );
end;

Function GetPe( const v, L: Double ): Double;
  {-Peclet number (-)}
begin
  Result := v * L / D( v );
end;

{Function U( const PestType: Integer ): Double;
begin
  Result := fu( PestType ) * Sw / ( WcFc * cNrOfDaysPerYear );
end;}

Function GetRF( const PestType: Integer; const CalcType: TCalcType ): Double;
  {-Retardation factor (-)}
begin
  Result := 1 + ( dBD * Kom( PestType ) * Mom( CalcType ) / WcFc );
end;

Function Mu_e( const PestType: Integer; const RF: Double  ): Double;
begin
  Result := Mu( PestType ) {+ ( U( PestType ) / RF )};
end;

Function GetVcomp( const PestType: Integer; const v: Double;
                   const CalcType: TCalcType ): Double;
var
  RF: Double;
begin
  RF := GetRF( PestType, CalcType );
  Result := v / ( Mu_e( PestType, RF ) * RF );
end;

Function Getmf( const L: Double; var mf: Double; var IErr: Integer ): Boolean;
var
  v, Pe, Vcomp, hlp: double;
begin
  IErr   := cNoError;
  Result := true;
  v      := Getv;
  Pe     := GetPe( v, L );
  Vcomp  := GetVcomp( PestType, v, CalcType );
  Try
    hlp    := 1 + ( ( 4 * L ) / ( Vcomp * Pe ) );
    mf := exp( -0.5 * Pe *( Sqrt( hlp ) - 1 ) );
  except
    IErr := cErr_calc_mf;
    Result := false;
  end;
end;

Function Get_mfTot( var mfTot: Double; var IErr: Integer ): Boolean;
var
  Ltot, RestL, dL: Double;
  AvBodDat: TBodRelDat;
  Function LastLayer: Boolean;
    {-True als in RecNr de laatste bodemlaag is van "BodemType". Deze
      functie geeft alleen een goed resultaat als RecNr, BodemType en
      LGT geldige waarden hebben }
  var
    BT: Integer;
  begin
    Result := true;
    with EP[ cEP0 ].xInDep.Items[ GetBodRelTableNr( LGT ) ] do begin
      if ( RecNr < GetNRows ) then begin
        BT     := Trunc( GetValue( RecNr+1, 1 ) );
        Result := ( BodemType <> BT );
      end;
    end;
  end;
  Procedure ZeroAvBodDat;
  begin
    with AvBodDat do begin
      Laagdikte := 0; OMCav := 0; OMCmin := 0; OMCmax := 0;
      dBD       := 0; WcFc  := 0; TotPor := 0; Ldis   := 0;
    end;
  end;
  Procedure AddLayerInfluence( const BodRelDat: TBodRelDat; const dL: Double );
  var
    Fraction: Double;
  begin
    with AvBodDat do begin
      Fraction := dL / L;
      Laagdikte := Laagdikte + dL;
      OMCav     := OMCav  + Fraction * BodRelDat.OMCav;
      OMCmin    := OMCmin + Fraction * BodRelDat.OMCmin;
      OMCmax    := OMCmax + Fraction * BodRelDat.OMCmax;
      dBD       := dBD    + Fraction * BodRelDat.dBD;
      WcFc      := WcFc   + Fraction * BodRelDat.WcFc;
      TotPor    := TotPor + Fraction * BodRelDat.TotPor;
      Ldis      := Ldis   + Fraction * BodRelDat.Ldis;
    end;
  end;
begin
  IErr   := cNoError;
  Ltot   := 0;
  Result := false;
  ZeroAvBodDat;
  repeat
    RestL := L - Ltot;
    if ( RestL > 0 ) then begin
      if ( not LastLayer ) then
        dL := Min( RestL, LaagDikte )
      else
        dL := RestL;
      Ltot := Ltot + dL;
      AddLayerInfluence( BodRelDat, dL );
      if ( not LastLayer ) then begin
        Inc( RecNr );
        FillBodRelDat( LGT, RecNr );
      end;
    end;
  until ( RestL <= 0 );
  Move( AvBodDat, BodRelDat, SizeOf( TBodRelDat ) );
  if Getmf( L, mfTot, IErr ) then
    Result := true
  else;
end; {-Function Get_mfTot}

begin
  IErr := cUnknownError;
  for i := 1 to cNrOfDepVar do {-Default speed = 0}
    dydx[ i ] := 0;

  {-Geef de aanroepende procedure een handvat naar het ModelProfiel}
  if ( nDC > 0 ) then
    aModelProfile := @ModelProfile
  else
    aModelProfile := NIL;

  if ( Context = UpdateYstart ) then begin
    {-*** Override initial values on ystart-vector here}
    {-*** END Override initial values on ystart-vector here}

    {-Converteer dag-waarden uit tijdreeksen en invoertijdstippen afkomstig van
      de Shell naar jaren}
    {Application.MessageBox( 'dsmodel103 Context = UpdateYstart', 'Info', MB_OKCANCEL );}
    if ( indx = cBoot2 ) then begin
      {Application.MessageBox( 'dsmodel103 ScaleTimesFromShell', 'Info', MB_OKCANCEL );}
      ScaleTimesFromShell( cFromDayToYear, EP );
    end;
    IErr   := cNoError;
  end else begin             {-Fill dydx-vector}
    if not SetKeyValues( IErr ) then
      Exit;
    if Get_mfTot( mfTot, IErr ) then begin
      dydx[ cMf ]    := mfTot;
      dydx[ cUitsp ] := mfTot * PestFlx
    end else
      Exit;
    IErr   := cNoError;
  end;
end; {-DerivsProc}

Function DefaultBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-xDep-tables (PestType, PestFlx, LandgebruikType, BodemType,
    Neerslagoverschot (q), L) are NOT set by this boot-procedure: they have to be
    initialised in another way}
Procedure SetMinMaxKeyValues;
begin
  with EP[ cEP0 ].xInDep.Items[ cTb_MinMaxValKeys ] do begin
    cMinPestType           := Trunc( GetValue( 1, 1 ) );
    cMaxPestType           := Trunc( GetValue( 1, 2 ) );
    cMinPestFlx            :=        GetValue( 1, 3 );
    cMaxPestFlx            :=        GetValue( 1, 4 );
    cMinLandgebruikType    := Trunc( GetValue( 1, 5 ) );
    cMaxLandgebruikType    := Trunc( GetValue( 1, 6 ) );
    cMinBodemType          := Trunc( GetValue( 1, 7 ) );
    cMaxBodemType          := Trunc( GetValue( 1, 8 ) );
    cMinq                  :=        GetValue( 2, 1 );
    cMaxq                  :=        GetValue( 2, 2 );
    cMinL                  :=        GetValue( 2, 3 );
    cMaxL                  :=        GetValue( 2, 4 );
    cMinCalcType           := Trunc( GetValue( 2, 5 ) );
    cMaxCalcType           := Trunc( GetValue( 2, 6 ) );
  end;
end;
Begin
  Result := DefaultBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cNrOfDepVar, nDC,
            cNrXIndepTblsInEP0, cNrXdepTblsInEP0, Indx, EP );
  if ( Result = cNoError ) then
    SetMinMaxKeyValues;
end;

Function TestBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Apart from the defaults for TestBootEP, this procedure also sets the
    xDep-tables, so the model is ready-to-run }
Begin
    {$ifdef test}
    Application.MessageBox( 'TestBootEP ', 'Info', MB_OKCANCEL );
    {$endif}


  Result := DefaultBootEP( EpDir, BootEpArrayOption, EP );
  if ( Result <> cNoError ) then Exit;
  Result := DefaultTestBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cnRP + cnSQ + cnRQ, Indx,
                                           EP );
  if ( Result <> cNoError ) then Exit;
  SetReadyToRun( EP);
  {$ifdef test}
  Application.MessageBox( 'ReadyToRun', 'Info', MB_OKCANCEL );
  {$endif}
end;

Function BootEPForShell( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-xDep-tables are NOT set by this boot-procedure: they must be supplied
    by the shell }
begin
    {$ifdef test}
    Application.MessageBox( 'BootEPForShell ', 'Info', MB_OKCANCEL );
    {$endif}
  Result := DefaultBootEP( EpDir, cBootEPFromTextFile, EP );
  if ( Result = cNoError ) then
    Result := DefaultBootEPForShell( cnRP, cnSQ, cnRQ, Indx, EP );
end;

Exports DerivsProc       index cModelIndxForTDSmodels, {999}
        DefaultBootEP    index cBoot0, {1}
        TestBootEP       index cBoot1, {2}
        BootEPForShell   index cBoot2; {3}
begin
  {-This 'DLL-Main-block' is executed  when the DLL is initially loaded into
    memory (Reason = DLL_PROCESS_ATTACH)}
  DLLProc := @MyDllProc;
  Indx := cBootEPArrayVariantIndexUnknown;
  if ( nDC > 0 ) then
    ModelProfile := TModelProfile.Create( nDC );
end.
