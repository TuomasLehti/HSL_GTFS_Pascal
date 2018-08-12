unit IntegerList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  {TIngerList pitää yllä listaa kokonaisluvuista samaan tapaan kuin TStringList
   merkkijonoista.}

  TIntegerList = class
    private
      FInts : array of Integer;
      function GetInt(AnIndex : Integer) : Integer;

    public

      {Palauttaa listassa olevien kokonaislukujen määrän. Jos lista on tyhjä,
       palauttaa 0.}
      function Count : Integer;

      {Lisää kokonaisluvun listan loppuun.}
      procedure Add(AnInt : Integer);

      {Palauttaa listassa olevan luvun. Jos haetaan lukua listan ulkopuolelta,
       palauttaa 0.}
      property Integers[AnIndex : Integer] : Integer read GetInt; default;

      {Palauttaa listassa olevat luvut merkkijonona. Lukujen välissä on pilkku.
       Jos lista on tyhjä, palauttaa tyhjän merkkijonon.}
      function GetCommaStr : String;

      {Vertaa kahta kokonaislukulistaa, ja palauttaa tosi, jos kummassakin
       listassa on samat luvut samassa järjestyksessä. Kaksi tyhjää listaa
       määritellään yhtäsuuriksi.}
      function Equals(Obj : TObject) : Boolean;

      {Palauttaa kokonaisluvun paikan listassa. Jos lukua ei löydy, tai jos
       lista on tyhjä, palauttaa -1.}
      function IndexOf(AnInt : Integer) : Integer;

  end;

implementation

{ TIntegerList }

function TIntegerList.GetInt(AnIndex: Integer): Integer;
begin
  if (AnIndex >= 0) and (AnIndex < Count) then
    Result := FInts[AnIndex]
  else
    Result := 0;
end;

function TIntegerList.Count: Integer;
begin
  Result := Length(FInts);
end;

procedure TIntegerList.Add(AnInt: Integer);
begin
  SetLength(FInts, Length(FInts)+1);
  FInts[Length(FInts)-1] := AnInt;
end;

function TIntegerList.GetCommaStr: String;
var
  Idx : Integer;
begin
  Result := '';
  for Idx := 0 to Count-1 do
    Result := Result + IntToStr(FInts[Idx]) + ',';
  Delete(Result, Length(Result), 1);
end;

function TIntegerList.Equals(Obj: TObject): Boolean;
var
  Other : TIntegerList;
begin
  Other := Obj as TIntegerList;
  Result := GetCommaStr = Other.GetCommaStr;
end;

function TIntegerList.IndexOf(AnInt: Integer): Integer;
var
  Idx : Integer;
begin
  Idx := 0;
  while (Idx < Count) and (GetInt(Idx) <> AnInt) do Inc(Idx);
  if Idx = Count then
    Result := -1
  else
    Result := Idx;
end;

end.

{Muutoshistoria:

2018-08-12: Ensimmäinen versio.}
