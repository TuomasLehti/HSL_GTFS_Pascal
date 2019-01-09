unit CSVDataBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  {
  CSV-muodossa olevaa tietokantaa käsittelevä luokka.

  CSV-tiedoston ensimmäisellä rivillä ovat tietueiden kenttien nimet.
  Seuraavilla riveillä ovat varsinaiset tietueet.
  }
  TCSVDataBase = class
    private
      FLines : TStringList;
      FKeys : TStringList;
      KeyString : String;
      function GetKey(I : Integer) : String;
      function GetRecord(I : Integer) : String;
    public

      {Create luo luokasta objektin.}
      constructor Create;

      {Destroy vapauttaa luokan varaaman muistin.}
      destructor Destroy;

      {LoadFromFile lukee tiedoston muistiin.}
      procedure LoadFromFile(Filename : String);

      {LoadFromStringList lukee rivit toisesta merkkijonolistasta. }
      procedure LoadFromStringList(AList : TStringList);

      {Keys on taulukko, joka sisältää tietueiden kenttien nimet. Jos yritetään
       hakea kenttää, jota ei ole olemassa, palautetaan tyhjä merkkijono.}
      property Keys[I : Integer] : String read GetKey;

      {Records on taulukko, joka sisältää tietueet tekstiriveinä.}
      property Records[I : Integer] : String read GetRecord;

      {NumOfRecords palauttaa tietokannassa olevien tietueiden määrän.
       Ei sisällä kenttien nimet sisältävää riviä.}
      function NumOfRecords : Cardinal;

      {HasRecords tarkastaa, onko tietokannassa tietueita.}
      function HasRecords : Boolean;

      {GetField palauttaa tietyn tietueen tietyn kentän. Idx on tietueen
       järjestysnumero, välillä 0 - NumOfRecords-1. Key on tietueen kentän
       nimi. Jos tietueen numero on liian suuri tai pieni, tai tietueesta ei
       löydy sen nimistä kenttää, palauttaa tyhjän merkkijonon.}
      function GetField(Idx : Integer; Key : String) : String;
  end;

implementation

{ TCSVDataBase }

function TCSVDataBase.GetKey(I: Integer): String;
begin
  if I < FKeys.Count then
    Result := FKeys[I]
  else
    Result := '';
end;

function TCSVDataBase.GetRecord(I: Integer): String;
begin
  Result := FLines[I];
end;

constructor TCSVDataBase.Create;
begin
  FLines := TStringList.Create;
  FKeys := TStringList.Create;
end;

destructor TCSVDataBase.Destroy;
begin
  FLines.Free;
  FKeys.Free;
end;

procedure TCSVDataBase.LoadFromFile(Filename: String);
begin
  FLines.Clear;
  FLines.LoadFromFile(Filename);
  KeyString := FLines[0];
  FLines.Delete(0);

  FKeys.Clear;
  FKeys.CommaText := KeyString;
end;

procedure TCSVDataBase.LoadFromStringList(AList: TStringList);
begin
  FLines.Clear;
  FLines.AddStrings(AList);
  KeyString := FLines[0];
  FLines.Delete(0);

  FKeys.Clear;
  FKeys.CommaText := KeyString;
end;

function TCSVDataBase.NumOfRecords: Cardinal;
begin
  Result := FLines.Count;
end;

function TCSVDataBase.HasRecords: Boolean;
begin
  Result := NumOfRecords > 0;
end;

function TCSVDataBase.GetField(Idx: Integer; Key: String): String;
var
  Separated : TStringList;
  FieldIdx : Integer;
begin
  Result := '';
  if (Idx < 0) or (Idx > NumOfRecords-1) then Exit;
  Separated := TStringList.Create;
  Separated.StrictDelimiter := true;
  Separated.CommaText := Records[Idx];
  FieldIdx := FKeys.IndexOf(Key);
  if FieldIdx > -1 then
    Result := Separated[FieldIdx]
  else
    Result := '';
  Separated.Free;
end;

end.

{
26.6.2017:
--Luokka luotu.

2.10.2018:
--Dokumentointi päivitetty.
--Keys[] palauttaa nyt tyhjän merkkijonon, jos haetaan avainta liian suurella
  indeksillä.

TODO: function Search:Integer
TODO: function SearchAll:TIntegerList
TODO: procedure SaveToFile
TODO: procedure SetField
TODO: procedure Remove
TODO: procedure AddRecord
TODO: procedure Replace?
TODO: testaus
TODO: poista GetRecord ja Records?
}