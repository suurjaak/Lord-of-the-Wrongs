(**
 * Application-wide global constants, utility functions, and global variables.
 *
 * @author    Erki Suurjaak
 * @created   20.12.2003
 * @modified  08.11.2011
 *)
unit Globals;

interface

uses DataClasses, Imaging, Persistence, classes, SysUtils, Graphics,
     SyncObjs, Windows, DCL_intf;

const
  // %s will be replaced by application version.
  COPYRIGHT_TEXT_TEMPLATE =
    'LOTW %s, written by Erki Suurjaak, initially in 2003. ' +
    'Most card media and game logic courtesy of Decipher, Inc.';
  // Whether some debug messages are printed or not
  DEBUG = False;

  DEFAULT_DB_FILENAME = 'lotw.db';

  UNIQUE_SYMBOL = #183;
  MIDDLE_DOT = #183;

  NEW_LINE = #13#10;

  TAG_NEW_CARD = 16;
  TAG_CHARACTER_CONTROL = 32;
  TAG_CONDITION_CONTROL = 64;
  TAG_EVENT_CONTROL = 128;
  TAG_POSSESSION_CONTROL = 256;
  TAG_SHADOW_CONTROL = 512;
  TAG_NEW_SITE = 1024;

  CARD_TYPE_CHARACTER = 'character';
  CARD_TYPE_CONDITION = 'condition';
  CARD_TYPE_EVENT = 'event';
  CARD_TYPE_POSSESSION = 'possession';



  CARD_TYPES: array[0..3] of String = ('character', 'condition', 'event', 'possession');

  CARD_TYPE_TAGS: array[0..3] of Integer =
    (TAG_CHARACTER_CONTROL, TAG_CONDITION_CONTROL, TAG_EVENT_CONTROL, TAG_POSSESSION_CONTROL);


  CARD_LIST_COLUMN_TITLE = 'Title';
  CARD_LIST_COLUMN_TWILIGHT_COST = 'Twilight Cost';
  CARD_LIST_COLUMN_STRENGTH = 'Strength';
  CARD_LIST_COLUMN_HEALTH = 'Health';
  CARD_LIST_COLUMN_RACE = 'Race';
  CARD_LIST_COLUMN_TYPE = 'Type';
  CARD_LIST_COLUMN_TIME = 'Time';
  CARD_LIST_COLUMN_TIME_CREATED = 'Created';

  SITE_LIST_COLUMN_TITLE = 'Title';
  SITE_LIST_COLUMN_TIME = 'Time';
  SITE_LIST_COLUMN_TWILIGHT_COST = 'Twilight Cost';

  DECK_LIST_COLUMN_TITLE   = 'Title';
  DECK_LIST_COLUMN_CARDS   = 'Cards';
  DECK_LIST_COLUMN_COMMENT = 'Comment';

  CARD_LIST_FILTER_RACES_ALL = ' ';
  CARD_LIST_FILTER_RACES_FELLOWSHIP = 'Fellowship Races';
  CARD_LIST_FILTER_RACES_SHADOW = 'Shadow Races';

  CARD_LIST_FILTER_TYPES_ALL = ' ';

  CARD_LIST_FILTER_TIMES_ALL = ' ';

  // The coordinates of the card image, used for saving it to file
  CARD_IMAGE_TOP = 65;
  CARD_IMAGE_LEFT = 23;
  CARD_IMAGE_WIDTH = 357;
  CARD_IMAGE_HEIGHT = 497;
  CARD_CONTENT_PICTURE_MAX_WIDTH  = 285;
  CARD_CONTENT_PICTURE_MAX_HEIGHT = 215;

  // The coordinates of the site image, used for saving it to file
  SITE_IMAGE_TOP = 309;
  SITE_IMAGE_LEFT = 15;
  SITE_IMAGE_WIDTH = 497;
  SITE_IMAGE_HEIGHT = 357;
  SITE_CONTENT_PICTURE_MAX_WIDTH  = 395;
  SITE_CONTENT_PICTURE_MAX_HEIGHT = 225;


// Returns the tag number for the specified type, required
// for automatic interface changes in the card editor
function GetCardTypeTag(CardType: String): Integer;
// Removes the card from the global cards array and elsewhere
procedure RemoveCardFromVariables(var Card: TCard);
// Removes the site from the global sites array and elsewhere
procedure RemoveSiteFromVariables(var Site: TSite);
// Adds the card to the global cards array and elsewhere (if missing)
procedure AddCardToVariables(var Card: TCard);
// Removes the deck from the global variables
procedure RemoveDeckFromVariables(var Deck: TDeck);
// Removes the card object from the deck object
procedure RemoveCardFromDeck(var Deck: TDeck; var Card: TDeckCard);
// Returns the card with the specified ID
function GetCardByID(ID: Integer): TCard;
// Returns the site with the specified ID
function GetSiteByID(ID: Integer): TSite;
// Returns the race with the specified ID
function GetRaceByID(ID: Integer): TRace;
// Returns the race with the specified name
function GetRaceByName(Name: String): TRace;
// Does nothing
procedure Nothing();
// Checks whether the String is a valid number
function IsNumber(Str: String): Boolean;
// A version of ExtractFileName that works with URLs also
function ExtractFileNameBetter(FilePath: String): String;
// The comparison function used for sorting Cards
function CardListSortCompare(Item1, Item2: Pointer): Integer;
// The comparison function used for sorting Sites
function SiteListSortCompare(Item1, Item2: Pointer): Integer;
// Returns a system setting, from the Settings map
function GetSetting(Name: String): String;
// Returns a system setting, from the Settings map
function GetSettingInt(Name: String): Integer;
// Splits the string into words
function SplitTextIntoWords(const S: string): TStringList;
// Returns a formatted size (e.g. '1.3 MB', '1.2 KB' etc)
function FormatByteSize(Size: Integer): String;
// Returns the application version, e.g. '1.0.1.44'
function GetApplicationVersion(): String;



var
  Database: TPersistence;
  // Maps the positions in ComboCardList to TCard objects
  ComboCardListMap: IStrMap;
  ComboListDecksMap: IStrMap;
  ComboSiteListMap: IStrMap;
  IsCardChanged: Boolean = False;
  IsDeckChanged: Boolean = False;
  IsSiteChanged: Boolean = False;
  ListCardsSortInfo: TSortInfo;
  ListDecksSortInfo: TSortInfo;
  ListDeckAllCardsSortInfo: TSortInfo;
  ListDeckCardsSortInfo: TSortInfo;
  ListDeckAllSitesSortInfo: TSortInfo;
  ListDeckSitesSortInfo: TSortInfo;
  ListCardsFilterRace: String = '';
  ListCardsFilterType: String = '';
  ListCardsFilterTime: String = '';
  DBFilename: String;
  ListCardsFilterDeck: TDeck = nil;
  ApplyFilter: Boolean = False;
  IgnoreCardChanges: Boolean = False;
  IgnoreSiteChanges: Boolean = False;
  IgnoreDeckChanges: Boolean = False;
  PreviewedCard: TCard = nil;
  TransparentPixel: TPicture;
  CurrentCard: TCard;
  CurrentDeck: TDeck;
  CurrentRace: TRace;
  CurrentSite: TSite;
  Times: TStringArray;
  Decks: TList;
  Cards: TList;
  Races: TList;
  Sites: TList;
  Settings: IStrStrMap;
  Imager: TImaging;
  CardLoadSection: TCriticalSection;



implementation

uses db, Forms, ShlObj;

// Returns the tag number for the specified type, required
// for automatic interface changes in the card editor
function GetCardTypeTag(CardType: String): Integer;
var I: Integer;
begin
  Result := -1;
  for I := Low(CARD_TYPES) to High(CARD_TYPES) do
    if (CARD_TYPES[I] = CardType) then
      Result := CARD_TYPE_TAGS[I];
end;


// Removes the card from the global cards array and elsewhere
procedure RemoveCardFromVariables(var Card: TCard);
var I, J: Integer;
begin
  while (Cards.Remove(Card) <> -1) do Nothing();
  Cards.Pack;
  for I := 0 to Decks.Count - 1 do begin
    for J := 0 to TDeck(Decks.Items[I]).Cards.Count - 1 do begin
      if (TDeckCard(TDeck(Decks.Items[I]).Cards.Items[J]).Card = Card) then
        TDeck(Decks.Items[I]).Cards.Items[J] := nil;
    end;
    TList(TDeck(Decks.Items[I]).Cards).Pack();
  end;
end;


// Adds the card to the global cards array and elsewhere (if missing)
procedure AddCardToVariables(var Card: TCard);
begin
  if (Cards.IndexOf(Card) = -1) then
    Cards.Add(Card);
end;


// Removes the deck from the global variables
procedure RemoveDeckFromVariables(var Deck: TDeck);
begin
  while (Decks.Remove(Deck) <> -1) do Nothing();
  Decks.Pack;
end;


// Returns the card with the specified ID
function GetCardByID(ID: Integer): TCard;
var I: Integer;
begin
  Result := nil;
  for I := 0 to Cards.Count - 1 do
    if (TCard(Cards.Items[I]).ID = ID) then begin
      Result := Cards[I];
      Break;
    end;
end;


// Returns the site with the specified ID
function GetSiteByID(ID: Integer): TSite;
var I: Integer;
begin
  Result := nil;
  for I := 0 to Sites.Count - 1 do
    if (TSite(Sites.Items[I]).ID = ID) then begin
      Result := Sites[I];
      Break;
    end;
end;




// Returns the race with the specified ID
function GetRaceByID(ID: Integer): TRace;
var I: Integer;
begin
  Result := nil;
  for I := 0 to Races.Count - 1 do
    if (TRace(Races.Items[I]).ID = ID) then begin
      Result := Races[I];
      Break;
    end;
end;


function GetRaceByName(Name: String): TRace;
var I: Integer;
begin
  Result := nil;
  for I := 0 to Races.Count - 1 do
    if (TRace(Races.Items[I]).Name = Name) then begin
      Result := Races[I];
      Break;
    end;
end;


procedure RemoveCardFromDeck(var Deck: TDeck; var Card: TDeckCard);
begin
  while (Deck.Cards.Remove(Card) <> -1) do Nothing();
  Deck.Cards.Pack();
end;


// Does nothing
procedure Nothing();
begin
end;


// Checks whether the String is a valid number
function IsNumber(Str: String): Boolean;
var I: Integer;
begin
  Result := True;
  if (Length(Str) = 0) then
    Result := False
  else for I := 1 to Length(Str) do
    if not (Str[I] in ['0'..'9', '-']) then begin
      Result := False;
      Break;
    end;
end;


// A version of ExtractFileName that works with URLs also
function ExtractFileNameBetter(FilePath: String): String;
begin
  Result := Copy(FilePath, LastDelimiter('/\', FilePath) + 1, Length(FilePath))
end;


// The comparison function used for sorting Cards
function CardListSortCompare(Item1, Item2: Pointer): Integer;
var Card1, Card2: TCard;
    Field1, Field2: String;
begin
  Card1 := Item1;
  Card2 := Item2;
  Field1 := Card1.Title;
  if (Length(Card1.Subtitle) > 0) then
    Field1 := Field1 + ', ' + Card1.Subtitle;
  Field2 := Card2.Title;
  if (Length(Card2.Subtitle) > 0) then
    Field2 := Field2 + ', ' + Card2.Subtitle;
  Result := StrIComp(PChar(Field1), PChar(Field2));
end;


// The comparison function used for sorting Sites
function SiteListSortCompare(Item1, Item2: Pointer): Integer;
var Site1, Site2: TSite;
    Field1, Field2: String;
begin
  Site1 := Item1;
  Site2 := Item2;
  Field1 := Site1.Title;
  Field2 := Site2.Title;
  Result := StrIComp(PChar(Field1), PChar(Field2));
end;

// Returns a system setting, from the Settings map
function GetSetting(Name: String): String;
begin
  Result := Settings.GetValue(Name);
end;


// Returns a system setting, from the Settings map
function GetSettingInt(Name: String): Integer;
var Temp: String;
begin
  Result := 0;
  Temp := Settings.GetValue(Name);
  if (Temp <> '') then
    Result := StrToInt(Temp);
end;

// Removes the site from the global sites array and elsewhere
procedure RemoveSiteFromVariables(var Site: TSite);
var I, J: Integer;
begin
  while (Sites.Remove(Site) <> -1) do Nothing();
  Sites.Pack;
  for I := 0 to Decks.Count - 1 do begin
    for J := 0 to TDeck(Decks.Items[I]).Sites.Count - 1 do begin
      if (TDeckSite(TDeck(Decks.Items[I]).Sites.Items[J]).Site = Site) then
        TDeck(Decks.Items[I]).Sites.Items[J] := nil;
    end;
    TList(TDeck(Decks.Items[I]).Sites).Pack();
  end;
end;


// Splits the string into words
function SplitTextIntoWords(const S: string): TStringList;
var
  startpos, endpos: Integer;
begin
  Result := TStringList.Create();
  startpos := 1; 
  while startpos <= Length(S) do 
  begin 
    while (startpos <= Length(S)) and not IsCharAlpha(S[startpos]) do 
      Inc(startpos); 
    if startpos <= Length(S) then 
    begin 
      endpos := startpos + 1; 
      while (endpos <= Length(S)) and IsCharAlpha(S[endpos]) do 
        Inc(endpos); 
      Result.Add(Copy(S, startpos, endpos - startpos)); 
      startpos := endpos + 1; 
    end;
  end;
end;


// Returns a formatted size (e.g. '1.3 MB', '1.2 KB' etc)
function FormatByteSize(Size: Integer): string;
const
  B = 1; //byte
  KB = 1024 * B; //kilobyte
  MB = 1024 * KB; //megabyte
  GB = 1024 * MB; //gigabyte
begin
  if Size > GB then
    Result := FormatFloat('#.## GB', Size / GB)
  else begin
    if Size > MB then
      Result := FormatFloat('#.## MB', Size / MB)
    else begin
      if Size > KB then
        Result := FormatFloat('#.## KB', Size / KB)
      else
        Result := FormatFloat('#.## bytes', Size);
    end;
  end;
end;


// Returns the application version, e.g. '1.0.1.44'
function GetApplicationVersion(): String;
var
  V1, V2, V3, V4: Word;
  VerInfoSize, VerValueSize, Dummy: DWORD;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
begin
  V1 := 0; V2 := 0; V3 := 0; V4 := 0;
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  VerInfo := nil;
  if VerInfoSize > 0 then
  begin
    try
      GetMem(VerInfo, VerInfoSize);
      if GetFileVersionInfo(PChar(Application.ExeName), 0, VerInfoSize, VerInfo) then
      begin
        VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
        with VerValue^ do
        begin
          V1 := dwFileVersionMS shr 16;
          V2 := dwFileVersionMS and $FFFF;
          V3 := dwFileVersionLS shr 16;
          V4 := dwFileVersionLS and $FFFF;
        end;
      end;
    finally
      FreeMem(VerInfo, VerInfoSize);
    end;
  end;
  Result := Format('%d.%d.%d.%d', [V1, V2, V3, V4]);
end;


end.
