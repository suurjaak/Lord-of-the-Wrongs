(**
 * Data container classes: races, cards, sites, decks.
 *
 * @author    Erki Suurjaak
 * @created   21.12.2003
 * @modified  14.11.2011
 *)
unit DataClasses;

interface

uses Graphics, classes;

type

  TSortInfo = class(TObject)
    SortColumn: String;
    IsSortReversed: Boolean;
    constructor Create();
  end;


  TDataClass = class(TObject)
  public
    ID: Integer;
  end;

  // Ancestor for cards and sites
  TPlayable = class(TDataClass)
  public
    TimeOfCreation: String;
    Title: String;
    Text: String;
    PictureFilename: String;
    PictureFileWidth: Integer;  // Original width of the recently loaded picture
    PictureFileHeight: Integer; // Original height of the recently loaded picture
    ContentPicture: TPicture;
    OriginalContentPicture: TPicture; // For undo purposes
    IsContentPictureChanged: Boolean;
    function GetShowName(): String; virtual; abstract;
  end;


  TCardType = class(TDataClass)
  public
    Name: String;
    IsCharacter: Boolean;
    IsPossession: Boolean;
    IsStrength: Boolean;
    IsHealth: Boolean;
    ContentPictureMaxWidth: Integer;
    ContentPictureMaxHeight: Integer;
    ContentPictureLeft: Integer;
    ContentPictureTop: Integer;
    MiddleTitleLeft: Integer;
    MiddleTitleWidth: Integer;
  end;


  TRace = class(TDataClass)
  public
    Name: String;
    IsGood: Boolean;
    CharacterName: String; // Name of a character card (e.g. minion,character)
    CharacterPicture: TPicture;
    OtherPicture: TPicture;
    StrengthPicture: TPicture;
    HealthPicture: TPicture;
    StrengthPictureLeft: Integer;
    StrengthPictureTop: Integer;
    HealthPictureLeft: Integer;
    HealthPictureTop: Integer;
    // Returns the TPicture of the specified type for this race
    // (types 'Character', 'Other', 'Strength', 'Health'.
    function GetPicture(PictureType: String): TPicture;
    destructor Destroy(); override;
  end;



  TCard = class(TPlayable)
  public
    CardType: TCardType;
    OriginalCardType: TCardType; // For undo purposes
    ExactTimeOfCreation: String;
    TimeOfModification: String;
    Subtitle: String;
    PossessionType: String;        // as in LOTR is hand weapon, helm etc
    TwilightCost: Integer;
    IsUnique: Boolean;
    Strength: String;
    Health: String;
    Time: String;
    Comment: String;
    Race: TRace;
    OriginalRace: TRace;     // For undo purposes
    ShowName: String; // The name shown in lists etc, incl. race and type
    InternalComment: String;
    REQUIRED_FIELDS_EMPTY_MESSAGE: String;
    Thumbnail: TPicture;
    constructor Create();
    destructor Destroy(); override;
    function AreRequiredFieldsEmpty(): Boolean;
    function GetShowName(): String; override;
  end;


  TSite = class(TPlayable)
  public
    Time: String;
    TwilightCost: Integer;
    Direction: String;
    ShowName: String;
    Comment: String;
    constructor Create();
    destructor Destroy(); override;
    function GetShowName(): String; override;
  end;


  TDeckCard = class(TDataClass)
  public
    ID: Integer;
    Card: TCard;
    IsDeleted: Boolean;
  end;


  TDeckSite = class(TDataClass)
  public
    ID: Integer;
    Site: TSite;
    IsDeleted: Boolean;
  end;


  TDeck = class(TDataClass)
  public
    Title: String;
    TimeOfCreation: String;
    Cards: TList;
    Sites: TList;
    ShowName: String; // The name shown in lists etc, incl. owner and count
    Comment: String;
    function GetShowName(): String;
    // The number of valid (undeleted) cards in the deck
    function GetCardsCount(): Integer;
    // The number of valid (undeleted) sites in the deck
    function GetSitesCount(): Integer;
    // Whether the deck contains this particular site
    function HasSite(Site: TSite): Boolean;
    constructor Create();
    destructor Destroy(); override;
  end;



implementation

uses SysUtils, Globals;


constructor TSortInfo.Create();
begin
  SortColumn := CARD_LIST_COLUMN_TITLE;
  IsSortReversed := False;
end;


// Returns the TPicture of the specified type for this race
// (types 'Character', 'Other', 'Strength', 'Health'.
function TRace.GetPicture(PictureType: String): TPicture;
var
  ResourceName: String;
begin
  ResourceName := StringReplace(Name + ' ' + PictureType, ' ', '_', [rfReplaceAll]);
  Result := Imager.GetResource(ResourceName, '.png');
end;


destructor TRace.Destroy();
begin
  CharacterPicture.Free();
  OtherPicture.Free();
  StrengthPicture.Free();
  HealthPicture.Free();
end;


function TDeck.GetCardsCount(): Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to Cards.Count - 1 do
    if (not TDeckCard(Cards.Items[I]).IsDeleted) then
      Inc(Result);
end;


function TDeck.GetSitesCount(): Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to Sites.Count - 1 do
    if (not TDeckSite(Sites.Items[I]).IsDeleted) then
      Inc(Result);
end;




function TDeck.GetShowName(): String;
var I, Count: integer;
begin
  if not (Length(ShowName) > 0) then begin
    Count := 0;
    for I := 0 to Cards.Count - 1 do
      if (not TDeckCard(Cards.Items[I]).IsDeleted) then
        Inc(Count);
    ShowName := Format('%s (%d cards)', [Title, Count]);
  end;
  Result := ShowName;
end;


constructor TDeck.Create();
begin
  Cards := TList.Create();
  Sites := TList.Create();
end;


destructor TDeck.Destroy();
var I: Integer;
begin
  for I := 0 to Cards.Count - 1 do begin
    TDeckCard(Cards.Items[I]).Free();
  end;
  for I := 0 to Sites.Count - 1 do begin
    TDeckSite(Sites.Items[I]).Free();
  end;
  Cards.Free();
  Sites.Free();
end;


// Whether the deck contains this particular site
function TDeck.HasSite(Site: TSite): Boolean;
var I: Integer;
    DeckSite: TDeckSite;
begin
  Result := False;
  for I := 0 to Sites.Count - 1 do begin
    DeckSite := Sites.Items[I];
    if ((DeckSite.Site = Site) and (not DeckSite.IsDeleted)) then begin
      Result := True;
      Break;
    end;
  end;
end;


constructor TCard.Create();
begin
  REQUIRED_FIELDS_EMPTY_MESSAGE := 'Every card must have a title.';
  ContentPicture := nil;
  OriginalContentPicture := nil;
  IsContentPictureChanged := False;
  Thumbnail := nil;
  CardType := nil;
end;


destructor TCard.Destroy();
begin
  ContentPicture.Free();
  OriginalContentPicture.Free();
  Thumbnail.Free();
end;



function TCard.GetShowName(): String;
begin
  if not (Length(ShowName) > 0) then begin
    ShowName := Title;
    if (Length(Subtitle) > 0)
      then ShowName := ShowName + ', ' + Subtitle;
    ShowName := ShowName + ' (' + Race.Name + ' ' + CardType.Name + ')';
  end;
  Result := ShowName;
end;


function TCard.AreRequiredFieldsEmpty(): Boolean;
begin
  Result := False;
  if (Length(Self.Title) = 0) then
    Result := True;
end;



constructor TSite.Create();
begin
  ContentPicture := nil;
  OriginalContentPicture := nil;
  IsContentPictureChanged := False;
end;


destructor TSite.Destroy();
begin
    ContentPicture.Free();
    OriginalContentPicture.Free();
end;


function TSite.GetShowName(): String;
begin
  if not (Length(ShowName) > 0) then begin
    ShowName := Format('%s (time %s)', [Title, Time]);
  end;
  Result := ShowName;
end;




end.
