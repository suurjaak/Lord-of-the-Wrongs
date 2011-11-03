(**
 * Data container classes: races, cards, sites, decks.
 *
 * @author    Erki Suurjaak
 * @created   21.12.2003
 * @modified  03.11.2011
 *)
unit DataClasses;

interface

uses Graphics, classes;

type
  TDataClass = class
  public
    ID: Integer;
  end;


  TRace = class(TDataClass)
  public
    Name: String;
    IsGood: Boolean;
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
  end;



  TCard = class(TDataClass)
  public
    CardType: String;
    OriginalCardType: String; // For undo purposes
    TimeOfCreation: String;
    ExactTimeOfCreation: String;
    TimeOfModification: String;
    Title: String;
    Subtitle: String;
    PossessionType: String;        // as in LOTR is hand weapon, helm etc
    TwilightCost: Integer;
    IsUnique: Boolean;
    Strength: String;
    Health: String;
    Time: String;
    Text: String;
    Comment: String;
    PictureFilename: String;
    Race: TRace;
    OriginalRace: TRace;     // For undo purposes
    ShowName: String; // The name shown in lists etc, incl. race and type
    HiddenComment: String;
    REQUIRED_FIELDS_EMPTY_MESSAGE: String;
    ContentPicture: TPicture;
    OriginalContentPicture: TPicture; // For undo purposes
    Thumbnail: TPicture;
    constructor Create();
    destructor Destroy(); override;
    function GetShowName(): String;
    // Checks whether all the required fields contain data
    function AreRequiredFieldsEmpty(): Boolean;
  end;


  TSite = class(TDataClass)
  public
    TimeOfCreation: String;
    Title: String;
    PictureFilename: String;
    Time: String;
    TwilightCost: Integer;
    Direction: String;
    Text: String;
    ContentPicture: TPicture;
    OriginalContentPicture: TPicture; // For undo purposes
    ShowName: String;
    Comment: String;
    constructor Create();
    destructor Destroy(); override;
    function GetShowName(): String;
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
    // The number of cards in the deck
    function GetCardsCount(): Integer;
    // Whether the deck contains this particular site
    function HasSite(Site: TSite): Boolean;
    constructor Create();
  end;



implementation

uses SysUtils, Globals;

function TRace.GetPicture(PictureType: String): TPicture;
var
  ResourceName: String;
begin
  ResourceName := StringReplace(Name + ' ' + PictureType, ' ', '_', [rfReplaceAll]);
  Result := Imager.GetResource(ResourceName, 'png');
end;



function TDeck.GetCardsCount(): Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to Cards.Count - 1 do
    if (not TDeckCard(Cards.Items[I]).IsDeleted) then
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
    Thumbnail := nil;
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
    ShowName := ShowName + ' (' + Race.Name + ' ' + CardType + ')';
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
end;


destructor TSite.Destroy();
begin
    ContentPicture.Free();
    OriginalContentPicture.Free();
end;


function TSite.GetShowName(): String;
begin
  if not (Length(ShowName) > 0) then begin
    ShowName := Title + ' (time ' + Time + ')';
  end;
  Result := ShowName;
end;




end.
