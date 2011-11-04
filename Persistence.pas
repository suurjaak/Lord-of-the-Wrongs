(**
 * Database storage, retrieval and creation functionality.
 *
 * @author    Erki Suurjaak
 * @created   21.12.2003
 * @modified  04.11.2011
 *)
unit Persistence;

interface

uses Windows, Dialogs, DataClasses, classes, ElHashList, DISQLite3Api, Graphics;

type
  TPersistence = class(TObject)
  private
    DB: sqlite3_ptr;
    // Displays an error with the message and information from the database connection
    procedure ErrorMessage(Msg: String = '');
    // Binds parameters to the specified SQL statement
    procedure BindParameters(Stmt: sqlite3_stmt_ptr; const Parameters: array of const);
    // Executes the SELECT statement in SQL and returns the result
    function ExecuteSelect(SQL: String): RecordSet; overload;
    function ExecuteSelect(SQL: String; const Parameters: array of const): RecordSet; overload;
    // Executes the action statement in SQL and returns the number of affected rows
    function ExecuteAction(SQL: String): Integer; overload;
    function ExecuteAction(SQL: String; const Parameters: array of const): Integer; overload;
    // Performs the INSERT/UPDATE clause and in case of INSERT, returns the LAST_INSERT_ID.
    function ExecuteStore(SQL: String): Integer; overload;
    function ExecuteStore(SQL: String; const Parameters: array of const): Integer; overload;
    // Saves the card in the database
    procedure StoreCard(Card: TCard);
    // Saves the site in the database
    procedure StoreSite(Site: TSite);
    // Saves the deck in the database, including all the cards of the deck
    procedure StoreDeck(Deck: TDeck);
    // Creates the database tables and inserts default data, if needed
    procedure CheckRequiredTables();
  public
    Filename: String;
    // Creates a new database connection. If the database does not exist,
    // creates it. If required tables do not exist, creates them, and fills
    // tables (like settings, races, times) with default values.
    constructor Create(Filename: String);
    // Closes the database connection.
    destructor Destroy(); override;
    // Returns the size of the database file, in bytes.
    function GetSize(): Integer;
    // Retrieves all the cards in the database
    function RetrieveCards(): TList;
    // Retrieves all the decks in the database
    function RetrieveDecks(): TList;
    // Retrieves all the races in the database
    function RetrieveRaces(): TList;
    // Retrieves all the times in the database
    function RetrieveTimes(): TStringArray;
    // Retrieves all the sites in the database
    function RetrieveSites(): TList;
    // Retrieves certain settings from the settings table, in name-value pairs.
    // Values are TStrings.
    function RetrieveSettings(): TElHashList;
    // Retrieves the content picture of the specified object.
    function RetrieveContentPicture(Item: TDataClass): TPicture;
    // Saves the object. If it's a new item, assigns it an ID.
    // If it has subitems, like a TDeck, saves those also.
    procedure Store(Item: TDataClass);
    // Deletes the specified object from the database. Deletes all
    // the db dependencies also
    procedure Delete(Item: TDataClass);
  end;

implementation

uses
  sysutils, jpeg, pngimage, DIUtils, Globals;



// Creates a new database connection. If the database does not exist,
// creates it. If required tables do not exist, creates them, and fills
// tables (like settings, races, times) with default values.
constructor TPersistence.Create(Filename: String);
begin
  try
    sqlite3_check(sqlite3_open(PChar(Filename), @DB));
    // Set to vacuum database automatically, keeping its size down
    sqlite3_exec_fast(DB, 'PRAGMA auto_vacuum = 1');
    CheckRequiredTables();
    Self.Filename := Filename;
  except on E: Exception do
    ErrorMessage('TPersistence.Create failed with message ''' + E.ClassName + ': ' + E.Message + '''.');
  end;
end;


// Displays an error with the message and information from the database connection
procedure TPersistence.ErrorMessage(Msg: String = '');
var
  ErrorCode: Integer;
  FailMessage: String;
begin
    FailMessage := 'Fatal Error. ' + Msg;
    ErrorCode := sqlite3_errcode(DB);
    if (ErrorCode > 0) then
      begin
      FailMessage := FailMessage + NEW_LINE + '===========' + NEW_LINE
                     + Format('%d - %s', [ErrorCode, sqlite3_errmsg(DB)])
      end;
    raise Exception.Create(FailMessage);
end;


// Binds parameters to the specified SQL statement
procedure TPersistence.BindParameters(Stmt: sqlite3_stmt_ptr; const Parameters: array of const);
var
  I: Integer;
  ParametersTyped: array[0..$FFF0 div SizeOf(TVarRec)] of TVarRec absolute Parameters;
  Len: Integer;
  Stream: ^TStringStream;
begin
  for I := Low(Parameters) to High(Parameters) do
    with ParametersTyped[I] do
      case VType of
        vtInteger:
          sqlite3_check(sqlite3_bind_int(Stmt, I + 1, VInteger), DB);
        vtBoolean:
          sqlite3_check(sqlite3_bind_int(Stmt, I + 1, Ord(VBoolean)), DB);
        vtAnsiString:
          sqlite3_check(sqlite3_bind_str(Stmt, I + 1, AnsiString(VAnsiString)), DB);
        vtPointer:
          begin
          Stream := VPointer;
          sqlite3_check(sqlite3_bind_blob(Stmt, I + 1, PAnsiString(Stream.DataString), Stream.Size, SQLITE_STATIC), DB);
          end;
        else
          ErrorMessage('Unknown type for binding ''' + IntToStr(VType) + '''.');
      end;
end;


// Executes the action statement in SQL and returns the number of affected rows
function TPersistence.ExecuteAction(SQL: String): Integer;
begin
  Result := ExecuteAction(SQL, [])
end;


// Executes the action statement in SQL and returns the number of affected rows
function TPersistence.ExecuteAction(SQL: String; const Parameters: array of const): Integer;
var
  Stmt: sqlite3_stmt_ptr;
  DBResult: Integer;
begin
  Result := 0;
  try
    sqlite3_check(sqlite3_prepare_v2(DB, PChar(SQL), -1, @Stmt, nil), DB);
    if (Length(Parameters) > 0) then
      BindParameters(Stmt, Parameters);
    DBResult := sqlite3_check(sqlite3_step(Stmt), DB);
    if (DBResult in [SQLITE_OK, SQLITE_DONE]) then begin
      Result := sqlite3_changes(DB);
    end;
  finally
    try sqlite3_finalize(Stmt); finally end;
  end;
end;



// Executes the SELECT statement in SQL and returns the result
function TPersistence.ExecuteSelect(SQL: String): RecordSet;
begin
  Result := ExecuteSelect(SQL, []);
end;

// Executes the SELECT statement in SQL and returns the result
function TPersistence.ExecuteSelect(SQL: String; const Parameters: array of const): RecordSet;
var
  NumberOfFields: Integer;
  I, J: Integer;
  Stmt: sqlite3_stmt_ptr;
  Value: AnsiString;
begin
  SetLength(Result, 0, 0);
  try
    sqlite3_check(sqlite3_prepare_v2(DB, PChar(SQL), -1, @Stmt, nil), DB);
    I := 0;
    if (Length(Parameters) > 0) then
      BindParameters(Stmt, Parameters);
    while (sqlite3_check(sqlite3_step(Stmt), DB) = SQLITE_ROW) do
      begin
        NumberOfFields := sqlite3_column_count(Stmt);
        SetLength(Result, I + 1, NumberOfFields);
        for J := 0 to NumberOfFields - 1 do begin
          Value := AnsiString(sqlite3_column_str(Stmt, J));
          Result[I, J] := Value;
          //SetString(Result[I, J], Value, Length(Value));
        end;
        Inc(I);
      end;
  except on E: Exception do
    ErrorMessage('TPersistence.Retrieve failed with message ''' + E.ClassName + ': ' + E.Message + '''.');
  end;
  sqlite3_finalize(Stmt);
end;



// Retrieves all the races in the database
function TPersistence.RetrieveRaces(): TList;
var SQL: String;
    Results: RecordSet;
    I, J: Integer;
    Race: TRace;
    PictureStream: TStringStream;
begin
  SQL := 'SELECT id, name, is_good, strength_picture_left, strength_picture_top, ' +
         'health_picture_left, health_picture_top, character_picture, other_picture, '+
         'strength_picture, health_picture FROM races';
  Results := ExecuteSelect(SQL);
  Result := TList.Create();
  for I := Low(Results) to High(Results) do begin
    Race := TRace.Create();
    Race.ID := StrToInt(Results[I][0]);
    Race.Name := Results[I][1];
    Race.IsGood := (Results[I][2] = '1');
    Race.StrengthPictureLeft := StrToInt(Results[I][3]);
    Race.StrengthPictureTop := StrToInt(Results[I][4]);
    Race.HealthPictureLeft := StrToInt(Results[I][5]);
    Race.HealthPictureTop := StrToInt(Results[I][6]);
    if (Length(Results[I][7]) > 0) then begin
      PictureStream := TStringStream.Create(Results[I][7]);
      Race.CharacterPicture := Imager.LoadPictureFromStream(PictureStream, '.png');
      PictureStream.Free();
    end;
    if (Length(Results[I][8]) > 0) then begin
      PictureStream := TStringStream.Create(Results[I][8]);
      Race.OtherPicture := Imager.LoadPictureFromStream(PictureStream, '.png');
      PictureStream.Free();
    end;
    if (Length(Results[I][9]) > 0) then begin
      PictureStream := TStringStream.Create(Results[I][9]);
      Race.StrengthPicture := Imager.LoadPictureFromStream(PictureStream, '.png');
      PictureStream.Free();
    end;
    if (Length(Results[I][10]) > 0) then begin
      PictureStream := TStringStream.Create(Results[I][10]);
      Race.HealthPicture := Imager.LoadPictureFromStream(PictureStream, '.png');
      PictureStream.Free();
    end;

    Result.Add(Race);
  end;
end;



// Retrieves all the times in the database
function TPersistence.RetrieveTimes(): TStringArray;
var SQL: String;
    Results: RecordSet;
    I: Integer;
begin
  SQL := 'SELECT time FROM times ORDER BY ordering';
  Results := ExecuteSelect(SQL);
  SetLength(Result, Length(Results));
  for I := Low(Results) to High(Results) do begin
    Result[I] := Results[I][0];
  end;
end;



// Retrieves all the cards in the database
function TPersistence.RetrieveCards(): TList;
var SQL: String;
    Results: RecordSet;
    I: Integer;
    Card: TCard;
    ThumbnailStream: TStringStream;
begin
  SQL := 'SELECT id, race_id, type, time_of_creation, title, subtitle, possession_type, ' +
         'twilight_cost, is_unique, strength, health, time, text, comment, ' +
         'picture_filename, thumbnail, time_of_creation, last_modified, internal_comment FROM cards ' +
         'ORDER BY title, subtitle';
  Results := ExecuteSelect(SQL);
  Result := TList.Create();
  for I := Low(Results) to High(Results) do begin
    Card := TCard.Create();
    Card.ID := StrToInt(Results[I][0]);
    Card.Race := GetRaceByID(StrToInt(Results[I][1]));
    Card.CardType := Results[I][2];
    Card.TimeOfCreation := Results[I][3];
    Card.Title := Results[I][4];
    Card.Subtitle := Results[I][5];
    Card.PossessionType := Results[I][6];
    Card.TwilightCost := StrToInt(Results[I][7]);
    Card.IsUnique := (Results[I][8] = '1');
    Card.Strength := Results[I][9];
    Card.Health := Results[I][10];
    Card.Time := Results[I][11];
    Card.Text := Results[I][12];
    Card.Comment := Results[I][13];
    Card.PictureFilename := Results[I][14];
    if (Length(Results[I][15]) > 0) then begin
      ThumbnailStream := TStringStream.Create(Results[I][15]);
      Card.Thumbnail := Imager.LoadPictureFromStream(ThumbnailStream, 'thumb.jpg');
      ThumbnailStream.Free();
    end;
    Card.ExactTimeOfCreation := Results[I][16];
    Card.TimeOfModification := Results[I][17];
    Card.InternalComment := Results[I][18];
    Result.Add(Card);
  end;
end;



// Retrieves all the sites in the database
function TPersistence.RetrieveSites(): TList;
var SQL: String;
    Results: RecordSet;
    I: Integer;
    Site: TSite;
begin
  SQL := 'SELECT id, time_of_creation, title, picture_filename, time, ' +
         'twilight_cost, direction, text, internal_comment FROM sites ORDER BY title';
  Results := ExecuteSelect(SQL);
  Result := TList.Create();
  for I := Low(Results) to High(Results) do begin
    Site := TSite.Create();
    Site.ID := StrToInt(Results[I][0]);
    Site.TimeOfCreation := Results[I][1];
    Site.Title := Results[I][2];
    Site.PictureFilename := Results[I][3];
    Site.Time := Results[I][4];
    Site.TwilightCost := StrToInt(Results[I][5]);
    Site.Direction := Results[I][6];
    Site.Text := Results[I][7];
    Site.Comment := Results[I][8];
    Result.Add(Site);
  end;
end;



// Retrieves certain settings from the settings table, in name-value pairs.
// Values are TStrings.
function TPersistence.RetrieveSettings(): TElHashList;
var SQL: String;
    Results: RecordSet;
    I: Integer;
    Name: String;
    Value: TString;
begin
  Result := TElHashList.Create();
  SQL := 'SELECT name, value FROM settings';
  Results := ExecuteSelect(SQL);
  for I := Low(Results) to High(Results) do begin
    Name := Results[I][0];
    Value := TString.Create();
    Value.Value := Results[I][1];
    Result.AddItem(Name, Value);
  end;
end;



// Retrieves all the decks in the database
function TPersistence.RetrieveDecks(): TList;
var SQL, SubSQL: String;
    Results, SubResults: RecordSet;
    I, J: Integer;
    Deck: TDeck;
    DeckCard: TDeckCard;
    DeckSite: TDeckSite;
begin
  SubResults := nil;
  SQL := 'SELECT id, title, time_of_creation, comment FROM decks ' +
         'ORDER BY title';
  Results := ExecuteSelect(SQL);
  Result := TList.Create();
  for I := Low(Results) to High(Results) do begin
    Deck := TDeck.Create();
    Deck.ID := StrToInt(Results[I][0]);
    Deck.Title := Results[I][1];
    Deck.TimeOfCreation := Results[I][2];
    Deck.Comment := Results[I][3];
    SubSQL := 'SELECT id, card_id FROM deck_cards WHERE deck_id = :deck_id';
    SubResults := ExecuteSelect(SubSQL, [Deck.Id]);
    Deck.Cards := TList.Create();
    for J := Low(SubResults) to High(SubResults) do begin
      DeckCard := TDeckCard.Create();
      DeckCard.ID := StrToInt(SubResults[J][0]);
      DeckCard.Card := GetCardByID(StrToInt(SubResults[J][1]));
      Deck.Cards.Add(DeckCard);
    end;
    SubSQL := 'SELECT id, site_id FROM deck_sites WHERE deck_id = :deck_id';
    SubResults := ExecuteSelect(SubSQL, [Deck.Id]);
    Deck.Sites := TList.Create();
    for J := Low(SubResults) to High(SubResults) do begin
      DeckSite := TDeckSite.Create();
      DeckSite.ID := StrToInt(SubResults[J][0]);
      DeckSite.Site := GetSiteByID(StrToInt(SubResults[J][1]));
      Deck.Sites.Add(DeckSite);
    end;
    Result.Add(Deck);
  end;
end;


// Saves the card in the database
procedure TPersistence.StoreCard(Card: TCard);
var SQL: String;
    NewID: Integer;
    IsInsert : Boolean;
    ContentPictureStream, ThumbnailStream: TStringStream;
begin
  ContentPictureStream := TStringStream.Create('');
  ThumbnailStream := TStringStream.Create('');
  if (Card.ContentPicture <> nil) then begin
    Imager.SavePictureToStream(Card.ContentPicture.Bitmap, ContentPictureStream, '.png');
  end;
  if (Card.Thumbnail <> nil) then begin
    Imager.SavePictureToStream(Card.Thumbnail.Bitmap, ThumbnailStream, '.jpg');
  end;
  IsInsert := (Card.ID = 0);
  if (IsInsert) then
    begin
    SQL := 'INSERT INTO cards (race_id, type, title, ' +
           'subtitle, possession_type, twilight_cost, is_unique, strength, health, ' +
           'time, text, comment, picture_filename, content_picture, thumbnail, ' +
           'last_modified, time_of_creation, internal_comment) VALUES ' +
           '(:race_id, :type, :title, ' +
           ':subtitle, :possession_type, :twilight_cost, :is_unique, :strength, :health, '+
           ':time, :text, :comment, :picture_filename, :content_picture, :thumbnail, ' +
           ':last_modified, :time_of_creation, :internal_comment)';
    NewID := ExecuteStore(SQL, [Card.Race.ID, Card.CardType, Card.Title,
                              Card.Subtitle, Card.PossessionType, Card.TwilightCost,
                              Card.IsUnique, Card.Strength, Card.Health, Card.Time,
                              Card.Text, Card.Comment, Card.PictureFilename,
                              @ContentPictureStream, @ThumbnailStream, Card.TimeOfModification,
                              Card.ExactTimeOfCreation, Card.InternalComment]);
    end
  else
    begin
    SQL := 'UPDATE cards SET race_id = :race_id, type = :type, title = :title, ' +
           'subtitle = :subtitle, possession_type = :possession_type, twilight_cost = :twilight_cost, ' +
           'is_unique = :is_unique, strength = :strength, health = :health, time = :time, text = :text, ' +
           'comment = :comment, picture_filename = :picture_filename, content_picture = :content_picture, ' +
           'thumbnail = :thumbnail, last_modified = :last_modified, internal_comment = :internal_comment WHERE id = :id';
    NewID := ExecuteStore(SQL, [Card.Race.ID, Card.CardType, Card.Title,
                                Card.Subtitle, Card.PossessionType, Card.TwilightCost,
                                Card.IsUnique, Card.Strength, Card.Health, Card.Time,
                                Card.Text, Card.Comment, Card.PictureFilename,
                                @ContentPictureStream, @ThumbnailStream, Card.TimeOfModification,
                                Card.InternalComment, Card.ID]);
    end;
  ThumbnailStream.Free();
  ContentPictureStream.Free();
  if (IsInsert) then
    Card.ID := NewID;
end;



// Saves the site in the database
procedure TPersistence.StoreSite(Site: TSite);
var SQL: String;
    NewID: Integer;
    IsInsert : Boolean;
    ContentPictureStream: TStringStream;
begin
  ContentPictureStream := TStringStream.Create('');
  if (Site.ContentPicture <> nil) then begin
    Imager.SavePictureToStream(Site.ContentPicture.Bitmap, ContentPictureStream, '.png');
  end;
  IsInsert := (Site.ID = 0);
  if (IsInsert) then
    begin
    SQL := 'INSERT INTO sites (time_of_creation, title, picture_filename, content_picture, ' +
           'time, twilight_cost, direction, text, internal_comment) ' +
           'VALUES (datetime("now"), :title, :picture_filename, ' +
           ':content_picture, :time, :twilight_cost, :direction, :text, :internal_comment)';
    NewID := ExecuteStore(SQL, [Site.Title, Site.PictureFilename, @ContentPictureStream,
                                Site.Time, Site.TwilightCost, Site.Direction,
                                Site.Text, Site.Comment]);
    end
  else begin
    SQL := 'UPDATE sites SET title = :title, picture_filename = :picture_filename, ' +
           'content_picture = :content_picture, time = :time, twilight_cost = :twilight_cost, ' +
           'direction = :direction, text = :text, internal_comment = :internal_comment WHERE id = :id';
    NewID := ExecuteStore(SQL, [Site.Title, Site.PictureFilename, @ContentPictureStream, Site.Time,
                                Site.TwilightCost, Site.Direction, Site.Text, Site.Comment, Site.ID]);
  end;
  if (IsInsert) then
    Site.ID := NewID;
end;



// Saves the deck in the database, including all the cards of the deck
procedure TPersistence.StoreDeck(Deck: TDeck);
var SQL: String;
    I, NewID, Result: Integer;
    IsInsert : Boolean;
    DeckCard: TDeckCard;
    DeckSite: TDeckSite;
    Results: RecordSet;
begin
  Results := nil;
  IsInsert := (Deck.ID = 0);
  if (IsInsert) then begin
    SQL := 'INSERT INTO decks (title, time_of_creation, comment) VALUES (:title, datetime("now"), :comment)';
    NewID := ExecuteStore(SQL, [Deck.Title]);
  end else begin
    SQL := 'UPDATE decks SET title = :title, comment = :comment WHERE id = :id';
    NewID := ExecuteStore(SQL, [Deck.Title, Deck.Comment, Deck.ID]);
  end;
  if (IsInsert) then begin
    Deck.ID := NewID;
    SQL := 'SELECT time_of_creation FROM decks WHERE id = :deck_id';
    Results := ExecuteSelect(SQL, [Deck.Id]);
    Deck.TimeOfCreation := Results[0][0];
  end;
  for I := 0 to Deck.Cards.Count - 1 do begin
    DeckCard := Deck.Cards.Items[I];
    IsInsert := (DeckCard.ID = 0);
    if (DeckCard.IsDeleted) then begin
      // If the item has not been saved yet, we need to do nothing
      if (not IsInsert) then begin
        SQL := 'DELETE FROM deck_cards WHERE id = :deckcard_id';
        Result := ExecuteAction(SQL, [DeckCard.ID]);
        if (Result = 0) then ErrorMessage();
        if (DEBUG) then ShowMessage('Deleted ' + IntToStr(Result) + ' rows from deck_cards.');
      end;
    end else begin
      // If the card already exists, we need to do nothing
      if (IsInsert) then begin
        SQL := 'INSERT INTO deck_cards (deck_id, card_id) VALUES (:deck_id, :card_id)';
        NewID := ExecuteStore(SQL, [Deck.ID, DeckCard.Card.ID]);
        DeckCard.ID := NewID;
      end;
    end;
  end;
  for I := 0 to Deck.Sites.Count - 1 do begin
    DeckSite := Deck.Sites.Items[I];
    IsInsert := (DeckSite.ID = 0);
    if (DeckSite.IsDeleted) then begin
      // If the item has not been saved yet, we need to do nothing
      if (not IsInsert) then begin
        SQL := 'DELETE FROM deck_sites WHERE id = :decksite_id';
        Result := ExecuteAction(SQL, [DeckSite.ID]);
        if (Result = 0) then ErrorMessage();
        if (DEBUG) then ShowMessage('Deleted ' + IntToStr(Result) + ' rows from deck_sites.');
      end;
    end else begin
      // If the card already exists, we need to do nothing
      if (IsInsert) then begin
        SQL := 'INSERT INTO deck_sites (deck_id, site_id) VALUES (:deck_id, :site_id)';
        DeckSite.ID := ExecuteStore(SQL, [Deck.ID, DeckSite.Site.ID]);
      end;
    end;
  end;
end;



// Saves the object. If it's a new item, assigns it an ID.
// If it has subitems, like a TDeck, saves those also.
procedure TPersistence.Store(Item: TDataClass);
begin
  if (Item is TCard) then begin
    StoreCard(Item as TCard);
  end else if (Item is TDeck) then begin
    StoreDeck(Item as TDeck);
  end else if (Item is TSite) then begin
    StoreSite(Item as TSite);
  end;
end;



// Performs the INSERT/UPDATE clause and in case of INSERT, returns the LAST_INSERT_ID.
function TPersistence.ExecuteStore(SQL: String): Integer;
begin
  Result := ExecuteStore(SQL, []);
end;


// Performs the INSERT/UPDATE clause and in case of INSERT, returns the LAST_INSERT_ID.
function TPersistence.ExecuteStore(SQL: String; const Parameters: array of const): Integer;
var
  Stmt: sqlite3_stmt_ptr;
  DBResult: Integer;
begin
  Result := 0;
  try
    sqlite3_check(sqlite3_prepare_v2(DB, PChar(SQL), -1, @Stmt, nil), DB);
    if (Length(Parameters) > 0) then
      BindParameters(Stmt, Parameters);
    DBResult := sqlite3_check(sqlite3_step(Stmt), DB);
    if (DBResult in [SQLITE_OK, SQLITE_DONE]) then
      Result := sqlite3_last_insert_rowid(DB);
  finally
    try sqlite3_finalize(Stmt); finally end;
  end;
end;



// Deletes the specified object from the database. Deletes all
// the db dependencies also
procedure TPersistence.Delete(Item: TDataClass);
var
  SQL: String;
  Result: Integer;
begin
  if (Item.ID <> 0) then begin
    if (Item is TCard) then begin
      SQL := 'DELETE FROM deck_cards WHERE card_id = :card_id';
      Result := ExecuteAction(SQL, [Item.ID]);
      if (DEBUG) then ShowMessage('Deleted ' + IntToStr(Result) + ' rows from deck_cards.');
      SQL := 'DELETE FROM cards WHERE id = :card_id';
      Result := ExecuteAction(SQL, [Item.ID]);
      if (DEBUG) then ShowMessage('Deleted ' + IntToStr(Result) + ' rows from cards.');
    end else if (Item is TDeck) then begin
      SQL := 'DELETE FROM deck_cards WHERE deck_id = :deck_id';
      Result := ExecuteAction(SQL, [Item.ID]);
      if (DEBUG) then ShowMessage('Deleted ' + IntToStr(Result) + ' rows from deck_cards.');
      SQL := 'DELETE FROM deck_sites WHERE deck_id = :deck_id';
      Result := ExecuteAction(SQL, [Item.ID]);
      if (DEBUG) then ShowMessage('Deleted ' + IntToStr(Result) + ' rows from deck_sites.');
      SQL := 'DELETE FROM decks WHERE id = :deck_id';
      Result := ExecuteAction(SQL, [Item.ID]);
      if (DEBUG) then ShowMessage('Deleted ' + IntToStr(Result) + ' rows from decks.');
    end else if (Item is TSite) then begin
      SQL := 'DELETE FROM deck_sites WHERE site_id = :site_id';
      Result := ExecuteAction(SQL, [Item.ID]);
      if (DEBUG) then ShowMessage('Deleted ' + IntToStr(Result) + ' rows from deck_sites.');
      SQL := 'DELETE FROM sites WHERE id = :site_id';
      Result := ExecuteAction(SQL, [Item.ID]);
      if (DEBUG) then ShowMessage('Deleted ' + IntToStr(Result) + ' rows from sites.');
    end else begin
      raise Exception.Create('Fatal Error: trying to save an object of unknown class ' + Item.ClassName);
    end;
  end;
end;


// Closes the database connection.
destructor TPersistence.Destroy();
begin
  sqlite3_close(DB);
end;


// Retrieves the content picture of the specified object.
function TPersistence.RetrieveContentPicture(Item: TDataClass): TPicture;
var
  Results: RecordSet;
  StringStream: TStringStream;
  SQL: String;
begin
  Result := nil;
  if Item is TCard then begin
    SQL := 'SELECT content_picture FROM cards WHERE id = :id';
  end else if Item is TSite then begin
    SQL := 'SELECT content_picture FROM sites WHERE id = :id';
  end;
  if SQL <> '' then begin
    Results := ExecuteSelect(SQL, [Item.ID]);
    if ((Length(Results) > 0) and (Length(Results[0][0]) > 0)) then begin
      StringStream := TStringStream.Create(Results[0][0]);
      Result := Imager.LoadPictureFromStream(StringStream, '.png');
      StringStream.Free();
    end;
  end else begin
    raise Exception.Create('Fatal Error: trying to get content picture for an object of unknown class ' + Item.ClassName);
  end;
end;



// Returns the size of the database file, in bytes.
function TPersistence.GetSize(): Integer;
begin
  Result := GetFileSize(Filename);
end;


// Creates the database tables and inserts default data, if needed
procedure TPersistence.CheckRequiredTables();
const
  TABLES: array[1..8] of String = (
    'cards', 'deck_cards', 'deck_sites', 'decks', 'races', 'settings', 'sites', 'times'
  );
  TABLE_DEFINITIONS: array[1..8] of String = (
    'CREATE TABLE IF NOT EXISTS cards ( id INTEGER PRIMARY KEY NOT NULL, race_id INTEGER NOT NULL DEFAULT 0, type TEXT NOT NULL, time_of_creation TEXT NOT NULL DEFAULT "", title TEXT NOT NULL, subtitle TEXT, ' + 'possession_type TEXT, twilight_cost INTEGER DEFAULT NULL, is_unique INTEGER DEFAULT 0, strength TEXT, health TEXT, time TEXT, TEXT TEXT, comment TEXT, ' + 'picture_filename TEXT, content_picture BLOB, thumbnail BLOB, last_modified TEXT DEFAULT NULL, internal_comment TEXT)',
    'CREATE TABLE IF NOT EXISTS deck_cards ( id INTEGER PRIMARY KEY NOT NULL, deck_id INTEGER NOT NULL DEFAULT 0, card_id INTEGER NOT NULL DEFAULT 0)',
    'CREATE TABLE IF NOT EXISTS deck_sites ( id INTEGER PRIMARY KEY NOT NULL, deck_id INTEGER NOT NULL DEFAULT 0, site_id INTEGER NOT NULL DEFAULT 0)',
    'CREATE TABLE IF NOT EXISTS decks ( id INTEGER PRIMARY KEY NOT NULL, title TEXT NOT NULL, time_of_creation TEXT NOT NULL DEFAULT "", comment TEXT)',
    'CREATE TABLE IF NOT EXISTS races ( id INTEGER PRIMARY KEY NOT NULL, name TEXT NOT NULL DEFAULT "", is_good INTEGER DEFAULT 0, health_picture_left INTEGER NOT NULL DEFAULT 0, health_picture_top INTEGER NOT NULL DEFAULT 0, ' + 'strength_picture_left INTEGER NOT NULL DEFAULT 0, strength_picture_top INTEGER NOT NULL DEFAULT 0, character_picture BLOB, other_picture BLOB, health_picture BLOB, strength_picture BLOB)',
    'CREATE TABLE IF NOT EXISTS settings ( name TEXT PRIMARY KEY NOT NULL DEFAULT "", value TEXT NOT NULL )',
    'CREATE TABLE IF NOT EXISTS sites ( id INTEGER PRIMARY KEY NOT NULL, time_of_creation TEXT NOT NULL DEFAULT "", title TEXT NOT NULL, picture_filename TEXT, content_picture BLOB, time TEXT NOT NULL, ' + 'twilight_cost INTEGER DEFAULT NULL, direction TEXT NOT NULL, TEXT TEXT, internal_comment TEXT)',
    'CREATE TABLE IF NOT EXISTS times ( time TEXT PRIMARY KEY NOT NULL DEFAULT "", ordering INTEGER DEFAULT NULL)'
  );
  RACE_DEFAULTS: array[1..4] of String = (
    'INSERT OR IGNORE INTO races (id, name, is_good, health_picture_left, health_picture_top, strength_picture_left, strength_picture_top, character_picture, other_picture, strength_picture, health_picture) ' + 'VALUES (1, "Fellowship", 1, 34, 386, 33, 320, :character_picture, :other_picture, :strength_picture, :health_picture)',
    'INSERT OR IGNORE INTO races (id, name, is_good, health_picture_left, health_picture_top, strength_picture_left, strength_picture_top, character_picture, other_picture, strength_picture, health_picture) ' + 'VALUES (5, "Soft Drink", 0, 34, 386, 35, 320, :character_picture, :other_picture, :strength_picture, :health_picture)',
    'INSERT OR IGNORE INTO races (id, name, is_good, health_picture_left, health_picture_top, strength_picture_left, strength_picture_top, character_picture, other_picture, strength_picture, health_picture) ' + 'VALUES (6, "Strong Drink", 0, 36, 387, 36, 320, :character_picture, :other_picture, :strength_picture, :health_picture)',
    'INSERT OR IGNORE INTO races (id, name, is_good, health_picture_left, health_picture_top, strength_picture_left, strength_picture_top, character_picture, other_picture, strength_picture, health_picture) ' + 'VALUES (7, "Medium Drink", 0, 38, 388, 35, 320, :character_picture, :other_picture, :strength_picture, :health_picture)'
  );
  RACE_NAMES: array[1..4] of String = (
    'Fellowship', 'Soft Drink', 'Strong Drink', 'Medium Drink'
  );
  RACE_PICTURES: array[1..4] of String = (
    'Character', 'Other', 'Strength', 'Health'
  );
  SETTING_DEFAULTS: array[1..4] of String = (
    'INSERT OR IGNORE INTO settings (name, value) VALUES ("SiteFirstTime", "14")',
    'INSERT OR IGNORE INTO settings (name, value) VALUES ("CardThumbnailWidth", "100")',
    'INSERT OR IGNORE INTO settings (name, value) VALUES ("CardThumbnailHeight", "140")',
    'INSERT OR IGNORE INTO settings (name, value) VALUES ("DatabaseCreated", datetime("now"))'
  );
  TIME_DEFAULTS: array[1..9] of String = (
    'INSERT OR IGNORE INTO times (time, ordering) VALUES ("14", 1)',
    'INSERT OR IGNORE INTO times (time, ordering) VALUES ("16", 2)',
    'INSERT OR IGNORE INTO times (time, ordering) VALUES ("18", 3)',
    'INSERT OR IGNORE INTO times (time, ordering) VALUES ("20", 4)',
    'INSERT OR IGNORE INTO times (time, ordering) VALUES ("22", 5)',
    'INSERT OR IGNORE INTO times (time, ordering) VALUES ("00", 6)',
    'INSERT OR IGNORE INTO times (time, ordering) VALUES ("02", 7)',
    'INSERT OR IGNORE INTO times (time, ordering) VALUES ("04", 8)',
    'INSERT OR IGNORE INTO times (time, ordering) VALUES ("07", 9)'
  );
var
  I, J, K: Integer;
  TableDefaultsMap: TElHashList;
  Defaults: TStringList;
  DBResults: RecordSet;
  Streams: array of TResourceStream;
begin
  TableDefaultsMap := TElHashlist.Create();
  Defaults := TStringList.Create();
  For I := Low(RACE_DEFAULTS) to High(RACE_DEFAULTS) do Defaults.Add(RACE_DEFAULTS[I]);
  TableDefaultsMap.AddItem('races', Defaults);
  Defaults := TStringList.Create();
  For I := Low(SETTING_DEFAULTS) to High(SETTING_DEFAULTS) do Defaults.Add(SETTING_DEFAULTS[I]);
  TableDefaultsMap.AddItem('settings', Defaults);
  Defaults := TStringList.Create();
  For I := Low(TIME_DEFAULTS) to High(TIME_DEFAULTS) do Defaults.Add(TIME_DEFAULTS[I]);
  TableDefaultsMap.AddItem('times',    Defaults);

  for I := Low(TABLES) to High(TABLES) do begin
    DBResults := ExecuteSelect('SELECT name FROM sqlite_master WHERE name = :name', [TABLES[I]]);
    if Length(DBResults) = 0 then begin
      ExecuteAction(TABLE_DEFINITIONS[I]);
      Defaults := TableDefaultsMap.Item[Tables[I]];
      if Defaults <> nil then begin
        for J := 0 to Defaults.Count - 1 do begin
          if 'races' = TABLES[I] then begin
            SetLength(Streams, Length(RACE_PICTURES));
            for K := Low(RACE_PICTURES) to High(RACE_PICTURES) do begin
              Streams[K - 1] := TResourceStream.Create(hInstance, StringReplace(RACE_NAMES[J + 1], ' ', '_', [rfReplaceAll]) + '_' + RACE_PICTURES[K], RT_RCDATA);
            end;
            ExecuteAction(Defaults[J], [@Streams[0], @Streams[1], @Streams[2], @Streams[3]]);
            for K := Low(Streams) to High(Streams) do Streams[K].Free();
          end else
            ExecuteAction(Defaults[J]);
        end;
        Defaults.Free();
      end;
    end;
  end;
end;




end.
