(**
 * Database storage, retrieval and creation functionality.
 *
 * @author    Erki Suurjaak
 * @created   21.12.2003
 * @modified  12.11.2011
 *)
unit Persistence;

interface

uses Windows, Dialogs, DataClasses, classes, DISQLite3Api, Graphics,
     DCL_intf;

type
  // A handy class for loading data from a database query.
  TRows = class(TObject)
  private
    PointerArray: array of array of Pointer;
    PointerTypes: array of String;
    // Returns the specified row and column as a boolean.
    function GetBool(Row, Col: Integer): Boolean;
    // Returns the specified row and column as an integer.
    function GetInt(Row, Col: Integer): Integer;
    // Loads a TPicture streamed from the contents at the specified row and column.
    // FileExtension specifies the file extension, e.g. '.png' or '.jpg'
    function GetPic(Row, Col: Integer; FileExtension: String = '.png'): TPicture;
    // Returns the specified row and column as a string.
    function GetStr(Row, Col: Integer): String;
  public
    Count: Integer;
    // Creates a matrix with all available rows from the SQLite statement result.
    constructor Create(Stmt: sqlite3_stmt_ptr; DB: sqlite3_ptr);
    destructor Destroy(); override;
    // Returns the specified row and column as a boolean.
    property Bool[Row, Col: Integer]: Boolean read GetBool;
    // Returns the specified row and column as an integer.
    property Int[Row, Col: Integer]: Integer read GetInt;
    // Loads a TPicture streamed from the contents at the specified row and column.
    // FileExtension specifies the file extension, e.g. '.png' or '.jpg'
    property Pic[Row, Col: Integer; FileExtension: String = '.png']: TPicture read GetPic;
    // Returns the specified row and column as a string.
    property Str[Row, Col: Integer]: String read GetStr;
  end;


  TPersistence = class(TObject)
  private
    DB: sqlite3_ptr;
    // Displays an error with the message and information from the database connection
    procedure ErrorMessage(Msg: String = '');
    // Binds parameters to the specified SQL statement
    procedure BindParameters(Stmt: sqlite3_stmt_ptr; const Parameters: array of const);
    // Executes the SELECT statement in SQL and returns the result
    function ExecuteSelect(SQL: String): TRows; overload;
    // Executes the SELECT statement in SQL and returns the result
    function ExecuteSelect(SQL: String; const Parameters: array of const): TRows; overload;
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
    // Updates the DatabaseModified setting.
    procedure UpdateModification();
  public
    Filename: String;
    IsNew: Boolean; // Whether it's a newly created database, or existed before
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
    function RetrieveTimes(): TStringList;
    // Retrieves all the sites in the database
    function RetrieveSites(): TList;
    // Retrieves certain settings from the settings table, in name-value pairs.
    function RetrieveSettings(): IStrStrMap;
    // Retrieves all the card types in the database
    function RetrieveCardTypes(): TList;
    // Retrieves the content picture of the specified object.
    function RetrieveContentPicture(Item: TDataClass): TPicture;
    // Saves the object. If it's a new item, assigns it an ID.
    // If it has subitems, like a TDeck, saves those also.
    procedure Store(Item: TDataClass);
    // Saves the thumbnail image of the card.
    procedure StoreCardThumbnail(Card: TCard);
    // Deletes the specified object from the database. Deletes all
    // the db dependencies also
    procedure Delete(Item: TDataClass);
  end;

implementation

uses
  sysutils, jpeg, pngimage, DIUtils, Globals, HashMap;


// Creates a matrix with all available rows from the SQLite statement result.
constructor TRows.Create(Stmt: sqlite3_stmt_ptr; DB: sqlite3_ptr);
var
  I, NumberOfFields: Integer;
  ColInt: PInteger;
  ColStr: PString;
  Stream: TMemoryStream;
begin
  Count := 0;
  SetLength(PointerArray, Count, 0);
  while (sqlite3_check(sqlite3_step(Stmt), DB) = SQLITE_ROW) do begin
    NumberOfFields := sqlite3_column_count(Stmt);
    SetLength(PointerArray, Count + 1, NumberOfFields);
    if (Length(PointerTypes) = 0) then SetLength(PointerTypes, NumberOfFields);
    for I := 0 to NumberOfFields - 1 do begin
      case sqlite3_column_type(Stmt, I) of
        SQLITE_INTEGER: begin
          New(ColInt);
          ColInt^ := sqlite3_column_int(Stmt, I);
          PointerArray[Count, I] := ColInt;
          PointerTypes[I] := 'integer';
        end;
        SQLITE_BLOB: begin
          Stream := TMemoryStream.Create();
          Stream.Write(sqlite3_column_blob(Stmt, I)^, sqlite3_column_bytes(Stmt, I));
          PointerArray[Count, I] := Stream;
          PointerTypes[I] := 'object';
        end;
        else begin
          New(ColStr);
          ColStr^ := sqlite3_column_str(Stmt, I);
          PointerArray[Count, I] := ColStr;
          PointerTypes[I] := 'ansistring';
        end;
      end;
    end;
    Inc(Count);
  end;
end;


destructor TRows.Destroy();
var
  I, J: Integer;
begin
  for I := 0 to Length(PointerArray) - 1 do begin
    for J := 0 to Length(PointerArray[I]) - 1 do begin
      if (PointerTypes[J] = 'ansistring') then begin
        // Free string memory by zeroing its length
        SetLength(PString(PointerArray[I, J])^, 0);
      end;
      Dispose(PointerArray[I, J]); // Free pointer or object
    end;
  end;
  SetLength(PointerArray, 0, 0);
  SetLength(PointerTypes, 0);
end;


// Returns the specified row and column as a boolean.
function TRows.GetBool(Row, Col: Integer): Boolean;
begin
  Result := PInteger(PointerArray[Row, Col])^ = 1;
end;


// Returns the specified row and column as an integer.
function TRows.GetInt(Row, Col: Integer): Integer;
begin
  Result := PInteger(PointerArray[Row, Col])^;
end;


// Loads a TPicture streamed from the contents at the specified row and column.
// Format specifies the file extension, e.g. '.png' or '.jpg'
function TRows.GetPic(Row, Col: Integer; FileExtension: String = '.png'): TPicture;
var
  Stream: TStream;
begin
  Result := nil;
  Stream := TStream(PointerArray[Row, Col]);
  if (Stream.Size > 0) then begin
    Result := Imager.LoadPictureFromStream(Stream, FileExtension);
  end;
end;


// Returns the specified row and column as a string.
function TRows.GetStr(Row, Col: Integer): String;
begin
  Result := PString(PointerArray[Row, Col])^;
end;


// Creates a new database connection. If the database does not exist,
// creates it. If required tables do not exist, creates them, and fills
// tables (like settings, races, times) with default values.
constructor TPersistence.Create(Filename: String);
begin
  try
    IsNew := not FileExists(Filename) or (GetFileSize(Filename) = 0);
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
  UpdateModification();
end;



// Executes the SELECT statement in SQL and returns the result
function TPersistence.ExecuteSelect(SQL: String): TRows;
begin
  Result := ExecuteSelect(SQL, []);
end;


// Executes the SELECT statement in SQL and returns the result
function TPersistence.ExecuteSelect(SQL: String; const Parameters: array of const): TRows;
var
  Stmt: sqlite3_stmt_ptr;
begin
  Result := nil;

  try
    sqlite3_check(sqlite3_prepare_v2(DB, PChar(SQL), -1, @Stmt, nil), DB);
    if (Length(Parameters) > 0) then
      BindParameters(Stmt, Parameters);
    Result := TRows.Create(Stmt, DB);
  except
    on E: Exception do begin
      FreeAndNil(Result);
      ErrorMessage('TPersistence.Retrieve failed with message ''' + E.ClassName + ': ' + E.Message + '''.');
    end;
  end;
  sqlite3_finalize(Stmt);
end;



// Retrieves all the races in the database
function TPersistence.RetrieveRaces(): TList;
var SQL: String;
    Rows: TRows;
    I: Integer;
    Race: TRace;
    PictureStream: TStringStream;
begin
  SQL := 'SELECT id, name, is_good, character_name, strength_picture_left, ' +
         'strength_picture_top, health_picture_left, health_picture_top, ' +
         'character_picture, other_picture, strength_picture, health_picture FROM races';
  Rows := ExecuteSelect(SQL);
  Result := TList.Create();
  for I := 0 to Rows.Count - 1 do begin
    Race := TRace.Create();
    Race.ID := Rows.Int[I, 0];
    Race.Name := Rows.Str[I, 1];
    Race.IsGood := Rows.Bool[I, 2];
    Race.CharacterName := Rows.Str[I, 3];
    Race.StrengthPictureLeft := Rows.Int[I, 4];
    Race.StrengthPictureTop := Rows.Int[I, 5];
    Race.HealthPictureLeft := Rows.Int[I, 6];
    Race.HealthPictureTop := Rows.Int[I, 7];
    Race.CharacterPicture := Rows.Pic[I, 8, '.png'];
    Race.OtherPicture := Rows.Pic[I, 9, '.png'];
    Race.StrengthPicture := Rows.Pic[I, 10, '.png'];
    Race.HealthPicture := Rows.Pic[I, 11, '.png'];
    Result.Add(Race);
  end;
  Rows.Free();
end;



// Retrieves all the times in the database
function TPersistence.RetrieveTimes(): TStringList;
var SQL: String;
    Rows: TRows;
    I: Integer;
begin
  SQL := 'SELECT time FROM times ORDER BY ordering';
  Rows := ExecuteSelect(SQL);
  Result := TStringList.Create();
  for I := 0 to Rows.Count - 1 do begin
    Result.Add(Rows.Str[I, 0]);
  end;
  Rows.Free();
end;



// Retrieves all the cards in the database
function TPersistence.RetrieveCards(): TList;
var SQL: String;
    Rows: TRows;
    I: Integer;
    Card: TCard;
begin
  SQL := 'SELECT id, race_id, type_id, time_of_creation, title, subtitle, possession_type, ' +
         'twilight_cost, is_unique, strength, health, time, text, comment, ' +
         'picture_filename, thumbnail, time_of_creation, last_modified, internal_comment FROM cards ' +
         'ORDER BY title, subtitle';
  Rows := ExecuteSelect(SQL);
  Result := TList.Create();
  for I := 0 to Rows.Count - 1 do begin
    Card := TCard.Create();
    Card.ID := Rows.Int[I, 0];
    Card.Race := GetRaceByID(Rows.Int[I, 1]);
    Card.CardType := GetCardTypeByID(Rows.Int[I, 2]);
    Card.TimeOfCreation := Rows.Str[I, 3];
    Card.Title := Rows.Str[I, 4];
    Card.Subtitle := Rows.Str[I, 5];
    Card.PossessionType := Rows.Str[I, 6];
    Card.TwilightCost := Rows.Int[I, 7];
    Card.IsUnique := Rows.Bool[I, 8];
    Card.Strength := Rows.Str[I, 9];
    Card.Health := Rows.Str[I, 10];
    Card.Time := Rows.Str[I, 11];
    Card.Text := Rows.Str[I, 12];
    Card.Comment := Rows.Str[I, 13];
    Card.PictureFilename := Rows.Str[I, 14];
    Card.Thumbnail := Rows.Pic[I, 15, '.jpg'];
    Card.ExactTimeOfCreation := Rows.Str[I, 16];;
    Card.TimeOfModification := Rows.Str[I, 17];;
    Card.InternalComment := Rows.Str[I, 18];;
    Result.Add(Card);
  end;
  Rows.Free();
end;



// Retrieves all the sites in the database
function TPersistence.RetrieveSites(): TList;
var SQL: String;
    Rows: TRows;
    I: Integer;
    Site: TSite;
begin
  SQL := 'SELECT id, time_of_creation, title, picture_filename, time, ' +
         'twilight_cost, direction, text, internal_comment FROM sites ORDER BY title';
  Rows := ExecuteSelect(SQL);
  Result := TList.Create();
  for I := 0 to Rows.Count - 1 do begin
    Site := TSite.Create();
    Site.ID := Rows.Int[I, 0];
    Site.TimeOfCreation := Rows.Str[I, 1];
    Site.Title := Rows.Str[I, 2];
    Site.PictureFilename := Rows.Str[I, 3];
    Site.Time := Rows.Str[I, 4];
    Site.TwilightCost := Rows.Int[I, 5];
    Site.Direction := Rows.Str[I, 6];
    Site.Text := Rows.Str[I, 7];
    Site.Comment := Rows.Str[I, 8];
    Result.Add(Site);
  end;
  Rows.Free();
end;



// Retrieves certain settings from the settings table, in name-value pairs.
function TPersistence.RetrieveSettings(): IStrStrMap;
var SQL: String;
    Rows: TRows;
    I: Integer;
begin
  Result := TStrStrHashMap.Create();
  SQL := 'SELECT name, value FROM settings';
  Rows := ExecuteSelect(SQL);
  for I := 0 to Rows.Count - 1 do begin
    Result.PutValue(Rows.Str[I, 0], Rows.Str[I, 1]);
  end;
  Rows.Free();
end;


// Retrieves all the card types in the database
function TPersistence.RetrieveCardTypes(): TList;
var SQL: String;
    Rows: TRows;
    I: Integer;
    CardType: TCardType;
begin
  SQL := 'SELECT id, name, is_character, is_possession, is_strength, is_health, ' +
         'content_picture_max_width, content_picture_max_height, ' +
         'content_picture_left, content_picture_top, middle_title_left, ' +
         'middle_title_width FROM card_types';
  Rows := ExecuteSelect(SQL);
  Result := TList.Create();
  for I := 0 to Rows.Count - 1 do begin
    CardType := TCardType.Create();

    CardType.ID := Rows.Int[I, 0];
    CardType.Name := Rows.Str[I, 1];
    CardType.IsCharacter := Rows.Bool[I, 2];
    CardType.IsPossession := Rows.Bool[I, 3];
    CardType.IsStrength := Rows.Bool[I, 4];
    CardType.IsHealth := Rows.Bool[I, 5];
    CardType.ContentPictureMaxWidth := Rows.Int[I, 6];
    CardType.ContentPictureMaxHeight := Rows.Int[I, 7];
    CardType.ContentPictureLeft := Rows.Int[I, 8];
    CardType.ContentPictureTop := Rows.Int[I, 9];
    CardType.MiddleTitleLeft := Rows.Int[I, 10];
    CardType.MiddleTitleWidth := Rows.Int[I, 11];

    Result.Add(CardType);
  end;
  Rows.Free();
end;


// Retrieves all the decks in the database
function TPersistence.RetrieveDecks(): TList;
var SQL, SubSQL: String;
    Rows, SubRows: TRows;
    I, J: Integer;
    Deck: TDeck;
    DeckCard: TDeckCard;
    DeckSite: TDeckSite;
begin
  SQL := 'SELECT id, title, time_of_creation, comment FROM decks ' +
         'ORDER BY title';
  Rows := ExecuteSelect(SQL);
  Result := TList.Create();
  for I := 0 to Rows.Count - 1 do begin
    Deck := TDeck.Create();
    Deck.ID := Rows.Int[I, 0];
    Deck.Title := Rows.Str[I, 1];
    Deck.TimeOfCreation := Rows.Str[I, 2];
    Deck.Comment := Rows.Str[I, 3];
    SubSQL := 'SELECT id, card_id FROM deck_cards WHERE deck_id = :deck_id';
    SubRows := ExecuteSelect(SubSQL, [Deck.Id]);
    for J := 0 to SubRows.Count - 1 do begin
      DeckCard := TDeckCard.Create();
      DeckCard.ID := SubRows.Int[J, 0];
      DeckCard.Card := GetCardByID(SubRows.Int[I, 1]);
      Deck.Cards.Add(DeckCard);
    end;
    SubRows.Free();
    SubSQL := 'SELECT id, site_id FROM deck_sites WHERE deck_id = :deck_id';
    SubRows := ExecuteSelect(SubSQL, [Deck.Id]);
    for J := 0 to SubRows.Count - 1 do begin
      DeckSite := TDeckSite.Create();
      DeckSite.ID := SubRows.Int[J, 0];
      DeckSite.Site := GetSiteByID(SubRows.Int[J, 1]);
      Deck.Sites.Add(DeckSite);
    end;
    SubRows.Free();
    Result.Add(Deck);
  end;
  Rows.Free();
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
    SQL := 'INSERT INTO cards (race_id, type_id, title, ' +
           'subtitle, possession_type, twilight_cost, is_unique, strength, health, ' +
           'time, text, comment, picture_filename, content_picture, thumbnail, ' +
           'last_modified, time_of_creation, internal_comment) VALUES ' +
           '(:race_id, :type_id, :title, ' +
           ':subtitle, :possession_type, :twilight_cost, :is_unique, :strength, :health, '+
           ':time, :text, :comment, :picture_filename, :content_picture, :thumbnail, ' +
           ':last_modified, :time_of_creation, :internal_comment)';
    NewID := ExecuteStore(SQL, [Card.Race.ID, Card.CardType.ID, Card.Title,
                              Card.Subtitle, Card.PossessionType, Card.TwilightCost,
                              Card.IsUnique, Card.Strength, Card.Health, Card.Time,
                              Card.Text, Card.Comment, Card.PictureFilename,
                              @ContentPictureStream, @ThumbnailStream, Card.TimeOfModification,
                              Card.ExactTimeOfCreation, Card.InternalComment]);
    end
  else
    begin
    SQL := 'UPDATE cards SET race_id = :race_id, type_id = :type_id, title = :title, ' +
           'subtitle = :subtitle, possession_type = :possession_type, twilight_cost = :twilight_cost, ' +
           'is_unique = :is_unique, strength = :strength, health = :health, time = :time, text = :text, ' +
           'comment = :comment, picture_filename = :picture_filename, content_picture = :content_picture, ' +
           'thumbnail = :thumbnail, last_modified = :last_modified, internal_comment = :internal_comment WHERE id = :id';
    NewID := ExecuteStore(SQL, [Card.Race.ID, Card.CardType.ID, Card.Title,
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
  ContentPictureStream.Free();
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
    Rows: TRows;
begin
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
    Rows := ExecuteSelect(SQL, [Deck.Id]);
    Deck.TimeOfCreation := Rows.Str[0, 0];
    Rows.Free();
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
  UpdateModification();
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
  Rows: TRows;
  SQL: String;
begin
  Result := nil;
  if Item is TCard then begin
    SQL := 'SELECT content_picture FROM cards WHERE id = :id';
  end else if Item is TSite then begin
    SQL := 'SELECT content_picture FROM sites WHERE id = :id';
  end;
  if SQL <> '' then begin
    Rows := ExecuteSelect(SQL, [Item.ID]);
    if (Rows.Count > 0) then begin
      Result := Rows.Pic[0, 0, '.png'];
    end;
    Rows.Free();
  end else begin
    raise Exception.Create('Fatal Error: trying to get content picture for an object of unknown class ' + Item.ClassName);
  end;
end;



// Returns the size of the database file, in bytes.
function TPersistence.GetSize(): Integer;
begin
  Result := GetFileSize(Filename);
end;


// Updates the DatabaseModified setting.
procedure TPersistence.UpdateModification();
var
  Rows: TRows;
begin
  try
    sqlite3_exec_fast(DB, 'INSERT OR REPLACE INTO settings (name, value) VALUES ("DatabaseModified", datetime("now"))');
    Rows := ExecuteSelect('SELECT value FROM settings WHERE name = "DatabaseModified"');
    if (Rows.Count > 0) and (Settings <> nil) then begin
      Settings.PutValue('DatabaseModified', Rows.Str[0, 0]);
    end;
    Rows.Free();
  except
  end;
end;


// Creates the database tables and inserts default data, if needed
procedure TPersistence.CheckRequiredTables();
const
  TABLES: array[1..9] of String = (
    'settings', 'cards', 'deck_cards', 'deck_sites', 'decks', 'races', 'sites', 'times', 'card_types'
  );
  TABLE_DEFINITIONS: array[1..9] of String = (
    'CREATE TABLE IF NOT EXISTS settings ( name TEXT PRIMARY KEY NOT NULL DEFAULT "", value TEXT NOT NULL )',
    'CREATE TABLE IF NOT EXISTS cards ( id INTEGER PRIMARY KEY NOT NULL, race_id INTEGER NOT NULL DEFAULT 0, type_id INTEGER NOT NULL, time_of_creation TEXT NOT NULL DEFAULT "", title TEXT NOT NULL, subtitle TEXT, ' + 'possession_type TEXT, twilight_cost INTEGER DEFAULT NULL, is_unique INTEGER DEFAULT 0, strength TEXT, health TEXT, time TEXT, TEXT TEXT, comment TEXT, ' + 'picture_filename TEXT, content_picture BLOB, thumbnail BLOB, last_modified TEXT DEFAULT NULL, internal_comment TEXT)',
    'CREATE TABLE IF NOT EXISTS deck_cards ( id INTEGER PRIMARY KEY NOT NULL, deck_id INTEGER NOT NULL DEFAULT 0, card_id INTEGER NOT NULL DEFAULT 0)',
    'CREATE TABLE IF NOT EXISTS deck_sites ( id INTEGER PRIMARY KEY NOT NULL, deck_id INTEGER NOT NULL DEFAULT 0, site_id INTEGER NOT NULL DEFAULT 0)',
    'CREATE TABLE IF NOT EXISTS decks ( id INTEGER PRIMARY KEY NOT NULL, title TEXT NOT NULL, time_of_creation TEXT NOT NULL DEFAULT "", comment TEXT)',
    'CREATE TABLE IF NOT EXISTS races ( id INTEGER PRIMARY KEY NOT NULL, name TEXT NOT NULL DEFAULT "", is_good INTEGER DEFAULT 0, character_name TEXT NOT NULL, health_picture_left INTEGER NOT NULL DEFAULT 0, health_picture_top INTEGER NOT NULL DEFAULT 0, ' + 'strength_picture_left INTEGER NOT NULL DEFAULT 0, strength_picture_top INTEGER NOT NULL DEFAULT 0, character_picture BLOB, other_picture BLOB, health_picture BLOB, strength_picture BLOB)',
    'CREATE TABLE IF NOT EXISTS sites ( id INTEGER PRIMARY KEY NOT NULL, time_of_creation TEXT NOT NULL DEFAULT "", title TEXT NOT NULL, picture_filename TEXT, content_picture BLOB, time TEXT NOT NULL, ' + 'twilight_cost INTEGER DEFAULT NULL, direction TEXT NOT NULL, TEXT TEXT, internal_comment TEXT)',
    'CREATE TABLE IF NOT EXISTS times ( time TEXT PRIMARY KEY NOT NULL DEFAULT "", ordering INTEGER DEFAULT NULL)',
    'CREATE TABLE IF NOT EXISTS card_types (id INTEGER PRIMARY KEY NOT NULL, name TEXT NOT NULL, is_character INTEGER NOT NULL, is_possession INTEGER NOT NULL, is_strength INTEGER NOT NULL, is_health INTEGER, ' + 'content_picture_max_width INTEGER, content_picture_max_height INTEGER, content_picture_left INTEGER NOT NULL, content_picture_top INTEGER NOT NULL, middle_title_left INTEGER NOT NULL, middle_title_width INTEGER NOT NULL)'
  );
  SETTING_DEFAULTS: array[1..6] of String = (
    'INSERT OR IGNORE INTO settings (name, value) VALUES ("CardThumbnailHeight", "140")',
    'INSERT OR IGNORE INTO settings (name, value) VALUES ("CardThumbnailWidth", "100")',
    'INSERT OR IGNORE INTO settings (name, value) VALUES ("DatabaseCreated", datetime("now"))',
    'INSERT OR IGNORE INTO settings (name, value) VALUES ("DatabaseModified", datetime("now"))',
    'INSERT OR IGNORE INTO settings (name, value) VALUES ("SiteContentPictureMaxHeight", "225")',
    'INSERT OR IGNORE INTO settings (name, value) VALUES ("SiteContentPictureMaxWidth", "395")'
  );
  RACE_DEFAULTS: array[1..4] of String = (
    'INSERT OR IGNORE INTO races (id, name, is_good, character_name, health_picture_left, health_picture_top, strength_picture_left, strength_picture_top, character_picture, other_picture, strength_picture, health_picture) ' + 'VALUES (1, "Fellowship", 1, "companion", 18, 370, 17, 304, :character_picture, :other_picture, :strength_picture, :health_picture)',
    'INSERT OR IGNORE INTO races (id, name, is_good, character_name, health_picture_left, health_picture_top, strength_picture_left, strength_picture_top, character_picture, other_picture, strength_picture, health_picture) ' + 'VALUES (5, "Soft Drink", 0, "minion", 18, 372, 19, 304, :character_picture, :other_picture, :strength_picture, :health_picture)',
    'INSERT OR IGNORE INTO races (id, name, is_good, character_name, health_picture_left, health_picture_top, strength_picture_left, strength_picture_top, character_picture, other_picture, strength_picture, health_picture) ' + 'VALUES (6, "Strong Drink", 0, "minion", 20, 371, 20, 304, :character_picture, :other_picture, :strength_picture, :health_picture)',
    'INSERT OR IGNORE INTO races (id, name, is_good, character_name, health_picture_left, health_picture_top, strength_picture_left, strength_picture_top, character_picture, other_picture, strength_picture, health_picture) ' + 'VALUES (7, "Medium Drink", 0, "minion", 22, 372, 19, 304, :character_picture, :other_picture, :strength_picture, :health_picture)'
  );
  RACE_NAMES: array[1..4] of String = (
    'Fellowship', 'Soft Drink', 'Strong Drink', 'Medium Drink'
  );
  RACE_PICTURES: array[1..4] of String = (
    'Character', 'Other', 'Strength', 'Health'
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
  CARD_TYPE_DEFAULTS: array[1..4] of String = (
    'INSERT OR IGNORE INTO card_types (id, name, is_character, is_possession, is_strength, is_health, content_picture_max_width, content_picture_max_height, content_picture_left, content_picture_top, middle_title_left, middle_title_width) VALUES ' + '(1, "character", 1, 0, 1, 1, 285, 211, 37, 70, 48, 241)',
    'INSERT OR IGNORE INTO card_types (id, name, is_character, is_possession, is_strength, is_health, content_picture_max_width, content_picture_max_height, content_picture_left, content_picture_top, middle_title_left, middle_title_width) VALUES ' + '(2, "condition", 0, 0, 1, 1, 254, 189, 75, 56, 84, 225)',
    'INSERT OR IGNORE INTO card_types (id, name, is_character, is_possession, is_strength, is_health, content_picture_max_width, content_picture_max_height, content_picture_left, content_picture_top, middle_title_left, middle_title_width) VALUES ' + '(3, "event", 0, 0, 0, 0, 254, 189, 75, 56, 84, 225)',
    'INSERT OR IGNORE INTO card_types (id, name, is_character, is_possession, is_strength, is_health, content_picture_max_width, content_picture_max_height, content_picture_left, content_picture_top, middle_title_left, middle_title_width) VALUES ' + '(4, "possession", 0, 1, 1, 1, 254, 189, 75, 56, 84, 225)'
  );
var
  I, J, K: Integer;
  TableDefaultsMap: IStrMap;
  Defaults: TStringList;
  Rows: TRows;
  Streams: array of TResourceStream;
begin
  TableDefaultsMap := TStrHashMap.Create();
  Defaults := TStringList.Create();
  for I := Low(RACE_DEFAULTS) to High(RACE_DEFAULTS) do Defaults.Add(RACE_DEFAULTS[I]);
  TableDefaultsMap.PutValue('races', Defaults);
  Defaults := TStringList.Create();
  for I := Low(TIME_DEFAULTS) to High(TIME_DEFAULTS) do Defaults.Add(TIME_DEFAULTS[I]);
  TableDefaultsMap.PutValue('times',    Defaults);
  Defaults := TStringList.Create();
  for I := Low(CARD_TYPE_DEFAULTS) to High(CARD_TYPE_DEFAULTS) do Defaults.Add(CARD_TYPE_DEFAULTS[I]);
  TableDefaultsMap.PutValue('card_types',    Defaults);

  for I := Low(TABLES) to High(TABLES) do begin
    Rows := ExecuteSelect('SELECT name FROM sqlite_master WHERE name = :name', [TABLES[I]]);
    Defaults := TableDefaultsMap.GetValue(Tables[I]) as TStringList;
    if Rows.Count = 0 then begin
      ExecuteAction(TABLE_DEFINITIONS[I]);
      if Defaults <> nil then begin
        for J := 0 to Defaults.Count - 1 do begin
          if 'races' = TABLES[I] then begin
            SetLength(Streams, Length(RACE_PICTURES));
            for K := Low(RACE_PICTURES) to High(RACE_PICTURES) do begin
              Streams[K - 1] := TResourceStream.Create(hInstance, StringReplace(RACE_NAMES[J + 1], ' ', '_', [rfReplaceAll]) + '_' + RACE_PICTURES[K], RT_RCDATA);
            end;
            ExecuteAction(Defaults[J], [@Streams[0], @Streams[1], @Streams[2], @Streams[3]]);
            for K := Low(Streams) to High(Streams) do Streams[K].Free();
            SetLength(Streams, 0);
          end else
            ExecuteAction(Defaults[J]);
        end;
      end;
    end;
    Rows.Free();
  end;
  // Setting defaults insert always
  for I := Low(SETTING_DEFAULTS) to High(SETTING_DEFAULTS) do begin
    sqlite3_exec_fast(DB, SETTING_DEFAULTS[I]);
  end;

end;


// Saves the thumbnail image of the card, if card exists in the database.
procedure TPersistence.StoreCardThumbnail(Card: TCard);
var SQL: String;
    ThumbnailStream: TStringStream;
begin
  if (Card.ID <> 0) then begin
    ThumbnailStream := TStringStream.Create('');
    if (Card.Thumbnail <> nil) then begin
      Imager.SavePictureToStream(Card.Thumbnail.Bitmap, ThumbnailStream, '.jpg');
    end;
    SQL := 'UPDATE cards SET thumbnail = :thumbnail WHERE id = :id';
    ExecuteStore(SQL, [@ThumbnailStream, Card.ID]);
    ThumbnailStream.Free();
  end;
end;



end.
