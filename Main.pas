(**
 * Application main form and all interface logic.
 *
 * @author    Erki Suurjaak
 * @created   12.20.2003
 * @modified  14.11.2011
 *)
unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Menus, PDJRotoLabel, Buttons, ExtDlgs,
  UHTMLabel, Globals, DataClasses, Imaging, Persistence;


type
  TMainForm = class(TForm)
    PageControl: TPageControl;
    PageDeckEditor: TTabSheet;
    PageCardEditor: TTabSheet;
    PageSiteEditor: TTabSheet;
    ComboCardList: TComboBox;
    LabelComboCardList: TLabel;
    PageDatabase: TTabSheet;
    PanelCardFields: TPanel;
    LabelEditPictureFilename: TLabel;
    EditPictureFilename: TEdit;
    ComboRace: TComboBox;
    ComboCardType: TComboBox;
    LabelComboRace: TLabel;
    LabelComboCardType: TLabel;
    ComboTime: TComboBox;
    LabelEditTime: TLabel;
    CheckIsUnique: TCheckBox;
    EditTwilightCost: TEdit;
    EditStrength: TEdit;
    EditHealth: TEdit;
    LabelEditTwilightCost: TLabel;
    LabelEditStrength: TLabel;
    LabelEditHealth: TLabel;
    EditTitle: TEdit;
    EditSubtitle: TEdit;
    EditPossessionType: TEdit;
    LabelEditTitle: TLabel;
    LabelEditSubtitle: TLabel;
    LabelEditPossessionType: TLabel;
    EditText: TMemo;
    EditComment: TMemo;
    LabelEditText: TLabel;
    LabelEditComment: TLabel;
    PopupAllCardsListMenu: TPopupMenu;
    AllCardsMenuViewCard: TMenuItem;
    Addcardtodeck1: TMenuItem;
    LabelListDecks: TLabel;
    LabelListDeckAllCards: TLabel;
    LabelListDeckCards: TLabel;
    PopupDeckCardsListMenu: TPopupMenu;
    CardListMenu: TMenuItem;
    Removecardfromdeck1: TMenuItem;
    ButtonRemoveCardFromDeck: TButton;
    ButtonAddCardToDeck: TButton;
    Time: TLabel;
    PanelCardPicture: TPanel;
    PageCardList: TTabSheet;
    LabelGridCards: TLabel;
    ComboListRaces: TComboBox;
    LabelComboListRaces: TLabel;
    LabelComboListCardTypes: TLabel;
    ComboListCardTypes: TComboBox;
    ComboListTimes: TComboBox;
    LabelComboListCards: TLabel;
    ListCards: TListView;
    ComboListDecks: TComboBox;
    LabelComboListDecks: TLabel;
    ListDeckAllCards: TListView;
    PanelSitePicture: TPanel;
    ListDeckCards: TListView;
    ButtonUndoCardChanges: TBitBtn;
    ButtonDeleteCard: TBitBtn;
    ButtonSaveCard: TBitBtn;
    ButtonNewCard: TBitBtn;
    ButtonNewDeck: TBitBtn;
    ButtonDeleteDeck: TBitBtn;
    ButtonSaveDeck: TBitBtn;
    ButtonUndoDeckChanges: TBitBtn;
    ButtonExit: TBitBtn;
    ButtonApplyListFilter: TBitBtn;
    ButtonClearListFilter: TBitBtn;
    SiteBackgroundPicture: TImage;
    SiteContentPicture: TImage;
    SiteTitle: TLabel;
    SiteTime: TLabel;
    SiteTwilightCostPicture: TImage;
    SiteTwilightCost: TLabel;
    SiteDirectionPicture: TImage;
    PanelSiteFields: TPanel;
    EditSiteTitle: TEdit;
    LabelEditSiteTitle: TLabel;
    LabelComboSiteTimes: TLabel;
    ComboSiteTimes: TComboBox;
    EditSiteTwilightCost: TEdit;
    LabelEditSiteTwilightCost: TLabel;
    EditSiteText: TMemo;
    LabelEditSiteText: TLabel;
    LabelComboSiteList: TLabel;
    ComboSiteList: TComboBox;
    EditSitePictureFilename: TEdit;
    ComboSiteDirections: TComboBox;
    LabelComboSiteDirections: TLabel;
    ButtonNewSite: TBitBtn;
    ButtonSaveSite: TBitBtn;
    ButtonDeleteSite: TBitBtn;
    ButtonUndoSiteChanges: TBitBtn;
    ButtonSaveSiteAs: TBitBtn;
    ButtonSaveSitePicture: TBitBtn;
    SiteCopyright: TPDJRotoLabel;
    SiteTimeOfCreation: TLabel;
    ListDeckSites: TListView;
    LabelListDeckSites: TLabel;
    ListDeckAllSites: TListView;
    LabelListDeckAllSites: TLabel;
    ButtonAddSiteToDeck: TButton;
    ButtonRemoveSiteFromDeck: TButton;
    SiteText: TUniHTMLabel;
    SaveImageDialog: TSaveDialog;
    ButtonSaveCardAs: TBitBtn;
    ButtonSaveDeckAs: TBitBtn;
    LabelCardListStats: TLabel;
    CardThumbnail: TImage;
    PreviewText: TUniHTMLabel;
    PreviewTitle: TLabel;
    PreviewSubtitle: TLabel;
    PreviewComment: TUniHTMLabel;
    OpenDatabaseDialog: TOpenDialog;
    OpenPictureDialog: TOpenPictureDialog;
    OpenPictureDialogButton: TSpeedButton;
    OpenSitePictureButton: TSpeedButton;
    OpenPictureURLDialogButton: TSpeedButton;
    OpenSitePictureURLDialogButton: TSpeedButton;
    DatabaseInfoPanel: TPanel;
    InfoDatabaseNameLabel: TLabel;
    InfoDatabaseCaption: TLabel;
    InfoDatabaseSizeCaption: TLabel;
    InfoDatabaseSizeLabel: TLabel;
    InfoDatabaseCreatedLabel: TLabel;
    InfoDatabaseCreatedCaption: TLabel;
    LOTWLogoPicture: TImage;
    CopyrightLabel: TLabel;
    LOTWInfoLabel: TMemo;
    LOTWCardspreadPicture: TImage;
    ButtonChooseDB: TBitBtn;
    EditInternalComment: TMemo;
    LabelEditHiddenComment: TLabel;
    EditSiteInternalComment: TMemo;
    LabelEditSiteComment: TLabel;
    PanelDeckFields: TPanel;
    LabelDeckShadowStats: TLabel;
    LabelDeckFellowshipStats: TLabel;
    LabelDeckStats: TLabel;
    DeckTimeOfCreation: TLabel;
    EditDeckComment: TMemo;
    EditDeckTitle: TEdit;
    LabelEditDeckTitle: TLabel;
    LabelEditDeckComment: TLabel;
    InfoDatabaseRacesLabel: TLabel;
    InfoDatabaseRacesCaption: TLabel;
    InfoDatabaseModifiedLabel: TLabel;
    InfoDatabaseModifiedCaption: TLabel;
    ButtonSaveCardImages: TBitBtn;
    ButtonSaveSiteImages: TBitBtn;
    EditTimeOfModification: TEdit;
    LabelEditTimeOfModification: TLabel;
    EditTimeOfCreation: TEdit;
    LabelEditTimeOfCreation: TLabel;
    InfoDatabaseCardsCaption: TLabel;
    InfoDatabaseCardsLabel: TLabel;
    LOTWCardsPicture: TImage;
    LOTWSitesPicture: TImage;
    InfoDatabaseSitesCaption: TLabel;
    InfoDatabaseSitesLabel: TLabel;
    LOTWDecksPicture: TImage;
    InfoDatabaseDecksCaption: TLabel;
    InfoDatabaseDecksLabel: TLabel;
    ButtonSavePicture: TBitBtn;
    ButtonSaveDeckImages: TBitBtn;
    ComboDeckList: TComboBox;
    BackgroundPicture: TImage;
    ContentPicture: TImage;
    TwilightCost: TLabel;
    Title: TLabel;
    Subtitle: TLabel;
    VerticalTitleLine1: TPDJRotoLabel;
    VerticalTitleLine2: TPDJRotoLabel;
    VerticalTitle: TPDJRotoLabel;
    StrengthPicture: TImage;
    HealthPicture: TImage;
    Strength: TLabel;
    Health: TLabel;
    Text: TUniHTMLabel;
    Copyright: TLabel;
    MiddleTitle: TLabel;
    StatusBar: TStatusBar;
    StatusTimer: TTimer;
    ProgressBar: TProgressBar;
    LabelStatus: TUniHTMLabel;
    procedure ButtonSavePictureClick(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure NumberEditKeyPress(Sender: TObject; var Key: Char);
    procedure ButtonNewCardClick(Sender: TObject);
    procedure ButtonSaveCardClick(Sender: TObject);
    procedure ComboCardListChange(Sender: TObject);
    procedure ButtonDeleteCardClick(Sender: TObject);
    procedure ButtonNewDeckClick(Sender: TObject);
    procedure ViewListDeckAllCardsCard(Sender: TObject);
    procedure ButtonDeleteDeckClick(Sender: TObject);
    procedure ButtonSaveDeckClick(Sender: TObject);
    procedure Addcardtodeck1Click(Sender: TObject);
    procedure Removecardfromdeck1Click(Sender: TObject);
    procedure ButtonAddCardToDeckClick(Sender: TObject);
    procedure ButtonRemoveCardFromDeckClick(Sender: TObject);
    procedure EditCardChange(Sender: TObject);
    procedure ComboRaceChange(Sender: TObject);
    procedure ComboCardTypeChange(Sender: TObject);
    procedure ButtonSaveCardAsClick(Sender: TObject);
    procedure ButtonUndoCardChangesClick(Sender: TObject);
    procedure EditDeckChange(Sender: TObject);
    procedure ButtonUndoDeckChangesClick(Sender: TObject);
    procedure ButtonSaveDeckAsClick(Sender: TObject);
    procedure AnyCardListColumnClick(Sender: TObject; Column: TListColumn);
    procedure AnyCardListCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ButtonApplyListFilterClick(Sender: TObject);
    procedure ButtonClearListFilterClick(Sender: TObject);
    procedure AnyCardListKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure AnyCardListDblClick(Sender: TObject);
    procedure ListDeckCardsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListDeckCardsDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure CardListMenuClick(Sender: TObject);
    procedure ComboSiteListChange(Sender: TObject);
    procedure ButtonNewSiteClick(Sender: TObject);
    procedure ButtonSaveSiteClick(Sender: TObject);
    procedure ButtonSaveSiteAsClick(Sender: TObject);
    procedure ButtonUndoSiteChangesClick(Sender: TObject);
    procedure ButtonDeleteSiteClick(Sender: TObject);
    procedure EditSiteChange(Sender: TObject);
    procedure ListDeckSitesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure AddSiteToDeck(Sender: TObject);
    procedure ListDeckSitesDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure ListDeckAllSitesDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure RemoveSiteFromDeck(Sender: TObject);
    procedure AnySiteListDblClick(Sender: TObject);
    procedure AnySiteListColumnClick(Sender: TObject;
      Column: TListColumn);
    procedure AnySiteListCompare(Sender: TObject; Item1,
      Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure ButtonSaveSitePictureClick(Sender: TObject);
    procedure ListCardsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure CardThumbnailClick(Sender: TObject);
    procedure ButtonChooseDBClick(Sender: TObject);
    procedure OpenPictureDialogButtonClick(Sender: TObject);
    procedure OpenSitePictureButtonClick(Sender: TObject);
    procedure OpenPictureURLDialogButtonClick(Sender: TObject);
    procedure OpenSitePictureURLDialogButtonClick(Sender: TObject);
    procedure LOTWCardsPictureClick(Sender: TObject);
    procedure LOTWSitesPictureClick(Sender: TObject);
    procedure LOTWDecksPictureClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonSaveCardImagesClick(Sender: TObject);
    procedure ButtonSaveSiteImagesClick(Sender: TObject);
    procedure ButtonSaveDeckImagesClick(Sender: TObject);
    procedure ComboDeckListChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
  private
    // Clears the edit components under the control, except those matching the tag
    procedure ClearFields(WinControl: TWinControl; TagNoClear: Integer);
    // Retrieves all the initial data from the database required for working
    procedure LoadData();
    // Checks whether the control's tag matches the specified tag.
    // Controls' tags are sums of powers of 2, and check tags
    // just powers of 2.
    function MatchesTag(var Control: TControl; Tag: Integer): Boolean;
    // Changes the interface for the type (shadow event, stud club possession etc)
    procedure ChangeCardEditorInterface(var Card: TCard);
    // Changes the visibility of the card editor interface components
    procedure ChangeCardEditorVisibility(Control: TWinControl; var Card: TCard);
    // Loads the specified card into the card editor
    procedure LoadCard(var Card: TCard);
    // Loads the specified site into the site editor
    procedure LoadSite(var Site: TSite);
    // Previews the card picture in the card editor
    procedure PreviewCard(var Card: TCard; UpdatePicture: Boolean = True);
    // Previews the site picture in the site editor
    procedure PreviewSite(var Site: TSite; UpdatePicture: Boolean = True);
    // Makes the specified item selected in the combobox, if it exists
    procedure SelectComboItem(ComboBox: TComboBox; Item: String);
    // Updates all the variables and controls to reflect a card change
    procedure CardsUpdate();
    // Updates all the variables and controls to reflect a deck change
    procedure DecksUpdate();
    // Updates all the variables and controls to reflect a site change
    procedure SitesUpdate();
    // Loads the deck in the deck editor
    procedure LoadDeck(Deck: TDeck);
    // Sets the interface to reflect whether the current card has been changed
    procedure SetCardChanged(IsChanged: Boolean);
    // Sets the interface to reflect whether the current site has been changed
    procedure SetSiteChanged(IsChanged: Boolean);
    // Sets the interface to reflect whether the current deck has been changed
    procedure SetDeckChanged(IsChanged: Boolean);
    // Fills in the deck statistics controls
    procedure ShowDeckStats();
    // Removes the currently active card from the deck's card list
    procedure RemoveCardFromDeckList();
    // Updates the ListDeckCards list of cards
    procedure UpdateDeckCardsList();
    // Adds the card to the deck and ListDeckCards
    procedure AddCardToDeck(Card: TCard);
    // Saves the current card, if any
    procedure SaveCurrentCard();
    // Saves the current site, if any
    procedure SaveCurrentSite();
    // Saves the current deck, if any
    procedure SaveCurrentDeck();
    // Sets the data in the card editor interface to the card object
    procedure SetInterfaceDataToCard(Card: TCard);
    // Sets the data in the site editor interface to the site object
    procedure SetInterfaceDataToSite(Site: TSite);
    // Sets the data in the card to the card editor interface
    procedure SetCardDataToInterface(Card: TCard);
    // Sets the data in the site to the site editor interface
    procedure SetSiteDataToInterface(Site: TSite);
    // Undos the changes made to the current deck
    procedure UndoCurrentDeckChanges();
    // Checks whether the current card, site and deck have changed,
    // and asks the user whether to save or not or to cancel the operation.
    // If the user chooses to save, saves the appropriate item.
    // Returns mrCancel if the user chose to cancel either operation.
    function GetCheckSavingResult(CheckCard, CheckSite, CheckDeck: Boolean): Integer;
    // Resets the deck editor interface, disabling controls etc
    procedure ResetDeckEditor();
    // Resets the card editor interface
    procedure ResetCardEditor(ClearComboList: Boolean = True);
    // Resets the site editor interface
    procedure ResetSiteEditor(ClearComboList: Boolean = True);
    // Checks whether the card has a unique title-subtitle combination
    function IsCardTitleUnique(Card: TCard): Boolean;
    // Checks whether the site has a unique title
    function IsSiteTitleUnique(Site: TSite): Boolean;
    // Checks whether the deck has a unique title
    function IsDeckTitleUnique(Deck: TDeck): Boolean;
    // Populates the card grid with card items
    procedure PopulateListCards();
    // Populates the ListDeckAllCards with card items
    procedure PopulateListDeckAllCards();
    // Populates the ListDeckCards with card items
    procedure PopulateListDeckCards();
    // Populates the ListDeckSites with sites
    procedure PopulateListDeckSites();
    // Populates the ListDeckAllSites with sites
    procedure PopulateListDeckAllSites();
    // Resets the card list page, disabling filters etc
    procedure ResetCardList();
    // Function passed to resizer threads as callback, will set the resized
    // picture to the appropriate item and UI attributes. Saves the card
    // thumbnail if card is in the database.
    procedure ResizeCallback(Resized: TBitmap; ResizeType: TResizeType; Item: TPlayable);
    // Loads a picture for the current card or site, either file file or URL
    procedure LoadItemPicture(Item: TPlayable; Location, LocationType: String);
    // Updates the database information (name, size, cards count) on first page
    procedure UpdateDatabaseInfo();
    // Loads the specified database in the application
    procedure LoadDatabase(DB: TPersistence);
    // Creates the new card dialog form
    procedure CreateNewCardDialog();
    // Takes a screenshot of the current card and queues it for resizing in a
    // background thread.
    procedure MakeCurrentCardThumbnail();
    // Batch saves the images of all specified cards and items and returns the saved count.
    function BatchSaveImages(Items: TList; Directory: String): Integer;
    // Sets a text to the specified status panel and schedules it for clearing
    // after timeout (0 - no clearing).
    procedure SetStatusText(Status: String; StatusPanel: Integer = 0; Timeout: Integer = 1500);
    // Saves all global cards, all global sites, or all current deck cards and sites,
    // depending on Target ('cards'/'sites'/'deck').
    procedure SaveSitesAndCards(Target: String);

  end;

  TListSortCompare = function (Item1, Item2: Pointer): Integer;

var
  MainForm: TMainForm;
  NewCardDialog: TForm;
  NewCardComboCardType: TComboBox;
  NewCardComboRace: TComboBox;


implementation

uses strutils, Math, Clipbrd, httpprot, SyncObjs, DCL_intf, HashMap, BrowseForFolderU;



{$R *.DFM}


procedure TMainForm.ButtonSavePictureClick(Sender: TObject);
var Filename: String;
    Bitmap: TBitmap;
begin
   SaveImageDialog.Filename := 'card ' + CurrentCard.GetDisplayName();
   if (SaveImageDialog.Execute()) then begin
     Application.ProcessMessages(); // Let dialog disappear
     Filename := SaveImageDialog.Filename;
     Bitmap := Imager.CaptureArea(CardImageArea);
     Imager.SavePictureToDisk(Bitmap, Filename);
     SetStatusText(Format('Saved image <a href="file://%s">%s</a>.', [StringReplace(Filename, '"', '%22', [rfReplaceAll]), ExtractFileName(Filename)]), 0, 0);
   end;
end;




procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
  Close();
end;


procedure TMainForm.FormCreate(Sender: TObject);
var
  TopLeft: TPoint;
begin
  MainForm.DoubleBuffered := True;
  PageControl.ActivePage := PageDatabase;
  PageDeckEditor.TabVisible := False;
  PageCardEditor.TabVisible := False;
  PageSiteEditor.TabVisible := False;
  PageCardList.TabVisible := False;
  DatabaseInfoPanel.Visible := False;
  Imager := TImaging.Create();
  CardLoadSection := TCriticalSection.Create();
  ListCardsSortInfo := TSortInfo.Create();
  ListDecksSortInfo := TSortInfo.Create();
  ListDeckAllCardsSortInfo := TSortInfo.Create();
  ListDeckCardsSortInfo := TSortInfo.Create();
  ListDeckSitesSortInfo := TSortInfo.Create();
  ListDeckAllSitesSortInfo := TSortInfo.Create();
  LOTWLogoPicture.Picture.Assign(Imager.GetResource('LOTW_Title', '.png'));
  LOTWCardspreadPicture.Picture.Assign(Imager.GetResource('LOTW_Cardspread', '.png'));
  LOTWCardsPicture.Picture.Assign(Imager.GetResource('LOTW_Cards', '.png'));
  LOTWSitesPicture.Picture.Assign(Imager.GetResource('LOTW_Sites', '.png'));
  LOTWDecksPicture.Picture.Assign(Imager.GetResource('LOTW_Decks', '.png'));
  SiteBackgroundPicture.Picture.Assign(Imager.GetResource('Site_Background', '.png'));
  SiteTwilightCostPicture.Picture.Assign(Imager.GetResource('Site_Twilight_Cost', '.png'));
  TransparentPixel := Imager.GetResource('Transparent_Pixel', '.png', False);
  CopyrightLabel.Caption := Format(COPYRIGHT_TEXT_TEMPLATE, [GetApplicationVersion()]);
  CreateNewCardDialog();
  TopLeft := BackgroundPicture.ClientToParent(Point(0, 0), MainForm);
  CardImageArea := Bounds(TopLeft.X, TopLeft.Y,
                          BackgroundPicture.Width, BackgroundPicture.Height);
  TopLeft := SiteBackgroundPicture.ClientToParent(Point(0, 0), MainForm);
  SiteImageArea := Bounds(TopLeft.X, TopLeft.Y,
                          SiteBackgroundPicture.Width, SiteBackgroundPicture.Height);
  // Insert status components into status bar
  ProgressBar.Parent := StatusBar;
  LabelStatus.Parent := StatusBar;
  StatusBar.Panels[0].Style := psOwnerDraw;
end;


procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  I, J: Integer;
  DataLists: TList;
begin
  if (GetCheckSavingResult(True, True, True) = mrCancel) then
    Action := caNone
  else begin
    DataLists := TList.Create();
    DataLists.Add(Decks); DataLists.Add(Cards); DataLists.Add(Sites);
    DataLists.Add(Races); DataLists.Add(CardTypes);
    for I := 0 to DataLists.Count - 1 do begin
      if DataLists.Items[I] <> nil then begin
        for J := 0 to TList(DataLists.Items[I]).Count - 1 do begin
          TDataClass(TList(DataLists.Items[I]).Items[J]).Free();
        end;
        TList(DataLists.Items[I]).Clear();
      end;
    end;
    DataLists.Free();

    FreeAndNil(Database);
    FreeAndNil(Decks);
    FreeAndNil(Cards);
    FreeAndNil(Sites);
    FreeAndNil(Races);
    FreeAndNil(CardTypes);
    FreeAndNil(Times);
    Settings := nil; // Must not be freed
    ComboCardListMap := nil; // Must not be freed
    ComboListDecksMap := nil; // Must not be freed
    ComboSiteListMap := nil; // Must not be freed
  end;
end;


// Checks whether the current card, site and deck have changed,
// and asks the user whether to save or not or to cancel the operation.
// If the user chooses to save, saves the appropriate item.
// Returns mrCancel if the user chose to cancel either operation.
function TMainForm.GetCheckSavingResult(CheckCard, CheckSite, CheckDeck: Boolean): Integer;
var CardResult, DeckResult, SiteResult: Integer;
begin
  CardResult := mrYes;
  DeckResult := mrYes;
  SiteResult := mrYes;
  if (CheckCard and IsCardChanged) then begin
    CardResult := MessageDlg(Format('Do you want to save the changes you made to the card "%s"?',
                                    [EditTitle.Text]), mtWarning, [mbYes, mbNo, mbCancel], 0);
    if (CardResult = mrYes) then begin
      SetInterfaceDataToCard(CurrentCard);
      SaveCurrentCard();
    end;
  end;
  if (CardResult <> mrCancel) then
    if (CheckSite and IsSiteChanged) then begin
      SiteResult := MessageDlg(Format('Do you want to save the changes you made to the site "%s"?',
                                      [EditSiteTitle.Text]), mtWarning, [mbYes, mbNo, mbCancel], 0);
      if (SiteResult = mrYes) then begin
        SetInterfaceDataToSite(CurrentSite);
        SaveCurrentSite();
      end;
    end;
  if (CardResult <> mrCancel) and (SiteResult <> mrCancel) then
    if (CheckDeck and IsDeckChanged) then begin
      DeckResult := MessageDlg(Format('Do you want to save the changes you made to the deck "%s"?',
                                      [EditDeckTitle.Text]), mtWarning, [mbYes, mbNo, mbCancel], 0);
      if (DeckResult = mrYes) then begin
        SaveCurrentDeck()
      end else if (DeckResult = mrNo) then begin
        UndoCurrentDeckChanges();
      end;
    end;
  if ((CardResult = mrCancel) or (DeckResult = mrCancel) or (SiteResult = mrCancel)) then
    Result := mrCancel
  else
    Result := mrIgnore;
end;


procedure TMainForm.NumberEditKeyPress(Sender: TObject; var Key: Char);
begin
  if (Sender is TEdit) then begin
    // #8 is backspace
    if Key in [#8, '0'..'9'] then Exit;
    if (Key = '-') and (TEdit(Sender).SelStart = 0)
       and (pos('-', TEdit(Sender).Text) = 0) then Exit;
    Key := #0;
  end;
end;


// Loads the specified database in the application
procedure TMainForm.LoadDatabase(DB: TPersistence);
var
  MainFormEnabled: Boolean;
begin
  MainFormEnabled := MainForm.Enabled;
  try
    Database := DB;
    DBFilename := DB.Filename;
    MainForm.Enabled := False;
    SetStatusText(Format('Loading database "%s"..', [ExtractFileName(DBFilename)]));
    DatabaseInfoPanel.Visible := True;
    UpdateDatabaseInfo();
    ResetCardEditor();
    ResetDeckEditor();
    ResetSiteEditor();
    ResetCardList();
    LoadData();
    if Database.IsNew then
      SetStatusText(Format('Created new database with %d races.', [Races.Count]), 0, 0)
    else
      SetStatusText(Format('Database contents: %d race%s, %d card%s, %d site%s, %d deck%s.',
        [Races.Count, Plural(Races.Count), Cards.Count, Plural(Cards.Count),
         Sites.Count, Plural(Sites.Count), Decks.Count, Plural(Decks.Count)]), 0, 0);
    PageDeckEditor.TabVisible := True;
    PageCardEditor.TabVisible := True;
    PageCardList.TabVisible := True;
    PageSiteEditor.TabVisible := True;

    PreviewTitle.Caption := '';
    PreviewSubtitle.Caption := '';
    PreviewText.HTMLText := '';
    PreviewComment.HTMLText := '';
  finally
    MainForm.Enabled := MainFormEnabled;
  end;
end;


procedure TMainForm.ButtonNewCardClick(Sender: TObject);
var Choice: Integer;
begin
  NewCardDialog.Show();
  if NewCardComboRace.ItemIndex < 0 then NewCardComboRace.ItemIndex := 0;
  if NewCardComboCardType.ItemIndex < 0 then NewCardComboCardType.ItemIndex := 0;
  NewCardComboRace.SetFocus(); // Reset focussing to first
  NewCardDialog.Hide();
  Choice := NewCardDialog.ShowModal();
  if (Choice = mrOk) then begin
    if (GetCheckSavingResult(True, False, False) <> mrCancel) then begin
      CurrentCard := TCard.Create();
      CurrentCard.CardType := GetCardTypeByName(NewCardComboCardType.Items[NewCardComboCardType.ItemIndex]);
      CurrentCard.Race := GetRaceByName(NewCardComboRace.Items[NewCardComboRace.ItemIndex]);
      ChangeCardEditorInterface(CurrentCard);
      PanelCardPicture.Visible := False;
      SetCardDataToInterface(CurrentCard);
      PreviewCard(CurrentCard);
      PanelCardPicture.Visible := True;
      SetCardChanged(False);
      ButtonSaveCard.Enabled := True;
      ButtonDeleteCard.Enabled := False;
      EditTitle.SetFocus();
    end;
  end;
end;


// Clears the edit components under the control, except those matching the tag
procedure TMainForm.ClearFields(WinControl: TWinControl; TagNoClear: Integer);
var I: Integer;
    CurrentControl: TControl;
begin
  for I := 0 to WinControl.ControlCount - 1 do begin
    CurrentControl := WinControl.Controls[I];
    // If the control is a TPanel, recurse into its controls
    if (CurrentControl is TPanel) then begin
      ClearFields(CurrentControl as TWinControl, TagNoClear);
    end else if not (MatchesTag(CurrentControl, TagNoClear)) then begin
      if (CurrentControl is TEdit) then begin
        (CurrentControl as TEdit).Text := ''
      end else if (CurrentControl is TMemo) then begin
        (CurrentControl as TMemo).Lines.Text := ''
      end else if ((CurrentControl is TComboBox) and (CurrentControl.Tag <> 0)) then begin
        (CurrentControl as TComboBox).ItemIndex := -1;
      end else if (CurrentControl is TCheckBox) then begin
        (CurrentControl as TCheckBox).Checked := False;
      end;
    end;
  end;
end;


procedure TMainForm.ButtonSaveCardClick(Sender: TObject);
var
  TempCard: TCard;
  MissingFields: String;
begin
  if (CurrentCard <> nil) then begin
    TempCard := TCard.Create();
    TempCard.ID := CurrentCard.ID;
    SetInterfaceDataToCard(TempCard);
    if (IsCardTitleUnique(TempCard)) then begin
      MissingFields := '';
      if (Length(EditTitle.Text) > 0) then MissingFields := ' a title';
      if (Length(EditTwilightCost.Text) = 0) then MissingFields := MissingFields + IfThen(Length(MissingFields) > 0, ' and ', '') + ' a twilight cost';
      if ((Length(EditTitle.Text) > 0) and (Length(EditTwilightCost.Text) > 0)) then begin
        SetInterfaceDataToCard(CurrentCard);
        MakeCurrentCardThumbnail(); // Thumbnail will be saved after generation
        SaveCurrentCard();
      end else begin
        MessageDlg('Every card must have' + MissingFields + '.', mtError, [mbOk], 0);
      end;
    end else begin
      MessageDlg('The card''s title (and together with subtitle, if character) is not unique. ' + NEW_LINE +
                  'Change either and try again.', mtError, [mbOk], 0);
    end;
    TempCard.Free();
  end;
end;


// Saves the current card, if any
procedure TMainForm.SaveCurrentCard();
var I: Integer;
    TempCard: TCard;
begin
  if (CurrentCard.AreRequiredFieldsEmpty()) then begin
    MessageDlg(CurrentCard.REQUIRED_FIELDS_EMPTY_MESSAGE, mtError, [mbOk], 0);
  end else begin
    CurrentCard.TimeOfModification := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now());
    if (CurrentCard.TimeOfCreation = '') then begin
      CurrentCard.ExactTimeOfCreation := CurrentCard.TimeOfModification;
      CurrentCard.TimeOfCreation := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now());
    end;
    Database.Store(CurrentCard);
    CurrentCard.ShowName := '';
    CurrentCard.OriginalRace := nil;
    CurrentCard.OriginalCardType := nil;
    FreeAndNil(CurrentCard.OriginalContentPicture);
    CurrentCard.IsContentPictureChanged := False;
    AddCardToVariables(CurrentCard);
    CardsUpdate();
    for I := 0 to ComboCardList.Items.Count - 1 do begin
      TempCard := ComboCardListMap.GetValue(IntToStr(I)) as TCard;
      if (TempCard = CurrentCard) then begin
        ComboCardList.ItemIndex := I;
        Break;
      end;
    end;
    SetCardChanged(False);
    ButtonDeleteCard.Enabled := True;
  end;
end;


// Saves the current site, if any
procedure TMainForm.SaveCurrentSite();
var I: Integer;
    TempSite: TSite;
    IsNewSite: Boolean;
begin
  if (CurrentSite.Title = '') then begin
    MessageDlg('Every site must have a title.', mtError, [mbOk], 0);
  end else begin
    IsNewSite := (CurrentSite.ID = 0);
    Database.Store(CurrentSite);
    if CurrentSite.OriginalContentPicture <> nil then
      FreeAndNil(CurrentSite.OriginalContentPicture);
    CurrentSite.IsContentPictureChanged := False;
    if (IsNewSite) then
      Sites.Add(CurrentSite);
    SitesUpdate();
    for I := 0 to ComboSiteList.Items.Count - 1 do begin
      TempSite := ComboSiteListMap.GetValue(IntToStr(I)) as TSite;
      if (TempSite = CurrentSite) then begin
        ComboSiteList.ItemIndex := I;
        Break;
      end;
    end;
    SetSiteChanged(False);
    ButtonDeleteSite.Enabled := True;
  end;
end;


// Sets the data in the card editor interface to the card object
procedure TMainForm.SetInterfaceDataToCard(Card: TCard);
begin
  if (Length(EditTwilightCost.Text) = 0) then
    raise Exception.Create('Error: the twilight cost must be a valid number.');
  Card.CardType := GetCardTypeByName(ComboCardType.Items[ComboCardType.ItemIndex]);
  Card.Race := GetRaceByName(ComboRace.Items[ComboRace.ItemIndex]);
  Card.Title := EditTitle.Text;
  Card.Subtitle := '';
  Card.Time := '';
  Card.PossessionType := '';
  Card.Strength := '';
  Card.Health := '';
  if (Card.CardType.IsCharacter) then begin
    Card.Subtitle := EditSubtitle.Text;
    if (not Card.Race.IsGood) then
      Card.Time := ComboTime.Text;
  end;
  if (Card.CardType.IsPossession) then
    Card.PossessionType := EditPossessionType.Text;
  Card.TwilightCost := StrToInt(EditTwilightCost.Text);
  Card.IsUnique := CheckIsUnique.Checked;
  if ((Card.CardType.IsCharacter) or (Card.CardType.IsPossession)) then begin
    Card.Strength := EditStrength.Text;
    Card.Health := EditHealth.Text;
  end;
  Card.Text := EditText.Text;
  Card.Comment := EditComment.Text;
  Card.PictureFilename := EditPictureFilename.Text;
  Card.Title := EditTitle.Text;
  Card.InternalComment := EditInternalComment.Text;
  Card.ShowName := '';
end;



// Sets the data in the site editor interface to the site object
procedure TMainForm.SetInterfaceDataToSite(Site: TSite);
begin
  Site.Title := EditSiteTitle.Text;
  Site.PictureFilename := EditSitePictureFilename.Text;
  Site.Time := ComboSiteTimes.Text;
  Site.TwilightCost := StrToInt(EditSiteTwilightCost.Text);
  Site.Text := EditSiteText.Text;
  Site.Direction := ComboSiteDirections.Text;
  Site.Comment := EditSiteInternalComment.Text;
  Site.ShowName := '';
end;



// Sets the data in the card to the card editor interface
procedure TMainForm.SetCardDataToInterface(Card: TCard);
begin
  ClearFields(PageCardEditor, TAG_NOCLEAR);
  SelectComboItem(ComboRace, Card.Race.Name);
  SelectComboItem(ComboCardType, Card.CardType.Name);
  ChangeCardEditorInterface(Card);
  EditTitle.Text := Card.Title;
  EditSubtitle.Text := Card.Subtitle;
  EditPossessionType.Text := Card.PossessionType;
  EditTwilightCost.Text := IntToStr(Card.TwilightCost);
  CheckIsUnique.Checked := Card.IsUnique;
  EditStrength.Text := Card.Strength;
  EditHealth.Text := Card.Health;
  if (Length(Card.Time) > 0) then
    SelectComboItem(ComboTime, Card.Time);
  EditText.Text := Card.Text;
  EditComment.Text := Card.Comment;
  EditPictureFilename.Text := Card.PictureFilename;
  EditTitle.Text := Card.Title;
  EditInternalComment.Text := Card.InternalComment;
  EditTimeOfCreation.Text := Card.TimeOfCreation;
  EditTimeOfModification.Text := Card.TimeOfModification;
  PanelCardFields.Visible := True;
  ButtonDeleteCard.Enabled := True;
  ButtonSaveCardAs.Enabled := True;
  ButtonSavePicture.Enabled := True;
end;



// Sets the data in the site to the site editor interface
procedure TMainForm.SetSiteDataToInterface(Site: TSite);
begin
  ClearFields(PageSiteEditor, TAG_NOCLEAR);
  EditSiteTitle.Text := Site.Title;
  EditSiteTwilightCost.Text := IntToStr(Site.TwilightCost);
  if (Length(Site.Time) > 0) then
    SelectComboItem(ComboSiteTimes, Site.Time)
  else
    SelectComboItem(ComboSiteTimes, Times[0]);
  if (Length(Site.Direction) > 0) then
    SelectComboItem(ComboSiteDirections, Site.Direction)
  else
    ComboSiteDirections.ItemIndex := 0;
  EditSiteText.Text := Site.Text;
  EditSitePictureFilename.Text:= Site.PictureFilename;
  if (Length(Site.TimeOfCreation) > 0) then
    SiteTimeOfCreation.Caption := 'Created ' + Site.TimeOfCreation
  else
    SiteTimeOfCreation.Caption := '';
  EditSiteInternalComment.Text := Site.Comment;    
  PanelSiteFields.Visible := True;
  ButtonDeleteSite.Enabled := True;
  ButtonSaveSiteAs.Enabled := True;
  ButtonSaveSitePicture.Enabled := True;
end;



// Retrieves all the initial data from the database required for working
procedure TMainForm.LoadData();
var I: Integer;
    Race: TRace;
    CardType: TCardType;
begin
  CurrentDeck := nil;
  CurrentCard := nil;
  ComboCardListMap := TStrHashMap.Create(16, False);  // Set to not own its
  ComboListDecksMap := TStrHashMap.Create(16, False); // objects, as they are
  ComboSiteListMap := TStrHashMap.Create(16, False);  // managed elsewhere
  UpdateDatabaseInfo();
  Times := Database.RetrieveTimes();
  Races := Database.RetrieveRaces();
  UpdateDatabaseInfo();
  Sites := Database.RetrieveSites();
  UpdateDatabaseInfo();
  Settings := Database.RetrieveSettings();
  UpdateDatabaseInfo();
  CardTypes := Database.RetrieveCardTypes();

  ComboRace.Items.Clear();
  NewCardComboRace.Items.Clear();
  ComboListRaces.Items.Clear();
  ComboListRaces.Items.Add(CARD_LIST_FILTER_RACES_ALL);
  ComboListRaces.Items.Add(CARD_LIST_FILTER_RACES_FELLOWSHIP);
  ComboListRaces.Items.Add(CARD_LIST_FILTER_RACES_SHADOW);
  for I := 0 to Races.Count - 1 do begin
    Race := Races.Items[I];
    ComboRace.Items.Add(Race.Name);
    NewCardComboRace.Items.Add(Race.Name);
    ComboListRaces.Items.Add(Race.Name);
  end;
  ComboListRaces.ItemIndex := 0;
  ComboTime.Items.Clear();
  ComboSiteTimes.Items.Clear();
  ComboListTimes.Items.Clear();
  ComboListTimes.Items.Add(CARD_LIST_FILTER_TIMES_ALL);
  ComboListTimes.ItemIndex := 0;
  for I := 0 to Times.Count - 1 do begin
    ComboTime.Items.Add(Times[I]);
    ComboSiteTimes.Items.Add(Times[I]);
    ComboListTimes.Items.Add(Times[I]);
  end;
  ComboCardType.Items.Clear();
  NewCardComboCardType.Items.Clear;
  ComboListCardTypes.Items.Clear();
  ComboListCardTypes.Items.Add(CARD_LIST_FILTER_TYPES_ALL);
  ComboListCardTypes.ItemIndex := 0;
  for I := 0 to CardTypes.Count - 1 do begin
    CardType := CardTypes[I];
    ComboCardType.Items.Add(CardType.Name);
    NewCardComboCardType.Items.Add(CardType.Name);
    ComboListCardTypes.Items.Add(CardType.Name);
  end;
  Cards := Database.RetrieveCards();
  UpdateDatabaseInfo();
  Decks := Database.RetrieveDecks();
  CardsUpdate();
  DecksUpdate();
  SitesUpdate();
end;


procedure TMainForm.CardsUpdate();
var I: Integer;
    Card: TCard;
begin
  Cards.Sort(CardListSortCompare);
  ComboCardList.Items.Clear;
  ComboCardListMap.Clear();
  for I := 0 to Cards.Count - 1 do begin
    Card := Cards.Items[I];
    ComboCardList.Items.Add(Card.GetDisplayName());
    ComboCardListMap.PutValue(IntToStr(I), Card);
  end;
  PopulateListCards();
  PopulateListDeckAllCards();
  PopulateListDeckCards();
  UpdateDatabaseInfo();
end;


// Updates all the variables and controls to reflect a deck change
procedure TMainForm.DecksUpdate();
var I, Index: Integer;
    Deck: TDeck;
begin
  ComboListDecks.Items.Clear();
  ComboDeckList.Items.Clear();
  ComboListDecksMap.Clear();
  Index := -1;
  ComboListDecks.Items.Add(' ');
  for I := 0 to Decks.Count - 1 do begin
    Deck := Decks.Items[I];
    ComboListDecks.Items.Add(Deck.GetDisplayName());
    ComboDeckList.Items.Add(Deck.GetDisplayName());
    ComboListDecksMap.PutValue(IntToStr(I), Deck);
    if (Deck = CurrentDeck) then
      Index := I;
  end;
  if (CurrentDeck = nil) then begin
    ListDeckCards.Items.Clear();
    ListDeckSites.Items.Clear();
  end else begin
    ComboDeckList.ItemIndex := Index;
    DeckTimeOfCreation.Caption := 'Created ' + CurrentDeck.TimeOfCreation;
    ShowDeckStats();
  end;
  UpdateDatabaseInfo();
end;


// Updates all the variables and controls to reflect a site change
procedure TMainForm.SitesUpdate();
var I: Integer;
    Site: TSite;
begin
  Sites.Sort(SiteListSortCompare);
  ComboSiteList.Items.Clear;
  ComboSiteListMap.Clear();
  for I := 0 to Sites.Count - 1 do begin
    Site := Sites.Items[I];
    ComboSiteList.Items.Add(Site.GetDisplayName());
    ComboSiteListMap.PutValue(IntToStr(I), Site);
  end;
  PopulateListDeckAllSites();
  PopulateListDeckSites();
  UpdateDatabaseInfo();
end;



// Checks whether the control's tag matches the specified tag.
// Controls' tags are sums of powers of 2, and check tags
// just powers of 2.
function TMainForm.MatchesTag(var Control: TControl; Tag: Integer): Boolean;
begin
  // if one power of 2 is contained in the other, xor returns
  // the difference, otherwise the sum of the operands
  // Example: control tag is 16+4, tag is 4, then xor returns 16.
  // Example: control tag is 16+2, tag is 4, then xor returns 22.
  if (Tag xor Control.Tag = Abs(Control.Tag - Tag)) then
    Result := True
  else
    Result := False;
end;


// Changes the interface for the type (shadow event, fellowship possession etc)
procedure TMainForm.ChangeCardEditorInterface(var Card: TCard);
begin
  StrengthPicture.Visible := False;
  HealthPicture.Visible := False;
  ContentPicture.Left := Card.CardType.ContentPictureLeft;
  ContentPicture.Top := Card.CardType.ContentPictureTop;
  MiddleTitle.Left := Card.CardType.MiddleTitleLeft;
  MiddleTitle.Width := Card.CardType.MiddleTitleWidth;
  if (Card.CardType.IsCharacter) then begin
    BackgroundPicture.Picture.Assign(Card.Race.CharacterPicture);
  end else begin
    BackgroundPicture.Picture.Assign(Card.Race.OtherPicture);
  end;
  if (Card.CardType.IsStrength) then begin
    StrengthPicture.Picture.Assign(Card.Race.StrengthPicture);
    StrengthPicture.Left := Card.Race.StrengthPictureLeft;
    StrengthPicture.Top := Card.Race.StrengthPictureTop;
  end;
  if (Card.CardType.IsHealth) then begin
    HealthPicture.Picture.Assign(Card.Race.HealthPicture);
    HealthPicture.Left := Card.Race.HealthPictureLeft;
    HealthPicture.Top := Card.Race.HealthPictureTop;
  end;
  ChangeCardEditorVisibility(PageCardEditor, Card);
end;


// Changes the visibility of the card editor interface components
procedure TMainForm.ChangeCardEditorVisibility(Control: TWinControl; var Card: TCard);
begin
  LabelEditTime.Visible := (not Card.Race.IsGood) and (Card.CardType.IsCharacter);
  LabelEditTime.FocusControl.Visible := LabelEditTime.Visible;
  LabelEditStrength.Visible := Card.CardType.IsStrength;
  LabelEditStrength.FocusControl.Visible := LabelEditStrength.Visible;
  LabelEditHealth.Visible := Card.CardType.IsHealth;
  LabelEditHealth.FocusControl.Visible := LabelEditHealth.Visible;
  LabelEditSubtitle.Visible := Card.CardType.IsCharacter;
  LabelEditSubtitle.FocusControl.Visible := LabelEditSubtitle.Visible;
  LabelEditPossessionType.Visible := Card.CardType.IsPossession;
  LabelEditPossessionType.FocusControl.Visible := LabelEditPossessionType.Visible;
end;



// Loads the specified card into the card editor
procedure TMainForm.LoadCard(var Card: TCard);
var I: Integer;
    TempCard: TCard;
begin
  PanelCardPicture.Visible := False;
  try
    IgnoreCardChanges := True;
    SetCardDataToInterface(Card);
    PreviewCard(Card);
    for I := 0 to ComboCardList.Items.Count - 1 do begin
      TempCard := ComboCardListMap.GetValue(IntToStr(I)) as TCard;
      if (TempCard = CurrentCard) then begin
        ComboCardList.ItemIndex := I;
        Break;
      end;
    end;
  finally
    PanelCardPicture.Visible := True;
    IgnoreCardChanges := False;
    SetCardChanged(False);
  end;
end;


// Previews the card picture in the card editor
procedure TMainForm.PreviewCard(var Card: TCard; UpdatePicture: Boolean = True);
var Temp, TimeTemp, TitleTemp, Inter, NextWord: String;
    Words: TStringList;
    I: Integer;
begin
  // Always compare before replacing values, to avoid flickering
  try
    TwilightCost.Caption := EditTwilightCost.Text;
    if ((Card.CardType.IsStrength)) then begin
      if (Strength.Caption <> EditStrength.Text) then Strength.Caption := EditStrength.Text;
    end else begin
      Strength.Caption := '';
    end;
    if ((Card.CardType.IsHealth)) then begin
      if (Health.Caption <> EditHealth.Text) then Health.Caption := EditHealth.Text;
    end else begin
      Health.Caption := '';
    end;
    TimeTemp := '';
    Temp := AnsiUpperCase(EditTitle.Text);
    if (CheckIsUnique.Checked) then Temp := UNIQUE_SYMBOL + ' ' + Temp;
    if (Card.CardType.IsCharacter) then begin
      VerticalTitle.Visible := False;
      VerticalTitleLine1.Visible := False;
      VerticalTitleLine2.Visible := False;
      Title.Caption := Temp;
      Title.Visible := True;
      if (not Card.Race.IsGood) then
        if (ComboTime.ItemIndex > -1) then
          TimeTemp := ComboTime.Items[ComboTime.ItemIndex];
    end else begin
      Title.Visible := False;
      // Longer texts do not fit into the label, and it doesn't wrap
      // automatically: hack it into two labels
      if (Length(Temp) > 22) then
        begin
        VerticalTitle.Visible := False;
        Words := SplitTextIntoWords(Temp);
        if (Words.Count > 0) then
          begin
          TitleTemp := '';
          Inter := '';
          NextWord := '';
          I := 0;
          while ((Length(TitleTemp + NextWord) < 22) and (I < Words.Count)) do
            begin
            TitleTemp := TitleTemp + Inter + Words.Strings[I];
            Inter := ' ';
            Inc(I);
            if (I < Words.Count) then NextWord := Words.Strings[I] else NextWord := '';
            end;
          if VerticalTitleLine1.Caption <> TitleTemp then VerticalTitleLine1.Caption := TitleTemp;
          VerticalTitleLine1.Visible := True;
          TitleTemp := '';
          Inter := '';
          while ((Length(TitleTemp + NextWord) < 22) and (I < Words.Count)) do
            begin
            TitleTemp := TitleTemp + Inter + Words.Strings[I];
            Inter := ' ';
            Inc(I);
            if (I < Words.Count) then NextWord := Words.Strings[I] else NextWord := '';
            end;
          if VerticalTitleLine2.Caption <> TitleTemp then VerticalTitleLine2.Caption := TitleTemp;
          VerticalTitleLine2.Visible := True;
          end;
        Words.Free();
        end
      else
        begin
        VerticalTitle.Visible := True;
        VerticalTitle.Caption := Temp;
        VerticalTitleLine1.Visible := False;
        VerticalTitleLine2.Visible := False;
        end;
    end;
    if Time.Caption <> TimeTemp then Time.Caption := TimeTemp;
    Temp := '';
    if (Card.CardType.IsCharacter) then begin
      if (Subtitle.Caption <> AnsiUpperCase(EditSubtitle.Text)) then Subtitle.Caption := AnsiUpperCase(EditSubtitle.Text);
      Temp := AnsiUpperCase(Card.Race.CharacterName) + ' ' +
              MIDDLE_DOT + ' ' + AnsiUpperCase(Card.Race.Name);
    end else if (Card.CardType.IsPossession) then begin
      Subtitle.Caption := '';
      Temp := AnsiUpperCase(Card.CardType.Name);
      if (Length(EditPossessionType.Text) > 0) then
        Temp := Temp + ' ' + MIDDLE_DOT + ' ' +
                AnsiUpperCase(EditPossessionType.Text);

      StrengthPicture.Visible := (Length(EditStrength.Text) > 0);
      HealthPicture.Visible := (Length(EditHealth.Text) > 0);
    end else begin
      Subtitle.Caption := '';
      Temp := AnsiUpperCase(Card.CardType.Name);
    end;
    if (MiddleTitle.Caption <> Temp) then MiddleTitle.Caption := Temp;

    Temp := EditText.Lines.Text;
    if (Trim(EditComment.Lines.Text) <> '') then begin
      // Leave a short empty line between text and comment
      if Temp <> '' then Temp := Temp + '<font size="6"><br><br></font>';
      Temp := Temp + '<font size="9"><i>' + EditComment.Lines.Text + '</i></font>';
    end;
    if (Text.HTMLText <> Temp) then Text.HTMLText := Temp;
    if (UpdatePicture) then begin
      ContentPicture.Picture.Assign(TransparentPixel);
      if Card.ContentPicture <> nil then begin
        ContentPicture.Picture.Assign(Card.ContentPicture);
      end else if (Length(Card.PictureFilename) > 0) then begin
        Card.ContentPicture := Database.RetrieveContentPicture(Card);
        ContentPicture.Picture.Assign(Card.ContentPicture);
      end;
    end;
  finally
    PanelCardPicture.Visible := True;
    Application.ProcessMessages(); // Allow UI to repaint
  end;
  if (UpdatePicture and (IsCardChanged or (Card.Thumbnail = nil))) then begin
    MakeCurrentCardThumbnail();
  end;
end;


// Takes a screenshot of the current card and queues it for resizing in a
// background thread.
procedure TMainForm.MakeCurrentCardThumbnail();
var Screenshot: TBitmap;
begin
  Screenshot := Imager.CaptureArea(CardImageArea);
  if CurrentCard.Thumbnail = nil then CurrentCard.Thumbnail := TPicture.Create();
  Imager.QueueResize(Screenshot, GetSettingInt('CardThumbnailWidth'),
                     GetSettingInt('CardThumbnailHeight'), CurrentCard, rsThumbnail,
                     ResizeCallback);
  Screenshot.Free();
end;



// Previews the site picture in the site editor
procedure TMainForm.PreviewSite(var Site: TSite; UpdatePicture: Boolean = True);
var Temp: String;
begin
//  PanelSitePicture.Visible := False;
  try

    if UpdatePicture then begin
      if Site.ContentPicture <> nil then begin
        SiteContentPicture.Picture.Assign(Site.ContentPicture);
      end else if (Length(Site.PictureFilename) > 0) then begin
        Site.ContentPicture := Database.RetrieveContentPicture(Site);
        SiteContentPicture.Picture.Assign(Site.ContentPicture);
      end else begin
        SiteContentPicture.Picture.Assign(TransparentPixel);
      end;
    end;
    SiteTitle.Caption := AnsiUpperCase(EditSiteTitle.Text);
    SiteTime.Caption := ComboSiteTimes.Text;
    if (SiteTime.Caption = Times[0]) then begin
      SiteTwilightCostPicture.Visible := False;
      SiteTwilightCost.Caption := '';
    end else begin
      SiteTwilightCostPicture.Visible := True;
      SiteTwilightCost.Caption := EditSiteTwilightCost.Text;
    end;
    if (ComboSiteDirections.Text = 'left') then begin
      SiteDirectionPicture.Picture.Assign(Imager.GetResource('Site_Direction_Left', '.png'));
    end else begin
      SiteDirectionPicture.Picture.Assign(Imager.GetResource('Site_Direction_Right', '.png'));
    end;
    Temp := StringReplace(EditSiteText.Lines.Text, NEW_LINE, '<br>', [rfReplaceAll]);
    SiteText.HTMLText := Temp;
  finally
    PanelSitePicture.Visible := True;
    Application.ProcessMessages();
  end;
end;



// Function passed to resizer threads as callback, will set the resized
// picture to the appropriate item and UI attributes. Saves the card
// thumbnail if card is in the database.
procedure TMainForm.ResizeCallback(Resized: TBitmap; ResizeType: TResizeType; Item: TPlayable);
var
  Card: TCard;
begin
  case ResizeType of
    rsThumbnail: begin
      if (Item is TCard) then begin
        Card := Item as TCard;
        Card.Thumbnail.Assign(Resized);
        if (PreviewedCard = Card) then CardThumbnail.Picture.Assign(Card.Thumbnail);
        Database.StoreCardThumbnail(Card);
      end else begin
        raise Exception('Unsupported data class for thumbnail resize: "' + Item.ClassName + '".');
      end;
    end;
    rsContent: begin
      Item.ContentPicture.Assign(Resized);
      if Item is TSite then
        SiteContentPicture.Picture.Assign(Item.ContentPicture)
      else
        ContentPicture.Picture.Assign(Item.ContentPicture);
      SetStatusText(Format('Picture "%s" resized from %dx%d to %dx%d.',
        [IfThen(Item is TSite, EditSitePictureFilename.Text, EditPictureFilename.Text),
         Item.PictureFileWidth, Item.PictureFileHeight,
         Item.ContentPicture.Width, Item.ContentPicture.Height]), 0, 0);
    end;
  end;
end;


// Loads the specified site into the site editor
procedure TMainForm.LoadSite(var Site: TSite);
var I: Integer;
    TempSite: TSite;
begin
  try
    IgnoreSiteChanges := True;
    PanelSitePicture.Visible := False;
    SetSiteDataToInterface(Site);
    PreviewSite(Site);
    for I := 0 to ComboSiteList.Items.Count - 1 do begin
      TempSite := ComboSiteListMap.GetValue(IntToStr(I)) as TSite;
      if (TempSite = CurrentSite) then begin
        ComboSiteList.ItemIndex := I;
        Break;
      end;
    end;
  finally
    PanelSitePicture.Visible := True;
    IgnoreSiteChanges := False;
    SetSiteChanged(False);
  end;
end;


// Makes the specified item selected in the combobox, if it exists
procedure TMainForm.SelectComboItem(ComboBox: TComboBox; Item: String);
var I: Integer;
begin
  for I := 0 to ComboBox.Items.Count - 1 do
    if (ComboBox.Items[I] = Item) then begin
      ComboBox.ItemIndex := I;
      Break;
    end;
end;


procedure TMainForm.ComboCardListChange(Sender: TObject);
var Card: TCard;
begin
  if not IgnoreCardChanges then begin
    CardLoadSection.Enter();
    try
      Card := ComboCardListMap.GetValue(IntToStr(ComboCardList.ItemIndex)) as TCard;
      if ((Card <> CurrentCard) and (GetCheckSavingResult(True, False, False) <> mrCancel)) then begin
        PageCardEditor.SetFocus();
        CurrentCard := Card;
        LoadCard(CurrentCard);
        ComboCardList.SetFocus();
      end;
    finally
      CardLoadSection.Leave();
    end;
  end;
end;


procedure TMainForm.ButtonDeleteCardClick(Sender: TObject);
var
  ItemIndex: Integer;
begin
  If (MessageDlg('Are you sure you want to delete this card?',
                 mtConfirmation, [mbOk, mbCancel], 0) = mrOk) then begin
    ItemIndex := ComboCardList.ItemIndex;                 
    Database.Delete(CurrentCard);
    RemoveCardFromVariables(CurrentCard);
    FreeAndNil(CurrentCard);
    CardsUpdate();
    DecksUpdate(); // To update the card counts in deck lists
    PanelCardPicture.Visible := False;
    PanelCardFields.Visible := False;
    ButtonSaveCard.Enabled := False;
    ButtonDeleteCard.Enabled := False;
    ButtonSaveCardAs.Enabled := False;
    ButtonUndoCardChanges.Enabled := False;
    ButtonSavePicture.Enabled := False;
    SetCardChanged(False);
    ItemIndex := Min(ItemIndex, ComboCardList.Items.Count - 1);
    if ItemIndex >= 0 then begin
      // Load the next card in the list after deletion
      CurrentCard := ComboCardListMap.GetValue(IntToStr(ItemIndex)) as TCard;
      LoadCard(CurrentCard);
      ComboCardList.SetFocus();
    end;
  end;
end;


procedure TMainForm.ButtonNewDeckClick(Sender: TObject);
var Temp: String;
begin
  Temp := Trim(InputBox('Create new deck', 'Enter the title of the new deck:', ''));
  if (Length(Temp) > 0) then begin
    if (GetCheckSavingResult(False, False, True) <> mrCancel) then begin
      CurrentDeck := TDeck.Create();
      CurrentDeck.Title := Temp;
      LoadDeck(CurrentDeck);
      ButtonSaveDeck.Enabled := True;
      ButtonDeleteDeck.Enabled := False;
      Decks.Add(CurrentDeck);
    end;
  end;
end;


// Loads the deck in the deck editor
procedure TMainForm.LoadDeck(Deck: TDeck);
begin
  IgnoreDeckChanges := True;
  ButtonAddCardToDeck.Enabled := True;
  ButtonRemoveCardFromDeck.Enabled := True;
  ButtonAddSiteToDeck.Enabled := True;
  ButtonRemoveSiteFromDeck.Enabled := True;
  PanelDeckFields.Visible := True;
  EditDeckTitle.Text := CurrentDeck.Title;
  EditDeckComment.Text := CurrentDeck.Comment;
  if (Length(CurrentDeck.TimeOfCreation) > 0) then
    DeckTimeOfCreation.Caption := 'Created ' + CurrentDeck.TimeOfCreation
  else
    DeckTimeOfCreation.Caption := '';
  ListDeckCards.Enabled := True;
  PopulateListDeckCards();
  ListDeckSites.Enabled := True;
  PopulateListDeckSites();
  PopupAllCardsListMenu.Items[1].Enabled := True;
  ButtonSaveDeck.Enabled := True;
  ButtonSaveDeckAs.Enabled := True;
  ButtonUndoDeckChanges.Enabled := False;
  ButtonDeleteDeck.Enabled := True;
  ButtonDeleteDeck.Enabled := True;
  ShowDeckStats();
  IgnoreDeckChanges := False;
  SetDeckChanged(False);
end;


procedure TMainForm.ViewListDeckAllCardsCard(Sender: TObject);
begin
  AnyCardListDblClick(ListDeckAllCards);
end;



procedure TMainForm.ButtonDeleteDeckClick(Sender: TObject);
begin
  If (MessageDlg('Are you sure you want to delete this deck?',
                 mtConfirmation, [mbOk, mbCancel], 0) = mrOk) then begin
    Database.Delete(CurrentDeck);
    RemoveDeckFromVariables(CurrentDeck);
    FreeAndNil(CurrentDeck);
    CurrentDeck := nil;
    EditDeckTitle.Text := '';
    DeckTimeOfCreation.Caption := '';
    ButtonSaveDeckImages.Enabled := False;
    PanelDeckFields.Visible := False;
    ButtonSaveDeck.Enabled := False;
    ButtonSaveDeckAs.Enabled := False;
    ButtonUndoDeckChanges.Enabled := False;
    ButtonDeleteDeck.Enabled := False;
    ListDeckCards.Enabled := False;
    ListDeckSites.Enabled := False;
    LabelDeckStats.Caption := '';
    LabelDeckShadowStats.Caption := '';
    LabelDeckFellowshipStats.Caption := '';
    ButtonAddCardToDeck.Enabled := False;
    ButtonRemoveCardFromDeck.Enabled := False;
    ButtonAddSiteToDeck.Enabled := False;
    ButtonRemoveSiteFromDeck.Enabled := False;
    SetDeckChanged(False);
  end;
  DecksUpdate();
end;


procedure TMainForm.ButtonSaveDeckClick(Sender: TObject);
begin
  if (CurrentDeck <> nil) then
    SaveCurrentDeck();
end;


// Saves the current deck, if any
procedure TMainForm.SaveCurrentDeck();
begin
  CurrentDeck.Title := EditDeckTitle.Text;
  CurrentDeck.Comment := EditDeckComment.Text;
  if (not IsDeckTitleUnique(CurrentDeck)) then begin
    MessageDlg('You cannot save the deck under this title, as a deck must have a unique title.', mtWarning, [mbOK], 0);
  end else begin
    Database.Store(CurrentDeck);
    CurrentDeck.ShowName := '';
    DecksUpdate();
    ButtonDeleteDeck.Enabled := True;
    SetDeckChanged(False);
  end;
end;


// Sets the interface to reflect whether the current card has been changed
procedure TMainForm.SetCardChanged(IsChanged: Boolean);
begin
  if (not IgnoreCardChanges) then begin
    IsCardChanged := IsChanged;
    ButtonUndoCardChanges.Enabled := IsCardChanged;
    ButtonSaveCard.Enabled := IsCardChanged;
    if ((CurrentCard <> nil) and (CurrentCard.ID = 0)) then
      ButtonSaveCard.Enabled := True;
  end;
end;


// Sets the interface to reflect whether the current site has been changed
procedure TMainForm.SetSiteChanged(IsChanged: Boolean);
begin
  if (not IgnoreSiteChanges) then begin
    IsSiteChanged := IsChanged;
    ButtonUndoSiteChanges.Enabled := IsSiteChanged;
    ButtonSaveSite.Enabled := IsSiteChanged;
    if ((CurrentSite <> nil) and (CurrentSite.ID = 0)) then
      ButtonSaveSite.Enabled := True;
  end;
end;


procedure TMainForm.SetDeckChanged(IsChanged: Boolean);
begin
  if (not IgnoreDeckChanges) then begin
    if (CurrentDeck = nil) then begin
      IsDeckChanged := False;
    end else begin
      IsDeckChanged := IsChanged;
      if (IsDeckChanged) then
        ShowDeckStats();
    end;
    ButtonUndoDeckChanges.Enabled := IsDeckChanged;
    ButtonSaveDeck.Enabled := IsDeckChanged;
    ButtonSaveDeckImages.Enabled := (CurrentDeck <> nil) and (CurrentDeck.GetCardsCount() + CurrentDeck.GetSitesCount() > 0);
    if ((CurrentDeck <> nil) and (CurrentDeck.ID = 0)) then
      ButtonSaveDeck.Enabled := True;
  end;
end;


// Fills in the deck statistics controls
procedure TMainForm.ShowDeckStats();
var I, J: Integer;
    FellowshipCards, ShadowCards: array of Integer; // counts for each card type
    FellowshipTotal, ShadowTotal: Integer;
begin
  FellowshipTotal := 0;
  ShadowTotal := 0;
  SetLength(FellowshipCards, CardTypes.Count);
  SetLength(ShadowCards, CardTypes.Count);
  for I := 0 to CurrentDeck.Cards.Count - 1 do
    if (not TDeckCard(CurrentDeck.Cards.Items[I]).IsDeleted) then
      with (TDeckCard(CurrentDeck.Cards[I]).Card) do begin
        if (Race.IsGood) then begin
          Inc(FellowshipTotal);
          for J := 0 to CardTypes.Count - 1 do
            if TCardType(CardTypes.Items[J]) = CardType then
              Inc(FellowshipCards[J]);
        end else begin
          Inc(ShadowTotal);
          for J := 0 to CardTypes.Count - 1 do
            if TCardType(CardTypes.Items[J]) = CardType then
              Inc(ShadowCards[J]);
        end;
      end;
  LabelDeckStats.Caption :=
    Format('%d cards, %d sites total.', [ShadowTotal + FellowshipTotal, CurrentDeck.GetSitesCount()]);
  LabelDeckFellowshipStats.Caption := Format('%d Fellowship cards:', [FellowshipTotal]);
  LabelDeckShadowStats.Caption := Format('%d Shadow cards:', [ShadowTotal]);
  for I := 0 to CardTypes.Count - 1 do begin
    LabelDeckFellowshipStats.Caption := LabelDeckFellowshipStats.Caption +
      Format('%s %d %s%s',
             [NEW_LINE, FellowshipCards[I], TCardType(CardTypes[I]).Name,
              Plural(FellowshipCards[I])]);
    LabelDeckShadowStats.Caption := LabelDeckShadowStats.Caption +
      Format('%s %d %s%s',
             [NEW_LINE, ShadowCards[I], TCardType(CardTypes[I]).Name,
              Plural(ShadowCards[I])]);
  end;
end;


// Removes the currently active card from the deck's card list
procedure TMainForm.RemoveCardFromDeckList();
var Card: TDeckCard;
    ListItem: TListItem;
begin
  ListItem := ListDeckCards.ItemFocused;
  if (ListItem <> nil) then begin
    Card := ListItem.Data;
    Card.IsDeleted := True;
    UpdateDeckCardsList();
    SetDeckChanged(True);
  end;
end;


// Updates the ListDeckCards list of cards
procedure TMainForm.UpdateDeckCardsList();
var I, Index: Integer;
    ListItem, SelectedListItem: TListItem;
    LastCard, Card: TDeckCard;
begin
  LastCard := nil;
  SelectedListItem := nil;
  Index := 0;
  ListItem := ListDeckCards.ItemFocused;
  if (ListItem <> nil) then begin
    Index := ListDeckCards.Items.IndexOf(ListItem);
    LastCard := ListItem.Data;
  end;
  PopulateListDeckCards();
  for I := 0 to ListDeckCards.Items.Count - 1 do begin
    Card := ListDeckCards.Items[I].Data;
    if (Card = LastCard) then begin
      Index := I;
      SelectedListItem := ListDeckCards.Items[I];
    end;
  end;
  if (SelectedListItem <> nil) then
    ListDeckCards.ItemFocused := SelectedListItem
  else begin
    if (Index >= ListDeckCards.Items.Count) then
      Index := ListDeckCards.Items.Count - 1;
    ListDeckCards.ItemFocused := ListDeckCards.Items[Index];
  end;
  ShowDeckStats();
end;



procedure TMainForm.AddCardToDeck(Card: TCard);
var DeckCard: TDeckCard;
begin
  DeckCard := TDeckCard.Create();
  DeckCard.Card := Card;
  CurrentDeck.Cards.Add(DeckCard);
  PopulateListDeckCards();
  SetDeckChanged(True);
end;



procedure TMainForm.Addcardtodeck1Click(Sender: TObject);
var ListItem: TListItem;
    Card: TCard;
begin
  ListItem := ListDeckAllCards.ItemFocused;
  if (ListItem <> nil) then begin
    Card := ListItem.Data;
    AddCardToDeck(Card);
  end;
end;


procedure TMainForm.Removecardfromdeck1Click(Sender: TObject);
begin
  RemoveCardFromDeckList();
end;


procedure TMainForm.ButtonAddCardToDeckClick(Sender: TObject);
var ListItem: TListItem;
    Card: TCard;
begin
  ListItem := ListDeckAllCards.ItemFocused;
  if (ListItem <> nil) then begin
    Card := ListItem.Data;
    AddCardToDeck(Card);
  end;
end;


procedure TMainForm.ButtonRemoveCardFromDeckClick(Sender: TObject);
begin
  if (CurrentDeck <> nil) then
    RemoveCardFromDeckList();
end;


procedure TMainForm.EditCardChange(Sender: TObject);
begin
  SetCardChanged(True);
  if (Sender <> EditInternalComment) then PreviewCard(CurrentCard, False);
end;



procedure TMainForm.ComboRaceChange(Sender: TObject);
begin
  if (CurrentCard.OriginalRace = nil) then
    CurrentCard.OriginalRace := CurrentCard.Race;
  CurrentCard.Race := GetRaceByName(ComboRace.Items[ComboRace.ItemIndex]);
  ChangeCardEditorInterface(CurrentCard);
  PreviewCard(CurrentCard, False);
  SetCardChanged(True);
end;


procedure TMainForm.ComboCardTypeChange(Sender: TObject);
begin
  if (CurrentCard.OriginalCardType = nil) then
    CurrentCard.OriginalCardType := CurrentCard.CardType;
  CurrentCard.CardType := GetCardTypeByName(ComboCardType.Items[ComboCardType.ItemIndex]);
  ChangeCardEditorInterface(CurrentCard);
  PreviewCard(CurrentCard, False);
  SetCardChanged(True);
end;


procedure TMainForm.ButtonSaveCardAsClick(Sender: TObject);
var
  Temp: String;
  NewCard: TCard;
begin
  Temp := InputBox('Save card as', 'Enter the new title of the card:', '');
  if (Length(Temp) > 0) then begin
    NewCard := TCard.Create();
    SetInterfaceDataToCard(NewCard);
    NewCard.Title := Temp;
    if (not IsCardTitleUnique(NewCard)) then begin
      MessageDlg('You cannot save the card as a new card, because its title (and ' +
                  'together with subtitle if character) is not unique.', mtWarning, [mbOK], 0);
      NewCard.Free();
    end else begin
      EditTitle.Text := Temp;
      if (CurrentCard.OriginalCardType <> nil) then
        CurrentCard.CardType := CurrentCard.OriginalCardType;
      if (CurrentCard.OriginalRace <> nil) then
        CurrentCard.Race := CurrentCard.OriginalRace;
      // Copy over content picture
      if CurrentCard.ContentPicture <> nil then begin
        NewCard.ContentPicture := TPicture.Create();
        NewCard.ContentPicture.Bitmap.Assign(CurrentCard.ContentPicture);
      end;
      if (CurrentCard.IsContentPictureChanged) then begin
        // Card picture has been changed, but is being saved under a different
        // name - restore the original card's condition.
        FreeAndNil(CurrentCard.ContentPicture);
        CurrentCard.ContentPicture := CurrentCard.OriginalContentPicture;
        CurrentCard.OriginalContentPicture := nil;
      end;
      CurrentCard.IsContentPictureChanged := False;
      CurrentCard.OriginalCardType := nil;
      CurrentCard.OriginalRace := nil;
      CurrentCard := NewCard;
      FreeAndNil(CurrentCard.Thumbnail); // Clear thumbnail to regenerate
      PreviewCard(CurrentCard);
      SaveCurrentCard();
    end;
  end;
end;


// Checks whether the card has a unique title-subtitle combination
function TMainForm.IsCardTitleUnique(Card: TCard): Boolean;
var I: Integer;
begin
  Result := True;
  for I := 0 to Cards.Count - 1 do
    if (TCard(Cards.Items[I]).ID <> Card.ID) then
      if (AnsiUpperCase(TCard(Cards.Items[I]).Title) = AnsiUpperCase(Card.Title)) then begin
        if (Card.CardType.IsCharacter) then begin
          Result := False;
          Break;
        end else if (TCard(Cards.Items[I]).Subtitle = Card.Subtitle) then begin
          Result := False;
          Break;
        end;
      end;
end;



// Checks whether the deck has a unique title
function TMainForm.IsDeckTitleUnique(Deck: TDeck): Boolean;
var I: Integer;
begin
  Result := True;
  for I := 0 to Decks.Count - 1 do
    if (TDeck(Decks.Items[I]).ID <> Deck.ID) then
      if (AnsiUpperCase(TDeck(Decks.Items[I]).Title) = AnsiUpperCase(Deck.Title)) then begin
        Result := False;
        Break;
      end;
end;


// Checks whether the site has a unique title
function TMainForm.IsSiteTitleUnique(Site: TSite): Boolean;
var I: Integer;
begin
  Result := True;
  for I := 0 to Sites.Count - 1 do
    if (TSite(Sites.Items[I]) <> Site) then
      if (AnsiUpperCase(TSite(Sites.Items[I]).Title) = AnsiUpperCase(Site.Title)) then begin
        Result := False;
        Break;
      end;
end;


procedure TMainForm.ButtonUndoCardChangesClick(Sender: TObject);
begin
  if (CurrentCard.OriginalCardType <> nil) then
    CurrentCard.CardType := CurrentCard.OriginalCardType;
  if (CurrentCard.OriginalRace <> nil) then
    CurrentCard.Race := CurrentCard.OriginalRace;
  if CurrentCard.IsContentPictureChanged then begin
    CurrentCard.ContentPicture.Free();
    CurrentCard.ContentPicture := CurrentCard.OriginalContentPicture;
    if CurrentCard.ContentPicture <> nil then
      ContentPicture.Picture.Assign(CurrentCard.ContentPicture)
    else
      ContentPicture.Picture.Assign(TransparentPixel)
  end;
  CurrentCard.OriginalCardType := nil;
  CurrentCard.OriginalRace := nil;
  CurrentCard.OriginalContentPicture := nil;
  SetCardDataToInterface(CurrentCard);
  PreviewCard(CurrentCard);
  CurrentCard.IsContentPictureChanged := False;
  SetStatusText(Format('Cleared unsaved changes to card "%s"', [CurrentCard.GetDisplayName()]), 0, 0);
  SetCardChanged(False);
end;


procedure TMainForm.EditDeckChange(Sender: TObject);
begin
  SetDeckChanged(True);
end;


procedure TMainForm.ButtonUndoDeckChangesClick(Sender: TObject);
begin
  UndoCurrentDeckChanges();
  SetStatusText(Format('Cleared unsaved changes to deck "%s"', [CurrentDeck.Title]), 0, 0);
end;


// Undos the changes made to the current deck
procedure TMainForm.UndoCurrentDeckChanges();
var I: Integer;
    DeckCard: TDeckCard;
    DeckSite: TDeckSite;
begin
  EditDeckTitle.Text := CurrentDeck.Title;
  EditDeckComment.Text := CurrentDeck.Comment;
  for I := 0 to CurrentDeck.Cards.Count - 1 do begin
    DeckCard := CurrentDeck.Cards.Items[I];
    if (DeckCard.ID = 0) then begin
      CurrentDeck.Cards.Items[I] := nil; // Unsaved card, undo -> abandon card
    end else begin
      DeckCard.IsDeleted := False; // Reset possible deletion
    end;
  end;
  CurrentDeck.Cards.Pack();
  UpdateDeckCardsList();
  for I := 0 to CurrentDeck.Sites.Count - 1 do begin
    DeckSite := CurrentDeck.Sites.Items[I];
    if (DeckSite.ID = 0) then begin
      CurrentDeck.Sites.Items[I] := nil;
    end else begin
      DeckSite.IsDeleted := False;
    end;
  end;
  CurrentDeck.Sites.Pack();
  PopulateListDeckSites();
  ButtonSaveDeck.Enabled := False;
  ButtonUndoDeckChanges.Enabled := False;
  ShowDeckStats();
  SetDeckChanged(False);
end;


procedure TMainForm.ButtonSaveDeckAsClick(Sender: TObject);
var Temp: String;
    I: Integer;
    NotUnique: Boolean;
    NewDeck: TDeck;
    TempCard: TDeckCard;
    TempSite: TDeckSite;
begin
  Temp := InputBox('Save deck as', 'Enter the new title of the deck:', '');
  if (Length(Temp) > 0) then begin
    NotUnique := False;
    for I := 0 to Decks.Count - 1 do
      if (TDeck(Decks.Items[I]).Title = Temp) then begin
        NotUnique := True;
        Break;
      end;
    if (NotUnique) then begin
      MessageDlg('You cannot save the deck under this title, as a deck must have a unique title.', mtWarning, [mbOK], 0);
    end else begin
      NewDeck := TDeck.Create();
      NewDeck.Title := Temp;
      NewDeck.Comment := EditDeckComment.Text;
      for I := 0 to CurrentDeck.Cards.Count - 1 do
        if (not TDeckCard(CurrentDeck.Cards.Items[I]).IsDeleted) then begin
          TempCard := TDeckCard.Create();
          TempCard.Card := TDeckCard(CurrentDeck.Cards.Items[I]).Card;
          NewDeck.Cards.Add(TempCard);
        end;
      for I := 0 to CurrentDeck.Sites.Count - 1 do
        if (not TDeckSite(CurrentDeck.Sites.Items[I]).IsDeleted) then begin
          TempSite := TDeckSite.Create();
          TempSite.Site := TDeckSite(CurrentDeck.Sites.Items[I]).Site;
          NewDeck.Sites.Add(TempSite);
        end;
      UndoCurrentDeckChanges();
      CurrentDeck := NewDeck;
      Decks.Add(CurrentDeck);
      Database.Store(CurrentDeck);
      DecksUpdate();
      LoadDeck(CurrentDeck);
    end;
  end;
end;


// Resets the deck editor interface, disabling controls etc
procedure TMainForm.ResetCardEditor(ClearComboList: Boolean = True);
begin
  PanelCardPicture.Visible := False;
  PanelCardFields.Visible := False;
  if (ClearComboList) then ComboCardList.Items.Clear();
  ButtonSaveCard.Enabled := False;
  ButtonSaveCardAs.Enabled := False;
  ButtonDeleteCard.Enabled := False;
  ButtonSavePicture.Enabled := False;
  ButtonUndoCardChanges.Enabled := False;
  ButtonClearListFilter.Enabled := False;
  IsCardChanged := False;
end;


// Resets the site editor interface
procedure TMainForm.ResetSiteEditor(ClearComboList: Boolean = True);
begin
  PanelSiteFields.Visible := False;
  PanelSitePicture.Visible := False;
  if (ClearComboList) then ComboSiteList.Items.Clear();
  ButtonSaveSite.Enabled := False;
  ButtonSaveSiteAs.Enabled := False;
  ButtonDeleteSite.Enabled := False;
  ButtonSaveSitePicture.Enabled := False;
  ButtonUndoSiteChanges.Enabled := False;
  IsSiteChanged := False;
end;


// Resets the card editor interface
procedure TMainForm.ResetDeckEditor();
begin
  IgnoreDeckChanges := True;
  ButtonAddCardToDeck.Enabled := False;
  ButtonRemoveCardFromDeck.Enabled := False;
  ButtonAddSiteToDeck.Enabled := False;
  ButtonRemoveSiteFromDeck.Enabled := False;
  ListDeckCards.Items.Clear();
  ListDeckSites.Items.Clear();
  EditDeckTitle.Text := '';
  PanelDeckFields.Visible := False;
  ButtonSaveDeckImages.Enabled := False;
  DeckTimeOfCreation.Caption := '';
  ComboDeckList.Items.Clear();
  ListDeckAllCards.Items.Clear();
  ListDeckAllSites.Items.Clear();
  ButtonSaveDeck.Enabled := False;
  ButtonSaveDeckAs.Enabled := False;
  ButtonDeleteDeck.Enabled := False;
  ButtonUndoDeckChanges.Enabled := False;
  LabelDeckFellowshipStats.Caption := '';
  LabelDeckShadowStats.Caption := '';
  LabelDeckStats.Caption := '';
  ListDeckCards.Enabled := False;
  ListDeckSites.Enabled := False;
  IgnoreDeckChanges := False;
  IsDeckChanged := False;
end;


// Populates the card grid with card items
procedure TMainForm.PopulateListCards();
var I, J: Integer;
    Card, SelectedCard: TCard;
    ListItem, Selected: TListItem;
    IsCardInDeck: boolean;
begin
  Selected := nil;
  SelectedCard := nil;
  if (ListCards.Selected <> nil) then
    SelectedCard := ListCards.Selected.Data;
  ListCards.Items.Clear();
  for I := 0 to Cards.Count - 1 do begin
    Card := Cards.Items[I];
    if (ApplyFilter) then begin
      if (Length(ListCardsFilterRace) > 0) then begin
        if (ListCardsFilterRace = CARD_LIST_FILTER_RACES_FELLOWSHIP) then begin
          if (not Card.Race.IsGood) then
            Continue;
        end else if (ListCardsFilterRace = CARD_LIST_FILTER_RACES_SHADOW) then begin
          if (Card.Race.IsGood) then
            Continue;
        end else if (Card.Race.Name <> ListCardsFilterRace) then
          Continue;
      end;
      if (Length(ListCardsFilterType) > 0) then begin
        if (Card.CardType.Name <> ListCardsFilterType) then
          Continue;
      end;
      if (Length(ListCardsFilterTime) > 0) then begin
        if (Card.Time <> ListCardsFilterTime) then
          Continue;
      end;
      if (ListCardsFilterDeck <> nil) then begin
        IsCardInDeck := False;
        for J := 0 to ListCardsFilterDeck.Cards.Count - 1 do begin
          if (TDeckCard(ListCardsFilterDeck.Cards.Items[J]).Card = Card) then
            IsCardInDeck := true;
        end;
        if (not IsCardInDeck) then
          Continue;
      end;
    end;
    ListItem := ListCards.Items.Add();
    ListItem.Caption := Card.Title;
    if (Length(Card.Subtitle) > 0) then
      ListItem.Caption := ListItem.Caption + ', ' + Card.Subtitle;
    ListItem.SubItems.Add(IntToStr(Card.TwiLightCost));
    ListItem.SubItems.Add(Card.Strength);
    ListItem.SubItems.Add(Card.Health);
    ListItem.SubItems.Add(Card.Race.Name);
    ListItem.SubItems.Add(Card.CardType.Name);
    ListItem.SubItems.Add(Card.Time);
    ListItem.SubItems.Add(Card.TimeOfCreation);
    ListItem.Data := Card;
    if (Card = SelectedCard) then
      Selected := ListItem;
  end;
  for I := 0 to ListCards.Columns.Count - 1 do begin
    ListCards.Columns.Items[I].AutoSize := True;
    ListCards.Columns.Items[I].Width := -2;
  end;
  ListCards.Columns.Items[0].Width := 300;
  ListCards.CustomSort(nil, 0);
  if (Selected <> nil) then begin
    ListCards.Selected := Selected;
    ListCards.ItemFocused := Selected;
  end;
  LabelCardListStats.Caption := Format('%d card%s',
    [ListCards.Items.Count, Plural(ListCards.Items.Count)]);
end;


// Populates the ListDeckAllCards with card items
procedure TMainForm.PopulateListDeckAllCards();
var I: Integer;
    Card: TCard;
    ListItem: TListItem;
begin
  ListDeckAllCards.Items.Clear();
  for I := 0 to Cards.Count - 1 do begin
    Card := Cards.Items[I];
    ListItem := ListDeckAllCards.Items.Add();
    ListItem.Caption := Card.Title;
    if (Length(Card.Subtitle) > 0) then
      ListItem.Caption := ListItem.Caption + ', ' + Card.Subtitle;
    ListItem.SubItems.Add(Card.Race.Name);
    ListItem.SubItems.Add(Card.CardType.Name);
    ListItem.Data := Card;
  end;
  ListDeckAllCards.CustomSort(nil, 0);
end;


// Populates the ListDeckCards with card items
procedure TMainForm.PopulateListDeckCards();
var I: Integer;
    Card: TDeckCard;
    ListItem: TListItem;
begin
  if (CurrentDeck <> nil) then begin
    ListDeckCards.Items.Clear();
    for I := 0 to CurrentDeck.Cards.Count - 1 do begin
      Card := CurrentDeck.Cards.Items[I];
      if (not Card.IsDeleted) then begin
        ListItem := ListDeckCards.Items.Add();
        ListItem.Caption := Card.Card.Title;
        if (Length(Card.Card.Subtitle) > 0) then
          ListItem.Caption := ListItem.Caption + ', ' + Card.Card.Subtitle;
        ListItem.SubItems.Add(Card.Card.Race.Name);
        ListItem.SubItems.Add(Card.Card.CardType.Name);
        ListItem.Data := Card;
      end;
    end;
    ListDeckCards.CustomSort(nil, 0);
  end;
end;


// Populates the ListDeckAllSites with sites
procedure TMainForm.PopulateListDeckAllSites();
var I: Integer;
    Site: TSite;
    ListItem: TListItem;
begin
  ListDeckAllSites.Items.Clear();
  for I := 0 to Sites.Count - 1 do begin
    Site := Sites.Items[I];
    ListItem := ListDeckAllSites.Items.Add();
    ListItem.Caption := Site.Title;
    ListItem.SubItems.Add(Site.Time);
    ListItem.SubItems.Add(IntToStr(Site.TwilightCost));
    ListItem.Data := Site;
  end;
  ListDeckAllCards.CustomSort(nil, 0);
end;


// Populates the ListDeckSites with sites
procedure TMainForm.PopulateListDeckSites();
var I: Integer;
    Site: TDeckSite;
    ListItem: TListItem;
begin
  if (CurrentDeck <> nil) then begin
    ListDeckSites.Items.Clear();
    for I := 0 to CurrentDeck.Sites.Count - 1 do begin
      Site := CurrentDeck.Sites.Items[I];
      if (not Site.IsDeleted) then begin
        ListItem := ListDeckSites.Items.Add();
        ListItem.Caption := Site.Site.Title;
        ListItem.SubItems.Add(Site.Site.Time);
        ListItem.SubItems.Add(IntToStr(Site.Site.TwilightCost));
        ListItem.Data := Site;
      end;
    end;
    ListDeckSites.CustomSort(nil, 0);
  end;
end;


procedure TMainForm.AnyCardListColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if (Sender = ListCards) then begin
    if (Column.Caption = ListCardsSortInfo.SortColumn) then
      ListCardsSortInfo.IsSortReversed := not ListCardsSortInfo.IsSortReversed else
    ListCardsSortInfo.SortColumn := Column.Caption;
    ListCards.CustomSort(nil, 0);
  end else if (Sender = ListDeckAllCards) then begin
    if (Column.Caption = ListDeckAllCardsSortInfo.SortColumn) then
      ListDeckAllCardsSortInfo.IsSortReversed := not ListDeckAllCardsSortInfo.IsSortReversed else
    ListDeckAllCardsSortInfo.SortColumn := Column.Caption;
    ListDeckAllCards.CustomSort(nil, 0);
  end else if (Sender = ListDeckCards) then begin
    if (Column.Caption = ListDeckCardsSortInfo.SortColumn) then
      ListDeckCardsSortInfo.IsSortReversed := not ListDeckCardsSortInfo.IsSortReversed else
    ListDeckCardsSortInfo.SortColumn := Column.Caption;
    ListDeckCards.CustomSort(nil, 0);
  end;
end;


procedure TMainForm.AnyCardListCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var Card1, Card2: TCard;
    Field1, Field2: String;
    IsNumberSort: Boolean;
    SortInfo: TSortInfo;
begin
  if (Sender = ListCards) then
    SortInfo := ListCardsSortInfo
  else if (Sender = ListDeckAllCards) then
    SortInfo := ListDeckAllCardsSortInfo
  else if (Sender = ListDeckCards) then
    SortInfo := ListDeckCardsSortInfo
  else begin
    SortInfo := nil;
    MessageDlg('Fatal error: the sender ' + Sender.ClassName + ' is an unknown list.', mtError, [mbOk], 0);
  end;
  IsNumberSort := False;
  if (Sender = ListDeckCards) then begin
    Card1 := TDeckCard(Item1.Data).Card;
    Card2 := TDeckCard(Item2.Data).Card;
  end else begin
    Card1 := Item1.Data;
    Card2 := Item2.Data;
  end;
  if (SortInfo.SortColumn = CARD_LIST_COLUMN_TITLE) then begin
    Field1 := Card1.Title;
    if (Length(Card1.Subtitle) > 0) then
      Field1 := Field1 + ', ' + Card1.Subtitle;
    Field2 := Card2.Title;
    if (Length(Card2.Subtitle) > 0) then
      Field2 := Field2 + ', ' + Card2.Subtitle;
  end else if (SortInfo.SortColumn = CARD_LIST_COLUMN_TWILIGHT_COST) then begin
    Field1 := IntToStr(Card1.TwilightCost);
    Field2 := IntToStr(Card2.TwilightCost);
    IsNumberSort := True;
  end else if (SortInfo.SortColumn = CARD_LIST_COLUMN_STRENGTH) then begin
    Field1 := Card1.Strength;
    Field2 := Card2.Strength;
    if (IsNumber(Field1) and IsNumber(Field2)) then
      IsNumberSort := True;
  end else if (SortInfo.SortColumn = CARD_LIST_COLUMN_HEALTH) then begin
    Field1 := Card1.Health;
    Field2 := Card2.Health;
    if (IsNumber(Field1) and IsNumber(Field2)) then
      IsNumberSort := True;
  end else if (SortInfo.SortColumn = CARD_LIST_COLUMN_RACE) then begin
    Field1 := Card1.Race.Name;
    Field2 := Card2.Race.Name;
  end else if (SortInfo.SortColumn = CARD_LIST_COLUMN_TYPE) then begin
    Field1 := Card1.CardType.Name;
    Field2 := Card2.CardType.Name;
  end else if (SortInfo.SortColumn = CARD_LIST_COLUMN_TIME) then begin
    Field1 := Card1.Time;
    Field2 := Card2.Time;
  end else if (SortInfo.SortColumn = CARD_LIST_COLUMN_TIME_CREATED) then begin
    Field1 := Card1.ExactTimeOfCreation;
    Field2 := Card2.ExactTimeOfCreation;
  end else begin
    raise Exception.Create('Error: column "' + SortInfo.SortColumn + '" is unknown.');
  end;
  if (IsNumberSort) then begin
    if (StrToInt(Field1) > StrToInt(Field2)) then
      Compare := 1
    else if (StrToInt(Field1) < StrToInt(Field2)) then
      Compare := -1
    else
      Compare := 0;
  end else
    Compare := StrIComp(PChar(Field1), PChar(Field2));
  // If equal, sort by title next
  if ((Compare = 0) and (SortInfo.SortColumn <> CARD_LIST_COLUMN_TITLE)) then begin
    Field1 := Card1.Title;
    if (Length(Card1.Subtitle) > 0) then
      Field1 := Field1 + ', ' + Card1.Subtitle;
    Field2 := Card2.Title;
    if (Length(Card2.Subtitle) > 0) then
      Field2 := Field2 + ', ' + Card2.Subtitle;
    Compare := StrIComp(PChar(Field1), PChar(Field2));
  end;
  if (SortInfo.IsSortReversed) then
    Compare := 0 - Compare;
end;


procedure TMainForm.ButtonApplyListFilterClick(Sender: TObject);
begin
  ListCardsFilterRace := Trim(ComboListRaces.Items[ComboListRaces.ItemIndex]);
  ListCardsFilterType := Trim(ComboListCardTypes.Items[ComboListCardTypes.ItemIndex]);
  ListCardsFilterTime := Trim(ComboListTimes.Items[ComboListTimes.ItemIndex]);
  // ItemIndex - 1, as ComboListDecks has an empty value at first
  ListCardsFilterDeck := ComboListDecksMap.GetValue(IntToStr(ComboListDecks.ItemIndex - 1)) as TDeck;
  ApplyFilter := True;
  ButtonClearListFilter.Enabled := True;
  PopulateListCards();
end;


// Resets the card list page, disabling filters etc
procedure TMainForm.ResetCardList();
begin
  ListCardsFilterRace := '';
  ListCardsFilterType := '';
  ListCardsFilterTime := '';
  ComboListRaces.ItemIndex := 0;
  ComboListCardTypes.ItemIndex := 0;
  ComboListTimes.ItemIndex := 0;
  ListCards.Items.Clear();
  ListCardsSortInfo.Free();
  ListCardsSortInfo := TSortInfo.Create();
  ListCards.Items.Clear();
  ApplyFilter := False;
end;


procedure TMainForm.ButtonClearListFilterClick(Sender: TObject);
begin
  ApplyFilter := False;
  PopulateListCards();
  ButtonClearListFilter.Enabled := False;
end;


procedure TMainForm.AnyCardListKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var ListItem: TListItem;
    Card: TDeckCard;
begin
  if (Sender = ListCards) then begin
    if (Key = VK_RETURN) then
      AnyCardListDblClick(Sender);
  end else if (Sender = ListDeckAllCards) then begin
    if (Key = VK_RETURN) then
      AnyCardListDblClick(Sender);
  end else if (Sender = ListDeckCards) then begin
    if (Key = VK_RETURN) then
      AnyCardListDblClick(Sender)
    else if (Key = VK_DELETE) then begin
      ListItem := TListView(Sender).ItemFocused;
      if (ListItem <> nil) then begin
        Card := ListItem.Data;
        Card.IsDeleted := True;
        UpdateDeckCardsList();
        SetDeckChanged(True);
      end;
    end;
  end;
end;


procedure TMainForm.AnyCardListDblClick(Sender: TObject);
var ListItem: TListItem;
    Card: TCard;
    PreviousActivePage: TTabsheet;
begin
  if (Sender is TListView) then begin
    ListItem := TListView(Sender).ItemFocused;
    if (ListItem <> nil) then begin
      if (Sender = ListDeckCards) then
        Card := TDeckCard(ListItem.Data).Card
      else
        Card := ListItem.Data;
      PreviousActivePage := PageControl.ActivePage;
      PageControl.ActivePage := PageCardEditor;
      if (Card <> CurrentCard) then begin
        if (GetCheckSavingResult(True, False, False) <> mrCancel) then begin
          CurrentCard := Card;
          LoadCard(CurrentCard);
        end else
          PageControl.ActivePage := PreviousActivePage;
      end;
    end;
  end;
end;



procedure TMainForm.ListDeckCardsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (Sender = ListDeckCards) and (Source = ListDeckAllCards) then
    Accept := True
  else
    Accept := False;
end;


procedure TMainForm.ListDeckCardsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var ListItem: TListItem;
    Card: TCard;
begin
  ListItem := TListView(Source).ItemFocused;
  if (ListItem <> nil) then begin
    Card := ListItem.Data;
    AddCardToDeck(Card);
  end;
end;


procedure TMainForm.CardListMenuClick(Sender: TObject);
begin
  AnyCardListDblClick(ListDeckCards);
end;


procedure TMainForm.ComboSiteListChange(Sender: TObject);
var Site: TSite;
begin
  Site := ComboSiteListMap.GetValue(IntToStr(ComboSiteList.ItemIndex)) as TSite;
  if ((Site <> CurrentSite) and (GetCheckSavingResult(False, True, False) <> mrCancel)) then begin
    PageSiteEditor.SetFocus();
    CurrentSite := Site;
    LoadSite(CurrentSite);
    ComboSiteList.SetFocus();
  end;
end;


procedure TMainForm.ButtonNewSiteClick(Sender: TObject);
begin
  if (GetCheckSavingResult(False, True, False) <> mrCancel) then begin
    PanelSitePicture.Visible := False;
    CurrentSite := TSite.Create();
    SetSiteDataToInterface(CurrentSite);
    ComboSiteDirections.ItemIndex := 0;
    PreviewSite(CurrentSite);
    SetSiteChanged(False);
    ButtonSaveSite.Enabled := True;
    ButtonDeleteSite.Enabled := False;
    EditSiteTitle.SetFocus();
  end;
end;



procedure TMainForm.ButtonSaveSiteClick(Sender: TObject);
begin
  if (CurrentSite <> nil) then begin
    if (IsSiteTitleUnique(CurrentSite)) then begin
      if (Length(EditSiteTitle.Text) > 0) then begin
        SetInterfaceDataToSite(CurrentSite);
        SaveCurrentSite();
      end else begin
        MessageDlg('Every site must have a title.', mtError, [mbOk], 0);
      end;
    end else begin
      MessageDlg('The site''s title is not unique. ' + NEW_LINE +
                  'Change it and try again.', mtError, [mbOk], 0);
    end;
  end;
end;



procedure TMainForm.ButtonSaveSiteAsClick(Sender: TObject);
var Temp: String;
    NewSite: TSite;
begin
  Temp := InputBox('Save site as', 'Enter the new title of the site:', '');
  if (Length(Temp) > 0) then begin
    NewSite := TSite.Create();
    SetInterfaceDataToSite(NewSite);
    NewSite.Title := Temp;
    if (not IsSiteTitleUnique(NewSite)) then begin
      MessageDlg('You cannot save the site under the title "' + Temp + '", ' +
                  'as a site with that name already exists.', mtWarning, [mbOK], 0);
      NewSite.Free();
    end else begin
      EditSiteTitle.Text := Temp;
      // Copy over content picture
      if CurrentSite.ContentPicture <> nil then begin
        NewSite.ContentPicture := TPicture.Create();
        NewSite.ContentPicture.Bitmap.Assign(CurrentSite.ContentPicture);
      end;
      if (CurrentSite.IsContentPictureChanged) then begin
        // Site picture has been changed, but is being saved under a different
        // name - restore the original site's condition.
        FreeAndNil(CurrentSite.ContentPicture);
        CurrentSite.ContentPicture := CurrentSite.OriginalContentPicture;
        CurrentSite.OriginalContentPicture := nil;
        CurrentSite.IsContentPictureChanged := False;
      end;

      CurrentSite := NewSite;
      SaveCurrentSite();
      PreviewSite(CurrentSite);
    end;
  end;
end;



procedure TMainForm.ButtonUndoSiteChangesClick(Sender: TObject);
begin
  if (CurrentSite.IsContentPictureChanged) then begin
    CurrentSite.ContentPicture.Free();
    CurrentSite.ContentPicture := CurrentSite.OriginalContentPicture;
    if (CurrentSite.ContentPicture <> nil) then
      ContentPicture.Picture.Assign(CurrentSite.ContentPicture)
    else
      ContentPicture.Picture.Assign(TransparentPixel)
  end;
  CurrentSite.OriginalContentPicture := nil;
  CurrentSite.IsContentPictureChanged := False;
  SetSiteDataToInterface(CurrentSite);
  PreviewSite(CurrentSite);
  SetStatusText(Format('Cleared unsaved changes to site "%s"', [CurrentSite.Title]), 0, 0);
  SetSiteChanged(False);
end;


procedure TMainForm.ButtonDeleteSiteClick(Sender: TObject);
var
  ItemIndex: Integer;
begin
  if (MessageDlg('Are you sure you want to delete this site?',
                 mtConfirmation, [mbOk, mbCancel], 0) = mrOk) then begin
    ItemIndex := ComboSiteList.ItemIndex;
    Database.Delete(CurrentSite);
    RemoveSiteFromVariables(CurrentSite);
    FreeAndNil(CurrentSite);
    SitesUpdate();
    DecksUpdate();
    PanelSiteFields.Visible := False;
    PanelSitePicture.Visible := False;
    ButtonSaveSite.Enabled := False;
    ButtonDeleteSite.Enabled := False;
    ButtonSaveSiteAs.Enabled := False;
    ButtonUndoSiteChanges.Enabled := False;
    ButtonSaveSitePicture.Enabled := False;
    SetSiteChanged(False);
    ItemIndex := Min(ItemIndex, ComboSiteList.Items.Count - 1);
    if ItemIndex >= 0 then begin
      // Load the next card in the list after deletion
      CurrentSite := ComboSiteListMap.GetValue(IntToStr(ItemIndex)) as TSite;
      LoadSite(CurrentSite);
      ComboSiteList.SetFocus();
    end;
  end;
end;


procedure TMainForm.EditSiteChange(Sender: TObject);
begin
  SetSiteChanged(True);
  if (Sender <> EditSiteInternalComment) then PreviewSite(CurrentSite, False);
end;


procedure TMainForm.ListDeckSitesDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (((Sender = ListDeckSites) or (Sender = ListDeckAllSites)) and
      ((Source = ListDeckSites) or (Source = ListDeckAllSites))) then
    Accept := True
  else
    Accept := False;
end;


procedure TMainForm.AddSiteToDeck(Sender: TObject);
var ListItem: TListItem;
    Site: TSite;
    DeckSite: TDeckSite;
begin
  ListItem := ListDeckAllSites.ItemFocused;
  if (ListItem <> nil) then begin
    Site := ListItem.Data;
    if (not CurrentDeck.HasSite(Site)) then begin
      DeckSite := TDeckSite.Create();
      DeckSite.Site := Site;
      CurrentDeck.Sites.Add(DeckSite);
      PopulateListDeckSites();
      SetDeckChanged(True);
    end;
  end;
end;


procedure TMainForm.ListDeckSitesDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  AddSiteToDeck(Sender);
end;


procedure TMainForm.ListDeckAllSitesDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  RemoveSiteFromDeck(Sender);
end;


procedure TMainForm.RemoveSiteFromDeck(Sender: TObject);
var Site: TDeckSite;
    ListItem: TListItem;
begin
  ListItem := ListDeckSites.ItemFocused;
  if (ListItem <> nil) then begin
    Site := ListItem.Data;
    Site.IsDeleted := True;
    PopulateListDeckSites();
    SetDeckChanged(True);
  end;
end;


procedure TMainForm.AnySiteListDblClick(Sender: TObject);
var ListItem: TListItem;
    Site: TSite;
    PreviousActivePage: TTabSheet;
begin
  if (Sender is TListView) then begin
    ListItem := TListView(Sender).ItemFocused;
    if (ListItem <> nil) then begin
      if (Sender = ListDeckSites) then
        Site := TDeckSite(ListItem.Data).Site
      else
        Site := ListItem.Data;
      PreviousActivePage := PageControl.ActivePage;
      PageControl.ActivePage := PageSiteEditor;
      if (Site <> CurrentSite) then begin
        if (GetCheckSavingResult(False, True, False) <> mrCancel) then begin
          CurrentSite := Site;
          LoadSite(CurrentSite);
        end else
          PageControl.ActivePage := PreviousActivePage;
      end;
    end;
  end;
end;


procedure TMainForm.AnySiteListColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if (Sender = ListDeckAllSites) then begin
    if (Column.Caption = ListDeckAllSitesSortInfo.SortColumn) then
      ListDeckAllSitesSortInfo.IsSortReversed := not ListDeckAllSitesSortInfo.IsSortReversed else
    ListDeckAllSitesSortInfo.SortColumn := Column.Caption;
    ListDeckAllSites.CustomSort(nil, 0);
  end else if (Sender = ListDeckSites) then begin
    if (Column.Caption = ListDeckSitesSortInfo.SortColumn) then
      ListDeckSitesSortInfo.IsSortReversed := not ListDeckSitesSortInfo.IsSortReversed else
    ListDeckSitesSortInfo.SortColumn := Column.Caption;
    ListDeckSites.CustomSort(nil, 0);
  end;
end;


procedure TMainForm.AnySiteListCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var Site1, Site2: TSite;
    Field1, Field2: String;
    IsNumberSort: Boolean;
    SortInfo: TSortInfo;
begin
  if (Sender = ListDeckSites) then
    SortInfo := ListDeckSitesSortInfo
  else if (Sender = ListDeckAllSites) then
    SortInfo := ListDeckAllSitesSortInfo
  else begin
    SortInfo := nil;
    MessageDlg('Fatal error: the sender ' + Sender.ClassName + ' is an unknown list.', mtError, [mbOk], 0);
  end;
  IsNumberSort := False;
  if (Sender = ListDeckSites) then begin
    Site1 := TDeckSite(Item1.Data).Site;
    Site2 := TDeckSite(Item2.Data).Site;
  end else begin
    Site1 := Item1.Data;
    Site2 := Item2.Data;
  end;
  if (SortInfo.SortColumn = SITE_LIST_COLUMN_TITLE) then begin
    Field1 := Site1.Title;
    Field2 := Site2.Title;
  end else if (SortInfo.SortColumn = SITE_LIST_COLUMN_TWILIGHT_COST) then begin
    Field1 := IntToStr(Site1.TwilightCost);
    Field2 := IntToStr(Site2.TwilightCost);
    IsNumberSort := True;
  end else if (SortInfo.SortColumn = SITE_LIST_COLUMN_TIME) then begin
    Field1 := Site1.Time;
    Field2 := Site2.Time;
  end else begin
    raise Exception.Create('Error: column "' + SortInfo.SortColumn + '" is unknown.');
  end;
  if (IsNumberSort) then begin
    if (StrToInt(Field1) > StrToInt(Field2)) then
      Compare := 1
    else if (StrToInt(Field1) < StrToInt(Field2)) then
      Compare := -1
    else
      Compare := 0;
  end else
    Compare := StrIComp(PChar(Field1), PChar(Field2));
  // If equal, sort by title next
  if ((Compare = 0) and (SortInfo.SortColumn <> SITE_LIST_COLUMN_TITLE)) then begin
    Field1 := Site1.Title;
    Field2 := Site2.Title;
    Compare := StrIComp(PChar(Field1), PChar(Field2));
  end;
  if (SortInfo.IsSortReversed) then
    Compare := 0 - Compare;
end;


procedure TMainForm.ButtonSaveSitePictureClick(Sender: TObject);
var
  Filename: String;
  Bitmap: TBitmap;
begin
   SaveImageDialog.Filename := 'site ' + CurrentSite.GetDisplayName();
   if (SaveImageDialog.Execute()) then begin
     Application.ProcessMessages();
     Filename := SaveImageDialog.Filename;
     Bitmap := Imager.CaptureArea(SiteImageArea);
     Imager.SavePictureToDisk(Bitmap, Filename);
     SetStatusText(Format('Saved image <a href="file://%s">%s</a>.', [StringReplace(Filename, '"', '%22', [rfReplaceAll]), ExtractFileName(Filename)]), 0, 0);
   end;
end;




procedure TMainForm.ListCardsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var ListItem: TListItem;
    Temp: String;
begin
  if ((Change = ctState) and (Sender is TListView)) then begin
    ListItem := TListView(Sender).ItemFocused;
    if (ListItem <> nil) then begin
      if (Sender = ListDeckCards) then
        PreviewedCard := TDeckCard(ListItem.Data).Card
      else
        PreviewedCard := ListItem.Data;
      if (PreviewedCard.Thumbnail <> nil) then begin
        CardThumbnail.Picture.Assign(PreviewedCard.Thumbnail.Graphic);
      end else begin
        CardThumbnail.Picture.Assign(TransparentPixel);
      end;
      Temp := AnsiUpperCase(PreviewedCard.Title);
      if (PreviewedCard.IsUnique) then Temp := UNIQUE_SYMBOL + ' ' + Temp;
      PreviewTitle.Caption := Temp;
      PreviewSubtitle.Caption := PreviewedCard.Subtitle;
      PreviewText.HTMLText := StringReplace(PreviewedCard.Text, NEW_LINE, '<br>', [rfReplaceAll]);
      PreviewComment.HTMLText := StringReplace(PreviewedCard.Comment, NEW_LINE, '<br>', [rfReplaceAll]);
      PreviewText.Refresh();
      PreviewComment.Top := PreviewText.Top + PreviewText.Height - 2;
      PreviewComment.Refresh();
    end else begin
      CardThumbnail.Picture.Assign(TransparentPixel);
      PreviewTitle.Caption := '';
      PreviewSubtitle.Caption := '';
      PreviewText.HTMLText := '';
      PreviewComment.HTMLText := '';
      PreviewText.Refresh();
    end;
  end;
end;


procedure TMainForm.CardThumbnailClick(Sender: TObject);
var PreviousActivePage: TTabSheet;
begin
  if (PreviewedCard <> nil) then begin
    PreviousActivePage := PageControl.ActivePage;
    PageControl.ActivePage := PageCardEditor;
    if (PreviewedCard <> CurrentCard) then begin
      if (GetCheckSavingResult(True, False, False) <> mrCancel) then begin
        CurrentCard := PreviewedCard;
        LoadCard(CurrentCard);
      end else
        PageControl.ActivePage := PreviousActivePage;
    end;
  end;
end;


procedure TMainForm.ButtonChooseDBClick(Sender: TObject);
var
  Filename: String;
  I, J: Integer;
  DataLists: TList;
  DB: TPersistence;
begin
  if (GetCheckSavingResult(True, True, True) <> mrCancel) then begin
    OpenDatabaseDialog.Filename := '';
    if (OpenDatabaseDialog.Execute()) then begin
      Filename := OpenDatabaseDialog.Filename;
      if not FileExists(Filename) then begin
        if (MessageDlg(Format('File "%s" does not exist. Do you want to create a new database?', [Filename]), mtWarning, [mbYes, mbNo], 0) <> mrYes) then begin
          Filename := '';
        end;
      end else if (Filename = DBFilename) and (Database <> nil) then begin
        MessageDlg(Format('Database "%s" is already open.', [Filename]), mtInformation, [mbOK], 0);
        Filename := '';
      end;
      if Filename <> '' then begin
        Application.ProcessMessages(); // Allow dialog time to close
        // Try to open the database and on success, load its content,
        // otherwise continue as before.
        try
          DB := TPersistence.Create(Filename);
        finally
        end;
        if (DB <> nil) then begin
          // Free old data
          DataLists := TList.Create();
          DataLists.Add(Cards); DataLists.Add(Sites); DataLists.Add(Decks);
          DataLists.Add(Races); DataLists.Add(CardTypes);
          for I := 0 to DataLists.Count - 1 do begin
            if (DataLists.Items[I] <> nil) then begin
              for J := 0 to TList(DataLists.Items[I]).Count - 1 do begin
                TDataClass(TList(DataLists.Items[I]).Items[J]).Free();
              end;
              TList(DataLists.Items[I]).Clear();
            end;
          end;
          DataLists.Free();
          FreeAndNil(Cards);
          FreeAndNil(Sites);
          FreeAndNil(Decks);
          FreeAndNil(Races);
          FreeAndNil(CardTypes);
          FreeAndNil(Database);
          FreeAndNil(Times);
          Settings := nil; // Must not be freed
          ComboCardListMap := nil; // Must not be freed
          ComboListDecksMap := nil; // Must not be freed
          ComboSiteListMap := nil; // Must not be freed
          ButtonSaveCardImages.Enabled := False;
          ButtonSaveSiteImages.Enabled := False;
          LoadDatabase(DB);
        end;
      end;
    end;
  end;
end;


// Loads a picture for the current card or site, either file file or URL
procedure TMainForm.LoadItemPicture(Item: TPlayable; Location, LocationType: String);
var
  Picture: TPicture;
  TargetImage: TImage;
  TargetEdit: TEdit;
  TitleEdit: TEdit;
  ResizeRatioX, ResizeRatioY: Single;
  MaxWidth, MaxHeight: Integer;
begin
  Picture := nil;
  if AnsiIndexText(LocationType, ['URL', 'file']) = -1 then
    raise Exception('Unknown location type "' + LocationType + '" for site picture "' + Location + '"');

  try
    if LocationType = 'URL' then
      Picture := Imager.LoadPictureFromURL(Location)
    else
      Picture := Imager.LoadPictureFromDisk(Location);
  except on E: Exception do begin
    MessageDlg(E.Message, mtWarning, [mbOK], 0);
    end
  end;
  if Picture <> nil then begin
     if Item.OriginalContentPicture = nil then begin
       Item.OriginalContentPicture := Item.ContentPicture;
     end else begin
       FreeAndNil(Item.ContentPicture);
     end;
     if Item is TCard then begin
       TargetImage := ContentPicture;
       TargetEdit := EditPictureFilename;
       TitleEdit := EditTitle;
       MaxWidth := CurrentCard.CardType.ContentPictureMaxWidth;
       MaxHeight := CurrentCard.CardType.ContentPictureMaxHeight;
       SetCardChanged(True);
     end else begin
       TargetImage := SiteContentPicture;
       TargetEdit := EditSitePictureFilename;
       TitleEdit := EditSiteTitle;
       MaxWidth := GetSettingInt('SiteContentPictureMaxWidth');
       MaxHeight := GetSettingInt('SiteContentPictureMaxHeight');
       SetSiteChanged(True);
     end;
     Item.ContentPicture := Picture;
     Item.IsContentPictureChanged := True;
     Item.PictureFileWidth := Picture.Width;
     Item.PictureFileHeight := Picture.Height;
     TargetEdit.Text := UrlDecode(ExtractFileNameBetter(Location));
     SetStatusText(Format('Loaded a %dx%d picture "%s" for "%s".',
       [Picture.Width, Picture.Height, TargetEdit.Text,
        TitleEdit.Text]), 0, 0);

     TargetImage.Picture.Assign(Item.ContentPicture);
     ResizeRatioX := 1;
     ResizeRatioY := 1;
     if Picture.Width <> MaxWidth then begin
       ResizeRatioX := Picture.Width / MaxWidth;
       TargetImage.Picture.Bitmap.Width := MaxWidth;
     end;
     if Picture.Height <> MaxHeight then begin
       ResizeRatioY := Picture.Height / MaxHeight;
       TargetImage.Picture.Bitmap.Height := MaxHeight;
     end;
     if (ResizeRatioX <> 1) or (ResizeRatioY <> 1) then begin
       Imager.QueueResize(Picture.Bitmap, Ceil(Picture.Width / Min(ResizeRatioX, ResizeRatioY)),
                          Ceil(Picture.Height / Min(ResizeRatioX, ResizeRatioY)),
                          Item, rsContent, ResizeCallback);
     end;
  end else begin
    MessageDlg('Failed to load a picture from ' + LocationType + ' "' + Location + '".', mtWarning, [mbOK], 0);
  end;
end;


procedure TMainForm.OpenPictureDialogButtonClick(Sender: TObject);
begin
   OpenPictureDialog.Filename := '';
   if (OpenPictureDialog.Execute()) then begin
     Application.ProcessMessages(); // Allow dialog time to close
     LoadItemPicture(CurrentCard, OpenPictureDialog.Filename, 'file');
   end;
end;


procedure TMainForm.OpenPictureURLDialogButtonClick(Sender: TObject);
var
  URL: String;
begin
  if Clipboard.HasFormat(CF_TEXT) then URL := Clipboard.AsText;
  if InputQuery('Enter a URL to load picture from', 'Picture URL:', URL) then begin
     Application.ProcessMessages(); // Allow dialog time to close
     LoadItemPicture(CurrentCard, URL, 'URL');
   end;
end;


procedure TMainForm.OpenSitePictureButtonClick(Sender: TObject);
begin
   OpenPictureDialog.Filename := '';
   if (OpenPictureDialog.Execute()) then begin
     Application.ProcessMessages(); // Allow dialog time to close
     LoadItemPicture(CurrentSite, OpenPictureDialog.Filename, 'file');
   end;
end;


procedure TMainForm.OpenSitePictureURLDialogButtonClick(Sender: TObject);
var
  URL: String;
begin
  if Clipboard.HasFormat(CF_TEXT) then URL := Clipboard.AsText;
  if InputQuery('Enter a URL to load picture from', 'Picture URL:', URL) then begin
     Application.ProcessMessages(); // Allow dialog time to close
     LoadItemPicture(CurrentSite, URL, 'URL');
  end;
end;


// Updates the database information (name, size, cards count) on first page
procedure TMainForm.UpdateDatabaseInfo();
var
  FellowshipRaces, ShadowRaces, I: Integer;
begin
  FellowshipRaces := 0;
  ShadowRaces := 0;
  if (Database = nil) then begin
    InfoDatabaseNameLabel.Caption := '';
    InfoDatabaseSizeLabel.Caption := '';
    SetStatusText('', 1, 0);
  end else begin
    InfoDatabaseNameLabel.Caption := Database.Filename;
    InfoDatabaseSizeLabel.Caption := FormatByteSize(Database.GetSize());
    SetStatusText('File: ' + ExtractFileName(Database.Filename), 1, 0);
  end;
  if (Cards = nil) then InfoDatabaseCardsLabel.Caption := ''
    else InfoDatabaseCardsLabel.Caption := IntToStr(Cards.Count);
  if (Sites = nil) then InfoDatabaseSitesLabel.Caption := ''
    else InfoDatabaseSitesLabel.Caption := IntToStr(Sites.Count);
  if (Decks = nil) then begin
    InfoDatabaseDecksLabel.Caption := ''
  end else begin
    if not (StatusTimer.Enabled) then
      // Update database status text only if the current text is not temporary
      SetStatusText(Format('Database contents: %d race%s, %d card%s, %d site%s, %d deck%s.',
        [Races.Count, Plural(Races.Count), Cards.Count, Plural(Cards.Count),
         Sites.Count, Plural(Sites.Count), Decks.Count, Plural(Decks.Count)]), 0, 0);
    InfoDatabaseDecksLabel.Caption := IntToStr(Decks.Count);
  end;
  InfoDatabaseCreatedLabel.Caption := GetSetting('DatabaseCreated');
  InfoDatabaseModifiedLabel.Caption := GetSetting('DatabaseModified');
  InfoDatabaseRacesLabel.Caption := '';
  if Races <> nil then begin
    for I := 0 to Races.Count - 1 do begin
      if TRace(Races.Items[I]).IsGood then
        Inc(FellowshipRaces)
      else
        Inc(ShadowRaces);
    end;
    InfoDatabaseRacesLabel.Caption := Format('%d Fellowship and %d Shadow races', [FellowshipRaces, ShadowRaces]);
  end;

  ButtonSaveCardImages.Enabled := (Cards <> nil) and (Cards.Count > 0);
  ButtonSaveSiteImages.Enabled := (Sites <> nil) and (Sites.Count > 0);
  Application.ProcessMessages();
end;



// Creates the new card dialog form
procedure TMainForm.CreateNewCardDialog();
var LabelComboRace: TLabel;
    LabelComboCardType: TLabel;
    I: Integer;
begin
  NewCardDialog := CreateMessageDialog('', mtCustom, [mbOK, mbCancel]);
  NewCardDialog.Height := 149; NewCardDialog.Width := 228;
  NewCardDialog.Caption := 'Choose card type';
  for I := 0 to NewCardDialog.ControlCount - 1 do begin
    // Move the buttons, as there are additional components coming
    if NewCardDialog.Controls[I] is TButton then begin
      NewCardDialog.Controls[I].Top := NewCardDialog.Controls[I].Top + 40; 
      NewCardDialog.Controls[I].Left := NewCardDialog.Controls[I].Left + 40; 
    end;
  end;

  NewCardComboRace := TComboBox.Create(NewCardDialog);
  NewCardComboRace.Parent := NewCardDialog;
  NewCardComboRace.ItemIndex := 0; NewCardComboRace.Style := csDropDownList;
  NewCardComboRace.Left := 88; NewCardComboRace.Top := 16; NewCardComboRace.Width := 100;

  LabelComboRace := TLabel.Create(NewCardDialog);
  LabelComboRace.Parent := NewCardDialog;
  LabelComboRace.Caption := '&Race:'; LabelComboRace.FocusControl := NewCardComboRace;
  LabelComboRace.Left := 32; LabelComboRace.Top := 16;

  NewCardComboCardType := TComboBox.Create(NewCardDialog);
  NewCardComboCardType.Parent := NewCardDialog;
  NewCardComboCardType.ItemIndex := 0; NewCardComboCardType.Style := csDropDownList;
  NewCardComboCardType.Left := 88; NewCardComboCardType.Top := 45; NewCardComboCardType.Width := 100;

  LabelComboCardType := TLabel.Create(NewCardDialog);
  LabelComboCardType.Parent := NewCardDialog;
  LabelComboCardType.Caption := 'Card &type:'; LabelComboCardType.FocusControl := NewCardComboCardType;
  LabelComboCardType.Left := 32; LabelComboCardType.Top := 48;
end;


procedure TMainForm.LOTWCardsPictureClick(Sender: TObject);
begin
  PageControl.ActivePage := PageCardList;
  if (ListCards.Selected = nil) and (ListCards.Items.Count > 0) then begin
    ListCards.Selected := ListCards.Items[0];
    ListCards.ItemFocused := ListCards.Items[0];
    ListCards.SetFocus();
  end;
end;


procedure TMainForm.LOTWSitesPictureClick(Sender: TObject);
begin
  PageControl.ActivePage := PageSiteEditor;
  ComboSiteList.SetFocus();
end;


procedure TMainForm.LOTWDecksPictureClick(Sender: TObject);
begin
  PageControl.ActivePage := PageDeckEditor;
  ComboDeckList.SetFocus();
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(CardLoadSection);
  FreeAndNil(ListCardsSortInfo);
  FreeAndNil(ListDecksSortInfo);
  FreeAndNil(ListDeckAllCardsSortInfo);
  FreeAndNil(ListDeckCardsSortInfo);
  FreeAndNil(ListDeckSitesSortInfo);
  FreeAndNil(ListDeckAllSitesSortInfo);
  FreeAndNil(Imager);
  FreeAndNil(TransparentPixel);
end;



procedure TMainForm.ButtonSaveCardImagesClick(Sender: TObject);
begin
  SaveSitesAndCards('cards');
end;


// Saves all global cards, all global sites, or all current deck cards and sites,
// depending on Target ('cards'/'sites'/'deck').
procedure TMainForm.SaveSitesAndCards(Target: String);
const
  BASE_MESSAGE =
    NEW_LINE + NEW_LINE +
    'The program will be unresponsive for a few seconds. ' +
    'Existing files will be overwritten. Proceed?';
var
  I, CardsCount, SitesCount: Integer;
  Directory: String;
  Message1, Message2, StatusMessage: String;
  DirectoryLink: String;
  TargetType: String;
  Items: TList;
  DeckSite: TDeckSite;
  DeckCard: TDeckCard;
  ActiveControl: TWinControl;
begin
    ActiveControl := MainForm.ActiveControl;
    if (GetCheckSavingResult(Pos('cards', Target) > 0, Pos('sites', Target) > 0, False) <> mrCancel) then begin
    if (LastExportDirectory = '') then LastExportDirectory := GetCurrentDir();
    if (Target = 'cards') then
      TargetType := 'card'
    else if (Target = 'sites') then
      TargetType := 'site'
    else
      TargetType := 'deck';
    Directory := BrowseForFolder(Format('Select a folder where to save %s images:', [TargetType]), LastExportDirectory, True);
    if Directory <> '' then begin
      DirectoryLink := Format('<a href="file://%s">%s</a>', [StringReplace(Directory, '"', '%22', [rfReplaceAll]), ExtractFileName(Directory)]);
      if (Target = 'cards') then begin
        Items := Cards;
        Message1 := Format('Saving %d %s to folder "%s".%s', [Items.Count, Target, Directory, BASE_MESSAGE]);
        Message2 := Format('Saved %d %s to folder %s.', [Items.Count, Target, Directory]);
        StatusMessage := Format('Saved %d %s to folder %s.', [Items.Count, Target, DirectoryLink]);
      end else if (Target = 'sites') then begin
        Items := Sites;
        Message1 := Format('Saving %d %s to folder "%s".%s', [Items.Count, Target, Directory, BASE_MESSAGE]);
        Message2 := Format('Saved %d %s to folder "%s".', [Items.Count, Target, Directory]);
        StatusMessage := Format('Saved %d %s to folder %s.', [Items.Count, Target, DirectoryLink]);
      end else begin
        Items := TList.Create();
        CardsCount := 0;
        SitesCount := 0;
        for I := 0 to CurrentDeck.Cards.Count - 1 do begin
          DeckCard := CurrentDeck.Cards.Items[I];
          if not (DeckCard.IsDeleted) then begin
            Items.Add(DeckCard.Card);
            Inc(CardsCount);
          end;
        end;
        for I := 0 to CurrentDeck.Sites.Count - 1 do begin
          DeckSite := CurrentDeck.Sites.Items[I];
          if not DeckSite.IsDeleted then begin
            Items.Add(DeckSite.Site);
            Inc(SitesCount);
          end;
        end;
        Message1 := Format('Saving deck "%s" to folder "%s" (%d cards and %d sites).%s', [CurrentDeck.Title, Directory, CardsCount, SitesCount, BASE_MESSAGE]);
        Message2 := Format('Saved deck "%s" to folder "%s" (%d cards and %d sites).', [CurrentDeck.Title, Directory, CardsCount, SitesCount]);
        StatusMessage := Format('Saved deck "%s" to folder %s (%d cards and %d sites).', [CurrentDeck.Title, DirectoryLink, CardsCount, SitesCount]);
      end;
      LastExportDirectory := Directory;
      if (mrCancel <> MessageDlg(Message1, mtInformation, [mbOk, mbCancel], 0)) then begin
        BatchSaveImages(Items, Directory);
        if (Items.Count > 0) then begin
          BringApplicationToTop(True);
          SetStatusText(StatusMessage, 0, 0);
          MessageDlg(Message2, mtInformation, [mbOk], 0);
        end;
      end;
      if (TargetType = 'deck') then Items.Free();
    end;
  end;
  MainForm.ActiveControl := ActiveControl;
end;





procedure TMainForm.ButtonSaveSiteImagesClick(Sender: TObject);
begin
  SaveSitesAndCards('sites');
end;


procedure TMainForm.ButtonSaveDeckImagesClick(Sender: TObject);
begin
  SaveSitesAndCards('deck');
end;

// Batch saves the images of all specified cards or items. Duplicate filenames
// get numbered, e.g. "card Na zdorovje! (Fellowship event) (1).png"
function TMainForm.BatchSaveImages(Items: TList; Directory: String): Integer;
var
  I: Integer;
  PreviousCurrentCard: TCard;
  PreviousCurrentSite: TSite;
  Item: TPlayable;
  ActivePage, PreviousActivePage: TTabSheet;
  Filename: String;
  Bitmap: TBitmap;
  DuplicateFilenames: IStrStrMap;
  UniqueCounter: Integer;
  PreviewArea: TRect;
begin
  Result := 0;
  if (Items.Count > 0) then begin
    ProgressBar.Visible := True;
    StatusBar.Panels[1].Style := psOwnerDraw;
    PreviousActivePage := PageControl.ActivePage;
    PageControl.Enabled := False;
    PreviousCurrentCard := CurrentCard;
    PreviousCurrentSite := CurrentSite;
    DuplicateFilenames := TStrStrHashMap.Create();
    // Mark duplicate filenames
    for I := 0 to Items.Count - 1 do begin
      Item := Items.Items[I];
      Filename := Format('%s\%s %s.png', [Directory, IfThen(Item is TCard, 'card', 'site'), Item.GetDisplayName()]);
      DuplicateFilenames.PutValue(Filename,
        IfThen(DuplicateFileNames.ContainsKey(Filename), 'duplicate', 'single'));
    end;
    for I := 0 to Items.Count - 1 do begin
      Item := Items.Items[I];
      if (Item is TCard) then begin
        CurrentCard := Item as TCard;
        PreviewArea := CardImageArea;
        ActivePage := PageCardEditor;
      end else begin
        CurrentSite := Item as TSite;
        PreviewArea := SiteImageArea;
        ActivePage := PageSiteEditor;
      end;
      SetStatusText(Format('Saving image %d of %d.', [Result + 1, Items.Count]));
      ProgressBar.Position := Floor((Result + 1) / Items.Count * ProgressBar.Max);

      PageControl.ActivePage := ActivePage;
      if (Item is TCard) then LoadCard(CurrentCard) else LoadSite(CurrentSite);
      Filename := Format('%s\%s %s.png', [Directory, IfThen(Item is TCard, 'card', 'site'), Item.GetDisplayName()]);
      if (DuplicateFilenames.GetValue(Filename) = 'duplicate') then begin
        UniqueCounter := 0;
        while (DuplicateFilenames.ContainsKey(Filename)) do begin
          // There can be duplicates which should be saved under different names
          Inc(UniqueCounter);
          Filename := Format('%s\%s %s (%d).png', [Directory, IfThen(Item is TCard, 'card', 'site'), Item.GetDisplayName(), UniqueCounter]);
        end;
        DuplicateFilenames.PutValue(Filename, ' ');
      end;
      Bitmap := Imager.CaptureArea(PreviewArea);
      Imager.SavePictureToDisk(Bitmap, Filename);
      Bitmap.Free();
      Inc(Result);
    end;
    CurrentCard := PreviousCurrentCard;
    if (PreviousCurrentCard = nil) then begin
      ComboCardList.ItemIndex := -1;
      ResetCardEditor(False);
    end else begin
      LoadCard(CurrentCard);
    end;
    CurrentSite := PreviousCurrentSite;
    if (PreviousCurrentSite = nil) then begin
      ComboSiteList.ItemIndex := -1;
      ResetSiteEditor(False);
    end else begin
      LoadSite(CurrentSite);
    end;
    PageControl.ActivePage := PreviousActivePage;
    PageControl.Enabled := True;
    ProgressBar.Visible := False;
    StatusBar.Panels[1].Style := psText;
  end;
end;


procedure TMainForm.ComboDeckListChange(Sender: TObject);
var
  Deck: TDeck;
  MainFormEnabled: Boolean;
begin
  MainFormEnabled := MainForm.Enabled;
  try
    MainForm.Enabled := False;
    Deck := ComboListDecksMap.GetValue(IntToStr(ComboDeckList.ItemIndex)) as TDeck;
    if ((Deck <> CurrentDeck) and (GetCheckSavingResult(False, False, True) <> mrCancel)) then begin
      CurrentDeck := Deck;
      LoadDeck(Deck);
    end;
  finally
    MainForm.Enabled := MainFormEnabled;
  end;
end;


procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not (IsMainFormActivated) then begin
    // On first activation, allow UI to refresh and load/create default database
    IsMainFormActivated := True;
    Application.ProcessMessages();
    LoadDatabase(TPersistence.Create(ExtractFilePath(Application.ExeName) + DEFAULT_DB_FILENAME));
  end;
end;


// Sets a text to the specified status panel and schedules it for clearing
// after timeout (0 - no clearing).
procedure TMainForm.SetStatusText(Status: String; StatusPanel: Integer = 0; Timeout: Integer = 1500);
var PanelWidth: Integer;
begin
  if StatusPanel = 0 then
    LabelStatus.HTMLText := Status
  else begin
    StatusBar.Panels[StatusPanel].Text := Status;
    // Resize panel to fit text
    if (Status <> '') then begin
      // Add 6 for 2*(border + border gap left + border gap right)
      PanelWidth := StatusBar.Canvas.TextWidth(Status) + 6;
      PanelWidth := Max(90, Min(PanelWidth, 150));
      StatusBar.Panels[0].Width := StatusBar.Width - PanelWidth;
      StatusBar.Panels[1].Width := PanelWidth;
    end;
  end;
  if (StatusTimer.Tag = StatusPanel) then StatusTimer.Enabled := False;
  if (Status <> '') and (Timeout > 0) then begin
    StatusTimer.Enabled := False;
    StatusTimer.Interval := Timeout;
    // Use the tag to send information to timer on which panel to clear
    StatusTimer.Tag := StatusPanel;
    StatusTimer.Enabled := True;
  end;
end;


procedure TMainForm.StatusTimerTimer(Sender: TObject);
begin
  if (StatusTimer.Tag = 0) then
    LabelStatus.HTMLText := ''
  else
    StatusBar.Panels[StatusTimer.Tag].Text := '';
  StatusTimer.Tag := 0;
  StatusTimer.Enabled := False;
end;


procedure TMainForm.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  if ((Panel = StatusBar.Panels[1]) and (ProgressBar.Visible)) then begin
    ProgressBar.Top := Rect.Top - 1;
    ProgressBar.Left := Rect.Left - 1;
    ProgressBar.Width := Rect.Right - Rect.Left + 2;
    ProgressBar.Height := Rect.Bottom - Rect.Top + 2;
  end else if (Panel = StatusBar.Panels[0]) then begin
    LabelStatus.Top := Rect.Top;
    LabelStatus.Left := Rect.Left;
  end;
end;
 

end.

