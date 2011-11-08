(**
 * Application main form and all interface logic.
 *
 * @author    Erki Suurjaak
 * @created   12.20.2003
 * @modified  08.11.2011
 *)
unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Globals, Mask, DataClasses,
  Menus, PDJRotoLabel, Buttons, UHTMLabel, htmlabel,
  ExtDlgs, Imaging, Persistence;

type
  TMainForm = class(TForm)
    PageControl: TPageControl;
    PageDeckEditor: TTabSheet;
    PageCardEditor: TTabSheet;
    PageSiteEditor: TTabSheet;
    BackgroundPicture: TImage;
    ContentPicture: TImage;
    Strength: TLabel;
    Health: TLabel;
    TwilightCost: TLabel;
    MiddleTitle: TLabel;
    Title: TLabel;
    Subtitle: TLabel;
    ButtonSavePicture: TButton;
    Copyright: TLabel;
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
    Viewcard1: TMenuItem;
    Removecardfromdeck1: TMenuItem;
    ButtonRemoveCardFromDeck: TButton;
    ButtonAddCardToDeck: TButton;
    VerticalTitle: TPDJRotoLabel;
    Time: TLabel;
    PanelCardCover: TPanel;
    StrengthPicture: TImage;
    HealthPicture: TImage;
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
    ListDecks: TListView;
    ButtonUndoCardChanges: TBitBtn;
    ButtonDeleteCard: TBitBtn;
    ButtonSaveCard: TBitBtn;
    ButtonNewCard: TBitBtn;
    ButtonPreviewCard: TBitBtn;
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
    Label1: TLabel;
    ComboSiteTimes: TComboBox;
    EditSiteTwilightCost: TEdit;
    LabelEditSiteTwilightCost: TLabel;
    EditSiteText: TMemo;
    LabelEditSiteText: TLabel;
    Label2: TLabel;
    ComboSiteList: TComboBox;
    EditSitePictureFilename: TEdit;
    ComboSiteDirections: TComboBox;
    Label3: TLabel;
    ButtonNewSite: TBitBtn;
    ButtonPreviewSite: TBitBtn;
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
    Text: TUniHTMLabel;
    Comment: TUniHTMLabel;
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
    VerticalTitleLine1: TPDJRotoLabel;
    VerticalTitleLine2: TPDJRotoLabel;
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
    LabelDeckStudClubStats: TLabel;
    LabelDeckStats: TLabel;
    DeckTimeOfCreation: TLabel;
    EditDeckComment: TMemo;
    EditDeckTitle: TEdit;
    LabelEditDeckTitle: TLabel;
    LabelEditDeckComment: TLabel;
    PanelCardCoverLeft: TPanel;
    PanelCardCoverBottom: TPanel;
    PanelSiteBottomCover: TPanel;
    PanelSiteLeftCover: TPanel;
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
    procedure ButtonSavePictureClick(Sender: TObject);
    procedure ButtonPreviewCardClick(Sender: TObject);
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
    procedure ButtonFunctionNotAvailableClick(Sender: TObject);
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
    procedure Viewcard1Click(Sender: TObject);
    procedure ListDecksDblClick(Sender: TObject);
    procedure ListDecksKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboSiteListChange(Sender: TObject);
    procedure ButtonNewSiteClick(Sender: TObject);
    procedure ButtonPreviewSiteClick(Sender: TObject);
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
    procedure FormShow(Sender: TObject);
    procedure LOTWCardsPictureClick(Sender: TObject);
    procedure LOTWSitesPictureClick(Sender: TObject);
    procedure LOTWDecksPictureClick(Sender: TObject);
    procedure ListDecksCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListDecksColumnClick(Sender: TObject; Column: TListColumn);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonSaveCardImagesClick(Sender: TObject);
    procedure ButtonSaveSiteImagesClick(Sender: TObject);
  private
    // Cleans the data fields in the card editor that correspond to the ID
    procedure ClearFields(WinControl: TWinControl; ID: Integer);
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
    // Checks whether the current card, deck and site have changed,
    // and asks the user whether to save or not or to cancel the operation.
    // If the user chooses to save, saves the appropriate item.
    // Returns mrCancel if the user chose to cancel either operation.
    function GetCheckSavingResult(CheckCard, CheckDeck, CheckSite: Boolean): Integer;
    // Resets the deck editor interface, disabling controls etc
    procedure ResetDeckEditor();
    // Resets the card editor interface
    procedure ResetCardEditor();
    // Resets the site editor interface
    procedure ResetSiteEditor();
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
    // Returns the screen image of the current card
    // Returns the screen image of the current site
    function GetCurrentCardImage(): TBitmap;
    function GetCurrentSiteImage(): TBitmap;
    // Function passed to resizer threads as callback, will set the resized
    // picture to the appropriate item and UI attributes
    procedure ResizeCallback(Resized: TBitmap; ResizeType: TResizeType; Item: TDataClass);
    // Loads a picture for the current card, either file file or URL
    procedure LoadCardPicture(Location, LocationType: String);
    // Loads a picture for the current site, either file file or URL
    procedure LoadSitePicture(Location, LocationType: String);
    // Updates the database information (name, size, cards count) on first page
    procedure UpdateDatabaseInfo();
    // Loads the specified database in the application
    procedure LoadDatabase(DB: TPersistence);
    // Creates the new card dialog form
    procedure CreateNewCardDialog();
  end;

  TListSortCompare = function (Item1, Item2: Pointer): Integer;

var
  MainForm: TMainForm;
  NewCardDialog: TForm;
  NewCardComboCardType: TComboBox;
  NewCardComboRace: TComboBox;


implementation

uses strutils, Math, Clipbrd, httpprot, SyncObjs, HashMap, BrowseForFolderU;



{$R *.DFM}


procedure TMainForm.ButtonSavePictureClick(Sender: TObject);
var Filename: String;
    Bitmap: TBitmap;
begin
   SaveImageDialog.Filename := CurrentCard.GetShowName();
   if (SaveImageDialog.Execute()) then begin
     Application.ProcessMessages(); // Let dialog disappear
     Filename := SaveImageDialog.Filename;
     Bitmap := GetCurrentCardImage();
     Imager.SavePictureToDisk(Bitmap, Filename);
   end;
end;


procedure TMainForm.ButtonPreviewCardClick(Sender: TObject);
begin
  PreviewCard(CurrentCard);
end;


procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
  Close();
end;


procedure TMainForm.FormCreate(Sender: TObject);
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
    DataLists.Add(Races);
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
    //FreeAndNil(Settings); Must not be freed, yet leaks no memory?
    //FreeAndNil(ComboCardListMap); Must not be freed, yet leaks no memory?
    //FreeAndNil(ComboListDecksMap); Must not be freed, yet leaks no memory?
    //FreeAndNil(ComboSiteListMap); Must not be freed, yet leaks no memory?
  end;
end;


// Checks whether the current card, deck and site have changed,
// and asks the user whether to save or not or to cancel the operation.
// If the user chooses to save, saves the appropriate item.
// Returns mrCancel if the user chose to cancel either operation.
function TMainForm.GetCheckSavingResult(CheckCard, CheckDeck, CheckSite: Boolean): Integer;
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
    if (CheckDeck and IsDeckChanged) then begin
      DeckResult := MessageDlg(Format('Do you want to save the changes you made to the deck "%s"?',
                                      [EditDeckTitle.Text]), mtWarning, [mbYes, mbNo, mbCancel], 0);
      if (DeckResult = mrYes) then begin
        SaveCurrentDeck()
      end else if (DeckResult = mrNo) then begin
        UndoCurrentDeckChanges();
      end;
    end;
  if ((CardResult <> mrCancel) and (DeckResult <> mrCancel)) then
    if (CheckSite and IsSiteChanged) then begin
      SiteResult := MessageDlg(Format('Do you want to save the changes you made to the site "%s"?',
                                      [EditSiteTitle.Text]), mtWarning, [mbYes, mbNo, mbCancel], 0);
      if (SiteResult = mrYes) then begin
        SetInterfaceDataToSite(CurrentSite);
        SaveCurrentSite()
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
    ResetCardEditor();
    ResetDeckEditor();
    ResetSiteEditor();
    ResetCardList();
    LoadData();
    DatabaseInfoPanel.Visible := True;
    PageDeckEditor.TabVisible := True;
    PageCardEditor.TabVisible := True;
    PageCardList.TabVisible := True;
    PageSiteEditor.TabVisible := True;

    PreviewTitle.Caption := '';
    PreviewSubtitle.Caption := '';
    PreviewText.HTMLText := '';
    PreviewComment.HTMLText := '';
    ButtonSaveCardImages.Enabled := True;
    ButtonSaveSiteImages.Enabled := True;
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
      CurrentCard.CardType := NewCardComboCardType.Items[NewCardComboCardType.ItemIndex];
      CurrentCard.Race := GetRaceByName(NewCardComboRace.Items[NewCardComboRace.ItemIndex]);
      ChangeCardEditorInterface(CurrentCard);
      SetCardDataToInterface(CurrentCard);
      PreviewCard(CurrentCard);
      SetCardChanged(False);
      ButtonSaveCard.Enabled := True;
      ButtonDeleteCard.Enabled := False;
      EditTitle.SetFocus();
    end;
  end;
end;


// Cleans the data fields in the card editor that correspond to the ID
procedure TMainForm.ClearFields(WinControl: TWinControl; ID: Integer);
var I: Integer;
    CurrentControl: TControl;
begin
  for I := 0 to WinControl.ControlCount - 1 do begin
    CurrentControl := WinControl.Controls[I];
    // If the control is a TPanel, recurse into its controls
    if (CurrentControl is TPanel) then begin
      ClearFields(CurrentControl as TWinControl, ID);
    end else if (MatchesTag(CurrentControl, ID)) then begin
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
var TempCard: TCard;
begin
  if (CurrentCard <> nil) then begin
    TempCard := TCard.Create();
    TempCard.ID := CurrentCard.ID;
    SetInterfaceDataToCard(TempCard);
    if (IsCardTitleUnique(TempCard)) then begin
      if ((Length(EditTitle.Text) > 0) and (Length(EditTwilightCost.Text) > 0)) then begin
        SetInterfaceDataToCard(CurrentCard);
        SaveCurrentCard();
      end else begin
        MessageDlg('Every card must have a title and a twilight cost.', mtError, [mbOk], 0);
      end;
    end else begin
      MessageDlg('The card''s title (and together with subtitle, if character) is not unique. ' + #13#13 +
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
    CurrentCard.OriginalCardType := '';
    FreeAndNil(CurrentCard.OriginalContentPicture);
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
  Card.CardType := ComboCardType.Items[ComboCardType.ItemIndex];
  Card.Race := GetRaceByName(ComboRace.Items[ComboRace.ItemIndex]);
  Card.Title := EditTitle.Text;
  Card.Subtitle := '';
  Card.Time := '';
  Card.PossessionType := '';
  Card.Strength := '';
  Card.Health := '';
  if (Card.CardType = CARD_TYPE_CHARACTER) then begin
    Card.Subtitle := EditSubtitle.Text;
    if (not Card.Race.IsGood) then
      Card.Time := ComboTime.Text;
  end;
  if (Card.CardType = CARD_TYPE_POSSESSION) then
    Card.PossessionType := EditPossessionType.Text;
  Card.TwilightCost := StrToInt(EditTwilightCost.Text);
  Card.IsUnique := CheckIsUnique.Checked;
  if ((Card.CardType = CARD_TYPE_POSSESSION) or (Card.CardType = CARD_TYPE_CHARACTER)) then begin
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
  ClearFields(PageCardEditor, TAG_NEW_CARD);
  SelectComboItem(ComboRace, Card.Race.Name);
  SelectComboItem(ComboCardType, Card.CardType);
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
  ButtonPreviewCard.Enabled := True;
  ButtonDeleteCard.Enabled := True;
  ButtonSaveCardAs.Enabled := True;
  ButtonSavePicture.Enabled := True;
end;



// Sets the data in the site to the site editor interface
procedure TMainForm.SetSiteDataToInterface(Site: TSite);
begin
  ClearFields(PageSiteEditor, TAG_NEW_SITE);
  EditSiteTitle.Text := Site.Title;
  EditSiteTwilightCost.Text := IntToStr(Site.TwilightCost);
  if (Length(Site.Time) > 0) then
    SelectComboItem(ComboSiteTimes, Site.Time)
  else
    SelectComboItem(ComboSiteTimes, GetSetting('SiteFirstTime'));
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
  ButtonPreviewSite.Enabled := True;
  ButtonDeleteSite.Enabled := True;
  ButtonSaveSiteAs.Enabled := True;
  ButtonSaveSitePicture.Enabled := True;
end;



// Retrieves all the initial data from the database required for working
procedure TMainForm.LoadData();
var I: Integer;
    Race: TRace;
begin
  CurrentDeck := nil;
  CurrentCard := nil;
  ComboCardListMap := TStrHashMap.Create(16, False);  // Set to not own its
  ComboListDecksMap := TStrHashMap.Create(16, False); // objects, as they are
  ComboSiteListMap := TStrHashMap.Create(16, False);  // managed elsewhere
  Times := Database.RetrieveTimes();
  Races := Database.RetrieveRaces();
  Sites := Database.RetrieveSites();
  Settings := Database.RetrieveSettings();
  
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
  for I := Low(Times) to High(Times) do begin
    ComboTime.Items.Add(Times[I]);
    ComboSiteTimes.Items.Add(Times[I]);
    ComboListTimes.Items.Add(Times[I]);
  end;
  ComboCardType.Items.Clear();
  NewCardComboCardType.Items.Clear;
  ComboListCardTypes.Items.Clear();
  ComboListCardTypes.Items.Add(CARD_LIST_FILTER_TYPES_ALL);
  ComboListCardTypes.ItemIndex := 0;
  for I := Low(CARD_TYPES) to High(CARD_TYPES) do begin
    ComboCardType.Items.Add(CARD_TYPES[I]);
    NewCardComboCardType.Items.Add(CARD_TYPES[I]);
    ComboListCardTypes.Items.Add(CARD_TYPES[I]);
  end;
  Cards := Database.RetrieveCards();
  Decks := Database.RetrieveDecks();
  CardsUpdate();
  DecksUpdate();
  PopulateListCards();
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
    ComboCardList.Items.Add(Card.GetShowName());
    ComboCardListMap.PutValue(IntToStr(I), Card);
  end;
  PopulateListCards();
  PopulateListDeckAllCards();
  PopulateListDeckCards();
  UpdateDatabaseInfo();
end;


// Updates all the variables and controls to reflect a deck change
procedure TMainForm.DecksUpdate();
var I: Integer;
    Deck: TDeck;
    ListItem, Index: TListItem;
begin
  ListDecks.Items.Clear();
  ComboListDecks.Items.Clear();
  ComboListDecksMap.Clear();
  Index := nil;
  ComboListDecks.Items.Add(' ');
  for I := 0 to Decks.Count - 1 do begin
    Deck := Decks.Items[I];
    ComboListDecks.Items.Add(Deck.GetShowName());
    ComboListDecksMap.PutValue(IntToStr(I + 1), Deck);
    ListItem := ListDecks.Items.Add();
    ListItem.Caption := Deck.Title;
    ListItem.SubItems.Add(IntToStr(Deck.GetCardsCount()));
    ListItem.SubItems.Add(Copy(Trim(Deck.Comment), 1, 15));
    ListItem.Data := Deck;
    if (Deck = CurrentDeck) then
      Index := ListItem;
  end;
  if (CurrentDeck = nil) then begin
    ListDeckCards.Items.Clear();
  end else begin
    ListDecks.Selected := Index;
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
    ComboSiteList.Items.Add(Site.GetShowName());
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


// Changes the interface for the type (shadow event, stud club possession etc)
procedure TMainForm.ChangeCardEditorInterface(var Card: TCard);
begin
  //BackgroundPicture.Picture := nil;
  StrengthPicture.Visible := False;
  HealthPicture.Visible := False;
  if (Card.CardType = CARD_TYPE_CHARACTER) then begin
    BackgroundPicture.Picture.Assign(Card.Race.CharacterPicture);
    ContentPicture.Left := 53;
    ContentPicture.Top := 86;
    MiddleTitle.Left := 64;
    MiddleTitle.Width := 257;
  end else begin
    BackgroundPicture.Picture.Assign(Card.Race.OtherPicture);
    ContentPicture.Left := 91;
    ContentPicture.Top := 72;
    MiddleTitle.Left := 100;
    MiddleTitle.Width := 241;
    if (Card.CardType = CARD_TYPE_POSSESSION) then begin
      StrengthPicture.Picture.Assign(Card.Race.StrengthPicture);
      HealthPicture.Picture.Assign(Card.Race.HealthPicture);
      StrengthPicture.Left := Card.Race.StrengthPictureLeft;
      StrengthPicture.Top := Card.Race.StrengthPictureTop;
      HealthPicture.Left := Card.Race.HealthPictureLeft;
      HealthPicture.Top := Card.Race.HealthPictureTop;
    end;
  end;
  ChangeCardEditorVisibility(PageCardEditor, Card);
end;


// Changes the visibility of the card editor interface components
procedure TMainForm.ChangeCardEditorVisibility(Control: TWinControl; var Card: TCard);
var I: Integer;
    CurrentControl: TControl;
begin
  for I := 0 to Control.ControlCount - 1 do begin
    CurrentControl := (Control.Controls[I] as TControl);
    // If the control is a TPanel, recurse into its controls
    if (CurrentControl is TPanel) then begin
      if (CurrentControl.Tag <> 0) then
        CurrentControl.Visible := MatchesTag(CurrentControl, GetCardTypeTag(Card.CardType));
      ChangeCardEditorVisibility(CurrentControl as TPanel, Card);
    end else if (CurrentControl.Tag <> 0) then begin
      CurrentControl.Visible := MatchesTag(CurrentControl, GetCardTypeTag(Card.CardType));
      if (MatchesTag(CurrentControl, TAG_SHADOW_CONTROL)) then
       if (MatchesTag(CurrentControl, GetCardTypeTag(Card.CardType))) then
         CurrentControl.Visible := not Card.Race.IsGood;
    end
  end;
end;



// Loads the specified card into the card editor
procedure TMainForm.LoadCard(var Card: TCard);
var I: Integer;
    TempCard: TCard;
    MainFormEnabled: Boolean;
begin
  MainFormEnabled := MainForm.Enabled;
  try
    IgnoreCardChanges := True;
    MainForm.Enabled := False;
    MainForm.Cursor := crHourGlass;
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
    MainForm.Enabled := MainFormEnabled;
    MainForm.Cursor := crDefault;
    PanelCardCover.Visible := False;
    IgnoreCardChanges := False;
    SetCardChanged(False);
  end;
end;


// Previews the card picture in the card editor
procedure TMainForm.PreviewCard(var Card: TCard; UpdatePicture: Boolean = True);
var Temp, TimeTemp, TitleTemp, Inter, NextWord: String;
    Screenshot: TBitmap;
    Words: TStringList;
    I: Integer;
begin
  // Always compare before replacing values, to avoid flickering
  try
    TwilightCost.Caption := EditTwilightCost.Text;
    if ((Card.CardType = CARD_TYPE_CHARACTER)
        or (Card.CardType = CARD_TYPE_POSSESSION)) then begin
      if (Strength.Caption <> EditStrength.Text) then Strength.Caption := EditStrength.Text;
      if (Health.Caption <> EditHealth.Text) then Health.Caption := EditHealth.Text;
    end else begin
      Strength.Caption := '';
      Health.Caption := '';
    end;
    TimeTemp := '';
    Temp := AnsiUpperCase(EditTitle.Text);
    if (CheckIsUnique.Checked) then Temp := UNIQUE_SYMBOL + ' ' + Temp;
    if (Card.CardType = CARD_TYPE_CHARACTER) then begin
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
    if (Card.CardType = CARD_TYPE_CHARACTER) then begin
      if (Subtitle.Caption <> AnsiUpperCase(EditSubtitle.Text)) then Subtitle.Caption := AnsiUpperCase(EditSubtitle.Text);
      if (Card.Race.IsGood) then
        Temp := AnsiUpperCase('companion')
      else
        Temp := AnsiUpperCase('minion');
      Temp := Temp + ' ' + MIDDLE_DOT + ' ' +
              AnsiUpperCase(Card.Race.Name);
    end else if (Card.CardType = CARD_TYPE_POSSESSION) then begin
      Subtitle.Caption := '';
      Temp := AnsiUpperCase(Card.CardType);
      if (Length(EditPossessionType.Text) > 0) then
        Temp := Temp + ' ' + MIDDLE_DOT + ' ' +
                AnsiUpperCase(EditPossessionType.Text);
      StrengthPicture.Visible := (Length(EditStrength.Text) > 0);
      HealthPicture.Visible := (Length(EditHealth.Text) > 0);
    end else begin
      Temp := AnsiUpperCase(Card.CardType);
    end;
    if (MiddleTitle.Caption <> Temp) then MiddleTitle.Caption := Temp;
    Temp := StringReplace(EditText.Lines.Text, NEW_LINE, '<br>', [rfReplaceAll]);
    if (Text.HTMLText <> Temp) then Text.HTMLText := Temp;
    Temp := StringReplace(EditComment.Lines.Text, NEW_LINE, '<br>', [rfReplaceAll]);
    if (Comment.HTMLText <> Temp) then Comment.HTMLText := Temp;
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
    PanelCardCover.Visible := False;
    Text.Refresh();
    Comment.Top := Text.Top + Text.Height - 2;
    Application.ProcessMessages();
  end;
  if (UpdatePicture and (IsCardChanged or (Card.Thumbnail = nil))) then begin
    Screenshot := GetCurrentCardImage();
    if Card.Thumbnail = nil then Card.Thumbnail := TPicture.Create();
    Imager.QueueResize(Screenshot, GetSettingInt('CardThumbnailWidth'),
                       GetSettingInt('CardThumbnailHeight'), Card, rsThumbnail,
                       ResizeCallback);
    Screenshot.Free();
  end;
end;


// Previews the site picture in the site editor
procedure TMainForm.PreviewSite(var Site: TSite; UpdatePicture: Boolean = True);
var Temp: String;
begin
//  PanelSitePicture.Visible := False;
  try

    if UpdatePicture then begin
      SiteContentPicture.Picture.Assign(TransparentPixel);
      if Site.ContentPicture <> nil then begin
        SiteContentPicture.Picture.Assign(Site.ContentPicture);
      end else if (Length(Site.PictureFilename) > 0) then begin
        Site.ContentPicture := Database.RetrieveContentPicture(Site);
        SiteContentPicture.Picture.Assign(Site.ContentPicture);
      end;
    end;
    SiteTitle.Caption := AnsiUpperCase(EditSiteTitle.Text);
    SiteTime.Caption := ComboSiteTimes.Text;
    if (SiteTime.Caption = GetSetting('SiteFirstTime')) then begin
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
//    PanelSitePicture.Visible := True;
  end;
end;



// Function passed to resizer threads as callback, will set the resized
// picture to the appropriate item and UI attributes
procedure TMainForm.ResizeCallback(Resized: TBitmap; ResizeType: TResizeType; Item: TDataClass);
var
  Card: TCard;
  Site: TSite;
begin
  if Item is TCard then begin
    Card := TCard(Item);
    case ResizeType of
      rsThumbnail: begin
        Card.Thumbnail.Assign(Resized);
        if (PreviewedCard = Card) then CardThumbnail.Picture.Assign(Card.Thumbnail);
      end;
      rsContent: begin
        Card.ContentPicture.Assign(Resized);
        ContentPicture.Picture.Assign(Card.ContentPicture);
      end;
    end;
  end else if Item is TSite then begin
    Site := TSite(Item);
    case ResizeType of
      rsContent: begin
        Site.ContentPicture.Assign(Resized);
        SiteContentPicture.Picture.Assign(Site.ContentPicture);
      end;
    end;
  end else begin
    raise Exception('Unsupported data class for resize: "' + Item.ClassName + '".');
  end;
end;


// Loads the specified site into the site editor
procedure TMainForm.LoadSite(var Site: TSite);
var I: Integer;
    TempSite: TSite;
    MainFormEnabled: Boolean;
begin
  MainFormEnabled := MainForm.Enabled;
  try
    IgnoreSiteChanges := True;
    PanelSitePicture.Visible := False;
    MainForm.Enabled := False;
    MainForm.Cursor := crHourGlass;
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
    MainForm.Enabled := MainFormEnabled;
    MainForm.Cursor := crDefault;
    PanelSitePicture.Visible := True;
    IgnoreSiteChanges := False;
    SetSiteChanged(False);
    Application.ProcessMessages();
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
    MainFormEnabled: Boolean;
begin
  if not IgnoreCardChanges then begin
    CardLoadSection.Enter();
    MainFormEnabled := MainForm.Enabled;
    try
      MainForm.Enabled := False;
      Card := ComboCardListMap.GetValue(IntToStr(ComboCardList.ItemIndex)) as TCard;
      if ((Card <> CurrentCard) and (GetCheckSavingResult(True, False, False) <> mrCancel)) then begin
        PageCardEditor.SetFocus();
        CurrentCard := Card;
        LoadCard(CurrentCard);
        ComboCardList.SetFocus();
      end;
    finally
      MainForm.Enabled := MainFormEnabled;
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
    CurrentCard := nil;
    CardsUpdate();
    DecksUpdate(); // To update the card counts in deck lists
    PanelCardCover.Visible := True;
    PanelCardFields.Visible := False;
    ButtonPreviewCard.Enabled := False;
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
  Temp := InputBox('Create new deck', 'Enter the title of the new deck:', '');
  if (Length(Temp) > 0) then begin
    if (GetCheckSavingResult(False, True, False) <> mrCancel) then begin
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
  PanelDeckFields.Enabled := True;
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
    CurrentDeck := nil;
    EditDeckTitle.Text := '';
    DeckTimeOfCreation.Caption := '';
    PanelDeckFields.Enabled := False;
    ButtonSaveDeck.Enabled := False;
    ButtonSaveDeckAs.Enabled := False;
    ButtonUndoDeckChanges.Enabled := False;
    ButtonDeleteDeck.Enabled := False;
    ListDeckCards.Enabled := False;
    ListDeckSites.Enabled := False;
    LabelDeckStats.Caption := '';
    LabelDeckShadowStats.Caption := '';
    LabelDeckStudClubStats.Caption := '';
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
  if (not IsDeckTitleUnique(CurrentDeck)) then
      ShowMessage('You cannot save the deck under this title, as a deck must ' +
                  'have a unique title.')
  else begin
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
    if ((CurrentDeck <> nil) and (CurrentDeck.ID = 0)) then
      ButtonSaveDeck.Enabled := True;
  end;
end;


// Fills in the deck statistics controls
procedure TMainForm.ShowDeckStats();
var I: Integer;
    ShadowCharacters, ShadowConditions, ShadowPossessions, ShadowEvents: Integer;
    StudClubCharacters, StudClubConditions, StudClubPossessions, StudClubEvents: Integer;
    ShadowCards, StudClubCards: Integer;
begin
  ShadowCharacters := 0;
  ShadowConditions := 0;
  ShadowPossessions := 0;
  ShadowEvents := 0;
  StudClubCharacters := 0;
  StudClubConditions := 0;
  StudClubPossessions := 0;
  StudClubEvents := 0;
  ShadowCards := 0;
  StudClubCards := 0;
  for I := 0 to CurrentDeck.Cards.Count - 1 do
    if (not TDeckCard(CurrentDeck.Cards.Items[I]).IsDeleted) then
      with (TDeckCard(CurrentDeck.Cards[I]).Card) do begin
        if (Race.IsGood) then begin
          Inc(StudClubCards);
          if (CardType = CARD_TYPE_CHARACTER)
            then Inc(StudClubCharacters)
          else if (CardType = CARD_TYPE_CONDITION)
            then Inc(StudClubConditions)
          else if (CardType = CARD_TYPE_EVENT)
            then Inc(StudClubEvents)
          else if (CardType = CARD_TYPE_POSSESSION)
            then Inc(StudClubPossessions);
        end else begin
          Inc(ShadowCards);
          if (CardType = CARD_TYPE_CHARACTER)
            then Inc(ShadowCharacters)
          else if (CardType = CARD_TYPE_CONDITION)
            then Inc(ShadowConditions)
          else if (CardType = CARD_TYPE_EVENT)
            then Inc(ShadowEvents)
          else if (CardType = CARD_TYPE_POSSESSION)
            then Inc(ShadowPossessions);
        end;
      end;
  LabelDeckStats.Caption :=
    Format('%d cards total.', [ShadowCards + StudClubCards]);
  LabelDeckShadowStats.Caption :=
    Format('%d Shadow cards: %s %d minions %s %d conditions %s %d possessions %s %d events',
           [ShadowCards, NEW_LINE, ShadowCharacters, NEW_LINE,
            ShadowConditions, NEW_LINE, ShadowPossessions, NEW_LINE, ShadowEvents]);
  LabelDeckStudClubStats.Caption :=
    Format('%d Fellowship cards: %s %d companions %s %d conditions %s %d possessions %s %d events',
           [StudClubCards, NEW_LINE, StudClubCharacters, NEW_LINE,
            StudClubConditions, NEW_LINE, StudClubPossessions, NEW_LINE, StudClubEvents]);
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
  PreviewCard(CurrentCard, False);
end;


procedure TMainForm.ButtonFunctionNotAvailableClick(Sender: TObject);
begin
  ShowMessage('This function not yet available, sorry!');
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
  if (Length(CurrentCard.OriginalCardType) = 0) then
    CurrentCard.OriginalCardType := CurrentCard.CardType;
  CurrentCard.CardType := ComboCardType.Items[ComboCardType.ItemIndex];
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
      ShowMessage('You cannot save the card as a new card, because its title (and ' +
                  'together with subtitle if character) is not unique.');
      NewCard.Free();
    end else begin
      EditTitle.Text := Temp;
      if (Length(CurrentCard.OriginalCardType) > 0) then
        CurrentCard.CardType := CurrentCard.OriginalCardType;
      if (CurrentCard.OriginalRace <> nil) then
        CurrentCard.Race := CurrentCard.OriginalRace;
      // Copy over content picture
      if CurrentCard.ContentPicture <> nil then begin
        NewCard.ContentPicture := TPicture.Create();
        NewCard.ContentPicture.Bitmap.Assign(CurrentCard.ContentPicture);
      end;
      if (CurrentCard.OriginalContentPicture <> nil) then begin
        // Card picture has been changed, but is being saved under a different
        // name - restore the original card's condition.
        FreeAndNil(CurrentCard.ContentPicture);
        CurrentCard.ContentPicture := CurrentCard.OriginalContentPicture;
        CurrentCard.OriginalContentPicture := nil;
      end;
      CurrentCard.OriginalCardType := '';
      CurrentCard.OriginalRace := nil;
      CurrentCard := NewCard;
      SaveCurrentCard();
      PreviewCard(CurrentCard);
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
        if (Card.CardType <> CARD_TYPE_CHARACTER) then begin
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
  if (Length(CurrentCard.OriginalCardType) > 0) then
    CurrentCard.CardType := CurrentCard.OriginalCardType;
  if (CurrentCard.OriginalRace <> nil) then
    CurrentCard.Race := CurrentCard.OriginalRace;
  if (CurrentCard.OriginalContentPicture <> nil) then begin
    CurrentCard.ContentPicture.Free();
    CurrentCard.ContentPicture := CurrentCard.OriginalContentPicture;
    ContentPicture.Picture.Assign(CurrentCard.ContentPicture);
  end;
  CurrentCard.OriginalCardType := '';
  CurrentCard.OriginalRace := nil;
  CurrentCard.OriginalContentPicture := nil;
  SetCardDataToInterface(CurrentCard);
  PreviewCard(CurrentCard);
  SetCardChanged(False);
end;


procedure TMainForm.EditDeckChange(Sender: TObject);
begin
  SetDeckChanged(True);
end;


procedure TMainForm.ButtonUndoDeckChangesClick(Sender: TObject);
begin
  UndoCurrentDeckChanges();
end;


// Undos the changes made to the current deck
procedure TMainForm.UndoCurrentDeckChanges();
var I: Integer;
    DeckCard: TDeckCard;
begin
  EditDeckTitle.Text := CurrentDeck.Title;
  EditDeckComment.Text := CurrentDeck.Comment;
  for I := 0 to CurrentDeck.Cards.Count - 1 do begin
    DeckCard := CurrentDeck.Cards.Items[I];
    if (DeckCard.ID = 0) then begin
      CurrentDeck.Cards.Items[I] := nil;
    end else begin
      DeckCard.IsDeleted := False;
    end;
  end;
  CurrentDeck.Cards.Pack();
  UpdateDeckCardsList();
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
      ShowMessage('You cannot save the deck under this title, as a deck must ' +
                  'have a unique title.')
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
procedure TMainForm.ResetCardEditor();
begin
  PanelCardCover.Visible := True;
  PanelCardFields.Visible := False;
  ComboCardList.Items.Clear();
  ButtonSaveCard.Enabled := False;
  ButtonSaveCardAs.Enabled := False;
  ButtonDeleteCard.Enabled := False;
  ButtonPreviewCard.Enabled := False;
  ButtonSavePicture.Enabled := False;
  ButtonUndoCardChanges.Enabled := False;
  ButtonClearListFilter.Enabled := False;
  IsCardChanged := False;
end;


// Resets the site editor interface
procedure TMainForm.ResetSiteEditor();
begin
  PanelSiteFields.Visible := False;
  PanelSitePicture.Visible := False;
  ComboSiteList.Items.Clear();
  ButtonSaveSite.Enabled := False;
  ButtonSaveSiteAs.Enabled := False;
  ButtonDeleteSite.Enabled := False;
  ButtonPreviewSite.Enabled := False;
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
  PanelDeckFields.Enabled := False;
  DeckTimeOfCreation.Caption := '';
  ListDecks.Items.Clear();
  ListDeckAllCards.Items.Clear();
  ListDeckAllSites.Items.Clear();
  ButtonSaveDeck.Enabled := False;
  ButtonSaveDeckAs.Enabled := False;
  ButtonDeleteDeck.Enabled := False;
  ButtonUndoDeckChanges.Enabled := False;
  LabelDeckStudClubStats.Caption := '';
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
        if (Card.CardType <> ListCardsFilterType) then
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
    ListItem.SubItems.Add(Card.CardType);
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
  if (Selected <> nil) then
    begin
    ListCards.Selected := Selected;
    ListCards.ItemFocused := Selected;
    end;
  LabelCardListStats.Caption := IntToStr(ListCards.Items.Count) + ' cards';
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
    ListItem.SubItems.Add(Card.CardType);
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
        ListItem.SubItems.Add(Card.Card.CardType);
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
    ShowMessage('Fatal error: the sender ' + Sender.ClassName + ' is an unknown list.');
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
    Field1 := Card1.CardType;
    Field2 := Card2.CardType;
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
  ListCardsFilterDeck := ComboListDecksMap.GetValue(IntToStr(ComboListDecks.ItemIndex)) as TDeck;
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


procedure TMainForm.Viewcard1Click(Sender: TObject);
begin
  AnyCardListDblClick(ListDeckCards);
end;


procedure TMainForm.ListDecksDblClick(Sender: TObject);
var ListItem: TListItem;
    Deck: TDeck;
begin
  ListItem := ListDecks.ItemFocused;
  if (ListItem <> nil) then begin
    Deck := ListItem.Data;
    if ((Deck <> nil)) then begin
      if (GetCheckSavingResult(False, True, False) <> mrCancel) then begin
        CurrentDeck := Deck;
        LoadDeck(Deck);
      end;
    end;
  end;
end;


procedure TMainForm.ListDecksKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    ListDecksDblClick(Sender);
end;



procedure TMainForm.ComboSiteListChange(Sender: TObject);
var Site: TSite;
    MainFormEnabled: Boolean;
begin
  MainFormEnabled := MainForm.Enabled;
  try
    MainForm.Enabled := False;
    Site := ComboSiteListMap.GetValue(IntToStr(ComboSiteList.ItemIndex)) as TSite;
    if ((Site <> CurrentSite) and (GetCheckSavingResult(False, False, True) <> mrCancel)) then begin
      PageSiteEditor.SetFocus();
      CurrentSite := Site;
      LoadSite(CurrentSite);
      ComboSiteList.SetFocus();
    end;
  finally
    MainForm.Enabled := MainFormEnabled;
  end;
end;


procedure TMainForm.ButtonNewSiteClick(Sender: TObject);
begin
  if (GetCheckSavingResult(False, False, True) <> mrCancel) then begin
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


procedure TMainForm.ButtonPreviewSiteClick(Sender: TObject);
begin
  PreviewSite(CurrentSite);
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
      MessageDlg('The Site''s title is not unique. ' + #13#13 +
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
      ShowMessage('You cannot save the site under the title ''' + Temp + ''', ' +
                  'as a site with that name already exists.');
      NewSite.Free();
    end else begin
      EditSiteTitle.Text := Temp;
      // Copy over content picture
      if CurrentSite.ContentPicture <> nil then begin
        NewSite.ContentPicture := TPicture.Create();
        NewSite.ContentPicture.Bitmap.Assign(CurrentSite.ContentPicture);
      end;
      if (CurrentSite.OriginalContentPicture <> nil) then begin
        // Site picture has been changed, but is being saved under a different
        // name - restore the original site's condition.
        FreeAndNil(CurrentSite.ContentPicture);
        CurrentSite.ContentPicture := CurrentSite.OriginalContentPicture;
        CurrentSite.OriginalContentPicture := nil;
      end;

      CurrentSite := NewSite;
      SaveCurrentSite();
      PreviewSite(CurrentSite);
    end;
  end;
end;



procedure TMainForm.ButtonUndoSiteChangesClick(Sender: TObject);
begin
  if (CurrentSite.OriginalContentPicture <> nil) then begin
    CurrentSite.ContentPicture.Free();
    CurrentSite.ContentPicture := CurrentSite.OriginalContentPicture;
    ContentPicture.Picture.Assign(CurrentSite.ContentPicture);
  end;
  CurrentSite.OriginalContentPicture := nil;
  SetSiteDataToInterface(CurrentSite);
  PreviewSite(CurrentSite);
  SetSiteChanged(False);
end;


procedure TMainForm.ButtonDeleteSiteClick(Sender: TObject);
var
  ItemIndex: Integer;
begin
  If (MessageDlg('Are you sure you want to delete this site?',
                 mtConfirmation, [mbOk, mbCancel], 0) = mrOk) then begin
    ItemIndex := ComboSiteList.ItemIndex;
    Database.Delete(CurrentSite);
    RemoveSiteFromVariables(CurrentSite);
    CurrentCard := nil;
    SitesUpdate();
    DecksUpdate();
    PanelSiteFields.Visible := False;
    PanelSitePicture.Visible := False;
    ButtonPreviewSite.Enabled := False;
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
  PreviewSite(CurrentSite, False);
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
        if (GetCheckSavingResult(False, False, True) <> mrCancel) then begin
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
    ShowMessage('Fatal error: the sender ' + Sender.ClassName + ' is an unknown list.');
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


// Returns the screen image of the current card
function TMainForm.GetCurrentCardImage(): TBitmap;
begin
  Result := Imager.CaptureRect(Bounds(CARD_IMAGE_LEFT, CARD_IMAGE_TOP,
                                      CARD_IMAGE_WIDTH, CARD_IMAGE_HEIGHT));
end;

// Returns the screen image of the current site
function TMainForm.GetCurrentSiteImage(): TBitmap;
begin
  Result := Imager.CaptureRect(Bounds(SITE_IMAGE_LEFT, SITE_IMAGE_TOP,
                                      SITE_IMAGE_WIDTH, SITE_IMAGE_HEIGHT));
end;


procedure TMainForm.ButtonSaveSitePictureClick(Sender: TObject);
var
  Filename: String;
  Bitmap: TBitmap;
begin
   SaveImageDialog.Filename := CurrentSite.GetShowName();
   if (SaveImageDialog.Execute()) then begin
     Application.ProcessMessages();
     Filename := SaveImageDialog.Filename;
     Bitmap := GetCurrentSiteImage();
     Imager.SavePictureToDisk(Bitmap, Filename);
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
          DataLists.Add(Races);
          for I := 0 to DataLists.Count - 1 do begin
            for J := 0 to TList(DataLists.Items[I]).Count - 1 do begin
              TDataClass(TList(DataLists.Items[I]).Items[J]).Free();
            end;
            TList(DataLists.Items[I]).Clear();
            TList(DataLists.Items[I]).Free();
          end;
          DataLists.Free();
          FreeAndNil(Database);
          ButtonSaveCardImages.Enabled := False;
          ButtonSaveSiteImages.Enabled := False;
          LoadDatabase(DB);
        end;
      end;
    end;
  end;
end;


// Loads a picture for the current card, either file file or URL
procedure TMainForm.LoadCardPicture(Location, LocationType: String);
var
  Picture: TPicture;
  ResizeRatioX, ResizeRatioY: Single;
  IsError: Boolean;
begin
  if AnsiIndexText(LocationType, ['URL', 'file']) = -1 then
    raise Exception('Unknown location type "' + LocationType + '" for card picture "' + Location + '"');
  Picture := nil;
  IsError := False;

  try
    if LocationType = 'URL' then
      Picture := Imager.LoadPictureFromURL(Location)
    else
      Picture := Imager.LoadPictureFromDisk(Location);
  except on E: Exception do begin
    IsError := True;
    MessageDlg(E.Message, mtWarning, [mbOK], 0);
    end
  end;
  if Picture <> nil then begin
     if CurrentCard.OriginalContentPicture = nil then begin
       CurrentCard.OriginalContentPicture := CurrentCard.ContentPicture;
     end else begin
       FreeAndNil(CurrentCard.ContentPicture)
     end;
     CurrentCard.ContentPicture := Picture;
     ContentPicture.Picture.Assign(CurrentCard.ContentPicture);
     ResizeRatioX := 1;
     ResizeRatioY := 1;
     if Picture.Width <> CARD_CONTENT_PICTURE_MAX_WIDTH then begin
       ResizeRatioX := Picture.Width / CARD_CONTENT_PICTURE_MAX_WIDTH;
       ContentPicture.Picture.Bitmap.Width := CARD_CONTENT_PICTURE_MAX_WIDTH;
     end;
     if Picture.Height <> CARD_CONTENT_PICTURE_MAX_HEIGHT then begin
       ResizeRatioY := Picture.Height / CARD_CONTENT_PICTURE_MAX_HEIGHT;
       ContentPicture.Picture.Bitmap.Height := CARD_CONTENT_PICTURE_MAX_HEIGHT;
     end;
     EditPictureFilename.Text := UrlDecode(ExtractFileNameBetter(Location));
     SetCardChanged(True);
     if (ResizeRatioX <> 1) or (ResizeRatioY <> 1) then begin
       Imager.QueueResize(Picture.Bitmap, Ceil(Picture.Width / Min(ResizeRatioX, ResizeRatioY)),
                          Ceil(Picture.Height / Min(ResizeRatioX, ResizeRatioY)),
                          CurrentCard, rsContent, ResizeCallback);
     end;
  end else if not IsError then begin
    MessageDlg('Failed to load a picture from ' + LocationType + ' "' + Location + '".', mtWarning, [mbOK], 0);
  end;
end;


// Loads a picture for the current site, either file file or URL
procedure TMainForm.LoadSitePicture(Location, LocationType: String);
var
  Picture: TPicture;
  ResizeRatioX, ResizeRatioY: Single;
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
     if CurrentSite.OriginalContentPicture = nil then begin
       CurrentSite.OriginalContentPicture := CurrentSite.ContentPicture;
     end else begin
       FreeAndNil(CurrentSite.ContentPicture);
     end;
     CurrentSite.ContentPicture := Picture;
     SiteContentPicture.Picture.Assign(CurrentSite.ContentPicture);
     ResizeRatioX := 1;
     ResizeRatioY := 1;
     if Picture.Width <> SITE_CONTENT_PICTURE_MAX_WIDTH then begin
       ResizeRatioX := Picture.Width / SITE_CONTENT_PICTURE_MAX_WIDTH;
       SiteContentPicture.Picture.Bitmap.Width := SITE_CONTENT_PICTURE_MAX_WIDTH;
     end;
     if Picture.Height <> SITE_CONTENT_PICTURE_MAX_HEIGHT then begin
       ResizeRatioY := Picture.Height / SITE_CONTENT_PICTURE_MAX_HEIGHT;
       SiteContentPicture.Picture.Bitmap.Height := SITE_CONTENT_PICTURE_MAX_HEIGHT;
     end;
     EditSitePictureFilename.Text := UrlDecode(ExtractFileNameBetter(Location));
     SetSiteChanged(True);
     if (ResizeRatioX <> 1) or (ResizeRatioY <> 1) then begin
       Imager.QueueResize(Picture.Bitmap, Ceil(Picture.Width / Min(ResizeRatioX, ResizeRatioY)),
                          Ceil(Picture.Height / Min(ResizeRatioX, ResizeRatioY)),
                          CurrentSite, rsContent, ResizeCallback);
     end;
  end else begin
    MessageDlg('Failed to load a picture from ' + LocationType + ' "' + Location + '".', mtWarning, [mbOK], 0);
  end;
end;


procedure TMainForm.OpenPictureDialogButtonClick(Sender: TObject);
begin
   OpenPictureDialog.Filename := '';
   if (OpenPictureDialog.Execute()) then begin
     Application.ProcessMessages();
     LoadCardPicture(OpenPictureDialog.Filename, 'file');
   end;
end;


procedure TMainForm.OpenPictureURLDialogButtonClick(Sender: TObject);
var
  URL: String;
begin
  if Clipboard.HasFormat(CF_TEXT) then URL := Clipboard.AsText;
  if InputQuery('Enter a URL to load picture from', 'Picture URL:', URL) then begin
     Application.ProcessMessages();
     LoadCardPicture(URL, 'URL');
   end;
end;


procedure TMainForm.OpenSitePictureButtonClick(Sender: TObject);
begin
   OpenPictureDialog.Filename := '';
   if (OpenPictureDialog.Execute()) then begin
     Application.ProcessMessages();
     LoadSitePicture(OpenPictureDialog.Filename, 'file');
   end;
end;


procedure TMainForm.OpenSitePictureURLDialogButtonClick(Sender: TObject);
var
  URL: String;
begin
  if Clipboard.HasFormat(CF_TEXT) then URL := Clipboard.AsText;
  if InputQuery('Enter a URL to load picture from', 'Picture URL:', URL) then begin
     Application.ProcessMessages();
     LoadSitePicture(URL, 'URL');
  end;
end;


// Updates the database information (name, size, cards count) on first page
procedure TMainForm.UpdateDatabaseInfo();
var
  FellowshipRaces, ShadowRaces, I: Integer;
begin
  FellowshipRaces := 0;
  ShadowRaces := 0;
  InfoDatabaseNameLabel.Caption := Database.Filename;
  InfoDatabaseSizeLabel.Caption := FormatByteSize(Database.GetSize());
  InfoDatabaseCardsLabel.Caption := IntToStr(Cards.Count);
  InfoDatabaseSitesLabel.Caption := IntToStr(Sites.Count);
  InfoDatabaseDecksLabel.Caption := IntToStr(Decks.Count);
  InfoDatabaseCreatedLabel.Caption := GetSetting('DatabaseCreated');
  InfoDatabaseModifiedLabel.Caption := GetSetting('DatabaseModified');
  for I := 0 to Races.Count - 1 do begin
    if TRace(Races.Items[I]).IsGood then
      Inc(FellowshipRaces)
    else
      Inc(ShadowRaces);
  end;
  InfoDatabaseRacesLabel.Caption := Format('%d Fellowship and %d Shadow races', [FellowshipRaces, ShadowRaces]);
  ButtonSaveCardImages.Enabled := (Cards.Count > 0);
  ButtonSaveSiteImages.Enabled := (Sites.Count > 0);
end;


procedure TMainForm.FormShow(Sender: TObject);
var DB: TPersistence;
begin
  DB := TPersistence.Create(ExtractFilePath(Application.ExeName) + DEFAULT_DB_FILENAME);
  LoadDatabase(DB);
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
  if (ListCards.ItemIndex < 0) and (ListCards.Items.Count > 0) then begin
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
end;


procedure TMainForm.ListDecksCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var Deck1, Deck2: TDeck;
    Field1, Field2: String;
    IsNumberSort: Boolean;
    SortInfo: TSortInfo;
begin
  SortInfo := ListDecksSortInfo;
  IsNumberSort := False;
  Deck1 := Item1.Data;
  Deck2 := Item2.Data;
  if (SortInfo.SortColumn = DECK_LIST_COLUMN_TITLE) then begin
    Field1 := Deck1.Title;
    Field2 := Deck2.Title;
  end else if (SortInfo.SortColumn = DECK_LIST_COLUMN_CARDS) then begin
    Field1 := IntToStr(Deck1.GetCardsCount());
    Field2 := IntToStr(Deck2.GetCardsCount());
    IsNumberSort := True;
  end else if (SortInfo.SortColumn = DECK_LIST_COLUMN_COMMENT) then begin
    Field1 := Deck1.Comment;
    Field2 := Deck2.Comment;
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
  if ((Compare = 0) and (SortInfo.SortColumn <> DECK_LIST_COLUMN_TITLE)) then begin
    Field1 := Deck1.Title;
    Field2 := Deck2.Title;
    Compare := StrIComp(PChar(Field1), PChar(Field2));
  end;
  if (SortInfo.IsSortReversed) then
    Compare := 0 - Compare;
end;


procedure TMainForm.ListDecksColumnClick(Sender: TObject;
  Column: TListColumn);
begin
 if (Column.Caption = ListDecksSortInfo.SortColumn) then
    ListDecksSortInfo.IsSortReversed := not ListDecksSortInfo.IsSortReversed else
  ListDecksSortInfo.SortColumn := Column.Caption;
  ListDecks.CustomSort(nil, 0);
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
var
  I: Integer;
  PreviousCurrentCard: TCard;
  Directory: String;
  Filename: String;
  Bitmap: TBitmap;
begin
  if (GetCheckSavingResult(True, False, False) <> mrCancel) then begin
    Directory := BrowseForFolder('Select a folder where to save card images', '', True);
    if Directory <> '' then begin
      ShowMessage(Format('Saving %d cards to folder "%s".' + #13#10#13#10 + 'The program will be unresponsive for a few seconds.' + #13#10#13#10 + 'Program should be focused and should have no other window on top of it, otherwise saved images will show the contents of those windows.', [Cards.Count, Directory]));
      Application.ProcessMessages(); // Let dialogs disappear
      PageControl.ActivePage := PageCardEditor;
      MainForm.Enabled := False;
      PreviousCurrentCard := CurrentCard;
      for I := 0 to Cards.Count - 1 do begin
        CurrentCard := Cards.Items[I];
        LoadCard(CurrentCard);
        Filename := Format('%s\%s.png', [Directory, CurrentCard.GetShowName(), '.png']);
        Bitmap := GetCurrentCardImage();
        Imager.SavePictureToDisk(Bitmap, Filename);
        Bitmap.Free();
      end;
      CurrentCard := PreviousCurrentCard;
      if (CurrentCard <> nil) then LoadCard(CurrentCard);
      PageControl.ActivePage := PageDatabase;
      MainForm.Enabled := True;
      ShowMessage(Format('Saved %d card images to folder "%s".', [Cards.Count, Directory]));
    end;
  end;
end;





procedure TMainForm.ButtonSaveSiteImagesClick(Sender: TObject);
var
  I: Integer;
  PreviousCurrentSite: TSite;
  Directory: String;
  Filename: String;
  Bitmap: TBitmap;
begin
  if (GetCheckSavingResult(False, False, True) <> mrCancel) then begin
    Directory := BrowseForFolder('Select a folder where to save site images', '', True);
    if Directory <> '' then begin
      ShowMessage(Format('Saving %d sites to folder "%s".' + #13#10#13#10 + 'The program will be unresponsive for a few seconds.' + #13#10#13#10 + 'Program should be focused and should have no other window on top of it, otherwise saved images will show the contents of those windows.', [Sites.Count, Directory]));
      Application.ProcessMessages(); // Let dialogs disappear
      PageControl.ActivePage := PageSiteEditor;
      MainForm.Enabled := False;
      PreviousCurrentSite := CurrentSite;
      for I := 0 to Sites.Count - 1 do begin
        CurrentSite := Sites.Items[I];
        LoadSite(CurrentSite);
        Filename := Format('%s\%s.png', [Directory, CurrentSite.GetShowName(), '.png']);
        Bitmap := GetCurrentSiteImage();
        Imager.SavePictureToDisk(Bitmap, Filename);
        Bitmap.Free();
      end;
      CurrentSite := PreviousCurrentSite;
      if (CurrentSite <> nil) then LoadSite(CurrentSite);
      PageControl.ActivePage := PageDatabase;
      MainForm.Enabled := True;
      ShowMessage(Format('Saved %d site images to folder "%s".', [Sites.Count, Directory]));
    end;
  end;    
end;

end.

